"""PyTorch models: MLP with player embeddings and a GRU over each player's recent games."""
import json
import sys
import time

import numpy as np
import polars as pl
import torch
import torch.nn as nn

from . import data as D
from . import features as F
from .gbm import prepared_folds
from .zoo import FEATS, PRED_DIR, matrices

torch.set_num_threads(4)
L = 20
SEQ_COLS = ["usg_pct", "min", "fga36", "fta36", "tov36", "ast_pct", "reb_pct", "ts_pct", "started"]
SEQ_SCALE = np.array([5.0, 1 / 48, 1 / 30, 1 / 10, 1 / 8, 3.0, 3.0, 1.5, 1.0], dtype=np.float32)


def seq_source():
    box, _ = F.load_games()
    p = F.player_history(box).select("player_id", "game_id", "game_date", *SEQ_COLS).sort(
        "player_id", "game_date", "game_id")
    return p.with_row_index("idx")


def make_seq(frame, src):
    """(n, L, d+1) tensor of the player's previous L played games, zero padded, plus mask."""
    S = np.nan_to_num(src.select(SEQ_COLS).to_numpy().astype(np.float32)) * SEQ_SCALE
    days = src["game_date"].cast(pl.Int32).to_numpy().astype(np.float32)
    pid = src["player_id"].to_numpy()
    idx = frame.select("player_id", "game_id").join(src.select("player_id", "game_id", "idx"),
                                                    on=["player_id", "game_id"], how="left")["idx"].cast(pl.Int64).to_numpy()
    n = len(idx)
    out = np.zeros((n, L, S.shape[1] + 2), dtype=np.float32)
    for k in range(1, L + 1):
        j = idx - k
        ok = (j >= 0) & (pid[np.clip(j, 0, None)] == pid[idx])
        jj = np.where(ok, j, 0)
        out[:, L - k, :S.shape[1]] = S[jj] * ok[:, None]
        gap = np.log1p(np.clip(days[idx] - days[jj], 0, 800)) / 7  # days before the target game
        out[:, L - k, S.shape[1]] = gap * ok
        out[:, L - k, S.shape[1] + 1] = ok
    return out


class Net(nn.Module):
    def __init__(self, n_feat, n_players, seq=False, d_seq=11):
        super().__init__()
        self.emb = nn.Embedding(n_players, 16)
        self.seq = seq
        if seq:
            self.gru = nn.GRU(d_seq, 64, batch_first=True)
        d_in = n_feat + 16 + (64 if seq else 0)
        self.mlp = nn.Sequential(nn.Linear(d_in, 256), nn.ReLU(), nn.Dropout(0.15), nn.Linear(256, 128), nn.ReLU(),
                                 nn.Dropout(0.15), nn.Linear(128, 1))

    def forward(self, x, pid, base, s=None):
        z = [x, self.emb(pid)]
        if self.seq:
            _, h = self.gru(s)
            z.append(h[-1])
        return base + 0.1 * self.mlp(torch.cat(z, 1)).squeeze(1)


def loss_fn(kind):
    if kind == "mse":
        return nn.MSELoss()
    if kind == "huber":
        return nn.HuberLoss(delta=0.05)
    return nn.L1Loss()


def run_fold(name, tr, va, src, seq, kind, epochs, seed=0):
    torch.manual_seed(seed)
    np.random.seed(seed)
    Xtr, Xva = matrices(tr, va, True)
    cnt = tr.group_by("player_id").len().filter(pl.col("len") >= 30)
    pmap = {p: i + 1 for i, p in enumerate(cnt["player_id"].to_list())}
    ptr = np.array([pmap.get(p, 0) for p in tr["player_id"].to_list()])
    pva = np.array([pmap.get(p, 0) for p in va["player_id"].to_list()])
    fb = tr["usg_pct"].mean()
    def base(fr):
        return fr.select(pl.coalesce("usg_ew1", "usg_season", "usg_career", pl.lit(fb)))[:, 0].to_numpy()
    btr, bva = base(tr), base(va)
    ytr, yva = tr["usg_pct"].to_numpy(), va["usg_pct"].to_numpy()
    Str = torch.from_numpy(make_seq(tr, src)) if seq else None
    Sva = torch.from_numpy(make_seq(va, src)) if seq else None
    T = lambda a, dt=torch.float32: torch.from_numpy(np.asarray(a)).to(dt)
    Xtr, Xva, btr_t, bva_t, ytr_t = T(Xtr), T(Xva), T(btr), T(bva), T(ytr)
    ptr_t, pva_t = T(ptr, torch.long), T(pva, torch.long)
    net = Net(Xtr.shape[1], len(pmap) + 1, seq)
    opt = torch.optim.AdamW(net.parameters(), lr=2e-3, weight_decay=1e-4)
    sched = torch.optim.lr_scheduler.OneCycleLR(opt, max_lr=3e-3, total_steps=epochs * ((len(ytr) + 1023) // 1024))
    crit = loss_fn(kind)
    curve = []
    def predict():
        net.eval()
        with torch.no_grad():
            ps = [net(Xva[i:i + 8192], pva_t[i:i + 8192], bva_t[i:i + 8192],
                      Sva[i:i + 8192] if seq else None) for i in range(0, len(yva), 8192)]
        return torch.cat(ps).numpy()
    for ep in range(epochs):
        net.train()
        perm = torch.randperm(len(ytr))
        tot = 0.0
        for i in range(0, len(perm), 1024):
            b = perm[i:i + 1024]
            p = net(Xtr[b], ptr_t[b], btr_t[b], Str[b] if seq else None)
            loss = crit(p, ytr_t[b])
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(b)
        pv = predict()
        curve.append({"epoch": ep + 1, "train_loss": tot / len(ytr), "val_mae": float(np.abs(pv - yva).mean())})
    return predict(), curve


def run(kinds=("huber",), which=("MLP", "GRU"), epochs=12):
    df, a = D.load()
    folds = list(prepared_folds(df, a))
    src = seq_source()
    out = json.load(open(D.CACHE / "nn.json")) if (D.CACHE / "nn.json").exists() else {}
    for mname in which:
        for kind in kinds:
            t = time.time()
            per, curves = [], []
            for n, tr, va in folds:
                p, c = run_fold(n, tr, va, src, mname == "GRU", kind, epochs)
                np.save(PRED_DIR / f"{mname}__{kind}__{n}.npy", p)
                per.append({"fold": n, **D.metrics(va["usg_pct"].to_numpy(), p)})
                curves.append(c)
                print(f"{mname} {kind} {n} mae={per[-1]['mae']:.4f} {time.time()-t:.0f}s", flush=True)
            out[f"{mname}|{kind}"] = {"res": per, "curves": curves}
            json.dump(out, open(D.CACHE / "nn.json", "w"))


if __name__ == "__main__":
    kinds = sys.argv[2].split(",") if len(sys.argv) > 2 else ["huber"]
    run(kinds=kinds, which=(sys.argv[1],) if len(sys.argv) > 1 else ("MLP", "GRU"))

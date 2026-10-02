"""PyTorch models: an MLP with player embeddings, and a GRU over recent games.

Both are fitted on the training seasons only, with the last training season
held out to decide when to stop, so the validation season is never used for
model selection. Player embeddings are indexed from the training seasons; a
player unseen in training falls into a shared unknown slot.
"""

from __future__ import annotations

import numpy as np
import polars as pl
import torch
from torch import nn

from .evaluate import metrics
from .models import design_matrix

SEQ_STATS = ["usg_pct", "min", "fga", "fta", "ast_pct", "ts_pct"]
SEQ_LEN = 10


def _player_index(train: pl.DataFrame) -> dict[int, int]:
    """Embedding slot per player; 0 is reserved for unseen players."""
    return {pid: i + 1 for i, pid in enumerate(sorted(train["player_id"].unique().to_list()))}


def _standardise(train_x: np.ndarray, *others: np.ndarray):
    med = np.nanmedian(train_x, axis=0)
    train_x = np.where(np.isnan(train_x), med, train_x)
    mu, sd = train_x.mean(0), train_x.std(0)
    sd[sd == 0] = 1.0
    out = [(train_x - mu) / sd]
    for o in others:
        o = np.where(np.isnan(o), med, o)
        out.append((o - mu) / sd)
    return out


class _MLP(nn.Module):
    def __init__(self, n_features: int, n_players: int, emb_dim: int = 16, hidden: int = 128):
        super().__init__()
        self.emb = nn.Embedding(n_players, emb_dim)
        nn.init.normal_(self.emb.weight, std=0.01)
        self.net = nn.Sequential(
            nn.Linear(n_features + emb_dim, hidden),
            nn.ReLU(),
            nn.Dropout(0.2),
            nn.Linear(hidden, hidden // 2),
            nn.ReLU(),
            nn.Dropout(0.1),
            nn.Linear(hidden // 2, 1),
        )

    def forward(self, x, pid):
        return self.net(torch.cat([x, self.emb(pid)], dim=1)).squeeze(-1)


class _GRU(nn.Module):
    """Recent games as a sequence, alongside the same static features."""

    def __init__(self, n_features: int, n_seq_stats: int, n_players: int, emb_dim: int = 16, hidden: int = 64):
        super().__init__()
        self.emb = nn.Embedding(n_players, emb_dim)
        nn.init.normal_(self.emb.weight, std=0.01)
        self.gru = nn.GRU(n_seq_stats, hidden, batch_first=True)
        self.head = nn.Sequential(
            nn.Linear(hidden + n_features + emb_dim, 128),
            nn.ReLU(),
            nn.Dropout(0.2),
            nn.Linear(128, 1),
        )

    def forward(self, x, seq, pid):
        _, h = self.gru(seq)
        return self.head(torch.cat([h[-1], x, self.emb(pid)], dim=1)).squeeze(-1)


def _train_loop(model, tensors_tr, tensors_va, y_tr, y_va, epochs=60, patience=8, lr=1e-3, bs=1024):
    opt = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-4)
    lossf = nn.MSELoss()
    n = len(y_tr)
    best, best_state, bad = float("inf"), None, 0
    g = torch.Generator().manual_seed(0)
    for _ in range(epochs):
        model.train()
        perm = torch.randperm(n, generator=g)
        for i in range(0, n, bs):
            idx = perm[i : i + bs]
            opt.zero_grad()
            loss = lossf(model(*[t[idx] for t in tensors_tr]), y_tr[idx])
            loss.backward()
            opt.step()
        model.eval()
        with torch.no_grad():
            vl = lossf(model(*tensors_va), y_va).item()
        if vl < best - 1e-7:
            best, bad = vl, 0
            best_state = {k: v.clone() for k, v in model.state_dict().items()}
        else:
            bad += 1
            if bad >= patience:
                break
    if best_state is not None:
        model.load_state_dict(best_state)
    return model


def _split_inner(train: pl.DataFrame):
    seasons = sorted(train["season"].unique().to_list())
    return train.filter(pl.col("season") != seasons[-1]), train.filter(pl.col("season") == seasons[-1])


def fit_mlp(
    train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str = "usg_pct"
) -> tuple[dict[str, float], object]:
    torch.manual_seed(0)
    inner_tr, inner_va = _split_inner(train)
    pidx = _player_index(train)

    xs = _standardise(design_matrix(inner_tr, cols), design_matrix(inner_va, cols), design_matrix(valid, cols))
    tens = [torch.tensor(x, dtype=torch.float32) for x in xs]
    pids = [
        torch.tensor([pidx.get(p, 0) for p in df["player_id"].to_list()], dtype=torch.long)
        for df in (inner_tr, inner_va, valid)
    ]
    ys = [torch.tensor(df[target].to_numpy(), dtype=torch.float32) for df in (inner_tr, inner_va)]

    model = _MLP(len(cols), len(pidx) + 1)
    model = _train_loop(model, (tens[0], pids[0]), (tens[1], pids[1]), ys[0], ys[1])
    model.eval()
    with torch.no_grad():
        pred = model(tens[2], pids[2]).numpy()
    return metrics(valid[target].to_numpy(), pred), model


def add_sequence_columns(played: pl.DataFrame, seq_len: int = SEQ_LEN) -> pl.DataFrame:
    """Lagged copies of a few per-game stats, oldest first, per player."""
    return played.sort("player_id", "game_date").with_columns(
        [
            pl.col(stat).shift(lag).over("player_id").alias(f"seq_{stat}_{lag}")
            for stat in SEQ_STATS
            for lag in range(1, seq_len + 1)
        ]
    )


def fit_gru(
    train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str = "usg_pct", seq_len: int = SEQ_LEN
) -> tuple[dict[str, float], object]:
    torch.manual_seed(0)
    inner_tr, inner_va = _split_inner(train)
    pidx = _player_index(train)
    seq_cols = [f"seq_{s}_{lag}" for lag in range(seq_len, 0, -1) for s in SEQ_STATS]

    xs = _standardise(design_matrix(inner_tr, cols), design_matrix(inner_va, cols), design_matrix(valid, cols))
    ss = _standardise(
        design_matrix(inner_tr, seq_cols), design_matrix(inner_va, seq_cols), design_matrix(valid, seq_cols)
    )
    tens = [torch.tensor(x, dtype=torch.float32) for x in xs]
    seqs = [
        torch.tensor(s, dtype=torch.float32).reshape(-1, seq_len, len(SEQ_STATS)) for s in ss
    ]
    pids = [
        torch.tensor([pidx.get(p, 0) for p in df["player_id"].to_list()], dtype=torch.long)
        for df in (inner_tr, inner_va, valid)
    ]
    ys = [torch.tensor(df[target].to_numpy(), dtype=torch.float32) for df in (inner_tr, inner_va)]

    model = _GRU(len(cols), len(SEQ_STATS), len(pidx) + 1)
    model = _train_loop(
        model, (tens[0], seqs[0], pids[0]), (tens[1], seqs[1], pids[1]), ys[0], ys[1]
    )
    model.eval()
    with torch.no_grad():
        pred = model(tens[2], seqs[2], pids[2]).numpy()
    return metrics(valid[target].to_numpy(), pred), model

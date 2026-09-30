"""Score a class survey export with the Week 3 Geller-typology rules.

Usage:
    python3 score_class.py data/class_responses.csv

Expected columns (one row per student):
    respondent_id, A1 (Yes/No), A2 (1-4), A3 (Yes/No), S1..S5 (1-4), B1 (text, optional)

Writes <input>_scored.csv next to the input and prints a summary table.
Uses only the Python standard library.
"""
import csv
import sys
from collections import Counter
from pathlib import Path

TYPES = {"SF": "Strong & Fearless", "EC": "Enthused & Confident",
         "IC": "Interested but Concerned", "NW": "No Way No How"}
GELLER = {"SF": 0.5, "EC": 7, "IC": 60, "NW": 33}


def classify(r):
    """Return (type_code, reason). Steps are applied in order."""
    if r["A1"] == "No":
        return "NW", "Step 1: not able to ride"
    if r["A2"] <= 2 and r["A3"] == "No":
        return "NW", "Step 2: not interested and not riding"
    if r["S5"] == 1:
        return "NW", "Step 2b: very uncomfortable even on a path"
    if r["S1"] == 4:
        return "SF", "Step 3: very comfortable with no bike lane"
    if r["S2"] == 4:
        return "EC", "Step 4: very comfortable with a painted lane"
    return "IC", "Interested, not very comfortable even with a painted lane"


def clean(row):
    r = {k.strip(): (v or "").strip() for k, v in row.items()}
    r["A1"] = "No" if r["A1"].lower().startswith("n") else "Yes"
    r["A3"] = "Yes" if r["A3"].lower().startswith("y") else "No"
    for k in ("A2", "S1", "S2", "S3", "S4", "S5"):
        r[k] = int(float(r[k][0])) if r[k] else 0  # accepts "4" or "4 - Very comfortable"
    return r


def main(path):
    path = Path(path)
    with path.open(newline="", encoding="utf-8-sig") as f:
        rows = [clean(r) for r in csv.DictReader(f)]
    for r in rows:
        r["type"], r["reason"] = classify(r)
        r["type_name"] = TYPES[r["type"]]
        r["mean_comfort"] = round(sum(r[f"S{i}"] for i in range(1, 6)) / 5, 2)
        r["separation_gain"] = max(r["S3"], r["S4"], r["S5"]) - r["S1"]

    out = path.with_name(path.stem + "_scored.csv")
    with out.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    n = len(rows)
    counts = Counter(r["type"] for r in rows)
    print(f"\nn = {n}\n")
    print(f"{'Type':<28}{'Class n':>8}{'Class %':>9}{'Geller %':>10}{'Mean gain':>11}")
    for t in ("SF", "EC", "IC", "NW"):
        g = [r["separation_gain"] for r in rows if r["type"] == t]
        mg = f"{sum(g) / len(g):+.1f}" if g else "–"
        print(f"{TYPES[t]:<28}{counts[t]:>8}{100 * counts[t] / n:>8.0f}%{GELLER[t]:>9}%{mg:>11}")
    barriers = Counter(r.get("B1") for r in rows if r.get("B1"))
    if barriers:
        print("\nTop barriers (B1):", ", ".join(f"{k} ({v})" for k, v in barriers.most_common(4)))
    print(f"\nScored file: {out}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "data/class_responses.csv")

# Week 3 · Employing Geller's Typology

Quarto reveal.js deck that turns Geller's *Four Types of Cyclists* into a short survey, a rule-based score, and a live class-vs-Portland comparison.

## Files

| File | What it is |
|---|---|
| `week03-geller-typology.qmd` | The slide deck (source) |
| `week03-geller-typology.html` + `_files/` | Rendered deck — open in a browser (needs internet for the interactive slides) |
| `custom.scss` | Theme: fonts, type colours, survey-scale styling |
| `images/` | Street scenarios S1–S5, four-types bar, scoring flowchart, comfort profiles |
| `make_figures.py` | Regenerates everything in `images/` (matplotlib) |
| `data/class_responses.csv` | **Sample** responses (IDs start with `SAMPLE-`) — replace with your class's |
| `data/class_responses_TEMPLATE.csv` | Empty file with the expected column headers |
| `score_class.py` | Scores a CSV outside the slides; writes `*_scored.csv` and prints a summary |

## Running it in class

1. Build a Google/Microsoft Form with items **A1, A2, A3, S1–S5, B1** exactly as worded on slides 8–14 (show the street images in the form, or project them while students answer).
2. Export responses to CSV with columns `respondent_id, A1, A2, A3, S1, S2, S3, S4, S5, B1`
   - A1, A3 = `Yes`/`No`; A2 and S1–S5 = `1`–`4` (a leading digit like "4 - Very comfortable" also works in the script).
3. Save it as `data/class_responses.csv` and run `quarto render week03-geller-typology.qmd`. The "Sample data" banner disappears automatically once no IDs start with `SAMPLE`.
4. Or, without re-rendering: `python3 score_class.py data/class_responses.csv`.

The **Score yourself** slide works in any browser — students click their answers and see their type, mean comfort, separation gain, and profile line. Nothing is sent anywhere.

## Scoring rules (applied in order)

1. A1 = No → **No Way No How**
2. A2 ≤ 2 and A3 = No → **No Way No How**
   2b. S5 = 1 → **No Way No How**
3. S1 = 4 → **Strong & Fearless**
4. S2 = 4 → **Enthused & Confident**, otherwise **Interested but Concerned**

Adapted and simplified from Dill & McNeil (2013), *TRR* 2387. Continuous companions: mean comfort (S1–S5) and separation gain = max(S3, S4, S5) − S1.

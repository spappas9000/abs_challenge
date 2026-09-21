# ABS Project - DSCI 470/STAT 495
Authors: August Decz, Coleton Grossman, Greg Matthews, Steven Pappas, and Oliver Schramm

This repository stores all of the code files for our ABS (Automated Balls and Strikes) project.

GOALS:
- **Optimal Stopping Theory:** We want to determine WHEN it is most valuable for a hitter or a catcher to use one of their ABS challenges.
- **Modeling:** We want to model the probability that hitters and catchers challenge *when they should* and *when they shouldn't*. This is 
  the first phase of this project.
  - **Response:** `challenge_hitter` / `challenge_catcher` (was the pitch challenged?)
  - **Covariates:** Pitch location (x and z), change in run expectancy ($\delta$), game state (outs, balls, strikes, inning),
                    challenges remaining
  - **Random Effects ($\gamma$):** Batter, Umpire, Pitcher, Catcher
  - **Sample:** ~647k pitches, 2026-03-25 through 2026-09-09 (as of 2026-09-21)

---

## 1. Prerequisites

| Requirement | Version used | Notes |
|---|---|---|
| R | 4.5.2 | |
| RStudio | any recent | open `ABS-MODEL.Rproj` so the working directory is the repo root |
| Quarto | any recent | only needed for `eda.qmd` |

Install packages:

```r
install.packages(c(
  "tidyverse", "data.table", "arrow", "httr", "httr2", "jsonlite",
  "lme4", "car", "caret", "pROC", "MLmetrics", "splines",
  "ggpubr", "gt", "gtExtras", "broom",
  "shiny", "shinydashboard", "shinydashboardPlus", "rsconnect",
  "remotes"
))

# baseballr and mlbplotR are GitHub-only / newer than CRAN
remotes::install_github("BillPetti/baseballr")
remotes::install_github("camdenk/mlbplotR")
```

**Always open the project via `ABS-MODEL.Rproj`.** Every path in this repo is relative to
the repo root, and scripts will not find `data/` otherwise.

---

## 2. Getting the data

Some data files are in git; the two large ones are **not**, because they exceed GitHub's
100 MB per-file limit. Get those separately before running anything. This process is written below.

### Already in the repo (just `git clone`)

| File | Size | What it is |
|---|---|---|
| `data/catcher.csv` | 2.2 MB | Baseball Savant export: all catcher-initiated ABS challenges |
| `data/hitter.csv` | 2.0 MB | Savant export: all hitter-initiated ABS challenges |
| `data/pitcher.csv` | 80 KB | Savant export: all pitcher-initiated ABS challenges |
| `data/chase.csv` | 240 KB | Savant leaderboard: per-batter chase/swing profile |

These are Baseball Savant **search-page CSV exports**, downloaded by hand. To refresh
them, re-run the saved Savant search for each challenger type and overwrite the file —
keep the column set identical.

### Not in the repo — you must generate or fetch these

| File | Size | How to get it |
|---|---|---|
| `data/statcast09-13-26.parquet` | 100 MB | Run `get_statcast.R` (see below), or copy from a teammate |
| `data/chadwick_batters.csv` | 86 MB | Chadwick Bureau player register — see below |
| `data/home_plate_umps.csv` | 44 KB | Second half of `get_statcast.R` |
| `takedistribution/statcastdata.rds` | — | Needed only for the Shiny app; see §5 |

**`statcast09-13-26.parquet`** — `get_statcast.R` pulls the full season from Statcast in
5-day chunks. It takes roughly an hour and hammers the Savant API, so **copy the file
from a teammate rather than re-pulling it** unless you specifically need fresher data.
Note the July gap in the script (7/13–7/15) is the All-Star break, which is intentional.

**`chadwick_batters.csv`** — the player-ID crosswalk used to attach names to MLBAM IDs.
Rather than passing an 86 MB file around, regenerate it locally:

```r
library(baseballr)
library(tidyverse)
chadwick_player_lu() |>
  select(key_mlbam, name_last, name_first) |>
  filter(!is.na(key_mlbam)) |>
  write_csv("data/chadwick_batters.csv")
```

**Never `git add` the parquet or the Chadwick CSV.** `.gitignore` excludes them; a push
containing either will be rejected by GitHub and is a nuisance to unwind.

---

## 3. Run order

```
get_statcast.R      # ONE TIME (or when refreshing data) — writes data/*.parquet + umps csv
      |
      v
data.R              # RUN EVERY SESSION — builds all analysis frames in memory
      |
      +--> eda_work.R          # exploratory tables, ANOVAs
      +--> model_work.R        # the models (this is the main file)
      +--> model_testing.R     # k-fold CV to pick spline/polynomial order
      +--> visualizations.R    # plots and gt tables for the deck
```

`data.R` is the only script that touches raw files, and every downstream script 
assumes its objects are already in the global environment. Start each
session with `source("data.R")`.

### What `data.R` leaves in your environment

| Object | Rows | What it is |
|---|---|---|
| `statcastlist_26` | ~647k | Raw season Statcast, all pitch outcomes |
| `challenge` | ~10k | The three Savant challenge exports stacked, with `call_change` flags |
| `playerid` | — | `key_mlbam` → `"Last, First"` |
| `modeldata` | — | Challenges + all takeable pitches, deduped, with adjusted location/movement |
| `runexpectancies` | — | Mean Δrun-expectancy by base-out-count-outcome state |
| `runexpectancies2` | — | Self-join of the above: the ball-vs-strike pair for each state |
| `modeldata2` | — | `modeldata` + `delta` (run-expectancy swing from the call) |
| `chase` | — | Per-batter chase profile, keyed on `batter` |

Key derived variables:

- **`plate_x_adj` / `plate_z_adj`** — location re-centered so positive x is always inside
  to the batter and z is relative to the middle of that batter's strike zone. Use these,
  not raw `plate_x` / `plate_z`, in models.
- **`delta`** — absolute run-expectancy difference between the call as made and the call
  reversed, for that exact base-out-count state. This is the "how much does this pitch
  matter" predictor.
- **`challenges_remaining`** — computed in `model_work.R`, not `data.R`, because the
  hitter and catcher sides count down separately (2 per team, +1 in extras).

---

## 4. File map

| File | Status | Purpose |
|---|---|---|
| `get_statcast.R` | stable | Pulls Statcast → parquet; scrapes home-plate umpires from the MLB StatsAPI |
| `data.R` | stable | Session initializer. Must run first. |
| `model_work.R` | **active** | Builds `modeldata_hitter` / `modeldata_catcher`, fits `glm` and `glmer` models, pulls random effects |
| `model_testing.R` | scratch | Cross-validation loops for choosing spline df |
| `eda_work.R` | scratch | One-off tables and ANOVAs |
| `visualizations.R` | stable | Density plots, prediction grids, gt tables for the deck |
| `takedistribution_work.R` | scratch | Prototype for the Shiny app's core function |
| `takedistribution/` | deployed | Shiny app: distribution of remaining takes in a game state |
| `eda.qmd` | stub | Currently holds open questions, not analysis |
| `notes` | living | Meeting notes with Greg — the de facto to-do list |

### Model naming

`mod{N}_call_change_{hitter,catcher}`, where `N` is model complexity:
`mod0` = location only, `mod1` = location + game state (fixed effects, `glm`),
`mod2` = `mod1` + random effects for batter, catcher, both teams, and umpire (`glmer`).
Suffix `_red` = the reduced model in a likelihood-ratio comparison.

---

## 5. The Shiny app

`takedistribution/` expects `takedistribution/statcastdata.rds`, which is gitignored and
**not currently present in the repo**. Regenerate it after running `data.R`:

```r
saveRDS(statcastlist_26, "takedistribution/statcastdata.rds")
```

Then `shiny::runApp("takedistribution")`. Deployment goes to Posit Connect Cloud under
`spappas9000/take_distribution`.

---

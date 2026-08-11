# Precomputed simulation atlas

## Purpose and scope

This self-contained auxiliary module generates precomputed summaries consumed
by the separate Shiny interface in `shiny-app/`. It complements the manuscript
simulations without replacing them. The
manuscript keeps a small number of tuned, transparent worked scenarios. The
atlas varies sample size, the two additive main effects, scale location, and a
small number of family-specific design or measurement parameters around those
same anchors. The generating-scale product term is exactly zero in every row.

The grid uses sensitivity slices instead of a full Cartesian product. In a
one-dimensional slice, only the named parameter differs from its central
anchor; the main-effect surface varies only the two additive main effects. This
keeps the scientific comparisons readable and the offline computation bounded.
Duplicate anchor combinations are stored once, with all of their memberships
in `slice_membership`.

The three families follow the current manuscript code:

- forced-choice accuracy follows `scripts/03a-simulation-forced-choice.R`;
- bounded sum scores follow `scripts/03c-simulation-sum-scores.R`;
- within-family logit/probit comparisons follow
  `scripts/03b-simulation-within-family-links.R`.

The targeted diagnostic component adapts the applicable AIC, DHARMa, and
Pregibon-style checks from `scripts/04-diagnostic-worked-example.R`. Checks that
are not structurally meaningful are recorded as inapplicable rather than
forced. In particular, the paper does not define a same-response,
same-likelihood diagnostic comparison for its sum-score models. The core
within-family runner aggregates identical Bernoulli trials to binomial counts,
which has the same likelihood but is much cheaper; the diagnostic anchor keeps
the manuscript's trial-level layout for its DHARMa checks.

## Contents

- `01-build-scenario-grid.R`: builds and validates the partial-factorial grid,
  including text-based checks against the current manuscript anchor settings.
- `02-run-atlas.R`: offline, resumable Monte Carlo runner for the core and
  targeted diagnostic atlas.
- `03-summarize-atlas.R`: creates compact CSV and RDS summaries for the app.
- `R/`: family-specific generating, fitting, deterministic, and diagnostic
  helpers local to the atlas.
- `data/scenario-grid.csv`: readable, stable scenario definitions.
- `data/*-smoke.csv` and `data/*-smoke.rds`: tiny validation results, when
  present. They are never presented as full results.
- `data/atlas-summary.csv/.rds` and
  `data/diagnostic-atlas-summary.csv/.rds`: full app-facing summaries, created
  only after a full run.
- `raw/`: ignored replication-level RDS files, one per scenario and run type.

## Reproducibility and modes

Everything you can change lives in one clearly marked settings block at the top
of `02-run-atlas.R`. There are no environment variables and nothing to edit
under `R/`. The block is:

```r
MODE         <- "smoke"   # "smoke" (quick check) or "full" (the real run)
RUN_DHARMA   <- FALSE     # TRUE also runs the DHARMa residual checks
N_CORES      <- 1         # scenarios computed in parallel
OVERWRITE    <- FALSE     # TRUE recomputes already complete raw files
B            <- NA        # NA = the default for the chosen MODE
B_WITHIN     <- NA        # NA = the default for the chosen MODE
DHARMA_N_SIM <- NA        # NA = the default for the chosen MODE
```

`MODE = "smoke"` runs only one manuscript anchor per core family and the two
applicable paper diagnostic anchors, with B = 3 (2 for within-family). Smoke B
must be 2 or 3. Smoke summaries have `-smoke` in their filenames and
`run_type = "smoke"`.

`MODE = "full"` runs every declared core scenario, with B = 500 for
forced-choice and sum scores and B = 300 for the more expensive within-family
models. Full mode never reduces B silently. `DHARMA_N_SIM` defaults to 25 in
smoke mode and 250 in full mode.

`03-summarize-atlas.R` has its own short settings block, but it normally needs
no editing: it summarizes whatever `02-run-atlas.R` left in `raw/`, preferring
full results over smoke ones, and reads B off the raw file names so the two
scripts cannot silently disagree.

DHARMa dominates diagnostic runtime, so `RUN_DHARMA` is `FALSE` by default. That
pass still computes the AIC link comparison and the Pregibon-style added-term
check, writes its raw files as `diagnostic-nodharma-*.rds`, and marks the DHARMa
rows as applicable but not computed. Setting `RUN_DHARMA <- TRUE` and running
`02-run-atlas.R` again later writes the ordinary `diagnostic-*.rds` files
alongside them; `03-summarize-atlas.R` then prefers the complete set
automatically, so only the summarizer has to be re-run. Note that
`applicable = FALSE` means a check is structurally meaningless for that family,
whereas `computed = FALSE` means it was simply not run; the app reports the two
differently.

Every replication receives its own deterministic seed:

```text
seed = (base_seed + stream_offset + scenario_number * 10000 + replication)
       modulo .Machine$integer.max
```

The fixed base seed is 20260807. Family and core/diagnostic stream offsets are
fixed in `R/atlas-common.R`. Results therefore do not depend on the number of
workers or scheduling order.

## Commands

Always run from the repository root. First build the grid, which takes no
settings:

```bash
Rscript simulation-atlas/01-build-scenario-grid.R
```

Then run the atlas itself. With the settings block as shipped
(`MODE <- "smoke"`, `RUN_DHARMA <- FALSE`) this is a quick check:

```bash
Rscript simulation-atlas/02-run-atlas.R
Rscript simulation-atlas/03-summarize-atlas.R
Rscript shiny-app/smoke-test.R
```

For the real run, open `simulation-atlas/02-run-atlas.R`, set `MODE <- "full"`
and `N_CORES` to the number of cores you want to use, then run exactly the same
two commands. Nothing else changes, and `03-summarize-atlas.R` picks the full
results up on its own.

The same works from an R console at the repository root:

```r
source("simulation-atlas/02-run-atlas.R")
source("simulation-atlas/03-summarize-atlas.R")
```

Launch the app after summarizing:

```r
shiny::runApp("shiny-app")
```

The within-family random-intercept GLMMs and their DHARMa diagnostic checks
dominate runtime: a single within-family replication costs roughly twenty-five
times a forced-choice one. Expect the full run to take a few hours on a desktop
with `N_CORES` around 7. The runner skips a complete scenario/B raw file unless
`OVERWRITE <- TRUE`, so an interrupted full run can be resumed by simply
starting `02-run-atlas.R` again.

## Version control and archive policy

The grid, code, README files, and compact summaries belong in Git. Raw
replication-level RDS files under `raw/` do not: they may be large and are
ignored except for `.gitkeep`. After a full run, archive those raw files and the
listed reproducibility materials on OSF or another durable repository; see
`OSF-UPLOAD.md`.

The Shiny app reads compact precomputed summaries directly from this folder's
`data/` directory. It performs no Monte Carlo simulation and no model fitting
at runtime; this folder is independent of the Shiny UI.

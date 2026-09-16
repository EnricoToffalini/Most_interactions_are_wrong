# Precomputed simulation atlas

The Atlas repeats the three manuscript analyses over an explicit sensitivity
grid. It writes precomputed summaries for Supplement B and the Shiny app.
Generating interactions remain zero. Scenario IDs, ordering, parameter
combinations, model labels, and slice memberships are retained.

## Files

- `01-build-scenario-grid.R`: declares the core scenarios and diagnostic grid;
  writes `data/scenario-grid.csv` and `data/diagnostic-grid.csv`.
- `02a-run-forced-choice.R`: mirrors `scripts/03a-simulation-forced-choice.R`,
  with trial-level Bernoulli responses, a subject random intercept, four lme4
  mixed models, and separate summaries for acceptable and flagged fits.
- `02b-run-within-family.R`: mirrors `scripts/03b-simulation-within-family-links.R`.
  It retains aggregated binomial draws and the existing GLM at ICC = 0.
- `02c-run-sum-scores.R`: mirrors `scripts/03c-simulation-sum-scores.R`, including
  ordinal item draws, the continuity correction, and all three fitted models.
- `02d-run-diagnostics.R`: preserves the targeted AIC, DHARMa and Pregibon
  calculations. Forced-choice and within-family diagnostic data use individual
  binary trials and retain their manuscript random-intercept structures.
- `02-run-atlas.R`: optional four-line runner for the family scripts above.
- `03-summarize-atlas.R`: reads raw results and writes app-facing CSV/RDS
  summaries, including acceptable-fit and flagged-fit rates. It performs no
  model fitting.
- `raw/`: one ignored RDS file per scenario, run mode, and B.
- `Supplement-B-Simulation-atlas.qmd`: reads precomputed CSV summaries.

There is no `simulation-atlas/R/` helper framework. Parameters, seed arithmetic,
DGPs, fits, and interaction extraction are local to each family runner. The
diagnostic-grid file is created by the grid script; it has not been generated
during the static refactor.

## Settings

| Environment variable | Default / meaning |
| --- | --- |
| `ATLAS_MODE` | `full`; `smoke` selects one core anchor per family and two diagnostic anchors |
| `N_SIM` | **3000 for every full scenario**; 3 for smoke, unless explicitly overridden |
| `N_CORES` | Explicit worker override |
| `SLURM_CPUS_PER_TASK` | Worker count when `N_CORES` is unset |
| `ATLAS_OVERWRITE` | `FALSE`; skip any existing scenario file; `TRUE` recomputes it |
| `ATLAS_RUN_DHARMA` | `FALSE`; `TRUE` includes the DHARMa residual checks |
| `DHARMA_N_SIM` | 250 full / 25 smoke simulated datasets per DHARMa check |

There is no separate within-family B setting. The grid's `B` column records the
intended full design, 3000. Raw and summary `B_requested` records the actual run.
Use the **same `ATLAS_MODE` and `N_SIM` for execution and summarization**.
The summarizer does not infer a mode or a replication count from old files.

If neither core variable is set, workers default to `detectCores() - 1`, with a
minimum of one. Scenarios run sequentially; replications use `mclapply()` on
Linux and `parLapply()` on Windows. There is no nested parallelism. Workers use
one BLAS/OpenMP thread. Each family passes its scenario and deterministic
quantities directly to its replication function.

Seed arithmetic appears directly in the family scripts:

```text
seed = (20260807 + family_offset + stream_offset
        + scenario_number * 10000 + replication) modulo .Machine$integer.max
family_offset: forced choice 1000000; sum scores 2000000; within family 3000000
stream_offset: core 0; diagnostics 4000000
```

## Run from the repository root

Build the two declared grids first:

```bash
Rscript simulation-atlas/01-build-scenario-grid.R
```

Small smoke run, including the diagnostic interfaces:

```bash
export ATLAS_MODE=smoke N_SIM=3 N_CORES=2
export ATLAS_RUN_DHARMA=TRUE DHARMA_N_SIM=25 ATLAS_OVERWRITE=TRUE
Rscript simulation-atlas/02-run-atlas.R
Rscript simulation-atlas/03-summarize-atlas.R
```

Smoke summaries have `-smoke` suffixes and `run_type = "smoke"`.

Full Atlas, including diagnostics:

```bash
export ATLAS_MODE=full N_SIM=3000
export ATLAS_RUN_DHARMA=TRUE DHARMA_N_SIM=250
export ATLAS_OVERWRITE=FALSE
Rscript simulation-atlas/02a-run-forced-choice.R
Rscript simulation-atlas/02b-run-within-family.R
Rscript simulation-atlas/02c-run-sum-scores.R
Rscript simulation-atlas/02d-run-diagnostics.R
Rscript simulation-atlas/03-summarize-atlas.R
quarto render simulation-atlas/Supplement-B-Simulation-atlas.qmd
```

On SLURM, leave `N_CORES` unset to use `SLURM_CPUS_PER_TASK` and prefix the R
commands with `srun`. The existing `slurm/05-atlas.slurm` shows this sequence.
On PowerShell use `$env:ATLAS_MODE = "full"`, `$env:N_SIM = "3000"`, etc., then
run the same script paths.

## Restarting and precomputed results

Rerun a family script to skip existing scenario files. There is no completeness
or atomic-write framework. Delete an interrupted scenario file manually, or use
`ATLAS_OVERWRITE=TRUE`. For the first old-versus-new comparison, preserve the old
raw directory separately and regenerate all compared files: existing B=3000
files would otherwise be skipped even if produced by older code.

DHARMa-free runs still compute AIC and Pregibon and write
`diagnostic-nodharma-*.rds`. A later pass with `ATLAS_RUN_DHARMA=TRUE` writes
`diagnostic-*.rds`. The summarizer prefers a complete set of the latter filenames,
otherwise reads the DHARMa-free set. It does not inspect file completeness.
`computed = FALSE` and `applicable = FALSE` retain their distinct meanings.
Sum-score diagnostics remain structurally inapplicable.

Full summaries remain `data/atlas-summary.csv/.rds` and
`data/diagnostic-atlas-summary.csv/.rds`. Supplement B and the Shiny app consume
these files; neither executes Atlas simulations. Tracked summaries and rendered
artifacts were not recomputed during the refactor. Their historical B values
must not be mistaken for the new full-run specification.

The main diagnostic script and Atlas retain their existing differences in
Pregibon calculations and applicability. This refactor does not reconcile or
reinterpret those methods. See [the static audit](../DE-ENGINEERING.md).

Keep code, grids, and compact summaries in Git. Raw replications remain ignored
and can be archived with the existing project archive materials.

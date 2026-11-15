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
  mixed models (the chance-corrected one with `nAGQ = 10`), and separate
  summaries for acceptable and flagged fits.
- `02b-run-within-family.R`: mirrors `scripts/03b-simulation-within-family-links.R`.
  It retains aggregated binomial draws (an equivalent likelihood) and the existing
  GLM at ICC = 0, which has no manuscript counterpart.
- `02c-run-sum-scores.R`: mirrors `scripts/03c-simulation-sum-scores.R`, including
  ordinal item draws, the continuity correction, and all three fitted models.
- `02d-run-diagnostics.R`: AIC, DHARMa and Pregibon calculations defined as in
  `scripts/04-diagnostic-worked-example.R` (DHARMa defaults, `testCategorical`
  across the 2 x 2 design cells; Pregibon on the population-level linear
  predictor, refitted with the family, link and `nAGQ` of the diagnosed model,
  and not applicable in the saturated 2 x 2 design). As in the manuscript, the
  DHARMa and Pregibon checks are applied both to the wrong-link model (detection)
  and to the correct-link model (calibration); the chance-corrected GLMM uses
  `nAGQ = 10`. Forced-choice and within-family diagnostic data use individual
  binary trials and retain their manuscript random-intercept structures. The
  grid also covers the manuscript's Poisson count (log generated, identity
  fitted), binary continuous-predictor (probit GLMM generated, logit fitted)
  and Gamma (log generated, inverse fitted) diagnostic scenarios, with the
  manuscript DGPs and GLM/GLMM fits; non-converged GLMs count as failed fits.
- `03-summarize-atlas.R`: reads raw results and writes app-facing CSV/RDS
  summaries. As in the manuscript, forced-choice primary rates use fits passing
  the minimal lme4 checks, with flagged fits summarized separately; within-family
  and sum-score rates use every finite p-value (convergence flags are recorded
  but do not filter). Diagnostic rows report the wrong-link detection rate and,
  in the `correct_link_*` columns, the correct-link calibration rate. It performs
  no model fitting.
- `raw/`: one ignored RDS file per scenario, run mode, and B.
- `Supplement-B-Simulation-atlas-explorer.qmd`: **Supplement B**. Interactive
  tables, one self-contained HTML file (`embed-resources`, `reactable`), usable
  as a paper attachment. It reads the precomputed summaries and reports the
  manuscript rates, one row per generating scenario with the fitted models as
  columns (forced choice: each rate with the share of flagged, excluded fits
  in brackets), plus the diagnostic atlas in the layout of the manuscript's
  diagnostic table. A short introduction, no figures or appendix.
- `archive/`: the earlier full report (text, figures, numerical appendix), no
  longer Supplement B and not maintained; see `archive/README.md`.

There is no `simulation-atlas/R/` helper framework. Parameters, seed arithmetic,
DGPs, fits, and interaction extraction are local to each family runner.

## Settings

| Environment variable | Default / meaning |
| --- | --- |
| `ATLAS_MODE` | `full`; `smoke` selects scenario 002 of each core family (FC, SS, WF) and of each diagnostic case (D-FC, D-WF, D-CT, D-BC, D-GA) |
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
minimum of one. Scenarios run sequentially; replications run on a PSOCK cluster
(`makeCluster()` + `parLapply()`) on every platform. There is no nested parallelism. Workers use
one BLAS/OpenMP thread. Each family passes its scenario and deterministic
quantities directly to its replication function.

Seed arithmetic appears directly in the family scripts:

```text
seed = (20260807 + family_offset + stream_offset
        + scenario_number * 10000 + replication) modulo .Machine$integer.max
family_offset: forced choice 1000000; sum scores 2000000; within family 3000000;
               count 5000000; binary continuous 6000000; Gamma 7000000 (diagnostics only)
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
Rscript simulation-atlas/02a-run-forced-choice.R
Rscript simulation-atlas/02b-run-within-family.R
Rscript simulation-atlas/02c-run-sum-scores.R
Rscript simulation-atlas/02d-run-diagnostics.R
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
quarto render simulation-atlas/Supplement-B-Simulation-atlas-explorer.qmd
```

To run the grid, all four families, and the summaries in separate R processes
with the same environment settings, use `sh run-atlas.sh` from the repository
root. This wrapper defaults to the full SLURM settings (`ATLAS_RUN_DHARMA=TRUE`,
`ATLAS_OVERWRITE=TRUE`); explicitly set either variable to `FALSE` to override
it. Render Supplement B afterward with the `quarto render` command above.

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
these files; neither executes Atlas simulations. Regenerate tracked summaries
and rendered artifacts when the scenario grid or simulation code changes.

The diagnostic definitions (checks, applicability, Pregibon refit, correct-link
calibration) match `scripts/04-diagnostic-worked-example.R`. The Atlas covers
all five manuscript diagnostic scenarios; in each case the rows vary sample size
and severity, and the `D-*-002` row belongs to both sweeps. Severity is the
intercept for the chance floor (`D-FC`) and the two binary repeated-trial cases
(`D-WF`, `D-BC`, reference intercept), and a common multiplier of the two main
effects for count (`D-CT`) and Gamma (`D-GA`): there the intercept only rescales
the means (for Gamma with inverse link it leaves the fit unchanged), whereas the
main effects set the curvature over the observed range.

Keep code, grids, and compact summaries in Git. Raw replications remain ignored
and can be archived with the existing project archive materials.

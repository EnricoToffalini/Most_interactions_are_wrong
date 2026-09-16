# De-engineering audit

Baseline `main`: `70dfefa4d811c9e12a198126bc26dafd2f9d7c42`.

All edits are on `de-engineerized`. No R scripts, R expressions, Quarto,
simulations, tests, package installation, Shiny, or rendering were executed.
Review is by reading source, tracing formulas and interfaces, and inspecting
Git diffs. Numerical equivalence has not been established.

## Scope and file structure

All eight substantive `scripts/00-...` through `scripts/04-...` files now carry
their own paths, settings and required scientific calculations. `run.R` is a
short ordered list of script calls. The three simulation scripts contain their
DGP, explicit model fits, interaction extraction, parallel replication call,
summaries, plots and saves. The forced-choice likelihood is visible locally.

The Atlas now consists of:

```text
simulation-atlas/
  01-build-scenario-grid.R
  02a-run-forced-choice.R
  02b-run-within-family.R
  02c-run-sum-scores.R
  02d-run-diagnostics.R
  02-run-atlas.R              # optional ordered script calls
  03-summarize-atlas.R
  data/scenario-grid.csv
  data/diagnostic-grid.csv    # created when the user runs the grid script
  Supplement-B-Simulation-atlas.qmd
```

Each core family repeats its manuscript counterpart's mathematics and fits,
with an outer scenario loop. Within-family Atlas data retain the existing
aggregated binomial draws and GLM at ICC zero; the manuscript retains individual
Bernoulli trials. Sum scores retain the ordinal thresholds, shared item uniform
draws and continuity correction. The explicit grid preserves the 93 existing
core rows, IDs, order, parameter combinations and slice memberships. Diagnostic
scenarios are declared separately. No source-code anchor parsing remains.

## Removed and retained infrastructure

Deleted all five manuscript helper files:

- `R/project-settings.R`
- `R/utils-reporting.R`
- `R/utils-link-functions.R`
- `R/utils-summaries.R`
- `R/utils-plots.R`

Deleted all five Atlas helper files:

- `simulation-atlas/R/atlas-common.R`
- `simulation-atlas/R/atlas-forced-choice.R`
- `simulation-atlas/R/atlas-within-family.R`
- `simulation-atlas/R/atlas-sum-scores.R`
- `simulation-atlas/R/atlas-diagnostics.R`

No scientific helper file remains. Small repeated mathematical transformations,
Wilson intervals and some local summary callbacks remain. Long replication
functions contain the actual calculations and are passed directly to parallel
iteration. Generic family dispatch, fit/extraction wrappers, warning capture,
row factories, signatures, deduplication, source parsing, atomic writes and
raw-file completeness machinery were removed. Restarting uses file existence.

The main diagnostic script retains a local chance-model fitter and nine S3
methods because DHARMa uses that interface. This is the principal remaining
engineered section. Its original five-start optimization, BFGS/Nelder-Mead
strategy and polishing were preserved. Core chance fits use simple lists,
without S3 wrappers. Shiny UI helpers were outside the refactor's scope.

## Statistical checks and deliberate corrections

Core chance fits visibly calculate the chance-floor binomial likelihood,
analytic gradient, three starts, BFGS optimization, symmetrized Hessian,
eigenvalues, inverse covariance, SEs, Wald z and p-values. Usability still
requires optimizer convergence, positive and sufficiently conditioned Hessian,
finite covariance, positive variances and finite inference. Failed fits yield
NA; finite estimates remain available where the existing schema requires them.
Mixed-model convergence/Hessian checks and diagnostic applicability rules remain.

Corrections that can affect output, or resolve a contradiction:

1. The old main chance helper used `stats::cbind` inside a guarded GLM start.
   `cbind` belongs to base R. Using `cbind` restores that intended start and may
   change optimization outcomes compared with the old silently failed start.
2. Atlas chance usability now matches the manuscript's Hessian eigenvalue and
   conditioning rules and treatment of nonpositive variances. The old Atlas
   rule was weaker. Successful-fit counts and inference can consequently change.
3. Every full Atlas scenario now uses B = 3000, replacing old 500/300 defaults
   and grid metadata. Main full defaults remain 3000. Old generated summaries
   have not been recomputed or relabeled.
4. Seed-rule metadata now includes the already-used family offset; Atlas seed
   arithmetic itself is retained explicitly in each runner.
5. Documentation and script headings now correctly identify 03b as within-family
   (Simulation 2) and 03c as sum scores (Simulation 3).

Main and Atlas diagnostics retain their original differences: the main GLMM
Pregibon predictor excludes random effects and retains its rank check; Atlas
retains its conditional predictor and original start construction. No scientific
reinterpretation or methodological revision was attempted.

## Parallelism and RNG

All expensive Monte Carlo components retain parallel replication execution.
Worker count uses `N_CORES`, then `SLURM_CPUS_PER_TASK`, then detected cores minus
one (minimum one). Scenarios run sequentially. Unix uses `mclapply`; Windows uses
PSOCK `parLapply` with explicit dependencies. OpenMP/OpenBLAS/MKL threads are
limited to one before loading packages. No nested parallelism was introduced.

Main seeds remain 20260525 (forced choice), 20260528 (within-family), 20260526
(sum scores), and 20260608 (diagnostics), with the existing Unix fork RNG calls.
Windows parallel workers use `clusterSetRNGStream`: they are not expected to
match the former Windows serial draws. Atlas sets a deterministic seed inside
each replication using base 20260807, family offsets 1000000/2000000/3000000,
diagnostic offset 4000000, scenario suffix times 10000, and replication number.
There is no claim of bitwise equivalence across core counts or platforms for
the main scripts.

## Static review and limits

Review covered every changed substantive script: variable definitions, package
namespaces, paths and directory creation, DGP equations and parameter values,
model formulas and random effects, deterministic quantities, interaction rows,
seed arithmetic, worker dependencies, summaries, writes and downstream schemas.
Deleted-helper references were searched. Git whitespace checks were used.
The manuscript's two plot-theme references were expanded to their existing
theme definitions. Supplement B still reads the same precomputed summaries;
its analysis/rendering code was not redesigned.

This is source inspection, not R parsing or execution. Runtime syntax, package
compatibility, Windows worker serialization, convergence behavior, numerical
equivalence, plotted appearance and successful manuscript/app consumption remain
unverified. In particular, the two chance-fit corrections above preclude a
blanket assertion of identical results. The diagnostic-grid output does not yet
exist: running its declaration script is a required first Atlas step. Existing
tables, figures, raw results and rendered documents remain historical artifacts.

## Manual validation sequence (not executed)

Run from the repository root, in a separate copy with the baseline outputs
preserved. Main smoke runs write to the usual output paths. Provision the
documented R packages and rendering environment yourself before these commands.
The following examples use a Linux shell; in PowerShell set each variable with
`$env:N_SIM = "3"` (and similarly for the others), then use the same script paths.

### 1. Small smoke runs

```bash
export N_SIM=3 N_CORES=2 DHARMA_N_SIM=25
Rscript scripts/00-sandbox-one-scenario.R
Rscript run.R
Rscript simulation-atlas/01-build-scenario-grid.R
export ATLAS_MODE=smoke ATLAS_RUN_DHARMA=TRUE ATLAS_OVERWRITE=TRUE
Rscript simulation-atlas/02-run-atlas.R
Rscript simulation-atlas/03-summarize-atlas.R
```

Inspect all outputs and error messages. Repeat the Atlas smoke with N_CORES=1
after preserving the first raw files, and compare seeded replication results.
Check Windows and Linux workers if both platforms will be used. Do not interpret
three-replication rejection rates as scientific evidence.

### 2. Main full analyses

```bash
export N_SIM=3000 N_CORES=4 DHARMA_N_SIM=250
Rscript run.R
```

Alternatively run the main simulations individually, after the review/figures:

```bash
Rscript scripts/03a-simulation-forced-choice.R
Rscript scripts/03b-simulation-within-family-links.R
Rscript scripts/03c-simulation-sum-scores.R
Rscript scripts/04-diagnostic-worked-example.R
```

### 3. Full Atlas and aggregation

Preserve old raw files before the first deliberate regeneration. TRUE below
ensures that old files with the same B are not mistaken for refactored output.
Use FALSE for subsequent restarts; delete an interrupted file manually.

```bash
export ATLAS_MODE=full N_SIM=3000
export ATLAS_RUN_DHARMA=TRUE DHARMA_N_SIM=250 ATLAS_OVERWRITE=TRUE
Rscript simulation-atlas/02a-run-forced-choice.R
Rscript simulation-atlas/02b-run-within-family.R
Rscript simulation-atlas/02c-run-sum-scores.R
Rscript simulation-atlas/02d-run-diagnostics.R
Rscript simulation-atlas/03-summarize-atlas.R
```

On SLURM, unset N_CORES to use SLURM_CPUS_PER_TASK and run the scripts with srun
inside your allocation. Keep ATLAS_MODE and N_SIM unchanged for summarization.

### 4. Render consumers

```bash
quarto render paper/paper-v2.qmd
quarto render paper/Supplement-A-Technical-details.qmd
quarto render simulation-atlas/Supplement-B-Simulation-atlas.qmd
```

Then inspect the Shiny app against the newly generated compact summaries.
Supplement B reads precomputed outputs and does not execute the Atlas.

### 5. Old-versus-new comparisons

- Grid IDs, ordering, scientific columns and slice membership; only intended
  B and seed-description metadata should differ.
- Deterministic cell means, expected sum scores and interaction contrasts.
- Illustrative datasets and fitted coefficients under matched RNG conditions.
- Raw column names/types, model labels, coefficient/SE/p-value extraction and
  contrast orientation; investigate chance-fit differences described above.
- Successful-fit denominators, common-convergence subsets, rejection rates,
  Wilson intervals, and requested/actual replication metadata.
- Diagnostic AIC, DHARMa, Pregibon, applicability and computed flags.
- Figure labels/layouts and manuscript, Supplement B and Shiny table ingestion.

Compare like-for-like B and execution settings. Historical 500/300-replication
Atlas estimates are not an exact numerical reference for a new B=3000 run.

**NO R / QUARTO / SIMULATION CODE WAS EXECUTED.**

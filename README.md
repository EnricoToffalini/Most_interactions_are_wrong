# The Link Function Problem in Psychological Interaction Testing

This repository contains the manuscript, review materials, simulation code, and reproducible outputs for the project:

**The Link Function Problem in Psychological Interaction Testing**

The project examines how interaction claims in psychology depend on the scale on which additivity is assumed. In generalized linear models, this scale is defined by the link function. The central point is not only that some outcomes require GLMs rather than linear models. It is that even within a broadly appropriate outcome family, different plausible links can define different no-interaction baselines and can therefore change interaction conclusions.

## Project overview

The paper has three main components.

1. **Descriptive review of current practice**
   We summarize how often recent psychological articles test interactions, how often outcome family and link functions are made explicit, and how often interaction tests are applied to outcomes for which identity-link additivity is not self-evident.

2. **Conceptual framework**
   We distinguish:
   - wrong-family problems, where the outcome model does not respect the response type;
   - wrong-link problems, where the family may be plausible but the link defines an inadequate scale of additivity;
   - measurement-metric problems, where the observed score may not be the scale on which the theoretical interaction claim is meant to hold.

3. **Worked examples and simulations**
   The simulations show how plausible but inadequate links can generate pseudo-interactions when the data-generating process contains no product term on the known generating scale. Matched-scale conditions report nominal rejection rates, mismatched-scale conditions report pseudo-interaction detection rates, and displays combining both use neutral product-term rejection rates. The main cases are:
   - forced-choice accuracy with a non-zero chance floor;
   - bounded and discrete sum scores;
   - within-family link choices, especially logit versus probit;
   - diagnostic checks for wrong-link problems.

## Reproducibility

Run every command from the repository root. The analysis scripts contain their
own settings, DGPs, model fits, summaries, and plotting code. Package requirements
are explicit: `ggplot2`, `readxl` (review), `glmmTMB` (mixed models), and `DHARMa`
(diagnostics). Provision the package environment separately; `run.R` neither
installs packages nor restores `renv`.

```bash
Rscript run.R
quarto render paper/paper-v2.qmd
quarto render paper/Supplement-A-Technical-details.qmd
```

`run.R` runs the review, two figures, three main simulations, and diagnostics in
order. The optional `scripts/00-sandbox-one-scenario.R` is run separately.

The refactor was reviewed statically. **No R / Quarto / simulation code was
executed during the refactor.** Existing generated results are historical and
have not been recomputed or relabeled as new 3000-replication results. See
[DE-ENGINEERING.md](DE-ENGINEERING.md) for corrections, limits, and the complete
manual validation sequence.

## Interactive simulation atlas

The manuscript results produced by `scripts/` remain the primary reproduction
pipeline. `simulation-atlas/` provides broader sensitivity analyses around the
manuscript scenarios, while `shiny-app/` remains the interactive explorer and
reads only the atlas's precomputed compact summaries. Full replication-level
atlas results may be archived on OSF; see
[`simulation-atlas/README.md`](simulation-atlas/README.md) for the separate
offline workflow.

## Main scripts

- `01-review-descriptives.R`
  Produces descriptive summaries of the preregistered review.

- `02a-figure-motivating-example.R`
  Generates the motivating example showing how different links imply different interaction conclusions.

- `02b-figure-logit-probit-fitted-example.R`
  Generates the fitted logit-versus-probit example used for the within-family link discussion.

- `03a-simulation-forced-choice.R`
  Simulates forced-choice accuracy data with a non-zero chance floor and compares standard and chance-corrected links.

- `03c-simulation-sum-scores.R`
  Simulates bounded, discrete sum scores from an underlying latent scale and compares manifest-score and alternative analyses.

- `03b-simulation-within-family-links.R`
  Examines how logit and probit links can differ for interaction claims even within the binomial family.

- `04-diagnostic-worked-example.R`
  Compares pseudo-interaction detection rates with diagnostic detection rates and same-formula AIC comparisons under deliberately wrong-link fits.

## Computational settings and individual runs

**FULL means 3000 Monte Carlo replications per scenario**, for forced choice,
within-family logit/probit, sum scores, and all corresponding Atlas scenarios.
The default is `B <- as.integer(Sys.getenv("N_SIM", "3000"))` in each main
simulation. `N_SIM` is an explicit override for smoke/debug runs; small values
are not publication results. Main-script alpha remains .05 by default (`ALPHA`
can override it); Atlas alpha remains .05.

```bash
# Individual full main simulations
N_SIM=3000 N_CORES=4 Rscript scripts/03a-simulation-forced-choice.R
N_SIM=3000 N_CORES=4 Rscript scripts/03b-simulation-within-family-links.R
N_SIM=3000 N_CORES=4 Rscript scripts/03c-simulation-sum-scores.R
N_SIM=3000 N_CORES=4 Rscript scripts/04-diagnostic-worked-example.R

# Very small check; writes to the usual main-output paths
N_SIM=3 N_CORES=2 DHARMA_N_SIM=25 Rscript run.R
```

Worker count is read in this order: **`N_CORES`**, then
**`SLURM_CPUS_PER_TASK`**, then `detectCores() - 1` (at least one).
Scenarios run in sequence; replications run in parallel. Linux uses
`parallel::mclapply()`; Windows uses a PSOCK cluster with explicit worker
dependencies. Set `N_CORES=1` for a serial comparison. BLAS/OpenMP worker threads
are limited to one. No RStudio session or pre-existing workspace is required.

The main scripts retain their original seeds and Linux fork RNG calls. New
Windows parallel runs use `clusterSetRNGStream()` with the script seed; they
are not expected to reproduce the old Windows serial draws. Atlas seeds are
explicit per scenario and replication, independent of worker scheduling.

For example, inside a SLURM allocation, from the repository root:

```bash
export N_SIM=3000
# Leave N_CORES unset to use SLURM_CPUS_PER_TASK.
srun Rscript --vanilla scripts/03b-simulation-within-family-links.R
```

On PowerShell, set variables before running the same script names:

```powershell
$env:N_SIM = "3"
$env:N_CORES = "2"
$env:DHARMA_N_SIM = "25"
Rscript run.R
```

The [Atlas README](simulation-atlas/README.md) gives the grid, family runner,
summarization, restart, and Supplement B commands. Supplement B reads
precomputed summaries and does not run simulations.

## Data

The review dataset is stored in:

```text
Literature_review/final-dataset-review.csv
```

The coding focuses on whether articles test interactions, whether non-identity links are used, whether link functions are explicit, and which outcome types are analyzed. The review is descriptive and should not be read as an audit of whether individual articles were wrong.

## Citation

A formal citation will be added after the manuscript is accepted or posted as a preprint.

For now, please cite the repository as:

```text
Calderan, M., Gambarota, F., Sità, L., Feraco, T., & Toffalini, E.
The Link Function Problem in Psychological Interaction Testing.
GitHub repository.
```

## License

This repository uses a dual-license structure.

- **Manuscript, figures, tables, and review data** are released under the Creative Commons Attribution 4.0 International License (**CC BY 4.0**), unless otherwise stated. This means that these materials may be shared and adapted, including for research and teaching, provided that appropriate credit is given.
- **Code** is released under the MIT License, unless otherwise stated. This means that the code may be reused, modified, and redistributed, provided that the original copyright and license notice are retained.

Third-party materials cited in the manuscript or used only as references remain under their original copyright and are not covered by the repository license.

Suggested attribution:

```text
Calderan, M., Gambarota, F., Sità, L., Feraco, T., & Toffalini, E.
The Link Function Problem in Psychological Interaction Testing.
GitHub repository.
```

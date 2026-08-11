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

The analyses are written in R. The manuscript is written in Quarto.

To reproduce the main outputs, run the scripts from the root of the repository:

```r
source("run.R")
```

Then render the manuscript:

```bash
quarto render paper/paper-v2.qmd
```

If the repository includes an `renv.lock` file, restore the package environment before running the analyses:

```r
renv::restore()
```

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

- `03b-simulation-sum-scores.R`  
  Simulates bounded, discrete sum scores from an underlying latent scale and compares manifest-score and alternative analyses.

- `03c-simulation-within-family-links.R`  
  Examines how logit and probit links can differ for interaction claims even within the binomial family.

- `04-diagnostic-worked-example.R`  
  Compares pseudo-interaction detection rates with diagnostic detection rates and same-formula AIC comparisons under deliberately wrong-link fits.

## Computational notes

Simulation settings are defined inside each script. Many scripts use environment variables to control the number of replications and cores. For example:

```bash
N_SIM=1000 N_CORES=4 Rscript scripts/03a-simulation-forced-choice.R
```

For quick checks, use smaller values of `N_SIM`. For manuscript results, use the values reported in the paper or supplement.

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

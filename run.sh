#!/bin/sh
# Run from the repository root. Set N_SIM / N_CORES in the environment first.
# FULL defaults to 3000 replications per scenario. Does not install packages.

set -e

Rscript scripts/01-review-descriptives.R
Rscript scripts/02a-figure-motivating-example.R
Rscript scripts/02b-figure-logit-probit-fitted-example.R
Rscript scripts/03a-simulation-forced-choice.R
Rscript scripts/03b-simulation-within-family-links.R
Rscript scripts/03c-simulation-sum-scores.R
Rscript scripts/04-diagnostic-worked-example.R

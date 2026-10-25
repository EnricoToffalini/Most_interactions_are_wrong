#!/bin/sh
# Run from the repository root. Set N_SIM / N_CORES in the environment first.
# FULL defaults to 3000 replications per scenario. Does not install packages.

set -e

Rscript --vanilla scripts/01-review-descriptives.R
Rscript --vanilla scripts/02a-figure-motivating-example.R
Rscript --vanilla scripts/02b-figure-logit-probit-fitted-example.R
Rscript --vanilla scripts/03a-simulation-forced-choice.R
Rscript --vanilla scripts/03b-simulation-within-family-links.R
Rscript --vanilla scripts/03c-simulation-sum-scores.R
Rscript --vanilla scripts/04-diagnostic-worked-example.R

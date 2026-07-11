# R/project-settings.R
# Project-level defaults for the link-function simulations.
#
# Keep this file limited to technical defaults that are genuinely shared
# across scripts. Scenario-specific values, such as age values for a table,
# x values for contrasts, plot ranges, thresholds, and candidate links,
# should live in the script that defines that simulation or figure.

# Monte Carlo defaults. These can be overridden from the shell, for example:
#   N_SIM=1000 Rscript scripts/03-simulation-forced-choice.R
#   ALPHA=0.01 Rscript scripts/03-simulation-forced-choice.R
default_B <- as.integer(Sys.getenv("N_SIM", "300"))
default_alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))

# Shared output defaults.
default_dpi <- 300
figure_width <- 7.2
figure_height <- 7.0

# Shared output directories.
# These are technical project paths, not simulation parameters.
project_output_dirs <- c(
  "tables",
  "figs",
  "outputs",
  "outputs/inspection"
)

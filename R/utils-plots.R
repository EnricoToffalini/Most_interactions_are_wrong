# R/utils-plots.R
# Plot helpers. Uses only ggplot2 and base grid.

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2.", call. = FALSE)
library(ggplot2)

link_theme <- function(base_size = 10) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

bin_midpoints <- function(x) {
  lev <- levels(x)
  mids <- vapply(strsplit(gsub("\\[|\\]|\\(|\\)", "", lev), ","), function(z) mean(as.numeric(z)), numeric(1))
  mids[as.integer(x)]
}

axis_title_group_gap_correct <- function(k_trials) {
  paste0("Group gap (Group 1 - Group 0), correct responses out of ", k_trials)
}

axis_title_change_group_gap_correct <- function(low, high, k_trials) {
  paste0("Change in group gap from ", low, " to ", high, ", correct responses out of ", k_trials)
}

axis_title_group_gap_sum_score <- function(max_score) {
  paste0("Group gap (Group 1 - Group 0), sum-score units on the 0-", max_score, " scale")
}

axis_title_change_group_gap_sum_score <- function(low, high, max_score) {
  paste0("Change in group gap from ", low, " to ", high, ", sum-score units on the 0-", max_score, " scale")
}

axis_title_group_gap_probability <- function() {
  "Group gap (Group 1 - Group 0), probability points"
}

axis_title_change_group_gap_probability <- function(low, high) {
  paste0("Change in group gap from ", low, " to ", high, ", probability points")
}

save_single_plot <- function(plot, filename_base, width = 7.2, height = 5, dpi = 300) {
  dir.create(dirname(filename_base), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(paste0(filename_base, ".pdf"), plot = plot, width = width, height = height)
  ggplot2::ggsave(paste0(filename_base, ".png"), plot = plot, width = width, height = height, dpi = dpi)
}

save_plot_grid <- function(plots, filename_base, width = 7.2, height = 7, ncol = 1, dpi = 300) {
  dir.create(dirname(filename_base), recursive = TRUE, showWarnings = FALSE)
  n <- length(plots)
  nrow <- ceiling(n / ncol)
  draw <- function() {
    grid::grid.newpage()
    vp <- grid::viewport(layout = grid::grid.layout(nrow, ncol))
    grid::pushViewport(vp)
    for (i in seq_along(plots)) {
      r <- ceiling(i / ncol)
      c <- i - (r - 1) * ncol
      print(plots[[i]], vp = grid::viewport(layout.pos.row = r, layout.pos.col = c))
    }
    grid::popViewport()
  }
  grDevices::pdf(paste0(filename_base, ".pdf"), width = width, height = height)
  draw()
  grDevices::dev.off()
  grDevices::png(paste0(filename_base, ".png"), width = width, height = height, units = "in", res = dpi)
  draw()
  grDevices::dev.off()
}

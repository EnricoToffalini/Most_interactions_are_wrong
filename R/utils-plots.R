# R/utils-plots.R
# Plot helpers. Uses ggplot2 and base grid only.

# ---------------------------------------------------------------------
# Shared visual system
# ---------------------------------------------------------------------

link_palette <- function() {
  c(
    "Group 0" = "#0072B2",
    "Group 1" = "#D55E00",
    "Identity" = "#009E73",
    "Gaussian identity" = "#009E73",
    "Standard logit" = "#0072B2",
    "Standard probit" = "#CC79A7",
    "Standard binomial logit" = "#0072B2",
    "Chance-corrected logit" = "#D55E00",
    "Chance-corrected binomial" = "#D55E00",
    "Chance-corrected binomial link" = "#D55E00",
    "Observed data" = "grey30",
    "Generating model" = "black"
  )
}

link_linetypes <- function() {
  c(
    "Group 0" = "solid",
    "Group 1" = "longdash",
    "Identity" = "solid",
    "Gaussian identity" = "solid",
    "Standard logit" = "solid",
    "Standard probit" = "dotdash",
    "Standard binomial logit" = "solid",
    "Chance-corrected logit" = "longdash",
    "Chance-corrected binomial" = "longdash",
    "Chance-corrected binomial link" = "longdash",
    "Observed data" = "blank",
    "Generating model" = "solid"
  )
}

link_shapes <- function() {
  c(
    "Group 0" = 16,
    "Group 1" = 17,
    "Identity" = 16,
    "Gaussian identity" = 16,
    "Standard logit" = 15,
    "Standard probit" = 18,
    "Standard binomial logit" = 15,
    "Chance-corrected logit" = 17,
    "Chance-corrected binomial" = 17,
    "Chance-corrected binomial link" = 17,
    "Observed data" = 16,
    "Generating model" = 1
  )
}

link_scale_color_discrete <- function(..., values = link_palette()) {
  ggplot2::scale_color_manual(values = values, ...)
}

link_scale_linetype_discrete <- function(..., values = link_linetypes()) {
  ggplot2::scale_linetype_manual(values = values, ...)
}

link_scale_shape_discrete <- function(..., values = link_shapes()) {
  ggplot2::scale_shape_manual(values = values, ...)
}

percent_labels <- function(accuracy = 1) {
  force(accuracy)
  function(x) paste0(round(100 * x / accuracy) * accuracy, "%")
}

link_theme <- function(base_size = 10, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 1,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1,
        color = "grey25",
        margin = ggplot2::margin(b = 6)
      ),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "grey20"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.width = grid::unit(1.25, "lines"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey88"),
      panel.spacing = grid::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(6, 8, 6, 8)
    )
}

# ---------------------------------------------------------------------
# Existing helpers retained
# ---------------------------------------------------------------------

bin_midpoints <- function(x) {
  lev <- levels(x)
  clean <- gsub("\\[|\\]|\\(|\\)", "", lev)
  mids <- vapply(
    strsplit(clean, ","),
    function(z) mean(as.numeric(z)),
    numeric(1)
  )
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
  
  ggplot2::ggsave(
    paste0(filename_base, ".pdf"),
    plot = plot,
    width = width,
    height = height
  )
  
  ggplot2::ggsave(
    paste0(filename_base, ".png"),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  invisible(filename_base)
}

save_plot_grid <- function(plots, filename_base, width = 7.2, height = 7, ncol = 1, dpi = 300) {
  dir.create(dirname(filename_base), recursive = TRUE, showWarnings = FALSE)

  n <- length(plots)
  nrow <- ceiling(n / ncol)
  
  draw <- function() {
    grid::grid.newpage()
    vp <- grid::viewport(layout = grid::grid.layout(nrow, ncol))
    grid::pushViewport(vp)
    on.exit(grid::popViewport(), add = TRUE)
    
    for (i in seq_along(plots)) {
      r <- ceiling(i / ncol)
      c <- i - (r - 1) * ncol
      print(
        plots[[i]],
        vp = grid::viewport(layout.pos.row = r, layout.pos.col = c)
      )
    }
  }
  
  grDevices::pdf(paste0(filename_base, ".pdf"), width = width, height = height)
  draw()
  grDevices::dev.off()
  
  grDevices::png(
    paste0(filename_base, ".png"),
    width = width,
    height = height,
    units = "in",
    res = dpi
  )
  draw()
  grDevices::dev.off()
  
  invisible(filename_base)
}

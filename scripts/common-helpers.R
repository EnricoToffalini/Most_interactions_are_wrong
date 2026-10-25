theme_paper <- function(size) {
  theme_minimal(base_size = size) +
    theme(plot.title = element_text(face = "bold", size = size + 1, margin = margin(b = 3)),
          plot.subtitle = element_text(size = size - 1, color = "grey25", margin = margin(b = 6)),
          axis.title = element_text(size = size),
          axis.text = element_text(size = size - 1, color = "grey20"),
          strip.text = element_text(face = "bold", size = size - 1),
          legend.position = "bottom",
          legend.title = element_text(size = size - 1),
          legend.text = element_text(size = size - 1),
          legend.key.width = unit(1.25, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
          panel.spacing = unit(0.9, "lines"),
          plot.margin = margin(6, 8, 6, 8))
}

wilson_ci95 <- function(x, n) {
  if (n <= 0) return(c(NA_real_, NA_real_))
  z <- qnorm(0.975)
  p <- x / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  c(center - half, center + half)
}

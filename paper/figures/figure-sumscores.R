library(ggplot2)

set.seed(0)

# ----------------------------
# SETTINGS
# ----------------------------
N = 200000
J = 9  # 9 items, 4-point Likert (ordinal via thresholds)

# Latent trait (true z-score scale)
z = rnorm(N, 0, 1)

# Four broad color bands for reference only
band = cut(
  z,
  breaks = c(-Inf, -1, 0, 1, Inf),
  labels = c("< -1", "-1 to 0", "0 to +1", "> +1"),
  right = FALSE
)
band = factor(band, levels = c("< -1", "-1 to 0", "0 to +1", "> +1"))

pal4 = c(
  "< -1"    = "#22CC66",
  "-1 to 0" = "#FFFFFF",
  "0 to +1" = "#FFBB55",
  "> +1"    = "#EE5511"
)

# Random item thresholds (3 per item -> 4 categories), then shift them
cuts0 = t(replicate(J, sort( seq(-1.3, 1.3, length.out=3)  )))  # J x 3

shift_floor = 1.4  # larger = more floor distortion
shift_mid   = -0.7  # smaller = less distortion

cuts_floor = cuts0 + shift_floor
cuts_mid   = cuts0 + shift_mid

# ----------------------------
# PLOT 1: latent Gaussian with shaded regions + cut lines
# ----------------------------
z_grid = seq(-3.5, 3.5, length.out = 3000)
dens   = dnorm(z_grid)

band_grid = cut(
  z_grid,
  breaks = c(-Inf, -1, 0, 1, Inf),
  labels = c("< -1", "-1 to 0", "0 to +1", "> +1"),
  right = FALSE
)
band_grid = factor(band_grid, levels = levels(band))

df_lat = data.frame(z = z_grid, dens = dens, band = band_grid)

p_latent = ggplot(df_lat, aes(x = z, y = dens)) +
  geom_text(data=data.frame(),aes(x = c(-1,0,1), y = Inf, label = c("z = -1", "z = 0", "z = +1")),
    vjust = 1.2, size = 8  ) +
  geom_area(aes(fill = band), color = NA) +
  geom_line(color = "black", linewidth = 1.3) +
  geom_segment(data = data.frame(),
    aes(x = c(-1,0,1), xend = c(-1,0,1), y = 0, yend = 0.45),
    linetype = 2, linewidth = 1.8  ) +
  scale_fill_manual(values = pal4, guide = "none") +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  geom_point( data = data.frame(), aes(x = c(-1,0,1), y = 0),
    shape = 17, size = 7 ) +
  labs(x = "Underlying normally-distributed true scores", y = "Density", title = "A)") +
  theme_minimal(base_size = 22) +
  theme(axis.text.y = element_blank(),
        text=element_text(size=30)); p_latent

# ----------------------------
# SIMULATION: sum scores under a given threshold matrix
# ----------------------------
simulate_sumscore = function(cuts_mat) {
  sumscore = rep.int(0L, N)
  for (j in 1:J) {
    ystar = z + rnorm(N,0,1)  # probit-style latent response
    sumscore = sumscore +
      1L +
      (ystar > cuts_mat[j, 1]) +
      (ystar > cuts_mat[j, 2]) +
      (ystar > cuts_mat[j, 3])
  }
  sumscore
}

sum_floor = simulate_sumscore(cuts_floor)
sum_mid   = simulate_sumscore(cuts_mid)

# Data frames with the 4-band coloring (no legend)
df_floor = data.frame(SumScore = sum_floor, band = band)
df_mid2  = data.frame(SumScore = sum_mid,   band = band)

# Stacked-bar data with GLOBAL density (avoids the "rare bands inflated" artifact)
make_counts = function(d) {
  out = as.data.frame(table(SumScore = d$SumScore, band = d$band))
  out$SumScore = as.numeric(as.character(out$SumScore))
  out$density = out$Freq / N  # binwidth = 1
  out
}

dfc_floor = make_counts(df_floor)
dfc_mid   = make_counts(df_mid2)

# ----------------------------
# Map z = -1, 0, +1 to expected sum score E(S | z) analytically
# ----------------------------
Ez_to_Esum = function(z0, cuts_mat) {
  out = 0
  for (j in 1:J) {
    t1 = cuts_mat[j, 1]; t2 = cuts_mat[j, 2]; t3 = cuts_mat[j, 3]
    # Probabilities for ordinal categories given z0 in probit model
    p1 = pnorm(t1 - z0)
    p2 = pnorm(t2 - z0) - pnorm(t1 - z0)
    p3 = pnorm(t3 - z0) - pnorm(t2 - z0)
    p4 = 1 - pnorm(t3 - z0)
    out = out + (1*p1 + 2*p2 + 3*p3 + 4*p4)
  }
  out
}

z_ref = c(-1, 0, 1)

E_floor = sapply(z_ref, Ez_to_Esum, cuts_mat = cuts_floor)
E_mid   = sapply(z_ref, Ez_to_Esum, cuts_mat = cuts_mid)

df_ref_floor = data.frame(SumScore = E_floor, lab = c("z = -1", "z = 0", "z = +1"))
df_ref_mid   = data.frame(SumScore = E_mid,   lab = c("z = -1", "z = 0", "z = +1"))

# ----------------------------
# PLOT 2 & 3: sum-score distributions with 4-band colors + cut lines (no legend)
# ----------------------------
plot_sum = function(dfc, df_ref, ttl) {
  dfc_tot <- aggregate(density ~ SumScore, data = dfc, sum)
  ymax <- max(dfc_tot$density)
  
  ggplot(dfc) +
    coord_cartesian(ylim=c(0,ymax*1.03))+
    geom_col(
      aes(x = SumScore, y = density, fill = band),
      width = 1, color = "black", linewidth = 0.8
    ) +
    # dashed lines that STOP before the top
    geom_segment(
      data = df_ref,
      aes(x = SumScore, xend = SumScore, y = 0, yend = ymax * 1),
      linetype = 2, linewidth = 1.8
    ) +
    geom_point(
      data = df_ref, aes(x = SumScore, y = 0),
      shape = 17, size = 7
    ) +
    # labels at the very top (inside panel), no overlap since lines stop lower
    geom_text(
      data = df_ref,
      aes(x = SumScore, y = Inf, label = lab),
      vjust = 1.2, size = 8
    ) +
    scale_fill_manual(values = pal4, guide = "none") +
    scale_x_continuous(
      breaks = seq(J, 4*J, 3),
      limits = c(J - 0.5, 4*J + 0.5)
    ) +
    labs(x = "Observed Sum Score", y = "Density", title = ttl) +
    theme_minimal(base_size = 22) +
    theme(axis.text.y = element_blank(),
          text=element_text(size=30))
}


p_sum_floor = plot_sum(dfc_floor, df_ref_floor, "B)")

p_sum_mid = plot_sum(dfc_mid, df_ref_mid, "C)")

# ----------------------------
# PRINT
# ----------------------------
p_latent
p_sum_floor
p_sum_mid


# ============================================================
# Figure S1 — Directional Semivariograms — Anisotropy Test
# SOM El Maleh sub-watershed (N = 52)
# Requires: gstat, sp, ggplot2
# ============================================================

library(gstat)
library(sp)
library(ggplot2)

# ============================================================
# 1. Load data
# ============================================================
data <- read.csv("Table_S1_LOOCV_52points.csv")
cat("N =", nrow(data), "\n")

# Create spatial object (UTM Zone 32N)
coordinates(data) <- ~ X.UTM..m. + Y.UTM..m.
proj4string(data) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

# ============================================================
# 2. Isotropic semivariogram (reference)
# ============================================================
vario_iso <- variogram(SOM.observed..g.kg. ~ 1, data,
                       cutoff = 25000,
                       width  = 25000 / 14)

# Fit Gaussian model
vgm_iso <- vgm(psill  = 0.374,
               model  = "Gau",
               range  = 14694,
               nugget = 0.671)
fit_iso <- fit.variogram(vario_iso, vgm_iso)

cat("\nFitted isotropic model:\n")
print(fit_iso)
cat(sprintf("Nugget = %.4f | Sill = %.4f | Range = %.0f m (%.2f km)\n",
            fit_iso$psill[1],
            sum(fit_iso$psill),
            fit_iso$range[2],
            fit_iso$range[2]/1000))

# ============================================================
# 3. Directional semivariograms (4 directions, tolerance 30°)
# ============================================================
vario_dir <- variogram(SOM.observed..g.kg. ~ 1, data,
                       alpha   = c(0, 45, 90, 135),
                       cutoff  = 25000,
                       width   = 25000 / 14,
                       tol.hor = 30)

dir_labels <- c("0"   = "N\u2013S (0\u00b0)",
                "45"  = "NE\u2013SW (45\u00b0)",
                "90"  = "E\u2013W (90\u00b0)",
                "135" = "NW\u2013SE (135\u00b0)")

vario_dir$Direction <- factor(
  as.character(vario_dir$dir.hor),
  levels = c("0", "45", "90", "135"),
  labels = c("N\u2013S (0\u00b0)", "NE\u2013SW (45\u00b0)",
             "E\u2013W (90\u00b0)", "NW\u2013SE (135\u00b0)")
)

# ============================================================
# 4. Fit Gaussian model per direction + anisotropy metrics
# ============================================================
cat("\n=== FITTED RANGES BY DIRECTION ===\n")
directions <- c(0, 45, 90, 135)
fitted_ranges <- numeric(length(directions))

for (i in seq_along(directions)) {
  vd <- vario_dir[vario_dir$dir.hor == directions[i], ]
  if (nrow(vd) >= 3) {
    tryCatch({
      fd <- fit.variogram(vd, vgm_iso)
      r  <- fd$range[2]
      if (r > 50000) {
        r <- fit_iso$range[2]
        cat(sprintf("  %d\u00b0: unrealistic range capped to isotropic (%.0f m)\n",
                    directions[i], r))
      } else {
        cat(sprintf("  %d\u00b0: range = %.0f m (%.2f km)\n",
                    directions[i], r, r/1000))
      }
      fitted_ranges[i] <- r
    }, error = function(e) {
      fitted_ranges[i] <<- fit_iso$range[2]
      cat(sprintf("  %d\u00b0: fit failed, using isotropic range\n", directions[i]))
    })
  } else {
    fitted_ranges[i] <- fit_iso$range[2]
  }
}

aniso_ratio <- max(fitted_ranges) / min(fitted_ranges)
range_cv    <- sd(fitted_ranges)  / mean(fitted_ranges) * 100

cat(sprintf("\nAnisotropy ratio (max/min range) = %.3f\n", aniso_ratio))
cat(sprintf("Range CV across directions       = %.1f%%\n", range_cv))

if (aniso_ratio < 2.0) {
  verdict <- "Weak anisotropy \u2192 isotropic model retained as parsimonious choice"
} else {
  verdict <- "Anisotropy present \u2192 ranges vary substantially by direction"
}
cat(sprintf("Conclusion: %s\n", verdict))

# ============================================================
# 5. Isotropic theoretical curve for plot
# ============================================================
h_seq <- seq(0, 26000, length.out = 300)
gamma_theo <- fit_iso$psill[1] +
  fit_iso$psill[2] * (1 - exp(-(h_seq / fit_iso$range[2])^2))
iso_curve <- data.frame(h_km = h_seq / 1000, gamma = gamma_theo)

# ============================================================
# 6. Build figure
# ============================================================
dir_colors <- c("N\u2013S (0\u00b0)"      = "#D85A30",
                "NE\u2013SW (45\u00b0)"   = "#1D9E75",
                "E\u2013W (90\u00b0)"     = "#3C3489",
                "NW\u2013SE (135\u00b0)"  = "#F0B429")

# Annotation box text
box_text <- paste0(
  "Anisotropy ratio = ", round(aniso_ratio, 2), "\n",
  "Range CV = ", round(range_cv, 1), "%\n",
  "Tolerance = 30\u00b0\n\n",
  verdict
)

p <- ggplot() +

  # Directional experimental points + lines
  geom_line(data = vario_dir,
            aes(x = dist/1000, y = gamma, color = Direction),
            linewidth = 0.9, linetype = "dashed", alpha = 0.7) +
  geom_point(data = vario_dir,
             aes(x = dist/1000, y = gamma, color = Direction),
             size = 3.5, alpha = 0.9,
             shape = 21,
             fill  = "white",
             stroke = 1.5) +
  geom_point(data = vario_dir,
             aes(x = dist/1000, y = gamma, color = Direction),
             size = 2.0, alpha = 0.9) +

  # Isotropic theoretical model (black solid)
  geom_line(data = iso_curve,
            aes(x = h_km, y = gamma,
                linetype = "Isotropic Gaussian model"),
            color = "black", linewidth = 1.8) +

  # Sill reference line
  geom_hline(yintercept = sum(fit_iso$psill),
             linetype = "dashed", color = "grey60",
             linewidth = 0.7, alpha = 0.7) +
  annotate("text",
           x = 0.5, y = sum(fit_iso$psill) + 0.04,
           label = paste0("Sill = ", round(sum(fit_iso$psill), 4)),
           hjust = 0, size = 3.3, color = "grey50") +

  # Annotation box
  annotate("label",
           x = Inf, y = Inf,
           label     = box_text,
           hjust     = 1.05, vjust = 1.1,
           size      = 3.0,
           fill      = "#F1EFE8",
           label.size = 0.4,
           color     = "grey30") +

  # Scales
  scale_color_manual(name   = "Direction",
                     values = dir_colors) +
  scale_linetype_manual(name   = NULL,
                        values = c("Isotropic Gaussian model" = "solid")) +

  # Axes
  scale_x_continuous(limits = c(0, 26),
                     breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(limits = c(0, 1.65),
                     breaks = seq(0, 1.5, by = 0.2)) +

  labs(
    title    = "Directional semivariograms \u2014 Anisotropy test",
    subtitle = paste0("SOM El Maleh sub-watershed (N = 52) | ",
                      "Tolerance angle: 30\u00b0"),
    x        = "Lag distance (km)",
    y        = expression(paste("Semivariance  ", gamma, "(h)"))
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(size = 11, hjust = 0.5, face = "plain"),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    legend.position  = "top",
    legend.text      = element_text(size = 9),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "grey70"),
    plot.background  = element_rect(fill = "white", color = NA)
  ) +

  guides(
    color    = guide_legend(order = 1,
                            override.aes = list(size = 3.5,
                                                linetype = "dashed")),
    linetype = guide_legend(order = 2,
                            override.aes = list(color = "black",
                                                linewidth = 1.5))
  )

# ============================================================
# 7. Save figure
# ============================================================
ggsave("Figure_S1_Anisotropy_Test.png",
       plot   = p,
       width  = 9,
       height = 6,
       dpi    = 300,
       bg     = "white")

cat("\nFigure saved: Figure_S1_Anisotropy_Test.png\n")

# ============================================================
# 8. Text for article (Discussion section)
# ============================================================
cat("\n=== TEXT FOR DISCUSSION (copy-paste) ===\n")
cat(sprintf(
"Directional semivariograms computed in four directions (0\u00b0, 45\u00b0,
90\u00b0, 135\u00b0) with a tolerance angle of 30\u00b0 revealed variability
in fitted ranges across directions (anisotropy ratio = %.2f, range CV = %.1f%%).
Given the limited number of sample pairs per directional class and the small
sample size (N = 52), these directional estimates carry substantial uncertainty.
An isotropic Gaussian semivariogram model was therefore retained as a
parsimonious choice, acknowledging that a larger dataset would be required
to reliably characterize and model anisotropy in this watershed
(Supplementary Figure S1).\n",
aniso_ratio, range_cv))

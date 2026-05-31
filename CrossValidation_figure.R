# ============================================================
# Figure 7 — Leave-one-out Cross-Validation
# Gaussian Ordinary Kriging — El Maleh sub-watershed
# Requires: ggplot2 uniquement (pas de factoextra)
# ============================================================

library(ggplot2)

# ============================================================
# 1. Load data
# ============================================================
cv <- read.csv("data_crossval_R.csv")

cat("N =", nrow(cv), "\n")
cat("ME   =", round(mean(cv$Error), 5), "\n")
cat("RMSE =", round(sqrt(mean(cv$Error^2)), 5), "\n")
cat("R2   =", round(cor(cv$Measured, cv$Predicted)^2, 4), "\n")

# Key statistics
ME   <- mean(cv$Error)
RMSE <- sqrt(mean(cv$Error^2))
R2   <- cor(cv$Measured, cv$Predicted)^2
N    <- nrow(cv)

# Axis limits for scatter plot
lim_min <- min(c(cv$Measured, cv$Predicted)) - 0.1
lim_max <- max(c(cv$Measured, cv$Predicted)) + 0.1

# ============================================================
# 2. Panel (a) — Predicted vs Measured
# ============================================================
# Regression line data
reg <- lm(Predicted ~ Measured, data = cv)
x_seq <- seq(lim_min, lim_max, length.out = 100)
reg_df <- data.frame(Measured  = x_seq,
                     Predicted = predict(reg,
                                         newdata = data.frame(Measured = x_seq)))

# Stats annotation
stats_label <- paste0(
  "ME = ", formatC(ME,   digits = 5, format = "f"), " g/kg\n",
  "RMSE = ", formatC(RMSE, digits = 4, format = "f"), " g/kg\n",
  "R\u00b2 = ", formatC(R2,   digits = 4, format = "f"), "\n",
  "N = ", N
)

p_scatter <- ggplot(cv, aes(x = Measured, y = Predicted)) +

  # 1:1 reference line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50",
              linewidth = 0.8) +

  # Points colored by measured SOM
  geom_point(aes(fill = Measured), shape = 21,
             size = 3, stroke = 0.4, color = "white") +
  scale_fill_gradientn(
    colors = c("#D73027", "#FC8D59", "#FEE090",
               "#91CF60", "#1A9850"),
    name   = "SOM (g/kg)",
    limits = c(0.3, 4.5)
  ) +

  # Regression line
  geom_line(data = reg_df, aes(x = Measured, y = Predicted),
            color = "#D85A30", linewidth = 1.5) +

  # Stats box
  annotate("label",
           x = lim_min + 0.05, y = lim_max - 0.05,
           label     = stats_label,
           hjust     = 0, vjust = 1,
           size      = 3.2,
           fill      = "#F1EFE8",
           label.size = 0.4,
           color     = "grey30",
           family    = "mono") +

  # Legend for lines
  annotate("segment",
           x = lim_min + 0.05, xend = lim_min + 0.45,
           y = lim_max - 0.75, yend = lim_max - 0.75,
           linetype = "dashed", color = "grey50", linewidth = 0.8) +
  annotate("text",
           x = lim_min + 0.5, y = lim_max - 0.75,
           label = "1:1 reference line",
           hjust = 0, size = 2.8, color = "grey40") +
  annotate("segment",
           x = lim_min + 0.05, xend = lim_min + 0.45,
           y = lim_max - 1.0, yend = lim_max - 1.0,
           color = "#D85A30", linewidth = 1.2) +
  annotate("text",
           x = lim_min + 0.5, y = lim_max - 1.0,
           label = paste0("Regression fit (R\u00b2=",
                          round(R2, 3), ")"),
           hjust = 0, size = 2.8, color = "#D85A30") +

  # Axes
  coord_fixed(ratio = 1,
              xlim = c(lim_min, lim_max),
              ylim = c(lim_min, lim_max)) +
  labs(title = "(a) Predicted vs. measured SOM values",
       x     = "Measured SOM (g/kg)",
       y     = "Predicted SOM (g/kg)") +

  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(size = 10, face = "plain"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "grey70"),
    legend.position  = "right",
    legend.key.height = unit(1.2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# ============================================================
# 3. Panel (b) — Error distribution
# ============================================================
p_hist <- ggplot(cv, aes(x = Error)) +

  # Histogram
  geom_histogram(fill  = "#7F77DD", color = "white",
                 bins  = 15, alpha = 0.85) +

  # Zero reference line
  geom_vline(xintercept = 0,
             linetype   = "dashed",
             color      = "#D85A30",
             linewidth  = 1.2) +

  # ME line
  geom_vline(xintercept = ME,
             linetype   = "solid",
             color      = "#1D9E75",
             linewidth  = 1.2) +

  # Legend annotation
  annotate("text", x = Inf, y = Inf,
           label = paste0("Prediction errors\n",
                          "--- Zero error reference\n",
                          "\u2014 ME = ",
                          formatC(ME, digits = 5, format = "f"),
                          " g/kg"),
           hjust = 1.05, vjust = 1.1,
           size  = 3.0, color = "grey30",
           family = "mono") +

  labs(title = "(b) Distribution of cross-validation errors",
       x     = "Prediction error (g/kg)",
       y     = "Frequency") +

  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(size = 10, face = "plain"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "grey70"),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# ============================================================
# 4. Combine panels with patchwork or cowplot
# ============================================================
# Try patchwork first
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p_combined <- p_scatter + p_hist +
    plot_annotation(
      title    = "Leave-one-out cross-validation \u2014 Gaussian ordinary kriging",
      subtitle = "El Maleh sub-watershed (N = 52)",
      theme    = theme(
        plot.title    = element_text(size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5,
                                     color = "grey40")
      )
    )
} else {
  # Fallback: save separately
  cat("patchwork not found — saving panels separately.\n")
  cat("Install with: install.packages('patchwork')\n")

  ggsave("Figure7a_scatter.png", p_scatter,
         width = 6, height = 5.5, dpi = 300, bg = "white")
  ggsave("Figure7b_histogram.png", p_hist,
         width = 6, height = 5.5, dpi = 300, bg = "white")
  cat("Saved: Figure7a_scatter.png and Figure7b_histogram.png\n")
  stop("Install patchwork for combined figure.")
}

# ============================================================
# 5. Save combined figure
# ============================================================
ggsave("Figure7_CrossValidation_R.png",
       plot   = p_combined,
       width  = 13,
       height = 5.5,
       dpi    = 300,
       bg     = "white")

cat("\nFigure saved: Figure7_CrossValidation_R.png\n")

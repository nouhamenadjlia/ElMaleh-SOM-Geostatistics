# ============================================================
# Figure 6 — Experimental Semivariogram + Gaussian Model Fit
# Soil Organic Matter — El Maleh sub-watershed (N = 52)
# Correction: C0 and C1 displayed with proper subscripts
# Requires: ggplot2
# ============================================================

library(ggplot2)

# ============================================================
# 1. Load data
# ============================================================
exp_data  <- read.csv("data_variogram_exp.csv")   # lag_km, gamma
theo_data <- read.csv("data_variogram_theo.csv")  # h_km, gamma_theo

# Model parameters
nugget       <- 0.6714
sill         <- 1.0456
partial_sill <- 0.3741
range_km     <- 14.69
spd          <- 35.8

cat("Experimental points:", nrow(exp_data), "\n")
cat("Theoretical points: ", nrow(theo_data), "\n")
cat(sprintf("Nugget=%.4f | Sill=%.4f | Range=%.2f km | SPD=%.1f%%\n",
            nugget, sill, range_km, spd))

# ============================================================
# 2. Build figure
# ============================================================
p <- ggplot() +

  # --- Reference lines (sill, nugget, range) ---
  geom_hline(yintercept = sill,
             linetype = "dashed", color = "grey60",
             linewidth = 0.7, alpha = 0.8) +
  geom_hline(yintercept = nugget,
             linetype = "dotted", color = "grey60",
             linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = range_km,
             linetype = "dashed", color = "grey60",
             linewidth = 0.7, alpha = 0.8) +

  # --- Theoretical Gaussian curve ---
  geom_line(data = theo_data,
            aes(x = h_km, y = gamma_theo,
                color = "Gaussian model fit"),
            linewidth = 2.0) +

  # --- Experimental semivariogram points ---
  geom_point(data  = exp_data,
             aes(x = lag_km, y = gamma,
                 fill = "Experimental semivariogram"),
             shape = 21, size = 4,
             color = "white", stroke = 0.4) +

  # --- Sill annotation ---
  annotate("text",
           x = 0.8, y = sill + 0.038,
           label = paste0("Sill = ", sill),
           hjust = 0, size = 3.5, color = "grey50") +

  # --- Nugget annotation ---
  annotate("text",
           x = 0.8, y = nugget + 0.038,
           label = paste0("Nugget = ", nugget),
           hjust = 0, size = 3.5, color = "grey50") +

  # --- Range annotation ---
  annotate("text",
           x = range_km + 0.3, y = 0.08,
           label = paste0("Range = ", range_km, " km"),
           hjust = 0, size = 3.5, color = "grey50") +

  # ============================================================
  # --- Parameters box with correct subscripts ---
  # Uses geom_label with a data frame + parse = TRUE
  # parse = TRUE allows plotmath expressions for subscripts
  # ============================================================
  geom_label(
    data = data.frame(
      x = Inf, y = -Inf,
      label = paste0(
        "atop(atop(atop(atop(atop(",
        '"Model: Gaussian",',
        'paste("Nugget~(C"[0]*") = ', nugget, '")),',
        'paste("Partial~sill~(C"[1]*") = ', partial_sill, '")),',
        'paste("Total~sill = ', sill, '")),',
        'paste("Range = ', range_km, '~km")),',
        'paste("SPD = ', spd, '%"))'
      )
    ),
    aes(x = x, y = y, label = label),
    hjust = 1.05, vjust = -0.1,
    size = 3.0,
    fill = "#F1EFE8",
    label.size = 0.4,
    color = "grey30",
    parse = TRUE
  ) +

  # --- Color and fill scales ---
  scale_color_manual(
    name   = NULL,
    values = c("Gaussian model fit" = "#D85A30")
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("Experimental semivariogram" = "#3C3489")
  ) +

  # --- Axes and title ---
  labs(
    title    = "Experimental semivariogram and Gaussian model fit",
    subtitle = "Soil organic matter \u2014 El Maleh sub-watershed (N = 52)",
    x        = "Lag distance (km)",
    y        = expression(paste("Semivariance  ", gamma, "(h)"))
  ) +

  # --- Axis limits ---
  scale_x_continuous(limits = c(0, 26),
                     breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(limits = c(0, 1.55),
                     breaks = seq(0, 1.5, by = 0.2)) +

  # --- Theme ---
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(size = 11, hjust = 0.5, face = "plain"),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    legend.key.size  = unit(0.8, "cm"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "grey70"),
    plot.background  = element_rect(fill = "white", color = NA)
  ) +

  # --- Legend: combine color + fill ---
  guides(
    color = guide_legend(order = 2,
                         override.aes = list(
                           fill      = NA,
                           linetype  = "solid",
                           linewidth = 1.5,
                           shape     = NA)),
    fill  = guide_legend(order = 1,
                         override.aes = list(
                           shape     = 21,
                           size      = 4,
                           color     = "white",
                           linetype  = 0,
                           linewidth = 0))
  )

# ============================================================
# 3. Save figure
# ============================================================
ggsave("Figure6_Semivariogram_R.png",
       plot   = p,
       width  = 8,
       height = 5.5,
       dpi    = 300,
       bg     = "white")

cat("\nFigure saved: Figure6_Semivariogram_R.png\n")
cat("C0 and C1 now display with correct subscripts.\n")

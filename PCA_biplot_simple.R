# ============================================================
# Figure 8 — PCA Biplot
# SOM, Slope (%), NDVI — El Maleh sub-watershed (N = 31)
# Requires: FactoMineR, ggplot2
# ============================================================

library(FactoMineR)
library(ggplot2)

# 1. Load data
data <- read.csv("data_for_R_PCA.csv")
cat("N =", nrow(data), "\n")

# 2. SOM classes
data$SOM_class <- cut(data$SOM,
                      breaks = c(-Inf, 1.8, 2.8, Inf),
                      labels = c("Low SOM (<1.8 g/kg)",
                                 "Moderate SOM (1.8-2.8 g/kg)",
                                 "High SOM (>2.8 g/kg)"))
cat("\nSOM classes:\n")
print(table(data$SOM_class))

# 3. PCA
res_pca <- PCA(data[, c("SOM", "Slope", "NDVI")],
               scale.unit = TRUE, graph = FALSE)

var1 <- round(res_pca$eig[1, 2], 1)
var2 <- round(res_pca$eig[2, 2], 1)
cum  <- round(var1 + var2, 1)

cat(sprintf("\nPC1 = %.1f%%, PC2 = %.1f%%, Cumulative = %.1f%%\n", var1, var2, cum))
cat("\nLoadings:\n")
print(round(res_pca$var$coord[, 1:2], 3))

# 4. Extract scores and loadings
scores        <- as.data.frame(res_pca$ind$coord[, 1:2])
names(scores) <- c("PC1", "PC2")
scores$SOM_class <- data$SOM_class

loadings        <- as.data.frame(res_pca$var$coord[, 1:2])
names(loadings) <- c("PC1", "PC2")
loadings$Variable <- rownames(loadings)
arrow_scale   <- 2.0
loadings$PC1s <- loadings$PC1 * arrow_scale
loadings$PC2s <- loadings$PC2 * arrow_scale

# 5. Colors and shapes
class_colors <- c("Low SOM (<1.8 g/kg)"        = "#D85A30",
                  "Moderate SOM (1.8-2.8 g/kg)" = "#F0B429",
                  "High SOM (>2.8 g/kg)"         = "#1D9E75")
class_shapes <- c("Low SOM (<1.8 g/kg)"        = 16,
                  "Moderate SOM (1.8-2.8 g/kg)" = 15,
                  "High SOM (>2.8 g/kg)"         = 17)
arrow_colors <- c("SOM" = "#3C3489", "Slope" = "#D85A30", "NDVI" = "#1D9E75")

# 6. Build biplot
p <- ggplot() +
  stat_ellipse(data = scores,
               aes(x = PC1, y = PC2, fill = SOM_class, color = SOM_class),
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.12, linewidth = 1.0) +
  geom_point(data = scores,
             aes(x = PC1, y = PC2, color = SOM_class, shape = SOM_class),
             size = 3.5, stroke = 0.5) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1s, yend = PC2s, color = Variable),
               arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
               linewidth = 1.2, show.legend = FALSE) +
  geom_label(data = loadings,
             aes(x = PC1s * 1.15, y = PC2s * 1.15,
                 label = Variable, color = Variable),
             size = 4, fontface = "bold", fill = "white",
             label.size = 0, show.legend = FALSE) +
  annotate("text", x =  3.2, y =  1.8, label = "High SOM zone",
           color = "#1D9E75", size = 3.3, fontface = "italic") +
  annotate("text", x = -2.0, y = -1.6, label = "Low SOM zone",
           color = "#D85A30", size = 3.3, fontface = "italic") +
  annotate("text", x =  0.3, y = -2.5, label = "Moderate SOM zone",
           color = "#C8960A", size = 3.3, fontface = "italic") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  annotate("label", x = Inf, y = -Inf,
           label = paste0("PC1 = ", var1, "%\nPC2 = ", var2, "%\nCumulative = ", cum, "%"),
           hjust = 1.05, vjust = -0.1, size = 3.0,
           fill = "#F1EFE8", label.size = 0.4, color = "grey40") +
  scale_color_manual(name = NULL, values = c(class_colors, arrow_colors)) +
  scale_fill_manual(name = NULL, values = class_colors) +
  scale_shape_manual(name = NULL, values = class_shapes) +
  labs(title    = "Principal component analysis biplot",
       subtitle = paste0("SOM, slope (%), and NDVI \u2014 El Maleh sub-watershed (N = 31)"),
       x = paste0("PC1 (", var1, "% variance explained)"),
       y = paste0("PC2 (", var2, "% variance explained)")) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
        legend.position  = "top",
        legend.text      = element_text(size = 9),
        panel.grid.major = element_line(color = "grey92"),
        panel.grid.minor = element_blank(),
        axis.line        = element_line(color = "grey70"),
        plot.background  = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(override.aes = list(shape = c(16,15,17,NA,NA,NA), size = 3.5)),
         fill = "none", shape = "none")

# 7. Save
ggsave("Figure8_PCA_Biplot_R.png", plot = p,
       width = 8, height = 6.5, dpi = 300, bg = "white")

cat("\nFigure saved: Figure8_PCA_Biplot_R.png\n")

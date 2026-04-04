set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df <- read_feather("results/with_idx/results_toy_example.feather")

# A demo for BART vs DART
# Feynman II.11.17
# SNR = 1, n = 1000, p0 = 6, p = 306, seed = 860
df_BART_VC <- df %>% filter(Algorithm == "BART VC-measure")
df_DART_VC <- df %>% filter(Algorithm == "DART VC-measure")
df_tmp <- left_join(df_BART_VC, df_DART_VC,
                    by = c("dataset_name", "random_state", "SNR", "n", "p", "p0"))
df_demo <- data.frame(vc_BART = df_tmp$vc_mu_K10.x[[1]],
                      vc_rank_BART = df_tmp$vc_rank_mu_K10.x[[1]],
                      vc_DART = df_tmp$vc_mu_K10.y[[1]],
                      vc_rank_DART = df_tmp$vc_rank_mu_K10.y[[1]])
df_demo <- apply(df_demo, 2, function(x) log1p(x))
df_demo <- apply(df_demo, 2, function(x) (x - min(x)) / (max(x) - min(x)))
df_demo <- as.data.frame(df_demo)
df_demo$idx <- c(rep("Relevant", df_tmp$p0), rep("Irrelevant", df_tmp$p - df_tmp$p0))
df_demo$idx <- factor(df_demo$idx, levels = c("Relevant", "Irrelevant"))
df_demo$BART_idx <- df_demo$DART_idx <- rep("Not selected", df_tmp$p)
df_demo$BART_idx[df_tmp$pos_idx.x[[1]]+1] <- "Selected"
df_demo$DART_idx[df_tmp$pos_idx.y[[1]]+1] <- "Selected"
df_demo$BART_idx <- factor(df_demo$BART_idx, levels = c("Selected", "Not selected"))
df_demo$DART_idx <- factor(df_demo$DART_idx, levels = c("Selected", "Not selected"))

# Identify false negatives
false_negatives <- df_demo %>%
  filter(idx == "Relevant", BART_idx == "Not selected")

# Label position and buffer
label_x <- 0.5
label_y <- 0.45
buffer_x <- 0.02
buffer_y <- 0.03

# Compute adjusted arrow start points (toward label, but with buffer)
arrow_segments <- false_negatives %>%
  mutate(
    x_start = label_x,
    y_start = label_y - 0.04,
    x_end = vc_BART - sign(vc_BART - label_x) * buffer_x,
    y_end = vc_rank_BART - sign(vc_rank_BART - label_y) * buffer_y
  )
arrow_segments$x_end[1] <- arrow_segments$x_end[1] + 0.005
arrow_segments$y_end[1] <- arrow_segments$y_end[1] - 0.01

# Plot
p4l <- ggplot(df_demo, aes(x = vc_BART, y = vc_rank_BART, shape = idx, color = BART_idx)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_shape_manual(values = c("Relevant" = 17, "Irrelevant" = 1)) +
  scale_color_manual(values = c("Not selected" = "dodgerblue", "Selected" = "tomato")) +
  labs(shape = "Ground Truth", color = "Selection", x = "BART VC", y = "BART VC Rank") +
  theme_minimal() +
  annotate("text", x = label_x, y = label_y, label = "False Negatives", size = 4, fontface = "bold", color = "gray30") +
  geom_segment(data = arrow_segments,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.2, "cm")),
               color = "gray50"
  ) +
  theme(text = element_text(family = "Arial"))

p4r <- ggplot(df_demo, aes(x = vc_DART, y = vc_rank_DART, shape = idx, color = DART_idx)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_shape_manual(values = c("Relevant" = 17, "Irrelevant" = 1)) +
  scale_color_manual(values = c("Not selected" = "dodgerblue", "Selected" = "tomato")) +
  labs(shape = "Ground Truth", color = "Selection", x = "DART VC", y = "DART VC Rank") +
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

p4 <- p4l + p4r + plot_layout(guides = "collect") & theme(legend.position = "right")
ggsave("figs/fig4_Scatterplot_DART_vs_BART_VC-measure.pdf", plot = p4,
       device = cairo_pdf, width = 9, height = 2.5)

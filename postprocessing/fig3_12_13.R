set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df_BART_VC <- read_feather("results/with_idx/results_BART_VC_measure.feather")
df_DART_VC <- read_feather("results/with_idx/results_DART_VC-measure.feather") %>%
  filter(Algorithm == "DART VC-measure (L=10)") %>%
  mutate(Algorithm = case_match(Algorithm,
                                "DART VC-measure (L=10)" ~ "DART VC-measure"))
df <- bind_rows(df_BART_VC, df_DART_VC)

# Convert SNR to factor to maintain the order
df <- df %>%
  mutate(SNR = ifelse(SNR == 0, "noiseless", as.character(SNR)))
df$SNR <- factor(df$SNR, levels = c("0.25", "0.5", "1", "2", "5", "10", "15", "20", "noiseless"))

# Convert Algorithm to factor to maintain the order
df$Algorithm <- factor(df$Algorithm, levels = c("DART VC-measure", "BART VC-measure"))

# Average over `random_state`
summary_df <- df %>%
  group_by(dataset_name, n, SNR, Algorithm) %>%
  summarize(TPR = mean(TPR),
            FPR = mean(FPR),
            F1 = mean(F1),
            .groups = 'drop')
summary_df_2 <- summary_df %>%
  group_by(n, SNR, Algorithm) %>%
  summarize(TPR = mean(TPR),
            FPR = mean(FPR),
            F1 = mean(F1),
            .groups = 'drop')

# F1
F1_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, F1) %>%
  rename(mean_value = F1)
p3 <- feynman_SR_plot(F1_summary, xlab = "SNR", ylab = expression(F[1]), title = "")
ggsave("figs/fig3_F1_DART_vs_BART_VC-measure.pdf", plot = p3,
       device = cairo_pdf, width = 10, height = 5.5)

# Supp A Fig 12
TPR_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, TPR) %>%
  rename(mean_value = TPR)
p12 <- feynman_SR_plot(TPR_summary, xlab = "SNR", ylab = "TPR", title = "")
ggsave("figs/fig12_TPR_DART_vs_BART_VC-measure.pdf", plot = p12,
       device = cairo_pdf, width = 10, height = 6.5)

# Supp A Fig 13
FPR_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, FPR) %>%
  rename(mean_value = FPR)
p13 <- feynman_SR_plot(FPR_summary, xlab = "SNR", ylab = "FPR", title = "")
ggsave("figs/fig13_FPR_DART_vs_BART_VC-measure.pdf", plot = p13,
       device = cairo_pdf, width = 10, height = 6.5)

# Runtime comparison
runtime <- df %>%
  group_by(dataset_name, n, p, Algorithm) %>%
  summarize(time = mean(Runtime),
            .groups = 'drop') %>%
  group_by(n, p, Algorithm) %>%
  summarize(time = mean(time),
            .groups = 'drop') %>%
  group_by(n, Algorithm) %>%
  summarize(time = mean(time),
            .groups = 'drop')

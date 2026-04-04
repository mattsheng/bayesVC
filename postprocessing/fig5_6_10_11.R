set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df_DART_VC <- read_feather("results/with_idx/results_DART_VC-measure.feather") %>%
  filter(Algorithm == "DART VC-measure (L=10)") %>%
  mutate(Algorithm = case_match(Algorithm,
                                "DART VC-measure (L=10)" ~ "DART VC-measure"))
df_ABC <- read_feather("results/with_idx/results_ABC_Bayesian_forests.feather")
df_BART_MI <- read_feather("results/with_idx/results_BART_MI.feather")
df_BART_perm <- read_feather("results/with_idx/results_BART_perm.feather")
df_BART_VIP_rank <- read_feather("results/with_idx/results_BART_VIP_Rank.feather")
df_DART_MPM <- read_feather("results/with_idx/results_DART_MPM.feather")
df_DART_MPM$Algorithm <- "DART"
df <- bind_rows(df_DART_VC, df_ABC, df_BART_MI, df_BART_perm, df_BART_VIP_rank, df_DART_MPM)

# Convert SNR to factor to maintain the order
df <- df %>%
  mutate(SNR = ifelse(SNR == 0, "noiseless", as.character(SNR)))
df$SNR <- factor(df$SNR, levels = c("0.25", "0.5", "1", "2", "5", "10", "15", "20", "noiseless"))

# Average over `random_state`
summary_df <- df %>%
  group_by(dataset_name, n, SNR, Algorithm) %>%
  summarize(TPR = mean(TPR),
            FPR = mean(FPR),
            F1 = mean(F1),
            time = mean(Runtime),
            .groups = 'drop')
summary_df_2 <- summary_df %>%
  group_by(n, SNR, Algorithm) %>%
  summarize(TPR = mean(TPR),
            FPR = mean(FPR),
            F1 = mean(F1),
            time = mean(time),
            .groups = 'drop')
summary_df_3 <- df %>%
  group_by(dataset_name, n, p, Algorithm) %>%
  summarize(time = mean(Runtime),
            .groups = 'drop') %>%
  group_by(n, p, Algorithm) %>%
  summarize(time = mean(time),
            .groups = 'drop')

# F1
F1_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, F1) %>%
  rename(mean_value = F1)
p5 <- feynman_SR_plot(F1_summary, xlab = "SNR", ylab = expression(F[1]), title = "")
ggsave("figs/fig5_F1_all.pdf", plot = p5,
       device = cairo_pdf, width = 12, height = 8)

# TPR
TPR_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, TPR) %>%
  rename(mean_value = TPR)
p6 <- feynman_SR_plot(TPR_summary, xlab = "SNR", ylab = "TPR", title = "")
ggsave("figs/fig6_TPR_all.pdf", plot = p6,
       device = cairo_pdf, width = 12, height = 7)

# FPR
FPR_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, FPR) %>%
  rename(mean_value = FPR)
p10 <- feynman_SR_plot(FPR_summary, xlab = "SNR", ylab = "FPR", title = "")
ggsave("figs/fig10_FPR_all.pdf", plot = p10,
       device = cairo_pdf, width = 12, height = 6.5)

# Runtime
runtime_summary <- summary_df_3 %>%
  select(n, p, Algorithm, time) %>%
  rename(mean_value = time)
p11 <- runtime_plot(runtime_summary, xlab = "p", ylab = "Runtime (s)", title = "")
ggsave("figs/fig11_runtime_all.pdf", plot = p11,
       device = cairo_pdf, width = 12, height = 6.5)

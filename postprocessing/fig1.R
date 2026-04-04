set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df_BART_VC_measure <- read_feather("results/with_idx/results_BART_VC_measure.feather")
df_BART_MI <- read_feather("results/with_idx/results_BART_MI.feather")
df_BART_VIP_rank <- read_feather("results/with_idx/results_BART_VIP_Rank.feather")
df_BART_perm <- read_feather("results/with_idx/results_BART_perm.feather")
df_BART_perm <- df_BART_perm %>%
  filter(Algorithm == "BART VIP-G.SE")

df <- bind_rows(df_BART_VC_measure, df_BART_MI, df_BART_perm, df_BART_VIP_rank)

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

# Fig1
F1_summary$Algorithm <- factor(F1_summary$Algorithm,
                               levels = c("BART VC-measure",
                                          "BART VIP-G.SE",
                                          "BART MI-Local",
                                          "BART VIP Rank"))
p1 <- feynman_SR_plot(F1_summary, xlab = "SNR", ylab = expression(F[1]), title = "")
ggsave("figs/fig1_F1_BART_VC-measure.pdf", plot = p1,
       device = cairo_pdf, width = 10, height = 6)

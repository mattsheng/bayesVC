set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df_DART_VC_measure <- read_feather("results/with_idx/results_DART_VC-measure.feather")
df_DART <- read_feather("results/with_idx/results_DART_MPM.feather")
df_ABC <- read_feather("results/with_idx/results_ABC_Bayesian_forests.feather")

# Rename algorithms
df_DART_VC_measure <- df_DART_VC_measure %>%
  filter(Algorithm == "DART VC-measure (L=10)") %>%
  mutate(Algorithm = case_match(Algorithm,
                                "DART VC-measure (L=10)" ~ "DART VC-measure"))
df_DART$Algorithm <- "DART"
df_ABC$Algorithm <- "ABC Bayesian forests"

df <- bind_rows(df_DART_VC_measure, df_DART, df_ABC)

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

# F1
F1_summary <- summary_df_2 %>%
  select(n, SNR, Algorithm, F1) %>%
  rename(mean_value = F1)

# Fig2
F1_summary$Algorithm <- factor(F1_summary$Algorithm,
                               levels = c("DART VC-measure",
                                          "DART",
                                          "ABC Bayesian forests"))
p2 <- feynman_SR_plot(F1_summary, xlab = "SNR", ylab = expression(F[1]), title = "")
ggsave("figs/fig2_F1_DART_VC-measure.pdf", plot = p2,
       device = cairo_pdf, width = 10, height = 5.5)

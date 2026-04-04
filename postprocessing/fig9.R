set.seed(123)
library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source("postprocessing/plot_funs.R")

# Load results
df <- read_feather("results/with_idx/results_DART_VC-measure.feather")

# Convert SNR to factor to maintain the order
df <- df %>%
  mutate(SNR = ifelse(SNR == 0, "noiseless", as.character(SNR)))
df$SNR <- factor(df$SNR, levels = c("0.25", "0.5", "1", "2", "5", "10", "15", "20", "noiseless"))

# Convert Algorithm to factor to maintain the order
df <- df %>% mutate(Algorithm = case_match(Algorithm,
                                           "DART VC-measure (L=20)" ~ "L=20",
                                           "DART VC-measure (L=15)" ~ "L=15",
                                           "DART VC-measure (L=10)" ~ "L=10",
                                           "DART VC-measure (L=5)" ~ "L=5",
                                           "DART VC-measure (L=4)" ~ "L=4",
                                           "DART VC-measure (L=3)" ~ "L=3",
                                           "DART VC-measure (L=2)" ~ "L=2",
                                           "DART VC-measure (L=1)" ~ "L=1")) %>%
  filter(Algorithm %in% c("L=20", "L=10", "L=5", "L=3", "L=2", "L=1"))
df$Algorithm <- factor(df$Algorithm, levels = c("L=20", "L=10", "L=5", "L=3", "L=2", "L=1"))

df_rel <- df %>%
  group_by(dataset_name, random_state, SNR, n) %>%
  mutate(TPR_base = TPR[Algorithm == "L=20"],
         FPR_base = FPR[Algorithm == "L=20"],
         FNR_base = FNR[Algorithm == "L=20"],
         F1_base  = F1[Algorithm == "L=20"]) %>%
  mutate(
    diff_TPR = TPR - TPR_base,
    diff_FPR = FPR - FPR_base,
    diff_FNR = FNR - FNR_base,
    diff_F1  = F1  - F1_base
  ) %>%
  ungroup() %>%
  filter(Algorithm  != "L=20")

# Average over `random_state`
summary_df <- df_rel %>%
  group_by(dataset_name, n, SNR, Algorithm) %>%
  summarize(TPR = median(diff_TPR),
            FPR = median(diff_FPR),
            F1 = median(diff_F1),
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
p9 <- feynman_SR_plot(F1_summary, xlab = "SNR", ylab = expression(Delta * F[1]), title = "")
ggsave("figs/fig9_F1_diff_L.pdf", plot = p9,
       device = cairo_pdf, width = 10, height = 5.5)

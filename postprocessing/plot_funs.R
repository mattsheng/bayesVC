feynman_SR_plot <- function(df, xlab = "", ylab = "", title = "") {
  ggplot(df, aes(x = SNR, y = mean_value, color = Algorithm, group = Algorithm)) +
    geom_point() +
    geom_line() +
    labs(x = xlab, y = ylab, color = "", title = title) +
    facet_wrap(~n, scales = "free_y", ncol = 2,
               labeller = labeller(n = function(x) paste0("n = ", x))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title = element_text(size = 14),       # Axis titles
          axis.text = element_text(size = 12),        # Axis text (tick labels)
          plot.title = element_text(size = 16), # Plot title
          legend.text = element_text(size = 12),      # Legend text
          legend.title = element_text(size = 14),     # Legend title
          strip.text = element_text(size = 14),       # Facet titles
          strip.placement = "outside",
          strip.background = element_blank(),
          legend.position = "bottom",
          text = element_text(family = "Arial"),
          plot.margin = margin(t = -25, b = -8, l = 5.5, r = 5.5, unit = "pt")
    )
}

runtime_plot <- function(df, xlab = "", ylab = "", title = "") {
  ggplot(df, aes(x = p, y = mean_value, color = Algorithm, group = Algorithm)) +
    geom_point() +
    geom_line() +
    labs(x = xlab, y = ylab, color = "", title = title) +
    scale_x_continuous(breaks = sort(unique(df$p))) +
    facet_wrap(~n, scales = "free_y", ncol = 2,
               labeller = labeller(n = function(x) paste0("n = ", x))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title = element_text(size = 14),       # Axis titles
          axis.text = element_text(size = 12),        # Axis text (tick labels)
          plot.title = element_text(size = 16), # Plot title
          legend.text = element_text(size = 12),      # Legend text
          legend.title = element_text(size = 14),     # Legend title
          strip.text = element_text(size = 14),        # Facet titles
          legend.position = "bottom"
    )
}

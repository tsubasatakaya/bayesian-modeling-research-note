source("code/setup.R")
source("code/functions.R")

posterior_path <- file.path(output_path, "posterior_files")

fit <- readRDS(file.path(output_path, "models", "vote_intention_second_fitted.RDS"))

diagnostics <- fit$summary() |> 
  select(variable, rhat, ess_bulk, ess_tail) |> 
  slice(2:n())

diag_stats_plot <- diagnostics |> 
  pivot_longer(!variable, names_to = "statistic", values_to = "value") |> 
  mutate(statistic = factor(statistic, levels = c("rhat", "ess_bulk", "ess_tail"))) |> 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ statistic,
             scale = "free_x",
             labeller = labeller(
               statistic = c("rhat" = "R hat",
                             "ess_bulk" = "Bulk-ESS",
                             "ess_tail" = "Tail-ESS")
               )
             ) +
  labs(x = "", y = "Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12,
                                    face = "bold"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10))

ggsave(file.path(output_path, "figures", "diagnostics_plot.pdf"),
       diag_stats_plot, width = 8, height = 6, units = "in",
       dpi = 300, useDingbats = TRUE)

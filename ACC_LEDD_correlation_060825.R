# Load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)

# Set working directory and load data
setwd("")
df <- read_excel("LEDD_DALEDD_CORR.xlsx")

# Define task groups
task_groups <- list(
  "Impulsivity (Emotional)" = c(2, 4),
  "Impulsivity (Non-Emotional)" = c(1, 3),
  "Face in the Crowd (Emotional)" = 5,
  "Face in the Crowd (Non-Emotional)" = 6,
  "Emotion Recognition (Emotional)" = c(7, 9),
  "Gender Recognition (Non-Emotional)" = 8
)
task_names <- names(task_groups)

# Aggregation function
aggregate_group <- function(data, tasks) {
  data %>%
    filter(task %in% tasks) %>%
    group_by(pseudonym) %>%
    summarize(
      acc_FU = mean(acc_FU, na.rm = TRUE),
      acc_BL = mean(acc_BL, na.rm = TRUE),
      LEDD_FU = first(LEDD_FU),
      LEDD_BL = first(LEDD_BL),
      DA_LEDD_FU = first(`DA-LEDD_FU`),
      DA_LEDD_BL = first(`DA-LEDD_BL`)
    ) %>%
    mutate(
      Accuracy_Change = acc_FU - acc_BL,
      LEDD_Change = LEDD_FU - LEDD_BL,
      DA_LEDD_Change = DA_LEDD_FU - DA_LEDD_BL
    )
}

# Bootstrap correlation function
bootstrap_correlation <- function(x, y, n_boot = 1000) {
  corrs <- numeric(n_boot)
  n <- length(x)
  for (i in 1:n_boot) {
    idx <- sample(1:n, n, replace = TRUE)
    corrs[i] <- cor(x[idx], y[idx], use = "complete.obs")
  }
  return(corrs)
}

# Create correlation data
create_plots <- function(agg, med_type = "LEDD", group_name, color, bootstrap_seed = 123) {
  if (med_type == "LEDD") {
    x_var <- agg$LEDD_Change
    x_lab <- "LEDD Change (mg)"
  } else if (med_type == "DA-LEDD") {
    x_var <- agg$DA_LEDD_Change
    x_lab <- "DA-LEDD Change (mg)"
  } else {
    stop("Invalid medication type. Use 'LEDD' or 'DA-LEDD'")
  }
  
  set.seed(bootstrap_seed)
  boot_corr <- bootstrap_correlation(x_var, agg$Accuracy_Change)
  obs_corr <- cor(x_var, agg$Accuracy_Change, use = "complete.obs")
  ci_corr <- quantile(boot_corr, c(0.025, 0.975))
  
  return(list(corr = obs_corr, ci = ci_corr))
}

# === Create summary data for forest plot ===
summary_list <- list()

for (i in seq_along(task_groups)) {
  group_name <- task_names[i]
  task_ids <- task_groups[[i]]
  agg <- aggregate_group(df, task_ids)
  
  # LEDD
  plots_ledd <- create_plots(agg, "LEDD", group_name, plot_colors[i], bootstrap_seed = 400 + i)
  summary_list[[length(summary_list) + 1]] <- data.frame(
    Task = group_name,
    Type = "LEDD",
    r = plots_ledd$corr,
    CI_low = plots_ledd$ci[1],
    CI_high = plots_ledd$ci[2]
  )
  
  # DA-LEDD
  plots_daledd <- create_plots(agg, "DA-LEDD", group_name, plot_colors[i], bootstrap_seed = 500 + i)
  summary_list[[length(summary_list) + 1]] <- data.frame(
    Task = group_name,
    Type = "DA-LEDD",
    r = plots_daledd$corr,
    CI_low = plots_daledd$ci[1],
    CI_high = plots_daledd$ci[2]
  )
}

summary_df <- bind_rows(summary_list)

# === Add overall row (tasks 1–9) ===
overall_tasks <- 1:9
agg_overall <- aggregate_group(df, overall_tasks)

# LEDD overall
overall_ledd <- create_plots(agg_overall, "LEDD", "Overall", "darkslategray4", bootstrap_seed = 999)
summary_list_overall_ledd <- data.frame(
  Task = "Overall",
  Type = "LEDD",
  r = overall_ledd$corr,
  CI_low = overall_ledd$ci[1],
  CI_high = overall_ledd$ci[2]
)

# DA-LEDD overall
overall_daledd <- create_plots(agg_overall, "DA-LEDD", "Overall", "deeppink3", bootstrap_seed = 1000)
summary_list_overall_daledd <- data.frame(
  Task = "Overall",
  Type = "DA-LEDD",
  r = overall_daledd$corr,
  CI_low = overall_daledd$ci[1],
  CI_high = overall_daledd$ci[2]
)

# Combine all
summary_df <- bind_rows(summary_list_overall_ledd, summary_list_overall_daledd, summary_df)

# Add labels and placement for text
summary_df <- summary_df %>%
  mutate(
    CI_label = paste0("[", round(CI_low, 2), ", ", round(CI_high, 2), "]"),
    label_x = CI_high + 0.03
  )

# Reorder factor levels (Overall at top)
summary_df$Task <- factor(summary_df$Task, levels = c("Overall", rev(task_names)))
summary_df$Type <- factor(summary_df$Type, levels = c("LEDD", "DA-LEDD"))

# === Forest Plot ===
forest_plot <- ggplot(summary_df, aes(x = r, y = Task, color = Type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.25,
                 position = position_dodge(width = 0.5), size = 0.6) +
  geom_text(aes(x = label_x, label = CI_label, color = Type),
            position = position_dodge(width = 0.5),
            hjust = 0, size = 3.1,
            show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_manual(values = c("LEDD" = "darkslategray4", "DA-LEDD" = "deeppink3")) +
  xlim(min(summary_df$CI_low) - 0.05, max(summary_df$CI_high) + 0.25) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Correlation between Medication Change and Accuracy Change by Task Group",
    subtitle = "Pearson's r with 95% confidence intervals - Accuracy Change (Follow-up - Baseline) vs LEDD / DA-LEDD",
    x = "Pearson Correlation (r) with Accuracy Change [95% CI]",
    y = "Task Group",
    color = "Medication Type"
  ) +
  theme(
    legend.position = "top",
    text = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10))
  )

# Save
ggsave("ForestPlot_CI_Aligned_Readable_With_Overall.pdf", forest_plot, width = 12, height = 6)
print(forest_plot)
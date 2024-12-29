# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Set working directory
setwd("")

# Read the data from the Excel file
data = read_excel("DA_LEDD_CORR.xlsx", sheet = "Tabelle1")

# Convert error rates to accuracy for tasks 1 to 4
data = data %>%
  mutate(
    acc_FU = ifelse(task %in% 1:4, 1 - as.numeric(acc_FU), as.numeric(acc_FU)),  # Accuracy = 1 - error rate
    acc_BL = ifelse(task %in% 1:4, 1 - as.numeric(acc_BL), as.numeric(acc_BL))   # Accuracy = 1 - error rate
  )

# ABSOLUTE CHANGES ############################################


# Calculate DA_LEDD_change and Accuracy_change
data = data %>%
  mutate(
    LEDD_change = as.numeric(`DA-LEDD_FU`) - as.numeric(`DA-LEDD_BL`),
    Accuracy_change = acc_FU - acc_BL  # Change in accuracy
  )

# Create an empty results data frame
results_abs = data.frame(Task = numeric(), Linear_P = numeric(), QuasiBinomial_P = numeric())

# Loop through each task
for (task in unique(data$task)) {
  # Filter data for the current task
  task_data = filter(data, task == !!task)
  
  # Run the linear regression
  linear_model = lm(acc_FU ~ acc_BL + LEDD_change, data = task_data, na.action = na.omit)
  linear_p = coef(summary(linear_model))["LEDD_change", "Pr(>|t|)"]  # P-value for LEDD_change in linear model
  
  # Run the GLM (quasi-binomial family for proportions)
  glm_model = glm(acc_FU ~ acc_BL + LEDD_change, data = task_data, family = quasibinomial(), na.action = na.omit)
  glm_p = coef(summary(glm_model))["LEDD_change", "Pr(>|t|)"]  # P-value for LEDD_change in GLM
  
  # Add results to the data frame
  results_abs = rbind(results_abs, data.frame(Task = task, Linear_P = linear_p, QuasiBinomial_P = glm_p))
}

# Print the results table
print(results_abs)

# Filter tasks where Linear_P < 0.05
significant_tasks = results_abs %>% filter(Linear_P < 0.05) %>% pull(Task)

# Generate graphs for significant tasks
for (task in significant_tasks) {
  task_data = filter(data, task == !!task)
  
  # Create the scatter plot

  plot = ggplot(task_data, aes(x = LEDD_change, y = Accuracy_change)) +
    geom_point(size = 3, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) + 
    labs(
      title = paste("Abs. Change in DA-LEDD vs. Change in Accuracy (Task", task, ")"),
      x = "Change in DA-LEDD",
      y = "Change in Accuracy"
    ) +
    theme_minimal()
  
  # Display the plot
  print(plot)
}

# RELATIVE CHANGES ############################################

# Calculate LEDD_rel_change and Accuracy_rel_change
data = data %>%
  mutate(
    LEDD_rel_change = (as.numeric(LEDD_FU) - as.numeric(LEDD_BL))/as.numeric(LEDD_BL),
    Accuracy_rel_change = (acc_FU - acc_BL)/acc_BL  # Change in accuracy
  )

# Create an empty results data frame
results_rel = data.frame(Task = numeric(), Linear_P = numeric(), QuasiBinomial_P = numeric())

# Loop through each task
for (task in unique(data$task)) {
  # Filter data for the current task
  task_data = filter(data, task == !!task)
  
  # Run the linear regression
  linear_model = lm(acc_FU ~ acc_BL + LEDD_rel_change, data = task_data, na.action = na.omit)
  linear_p = coef(summary(linear_model))["LEDD_rel_change", "Pr(>|t|)"]  # P-value for LEDD_rel_change in linear model
  
  # Run the GLM (quasi-binomial family for proportions)
  glm_model = glm(acc_FU ~ acc_BL + LEDD_rel_change, data = task_data, family = quasibinomial(), na.action = na.omit)
  glm_p = coef(summary(glm_model))["LEDD_rel_change", "Pr(>|t|)"]  # P-value for LEDD_rel_change in GLM
  
  # Add results to the data frame
  results_rel = rbind(results_rel, data.frame(Task = task, Linear_P = linear_p, QuasiBinomial_P = glm_p))
}

# Print the results table
print(results_rel)

# Filter tasks where Linear_P < 0.05
significant_tasks = results_rel %>% filter(Linear_P < 0.05) %>% pull(Task)

# Generate graphs for significant tasks
for (task in significant_tasks) {
  task_data = filter(data, task == !!task)
  
  # Create the scatter plot

  plot = ggplot(task_data, aes(x = LEDD_rel_change, y = Accuracy_rel_change)) +
    geom_point(size = 3, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) + 
    labs(
      title = paste("Rel. Change in DA-LEDD vs. Change in Accuracy (Task", task, ")"),
      x = "Change in DA-LEDD",
      y = "Change in Accuracy"
    ) +
    theme_minimal()
  
  # Display the plot
  print(plot)
}



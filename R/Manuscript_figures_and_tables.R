#'*Read in libraries*
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(lubridate)
library(PMCMRplus)
library(FSA)

#'*Table 1 - Demographics table*
# Function to create a table with counts and percentages
create_table <- function(data, var_name, total_users) {
  table_data <- table(data[[var_name]]) %>%
    as.data.frame() %>%
    rename(Subgroup = Var1, Count = Freq) %>%
    mutate(Percentage = round((Count / total_users) * 100, 1))
  table_data <- cbind(Variable = var_name, table_data)
  return(table_data)
}

if(file.exists('Cleaned_model_user_info_all.rds'))
{
  Cleaned_model_user_info_all                         <- read_rds('Cleaned_model_user_info_all.rds')
  Cleaned_model_survey_data                           <- read_rds('Cleaned_model_survey_data.rds')
  Full_menstrual_symptom_data_all_cycles_within_range <- read_rds('Full_menstrual_symptom_data_all_cycles_within_range.rds')  
  
  # Total users
  total_users <- 948
  
  # read in data from file
  # check if file exists in if statement
  
  # Create tables for each variable
  gender_table <- create_table(Cleaned_model_user_info_all, "gender_id", total_users)
  disabled_table <- create_table(Cleaned_model_user_info_all, "disabled", total_users)
  disease_type_table <- create_table(Cleaned_model_user_info_all, "disease_type_simple", total_users)
  age_cat_table <- create_table(Cleaned_model_user_info_all, "age_category", total_users)
  
  # Filter and create tables for survey data
  contraception_users <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 1) %>%
    mutate(answer = ifelse(answer == "No", "None", answer))
  contraception_table <- create_table(contraception_users, "answer", total_users)
  
  gravidity_users <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 4)
  gravidity_table <- create_table(gravidity_users, "answer", total_users)
  
  parity_users <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 5)
  parity_table <- create_table(parity_users, "answer", total_users)
  
  # Combine all tables
  final_table <- bind_rows(gender_table, disabled_table, disease_type_table, age_cat_table, 
                           contraception_table, gravidity_table, parity_table)
  
  # Save the final table as a CSV file
  write.csv(final_table, "demographics_table.csv", row.names = FALSE)
  
  #'*Prepare data for Figure 2B-G and negative binomial mixed effects regression model*
  
  #Menstrual phase & overall symptom score data prep
  
  Overall_symptom_scores_model_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(observation_date_pk, user_id_pk, Menstrual_phase, cycle_id) %>%
    summarize(overall_score = sum(observation_value))
  
  Overall_symptom_scores_model_data_user_info <- Overall_symptom_scores_model_data %>%
    inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
  
  Overall_symptom_scores_model_data_user_info_model <- Overall_symptom_scores_model_data_user_info %>%
    mutate(
      Contraception_type = as.factor(Contraception_type),
      Contraception_type = relevel(Contraception_type, ref = "None"),
      Menstrual_phase = factor(Menstrual_phase, levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    )
  
  #Add in quarters post-joining 
  
  Overall_symptom_scores_model_data_user_info_model <- Overall_symptom_scores_model_data_user_info_model %>%
    mutate(
      observation_date_pk = as.Date(observation_date_pk),
      weeks_post_joining = as.numeric(difftime(observation_date_pk, user_created_date, units = "weeks"))
    )
  
  Overall_symptom_scores_model_data_user_info_model <- Overall_symptom_scores_model_data_user_info_model %>%
    mutate(quarters_post_joining = ceiling(pmax(1, interval(user_created_date, observation_date_pk) / months(3))))
  
  Overall_symptom_scores_model_data_user_info_model <- Overall_symptom_scores_model_data_user_info_model %>%
    mutate(quarters_post_joining = as.factor(quarters_post_joining))
  
  user_count <- n_distinct(Overall_symptom_scores_model_data_user_info_model$user_id_pk)
  cycle_count <- n_distinct(Overall_symptom_scores_model_data_user_info_model$cycle_id)
  
  #'*Figure 2A - Histogram of mean symptom score*
  
  # Define common y-axis limits
  y_axis_limits <- c(13.0, 24.0)
  
  # Define a consistent theme for all plots
  common_theme <- theme(
    plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "cm"), # Same margins for all plots
    axis.title = element_text(size = 6, face = "bold"),
    axis.text = element_text(size = 6, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.75)
  )
  
  Mean_symptoms_scores_users <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  Mean_symptom_scores_histogram <- ggplot(Mean_symptoms_scores_users, aes(x = mean_symptom_score)) +
    geom_histogram(binwidth = 3, fill = "#008B8B", color = "#008B8B", alpha = 0.7) +
    labs(x = "Mean Symptom Score",
         y = "Frequency") +
    theme_pubr() + common_theme
  
  ggsave("Mean_symptom_scores_histogram.svg", plot = Mean_symptom_scores_histogram, width = 6, height = 5, units = "cm")
  
  # Calculate the mean and variance 
  mean_symptom_score_mean <- mean(Mean_symptoms_scores_users$mean_symptom_score, na.rm = TRUE)
  mean_symptom_score_variance <- var(Mean_symptoms_scores_users$mean_symptom_score, na.rm = TRUE)
  
  #'*Figure 2B - Menstrual phase symptom score scatter plot*
  
  Mean_symptoms_scores <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, Menstrual_phase) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # Calculate the number of unique menstrual phase values for each user
  unique_values <- Mean_symptoms_scores %>%
    group_by(user_id_pk) %>%
    summarize(num_unique_values = length(unique(Menstrual_phase)))
  
  # Filter out users who do not have values for all 5 menstrual phases (n=4)
  filtered_users <- unique_values %>%
    filter(num_unique_values == 5)
  
  # Use the filtered list of user IDs to subset original dataframe
  Filtered_mean_symptom_scores <- Mean_symptoms_scores %>%
    filter(user_id_pk %in% filtered_users$user_id_pk)
  
  #Shapiro wilk test 
  shapiro.test(Filtered_mean_symptom_scores$mean_symptom_score)
  
  # Calculate means and SEMs
  mean_sem <- Filtered_mean_symptom_scores %>%
    group_by(Menstrual_phase) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  # Plot means with SEM error bars
  scatter_plot_phase_symptom_score <- ggplot(mean_sem, aes(x = Menstrual_phase, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Menstrual Phase",
         y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = c(16.2, 18.0)) 
  
  ggsave("scatter_plot_phase_symptom_score.svg", plot = scatter_plot_phase_symptom_score, width = 6, height = 5, units = "cm")
  
  #Perform Friedman test
  Filtered_mean_symptom_scores$user_id_pk <- factor(Filtered_mean_symptom_scores$user_id_pk)
  
  friedman_test_mean_symptom_score <- friedman.test(mean_symptom_score ~ Menstrual_phase | user_id_pk, data = Filtered_mean_symptom_scores)
  
  #' Perform post-hoc pairwise comparisons using the Nemenyi test
  nemenyi_mean_symptom_score <- frdAllPairsNemenyiTest(Filtered_mean_symptom_scores$mean_symptom_score, Filtered_mean_symptom_scores$Menstrual_phase, blocks = Filtered_mean_symptom_scores$user_id_pk)
  
  #'*Figure 2C - Disease type symptom score scatter plot*
  
  Overall_symptom_scores_disease_means_only <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, disease_type_simple) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # '*Look at overall symptom scores and disease categories*
  mean_sem <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, disease_type_simple) %>%
    summarize(mean_symptom_score = mean(overall_score)) %>%
    group_by(disease_type_simple) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  scatter_plot_phase_symptom_score_disease_only <- ggplot(mean_sem, aes(x = disease_type_simple, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), 
                  linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Disease type", y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = y_axis_limits)
  
  ggsave("scatter_plot_phase_symptom_score_disease_only.svg", 
         plot = scatter_plot_phase_symptom_score_disease_only, width = 6, height = 5, units = "cm")
  
  # Kruskal-Wallis test to compare symptom scores between disease types
  kruskal_test_result <- kruskal.test(mean_symptom_score ~ disease_type_simple, data = Overall_symptom_scores_disease_means_only)
  kruskal_test_result
  
  # Post-hoc pairwise comparisons for Kruskal-Wallis test
  pairwise_dunn_test_result <- 
    dunnTest(Overall_symptom_scores_disease_means_only$mean_symptom_score ~  Overall_symptom_scores_disease_means_only$disease_type_simple, method = "bonferroni")
  
  pairwise_dunn_test_result 
  
  #'*Figure 2D - Contraception type symptom score scatter plot*
  
  Overall_symptom_scores_contraception_means_only <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, Contraception_type) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # '*Contraception only*
  mean_sem <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, Contraception_type) %>%
    summarize(mean_symptom_score = mean(overall_score)) %>%
    group_by(Contraception_type) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  scatter_plot_phase_symptom_score_contraception_only <- ggplot(mean_sem, aes(x = Contraception_type, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), 
                  linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Contraception type", y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = y_axis_limits)
  
  ggsave("scatter_plot_phase_symptom_score_contraception_only.svg", 
         plot = scatter_plot_phase_symptom_score_contraception_only, width = 6, height = 5, units = "cm")
  
  # Kruskal-Wallis test to compare symptom scores between contraception types
  kruskal_test_result <- kruskal.test(mean_symptom_score ~ Contraception_type, data = Overall_symptom_scores_contraception_means_only)
  kruskal_test_result
  
  # Post-hoc pairwise comparisons 
  pairwise_dunn_test_result <- 
    dunnTest(Overall_symptom_scores_contraception_means_only$mean_symptom_score ~ Overall_symptom_scores_contraception_means_only$Contraception_type, method = "bonferroni")
  
  pairwise_dunn_test_result 
  
  #'*Figure 2E - Time (as quarters) and symptom score scatter plot*
  
  Overall_symptom_scores_quarters_means_only <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, quarters_post_joining) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # '*Look at overall symptom scores and time categories*
  mean_sem <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, quarters_post_joining) %>%
    summarize(mean_symptom_score = mean(overall_score)) %>%
    group_by(quarters_post_joining) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  scatter_plot_phase_symptom_score_quarters_only <- ggplot(mean_sem, aes(x = quarters_post_joining, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), 
                  linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Quarters post-joining", y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = y_axis_limits)
  
  ggsave("scatter_plot_phase_symptom_score_quarters_only.svg", 
         plot = scatter_plot_phase_symptom_score_quarters_only, width = 6, height = 5, units = "cm")
  
  # Kruskal-Wallis test to compare symptom scores between quarters
  kruskal_test_result <- kruskal.test(mean_symptom_score ~ quarters_post_joining, data = Overall_symptom_scores_quarters_means_only)
  kruskal_test_result
  
  # Post-hoc pairwise comparisons
  pairwise_dunn_test_result <- 
    dunnTest(Overall_symptom_scores_quarters_means_only$mean_symptom_score ~  Overall_symptom_scores_quarters_means_only$quarters_post_joining, method = "bonferroni")
  
  pairwise_dunn_test_result 
  
  #'*Figure 2F - Gravidity symptom score scatter plot*
  
  Overall_symptom_scores_gravidity_means_only <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, nulligravida) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # '*Look at overall symptom scores and gravidity*
  mean_sem <- Overall_symptom_scores_model_data_user_info_model %>%
    mutate(nulligravida = factor(nulligravida, levels = c(0, 1), labels = c("Multigravida", "Nulligravida"))) %>%
    group_by(user_id_pk, nulligravida) %>%
    summarize(mean_symptom_score = mean(overall_score)) %>%
    group_by(nulligravida) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  scatter_plot_phase_symptom_score_gravidity_only <- ggplot(mean_sem, aes(x = nulligravida, y = mean_score)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), 
                  linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Gravidity", y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = y_axis_limits)
  
  ggsave("scatter_plot_phase_symptom_score_gravidity_only.svg", 
         plot = scatter_plot_phase_symptom_score_gravidity_only, width = 6, height = 5, units = "cm")
  
  # Wilcoxon rank-sum test
  wilcox_test_result <- wilcox.test(mean_symptom_score ~ nulligravida, data = Overall_symptom_scores_gravidity_means_only)
  wilcox_test_result
  
  #'*Figure 2G - Age categories symptom score scatter plot*
  
  Overall_symptom_scores_age_means_only <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, age_category) %>%
    summarize(mean_symptom_score = mean(overall_score))
  
  # '*Look at overall symptom scores and age categories*
  mean_sem <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, age_category) %>%
    summarize(mean_symptom_score = mean(overall_score)) %>%
    group_by(age_category) %>%
    summarize(
      mean_score = mean(mean_symptom_score),
      sem_score = sd(mean_symptom_score) / sqrt(n())
    )
  
  scatter_plot_phase_symptom_score_age_only <- ggplot(mean_sem, aes(x = age_category, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), 
                  linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Age Category", y = "Mean Symptom Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = y_axis_limits)
  
  ggsave("scatter_plot_phase_symptom_score_age_only.svg", 
         plot = scatter_plot_phase_symptom_score_age_only, width = 6, height = 5, units = "cm")
  
  # Kruskal-Wallis test to compare symptom scores between age categories
  kruskal_test_result <- kruskal.test(mean_symptom_score ~ age_category, data = Overall_symptom_scores_age_means_only)
  kruskal_test_result
  
  # Post-hoc pairwise comparisons 
  pairwise_dunn_test_result <- 
    dunnTest(Overall_symptom_scores_age_means_only$mean_symptom_score ~ Overall_symptom_scores_age_means_only$age_category, method = "bonferroni")
  
  pairwise_dunn_test_result 
  
  #'*Figure 2G and Table 3 menstrual phase & crashes data prep*
  Crash_model_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(health_variable_id_pk == 60)
  
  Crash_model_data_user_info <- Crash_model_data %>%
    inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
  
  Crash_model_data_user_info_model <- Crash_model_data_user_info  %>%
    mutate(
      Contraception_type = as.factor(Contraception_type),
      Contraception_type = relevel(Contraception_type, ref = "None"),
      Menstrual_phase = factor(Menstrual_phase, levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    )
  
  #Add in quarters post-joining 
  
  Crash_model_data_user_info_model <- Crash_model_data_user_info_model %>%
    mutate(
      observation_date_pk = as.Date(observation_date_pk),
      weeks_post_joining = as.numeric(difftime(observation_date_pk, user_created_date, units = "weeks"))
    )
  
  Crash_model_data_user_info_model <- Crash_model_data_user_info_model %>%
    mutate(quarters_post_joining = ceiling(pmax(1, interval(user_created_date, observation_date_pk) / months(3))))
  
  Crash_model_data_user_info_model <- Crash_model_data_user_info_model %>%
    mutate(quarters_post_joining = as.factor(quarters_post_joining))
  
  n_distinct(Crash_model_data_user_info_model$user_id_pk)
  n_distinct(Crash_model_data_user_info_model$cycle_id)
  
  #'*Figure 2H - Percentage of days classified as a crash*
  
  # Calculate the percentage of days with a crash for each individual and phase
  Percentage_crashes <- Crash_model_data_user_info_model %>%
    group_by(user_id_pk, Menstrual_phase) %>%
    summarize(percentage_crash = mean(observation_value) * 100) %>%
    ungroup()
  
  # Calculate means and SEMs
  mean_sem <- Percentage_crashes %>%
    group_by(Menstrual_phase) %>%
    summarize(
      mean_score = mean(percentage_crash),
      sem_score = sd(percentage_crash) / sqrt(n())
    )
  
  # Plot means with SEM error bars
  scatter_plot_phase_crash <- ggplot(mean_sem, aes(x = Menstrual_phase, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), linewidth = 0.2, width = 0.2, colour = "#c76706") +
    geom_point(size = 1, colour = "#c76706") +
    labs(x = "Menstrual Phase",
         y = "Mean % Days with Crash") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = c(19, 25)) 
  
  ggsave("scatter_plot_phase_crash.svg", plot = scatter_plot_phase_crash, width = 6, height = 5, units = "cm")
  
  #'*Table 2 - Negative binomial mixed effects regression model for overall symptom score*
  
  # Negative binomial mixed effects regression model 
  
  overall_symptom_scores_model_all_cycles_quarters <- glmer.nb(overall_score ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + quarters_post_joining + (1 | user_id_pk), 
                                                               data = Overall_symptom_scores_model_data_user_info_model)
  
  saveRDS(overall_symptom_scores_model_all_cycles_quarters, file = "overall_symptom_scores_model_all_cycles_quarters.rds")
  
  summary(overall_symptom_scores_model_all_cycles_quarters)
  
  # Extract fixed effects
  overall_symptom_scores_model_all_cycles_quarters_model_summary <- summary(overall_symptom_scores_model_all_cycles_quarters)
  overall_symptom_scores_model_all_cycles_quarters_coef_table <- overall_symptom_scores_model_all_cycles_quarters_model_summary$coefficients
  
  # Create a data frame for fixed effects
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects <- data.frame(
    Variable = rownames(overall_symptom_scores_model_all_cycles_quarters_coef_table),
    `Incidence_rate_ratio` = exp(overall_symptom_scores_model_all_cycles_quarters_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(overall_symptom_scores_model_all_cycles_quarters_coef_table[, 1] - 1.96 * overall_symptom_scores_model_all_cycles_quarters_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(overall_symptom_scores_model_all_cycles_quarters_coef_table[, 1] + 1.96 * overall_symptom_scores_model_all_cycles_quarters_coef_table[, 2]),  # Upper CI
    `P_value` = overall_symptom_scores_model_all_cycles_quarters_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio <- signif(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio, 3)
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_lower <- signif(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_lower, 3)
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_upper <- signif(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_upper, 3)
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$P_value <- signif(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$P_value, 3)
  
  # Save fixed effects as CSV files
  write.csv(overall_symptom_scores_model_all_cycles_quarters_fixed_effects, "overall_symptom_scores_model_all_cycles_quarters_fixed_effects_table.csv", row.names = FALSE)
  
  #'*Table 3 - Binomial mixed effects regression model for crashes*
  
  # All cycles, quarter post-joining as factor
  crash_model_all_cycles_quarters <- glmer(observation_value ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + quarters_post_joining + (1 | user_id_pk),
                                           data = Crash_model_data_user_info_model, 
                                           family = binomial(link = "logit"))
  saveRDS(crash_model_all_cycles_quarters, file = "crash_model_all_cycles_quarters.rds")
  summary(crash_model_all_cycles_quarters)
  
  # Extract fixed effects
  crash_model_all_cycles_quarters_model_summary <- summary(crash_model_all_cycles_quarters)
  crash_model_all_cycles_quarters_coef_table <- crash_model_all_cycles_quarters_model_summary$coefficients
  
  # Create a data frame for fixed effects
  crash_model_all_cycles_quarters_fixed_effects <- data.frame(
    Variable = rownames(crash_model_all_cycles_quarters_coef_table),
    `Incidence_rate_ratio` = exp(crash_model_all_cycles_quarters_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(crash_model_all_cycles_quarters_coef_table[, 1] - 1.96 * crash_model_all_cycles_quarters_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(crash_model_all_cycles_quarters_coef_table[, 1] + 1.96 * crash_model_all_cycles_quarters_coef_table[, 2]),  # Upper CI
    `P_value` = crash_model_all_cycles_quarters_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  crash_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio <- signif(crash_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio, 3)
  crash_model_all_cycles_quarters_fixed_effects$CI_lower <- signif(crash_model_all_cycles_quarters_fixed_effects$CI_lower, 3)
  crash_model_all_cycles_quarters_fixed_effects$CI_upper <- signif(crash_model_all_cycles_quarters_fixed_effects$CI_upper, 3)
  crash_model_all_cycles_quarters_fixed_effects$P_value <- signif(crash_model_all_cycles_quarters_fixed_effects$P_value, 3)
  
  # Save fixed effects as CSV files
  write.csv(crash_model_all_cycles_quarters_fixed_effects, "crash_model_all_cycles_quarters_fixed_effects_table.csv", row.names = FALSE)
} else {
  print("If you would like to request these files for scientific purposes, contact Visible Health Inc at info@makevisible.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}

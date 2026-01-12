#'*Read in libraries*
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(lubridate)
library(PMCMRplus)
library(FSA)
library(svglite)
library(ordinal)


if(file.exists('Cleaned_model_user_info_all.rds'))
{
  Cleaned_model_user_info_all                         <- read_rds('Cleaned_model_user_info_all.rds')
  Cleaned_model_survey_data                           <- read_rds('Cleaned_model_survey_data.rds')
  Full_menstrual_symptom_data_all_cycles_within_range <- read_rds('Full_menstrual_symptom_data_all_cycles_within_range.rds')  
  
  # Total users
  total_users <- 948
  
  #'*Table 1: Demographics of disease cohort*
  # Function to create a table with counts and percentages
  create_table <- function(data, var_name, total_users) {
    table_data <- table(data[[var_name]]) %>%
      as.data.frame() %>%
      rename(Subgroup = Var1, Count = Freq) %>%
      mutate(Percentage = round((Count / total_users) * 100, 1))
    table_data <- cbind(Variable = var_name, table_data)
    return(table_data)
  }
  
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
  write.csv(final_table, "figures_and_tables/Main_manuscript/Table_1_R.csv", row.names = FALSE)
  
  #'*Define common theme for Figure 2*
  
  # Define common y-axis limits
  y_axis_limits <- c(13.0, 24.0)
  
  # Define a consistent theme for all plots
  common_theme <- theme(
    plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "cm"), # Same margins for all plots
    axis.title = element_text(size = 6, face = "bold"),
    axis.text = element_text(size = 6, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.75)
  )
  
  #'*Figure 2A-C and Table 2 menstrual phase & crashes data prep*
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
  
  #'*Figure 2A - Crash days by menstrual phase scatter plot*
  
  # Calculate the percentage of days with a crash for each individual and phase
  Percentage_crashes <- Crash_model_data_user_info_model %>%
    group_by(user_id_pk, Menstrual_phase) %>%
    summarize(percentage_crash = mean(observation_value) * 100) %>%
    ungroup()
  
  # Step 1: Count unique phases per user
  unique_values <- Percentage_crashes %>%
    group_by(user_id_pk) %>%
    summarise(num_unique_values = n_distinct(Menstrual_phase), .groups = "drop")
  
  # Step 2: Keep only users with all 5 phases
  filtered_users <- unique_values %>%
    filter(num_unique_values == 5)
  
  # Step 3: Subset original data
  Percentage_crashes_friedman <- Percentage_crashes %>%
    filter(user_id_pk %in% filtered_users$user_id_pk)
  
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
    scale_y_continuous(limits = c(14, 30)) 
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2A.svg", plot = scatter_plot_phase_crash, width = 6, height = 5, units = "cm")
  
  
  #'*Figure 2B - Crash days by disease type scatter plot*
  
  # 1. Compute % crashes per user per disease type
  Percentage_crashes_by_disease <- Crash_model_data_user_info_model %>%
    group_by(user_id_pk, disease_type_simple) %>%
    summarize(percentage_crash = mean(observation_value) * 100) %>%
    ungroup()
  
  # 2. Compute mean + SEM across disease types
  mean_sem_disease <- Percentage_crashes_by_disease %>%
    group_by(disease_type_simple) %>%
    summarize(
      mean_score = mean(percentage_crash),
      sem_score = sd(percentage_crash) / sqrt(n())
    )
  
  # 3. Plot means with SEM error bars
  scatter_plot_phase_crash_disease <- ggplot(mean_sem_disease, aes(x = disease_type_simple, y = mean_score)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), linewidth = 0.2, width = 0.2, colour = "#c76706") +
    geom_point(size = 1, colour = "#c76706") +
    labs(x = "Disease type",
         y = "Mean % Days with Crash") +
    theme_pubr() + 
    common_theme +
    scale_y_continuous(limits = c(14, 30))
  
  # 4. Save figure
  ggsave("figures_and_tables/Main_manuscript/Figure_2B.svg",
         plot = scatter_plot_phase_crash_disease,
         width = 6, height = 5, units = "cm")
  
  #'*Figure 2C - Crash days by contraception type scatter plot*
  
  Percentage_crashes_by_contraception <- Crash_model_data_user_info_model %>%
    group_by(user_id_pk, Contraception_type) %>%
    summarize(percentage_crash = mean(observation_value) * 100) %>%
    ungroup()
  
  # Calculate means and SEMs
  mean_sem <- Percentage_crashes_by_contraception %>%
    group_by(Contraception_type) %>%
    summarize(
      mean_score = mean(percentage_crash),
      sem_score = sd(percentage_crash) / sqrt(n())
    )
  
  # Plot means with SEM error bars

  scatter_plot_phase_crash_contraception <- ggplot(mean_sem, aes(x = Contraception_type, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), linewidth = 0.2, width = 0.2, colour = "#c76706") +
    geom_point(size = 1, colour = "#c76706") +
    labs(x = "Contraception type", y = "Mean % Days with Crash") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = c(14, 30)) +
    scale_x_discrete(
      limits = c("None", "Progestin only", "Estrogen & Progestin"),
      labels = c(
        "None" = "None",
        "Progestin only" = "Progestin only",
        "Estrogen & Progestin" = "Oestrogen & Progestin"))
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2C.svg", plot = scatter_plot_phase_crash_contraception, width = 6, height = 5, units = "cm")

  
  #'*Prepare data for Figure 2D-I and negative binomial mixed effects regression model*
  
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
  
  #Add in Z score 
  
  Overall_symptom_scores_model_data_user_info_model <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk) %>%
    mutate(
      mean_symptom = mean(overall_score, na.rm = TRUE),
      sd_symptom = sd(overall_score, na.rm = TRUE),
      z_score = (overall_score - mean_symptom) / sd_symptom
    ) %>%
    ungroup()
  
  user_count <- n_distinct(Overall_symptom_scores_model_data_user_info_model$user_id_pk)
  cycle_count <- n_distinct(Overall_symptom_scores_model_data_user_info_model$cycle_id)
  
  
  #'*Figure 2D - Overall symptom score by menstrual phase (Z-score) scatter plot*
  Mean_symptoms_scores <- Overall_symptom_scores_model_data_user_info_model %>%
    group_by(user_id_pk, Menstrual_phase) %>%
    summarize(mean_z_score = mean(z_score, na.rm = TRUE))
  
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
  shapiro.test(Filtered_mean_symptom_scores$mean_z_score)
  
  # Calculate means and SEMs
  mean_sem <- Filtered_mean_symptom_scores %>%
    group_by(Menstrual_phase) %>%
    summarize(
      mean_score = mean(mean_z_score),
      sem_score = sd(mean_z_score) / sqrt(n())
    )
  
  # Plot means with SEM error bars
  scatter_plot_phase_symptom_score <- ggplot(mean_sem, aes(x = Menstrual_phase, y = mean_score, group = 1)) +
    geom_errorbar(aes(ymin = mean_score - sem_score, ymax = mean_score + sem_score), linewidth = 0.2, width = 0.2, colour = "#008B8B") +
    geom_point(size = 1, colour = "#008B8B") +
    labs(x = "Menstrual Phase", y = "Mean Symptom Z-Score") +
    theme_pubr() + common_theme +
    scale_y_continuous(limits = c(-0.1, 0.1))
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2D.svg", plot = scatter_plot_phase_symptom_score, width = 6, height = 5, units = "cm")
  
  #'*Figure 2E - Overall symptom score by disease type scatter plot*
  
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
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2E.svg", 
         plot = scatter_plot_phase_symptom_score_disease_only, width = 6, height = 5, units = "cm")

  #'*Figure 2F - Overall symptom score by contraception type scatter plot*
  
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
    scale_y_continuous(limits = y_axis_limits) +
    scale_x_discrete(
      limits = c("None", "Progestin only", "Estrogen & Progestin"),
      labels = c(
        "None" = "None",
        "Progestin only" = "Progestin only",
        "Estrogen & Progestin" = "Oestrogen & Progestin"
      ))
    
  ggsave("figures_and_tables/Main_manuscript/Figure_2F.svg", 
         plot = scatter_plot_phase_symptom_score_contraception_only, width = 6, height = 5, units = "cm")
  
  #'*Figure 2G - Overall symptom score by age category scatter plot*
  
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
    scale_y_continuous(limits = y_axis_limits) + 
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2G.svg", 
         plot = scatter_plot_phase_symptom_score_age_only, width = 6, height = 5, units = "cm")
  
 
  #'*Figure 2H - Overall symptom score by time (quarters post-joining) scatter plot*
  
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
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2H.svg", 
         plot = scatter_plot_phase_symptom_score_quarters_only, width = 6, height = 5, units = "cm")

  # Identify number of participants in the last quarter
  num_participants_last_quarter <- Overall_symptom_scores_model_data_user_info_model %>%
    filter(quarters_post_joining == 6) %>%
    summarise(n_users = n_distinct(user_id_pk))
  
  num_participants_last_quarter
  
  #'*Figure 2I - Overall symptom score by gravidity scatter plot*
  
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
  
  ggsave("figures_and_tables/Main_manuscript/Figure_2I.svg", 
         plot = scatter_plot_phase_symptom_score_gravidity_only, width = 6, height = 5, units = "cm")
  
  #'*Figure 2 - Perform statistical tests and adjust p values for multiple comparisons*
  
  #Figure 2A
  Percentage_crashes_friedman$user_id_pk <- factor(Percentage_crashes_friedman$user_id_pk)
  friedman_test_crashes_phase <- friedman.test(percentage_crash ~ Menstrual_phase | user_id_pk, data = Percentage_crashes_friedman)

  #Figure 2B
  kruskal_test_result_disease_crash <- kruskal.test(percentage_crash ~ disease_type_simple, data = Percentage_crashes_by_disease)
  
  #Figure 2C
  kruskal_test_result_contraception_crash <- kruskal.test(percentage_crash ~ Contraception_type, data = Percentage_crashes_by_contraception)
  
  #Figure 2D
  Filtered_mean_symptom_scores$user_id_pk <- factor(Filtered_mean_symptom_scores$user_id_pk)
  friedman_test_mean_symptom_score <- friedman.test(mean_z_score ~ Menstrual_phase | user_id_pk, data = Filtered_mean_symptom_scores)
  
  #Figure 2E
  kruskal_test_result_disease <- kruskal.test(mean_symptom_score ~ disease_type_simple, data = Overall_symptom_scores_disease_means_only)
  
  #Figure 2F
  kruskal_test_result_contraception <- kruskal.test(mean_symptom_score ~ Contraception_type, data = Overall_symptom_scores_contraception_means_only)
  
  #Figure 2G
  kruskal_test_result_age <- kruskal.test(mean_symptom_score ~ age_category, data = Overall_symptom_scores_age_means_only)
  
  #Figure 2H
  kruskal_test_result_quarters <- kruskal.test(mean_symptom_score ~ quarters_post_joining, data = Overall_symptom_scores_quarters_means_only)
  
  #Figure 2I
  wilcox_test_result_nulligravidity <- wilcox.test(mean_symptom_score ~ nulligravida, data = Overall_symptom_scores_gravidity_means_only)
  
  # Extract p-values from all tests
  p_values <- c(
    friedman_crashes_phase = friedman_test_crashes_phase$p.value,
    kruskal_crash_disease = kruskal_test_result_disease_crash$p.value,
    kruskal_crash_contraception = kruskal_test_result_contraception_crash$p.value,
    friedman_symptom_phase = friedman_test_mean_symptom_score$p.value,
    kruskal_symptom_disease = kruskal_test_result_disease$p.value,
    kruskal_symptom_contraception = kruskal_test_result_contraception$p.value,
    kruskal_symptom_age = kruskal_test_result_age$p.value,
    kruskal_symptom_quarters = kruskal_test_result_quarters$p.value,
    wilcox_nulligravidity = wilcox_test_result_nulligravidity$p.value
  )
  
  # Adjust p-values for multiple comparisons
  p_values_adjusted <- p.adjust(p_values, method = "BH")
  
  # Round to 3 decimal places
  p_values_rounded <- round(p_values_adjusted, 3)
  
  # Create a data frame
  figure2_p_values <- data.frame(
    Test = names(p_values_rounded),
    Adjusted_p_value = p_values_rounded,
    row.names = NULL
  )
  
  figure2_p_values
  
  # Save the adjusted p-values to CSV
  write.csv(
    figure2_p_values,
    "figures_and_tables/Main_manuscript/Figure2_p_values.csv",
    row.names = FALSE
  )
  
  #'*Table 2 - Binomial mixed effects regression model for crashes*
  
  # All cycles, quarter post-joining as factor
  crash_model_all_cycles_quarters <- glmer(observation_value ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + quarters_post_joining + (1 | user_id_pk),
                                           data = Crash_model_data_user_info_model, 
                                           family = binomial(link = "logit"))
  saveRDS(crash_model_all_cycles_quarters, file = "figures_and_tables/Models/crash_model_all_cycles_quarters.rds")
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
  
  # Round the numeric columns to 3 decimal places
  crash_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio <-
    round(crash_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio, 3)
  
  crash_model_all_cycles_quarters_fixed_effects$CI_lower <-
    round(crash_model_all_cycles_quarters_fixed_effects$CI_lower, 3)
  
  crash_model_all_cycles_quarters_fixed_effects$CI_upper <-
    round(crash_model_all_cycles_quarters_fixed_effects$CI_upper, 3)
  
  crash_model_all_cycles_quarters_fixed_effects$P_value <-
    round(crash_model_all_cycles_quarters_fixed_effects$P_value, 3)
  
  # Special formatting for small p-values
  crash_model_all_cycles_quarters_fixed_effects$P_value <-
    ifelse(crash_model_all_cycles_quarters_fixed_effects$P_value < 0.001,
           "P < 0.001",
           round(crash_model_all_cycles_quarters_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(crash_model_all_cycles_quarters_fixed_effects, "figures_and_tables/Main_manuscript/Table_2_R.csv", row.names = FALSE)
  
  #'*Table 3 - Negative binomial mixed effects regression model for overall symptom score*
  
  # Negative binomial mixed effects regression model 
  
  overall_symptom_scores_model_all_cycles_quarters <- glmer.nb(overall_score ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + quarters_post_joining + (1 | user_id_pk), 
                                                               data = Overall_symptom_scores_model_data_user_info_model)
  
  saveRDS(overall_symptom_scores_model_all_cycles_quarters, file = "figures_and_tables/Models/overall_symptom_scores_model_all_cycles_quarters.rds")
  
  overall_symptom_scores_model_all_cycles_quarters <-
    readRDS("figures_and_tables/Models/overall_symptom_scores_model_all_cycles_quarters.rds")
  
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
  
  # Round the numeric columns to 3 decimal places
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio <-
    round(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$Incidence_rate_ratio, 3)
  
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_lower <-
    round(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_lower, 3)
  
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_upper <-
    round(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$CI_upper, 3)
  
  # P-values with special formatting for very small values
  overall_symptom_scores_model_all_cycles_quarters_fixed_effects$P_value <-
    ifelse(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$P_value < 0.001,
           "P < 0.001",
           round(overall_symptom_scores_model_all_cycles_quarters_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(overall_symptom_scores_model_all_cycles_quarters_fixed_effects, "figures_and_tables/Main_manuscript/Table_3_R.csv", row.names = FALSE)
  
  #'*Table 4A - The fitting result of fatigue regression models*
  
  #'*Prepare disease cohort (Visible) fatigue data*
  
  #Menstrual phase & overall symptom score data prep
  
  Fatigue_V_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    
    # keep only users who ever tracked fatigue
    group_by(user_id_pk) %>%
    filter(any(health_variable_name == "Fatigue")) %>%
    
    # reduce to one row per user/date, prioritising fatigue rows
    group_by(user_id_pk, observation_date_pk) %>%
    arrange(desc(health_variable_name == "Fatigue")) %>%
    dplyr::slice(1) %>%   
    ungroup() %>%
    
    # add fatigue rating column 
    mutate(
      fatigue = if_else(
        health_variable_name == "Fatigue" & !is.na(observation_value),
        as.integer(observation_value),
        0L  # if not Fatigue or missing
      )
    )
  
  Fatigue_V_data <- Fatigue_V_data %>%
    group_by(user_id_pk) %>%
    mutate(new_cycle_id = dense_rank(cycle_id)) %>%
    ungroup()
  
  
  Fatigue_V_data <- Fatigue_V_data %>%
    inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
  
  # Make variables into factors & choose reference groups | add in disease type
  Fatigue_V_data <- Fatigue_V_data %>%
    mutate(
      fatigue = factor(fatigue,
                       levels = c(0, 1, 2, 3),
                       ordered = TRUE),
      Menstrual_phase   = factor(Menstrual_phase, 
                                 levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      disease_type_simple = factor(disease_type_simple),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  #'*Prepare population cohort (Hertility) fatigue data*
  
  Fatigue_h_data <- Cleaned_h_data %>%
    # keep only users who ever tracked fatigue
    group_by(user_id) %>%
    filter(any(symptom_response == "fatigue")) %>%
    
    # reduce to one row per user/date, prioritising fatigue rows
    group_by(user_id, observation_date) %>%
    arrange(desc(symptom_response == "fatigue")) %>%
    dplyr::slice(1) %>%   
    ungroup() %>%
    
    # add binary fatigue column
    mutate(fatigue = if_else(symptom_response == "fatigue", 1L, 0L, missing = 0L))
  
  n_distinct(Fatigue_h_data$user_id)
  
  # Make variables into factors & choose reference groups 
  Fatigue_h_data <- Fatigue_h_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  #'*Run disease cohort fatigue model with menstrual phase, age, contraception & gravidity*
  
  Fatigue_V_model_clmm <- clmm(fatigue ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + (1 | user_id_pk),
                               data = Fatigue_V_data, 
                               link = "logit",
                               control = clmm.control(maxIter = 40000, gradTol = 1e-4))
  
  saveRDS(Fatigue_V_model_clmm, file = "figures_and_tables/Models/Table_4/Fatigue_V_model_clmm_2.rds")
  
  print(summary(Fatigue_V_model_clmm))
  
  # Extract fixed effects
  Fatigue_V_model_summary <- summary(Fatigue_V_model_clmm)
  Fatigue_V_model_coef_table <- Fatigue_V_model_summary$coefficients
  
  # Create a data frame for fixed effects
  Fatigue_V_model_fixed_effects <- data.frame(
    Variable = rownames(Fatigue_V_model_coef_table),
    `Incidence_rate_ratio` = exp(Fatigue_V_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(Fatigue_V_model_coef_table[, 1] - 1.96 * Fatigue_V_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(Fatigue_V_model_coef_table[, 1] + 1.96 * Fatigue_V_model_coef_table[, 2]),  # Upper CI
    `P_value` = Fatigue_V_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  Fatigue_V_model_fixed_effects$Incidence_rate_ratio <- signif(Fatigue_V_model_fixed_effects$Incidence_rate_ratio, 3)
  Fatigue_V_model_fixed_effects$CI_lower <- signif(Fatigue_V_model_fixed_effects$CI_lower, 3)
  Fatigue_V_model_fixed_effects$CI_upper <- signif(Fatigue_V_model_fixed_effects$CI_upper, 3)
  Fatigue_V_model_fixed_effects$P_value <- signif(Fatigue_V_model_fixed_effects$P_value, 3)
  Fatigue_V_model_fixed_effects$P_value <-
    ifelse(Fatigue_V_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(Fatigue_V_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(Fatigue_V_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4A_disease.csv", row.names = FALSE)
  
  #'*Run population cohort fatigue model with menstrual phase, age, contraception & gravidity*

  # Model with menstrual phase, age, contraception & gravidity
  
  Fatigue_h_data_model <- glmer(fatigue ~ Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id),
                                data = Fatigue_h_data, 
                                family = binomial(link = "logit"))
  saveRDS(Fatigue_h_data_model, file = "figures_and_tables/Models/Table_4/Fatigue_h_data_model.rds")
  
  # Extract fixed effects
  Fatigue_h_data_model_summary <- summary(Fatigue_h_data_model)
  Fatigue_h_data_model_coef_table <- Fatigue_h_data_model_summary$coefficients
  
  # Create a data frame for fixed effects
  Fatigue_h_data_model_fixed_effects <- data.frame(
    Variable = rownames(Fatigue_h_data_model_coef_table),
    `Incidence_rate_ratio` = exp(Fatigue_h_data_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(Fatigue_h_data_model_coef_table[, 1] - 1.96 * Fatigue_h_data_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(Fatigue_h_data_model_coef_table[, 1] + 1.96 * Fatigue_h_data_model_coef_table[, 2]),  # Upper CI
    `P_value` = Fatigue_h_data_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  Fatigue_h_data_model_fixed_effects$Incidence_rate_ratio <- signif(Fatigue_h_data_model_fixed_effects$Incidence_rate_ratio, 3)
  Fatigue_h_data_model_fixed_effects$CI_lower <- signif(Fatigue_h_data_model_fixed_effects$CI_lower, 3)
  Fatigue_h_data_model_fixed_effects$CI_upper <- signif(Fatigue_h_data_model_fixed_effects$CI_upper, 3)
  Fatigue_h_data_model_fixed_effects$P_value <- signif(Fatigue_h_data_model_fixed_effects$P_value, 3)
  Fatigue_h_data_model_fixed_effects$P_value <-
    ifelse(Fatigue_h_data_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(Fatigue_h_data_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(Fatigue_h_data_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4A_population.csv", row.names = FALSE)
  
  #'*Table 4B - The fitting result of brain fog regression models*
  
  #'*Prepare disease cohort (Visible) brain fog data*
  
  #Menstrual phase & overall symptom score data prep
  
  brain_fog_V_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    
    # keep only users who ever tracked brain_fog
    group_by(user_id_pk) %>%
    filter(any(health_variable_name == "Brain Fog")) %>%
    
    # reduce to one row per user/date, prioritising brain_fog rows
    group_by(user_id_pk, observation_date_pk) %>%
    arrange(desc(health_variable_name == "Brain Fog")) %>%
    dplyr::slice(1) %>%   
    ungroup() %>%
    
    # add brain_fog rating column 
    mutate(
      brain_fog = if_else(
        health_variable_name == "Brain Fog" & !is.na(observation_value),
        as.integer(observation_value),
        0L  # if not brain_fog or missing
      )
    )
  
  brain_fog_V_data <- brain_fog_V_data %>%
    group_by(user_id_pk) %>%
    mutate(new_cycle_id = dense_rank(cycle_id)) %>%
    ungroup()
  
  
  brain_fog_V_data <- brain_fog_V_data %>%
    inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
  
  # Make variables into factors & choose reference groups 
  brain_fog_V_data <- brain_fog_V_data %>%
    mutate(
      brain_fog = factor(brain_fog,
                         levels = c(0, 1, 2, 3),
                         ordered = TRUE),
      Menstrual_phase   = factor(Menstrual_phase, 
                                 levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      disease_type_simple = factor(disease_type_simple),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  #'*Prepare population cohort (Hertility) brain fog data*
  
  brain_fog_h_data <- Cleaned_h_data %>%
    # keep only users who ever tracked brain_fog
    group_by(user_id) %>%
    filter(any(symptom_response == "brain-fog")) %>%
    
    # reduce to one row per user/date, prioritising brain_fog rows
    group_by(user_id, observation_date) %>%
    arrange(desc(symptom_response == "brain-fog")) %>%
    dplyr::slice(1) %>%   
    ungroup() %>%
    
    # add binary brain_fog column
    mutate(brain_fog = if_else(symptom_response == "brain-fog", 1L, 0L, missing = 0L))
  
  n_distinct(brain_fog_h_data$user_id)
  
  # Make variables into factors & choose reference groups 
  brain_fog_h_data <- brain_fog_h_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  
  #'*Run disease cohort brain fog model with menstrual phase, age, contraception & gravidity*
  
  brain_fog_V_model_clmm <- clmm(brain_fog ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + (1 | user_id_pk),
                                 data = brain_fog_V_data, 
                                 link = "logit",
                                 control = clmm.control(maxIter = 40000, gradTol = 1e-4))
  
  saveRDS(brain_fog_V_model_clmm, file = "figures_and_tables/Models/Table_4/brain_fog_V_model_clmm.rds")
  
  
  # Extract fixed effects
  brain_fog_V_model_summary <- summary(brain_fog_V_model_clmm)
  brain_fog_V_model_coef_table <- brain_fog_V_model_summary$coefficients
  
  # Create a data frame for fixed effects
  brain_fog_V_model_fixed_effects <- data.frame(
    Variable = rownames(brain_fog_V_model_coef_table),
    `Incidence_rate_ratio` = exp(brain_fog_V_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(brain_fog_V_model_coef_table[, 1] - 1.96 * brain_fog_V_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(brain_fog_V_model_coef_table[, 1] + 1.96 * brain_fog_V_model_coef_table[, 2]),  # Upper CI
    `P_value` = brain_fog_V_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  brain_fog_V_model_fixed_effects$Incidence_rate_ratio <- signif(brain_fog_V_model_fixed_effects$Incidence_rate_ratio, 3)
  brain_fog_V_model_fixed_effects$CI_lower <- signif(brain_fog_V_model_fixed_effects$CI_lower, 3)
  brain_fog_V_model_fixed_effects$CI_upper <- signif(brain_fog_V_model_fixed_effects$CI_upper, 3)
  brain_fog_V_model_fixed_effects$P_value <- signif(brain_fog_V_model_fixed_effects$P_value, 3)
  brain_fog_V_model_fixed_effects$P_value <-
    ifelse(brain_fog_V_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(brain_fog_V_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(brain_fog_V_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4B_disease.csv", row.names = FALSE)
  
  print(brain_fog_V_model_summary)
  
  #'*Run population cohort brain fog model with menstrual phase, age, contraception & gravidity*
  
  brain_fog_h_data_model <- glmer(brain_fog ~ Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id),
                                  data = brain_fog_h_data, 
                                  family = binomial(link = "logit"))
  saveRDS(brain_fog_h_data_model, file = "figures_and_tables/Models/Table_4/brain_fog_h_data_model.rds")
  
  # Extract fixed effects
  brain_fog_h_data_model_summary <- summary(brain_fog_h_data_model)
  brain_fog_h_data_model_coef_table <- brain_fog_h_data_model_summary$coefficients
  
  # Create a data frame for fixed effects
  brain_fog_h_data_model_fixed_effects <- data.frame(
    Variable = rownames(brain_fog_h_data_model_coef_table),
    `Incidence_rate_ratio` = exp(brain_fog_h_data_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(brain_fog_h_data_model_coef_table[, 1] - 1.96 * brain_fog_h_data_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(brain_fog_h_data_model_coef_table[, 1] + 1.96 * brain_fog_h_data_model_coef_table[, 2]),  # Upper CI
    `P_value` = brain_fog_h_data_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  brain_fog_h_data_model_fixed_effects$Incidence_rate_ratio <- signif(brain_fog_h_data_model_fixed_effects$Incidence_rate_ratio, 3)
  brain_fog_h_data_model_fixed_effects$CI_lower <- signif(brain_fog_h_data_model_fixed_effects$CI_lower, 3)
  brain_fog_h_data_model_fixed_effects$CI_upper <- signif(brain_fog_h_data_model_fixed_effects$CI_upper, 3)
  brain_fog_h_data_model_fixed_effects$P_value <- signif(brain_fog_h_data_model_fixed_effects$P_value, 3)
  brain_fog_h_data_model_fixed_effects$P_value <-
    ifelse(brain_fog_h_data_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(brain_fog_h_data_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(brain_fog_h_data_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4B_population.csv", row.names = FALSE)
  
  #'*Table 4C - The fitting result of headache regression models*
  
  #'*Prepare Visible headache data*
  
  #Menstrual phase & overall symptom score data prep
  
  headache_V_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    
    # keep only users who ever tracked headache
    group_by(user_id_pk) %>%
    filter(any(health_variable_name == "Headache")) %>%
    
    # reduce to one row per user/date, prioritising headache rows
    group_by(user_id_pk, observation_date_pk) %>%
    arrange(desc(health_variable_name == "Headache")) %>%
    dplyr::slice(1) %>%   
    ungroup() %>%
    
    # add headache rating column 
    mutate(
      headache = if_else(
        health_variable_name == "Headache" & !is.na(observation_value),
        as.integer(observation_value),
        0L  # if not headache or missing
      )
    )
  
  headache_V_data <- headache_V_data %>%
    group_by(user_id_pk) %>%
    mutate(new_cycle_id = dense_rank(cycle_id)) %>%
    ungroup()
  
  
  headache_V_data <- headache_V_data %>%
    inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
  
  # Make variables into factors & choose reference groups 
  headache_V_data <- headache_V_data %>%
    mutate(
      headache = factor(headache,
                        levels = c(0, 1, 2, 3),
                        ordered = TRUE),
      Menstrual_phase   = factor(Menstrual_phase, 
                                 levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      disease_type_simple = factor(disease_type_simple),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  #'*Prepare Hertility headache data*
  
  headache_h_data <- Cleaned_h_data %>%
    # keep only users who ever tracked headache
    group_by(user_id) %>%
    filter(any(symptom_name == "headache")) %>%
    
    # reduce to one row per user/date, prioritising headache rows
    group_by(user_id, observation_date) %>%
    arrange(desc(symptom_name == "headache")) %>%
    dplyr::slice(1) %>%  
    ungroup() %>%
    
    # add binary headache column
    mutate(headache = if_else(symptom_name == "headache", 1L, 0L, missing = 0L))
  
  n_distinct(headache_h_data$user_id)
  
  # Make variables into factors & choose reference groups 
  headache_h_data <- headache_h_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"))
  
  
  #'*Run disease cohort headache model with menstrual phase, age, contraception & gravidity*
  
  headache_V_model_clmm <- clmm(headache ~ Menstrual_phase + age_category + disease_type_simple + Contraception_type + nulligravida + (1 | user_id_pk),
                                data = headache_V_data, 
                                link = "logit",
                                control = clmm.control(maxIter = 40000, gradTol = 1e-4))
  
  saveRDS(headache_V_model_clmm, file = "figures_and_tables/Models/Table_4/headache_V_model_clmm.rds")
  
  # Extract fixed effects
  headache_V_model_summary <- summary(headache_V_model_clmm)
  headache_V_model_coef_table <- headache_V_model_summary$coefficients
  
  # Create a data frame for fixed effects
  headache_V_model_fixed_effects <- data.frame(
    Variable = rownames(headache_V_model_coef_table),
    `Incidence_rate_ratio` = exp(headache_V_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(headache_V_model_coef_table[, 1] - 1.96 * headache_V_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(headache_V_model_coef_table[, 1] + 1.96 * headache_V_model_coef_table[, 2]),  # Upper CI
    `P_value` = headache_V_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  headache_V_model_fixed_effects$Incidence_rate_ratio <- signif(headache_V_model_fixed_effects$Incidence_rate_ratio, 3)
  headache_V_model_fixed_effects$CI_lower <- signif(headache_V_model_fixed_effects$CI_lower, 3)
  headache_V_model_fixed_effects$CI_upper <- signif(headache_V_model_fixed_effects$CI_upper, 3)
  headache_V_model_fixed_effects$P_value <- signif(headache_V_model_fixed_effects$P_value, 3)
  headache_V_model_fixed_effects$P_value <-
    ifelse(headache_V_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(headache_V_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(headache_V_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4C_disease.csv", row.names = FALSE)
  
  print(headache_V_model_summary)
  
  #'*Run population cohort headache model with menstrual phase, age, contraception & gravidity*

  headache_h_data_model <- glmer(headache ~ Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id),
                                 data = headache_h_data, 
                                 family = binomial(link = "logit"))
  saveRDS(headache_h_data_model, file = "figures_and_tables/Models/Table_4/headache_h_data_model.rds")
  
  # Extract fixed effects
  headache_h_data_model_summary <- summary(headache_h_data_model)
  headache_h_data_model_coef_table <- headache_h_data_model_summary$coefficients
  
  # Create a data frame for fixed effects
  headache_h_data_model_fixed_effects <- data.frame(
    Variable = rownames(headache_h_data_model_coef_table),
    `Incidence_rate_ratio` = exp(headache_h_data_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(headache_h_data_model_coef_table[, 1] - 1.96 * headache_h_data_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(headache_h_data_model_coef_table[, 1] + 1.96 * headache_h_data_model_coef_table[, 2]),  # Upper CI
    `P_value` = headache_h_data_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  headache_h_data_model_fixed_effects$Incidence_rate_ratio <- signif(headache_h_data_model_fixed_effects$Incidence_rate_ratio, 3)
  headache_h_data_model_fixed_effects$CI_lower <- signif(headache_h_data_model_fixed_effects$CI_lower, 3)
  headache_h_data_model_fixed_effects$CI_upper <- signif(headache_h_data_model_fixed_effects$CI_upper, 3)
  headache_h_data_model_fixed_effects$P_value <- signif(headache_h_data_model_fixed_effects$P_value, 3)
  headache_h_data_model_fixed_effects$P_value <-
    ifelse(headache_h_data_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(headache_h_data_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(headache_h_data_model_fixed_effects, "figures_and_tables/Main_manuscript/Table_4C_population.csv", row.names = FALSE)

  #'*Figure 3: The impact of menstrual cycle phase on severity of the 3 most tracked symptoms in the disease cohort, compared to the population cohort*
  
  #'*Figure 3A: Fatigue, disease cohort*
  
  # ensure Menstrual_phase order and create a labelled ordered factor for fatigue
  Fatigue_V_data <- Fatigue_V_data %>%
    mutate(
      Menstrual_phase = factor(
        Menstrual_phase,
        levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
      ),
      # fatigue currently numeric 0..3 where 3=severe, 0=none
      fatigue_factor = factor(fatigue,
                              levels = c(3, 2, 1, 0),           # numeric levels
                              labels = c("Severe","Moderate","Mild","None"),
                              ordered = TRUE)
    )
  
  # Build contingency table (rows = phases, cols = severity levels)
  fatigue_v_data_table <- xtabs(~ Menstrual_phase + fatigue_factor, data = Fatigue_V_data)
  
  colnames(fatigue_v_data_table)
  
  # Function to generate a dark -> light 4-shade palette
  generate_colour_palette_dark_to_light <- function(base_colour) {
    colorRampPalette(c(base_colour, "white"))(4)
  }
  
  # Colour for fatigue (dark red)
  shades_of_fatigue <- generate_colour_palette_dark_to_light("#e31a1c")
  
  # Check mapping
  print(shades_of_fatigue)   # [1] darkest ... [4] lightest
  
  # Plot 
  pdf("figures_and_tables/Main_manuscript/Figure_3A.pdf", width = 6, height = 5)
  mosaicplot(
    fatigue_v_data_table,
    color = shades_of_fatigue,
    cex.axis = 0.8,
    main = "Fatigue severity across menstrual phases",
    xlab = "Menstrual phase",
    ylab = "Proportion of days"
  )
  dev.off()
  
  #'*Figure 3B: Brain fog, disease cohort*
  
  # ensure Menstrual_phase order and create a labelled ordered factor for brain_fog
  brain_fog_V_data <- brain_fog_V_data %>%
    mutate(
      Menstrual_phase = factor(
        Menstrual_phase,
        levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
      ),
      # brain_fog currently numeric 0..3 where 3=severe, 0=none
      brain_fog_factor = factor(brain_fog,
                                levels = c(3, 2, 1, 0),           # numeric levels
                                labels = c("Severe","Moderate","Mild","None"),
                                ordered = TRUE)
    )
  
  # Build contingency table (rows = phases, cols = severity levels)
  brain_fog_v_data_table <- xtabs(~ Menstrual_phase + brain_fog_factor, data = brain_fog_V_data)
  colnames(brain_fog_v_data_table)
  
  # Colour for brain_fog (purple)
  shades_of_brain_fog <- generate_colour_palette_dark_to_light("#6a3d9a")
  
  # Check mapping
  print(shades_of_brain_fog)   # [1] darkest ... [4] lightest
  
  # Plot 
  pdf("figures_and_tables/Main_manuscript/Figure_3B.pdf", width = 6, height = 5)
  mosaicplot(
    brain_fog_v_data_table,
    color = shades_of_brain_fog,
    cex.axis = 0.8,
    main = "brain_fog severity across menstrual phases",
    xlab = "Menstrual phase",
    ylab = "Proportion of days"
  )
  dev.off()

  #'*Figure 3C: Headache, disease cohort*
  
  # ensure Menstrual_phase order and create a labelled ordered factor for headache
  headache_V_data <- headache_V_data %>%
    mutate(
      Menstrual_phase = factor(
        Menstrual_phase,
        levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
      ),
      # headache currently numeric 0..3 where 3=severe, 0=none
      headache_factor = factor(headache,
                               levels = c(3, 2, 1, 0),           # numeric levels
                               labels = c("Severe","Moderate","Mild","None"),
                               ordered = TRUE)
    )
  
  # Build contingency table (rows = phases, cols = severity levels)
  headache_v_data_table <- xtabs(~ Menstrual_phase + headache_factor, data = headache_V_data)
  colnames(headache_v_data_table)
  
  # Colour for headache (orange)
  shades_of_headache <- generate_colour_palette_dark_to_light("#ff7f00")
  
  # Check mapping
  print(shades_of_headache)   # [1] darkest ... [4] lightest
  
  # Plot 
  pdf("figures_and_tables/Main_manuscript/Figure_3C.pdf", width = 6, height = 5)
  mosaicplot(
    headache_v_data_table,
    color = shades_of_headache,
    cex.axis = 0.8,
    main = "Headache severity across menstrual phases",
    xlab = "Menstrual phase",
    ylab = "Proportion of days"
  )
  dev.off()
  
  #'*Figure 3D: Fatigue, population cohort*
  
  # Create a contingency table of number of fatigues in each menstrual phases 
  
  Fatigue_h_data$Menstrual_phase <- factor(
    Fatigue_h_data$Menstrual_phase,
    levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
  )
  
  fatigue_h_data_table <- xtabs(~ Menstrual_phase + fatigue, data = Fatigue_h_data)
  
  # Rename columns (0 = No fatigue, 1 = Fatigue)
  colnames(fatigue_h_data_table) <- c("No fatigue", "Fatigue")
  
  # Reorder so Fatigue is on top
  fatigue_h_data_table <- fatigue_h_data_table[, c("Fatigue", "No fatigue")]
  
  # Generate colours for spine plot
  shades_of_red <- colorRampPalette(c("#e41a1c", "white"))(2)
  
  #Save as pdf
  pdf("figures_and_tables/Main_manuscript/Figure_3D.pdf", width = 6, height = 5)
  mosaicplot(fatigue_h_data_table, 
             color=shades_of_red, 
             cex.axis=0.8,
             xlab = "Menstrual phase", ylab = "")
  dev.off() 
  
  #'*Figure 3E: Brain fog, population cohort*
  
  # Create a contingency table of number of brain_fogs in each menstrual phases 
  
  brain_fog_h_data$Menstrual_phase <- factor(
    brain_fog_h_data$Menstrual_phase,
    levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
  )
  
  brain_fog_h_data_table <- xtabs(~ Menstrual_phase + brain_fog, data = brain_fog_h_data)
  
  # Rename columns (0 = No brain_fog, 1 = brain_fog)
  colnames(brain_fog_h_data_table) <- c("No brain_fog", "brain_fog")
  
  # Reorder so brain_fog is on top
  brain_fog_h_data_table <- brain_fog_h_data_table[, c("brain_fog", "No brain_fog")]
  
  # Generate colours for spine plot
  shades_of_purple <- colorRampPalette(c("#6a3d9a", "white"))(2)
  
  #Save as pdf
  pdf("figures_and_tables/Main_manuscript/Figure_3E.pdf", width = 6, height = 5)
  mosaicplot(brain_fog_h_data_table, 
             color=shades_of_purple, 
             cex.axis=0.8,
             xlab = "Menstrual phase", ylab = "")
  dev.off() 
  
  
  #'*Figure 3F: Headache, population cohort*
  
  # Create a contingency table of number of headaches in each menstrual phases 
  
  headache_h_data$Menstrual_phase <- factor(
    headache_h_data$Menstrual_phase,
    levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
  )
  
  headache_h_data_table <- xtabs(~ Menstrual_phase + headache, data = headache_h_data)
  
  # Rename columns (0 = No headache, 1 = headache)
  colnames(headache_h_data_table) <- c("No headache", "headache")
  
  # Reorder so headache is on top
  headache_h_data_table <- headache_h_data_table[, c("headache", "No headache")]
  
  # Generate colours for spine plot
  shades_of_orange <- colorRampPalette(c("#ff7f00", "white"))(2)
  
  #Save as pdf
  pdf("figures_and_tables/Main_manuscript/Figure_3F.pdf", width = 6, height = 5)
  mosaicplot(headache_h_data_table, 
             color=shades_of_orange, 
             cex.axis=0.8,
             xlab = "Menstrual phase", ylab = "")
  dev.off() 
  
} else {
  print("If you would like to request these files for scientific purposes, contact Visible Health Inc at info@makevisible.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}

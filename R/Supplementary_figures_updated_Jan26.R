#'*Read in libraries*
library(tidyverse)
library(ggpubr)
library(grDevices)
library(rcompanion)
library(viridis)
library(pheatmap)
library(ordinal)
library(coin)
library(FSA)
library(lubridate)
library(PMCMRplus)

if(file.exists('Cleaned_model_user_info_all.rds'))
{
  Cleaned_model_user_info_all                         <- read_rds('Cleaned_model_user_info_all.rds')
  Cleaned_model_survey_data                           <- read_rds('Cleaned_model_survey_data.rds')
  Full_menstrual_symptom_data_all_cycles_within_range <- read_rds('Full_menstrual_symptom_data_all_cycles_within_range.rds')  
  
  #'*Table S1: Demographics of each filtering stage of the disease cohort cleaning*
  
  #'*Identify demographics of the 1932 users*
  
  # Percentage in each disease group
  round(prop.table(table(Cleaned_user_info_1932$disease_type_simple)) * 100, 2)
  
  # Percentage in each contraception group 
  round(prop.table(table(Cleaned_user_info_1932$Contraception_type)) * 100, 2)
  
  # Percentage nulligravida
  round(prop.table(table(Cleaned_user_info_1932$nulligravida)) * 100, 2)
  
  # Mean age 
  mean(Cleaned_user_info_1932$age, na.rm = TRUE)
  
  # Mean daily symptom score
  Overall_symptom_score_1932 <- Cleaned_app_data_1932 %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(observation_date_pk, user_id_pk) %>%
    summarise(overall_score = sum(observation_value))
  
  mean(Overall_symptom_score_1932$overall_score, na.rm = TRUE)
  
  # Mean chance of crash
  Crash_1932 <- Cleaned_app_data_1932 %>%
    filter(health_variable_id_pk == 60) %>%
    group_by(user_id_pk) %>%
    summarise(crash_rate = mean(observation_value, na.rm = TRUE), .groups = "drop")  # vector for stats
  
  mean_crash_1932 <- mean(Crash_1932$crash_rate, na.rm = TRUE) * 100
  
  mean_crash_1932
  
  #'*Identify demographics of the 1765 users*
  
  # Percentage in each disease group
  round(prop.table(table(Cleaned_user_info_1765$disease_type_simple)) * 100, 2)
  
  # Percentage in each contraception group 
  round(prop.table(table(Cleaned_user_info_1765$Contraception_type)) * 100, 2)
  
  # Percentage nulligravida
  round(prop.table(table(Cleaned_user_info_1765$nulligravida)) * 100, 2)
  
  # Mean age 
  mean(Cleaned_user_info_1765$age, na.rm = TRUE)
  
  # Mean daily symptom score
  Overall_symptom_score_1765 <- Cleaned_app_data_1765 %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(observation_date_pk, user_id_pk) %>%
    summarise(overall_score = sum(observation_value))
  
  mean(Overall_symptom_score_1765$overall_score, na.rm = TRUE)
  
  # Mean chance of crash
  Crash_1765 <- Cleaned_app_data_1765 %>%
    filter(health_variable_id_pk == 60) %>%
    group_by(user_id_pk) %>%
    summarise(crash_rate = mean(observation_value, na.rm = TRUE), .groups = "drop")
  
  mean_crash_1765 <- mean(Crash_1765$crash_rate, na.rm = TRUE) * 100
  
  #'*Identify demographics of the 1109 users*
  
  # Percentage in each disease group
  round(prop.table(table(Cleaned_user_info_1109$disease_type_simple)) * 100, 2)
  
  # Percentage in each contraception group 
  round(prop.table(table(Cleaned_user_info_1109$Contraception_type)) * 100, 2)
  
  # Percentage nulligravida
  round(prop.table(table(Cleaned_user_info_1109$nulligravida)) * 100, 2)
  
  # Mean age 
  mean(Cleaned_user_info_1109$age, na.rm = TRUE)
  
  # Mean daily symptom score
  Overall_symptom_score_1109 <- Cleaned_app_data_1109 %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(observation_date_pk, user_id_pk) %>%
    summarise(overall_score = sum(observation_value))
  
  mean(Overall_symptom_score_1109$overall_score, na.rm = TRUE)
  
  # Mean chance of crash
  Crash_1109 <- Cleaned_app_data_1109 %>%
    filter(health_variable_id_pk == 60) %>%
    group_by(user_id_pk) %>%
    summarise(crash_rate = mean(observation_value, na.rm = TRUE), .groups = "drop")
  
  mean_crash_1109 <- mean(Crash_1109$crash_rate, na.rm = TRUE) * 100
  
  #'*Identify demographics of the 948 users*
  
  # Percentage in each disease group
  round(prop.table(table(Cleaned_model_user_info_all$disease_type_simple)) * 100, 2)
  
  # Percentage in each contraception group 
  round(prop.table(table(Cleaned_model_user_info_all$Contraception_type)) * 100, 2)
  
  # Percentage nulligravida
  round(prop.table(table(Cleaned_model_user_info_all$nulligravida)) * 100, 2)
  
  # Mean age 
  mean(Cleaned_model_user_info_all$age, na.rm = TRUE)
  
  # Mean daily symptom score
  Overall_symptom_score_948 <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(observation_date_pk, user_id_pk) %>%
    summarise(overall_score = sum(observation_value))
  
  mean(Overall_symptom_score_948$overall_score, na.rm = TRUE)
  
  # Mean chance of crash
  Crash_948 <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(health_variable_id_pk == 60) %>%
    group_by(user_id_pk) %>%
    summarise(crash_rate = mean(observation_value, na.rm = TRUE), .groups = "drop")
  
  mean_crash_948 <- mean(Crash_948$crash_rate, na.rm = TRUE) * 100
  
  
  #'*Compare demographics of the different filtering stages*
  
  # Disease type
  disease_tab <- rbind(
    table(Cleaned_user_info$disease_type_simple),
    table(Cleaned_user_info_1765$disease_type_simple),
    table(Cleaned_user_info_1109$disease_type_simple),
    table(Cleaned_model_user_info_all$disease_type_simple)
  )
  rownames(disease_tab) <- c("stage1", "stage2", "stage3", "stage4")
  
  # Chi-square test
  disease_chi <- chisq.test(disease_tab)
  disease_chi
  
  # Standardized residuals to see direction
  round(disease_chi$stdres, 2)

  # Proportion per stage (direction)
  prop.table(disease_tab, margin = 1)  # row-wise proportions
  
  # Contraception type
  Contraception_tab <- rbind(
    table(Cleaned_user_info$Contraception_type),
    table(Cleaned_user_info_1765$Contraception_type),
    table(Cleaned_user_info_1109$Contraception_type),
    table(Cleaned_model_user_info_all$Contraception_type)
  )
  rownames(Contraception_tab) <- c("stage1", "stage2", "stage3", "stage4")
  
  # Chi-square test
  contraception_chi <- chisq.test(Contraception_tab)
  contraception_chi
  
  # Standardized residuals for direction
  round(contraception_chi$stdres, 2)
  
  # Proportion per stage
  prop.table(Contraception_tab, margin = 1)
  
  # Nulligravida counts per stage
  nulligravida_tab <- rbind(
    table(Cleaned_user_info$nulligravida),         # stage1
    table(Cleaned_user_info_1765$nulligravida),   # stage2
    table(Cleaned_user_info_1109$nulligravida),   # stage3
    table(Cleaned_model_user_info_all$nulligravida) # stage4
  )
  rownames(nulligravida_tab) <- c("stage1","stage2","stage3","stage4")
  
  # Chi-square test
  nulligravida_chi <- chisq.test(nulligravida_tab)
  nulligravida_chi
  
  # Standardized residuals (direction)
  round(nulligravida_chi$stdres, 2)

  # Row-wise proportions per stage (actual percentages)
  prop.table(nulligravida_tab, margin = 1)
  
  # List of variables and corresponding stage vectors
  test_list <- list(
    "Mean Daily Symptom Score" = list(
      Overall_symptom_score_1932$overall_score,
      Overall_symptom_score_1765$overall_score,
      Overall_symptom_score_1109$overall_score,
      Overall_symptom_score_948$overall_score
    ),
    "Percentage Chance of Crash" = list(
      Crash_1932$crash_rate,
      Crash_1765$crash_rate,
      Crash_1109$crash_rate,
      Crash_948$crash_rate
    ),
    "Age" = list(
      Cleaned_user_info$age,
      Cleaned_user_info_1765$age,
      Cleaned_user_info_1109$age,
      Cleaned_model_user_info_all$age
    )
  )
  
  stages <- c("stage1","stage2","stage3","stage4")
  
  # Run Kruskal-Wallis + Dunn's test for all variables
  imap(test_list, ~ {
    df <- bind_rows(lapply(seq_along(.x), function(i) data.frame(value = .x[[i]], stage = stages[i])))
    cat("\n===== ", .y, " =====\n")
    print(df %>% group_by(stage) %>% summarise(median = median(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE)))
    print(kruskal.test(value ~ stage, data = df))
    print(dunnTest(value ~ stage, data = df, method = "bonferroni"))
  })
  
  #'*Figure S3: Number of observations reported in each menstrual phase (before cleaning of users who did not consistently track observations) in the population cohort*
  
  Phase_engagement_h_data <- Menstrual_and_all_symptom_data_all_cycles_within_range_h %>%
    filter(!(symptom_type == "flow")) %>%
    group_by(Menstrual_phase) %>%
    summarise(n_obs = n(), .groups = "drop")
  
  Figure_S3 <- ggplot(Phase_engagement_h_data, aes(x = Menstrual_phase, y = n_obs, fill = Menstrual_phase)) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Menstrual Phase",
      y = "Number of Observations"
    ) +
    theme_pubr() +
    theme(text = element_text(family = "Times"))
  
  ggsave('figures_and_tables/Supplementary/Figure_S3.pdf', plot = Figure_S3, width = 15, height = 10, units = "cm")
  
  #'*Figure S4: Number of observations reported in each menstrual phase (after cleaning of users who did not consistently track observations) in the population cohort*
  
  Phase_engagement_h_data <- Users_all_phases_data %>%
    filter(!(symptom_type == "flow")) %>%
    group_by(Menstrual_phase) %>%
    summarise(n_obs = n(), .groups = "drop")
  
  Figure_S4 <- ggplot(Phase_engagement_h_data, aes(x = Menstrual_phase, y = n_obs, fill = Menstrual_phase)) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Menstrual Phase",
      y = "Number of Observations"
    ) +
    theme_pubr() +
    theme(text = element_text(family = "Times"))
  
  ggsave('figures_and_tables/Supplementary/Figure_S4.pdf', plot = Figure_S4, width = 15, height = 10, units = "cm")
  
  ##'*Figure S5 - Age distribution of the disease cohort*
  
  median_age <- median(Cleaned_model_user_info_all$age, na.rm = TRUE)
  age_iqr <- IQR(Cleaned_model_user_info_all$age, na.rm = TRUE)
  
  Age_histogram <- ggplot(Cleaned_model_user_info_all, aes(x = age)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    theme_pubr() +
    theme(text = element_text(family = "Times")) +
    labs(x = "Age",
         y = "Frequency")
  
  ggsave('figures_and_tables/Supplementary/Figure_S5.pdf', plot = Age_histogram, width = 10, height = 5, units = "cm")
  
  ##'*Figure S6 - Age distribution of the population cohort*

  #Remove rows where have multiple user_id & oha_date combos
  Single_user_info <- Cleaned_model_user_info_h %>%
    dplyr::group_by(user_id) %>%
    dplyr::filter(oha_date == max(oha_date, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  n_distinct(Single_user_info$user_id)
  n_distinct(Cleaned_model_user_info_h$user_id)
  
  #Age distribution
  median_age <- median(Single_user_info$age_years, na.rm = TRUE)
  age_iqr <- IQR(Single_user_info$age_years, na.rm = TRUE)
  
  Age_histogram <- ggplot(Single_user_info, aes(x = age_years)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    theme_pubr() +
    theme(text = element_text(family = "Times")) +
    labs(x = "Age",
         y = "Frequency")
  
  ggsave('figures_and_tables/Supplementary/Figure_S6.pdf', plot = Age_histogram, width = 10, height = 5, units = "cm")
  
  #'*Table S2: Demographics of disease population, split by disease type*
  
  # Function to create a table with counts and percentages
  create_table <- function(data, var_name) {
    total_users <- nrow(data)
    table_data <- table(data[[var_name]]) %>%
      as.data.frame() %>%
      rename(Subgroup = Var1, Count = Freq) %>%
      mutate(Percentage = round((Count / total_users) * 100, 1))
    table_data <- cbind(Variable = var_name, table_data)
    return(table_data)
  }
  
  # Filter data for each disease group
  long_covid_data <- Cleaned_model_user_info_all %>%
    filter(disease_type_simple == "Long COVID")
  me_cfs_data <- Cleaned_model_user_info_all %>%
    filter(disease_type_simple == "ME/CFS")
  both_diseases_data <- Cleaned_model_user_info_all %>%
    filter(disease_type_simple == "Long COVID and ME/CFS")
  
  # Filter survey data for each disease group
  survey_long_covid <- Cleaned_model_survey_data %>%
    semi_join(long_covid_data, by = "user_id_pk")
  survey_me_cfs <- Cleaned_model_survey_data %>%
    semi_join(me_cfs_data, by = "user_id_pk")
  survey_both <- Cleaned_model_survey_data %>%
    semi_join(both_diseases_data, by = "user_id_pk")
  
  # Create demographic tables for Long COVID group
  gender_table_long_covid <- create_table(long_covid_data, "gender_id")
  disabled_table_long_covid <- create_table(long_covid_data, "disabled")
  age_cat_table_long_covid <- create_table(long_covid_data, "age_category")
  
  # Create survey tables for Long COVID group
  contraception_users_long_covid <- survey_long_covid %>%
    filter(question_id_pk == 1) %>%
    mutate(answer = ifelse(answer == "No", "None", answer))
  contraception_table_long_covid <- create_table(contraception_users_long_covid, "answer")
  
  gravidity_users_long_covid <- survey_long_covid %>%
    filter(question_id_pk == 4)
  gravidity_table_long_covid <- create_table(gravidity_users_long_covid, "answer")
  
  parity_users_long_covid <- survey_long_covid %>%
    filter(question_id_pk == 5)
  parity_table_long_covid <- create_table(parity_users_long_covid, "answer")
  
  # Repeat the process for ME/CFS group
  gender_table_me_cfs <- create_table(me_cfs_data, "gender_id")
  disabled_table_me_cfs <- create_table(me_cfs_data, "disabled")
  age_cat_table_me_cfs <- create_table(me_cfs_data, "age_category")
  
  contraception_users_me_cfs <- survey_me_cfs %>%
    filter(question_id_pk == 1) %>%
    mutate(answer = ifelse(answer == "No", "None", answer))
  contraception_table_me_cfs <- create_table(contraception_users_me_cfs, "answer")
  
  gravidity_users_me_cfs <- survey_me_cfs %>%
    filter(question_id_pk == 4)
  gravidity_table_me_cfs <- create_table(gravidity_users_me_cfs, "answer")
  
  parity_users_me_cfs <- survey_me_cfs %>%
    filter(question_id_pk == 5)
  parity_table_me_cfs <- create_table(parity_users_me_cfs, "answer")
  
  # Repeat the process for Both Diseases group
  gender_table_both <- create_table(both_diseases_data, "gender_id")
  disabled_table_both <- create_table(both_diseases_data, "disabled")
  age_cat_table_both <- create_table(both_diseases_data, "age_category")
  
  contraception_users_both <- survey_both %>%
    filter(question_id_pk == 1) %>%
    mutate(answer = ifelse(answer == "No", "None", answer))
  contraception_table_both <- create_table(contraception_users_both, "answer")
  
  gravidity_users_both <- survey_both %>%
    filter(question_id_pk == 4)
  gravidity_table_both <- create_table(gravidity_users_both, "answer")
  
  parity_users_both <- survey_both %>%
    filter(question_id_pk == 5)
  parity_table_both <- create_table(parity_users_both, "answer")
  
  # Combine tables for each group
  final_table_long_covid <- bind_rows(
    gender_table_long_covid, disabled_table_long_covid, age_cat_table_long_covid,
    contraception_table_long_covid, gravidity_table_long_covid, parity_table_long_covid
  )
  final_table_me_cfs <- bind_rows(
    gender_table_me_cfs, disabled_table_me_cfs, age_cat_table_me_cfs,
    contraception_table_me_cfs, gravidity_table_me_cfs, parity_table_me_cfs
  )
  final_table_both <- bind_rows(
    gender_table_both, disabled_table_both, age_cat_table_both,
    contraception_table_both, gravidity_table_both, parity_table_both
  )
  
  # Save tables as separate CSV files
  write.csv(final_table_long_covid, "figures_and_tables/Supplementary/Table_S2_demographics_long_covid.csv", row.names = FALSE)
  write.csv(final_table_me_cfs, "figures_and_tables/Supplementary/Table_S2_demographics_me_cfs.csv", row.names = FALSE)
  write.csv(final_table_both, "figures_and_tables/Supplementary/Table_S2_demographics_both.csv", row.names = FALSE)
  
  # Optionally print tables for review
  print("Long COVID Demographics Table")
  print(final_table_long_covid)
  
  print("ME/CFS Demographics Table")
  print(final_table_me_cfs)
  
  print("Both Diseases Demographics Table")
  print(final_table_both)
  
  #Test for significance between the groups
  
  gender_by_disease <- data.frame(
    LC = c(409, 17, 4, 1),
    ME = c(296, 30, 1, 2),
    Both = c(173, 14, 1, 0)
  )
  
  age_by_disease <- data.frame(
    LC = c(69, 190, 172),
    ME = c(70, 132, 127),
    Both = c(40, 78, 70)
  )
  
  contraception_by_disease <- data.frame(
    LC = c(354, 34, 43),
    ME = c(270, 26, 33),
    Both = c(162, 10, 16)
  )
  
  pregnancies_by_disease <- data.frame(
    LC = c(267, 41, 67, 56),
    ME = c(226, 29, 42, 32),
    Both = c(124, 17, 23, 24)
  )
  
  births_by_disease <- data.frame(
    LC = c(297, 33, 77, 24),
    ME = c(253, 22, 37, 17),
    Both = c(137, 16, 21, 14)
  )
  gender_by_disease_chi <- chisq.test(gender_by_disease)
  age_by_disease_chi <- chisq.test(age_by_disease)
  contraception_by_disease_chi <- chisq.test(contraception_by_disease)
  pregnancies_by_disease_chi <- chisq.test(pregnancies_by_disease)
  births_by_disease_chi <- chisq.test(births_by_disease)
  
  median(long_covid_data$age)
  median(me_cfs_data$age)
  median(both_diseases_data$age)
  
  IQR(long_covid_data$age)
  IQR(me_cfs_data$age)
  IQR(both_diseases_data$age)
  
  # Kruskal Wallis 
  age_long_COVID <- long_covid_data$age
  age_ME_CFS <- me_cfs_data$age
  age_both_diseases <- both_diseases_data$age
  
  # Combine the age data into one vector
  age_data <- c(age_long_COVID, age_ME_CFS, age_both_diseases)
  
  # Create a grouping factor to indicate the group for each age value
  disease_type <- factor(c(rep("Long_COVID", length(age_long_COVID)),
                           rep("ME_CFS", length(age_ME_CFS)),
                           rep("Both", length(age_both_diseases))))
  
  # Create a data frame for Kruskal-Wallis test
  combined_disease_age_data <- data.frame(age = age_data, group = disease_type)
  
  kruskal_result <- kruskal.test(age ~ disease_type, data = combined_disease_age_data)
  print(kruskal_result)
  
  #'*Table S3: Demographics of disease population, split by contraception type*
  
  # Assign contraception groups
  
  contraception_info <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 1) %>%
    mutate(answer = ifelse(answer == "No", "None", answer)) %>%
    mutate(
      contraception_group = case_when(
        answer %in% c("Combined pill", "Contraceptive patch") ~ "Combined_contraception",
        answer %in% c("Contraceptive implant", "IUS", "Progesterone only pill") ~ "Pro_contraception",
        answer == "None" ~ "No_contraception",
        TRUE ~ "Other"
      )
    ) %>%
    rename(contraception_type = answer)
  
  # Join with demographics
  contraception_demo_data <- Cleaned_model_user_info_all %>%
    inner_join(contraception_info, by = "user_id_pk")
  
  # Define subgroups based on new contraception_group variable
  combined_group <- contraception_demo_data %>% filter(contraception_group == "Combined_contraception")
  pro_group      <- contraception_demo_data %>% filter(contraception_group == "Pro_contraception")
  none_group     <- contraception_demo_data %>% filter(contraception_group == "No_contraception")
  
  # Filter survey data for each group
  survey_data <- Cleaned_model_survey_data
  survey_combined <- survey_data %>% semi_join(combined_group, by = "user_id_pk")
  survey_pro      <- survey_data %>% semi_join(pro_group, by = "user_id_pk")
  survey_none     <- survey_data %>% semi_join(none_group, by = "user_id_pk")
  
  # Create demographic tables function
  
  create_demo_tables <- function(demo_data, survey_data) {
    bind_rows(
      create_table(demo_data, "gender_id"),
      create_table(demo_data, "disabled"),
      create_table(demo_data, "age_category"),
      create_table(demo_data, "disease_type_simple"),
      create_table(survey_data %>% filter(question_id_pk == 4), "answer"), # gravidity
      create_table(survey_data %>% filter(question_id_pk == 5), "answer")  # parity
    )
  }
  
  combined_table <- create_demo_tables(combined_group, survey_combined)
  pro_table      <- create_demo_tables(pro_group, survey_pro)
  none_table     <- create_demo_tables(none_group, survey_none)
  
  # Save CSVs
  write.csv(combined_table, "figures_and_tables/Supplementary/Table_S3_demographics_combined_contraception.csv", row.names = FALSE)
  write.csv(pro_table, "figures_and_tables/Supplementary/Table_S3_demographics_pro_contraception.csv", row.names = FALSE)
  write.csv(none_table, "figures_and_tables/Supplementary/Table_S3_demographics_no_contraception.csv", row.names = FALSE)
  
  print("Combined Contraception Demographics Table"); print(combined_table)
  print("Pro Contraception Demographics Table"); print(pro_table)
  print("No Contraception Demographics Table"); print(none_table)
  
  # Chi-square test (for gender, age category, disease)
  
  # Gender by contraception group (order: Woman, Non-binary, Prefer not to say, Prefer to self-describe)
  gender_by_contraception <- data.frame(
    Combined = c(65, 5, 0, 0),
    Pro = c(86, 6, 0, 0),
    None = c(727, 50, 6, 3)
  )
  
  # Age category by contraception group (order: <30, 30-39, ≥40)
  age_by_contraception <- data.frame(
    Combined = c(21, 33, 16),
    Pro = c(21, 31, 40),
    None = c(137, 336, 313)
  )
  
  # Disease type by contraception group (order: Long COVID, ME/CFS, Long COVID and ME/CFS)
  disease_by_contraception <- data.frame(
    Combined = c(34, 26, 10),
    Pro = c(43, 33, 16),
    None = c(354, 270, 162)
  )
  
  # Disease type by contraception group (order: Long COVID, ME/CFS, Long COVID and ME/CFS)
  pregnancies_by_contraception <- data.frame(
    Combined = c(56, 4, 7, 3),
    POP = c(55, 10, 14, 13),
    None = c(506, 73, 111, 96)
  )
  
  # Disease type by contraception group (order: Long COVID, ME/CFS, Long COVID and ME/CFS)
  births_by_contraception <- data.frame(
    Combined = c(58, 4, 6, 2),
    POP = c(61, 7, 18, 6),
    None = c(568, 60, 111, 47)
  )
  
  # Run Chi-square tests
  gender_chi <- chisq.test(gender_by_contraception)
  age_chi <- chisq.test(age_by_contraception)
  disease_chi <- chisq.test(disease_by_contraception)
  pregnancies_chi <- chisq.test(pregnancies_by_contraception)
  births_chi <- chisq.test(births_by_contraception)
  
  # Print results with labels
  cat("\nChi-square test for Gender by Contraception Group:\n")
  print(gender_chi)
  
  cat("\nChi-square test for Age Category by Contraception Group:\n")
  print(age_chi)
  
  cat("\nChi-square test for Disease Type by Contraception Group:\n")
  print(disease_chi)
  
  cat("\nChi-square test for Pregnancies by Contraception Group:\n")
  print(pregnancies_chi)
  
  cat("\nChi-square test for Births by Contraception Group:\n")
  print(births_chi)
  
  # Step 7: Age Kruskal-Wallis test
  median(none_group$age)
  median(pro_group$age)
  median(combined_group$age)
  
  IQR(none_group$age)
  IQR(pro_group$age)
  IQR(combined_group$age)
  
  
  age_data <- c(combined_group$age, pro_group$age, none_group$age)
  group_label <- factor(c(rep("Combined_contraception", nrow(combined_group)),
                          rep("Pro_contraception", nrow(pro_group)),
                          rep("No_contraception", nrow(none_group))))
  
  kruskal_test_data <- data.frame(age = age_data, group = group_label)
  kruskal_result <- kruskal.test(age ~ group, data = kruskal_test_data)
  print(kruskal_result)
  
  dunn_result <- dunnTest(age ~ group, data = kruskal_test_data, method = "bh")
  print(dunn_result)

  #'*Figure S7: Age distribution across contraception types, in the disease cohort*
  # Prepare data
  age_data <- c(combined_group$age, pro_group$age, none_group$age)
  group_label <- factor(c(rep("Combined_contraception", length(combined_group$age)),
                          rep("Pro_contraception", length(pro_group$age)),
                          rep("No_contraception", length(none_group$age))))
  
  age_data_contraception_graph <- data.frame(age = age_data, group = group_label)
  
  # Basic boxplot with jittered points
  Figure_S7 <- ggplot(
    age_data_contraception_graph,
    aes(x = group, y = age, fill = group)
  ) +
    geom_boxplot(alpha = 0.6) +
    geom_jitter(width = 0.15, alpha = 0.4) +
    scale_x_discrete(labels = c(
      "Combined_contraception" = "Oestrogen & Progestin",
      "Pro_contraception"      = "Progestin only",
      "No_contraception"       = "No hormonal contraception"
    )) +
    theme_pubr() +
    theme(
      text = element_text(family = "Times"),
      legend.position = "none",
      axis.title.x = element_blank()
    ) +
    labs(y = "Age")
  
  # Add pairwise comparisons with significance
  comparisons <- list(
    c("Combined_contraception", "No_contraception"),
    c("Combined_contraception", "Pro_contraception"),
    c("No_contraception", "Pro_contraception")
  )
  
  Figure_S7_graph <- Figure_S7 + stat_compare_means(comparisons = comparisons, method = "wilcox.test", 
                         p.adjust.method = "BH")
  
  ggsave("figures_and_tables/Supplementary/Figure_S7.pdf", plot = Figure_S7_graph, width = 18, height = 10, units = "cm")
  
  #'*Table S4: Demographics of population cohort (using data from most recent online health assessment)*
  
  total_users <- n_distinct(Cleaned_h_data$user_id)
  
  create_table <- function(data, var_name, total_users) {
    table_data <- table(data[[var_name]]) %>%
      as.data.frame() %>%
      rename(Subgroup = Var1, Count = Freq) %>%
      mutate(Percentage = round((Count / total_users) * 100, 1))
    table_data <- cbind(Variable = var_name, table_data)
    return(table_data)
  }
  
  # Create tables for each variable
  gender_table <- create_table(Single_user_info, "identify_as_female", total_users)
  ethnicity_table <- create_table(Single_user_info, "ethnicity_main", total_users)
  age_cat_table <- create_table(Single_user_info, "age_category", total_users)
  cycle_descriptor_table <- create_table(Single_user_info, "cycle_description", total_users)
  nulligravida_table <- create_table(Single_user_info, "nulligravida", total_users)
  contraception_type_table <- create_table(Single_user_info, "Contraception_type", total_users)
  bmi_cat_table <- create_table(Single_user_info, "bmi_category", total_users)
  
  # Combine all tables
  final_table <- bind_rows(age_cat_table, gender_table, contraception_type_table, nulligravida_table, 
                           ethnicity_table, cycle_descriptor_table, bmi_cat_table)
  
  # Save the final table as a CSV file
  write.csv(final_table, "figures_and_tables/Supplementary/Table_S4.csv", row.names = FALSE)
  
  #'*Figure S8: Distribution of mean symptom score and how it varies over the menstrual cycle (disease cohort)*
  
  #Figure S8A
  
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
    theme_pubr() + common_theme + theme(text = element_text(family = "Times"))
  
  ggsave("figures_and_tables/Supplementary/Figure_S8A.pdf", plot = Mean_symptom_scores_histogram, width = 6, height = 5, units = "cm")
  
  # Calculate the mean and variance 
  mean_symptom_score_mean <- mean(Mean_symptoms_scores_users$mean_symptom_score, na.rm = TRUE)
  mean_symptom_score_variance <- var(Mean_symptoms_scores_users$mean_symptom_score, na.rm = TRUE)
  
  #Figure S8B
  
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
    theme_pubr() + common_theme + theme(text = element_text(family = "Times")) +
    scale_y_continuous(limits = c(16.2, 18.0)) 
  
  ggsave("figures_and_tables/Supplementary/Figure_S8B.pdf", plot = scatter_plot_phase_symptom_score, width = 6, height = 5, units = "cm")
  
  #Perform Friedman test
  Filtered_mean_symptom_scores$user_id_pk <- factor(Filtered_mean_symptom_scores$user_id_pk)
  
  friedman_test_mean_symptom_score <- friedman.test(mean_symptom_score ~ Menstrual_phase | user_id_pk, data = Filtered_mean_symptom_scores)
  
  #' Perform post-hoc pairwise comparisons using the Nemenyi test
  nemenyi_mean_symptom_score <- frdAllPairsNemenyiTest(Filtered_mean_symptom_scores$mean_symptom_score, Filtered_mean_symptom_scores$Menstrual_phase, blocks = Filtered_mean_symptom_scores$user_id_pk)
  
  #'*Figure S9: Distribution of users tracking symptoms over study time period (disease cohort)*

  # Prepare menstrual data with quarters post-joining
  Standard_menstrual_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Menstrual") %>%
    mutate(observation_date_pk = as.Date(observation_date_pk)) %>%
    left_join(
      Cleaned_model_user_info_all %>% select(user_id_pk, user_created_date),
      by = "user_id_pk"
    ) %>%
    mutate(
      quarters_post_joining = ceiling(
        interval(user_created_date, observation_date_pk) / months(3)
      ),
      quarters_post_joining = as.numeric(quarters_post_joining),
      quarters_post_joining = ifelse(quarters_post_joining == 0, 1, quarters_post_joining)
    )
  
  # Total users tracking menstrual data per quarter
  total_menstrual_users_over_time <- Standard_menstrual_data %>%
    filter(observation_date_pk < as.Date("2024-03-08")) %>%
    group_by(quarters_post_joining) %>%
    summarize(
      total_users = n_distinct(user_id_pk),
      .groups = "drop"
    )
  
  # Total users tracking individual symptoms per quarter
  symptoms_over_time <- Standard_symptom_data %>% 
    filter(observation_date_pk < as.Date("2024-03-08")) %>% 
    group_by(health_variable_name, quarters_post_joining) %>% 
    summarize( users_tracking = n_distinct(user_id_pk), .groups = "drop" )
  
  # Total users tracking symptom data per quarter
  total_users_over_time <- Standard_symptom_data %>% 
    filter(observation_date_pk < as.Date("2024-03-08")) %>% 
    group_by(quarters_post_joining) %>% 
    summarize( total_users = n_distinct(user_id_pk), .groups = "drop" )
  
  symptom_tracking_plot <- ggplot(
    symptoms_over_time,
    aes(
      x = quarters_post_joining,
      y = users_tracking,
      colour = health_variable_name,
      group = health_variable_name
    )
  ) +
    geom_line(size = 1, alpha = 0.7) +
    
    # --- BLACK TOTAL SYMPTOM USERS ---
    geom_line(
      data = total_users_over_time,
      aes(x = quarters_post_joining, y = total_users),
      colour = "black",
      size = 1.4,
      inherit.aes = FALSE
    ) +
    
    # --- RED DOTTED TOTAL MENSTRUAL TRACKERS ---
    geom_line(
      data = total_menstrual_users_over_time,
      aes(x = quarters_post_joining, y = total_users),
      colour = "red",
      linetype = "dotted",
      size = 1.4,
      inherit.aes = FALSE
    ) +
    
    labs(
      x = "Quarters Post-Joining",
      y = "Number of Users Tracking",
      colour = NULL
    ) +
    theme_pubr() +
    theme(
      text = element_text(family = "Times"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  ggsave(
    "figures_and_tables/Supplementary/Figure_S9.pdf",
    plot = symptom_tracking_plot,
    units = "cm",
    width = 30,
    height = 16
  )
  
  #'*Figure S10: The mean number of symptoms tracked over study time period (disease cohort)*
  
  Standard_symptom_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(
      is_published == "true",
      health_variable_type == "Symptom"
    ) %>%
    mutate(observation_date_pk = as.Date(observation_date_pk)) %>%
    left_join(
      Cleaned_model_user_info_all %>% 
        select(user_id_pk, user_created_date),
      by = "user_id_pk"
    ) %>%
    mutate(
      quarters_post_joining = ceiling(
        interval(user_created_date, observation_date_pk) / months(3)
      ),
      quarters_post_joining = as.numeric(quarters_post_joining),
      quarters_post_joining = ifelse(quarters_post_joining == 0, 1, quarters_post_joining)
    ) %>%
    filter(observation_date_pk < as.Date("2024-03-08"))
  
  symptoms_per_user_quarter <- Standard_symptom_data %>%
    group_by(user_id_pk, quarters_post_joining) %>%
    summarise(
      n_symptoms_tracked = n_distinct(health_variable_name),
      .groups = "drop"
    )
  
  symptom_quarter_summary <- symptoms_per_user_quarter %>%
    group_by(quarters_post_joining) %>%
    summarise(
      mean_symptoms = mean(n_symptoms_tracked),
      se_symptoms   = sd(n_symptoms_tracked) / sqrt(n()),
      n_users       = n(),
      .groups = "drop"
    )
  
  symptom_tracking_plot_quarters <- ggplot(
    symptom_quarter_summary,
    aes(x = quarters_post_joining, y = mean_symptoms)
  ) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(
        ymin = mean_symptoms - se_symptoms,
        ymax = mean_symptoms + se_symptoms
      ),
      width = 0.2
    ) +
    labs(
      x = "Quarters Since Joining",
      y = "Mean Number of Symptoms Tracked"
    ) +
    theme_pubr() +
    theme(
      text = element_text(family = "Times"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  # Save plot
  ggsave("figures_and_tables/Supplementary/Figure_S10.pdf",
         plot = symptom_tracking_plot_quarters,
         units = "cm",
         width = 15, height = 10)
  
  #'*Figure S11: Distribution of the number of days symptoms were tracked by users (disease cohort)*
  
  # Define total number of users
  total_users <- 948
  
  # Group the data by health_variable_name and user_id_pk, and count the observations
  user_obs_count <- Standard_symptom_data %>%
    group_by(health_variable_name, user_id_pk) %>%
    summarize(observations = n())
  
  # Combine user_obs_count with health_variable_users
  Symptom_tracking_numbers <- Standard_symptom_data %>%
    group_by(health_variable_name) %>%
    summarize(unique_users = n_distinct(user_id_pk))
  
  user_obs_count <- user_obs_count %>%
    left_join(Symptom_tracking_numbers, by = "health_variable_name")
  
  # Create a new column combining health variable name and number of unique users
  user_obs_count <- user_obs_count %>%
    mutate(percentage_users = (unique_users / total_users) * 100,
           facet_label = paste(health_variable_name, "\n(n = ", unique_users, ", ", round(percentage_users, 1), "%)", sep = ""))
  
  # Create a histogram faceted by health variables
  histogram_faceted_symptom_plot <- ggplot(user_obs_count, aes(x = observations)) +
    geom_histogram(fill = "skyblue", alpha = 0.7, bins = 30) +
    labs(x = "Total number of days symptom was tracked by user", y = "Number of users") +
    facet_wrap(~ reorder(facet_label, -unique_users)) +
    theme_pubr() +
    theme(strip.background = element_blank(),
          text = element_text(family = "Times"),
          strip.text = element_text(size = 9, face = "bold"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 12, face = "bold"))
  
  ggsave("figures_and_tables/Supplementary/Figure_S11.pdf",
         plot = histogram_faceted_symptom_plot, 
         units = "cm",
         width = 20, height = 16)
  
  #'*Figure S12: Jaccard similarity heatmap of reported symptoms with hierarchical clustering (disease cohort) and Figure S13: Heatmap of symptoms and users with hierarchical clustering (disease cohort)*
  
  # Pivot the original dataframe to get a binary indicator for each symptom
  binary_df <- Standard_symptom_data %>%
    select(user_id_pk, observation_date_pk, row_id, health_variable_name) %>%
    mutate(tracked = 1) %>%
    pivot_wider(names_from = health_variable_name, values_from = tracked, values_fill = 0)
  
  # Create a new dataframe with user_id and whether they have tracked each symptom
  user_symptom_df <- binary_df %>%
    select(-observation_date_pk, -row_id) %>%
    group_by(user_id_pk) %>%
    summarise_all(any) %>%
    ungroup()
  
  # Fill missing values with 0
  user_symptom_df[is.na(user_symptom_df)] <- 0
  
  # Convert categorical data to numeric & remove user_id_column 
  user_symptom_df_numeric <- user_symptom_df %>%
    mutate(across(-user_id_pk, as.integer)) %>% # Convert all columns except user_id to integer
    select(-user_id_pk)
  
  # Compute Jaccard similarity matrix for pairs of symptoms
  jaccard_similarities <- matrix(NA, nrow = ncol(user_symptom_df_numeric), ncol = ncol(user_symptom_df_numeric), 
                                 dimnames = list(colnames(user_symptom_df_numeric), colnames(user_symptom_df_numeric)))
  
  for (i in 1:(ncol(user_symptom_df_numeric) - 1)) {
    for (j in (i + 1):ncol(user_symptom_df_numeric)) {
      intersection <- sum(user_symptom_df_numeric[, i] & user_symptom_df_numeric[, j])  # Count common occurrences
      union <- sum(user_symptom_df_numeric[, i] | user_symptom_df_numeric[, j])  # Count total occurrences
      jaccard_similarities[i, j] <- intersection / union  # Compute Jaccard similarity
      jaccard_similarities[j, i] <- jaccard_similarities[i, j]  # Symmetric matrix
    }
  }
  
  # Convert the Jaccard similarity matrix to a distance matrix
  jaccard_distances <- as.dist(1 - jaccard_similarities)
  
  # Compute hierarchical clustering
  hclust_result <- hclust(jaccard_distances, method = "complete")
  
  # Plot the symptom heatmap with dendrogram
  symptom_symptom_heatmap <- pheatmap(jaccard_similarities,
                                      clustering_distance_rows = jaccard_distances,
                                      clustering_distance_cols = jaccard_distances,
                                      clustering_method = "complete",
                                      scale = "none",  
                                      border_color = NA, 
                                      main = "",
                                      fontsize_row = 8,  
                                      fontsize_col = 8,
                                      fontfamily = "Times") 
  
  ggsave("figures_and_tables/Supplementary/Figure_S12.pdf",
         plot = symptom_symptom_heatmap, 
         units = "cm",
         width = 17, height = 15)
  
  # Plot users/symptoms heatmap with dendrogram 
  binary_palette <- c("white", "black")
  
  symptom_user_heatmap <- pheatmap(as.matrix(user_symptom_df_numeric),
                                   cluster_rows = TRUE,
                                   cluster_cols = TRUE,
                                   color = binary_palette, 
                                   fontfamily = "Times",
                                   main = "")
  
  ggsave("figures_and_tables/Supplementary/Figure_S13.pdf",
         plot = symptom_user_heatmap, 
         units = "cm",
         width = 17, height = 15)
  
  
  # Count the number of distinct users tracking each symptom
  full_symptom_user_counts <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(health_variable_name) %>%
    summarise(user_count = n_distinct(user_id_pk)) %>%
    arrange(desc(user_count))
  
  #'*Figure S14: Impact of menstrual cycle phase on individual symptom severity in the disease cohort*
  
  Symptom_full_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom")
  
  # Function to generate a colour palette for each symptom
  generate_colour_palette <- function(base_colour) {
    colorRampPalette(c(base_colour, "white"))(4)[4:1]
  }
  
  # Generate the viridis palette with 36 colours
  viridis_palette <- viridis(36)
  
  # Count the number of distinct users tracking each symptom
  symptom_user_counts <- Symptom_full_data %>%
    group_by(health_variable_name) %>%
    summarise(user_count = n_distinct(user_id_pk), .groups = "drop") %>%
    arrange(desc(user_count))
  
  # Extract symptoms sorted by the number of users
  sorted_symptoms <- symptom_user_counts$health_variable_name
  
  # Create a list to store spine plot tables
  spine_plot_tables <- list()
  
  # Loop through each symptom in the sorted order
  for (symptom in sorted_symptoms) {
    cat("Processing", symptom, "...\n")
    
    symptom_data_spines <- Symptom_full_data %>%
      filter(health_variable_name == symptom)
    
    if (nrow(symptom_data_spines) == 0) {
      cat("No data available for", symptom, "- skipping...\n")
      next
    }
    
    # Ensure correct factor levels for Menstrual_phase
    symptom_data_spines$Menstrual_phase <- factor(
      symptom_data_spines$Menstrual_phase,
      levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual")
    )
    
    # Convert observation_value to numeric
    symptom_data_spines$observation_value <- as.numeric(symptom_data_spines$observation_value)
    
    # Create spine plot table
    symptom_data_spines_table <- xtabs(
      ~ Menstrual_phase + factor(observation_value, levels = c("3", "2", "1", "0")),
      data = symptom_data_spines
    )
    
    spine_plot_tables[[symptom]] <- symptom_data_spines_table
    
    cat("Completed processing for", symptom, "\n")
  }
  
  # Open a PDF device for the combined plot
  pdf(
    "figures_and_tables/Supplementary/Figure_S14.pdf",
    width = 15 / 2.54,
    height = 23 / 2.54
  )
  
  # Set up the layout with 4 columns and 9 rows (36 plots)
  layout_matrix <- matrix(1:36, ncol = 4, byrow = TRUE)
  layout(layout_matrix)
  
  # Margins and font
  par(oma = c(1.5, 1.5, 1.5, 1.5))
  par(mar = c(0.6, 0.6, 0.6, 0.6))
  par(family = "serif")
  
  # Custom plotting function
  plot_spine <- function(data, col, title) {
    spineplot(data, col = col, xlab = "", ylab = "", axes = FALSE)
    mtext(title, side = 3, line = 0.2, cex = 0.5, font = 2)
  }
  
  # Loop over spine plots
  for (symptom in names(spine_plot_tables)) {
    
    # Assign colour palette deterministically by symptom rank
    base_colour <- viridis_palette[which(sorted_symptoms == symptom)]
    symptom_palette <- generate_colour_palette(base_colour)
    
    # Number of users tracking this symptom
    user_count <- symptom_user_counts$user_count[
      symptom_user_counts$health_variable_name == symptom
    ]
    
    plot_title <- paste0(symptom, " (n=", user_count, ")")
    
    plot_spine(
      spine_plot_tables[[symptom]],
      symptom_palette,
      plot_title
    )
  }
  
  # Close the PDF device
  dev.off()
  
  #'*Figure S15: Impact of menstrual cycle phase on individual symptom severity in the population cohort*
  
  # Prepare data
  Standard_symptom_data_h <- Cleaned_h_data %>%
    filter(!(symptom_type == "flow")) %>%
    mutate(observation_date = as.Date(observation_date))
  
  # Define symptoms of interest
  
  symptoms <- c(
    "anxiety", "brain-fog", "depressed", "fatigue", "blurred-vision",
    "loss", "nausea", "hard", "watery"
  )
  
  symptom_labels <- c(
    "anxiety" = "Anxiety",
    "brain-fog" = "Brain fog",
    "depressed" = "Depression",
    "fatigue" = "Fatigue",
    "blurred-vision" = "Blurred vision",
    "loss" = "Loss of appetite",
    "nausea" = "Nausea",
    "hard" = "Constipation",
    "watery" = "Diarrhoea"
  )
  
  # Number of users who reported each symptom at least once
  
  symptom_user_counts_h <- Standard_symptom_data_h %>%
    filter(symptom_response %in% symptoms) %>%
    distinct(user_id, symptom_response) %>%
    count(symptom_response, name = "user_count") %>%
    rename(symptom = symptom_response) %>%
    arrange(desc(user_count))
  
  # Ordered symptom list 
  sorted_symptoms <- symptom_user_counts_h$symptom
  
  # Colour utilities
  
  generate_colour_palette <- function(base_colour) {
    colorRampPalette(c(base_colour, "white"))(2)[2:1]
  }
  
  viridis_palette <- viridis(length(sorted_symptoms))
  
  # Create spine plot tables
  
  spine_plot_tables_h <- list()
  
  for (i in seq_along(sorted_symptoms)) {
    
    symptom <- sorted_symptoms[i]
    cat("Processing", symptom, "...\n")
    
    # One row per user per day, prioritising presence of this symptom
    symptom_data <- Standard_symptom_data_h %>%
      group_by(user_id, observation_date) %>%
      arrange(desc(symptom_response == symptom)) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      mutate(
        symptom_present = if_else(symptom_response == symptom, 1L, 0L),
        Menstrual_phase = factor(
          Menstrual_phase,
          levels = c(
            "Menstrual",
            "Follicular",
            "Early luteal",
            "Late luteal",
            "Premenstrual"
          )
        )
      )
    
    # Spine plot contingency table
    spine_tab <- xtabs(
      ~ Menstrual_phase + factor(symptom_present, levels = c(1, 0)),
      data = symptom_data
    )
    
    spine_plot_tables_h[[symptom]] <- spine_tab
    
    cat("Completed", symptom, "\n")
  }
  
  
  # Plot all spine plots to PDF
  
  pdf(
    "figures_and_tables/Supplementary/Figure_S15.pdf",
    width = 15 / 2.54,
    height = 10 / 2.54
  )
  
  layout(matrix(seq_along(sorted_symptoms), ncol = 3, byrow = TRUE))
  
  par(oma = c(1.5, 1.5, 1.5, 1.5))
  par(mar = c(0.6, 0.6, 0.6, 0.6))
  par(family = "serif")
  
  plot_spine <- function(data, col, title) {
    spineplot(data, col = col, xlab = "", ylab = "", axes = FALSE)
    mtext(title, side = 3, line = 0.2, cex = 0.5, font = 2)
  }
  
  for (i in seq_along(sorted_symptoms)) {
    
    symptom <- sorted_symptoms[i]
    symptom_palette <- generate_colour_palette(viridis_palette[i])
    
    n_users <- symptom_user_counts_h$user_count[
      symptom_user_counts_h$symptom == symptom
    ]
    
    plot_title <- paste0(symptom_labels[symptom], " (n=", n_users, ")")
    
    plot_spine(
      spine_plot_tables_h[[symptom]],
      symptom_palette,
      plot_title
    )
  }
  
  dev.off()
  
  #'*Table S5: The fitting results of cumulative link mixed-effects models for individual symptoms joined disease and population cohorts*

  #'*Prepare joint fatigue data*
  Fatigue_h_data_to_join <- Fatigue_h_data %>%
    mutate(disease_type_simple = "Control",
           nulligravida = as.numeric(nulligravida),
           fatigue = as.numeric(as.character(fatigue)),   # convert ordered factor to numeric
           fatigue = if_else(fatigue == 1, 3, fatigue),   # change 1 → 3
           fatigue = factor(fatigue,
                            levels = c(0, 1, 2, 3),
                            ordered = TRUE)              # convert back to ordered factor
    ) %>%
    rename(user_id_pk = user_id,
           observation_date_pk = observation_date)
  
  Fatigue_V_data_to_join <- Fatigue_V_data %>%
    mutate(user_id_pk = as.character(user_id_pk),
           nulligravida = as.numeric(nulligravida))
  
  # now fully join
  Joined_fatigue_data <- full_join(Fatigue_V_data_to_join, Fatigue_h_data_to_join)
  
  # Add group type column 
  
  Joined_fatigue_data <- Joined_fatigue_data %>%
    mutate(group_type = if_else(disease_type_simple == "Control", "Control", "Disease"),) 
  
  # Make variables into factors & choose reference groups 
  Joined_fatigue_data <- Joined_fatigue_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida),
      disease_type_simple      = factor(disease_type_simple)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"),
           disease_type_simple = relevel(disease_type_simple, ref = "Control"))
  
  #'*Run joint fatigue model with menstrual phase, age, contraception & gravidity*
  
  Joined_fatigue_model <- clmm(fatigue ~ group_type * Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id_pk),
                               data = Joined_fatigue_data, 
                               link = "logit", 
                               control = clmm.control(maxIter = 40000, gradTol = 1e-4))
  saveRDS(Joined_fatigue_model, file = "figures_and_tables/Models/Joined_fatigue_model_3.rds")
  
  # Extract fixed effects
  Joined_fatigue_model_summary <- summary(Joined_fatigue_model)
  Joined_fatigue_model_coef_table <- Joined_fatigue_model_summary$coefficients
  
  # Create a data frame for fixed effects
  Joined_fatigue_model_fixed_effects <- data.frame(
    Variable = rownames(Joined_fatigue_model_coef_table),
    `Incidence_rate_ratio` = exp(Joined_fatigue_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(Joined_fatigue_model_coef_table[, 1] - 1.96 * Joined_fatigue_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(Joined_fatigue_model_coef_table[, 1] + 1.96 * Joined_fatigue_model_coef_table[, 2]),  # Upper CI
    `P_value` = Joined_fatigue_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  Joined_fatigue_model_fixed_effects$Incidence_rate_ratio <- signif(Joined_fatigue_model_fixed_effects$Incidence_rate_ratio, 3)
  Joined_fatigue_model_fixed_effects$CI_lower <- signif(Joined_fatigue_model_fixed_effects$CI_lower, 3)
  Joined_fatigue_model_fixed_effects$CI_upper <- signif(Joined_fatigue_model_fixed_effects$CI_upper, 3)
  Joined_fatigue_model_fixed_effects$P_value <- signif(Joined_fatigue_model_fixed_effects$P_value, 3)
  Joined_fatigue_model_fixed_effects$P_value <-
    ifelse(Joined_fatigue_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(Joined_fatigue_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(Joined_fatigue_model_fixed_effects, "figures_and_tables/Main_manuscript/Joined_fatigue_model_fixed_effects_table_2.csv", row.names = FALSE)
  
  #'*Prepare joint brain fog data*
  brain_fog_h_data_to_join <- brain_fog_h_data %>%
    mutate(disease_type_simple = "Control",
           nulligravida = as.numeric(nulligravida),
           brain_fog = as.numeric(as.character(brain_fog)),   # convert ordered factor to numeric
           brain_fog = if_else(brain_fog == 1, 3, brain_fog),   # change 1 → 3
           brain_fog = factor(brain_fog,
                              levels = c(0, 1, 2, 3),
                              ordered = TRUE)              # convert back to ordered factor
    ) %>%
    rename(user_id_pk = user_id,
           observation_date_pk = observation_date)
  
  brain_fog_V_data_to_join <- brain_fog_V_data %>%
    mutate(user_id_pk = as.character(user_id_pk),
           nulligravida = as.numeric(nulligravida))
  
  # now fully join
  Joined_brain_fog_data <- full_join(brain_fog_V_data_to_join, brain_fog_h_data_to_join)
  
  # Add group type column 
  
  Joined_brain_fog_data <- Joined_brain_fog_data %>%
    mutate(group_type = if_else(disease_type_simple == "Control", "Control", "Disease"),) 
  
  # Make variables into factors & choose reference groups 
  Joined_brain_fog_data <- Joined_brain_fog_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida),
      disease_type_simple      = factor(disease_type_simple)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"),
           disease_type_simple = relevel(disease_type_simple, ref = "Control"))
  
  #'*Run joint brain fog model with menstrual phase, age, contraception & gravidity*
  
  Joined_brain_fog_model <- clmm(brain_fog ~ group_type * Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id_pk),
                                 data = Joined_brain_fog_data, 
                                 link = "logit", 
                                 control = clmm.control(maxIter = 40000, gradTol = 1e-4, method = "ucminf"))
  saveRDS(Joined_brain_fog_model, file = "figures_and_tables/Models/Joined_brain_fog_model_3.rds")
  
  # Extract fixed effects
  Joined_brain_fog_model_summary <- summary(Joined_brain_fog_model)
  Joined_brain_fog_model_coef_table <- Joined_brain_fog_model_summary$coefficients
  
  # Create a data frame for fixed effects
  Joined_brain_fog_model_fixed_effects <- data.frame(
    Variable = rownames(Joined_brain_fog_model_coef_table),
    `Incidence_rate_ratio` = exp(Joined_brain_fog_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(Joined_brain_fog_model_coef_table[, 1] - 1.96 * Joined_brain_fog_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(Joined_brain_fog_model_coef_table[, 1] + 1.96 * Joined_brain_fog_model_coef_table[, 2]),  # Upper CI
    `P_value` = Joined_brain_fog_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  Joined_brain_fog_model_fixed_effects$Incidence_rate_ratio <- signif(Joined_brain_fog_model_fixed_effects$Incidence_rate_ratio, 3)
  Joined_brain_fog_model_fixed_effects$CI_lower <- signif(Joined_brain_fog_model_fixed_effects$CI_lower, 3)
  Joined_brain_fog_model_fixed_effects$CI_upper <- signif(Joined_brain_fog_model_fixed_effects$CI_upper, 3)
  Joined_brain_fog_model_fixed_effects$P_value <- signif(Joined_brain_fog_model_fixed_effects$P_value, 3)
  Joined_brain_fog_model_fixed_effects$P_value <-
    ifelse(Joined_brain_fog_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(Joined_brain_fog_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(Joined_brain_fog_model_fixed_effects, "figures_and_tables/Main_manuscript/Joined_brain_fog_model_fixed_effects_table_2.csv", row.names = FALSE)
  
  #'*Prepare joint headache data*
  headache_h_data_to_join <- headache_h_data %>%
    mutate(disease_type_simple = "Control",
           nulligravida = as.numeric(nulligravida),
           headache = as.numeric(as.character(headache)),   # convert ordered factor to numeric
           headache = if_else(headache == 1, 3, headache),   # change 1 → 3
           headache = factor(headache,
                             levels = c(0, 1, 2, 3),
                             ordered = TRUE)              # convert back to ordered factor
    ) %>%
    rename(user_id_pk = user_id,
           observation_date_pk = observation_date)
  
  headache_V_data_to_join <- headache_V_data %>%
    mutate(user_id_pk = as.character(user_id_pk),
           nulligravida = as.numeric(nulligravida))
  
  # now fully join
  Joined_headache_data <- full_join(headache_V_data_to_join, headache_h_data_to_join)
  
  # Add group type column 
  
  Joined_headache_data <- Joined_headache_data %>%
    mutate(group_type = if_else(disease_type_simple == "Control", "Control", "Disease"),) 
  
  # Make variables into factors & choose reference groups 
  Joined_headache_data <- Joined_headache_data %>%
    mutate(
      Menstrual_phase   = factor(Menstrual_phase),
      age_category      = factor(age_category),
      Contraception_type = factor(Contraception_type),
      nulligravida      = factor(nulligravida),
      disease_type_simple      = factor(disease_type_simple)) %>%
    mutate(Menstrual_phase = relevel(Menstrual_phase, ref = "Menstrual"),
           age_category = relevel(age_category, ref = "<30"),
           Contraception_type = relevel(Contraception_type, ref = "None"),
           disease_type_simple = relevel(disease_type_simple, ref = "Control"))
  
  #'*Prepare joint headache model with menstrual phase, age, contraception & gravidity*
  Joined_headache_model <- clmm(headache ~ group_type * Menstrual_phase + age_category + Contraception_type + nulligravida + (1 | user_id_pk),
                                data = Joined_headache_data, 
                                link = "logit", 
                                control = clmm.control(maxIter = 40000, gradTol = 1e-4, method = "ucminf"))
  saveRDS(Joined_headache_model, file = "figures_and_tables/Models/Joined_headache_model_3.rds")
  
  # Extract fixed effects
  Joined_headache_model_summary <- summary(Joined_headache_model)
  Joined_headache_model_coef_table <- Joined_headache_model_summary$coefficients
  
  # Create a data frame for fixed effects
  Joined_headache_model_fixed_effects <- data.frame(
    Variable = rownames(Joined_headache_model_coef_table),
    `Incidence_rate_ratio` = exp(Joined_headache_model_coef_table[, 1]),  # Exponentiated coefficients (IRR)
    `CI_lower` = exp(Joined_headache_model_coef_table[, 1] - 1.96 * Joined_headache_model_coef_table[, 2]),  # Lower CI
    `CI_upper` = exp(Joined_headache_model_coef_table[, 1] + 1.96 * Joined_headache_model_coef_table[, 2]),  # Upper CI
    `P_value` = Joined_headache_model_coef_table[, 4]
  )
  
  # Round the numeric columns to 3 significant figures
  Joined_headache_model_fixed_effects$Incidence_rate_ratio <- signif(Joined_headache_model_fixed_effects$Incidence_rate_ratio, 3)
  Joined_headache_model_fixed_effects$CI_lower <- signif(Joined_headache_model_fixed_effects$CI_lower, 3)
  Joined_headache_model_fixed_effects$CI_upper <- signif(Joined_headache_model_fixed_effects$CI_upper, 3)
  Joined_headache_model_fixed_effects$P_value <- signif(Joined_headache_model_fixed_effects$P_value, 3)
  Joined_headache_model_fixed_effects$P_value <-
    ifelse(Joined_headache_model_fixed_effects$P_value < 0.0001,
           "P < 0.0001", signif(Joined_headache_model_fixed_effects$P_value, 3))
  
  # Save fixed effects as CSV files
  write.csv(Joined_headache_model_fixed_effects, "figures_and_tables/Main_manuscript/Joined_headache_model_fixed_effects_table_2.csv", row.names = FALSE)
  
  #'*Table S6: The fitting results of cumulative link mixed-effects models for individual symptoms in the disease cohort*
  
  # Fitting using normal optimizer
  # Define the full list of symptoms
  symptoms <- c(
    "Acid Reflux", "Allergies", "Altered smell", "Altered taste", "Anxiety", "Blurred vision", 
    "Brain Fog", "Chest pain", "Constipation", "Cough", "Depression", "Derealization", 
    "Diarrhea", "Dizziness", "Fatigue", "Fever", "Headache", "Joint pain", "Lack of appetite", 
    "Light sensitivity", "Lightheadedness", "Memory issues", "Migraine", "Muscle aches", 
    "Muscle weakness", "Nausea", "Nerve pain", "Noise sensitivity", "Numbness", 
    "Palpitations", "Pins and Needles", "Shortness of breath", "Sore throat", 
    "Stomach pain", "Tinnitus", "Tremors"
  )
  
  # Loop through each symptom
  for (symptom in symptoms) {
    
    # Prepare the data for each symptom
    symptom_model_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
      filter(health_variable_name == symptom)
    
    symptom_model_data_user_info <- symptom_model_data %>%
      inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
    
    symptom_model_data_user_info_model <- symptom_model_data_user_info %>%
      group_by(user_id_pk) %>%
      mutate(new_cycle_id = dense_rank(cycle_id)) %>%
      ungroup()
    
    symptom_model_data_user_info_model$Menstrual_phase <- factor(symptom_model_data_user_info_model$Menstrual_phase,levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    
    symptom_model_data_user_info_model$observation_value <- 
      factor(symptom_model_data_user_info_model$observation_value, 
             levels = c(0, 1, 2, 3),
             ordered = TRUE)
    
    # Running ordinal mixed-effects model for the current symptom
    model_name_phase_only <- paste0(symptom, "_model_phase_only")
    assign(model_name_phase_only, clmm(observation_value ~ Menstrual_phase + (1|user_id_pk),
                                       data = symptom_model_data_user_info_model,
                                       link = "logit",
                                       control = clmm.control(maxIter = 40000, gradTol = 1e-4)))
    
    saveRDS(get(model_name_phase_only), file = paste0('figures_and_tables/Models/',model_name_phase_only, ".rds"))
    print(summary(get(model_name_phase_only)))
    
  }
  
  # Fitting using ucminf optimizer
  # Define the list of symptoms
  symptoms <- c("Lack of appetite", "Pins and Needles")
  # Loop through each symptom
  for (symptom in symptoms) {
    # Prepare the data for each symptom
    symptom_model_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
      filter(health_variable_name == symptom)
    symptom_model_data_user_info <- symptom_model_data %>%
      inner_join(Cleaned_model_user_info_all, by = c("user_id_pk"))
    symptom_model_data_user_info_model <- symptom_model_data_user_info %>%
      group_by(user_id_pk) %>%
      mutate(new_cycle_id = dense_rank(cycle_id)) %>%
      ungroup()
    symptom_model_data_user_info_model$Menstrual_phase <- factor(symptom_model_data_user_info_model$Menstrual_phase,levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    symptom_model_data_user_info_model$observation_value <-
      factor(symptom_model_data_user_info_model$observation_value,
             levels = c(0, 1, 2, 3),
             ordered = TRUE)
    # Running ordinal mixed-effects model for the current symptom
    model_name_phase_only <- paste0(symptom, "_model_phase_only")
    assign(model_name_phase_only, clmm(observation_value ~ Menstrual_phase + (1|user_id_pk),
                                       data = symptom_model_data_user_info_model,
                                       link = "logit",
                                       control = clmm.control(maxIter = 50000, gradTol = 1e-4, method = "ucminf")))
    saveRDS(get(model_name_phase_only), file = paste0('figures_and_tables/Models/',model_name_phase_only, ".rds"))
    print(summary(get(model_name_phase_only)))
  }
  
  # Outputting relevant information
  # List of symptoms
  symptoms <- c( "Acid_Reflux", "Allergies", "Altered_smell", "Altered_taste", "Anxiety", "Blurred_vision", 
                 "Brain_Fog", "Chest_pain", "Constipation", "Cough", "Depression", "Derealization", 
                 "Diarrhea", "Dizziness", "Fatigue", "Fever", "Headache", "Joint_pain", "Lack_of_appetite", 
                 "Light_sensitivity", "Lightheadedness", "Memory_issues", "Migraine", "Muscle_aches", 
                 "Muscle_weakness", "Nausea", "Nerve_pain", "Noise_sensitivity", "Numbness", 
                 "Palpitations", "Pins_and_Needles", "Shortness_of_breath", "Sore_throat", 
                 "Stomach_pain", "Tinnitus", "Tremors"
  )
  
  # Create an empty list to store all the tables
  all_symptom_summaries <- list()
  
  # Loop through each symptom
  for (symptom in symptoms) {
    
    # Read the model for the current symptom
    model_name <- paste0(symptom, "_model_phase_only")
    
    # Extract the fixed effects
    fixed_effects_df <- as.data.frame(summary(model)$coefficients)
    fixed_effects_df$Effect <- rownames(fixed_effects_df)
    fixed_effects_df <- fixed_effects_df[, c("Effect", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
    
    # Format the fixed effects to 3 significant figures
    fixed_effects_df$Estimate <- signif(fixed_effects_df$Estimate, 3)
    fixed_effects_df$`Std. Error` <- signif(fixed_effects_df$`Std. Error`, 3)
    fixed_effects_df$`z value` <- signif(fixed_effects_df$`z value`, 3)
    fixed_effects_df$`Pr(>|z|)` <- format(fixed_effects_df$`Pr(>|z|)`, digits = 3)
    
    # Check if thresholds are part of the fixed effects
    threshold_rows <- grep("threshold", rownames(fixed_effects_df), ignore.case = TRUE)
    
    # Extract the threshold coefficients
    thresholds_df <- fixed_effects_df[threshold_rows, ]
    thresholds_df$Effect <- rownames(thresholds_df)
    
    # Combine both fixed effects and threshold coefficients into one data frame
    combined_df <- rbind(fixed_effects_df, thresholds_df)
    
    # Add a new column for the symptom name
    combined_df$Symptom <- symptom
    
    # Append the result to the list
    all_symptom_summaries[[symptom]] <- combined_df
  }
  
  # Combine all tables into a single data frame
  final_summary_df <- do.call(rbind, all_symptom_summaries)
  
  # Write the final combined summary to a CSV file
  write.csv(final_summary_df, "figures_and_tables/Supplementary/Symptom_Model_Summaries.csv", row.names = FALSE)
  
  # Optionally, print the final summary
  print("Symptom summaries saved to Symptom_Model_Summaries.csv")
  
  
  # Create an empty list to store model fit statistics and random effects
  model_fit_stats_list <- list()
  
  # Loop through each symptom
  for (symptom in symptoms) {
    
    # Read the model for the current symptom
    model_name <- paste0(symptom, "_model_phase_only")
    
    # Extract model fit statistics
    logLik <- as.numeric(logLik(model))
    AIC <- AIC(model)
    BIC <- BIC(model)
    deviance <- deviance(model)
    nobs <- length(model$fitted.values)
    n_groups <- length(unique(model$model$user_id_pk))
    
    # Extract random effects
    random_effects <- summary(model)$random
    variance_intercept <- random_effects$groups$`user_id_pk`[1, "Variance"]
    
    # Create a data frame with the extracted statistics
    model_fit_df <- data.frame(
      Symptom = symptom,
      Log_Likelihood = signif(logLik, 3),
      AIC = signif(AIC, 3),
      BIC = signif(BIC, 3),
      N_Observations = nobs,
      N_Groups = n_groups,
    )
    
    # Append the result to the list
    model_fit_stats_list[[symptom]] <- model_fit_df
  }
  
  # Combine all model fit statistics and random effects into a single data frame
  final_model_fit_df <- do.call(rbind, model_fit_stats_list)
  
  # Write the model fit statistics and random effects to a CSV file
  write.csv(final_model_fit_df, "figures_and_tables/Supplementary/Symptom_Model_Fit_Stats.csv", row.names = FALSE)
  
  # Print the final summary
  print("Model fit statistics and random effects saved to Symptom_Model_Fit_Stats.csv")
  
  #'*Table S7: The fitting results of binomial mixed-effects models for individual symptoms (that overlap exactly with the symptoms tracked in the disease cohort) in the population cohort*
  
  # Fitting using normal optimizer
  # Define the full list of symptoms
  symptoms <- c("anxiety", "brain-fog", "depressed", "fatigue", "blurred-vision", "loss", 
                "nausea", "hard", "watery")
  
  for (symptom in symptoms) {
    
    # Prepare data: keep one row per user/date, mark presence/absence, label symptom
    symptom_model_data <- Cleaned_h_data %>%
      group_by(user_id, observation_date) %>%
      # prioritise rows where this symptom is present
      arrange(desc(symptom_response == symptom)) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      mutate(
        symptom_present = if_else(symptom_response == symptom, 1L, 0L),
        symptom_name = symptom  # store which symptom this dataset is for
      )
    
    # Re-rank cycles
    symptom_model_data_user_info_model <- symptom_model_data %>%
      group_by(user_id) %>%
      mutate(new_cycle_id = dense_rank(cycle_id)) %>%
      ungroup()
    
    # Factor handling
    symptom_model_data_user_info_model$Menstrual_phase <- 
      factor(symptom_model_data_user_info_model$Menstrual_phase,
             levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    
    symptom_model_data_user_info_model$observation_value <- 
      factor(symptom_model_data_user_info_model$symptom_present)
    
    # Model
    model_name_phase_only <- paste0(symptom, "_model_phase_only")
    
    assign(
      model_name_phase_only,
      glmer(symptom_present ~ Menstrual_phase + (1|user_id),
            data = symptom_model_data_user_info_model,
            family = binomial(link = "logit"))
    )
    
    saveRDS(get(model_name_phase_only),
            file = paste0('figures_and_tables/Models/', model_name_phase_only, ".rds"))
    
    print(summary(get(model_name_phase_only)))
  }
  # Outputting relevant info 
  
  # List of symptoms
  symptoms <- c("anxiety", "brain-fog", "depressed", "fatigue", 
                "blurred-vision", "loss", "nausea", 
                "hard", "watery")
  
  # Extract fixed effect summaries
  
  # Create an empty list to store all the tables
  all_symptom_summaries <- list()
  
  # Loop through each symptom
  for (symptom in symptoms) {
    
    # Get the model object for the current symptom
    model_name <- paste0(symptom, "_model_phase_only")
    model_obj <- get(model_name)
    
    # Extract the fixed effects
    fixed_effects_df <- as.data.frame(summary(model_obj)$coefficients)
    fixed_effects_df$Effect <- rownames(fixed_effects_df)
    fixed_effects_df <- fixed_effects_df[, c("Effect", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
    
    # Format the fixed effects to 3 significant figures
    fixed_effects_df$Estimate <- signif(fixed_effects_df$Estimate, 3)
    fixed_effects_df$`Std. Error` <- signif(fixed_effects_df$`Std. Error`, 3)
    fixed_effects_df$`z value` <- signif(fixed_effects_df$`z value`, 3)
    fixed_effects_df$`Pr(>|z|)` <- format(fixed_effects_df$`Pr(>|z|)`, digits = 3)
    
    # Add a new column for the symptom name
    fixed_effects_df$Symptom <- symptom
    
    # Append the result to the list
    all_symptom_summaries[[symptom]] <- fixed_effects_df
  }
  
  # Combine all tables into a single data frame
  final_summary_df <- do.call(rbind, all_symptom_summaries)
  
  # Write the final combined summary to a CSV file
  write.csv(final_summary_df, "figures_and_tables/Supplementary/Symptom_Model_Summaries.csv", row.names = FALSE)
  
  print("Symptom summaries saved to Symptom_Model_Summaries.csv")


  # Extract model fit statistics and random effects
  
  # Create an empty list to store model fit statistics
  model_fit_stats_list <- list()
  
  # Loop through each symptom
  for (symptom in symptoms) {
    
    # Get the model object for the current symptom
    model_name <- paste0(symptom, "_model_phase_only")
    model_obj <- get(model_name)
    
    # Extract model fit statistics
    logLik_val <- as.numeric(logLik(model_obj))
    AIC_val <- AIC(model_obj)
    BIC_val <- BIC(model_obj)
    deviance_val <- deviance(model_obj)
    nobs_val <- length(fitted(model_obj))                  
    n_groups <- length(ranef(model_obj)$user_id_pk)     
    
    # Extract random effect variance for the intercept
    variance_intercept <- as.data.frame(VarCorr(model_obj))$vcov[1]
    
    # Create a data frame with the extracted statistics
    model_fit_df <- data.frame(
      Symptom = symptom,
      Log_Likelihood = signif(logLik_val, 3),
      AIC = signif(AIC_val, 3),
      BIC = signif(BIC_val, 3),
      Deviance = signif(deviance_val, 3),
      N_Observations = nobs_val,
      N_Groups = n_groups,
      Random_Effect_Intercept_Variance = variance_intercept
    )
    
    # Append the result to the list
    model_fit_stats_list[[symptom]] <- model_fit_df
  }
  
  # Combine all model fit statistics into a single data frame
  final_model_fit_df <- do.call(rbind, model_fit_stats_list)
  
  # Write the model fit statistics and random effects to a CSV file
  write.csv(final_model_fit_df, "figures_and_tables/Supplementary/Symptom_Model_Fit_Stats.csv", row.names = FALSE)
  
  print("Model fit statistics and random effects saved to Symptom_Model_Fit_Stats.csv")
  
  
} else {
  print("If you would like to request these files for scientific purposes, contact Visible Health Inc at info@makevisible.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}
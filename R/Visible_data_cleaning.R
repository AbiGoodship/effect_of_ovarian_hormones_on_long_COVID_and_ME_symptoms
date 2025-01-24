#'*Read in libraries*
library(tidyverse)
library(lubridate)

#'*Read data files in*
if(file.exists("dim_001_userinfo__user-000000000000.csv"))
{
  #Read in user info
  User_info <- read.csv("dim_001_userinfo__user-000000000000.csv")
  User_illness <- read.csv("dim_002_userinfo__user_illness-000000000000.csv")
  
  Full_user_info <- User_info %>%
    full_join(User_illness, by = "user_id_pk")
  
  #Read in app observation data
  App_observations_file_0 <- read.csv("fact_001_app__observations-000000000000.csv")
  App_observations_file_1 <- read.csv("fact_001_app__observations-000000000001.csv")
  App_observations_file_2 <- read.csv("fact_001_app__observations-000000000002.csv")
  App_observations_file_3 <- read.csv("fact_001_app__observations-000000000003.csv")
  App_observations_file_4 <- read.csv("fact_001_app__observations-000000000004.csv")
  
  #Join together app observation data
  Full_app_data <- rbind(
    App_observations_file_0,
    App_observations_file_1,
    App_observations_file_2,
    App_observations_file_3,
    App_observations_file_4)
  
  n_distinct(Full_app_data$user_id_pk)
  
  #Read in survey data
  Full_survey_data <- read.csv("study_002_1_imperial_male__survey_responses-000000000000.csv")
  
  n_distinct(Full_survey_data$user_id_pk)
  
  #Read in symptom info
  Symptom_info <- read.csv("dim_007_lookups__health_variables-000000000000.csv")
  Symptom_classification_info <- read.csv("dim_008_lookups__health_variable_classification-000000000000.csv")
  
  #'*Only keep users that appear in both datasets*
  
  # Find user ids that appear in both data frames
  common_users <- intersect(Full_app_data$user_id_pk, Full_survey_data$user_id_pk)
  
  # Filter rows in both data frames to keep only common users
  Full_app_data_clean <- Full_app_data %>% 
    filter(user_id_pk %in% common_users)
  
  Full_survey_data_clean <- Full_survey_data %>% 
    filter(user_id_pk %in% common_users)
  
  # Filter user info to keep only common users
  Full_user_info_clean <- Full_user_info %>%
    filter(user_id_pk %in% common_users)
  
  n_distinct(Full_user_info_clean$user_id_pk)
  
  ## Find and remove duplicated user_ids in user info 
  duplicated_users <- Full_user_info_clean$user_id_pk[duplicated(Full_user_info_clean$user_id_pk)]
  df_duplicated <- Full_user_info_clean %>% filter(user_id_pk %in% duplicated_users)
  
  Full_user_info_unique <- distinct(Full_user_info_clean, user_id_pk, .keep_all = TRUE)
  
  n_distinct(Full_survey_data_clean$user_id_pk)
  
  #'*Find users that are pregnant/breastfeeding*
  Pregnant_users <- Full_survey_data_clean %>%
    filter(question_id_pk == 2 & answer == 1)
  
  n_distinct(Pregnant_users$user_id_pk)
  
  Breastfeeding_users <- Full_survey_data_clean %>%
    filter(question_id_pk == 3 & answer == 1)
  
  n_distinct(Breastfeeding_users$user_id_pk)
  
  #'*Remove pregnant/breastfeeding users*
  Users_to_exclude <- c(Pregnant_users$user_id_pk, Breastfeeding_users$user_id_pk)
  
  Cleaned_survey_data <- Full_survey_data_clean[!(Full_survey_data_clean$user_id_pk %in% Users_to_exclude), ]
  Cleaned_app_data <- Full_app_data_clean[!(Full_app_data_clean$user_id_pk %in% Users_to_exclude), ]
  Cleaned_user_info <- Full_user_info_unique[!(Full_user_info_unique$user_id_pk %in% Users_to_exclude), ]
  
  n_distinct(Cleaned_user_info$user_id_pk)
  
  #'*Remove users that are outside the age criteria*
  
  User_dates <- Cleaned_survey_data %>%
    distinct(user_id_pk, .keep_all = TRUE) %>%
    mutate(
      submission_timestamp = as.Date(submission_timestamp),
      year_created = as.numeric(format(submission_timestamp, "%Y"))
    )
  
  Cleaned_user_info <- Cleaned_user_info %>%
    full_join(User_dates, by = "user_id_pk")
  
  Cleaned_user_info$age <- Cleaned_user_info$year_created - Cleaned_user_info$year_of_birth
  
  Users_outside_age_crit <- Cleaned_user_info %>%
    filter(!(age >= 18 & age <= 45))
  
  n_distinct(Users_outside_age_crit$user_id_pk)
  
  Users_to_exclude <- c(Users_outside_age_crit$user_id_pk)
  
  Cleaned_survey_data <- Cleaned_survey_data[!(Cleaned_survey_data$user_id_pk %in% Users_to_exclude), ]
  Cleaned_app_data <- Cleaned_app_data [!(Cleaned_app_data$user_id_pk %in% Users_to_exclude), ]
  Cleaned_user_info <- Cleaned_user_info[!(Cleaned_user_info$user_id_pk %in% Users_to_exclude), ]
  
  n_distinct(Cleaned_survey_data$user_id_pk)
  n_distinct(Cleaned_user_info$user_id_pk)
  n_distinct(Cleaned_app_data$user_id_pk)
  
  #'*Add in demographic info*
  
  Cleaned_user_info <- Cleaned_user_info %>%
    mutate(gender_id = case_when(
      gender_id == 1 ~ "Man",
      gender_id == 2 ~ "Woman",
      gender_id == 3 ~ "Non-binary",
      gender_id == 4 ~ "Prefer not to say",
      gender_id == 5 ~ "Prefer to self-describe",
      TRUE ~ as.character(gender_id)
    ))
  
  Cleaned_user_info <- Cleaned_user_info %>%
    mutate(disease_type = case_when(
      long_covid == "true" & me_cfs == "false" & other_illness == "" ~ "Long COVID only",
      long_covid == "false" & me_cfs == "true" & other_illness == "" ~ "ME/CFS only",
      long_covid == "false" & me_cfs == "false" ~ "Other illness only",
      long_covid == "true" & me_cfs == "true" & other_illness == ""  ~ "Just Long COVID and ME/CFS",
      long_covid == "true" & me_cfs == "false" & other_illness != "" ~ "Long COVID plus other illness(es)",
      long_covid == "false" & me_cfs == "true" & other_illness != "" ~ "ME/CFS plus other illness(es)",
      long_covid == "true" & me_cfs == "true" & other_illness != ""  ~ "Long COVID and ME/CFS plus other illness(es)",
      TRUE ~ NA_character_ # Handle other cases as NA
    ))
  
  Cleaned_user_info <- Cleaned_user_info %>%
    mutate(age_category = case_when(
      age < 30 ~ "<30",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 ~ "≥40"),
      Contraception_type = case_when(answer %in% c("Combined pill", "Contraceptive patch") ~ "Estrogen & Progestin",
                                     answer %in% c("Contraceptive implant", "IUS", "Progesterone only pill") ~ "Progestin only",
                                     answer == "No" ~ "None"),
      disease_type_simple = case_when(
        long_covid == "true" & me_cfs == "false" ~ "Long COVID",
        long_covid == "false" & me_cfs == "true" ~ "ME/CFS",
        long_covid == "true" & me_cfs == "true" ~ "Long COVID and ME/CFS",
        long_covid == "false" & me_cfs == "false" ~ "Other"),
    )
  
  #'*Exclude 'other users'*
  
  # Identify users with disease type "Other illness only" or disease type NA
  Users_to_exclude <- Cleaned_user_info %>%
    filter(disease_type == "Other illness only" | is.na(disease_type)) %>%
    select(user_id_pk)
  
  # Extract user IDs to exclude
  Users_to_exclude <- Users_to_exclude$user_id_pk
  
  n_distinct(Users_to_exclude)
  
  # Filter out these users from all three datasets
  Cleaned_survey_data <- Cleaned_survey_data %>%
    filter(!user_id_pk %in% Users_to_exclude)
  
  Cleaned_app_data <- Cleaned_app_data %>%
    filter(!user_id_pk %in% Users_to_exclude)
  
  Cleaned_user_info <- Cleaned_user_info %>%
    filter(!user_id_pk %in% Users_to_exclude)
  
  # Check the number of distinct users remaining in each dataset
  n_distinct(Cleaned_survey_data$user_id_pk)
  n_distinct(Cleaned_user_info$user_id_pk)
  n_distinct(Cleaned_app_data$user_id_pk)
  
  #'*Find and remove those who never have a period*
  
  #filter to get those tracking period 
  Period_app_data <- Cleaned_app_data %>%
    filter(health_variable_id_pk == 49)
  
  n_distinct(Period_app_data$user_id_pk)
  
  Users_with_periods <- Period_app_data %>%
    group_by(user_id_pk) %>%
    filter(!(all(observation_value == 0))) %>%
    pull(user_id_pk) %>%
    unique()
  
  Cleaned_period_app_data <- Period_app_data[(Period_app_data$user_id_pk %in% Users_with_periods), ]
  n_distinct(Cleaned_period_app_data$user_id_pk)
  
  #'*Add in missing dates*
  
  Complete_period_data <- Cleaned_period_app_data %>%
    complete(user_id_pk, observation_date_pk) %>%
    filter(!(observation_date_pk == "2022-08-31"))
  
  #'*Assign period start date for each user*
  
  # Replace missing values with '2'
  Complete_period_data$observation_value[is.na(Complete_period_data$observation_value)] <- 2
  Complete_period_data$observation_value <- as.numeric(as.character(Complete_period_data$observation_value))
  
  #Assign period start date 
  Period_start_date_assigned <- Complete_period_data %>%
    mutate(period_start = observation_value == 1 & #assigns period start data: true if 1 and if...
             lag(observation_value, default = 0) != 1 & #the row 1 above is not 1 
             lag(observation_value, default = 0, n = 2) != 1 & #the row 2 above is not 1 
             lag(observation_value, default = 0, n = 3) != 1 & #the row 3 above is not 1 
             lag(observation_value, default = 0, n = 4) != 1 & #the row 4 above is not 1 
             lag(observation_value, default = 0, n = 4) != 1 & #the row 5 above is not 1 
             lead(observation_value, default = 1, n = 1) != 0 & #the row 1 below is not 0
             lead(observation_value, default = 1, n = 2) != 0 & #the row 2 below is not 0 
             lead(observation_value, default = 0, n = 9) != 1 & #the row 9 below is not 1
             lead(observation_value, default = 0, n = 10) != 1, #the row 10 below is not 1 
           period_group = cumsum(period_start)) #Assign a unique group ID to each period_start
  
  Period_start_date_assigned$cycle_id = paste(Period_start_date_assigned$user_id_pk, Period_start_date_assigned$period_group, sep="_")
  
  n_distinct(Period_start_date_assigned$user_id_pk)
  n_distinct(Period_start_date_assigned$cycle_id)
  
  #'*Calculate cycle length and plot cycle length distribution*
  
  #Calculate cycle lengths
  Cycle_length_data <- Period_start_date_assigned %>%
    group_by(user_id_pk, period_group) %>%
    summarize(cycle_length = n()) %>% 
    ungroup()
  
  # Put cycle length data back in 
  Cycle_data <- left_join(Period_start_date_assigned, 
                          Cycle_length_data, by = c("user_id_pk", "period_group"))
  
  n_distinct(Cycle_data$cycle_id)
  n_distinct(Cycle_data$user_id_pk)
  
  #'*Remove cycles where there are more than two consecutive missing days*
  Cycles_missing_less_2_days <- Cycle_data %>%
    group_by(user_id_pk, period_group) %>%
    filter(!any(observation_value == 2 & 
                  lag(observation_value) == 2 & 
                  lead(observation_value) == 2))
  
  n_distinct(Cycles_missing_less_2_days$cycle_id)
  n_distinct(Cycles_missing_less_2_days$user_id_pk) 
  
  Number_cycles_missing_less_2_days <- Cycles_missing_less_2_days %>%
    group_by(user_id_pk, period_group) %>%
    summarise(num_rows = n())
  
  #'*Find number of cycles falling within the range of 24 to 38 days & remove cycles that are too short/too long*
  
  #Remove cycles out of range 
  Short_cycles <- Cycles_missing_less_2_days %>%
    filter(cycle_length < 24)
  
  n_distinct(Short_cycles$cycle_id)
  n_distinct(Short_cycles$user_id_pk)
  
  Long_cycles <- Cycles_missing_less_2_days %>%
    filter(cycle_length > 38)
  
  n_distinct(Long_cycles$cycle_id)
  n_distinct(Long_cycles$user_id_pk)
  
  Cycles_within_range <- Cycles_missing_less_2_days %>%
    filter(cycle_length >= 24 & cycle_length <= 38)
  
  n_distinct(Cycles_within_range$cycle_id)
  n_distinct(Cycles_within_range$user_id_pk)
  
  #'*Impute any missing menstrual cycle data for users*
  
  #Impute any missing data for users
  Imputed_data_complete_all_cycles_within_range <- Cycles_within_range %>%
    mutate(observation_value_imputed = ifelse((observation_value == 2 & lag(period_start, n = 8, default = FALSE)) |
                                                (observation_value == 2 & lag(period_start, n = 9, default = FALSE)), 
                                              0, observation_value)) %>% #Impute 0 if missing and on the 9th or 10th day of the cycle 
    mutate(observation_value_imputed = ifelse(observation_value == 2, 
                                              ifelse(lag(observation_value) != 2, 
                                                     lag(observation_value), 
                                                     lag(observation_value, 2)), 
                                              observation_value)) %>% #Impute data from row before, 
    #shouldn't be imputing data from another user as first date for each user will always be a 1
    mutate(period_end = !period_start & #Period end when not period start &... 
             observation_value_imputed == 1 & #bleeding &
             lag(observation_value_imputed, n = 1) == 1 & #2 days before also bleeding (should have been imputed if missing) &
             lag(observation_value_imputed, n = 2) == 1 &
             lead(observation_value_imputed) == 0) #day after is not bleeding
  
  Imputed_data_complete_all_cycles_within_range <- Imputed_data_complete_all_cycles_within_range %>%
    mutate(observation_value = factor(observation_value),
           observation_date_pk = as.Date(observation_date_pk))
  
  Menstrual_phase_data_all_cycles_within_range<- Imputed_data_complete_all_cycles_within_range %>%
    group_by(cycle_id) %>%
    mutate(
      period_start_date = if_else(period_start, observation_date_pk, as.Date(NA)),
      period_end_date = if_else(period_end, observation_date_pk, as.Date(NA))
    ) %>%
    fill(period_start_date) %>%
    fill(period_end_date, .direction = "up") %>%
    ungroup()
  
  Menstrual_phase_data_all_cycles_within_range <- Menstrual_phase_data_all_cycles_within_range %>%
    mutate(Menstrual = observation_date_pk >= period_start_date & observation_date_pk <= period_end_date,
           Menstrual = if_else(is.na(Menstrual), "FALSE", as.character(Menstrual)),
           Menstrual_phase = case_when(
             Menstrual == "TRUE" ~ "Menstrual",
             Menstrual == "FALSE" & (lead(Menstrual, n = 1) == "TRUE" | lead(Menstrual, n = 2) == "TRUE") ~ "Premenstrual",
             Menstrual == "FALSE" & (lead(Menstrual, n = 1) == "FALSE" | lead(Menstrual, n = 2) == "FALSE") & 
               (lead(Menstrual, n = 3) == "TRUE" | lead(Menstrual, n = 4) == "TRUE"| lead(Menstrual, n = 5) == "TRUE"
                | lead(Menstrual, n = 6) == "TRUE" | lead(Menstrual, n = 7) == "TRUE") ~ "Late luteal", 
             Menstrual == "FALSE" & (lead(Menstrual, n = 1) == "FALSE" | lead(Menstrual, n = 2) == "FALSE") & 
               (lead(Menstrual, n = 3) == "FALSE" | lead(Menstrual, n = 4) == "FALSE"| lead(Menstrual, n = 5) == "FALSE"
                | lead(Menstrual, n = 6) == "FALSE" | lead(Menstrual, n = 7) == "FALSE") & 
               (lead(Menstrual, n = 8) == "TRUE" | lead(Menstrual, n = 9) == "TRUE"| lead(Menstrual, n = 10) == "TRUE"
                | lead(Menstrual, n = 11) == "TRUE" | lead(Menstrual, n = 12) == "TRUE" | lead(Menstrual, n = 13) == "TRUE" 
                | lead(Menstrual, n = 14) == "TRUE") ~ "Early luteal", 
             TRUE ~ "Follicular"
           ))
  
  #'*Integrate menstrual phase data with the original data set*
  
  #Add missing dates to app data
  Complete_app_data <- Cleaned_app_data %>%
    filter(!(observation_date_pk < "2022-09-07")) %>%
    complete(user_id_pk, observation_date_pk) 
  
  Complete_app_data$observation_date_pk <- as.Date(Complete_app_data$observation_date_pk)
  
  #Join menstrual data & symptom data 
  Menstrual_and_all_symptom_data_all_cycles_within_range <- Complete_app_data %>%
    inner_join(Menstrual_phase_data_all_cycles_within_range, by = c("user_id_pk", "observation_date_pk"), suffix = c("", ".x")) %>%
    select(-health_variable_id_pk.x, -observation_value.x, -quality.x, -observation_descriptor.x,
           -rating_scale_value_id.x, -row_id.x, -created_at.x, -updated_at.x)
  
  n_distinct(Menstrual_and_all_symptom_data_all_cycles_within_range$user_id_pk)
  n_distinct(Menstrual_and_all_symptom_data_all_cycles_within_range$cycle_id)
  
  #Find and remove any retrospective symptom reporting
  
  Menstrual_and_all_symptom_data_all_cycles_within_range <- Menstrual_and_all_symptom_data_all_cycles_within_range %>%
    mutate(observation_date_pk = as.Date(observation_date_pk), 
           created_at = as.Date(created_at),
           time_difference = as.numeric(difftime(observation_date_pk, created_at, units = "days")))
  
  Menstrual_and_all_symptom_data_all_cycles_within_range <- Menstrual_and_all_symptom_data_all_cycles_within_range %>%
    filter(!(health_variable_id_pk != 49 & time_difference < -2))
  
  #Add symptom names in
  
  Symptom_classification_info <- Symptom_classification_info %>%
    select(health_variable_classification_id_pk, health_variable_category, health_variable_type) %>%
    rename(health_variable_classification_id = health_variable_classification_id_pk)
  
  Full_menstrual_symptom_data_all_cycles_within_range <- Menstrual_and_all_symptom_data_all_cycles_within_range %>%
    inner_join(Symptom_info, by = "health_variable_id_pk") %>%
    inner_join(Symptom_classification_info, by = "health_variable_classification_id")
  
  #'*Add user info to prepare for negative binomial and binomial regression models*
  
  Users_in_model_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    group_by(user_id_pk) %>%
    pull(user_id_pk) %>%
    unique()
  
  Cleaned_model_survey_data <- Cleaned_survey_data[(Cleaned_survey_data$user_id_pk %in% Users_in_model_data), ]
  Cleaned_model_user_info <- Cleaned_user_info[(Cleaned_user_info$user_id_pk %in% Users_in_model_data), ]
  
  n_distinct(Cleaned_model_survey_data$user_id_pk)
  n_distinct(Cleaned_model_user_info$user_id_pk)
  
  Gravidity_summarised <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 4) %>% 
    rename(number_pregnancies = answer) %>%
    mutate(nulligravida = case_when(
      number_pregnancies == 0 ~ 1,
      number_pregnancies > 0 ~ 0)) %>%
    select(-completed_date, -submission_timestamp, -question_id_pk, -survey_id_pk)
  
  Parity_summarised <- Cleaned_model_survey_data %>%
    filter(question_id_pk == 5) %>% 
    rename(number_births = answer) %>%
    mutate(nulliparous = case_when(
      number_births == 0 ~ 1,
      number_births > 0 ~ 0)) %>%
    select(-completed_date, -submission_timestamp, -question_id_pk, -survey_id_pk)
  
  Cleaned_model_user_info_all <- Cleaned_model_user_info %>%
    inner_join(Gravidity_summarised, by = "user_id_pk") %>%
    inner_join(Parity_summarised, by = "user_id_pk")
  
  Cleaned_model_user_info_all <- Cleaned_model_user_info_all %>%
    rename(user_created_date = created_at.x) %>%
    mutate(user_created_date = as.Date(user_created_date))
  
  write_rds(Cleaned_model_user_info_all,'Cleaned_model_user_info_all.rds')
  write_rds(Cleaned_model_survey_data,'Cleaned_model_survey_data.rds')
  write_rds(Full_menstrual_symptom_data_all_cycles_within_range,'Full_menstrual_symptom_data_all_cycles_within_range.rds')  
} else {
  print("If you would like to request these files for scientific purposes, contact Visible Health Inc at info@makevisible.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}



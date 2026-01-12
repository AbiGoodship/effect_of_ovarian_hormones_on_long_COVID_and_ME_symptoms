#'*Read in libraries*
library(tidyverse)
library(lubridate)
library(ggpubr)

#'*Read data files in*
if (all(file.exists(c("250921-oha_info.csv", "250921-app_tracking_symptoms.csv")))) 
  {
  
#Read in user info
Full_h_user_info <- read.csv("250921-oha_info.csv")

#Read in app observation data
Full_h_app_data <- read.csv("250921-app_tracking_symptoms.csv")

n_distinct(Full_h_user_info$user_id)
n_distinct(Full_h_app_data$user_id)

# Remove observations that are before the Visible study period 
Study_period_h_app_data <- Full_h_app_data %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  filter(observation_date >= as.Date("2022-09-07"))

n_distinct(Study_period_h_app_data$user_id)

#'*Only keep users that appear in both datasets*

# Find user ids that appear in both data frames
common_users_h <- intersect(Full_h_user_info$user_id, Study_period_h_app_data$user_id)

H_app_data_clean <- Study_period_h_app_data %>% 
  filter(user_id %in% common_users_h)

Full_h_user_info_clean <- Full_h_user_info %>% 
  filter(user_id %in% common_users_h)

n_distinct(H_app_data_clean$user_id)
n_distinct(Full_h_user_info_clean$user_id)

#'*Remove users that are outside the age criteria*

Users_outside_age_crit_h <- Full_h_user_info_clean %>%
  filter(!(age_years >= 18 & age_years <= 45))

n_distinct(Users_outside_age_crit_h$user_id)

Users_to_exclude_h <- c(Users_outside_age_crit_h$user_id)

H_app_data_clean <- H_app_data_clean [!(H_app_data_clean$user_id %in% Users_to_exclude_h), ]
Full_h_user_info_clean <- Full_h_user_info_clean[!(Full_h_user_info_clean$user_id %in% Users_to_exclude_h), ]

n_distinct(Full_h_user_info_clean$user_id)
n_distinct(H_app_data_clean$user_id)

#'*Remove users that are on HRT or don't disclose contraception type*

# HRT users 
HRT_data <- Full_h_user_info_clean %>%
  filter(contraception_hrt == 1)

n_distinct(HRT_data$user_id)

# Other contraception users 
Other_contra_data <- Full_h_user_info_clean %>%
  filter(contraception_other == 1)

n_distinct(Other_contra_data$user_id)

# Remove
Users_to_exclude_contra <- Full_h_user_info_clean %>%
  filter(if_any(c(
    contraception_hrt,
    contraception_other), ~ . == 1)) %>%
  pull(user_id)

H_app_data_clean <- H_app_data_clean %>% 
  filter(!user_id %in% Users_to_exclude_contra)

Full_h_user_info_clean <- Full_h_user_info_clean %>%
  filter(!user_id %in% Users_to_exclude_contra)

n_distinct(Full_h_user_info_clean$user_id)
n_distinct(H_app_data_clean$user_id)

#'*Add categories to user info*

# Put age into categories 

Full_h_user_info_clean <- Full_h_user_info_clean %>%
  mutate(age_category = case_when(
    age_years < 30 ~ "<30",
    age_years >= 30 & age_years <= 39 ~ "30-39",
    age_years >= 40 ~ "≥40"))

# Put bmi into categories 

Full_h_user_info_clean <- Full_h_user_info_clean %>%
  mutate(
    bmi_category = case_when(
      round(bmi, 1) < 18.5 ~ "Underweight",
      round(bmi, 1) >= 18.5 & round(bmi, 1) <= 24.9 ~ "Healthy weight",
      round(bmi, 1) >= 25.0 & round(bmi, 1) <= 29.9 ~ "Overweight",
      round(bmi, 1) >= 30.0 ~ "Obese"
    )
  )

# Add nulligravidity as a category
Full_h_user_info_clean <- Full_h_user_info_clean %>%
  mutate(nulligravida = if_else(pregnancy == "never_been_pregnant", "1", "0"))

n_distinct(Full_h_user_info_clean$user_id)

# Categorise contraception with trumping rules (combined trumps progestin only, none only applies if no hormonal contraception is selected)
Full_h_user_info_clean <- Full_h_user_info_clean %>%
  rowwise() %>%
  mutate(
    contraception_list = list(c(
      if (contraception_none == 1 |
          contraception_recently_came_off_contraception == 1 |
          contraception_homeopathic_remedies == 1 |
          contraception_fertility_awareness == 1 |
          contraception_condoms == 1 |
          contraception_copper_iud == 1 |
          contraception_withdrawal_method == 1 |
          contraception_partner_vasectomy == 1 |
          contraception_cyproterone_cocyprindiol == 1 |
          contraception_female_sterilisation == 1 |
          contraception_iub_ballerine == 1) "None" else NULL,
      
      if (contraception_progesterone_only_pill == 1 |
          contraception_nexplanon_implant == 1 |
          contraception_depot_injection_or_implant == 1 |
          contraception_hormonal_coil == 1 |
          contraception_depot_injection == 1) "Progestin only" else NULL,
      
      if (contraception_vaginal_ring == 1 |
          contraception_combined_pill == 1 |
          contraception_contraceptive_patch == 1) "Estrogen & Progestin" else NULL)),
    
    # Apply trumping rule
    Contraception_type = {
      types <- contraception_list[[1]]
      
      # "Estrogen & Progestin" trumps "Progestin only"
      if ("Estrogen & Progestin" %in% types & "Progestin only" %in% types) {
        types <- setdiff(types, "Progestin only")
      }
      
      # Remove "None" if any hormonal contraception is present
      if (any(c("Estrogen & Progestin", "Progestin only") %in% types)) {
        types <- setdiff(types, "None")
      }
      
      if (length(types) == 0) NA_character_ else paste(types, collapse = "; ")
    }
  ) %>%
  ungroup() %>%
  select(-contraception_list)

#'*Keep only those who report at least one bleeding day*

Period_h_app_data <- H_app_data_clean %>%
  filter(symptom_type == "flow") %>% # Find period symptoms
  distinct(user_id, observation_date, .keep_all = FALSE) %>% # Collapse multiple period-related symptoms into one row per user per day
  mutate(period = 1) %>% # Add a new column called period, set to 1 for every row 
  complete(user_id, observation_date, fill = list(period = 0)) # Ensures all combinations of user_id and observation date are created, and imputes 0 for all new combos

n_distinct(Period_h_app_data$user_id)

#'*Assign period start date for each user*

# Assign period start date 
Period_start_date_assigned_h <- Period_h_app_data %>%
  arrange(user_id, observation_date) %>%      # ensure correct order
  group_by(user_id) %>%                       
  mutate(
    period_start =
      period == 1 &                           # bleeding today
      lag(period, 1, default = 0) != 1 &      # no bleeding 1 day before
      lag(period, 2, default = 0) != 1 &      # no bleeding 2 days before
      lag(period, 3, default = 0) != 1 &      # no bleeding 3 days before
      lag(period, 4, default = 0) != 1 &      # no bleeding 4 days before
      lead(period, 1, default = 0) == 1 &     # bleeding continues
      lead(period, 2, default = 0) == 1 &     # bleeding continues
      lead(period, 9, default = 0) != 1 &     # not another bleed too soon
      lead(period, 10, default = 0) != 1,     # not another bleed too soon
    
    period_group = cumsum(period_start)       # cycle counter per user
  ) %>%
  ungroup() %>%
  mutate(
    cycle_id = paste(user_id, period_group, sep = "_")
  )

n_distinct(Period_start_date_assigned_h$user_id)
n_distinct(Period_start_date_assigned_h$cycle_id)

#'*Calculate cycle length and plot cycle length distribution*

# Calculate cycle lengths
Cycle_length_data_h <- Period_start_date_assigned_h %>%
  group_by(user_id, period_group) %>%
  summarize(cycle_length = n()) %>% 
  ungroup()

# Put cycle length data back in 
Cycle_data_h <- left_join(Period_start_date_assigned_h, 
                          Cycle_length_data_h, by = c("user_id", "period_group"))

n_distinct(Cycle_data_h$cycle_id)
n_distinct(Cycle_data_h$user_id)

#'*Find number of cycles falling within the range of 24 to 38 days & remove cycles that are too short/too long*

#Remove cycles out of range 
Short_cycles_h <- Cycle_data_h %>%
  filter(cycle_length < 24)

n_distinct(Short_cycles_h$cycle_id)
n_distinct(Short_cycles_h$user_id)

Long_cycles_h <- Cycle_data_h %>%
  filter(cycle_length > 38)

n_distinct(Long_cycles_h$cycle_id)
n_distinct(Long_cycles_h$user_id)

Cycles_within_range_h <- Cycle_data_h %>%
  filter(cycle_length >= 24 & cycle_length <= 38)

n_distinct(Cycles_within_range_h$cycle_id)
n_distinct(Cycles_within_range_h$user_id)

#'*Assign menstrual phase*

Cycles_within_range_h <- Cycles_within_range_h %>%
  arrange(user_id, observation_date) %>%      # ensure correct order
  group_by(user_id) %>%       
  mutate(period_end = !period_start & # Period end when not period start &... 
           period == 1 & # bleeding &
           lag(period, n = 1) == 1 & # 2 days before also bleeding (should have been imputed if missing) &
           lag(period, n = 2) == 1 &
           lead(period) == 0) %>% # day after is not bleeding 
  ungroup()

Menstrual_phase_data_all_cycles_within_range_h <- Cycles_within_range_h %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  group_by(cycle_id) %>%
  mutate(
    period_start_date = if_else(period_start, observation_date, as.Date(NA)),
    period_end_date = if_else(period_end, observation_date, as.Date(NA))
  ) %>%
  fill(period_start_date) %>%
  fill(period_end_date, .direction = "up") %>%
  ungroup()

Menstrual_phase_data_all_cycles_within_range_h <- Menstrual_phase_data_all_cycles_within_range_h %>%
  mutate(Menstrual = observation_date >= period_start_date & observation_date <= period_end_date,
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

#'*Integrate menstrual phase data with the original symptom data set*

# Add missing dates to app data
Complete_app_data_h <- H_app_data_clean %>%
  complete(user_id, observation_date) 

Complete_app_data_h$observation_date <- as.Date(Complete_app_data_h$observation_date)

# Join menstrual data & symptom data 
Menstrual_and_all_symptom_data_all_cycles_within_range_h <- Complete_app_data_h %>%
  inner_join(Menstrual_phase_data_all_cycles_within_range_h, by = c("user_id", "observation_date"), suffix = c("", ".x"))

n_distinct(Menstrual_and_all_symptom_data_all_cycles_within_range_h$user_id)
n_distinct(Menstrual_and_all_symptom_data_all_cycles_within_range_h$cycle_id)

#'*Keep only users that have at least one observation in every phase*

# Identify users with at least one observation in every phase
users_all_phases <- Menstrual_and_all_symptom_data_all_cycles_within_range_h %>%
  filter(symptom_type != "flow") %>%
  distinct(user_id, Menstrual_phase) %>%
  count(user_id) %>%
  filter(n == n_distinct(Menstrual_and_all_symptom_data_all_cycles_within_range_h$Menstrual_phase)) %>%
  pull(user_id)

# Keep only rows for those users
Users_all_phases_data <- Menstrual_and_all_symptom_data_all_cycles_within_range_h %>%
  filter(user_id %in% users_all_phases)

n_distinct(Users_all_phases_data$user_id)
n_distinct(Users_all_phases_data$cycle_id)

#'*Clean user info to match cleaned app data*

Users_in_model_data_h <- Users_all_phases_data %>%
  group_by(user_id) %>%
  pull(user_id) %>%
  unique()

Cleaned_model_user_info_h <- Full_h_user_info_clean[(Full_h_user_info_clean$user_id %in% Users_in_model_data_h), ]

#'*Remove rows where have multiple user_id & oha_date combos*
Cleaned_model_user_info_h <- Cleaned_model_user_info_h %>%
  group_by(user_id, oha_date) %>%
  summarise(
    across(everything(), ~ first(.[!is.na(.)]), .names = "{.col}"),
    .groups = "drop")

# Make sure dates are Date objects
Users_all_phases_data <- Users_all_phases_data %>%
  mutate(observation_date = as.Date(observation_date))

Cleaned_model_user_info_h <- Cleaned_model_user_info_h %>%
  mutate(oha_date = as.Date(oha_date))

#'*Combine menstrual and symptom data with user info*

# Sort user_info by user_id and oha_date
Cleaned_model_user_info_h <- Cleaned_model_user_info_h %>% arrange(user_id, oha_date)

# Join and pick the appropriate oha_date
Cleaned_h_data <- Users_all_phases_data %>%
  left_join(Cleaned_model_user_info_h, by = "user_id", relationship = "many-to-many") %>%
  group_by(user_id, observation_date) %>%
  mutate(
    # pick latest oha_date <= observation_date, if none exists, use earliest oha_date
    oha_date_use = if(any(oha_date <= observation_date)) {
      max(oha_date[oha_date <= observation_date])
    } else {
      min(oha_date)
    }
  ) %>%
  # keep only the row corresponding to oha_date_use
  filter(oha_date == oha_date_use) %>%
  ungroup() %>%
  select(-oha_date_use)

n_distinct(Cleaned_h_data$user_id)
n_distinct(Cleaned_h_data$cycle_id)

# Find the first and last observation_date
date_range <- Cleaned_h_data %>%
  summarise(
    first_date = min(observation_date, na.rm = TRUE),
    last_date  = max(observation_date, na.rm = TRUE)
  )

date_range

} else {
  print("If you would like to request these files for scientific purposes, contact Hertility Health at natalie@hertilityhealth.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}

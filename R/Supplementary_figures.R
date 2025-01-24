#'*Read in libraries*
library(tidyverse)
library(ggpubr)
library(grDevices)
library(rcompanion)
library(viridis)
library(pheatmap)
library(ordinal)
library(coin)

if(file.exists('Cleaned_model_user_info_all.rds'))
{
  Cleaned_model_user_info_all                         <- read_rds('Cleaned_model_user_info_all.rds')
  Cleaned_model_survey_data                           <- read_rds('Cleaned_model_survey_data.rds')
  Full_menstrual_symptom_data_all_cycles_within_range <- read_rds('Full_menstrual_symptom_data_all_cycles_within_range.rds')  
  
  ##'*Figure S1 - Age distribution*
  
  median_age <- median(Cleaned_model_user_info_all$age, na.rm = TRUE)
  age_iqr <- IQR(Cleaned_model_user_info_all$age, na.rm = TRUE)
  
  Age_histogram <- ggplot(Cleaned_model_user_info_all, aes(x = age)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    theme_pubr() +
    theme(text = element_text(family = "Times")) +
    labs(x = "Age",
         y = "Frequency")
  
  ggsave('figures_and_tables/Supplementary/Age_histogram.pdf', plot = Age_histogram, width = 10, height = 5, units = "cm")
  
  #'*Table S1 Demographics split by disease type*
  
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
  write.csv(final_table_long_covid, "figures_and_tables/Supplementary/demographics_long_covid.csv", row.names = FALSE)
  write.csv(final_table_me_cfs, "figures_and_tables/Supplementary/demographics_me_cfs.csv", row.names = FALSE)
  write.csv(final_table_both, "figures_and_tables/Supplementary/demographics_both.csv", row.names = FALSE)
  
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
  
  #Kruskal Wallis 
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
  
  #'*Figure S2 Distribution of the number of users tracking symptoms over time period*
  
  Standard_symptom_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") 
  
  # Group the data by date and health variable name, and count the number of users tracking each symptom
  symptoms_over_time <- Standard_symptom_data %>%
    mutate(observation_date_pk = as.Date(observation_date_pk)) %>%
    filter(observation_date_pk < as.Date('2024-03-08')) %>%
    group_by(observation_date_pk, health_variable_name) %>%
    summarize(users_tracking = n_distinct(user_id_pk), .groups = "drop") 
  
  # Create a line plot to track the number of symptoms logged over time
  symptom_tracking_plot <- ggplot(symptoms_over_time, aes(x = observation_date_pk, y = users_tracking, colour = health_variable_name)) +
    geom_line(size = 1) +
    labs(x = "Date", y = "Number of Users Tracking", colour = NULL) +
    theme_pubr() +
    theme(
      text = element_text(family = "Times"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  # Save the plot
  ggsave("figures_and_tables/Supplementary/symptom_tracking_plot.pdf",
         plot = symptom_tracking_plot,
         units = "cm",
         width = 30, height = 16)
  
  ##'*Figure S3 How many track each symptom - visualised*
  
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
  
  ggsave("figures_and_tables/Supplementary/histogram_faceted_symptom_plot.tiff",
         plot = histogram_faceted_symptom_plot, 
         units = "cm",
         width = 20, height = 16)
  
  ##'*Figure S4 and S5, symptom heatmaps*
  
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
  
  ggsave("figures_and_tables/Supplementary/symptom_symptom_heatmap.pdf",
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
  
  ggsave("figures_and_tables/Supplementary/symptom_user_heatmap.pdf",
         plot = symptom_user_heatmap, 
         units = "cm",
         width = 17, height = 15)
  
  
  # Count the number of distinct users tracking each symptom
  full_symptom_user_counts <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(health_variable_name) %>%
    summarise(user_count = n_distinct(user_id_pk)) %>%
    arrange(desc(user_count))
  
  write_csv(full_symptom_user_counts, file = "figures_and_tables/Supplementary/full_symptom_user_counts.csv")
  
  ##'*Figure S6 and Table S2 - Individual symptom spine plots and extended cochran armitage tests*
  ##'*(analysis restricted to users that have at least three cycles, and taking data only from the first three cycles)*
  
  # Retain only users with at least 3 cycles, then filter for their first 3 cycles
  First_three_cycles_and_all_symptom_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(is_published == "true" & health_variable_type == "Symptom") %>%
    group_by(user_id_pk) %>%
    filter(n_distinct(cycle_id) >= 3) %>%  
    arrange(cycle_id) %>%                
    filter(cycle_id %in% unique(cycle_id)[1:3])  
  
  # Function to generate a colour palette for each symptom
  generate_colour_palette <- function(base_colour) {
    # Create a palette of 4 shades from the base colour, going from dark to light
    colorRampPalette(c(base_colour, "white"))(4)[4:1]  # Reverse the order to go from dark to light
  }
  
  # Generate the viridis palette with 36 colours
  viridis_palette <- viridis(36)
  
  # Count the number of distinct users tracking each symptom
  symptom_user_counts <- First_three_cycles_and_all_symptom_data %>%
    group_by(health_variable_name) %>%
    summarise(user_count = n_distinct(user_id_pk)) %>%
    arrange(desc(user_count))
  
  # Extract symptoms sorted by the number of users
  sorted_symptoms <- symptom_user_counts$health_variable_name
  
  # Create a list to store spine plot tables
  spine_plot_tables <- list()
  
  # Chi-Square Results List
  chi_square_results <- list()
  
  # Loop through each symptom in the sorted order
  for (symptom in sorted_symptoms) {
    cat("Processing", symptom, "...\n")
    
    # Filter data by symptom name
    symptom_data_first_3 <- First_three_cycles_and_all_symptom_data %>%
      filter(health_variable_name == symptom)
    
    if (nrow(symptom_data_first_3) == 0) {
      cat("No data available for", symptom, "- skipping...\n")
      next
    }
    
    # Ensure correct factor levels for Menstrual_phase
    symptom_data_first_3$Menstrual_phase <- factor(symptom_data_first_3$Menstrual_phase, 
                                                   levels = c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
    
    # Convert observation_value to numeric
    symptom_data_first_3$observation_value <- as.numeric(symptom_data_first_3$observation_value)
    
    # Create spine plot table
    symptom_data_first_3_table_spine <- xtabs(~ Menstrual_phase + factor(observation_value, 
                                                                         levels = c("3", "2", "1", "0")), data = symptom_data_first_3)
    
    # Save to the list
    spine_plot_tables[[symptom]] <- symptom_data_first_3_table_spine
    
    # Extended Cochran-Armitage Test
    symptom_data_first_3_table <- xtabs(~ Menstrual_phase + observation_value, data = symptom_data_first_3)
    
    # Apply Cochran-Armitage test
    chisq_result <- chisq_test(symptom_data_first_3_table, 
                               scores = list("observation_value" = c(0, 1, 2, 3)))
    
    # Pairwise comparisons using the pairwiseOrdinalIndependence function
    PT <- pairwiseOrdinalIndependence(symptom_data_first_3_table, compare = "row")
    
    # Filter significant comparisons (p.adjust < 0.05)
    significant_comparisons <- as.data.frame(PT[PT$p.adjust < 0.05, ])
    
    significant_comparisons$formatted_p.adjust <- formatC(signif(significant_comparisons$p.adjust, digits = 3), 
                                                          format = "fg", flag = "#")
    
    significant_comparisons$asterisks <- cut(significant_comparisons$p.adjust,
                                             breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                             labels = c("****", "***", "**", "*", ""))
    
    # Save significant comparisons as CSV
    result_path <- paste0('figures_and_tables/Supplementary/',gsub(" ", "_", symptom), "_significant_comparisons.csv")
    write.csv(significant_comparisons, result_path, row.names = FALSE)
    
    # Save chi-square test result for each symptom
    chi_square_results[[symptom]] <- chisq_result
    
    # Get the number of users tracking this symptom
    user_count <- symptom_user_counts$user_count[symptom_user_counts$health_variable_name == symptom]
    
    cat("Completed processing for", symptom, "\n")
  }
  
  # Print chi-square test results
  cat("\nChi-Square Test Results:\n")
  for (symptom in names(chi_square_results)) {
    result <- chi_square_results[[symptom]]
    cat("\n", symptom, "Chi-Square Test:\n")
    print(result)
  }
  # Open a PDF device for the combined plot
  pdf("figures_and_tables/Supplementary/All_symptoms_plot_supp.pdf", width = 15 / 2.54, height = 23 / 2.54)  # Dimensions in inches
  
  # Set up the layout with 4 columns and 9 rows for a total of 36 plots
  layout_matrix <- matrix(1:36, ncol = 4, byrow = TRUE)
  layout(layout_matrix)
  
  # Adjust outer margins and plot-specific margins
  par(oma = c(1.5, 1.5, 1.5, 1.5))  # Larger outer margins for the overall layout
  par(mar = c(0.6, 0.6, 0.6, 0.6))      # Margins for individual plots to prevent cutting off edges
  par(family = "serif") 
  
  
  # Custom function to plot and add titles with appropriately scaled text
  plot_spine <- function(data, col, title) {
    spineplot(data, col = col, xlab = "", ylab = "", axes = FALSE)
    mtext(title, side = 3, line = 0.2, cex = 0.5, font = 2)  # Title size and position adjusted
  }
  
  # Loop over the stored spine plot tables
  for (symptom in names(spine_plot_tables)) {
    # Check if the symptom has significant comparisons
    symptom_data_first_3 <- First_three_cycles_and_all_symptom_data %>%
      filter(health_variable_name == symptom)
    symptom_data_first_3_table <- xtabs(~ Menstrual_phase + observation_value, data = symptom_data_first_3)
    
    # Apply Cochran-Armitage test and check for significance
    PT <- pairwiseOrdinalIndependence(symptom_data_first_3_table, compare = "row")
    significant_comparisons <- as.data.frame(PT[PT$p.adjust < 0.05, ])
    
    # Assign the appropriate colour palette
    if (nrow(significant_comparisons) == 0) {
      symptom_palette <- grey.colors(4, start = 1, end = 0.3)  # Shades from white to dark grey
    } else {
      base_colour <- viridis_palette[which(sorted_symptoms == symptom)]
      symptom_palette <- generate_colour_palette(base_colour)
    }
    
    # Get the number of users tracking this symptom
    user_count <- symptom_user_counts$user_count[symptom_user_counts$health_variable_name == symptom]
    
    # Create the plot title including the number of users
    plot_title <- paste(symptom, " (n=", user_count, ")", sep = "")
    
    # Plot the spine plot with the correct palette and title
    plot_spine(spine_plot_tables[[symptom]], symptom_palette, plot_title)
  }
  
  # Close the PDF device
  dev.off()
  
  ##'*Tables S3-S38 - Individual symptom mixed effects models*
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
  
  # Optionally, print the final summary
  print("Model fit statistics and random effects saved to Symptom_Model_Fit_Stats.csv")
  
  ##'*Figure S7 and Table S39 - Crash spine plot and chi-squared test*
  ##'*(analysis restricted to users that have at least three cycles, and taking data only from the first three cycles)*
  
  #'*Look at menstrual phase and crash data
  Crash_data <- Full_menstrual_symptom_data_all_cycles_within_range %>%
    filter(health_variable_id_pk == 60) %>%
    group_by(user_id_pk) %>%
    filter(n_distinct(cycle_id) >= 3) %>%  
    arrange(cycle_id) %>%                
    filter(cycle_id %in% unique(cycle_id)[1:3])  
  
  Crash_data$Menstrual_phase <- factor(Crash_data$Menstrual_phase,levels=c("Menstrual", "Follicular", "Early luteal", "Late luteal", "Premenstrual"))
  
  n_distinct(First_three_cycles_and_all_symptom_data$user_id_pk)
  n_distinct(Crash_data$user_id_pk)
  
  #'Chi-square test
  
  # Create a contingency table of number of crashes in each menstrual phases 
  Crash_data_table <- xtabs(~ Menstrual_phase + observation_value, data = Crash_data)
  
  Crash_data_table <- Crash_data_table[, c("1", "0")]
  
  # Generate colours for spine plot
  shades_of_orange <- colorRampPalette(c("#c76706", "#f0a150"))(2)
  
  mosaicplot(Crash_data_table, 
             color=shades_of_orange, 
             cex.axis=0.8,
             xlab = "Menstrual phase", ylab = "")
  
  #Save as pdf
  pdf("Crash_mosaic_plot.pdf", width = 6, height = 5)
  mosaicplot(Crash_data_table, 
             color=shades_of_orange, 
             cex.axis=0.8,
             xlab = "Menstrual phase", ylab = "")
  dev.off() 
  
  #Chi-square test
  chisq_result = chisq.test(Crash_data_table)
  
  #Pairwise comparisons
  PT = pairwiseNominalIndependence(Crash_data_table,
                                   compare = "row",
                                   fisher  = FALSE,
                                   gtest   = FALSE,
                                   chisq   = TRUE,
                                   method  = "fdr",
                                   digits  = 3)
  
  # Filter for significant comparisons
  as.data.frame(significant_comparisons_crashes <- PT[PT$p.adj.Chisq < 0.05, ])
  
  significant_comparisons_crashes$formatted_p.adjust <- formatC(signif(significant_comparisons_crashes$p.adj.Chisq, digits = 3), format = "fg", flag = "#")
  
  # Add a new column with asterisks based on p-value thresholds
  significant_comparisons_crashes$asterisks <- cut(significant_comparisons_crashes$p.adj.Chisq,
                                                   breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                                   labels = c("****", "***", "**", "*", ""))
  
} else {
  print("If you would like to request these files for scientific purposes, contact Visible Health Inc at info@makevisible.com and Abigail Goodship at abigail.goodship21@imperial.ac.uk")
}  
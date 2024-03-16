# Load R libraries ----
library(ctrdata)
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(gtsummary)

# LOAD DATA FROM REGISTRIES ----

# Connect to an SQLite database that is stored in a file on the local system:
db <- nodbi::src_sqlite(
  dbname = "tfm-FF_nov23.sqlite",
  collection = "tfm-FF_nov23"
)

# Retrieve protocol data from EUCTR:
ctrLoadQueryIntoDb(
  queryterm = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=&age=under-18",
  con = db
)

# Get all records that have values in the fields of interest:
ff_nov23_data <- dbGetFieldsIntoDf(
  fields = c(
    #EUCTR
    "a2_eudract_number",
    "a1_member_state_concerned",
    "a3_full_title_of_the_trial",
    "a7_trial_is_part_of_a_paediatric_investigation_plan",
    "b1_sponsor",
    "b1_sponsor._b1_sponsor",
    "b1_sponsor.b11_name_of_sponsor",
    "b1_sponsor.b31_and_b32_status_of_the_sponsor",
    "b1_sponsor.b134_country",
    "b1_sponsor.b4_sources_of_monetary_or_material_support",
    "b1_sponsor.b4_sources_of_monetary_or_material_support.b41_name_of_organisation_providing_support",
    "b1_sponsor.b4_sources_of_monetary_or_material_support.b42_country",
    "b1_sponsor.b51_name_of_organisation",
    "ctrname",
    "e11_medical_conditions_being_investigated",
    "e112_therapeutic_area",
    "e13_condition_being_studied_is_a_rare_disease",
    "e61_diagnosis",
    "e610_pharmacogenetic",
    "e611_pharmacogenomic",
    "e612_pharmacoeconomic",
    "e613_others",
    "e62_prophylaxis",
    "e63_therapy",
    "e64_safety",
    "e65_efficacy",
    "e66_pharmacokinetic",
    "e67_pharmacodynamic",
    "e68_bioequivalence",
    "e69_dose_response",
    "e71_human_pharmacology_phase_i",
    "e711_first_administration_to_humans",
    "e712_bioequivalence_study",
    "e713_other",
    "e72_therapeutic_exploratory_phase_ii",
    "e73_therapeutic_confirmatory_phase_iii",
    "e74_therapeutic_use_phase_iv",
    "e81_controlled",
    "e811_randomised",
    "e812_open",
    "e813_single_blind",
    "e814_double_blind",
    "e815_parallel_group",
    "e816_cross_over",
    "e817_other",
    "e821_other_medicinal_products",
    "e822_placebo",
    "e823_other",
    "e824_number_of_treatment_arms_in_the_trial",
    "e841_number_of_sites_anticipated_in_member_state_concerned",
    "e85_the_trial_involves_multiple_member_states",
    "e851_number_of_sites_anticipated_in_the_eea",
    "e861_trial_being_conducted_both_within_and_outside_the_eea",
    "e862_trial_being_conducted_completely_outside_of_the_eea",
    "e87_trial_has_a_data_monitoring_committee",
    "e891_in_the_member_state_concerned_months",
    "e891_in_the_member_state_concerned_years",
    "e892_in_all_countries_concerned_by_the_trial_months",
    "e892_in_all_countries_concerned_by_the_trial_years",
    "f11_number_of_subjects_for_this_age_range",
    "f11_trial_has_subjects_under_18",
    "f111_in_utero",
    "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks",
    "f113_newborns_027_days",
    "f114_infants_and_toddlers_28_days23_months",
    "f115_children_211years",
    "f116_adolescents_1217_years",
    "f1161_number_of_subjects_for_this_age_range",
    "f12_adults_1864_years",
    "f121_number_of_subjects_for_this_age_range",
    "f13_elderly_65_years",
    "f21_female",
    "f22_male",
    "f31_healthy_volunteers",
    "f32_patients",
    "f33_specific_vulnerable_populations",
    "f331_women_of_childbearing_potential_not_using_contraception_",
    "f332_women_of_childbearing_potential_using_contraception",
    "f333_pregnant_women",
    "f334_nursing_women",
    "f335_emergency_situation",
    "f336_subjects_incapable_of_giving_consent_personally",
    "f337_others",
    "f3371_details_of_other_specific_vulnerable_populations",
    "f41_in_the_member_state",
    "f421_in_the_eea",
    "f422_in_the_whole_clinical_trial",
    "f5_plans_for_treatment_or_care_after_the_subject_has_ended_the_participation_in_the_trial_if_it_is_different_from_the_expected_normal_treatment_of_that_condition",
    "g4_investigator_networks.g4_investigator_network_to_be_involved_in_the_trial",
    "g4_investigator_networks.g41_name_of_organisation",
    "n_competent_authority_decision",
    "n_date_of_competent_authority_decision",
    "p_date_of_the_global_end_of_the_trial",
    "p_end_of_trial_status",
    "record_last_import",
    "x4_clinical_trial_type",
    "x5_trial_status",
    "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
    "e12_meddra_classification",
    "e12_meddra_classification.e12_classification_code",
    "e12_meddra_classification.e12_level",
    "e12_meddra_classification.e12_system_organ_class",
    "e12_meddra_classification.e12_term",
    "e12_meddra_classification.e12_version",
    "e83_single_site_trial",
    "e84_multiple_sites_in_member_state",
    "e841_number_of_sites_anticipated_in_member_state_concerned",
    "e840_multiple_sites_globally",
    "e85_the_trial_involves_multiple_member_states",
    "e861_trial_being_conducted_both_within_and_outside_the_eea",
    "e862_trial_being_conducted_completely_outside_of_the_eea",
    "e863_trial_sites_planned_in",
    "e87_trial_has_a_data_monitoring_committee",
    "e891_in_the_member_state_concerned_days",
    "e891_in_the_member_state_concerned_months",
    "e891_in_the_member_state_concerned_years",
    "e892_in_all_countries_concerned_by_the_trial_days",
    "e892_in_all_countries_concerned_by_the_trial_months",
    "e892_in_all_countries_concerned_by_the_trial_years",
    "n_competent_authority_decision",
    "n_date_of_competent_authority_decision",
    "n_date_of_ethics_committee_opinion",
    "n_ethics_committee_opinion_of_the_trial_application",
    "p_end_of_trial_status",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "a8_ema_decision_number_of_paediatric_investigation_plan",
    "f11_number_of_subjects_for_this_age_range",
    "f1151_number_of_subjects_for_this_age_range",
    "h41_third_country_in_which_the_trial_was_first_authorised",
    "p_date_of_the_global_end_of_the_trial"
  ),
  con = db
)

ff_nov23_sponsor <- dbGetFieldsIntoDf(
  fields = c(
    #EUCTR
    "b1_sponsor"
  ),
  con = db
)

ff_nov23_network <- dbGetFieldsIntoDf(
  fields = c(
    #EUCTR
    "g4_investigator_networks"
  ),
  con = db
)

ff_nov23_meddra <- dbGetFieldsIntoDf(
  fields = c(
    #EUCTR
    "e12_meddra_classification"
  ),
  con = db
)

# Convert the data from wide to long format
ff_nov23_sponsor_long <- dfTrials2Long(df = ff_nov23_sponsor)
ff_nov23_network_long <- dfTrials2Long(df = ff_nov23_network)
ff_nov23_meddra_long <- dfTrials2Long(df = ff_nov23_meddra)


# LOAD LOCAL DATA ----

# Specify the filepath of the Excel file
filepath_match <- "TFM-FF_g41-new_name_match.xlsx"

# Read the data from the Excel file
ff_nov23_match <- read_excel(filepath_match)

# Specify the filepath of the Excel file
filepath_char <- "TFM-FF_network_char.xlsx"

# Read the data from the Excel file
ff_nov23_char <- read_excel(filepath_char)

# Specify the filepath of the Excel file
filepath_country <- "ff-nov23-country_match.xlsx"

# Read the data from the Excel file
ff_nov23_country_match <- read_excel(filepath_country)



# CHECK LOCAL DATA ----

# Clean network names
ff_nov23_network_long$value <- gsub(" /", "/", trimws(tolower(ff_nov23_network_long$value)))

# Clean match names
ff_nov23_match$g4_investigator_networks.g41_name_of_organisation <- gsub(" /", "/", trimws(tolower(ff_nov23_match$g4_investigator_networks.g41_name_of_organisation)))

# Redo the matching
network_names <- ff_nov23_network_long$value[ff_nov23_network_long$name == "g4_investigator_networks.g41_name_of_organisation"]
non_matching_names <- network_names[!(network_names %in% ff_nov23_match$g4_investigator_networks.g41_name_of_organisation)]



# TRIALS DATAFRAME CLEANING AND TRANSFORMING VARIABLES ----

ff_nov23_trials <- data.frame(a2_eudract_number = unique(ff_nov23_data$a2_eudract_number))

## n_protocol variables ----

protocol_counts <- as.data.frame(table(ff_nov23_data$a2_eudract_number))
colnames(protocol_counts) <- c("a2_eudract_number", "n_protocols")
ff_nov23_trials <- merge(ff_nov23_trials, protocol_counts, by = "a2_eudract_number")

ff_nov23_data_eu <- ff_nov23_data[!grepl("3RD$", ff_nov23_data$`_id`), ]
protocol_counts_eu <- as.data.frame(table(ff_nov23_data_eu$a2_eudract_number))
colnames(protocol_counts_eu) <- c("a2_eudract_number", "n_protocols_EU")
ff_nov23_trials <- merge(ff_nov23_trials, protocol_counts_eu, by = "a2_eudract_number", all.x = TRUE)
ff_nov23_trials$n_protocols_EU[is.na(ff_nov23_trials$n_protocols_EU)] <- 0

ff_nov23_data_3rd <- ff_nov23_data[grepl("3RD$", ff_nov23_data$`_id`), ]
protocol_counts_3rd <- as.data.frame(table(ff_nov23_data_3rd$a2_eudract_number))
colnames(protocol_counts_3rd) <- c("a2_eudract_number", "n_protocols_3rd")
ff_nov23_trials <- merge(ff_nov23_trials, protocol_counts_3rd, by = "a2_eudract_number", all.x = TRUE)
ff_nov23_trials$n_protocols_3rd[is.na(ff_nov23_trials$n_protocols_3rd)] <- 0

## sponsor variable ----

commercial_ids <- ff_nov23_sponsor_long$`_id`[ff_nov23_sponsor_long$name == "b1_sponsor.b31_and_b32_status_of_the_sponsor" & tolower(ff_nov23_sponsor_long$value) == "commercial"]
ff_nov23_data$sponsor_commercial <- ff_nov23_data$`_id` %in% commercial_ids

non_commercial_ids <- ff_nov23_sponsor_long$`_id`[ff_nov23_sponsor_long$name == "b1_sponsor.b31_and_b32_status_of_the_sponsor" & tolower(ff_nov23_sponsor_long$value) == "non-commercial"]
ff_nov23_data$sponsor_non_commercial <- ff_nov23_data$`_id` %in% non_commercial_ids


ff_nov23_trials$sponsor_commercial <- sapply(ff_nov23_trials$a2_eudract_number, function(x) any(ff_nov23_data$sponsor_commercial[ff_nov23_data$a2_eudract_number == x]))
ff_nov23_trials$sponsor_non_commercial <- sapply(ff_nov23_trials$a2_eudract_number, function(x) any(ff_nov23_data$sponsor_non_commercial[ff_nov23_data$a2_eudract_number == x]))

ff_nov23_trials$sponsor <- ifelse(ff_nov23_trials$sponsor_commercial & ff_nov23_trials$sponsor_non_commercial, 
                                  "Both", 
                                  ifelse(!ff_nov23_trials$sponsor_commercial & !ff_nov23_trials$sponsor_non_commercial, 
                                         "No information", 
                                         ifelse(ff_nov23_trials$sponsor_commercial & !ff_nov23_trials$sponsor_non_commercial, 
                                                "Commercial", 
                                                "Non-commercial"
                                         )
                                  )
)



## status variable ----

status_list <- c("Ongoing", "Completed", "GB - no longer in EU/EEA", "Restarted", "Not Authorised", 
                 "Prematurely Ended", "Temporarily Halted", "Trial now transitioned", "Suspended by CA", "NA")

status_variables <- c("status_ongoing", "status_completed", "status_GB", "status_restarted", "status_notauthorised", 
                      "status_prematurely", "status_temporarily", "status_transitioned", "status_suspended", "status_blank")

for (i in 1:length(status_list)) {
  ff_nov23_trials[, status_variables[i]] <- sapply(ff_nov23_trials$a2_eudract_number, 
                                                   function(x) sum(ff_nov23_data$p_end_of_trial_status[ff_nov23_data$a2_eudract_number == x] == status_list[i]))
}

# Adding status_blank separately
ff_nov23_trials$status_blank <- sapply(ff_nov23_trials$a2_eudract_number, 
                                       function(x) sum(is.na(ff_nov23_data$p_end_of_trial_status[ff_nov23_data$a2_eudract_number == x])))


for (i in 1:nrow(ff_nov23_trials)) {
  if (!is.na(ff_nov23_trials$status_temporarily[i]) && ff_nov23_trials$status_temporarily[i] > 0 |
      !is.na(ff_nov23_trials$status_suspended[i]) && ff_nov23_trials$status_suspended[i] > 0) {
    ff_nov23_trials$status[i] <- "Temporarily Halted"
  } else if (!is.na(ff_nov23_trials$status_prematurely[i]) && ff_nov23_trials$status_prematurely[i] > 0) {
    ff_nov23_trials$status[i] <- "Prematurely Ended"
  } else if (!is.na(ff_nov23_trials$status_ongoing[i]) && ff_nov23_trials$status_ongoing[i] > 0 | 
             !is.na(ff_nov23_trials$status_restarted[i]) && ff_nov23_trials$status_restarted[i] > 0 | 
             !is.na(ff_nov23_trials$status_transitioned[i]) && ff_nov23_trials$status_transitioned[i] > 0) {
    ff_nov23_trials$status[i] <- "Ongoing"
  } else if (!is.na(ff_nov23_trials$status_completed[i]) && ff_nov23_trials$status_completed[i] > 0) {
    ff_nov23_trials$status[i] <- "Completed"
  } else if (!is.na(ff_nov23_trials$status_notauthorised[i]) && ff_nov23_trials$status_notauthorised[i] > 0) {
    ff_nov23_trials$status[i] <- "Not Authorised"
  } else if (!is.na(ff_nov23_trials$status_blank[i]) && ff_nov23_trials$status_blank[i] > 0 |
             !is.na(ff_nov23_trials$status_GB[i]) && ff_nov23_trials$status_GB[i] > 0) {
    ff_nov23_trials$status[i] <- "Unknown"
  }
}

## multinational variable ----

ff_nov23_data <- merge(ff_nov23_data, ff_nov23_country_match[, c("e863_trial_sites_planned_in", "transf_e863")], 
                       by = "e863_trial_sites_planned_in", all.x = TRUE)

# create new "multinational" variable
ff_nov23_trials$multinational <- NA

# loop over each row in the dataframe
for (i in 1:nrow(ff_nov23_trials)) {
  
  # if 'n_protocols' > 1, 'multinational' <- TRUE
  if(ff_nov23_trials$n_protocols[i] > 1){
    
    ff_nov23_trials$multinational[i] <- TRUE
    
    # else if 'n_protocols_EU'=1 and 'n_protocols_3rd'=0, 'multinational' <- FALSE
  } else if(ff_nov23_trials$n_protocols_EU[i] == 1 & ff_nov23_trials$n_protocols_3rd[i] == 0){
    
    ff_nov23_trials$multinational[i] <- FALSE
    
    # else if 'n_protocols_EU'=0 and 'n_protocols_3rd'=1
  } else if(ff_nov23_trials$n_protocols_EU[i] == 0 & ff_nov23_trials$n_protocols_3rd[i] == 1){
    
    # lookup transf_e863 value in ff_nov23_data dataframe for rows with the same 'a2_eudract_number'
    transf_e863_val <- ff_nov23_data$transf_e863[ff_nov23_data$a2_eudract_number == ff_nov23_trials$a2_eudract_number[i]]
    
    if(transf_e863_val == "One") {
      # 'multinational' <- FALSE
      ff_nov23_trials$multinational[i] <- FALSE
    } else if (transf_e863_val == "More") {
      # 'multinational' <- TRUE
      ff_nov23_trials$multinational[i] <- TRUE
    } # no need for a condition for NA, as 'multinational' already defaults to NA
  }
}





## multicentric variable ----

# initialize multicentric variable
ff_nov23_trials$multicentric <- NA

# loop over each row in the trial dataframe
for (i in 1:nrow(ff_nov23_trials)) {
  
  # if 'n_protocols' > 1, 'multicentric' <- TRUE
  if(ff_nov23_trials$n_protocols[i] > 1){
    ff_nov23_trials$multicentric[i] <- TRUE
    
  } else if(ff_nov23_trials$n_protocols_EU[i] == 1){ 
    # if 'n_protocols_EU'=1, lookup e84_multiple_sites_in_member_state in ff_nov23_data
    e84_val <- ff_nov23_data$e84_multiple_sites_in_member_state[ff_nov23_data$a2_eudract_number == ff_nov23_trials$a2_eudract_number[i]]
    
    # replace "Yes" and "No" with TRUE and FALSE for boolean comparison
    if(is.na(e84_val)) {
      # 'multicentric' <- NA
      ff_nov23_trials$multicentric[i] <- NA
    } else if (e84_val == TRUE) {
      # 'multicentric' <- TRUE
      ff_nov23_trials$multicentric[i] <- TRUE
    } else if (e84_val == FALSE) {
      # 'multicentric' <- FALSE
      ff_nov23_trials$multicentric[i] <- FALSE
    }
    
  } else if(ff_nov23_trials$n_protocols_3rd[i] == 1){
    # if 'n_protocols_3rd'=1, lookup e840_multiple_sites_globally in ff_nov23_data
    e840_val <- ff_nov23_data$e840_multiple_sites_globally[ff_nov23_data$a2_eudract_number == ff_nov23_trials$a2_eudract_number[i]]
    
    if(is.na(e840_val)) {
      # 'multicentric' <- NA
      ff_nov23_trials$multicentric[i] <- NA
    } else if (e840_val == TRUE) {
      # 'multicentric' <- TRUE
      ff_nov23_trials$multicentric[i] <- TRUE      
    } else if (e840_val == FALSE) {
      # 'multicentric' <- FALSE
      ff_nov23_trials$multicentric[i] <- FALSE
    }
  }
}

## subjects variable ----

# Group ff_nov23_data by a2_eudract_number
ff_nov23_data <- ff_nov23_data %>%
  group_by(a2_eudract_number)

# Compute subjects variable in the grouped data
subjects_data <- ff_nov23_data %>%
  summarise(subjects = max(f422_in_the_whole_clinical_trial, na.rm = TRUE),
            subjects_eea = max(f421_in_the_eea, na.rm = TRUE),
            subjects_member_state = max(f41_in_the_member_state, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(subjects = case_when(
    subjects > 0 ~ subjects,
    subjects_eea > 0 ~ subjects_eea,
    subjects_member_state > 0 ~ subjects_member_state,
    TRUE ~ NA_real_
  )) %>%
  select(-subjects_eea, -subjects_member_state)

# Merge subjects variable into ff_nov23_trials
ff_nov23_trials <- left_join(ff_nov23_trials, subjects_data, by = "a2_eudract_number")


## subjects dichotomic variable ----

# Define threshold
threshold <- 100

# Create subjects_dic variable based on conditions
ff_nov23_trials$subjects_dic <- ifelse(ff_nov23_trials$subjects >= threshold, TRUE, FALSE)


## average age groups, phase and rare variables ----

# Define variable names
variables <- c("f11_trial_has_subjects_under_18", "f111_in_utero", 
               "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks", 
               "f113_newborns_027_days", "f114_infants_and_toddlers_28_days23_months", 
               "f115_children_211years", "f116_adolescents_1217_years", 
               "f12_adults_1864_years", "f13_elderly_65_years",
               "e71_human_pharmacology_phase_i",	"e72_therapeutic_exploratory_phase_ii",
               "e73_therapeutic_confirmatory_phase_iii",	"e74_therapeutic_use_phase_iv",
               "e13_condition_being_studied_is_a_rare_disease"
               
)

# Initialize resulting variables in ff_nov23_trials
ff_nov23_trials[variables] <- NA


# Loop over each variable
for (variable in variables){
  
  # Loop over each row in the trial dataframe
  for (i in 1:nrow(ff_nov23_trials)) {
    
    # Get matching data
    matching_data <- ff_nov23_data[ff_nov23_data$a2_eudract_number == ff_nov23_trials$a2_eudract_number[i], variable]
    
    # Check if matching data is non-empty
    if(length(matching_data) > 0) {
      # Unlist and convert logical values to numeric
      matching_data <- as.numeric(unlist(matching_data))
      
      # Replace NA values with 0 (which is equivalent to 'FALSE')
      matching_data[is.na(matching_data)] <- 0 
      
      # Calculate the average of TRUEs
      average_true <- mean(matching_data)
      
      # Assign the value to the trial dataframe
      ff_nov23_trials[i, variable] <- average_true
    }
  }
}



## adults variable ----

ff_nov23_trials <- ff_nov23_trials %>%
  mutate(adults = ifelse(f12_adults_1864_years > 0 | f13_elderly_65_years > 0, TRUE, FALSE))

## rare variable ----

ff_nov23_trials$e13_dic <- ifelse(ff_nov23_trials$e13_condition_being_studied_is_a_rare_disease >= 0.5, TRUE, FALSE)

## age groups dichot variables ----

ff_nov23_trials$f111_in_utero_dic <- ifelse(ff_nov23_trials$f111_in_utero >= 0.5, TRUE, FALSE)
ff_nov23_trials$f112_preterm_newborn_infants_up_to_gestational_age__37_weeks_dic <- ifelse(ff_nov23_trials$f112_preterm_newborn_infants_up_to_gestational_age__37_weeks >= 0.5, TRUE, FALSE)
ff_nov23_trials$f113_newborns_027_days_dic <- ifelse(ff_nov23_trials$f113_newborns_027_days >= 0.5, TRUE, FALSE)
ff_nov23_trials$f114_infants_and_toddlers_28_days23_months_dic <- ifelse(ff_nov23_trials$f114_infants_and_toddlers_28_days23_months >= 0.5, TRUE, FALSE)
ff_nov23_trials$f115_children_211years_dic <- ifelse(ff_nov23_trials$f115_children_211years >= 0.5, TRUE, FALSE)
ff_nov23_trials$f116_adolescents_1217_years_dic <- ifelse(ff_nov23_trials$f116_adolescents_1217_years >= 0.5, TRUE, FALSE)
ff_nov23_trials$f12_adults_1864_years_dic <- ifelse(ff_nov23_trials$f12_adults_1864_years >= 0.5, TRUE, FALSE)
ff_nov23_trials$f13_elderly_65_years_dic <- ifelse(ff_nov23_trials$f13_elderly_65_years >= 0.5, TRUE, FALSE)




## phase variable ----

# initialize phase variable
ff_nov23_trials$phase <- NA

# loop over every row in the trials dataframe
for (i in 1:nrow(ff_nov23_trials)) {
  
  phase1 <- ff_nov23_trials$e71_human_pharmacology_phase_i[i]
  phase2 <- ff_nov23_trials$e72_therapeutic_exploratory_phase_ii[i]
  phase3 <- ff_nov23_trials$e73_therapeutic_confirmatory_phase_iii[i]
  phase4 <- ff_nov23_trials$e74_therapeutic_use_phase_iv[i]
  
  allphases <- c(phase1, phase2, phase3, phase4)
  
  # Use conditions from problem statement to assign phase
  if (phase1 == 1 & phase2 == 1) {
    ff_nov23_trials$phase[i] <- "Phase 1-2"
  } else if (phase2 == 1 & phase3 == 1) {
    ff_nov23_trials$phase[i] <- "Phase 2-3" 
  } else if (phase3 == 1 & phase4 == 1) {
    ff_nov23_trials$phase[i] <- "Phase 3-4" 
  } else if (all(allphases == 0)) {
    ff_nov23_trials$phase[i] <- "NA" 
  } else { 
    maxphase <- which.max(allphases)
    ff_nov23_trials$phase[i] <- paste0("Phase ", maxphase) 
  }
  
}


## therapeutic area variable ----

# Initialize variable in trial dataframe
ff_nov23_trials$e112_therapeutic_area <- NA

# Loop over trial dataframe
for (i in 1:nrow(ff_nov23_trials)) {
  
  # Get the EudraCT number for the current trial
  curr_eudract_num <- ff_nov23_trials$a2_eudract_number[i]
  
  # Lookup all the instances of this EudraCT number in the data dataframe
  matching_data <- ff_nov23_data$e112_therapeutic_area[ff_nov23_data$a2_eudract_number == curr_eudract_num]
  
  # Check for no match
  if(length(matching_data) == 0) {
    next #skip to the next iteration if there is no match
  }
  
  # Use the first match, if there are any
  ff_nov23_trials[i, "e112_therapeutic_area"] <- matching_data[1]
}

## authority decision variable ----

# Group by a2_eudract_number and calculate minimum n_date_of_competent_authority_decision
min_date_data <- ff_nov23_data %>%
  group_by(a2_eudract_number) %>%
  summarise(n_date_of_competent_authority_decision = min(n_date_of_competent_authority_decision, na.rm = TRUE))

# Join with ff_nov23_trials dataframe
ff_nov23_trials <- ff_nov23_trials %>%
  left_join(min_date_data, by = "a2_eudract_number")


## year variable ----


# Add 'year' variable
ff_nov23_data$year <- str_sub(ff_nov23_data$n_date_of_competent_authority_decision, 1, 4)
ff_nov23_data$year <- as.numeric(ff_nov23_data$year)

# Group by a2_eudract_number and calculate minimum year
min_year_data <- ff_nov23_data %>%
  group_by(a2_eudract_number) %>%
  summarise(year = min(year, na.rm = TRUE))

# Join with ff_nov23_trials dataframe
ff_nov23_trials <- ff_nov23_trials %>%
  left_join(min_year_data, by = "a2_eudract_number")


## date of end of trial variable ----

# Group by a2_eudract_number and calculate maximum p_date_of_the_global_end_of_the_trial
max_end_date_data <- ff_nov23_data %>%
  group_by(a2_eudract_number) %>%
  summarise(p_date_of_the_global_end_of_the_trial = max(p_date_of_the_global_end_of_the_trial, na.rm = TRUE))

# Join with ff_nov23_trials dataframe
ff_nov23_trials <- ff_nov23_trials %>%
  left_join(max_end_date_data, by = "a2_eudract_number")

# Add 'trial_duration' variable
ff_nov23_trials$trial_duration <- as.numeric(ff_nov23_trials$p_date_of_the_global_end_of_the_trial - ff_nov23_trials$n_date_of_competent_authority_decision)

# If value of trial duration -Inf > NA
ff_nov23_trials$trial_duration[ff_nov23_trials$trial_duration == -Inf] <- NA


## PIP variable ----

ff_nov23_trials$PIP <- sapply(ff_nov23_trials$a2_eudract_number, function(x){
  any(!is.na(ff_nov23_data$a8_ema_decision_number_of_paediatric_investigation_plan[ff_nov23_data$a2_eudract_number == x]))
})


## exclusion variables ----

# If f11_trial_has_subjects_under_18 is < 0.5, 'excl_f11' is set to TRUE. Otherwise, it's FALSE.
ff_nov23_trials <- ff_nov23_trials %>%
  mutate(excl_f11 = ifelse(f11_trial_has_subjects_under_18 < 0.5, TRUE, FALSE))

## If any f111, f112, f113, f114, f115, f116 variables are >= 0.5, set excl_f11_16 to FALSE. Otherwise, it's TRUE.
ff_nov23_trials <- ff_nov23_trials %>%
  mutate(excl_f11_16 = ifelse(f111_in_utero >= 0.5 | f112_preterm_newborn_infants_up_to_gestational_age__37_weeks >= 0.5 | 
                                f113_newborns_027_days >= 0.5 | f114_infants_and_toddlers_28_days23_months >= 0.5 | 
                                f115_children_211years >= 0.5 | f116_adolescents_1217_years >= 0.5, FALSE, TRUE))
## final exclusion variable


## If either excl_f11 or excl_f11_16 is TRUE then final_inclusion is FALSE. Otherwise, it's TRUE.
ff_nov23_trials <- ff_nov23_trials %>%
  mutate(final_inclusion = ifelse(excl_f11 | excl_f11_16, FALSE, TRUE))

# NETWORK VARIABLE ----


ff_nov23_network_long$a2_eudract_number <- ff_nov23_data$a2_eudract_number[match(ff_nov23_network_long$`_id`, ff_nov23_data$`_id`)]

ff_nov23_orgs_data <- subset(ff_nov23_network_long, name == "g4_investigator_networks.g41_name_of_organisation")

ff_nov23_orgs_data$new_name <- ff_nov23_match$new_name[match(ff_nov23_orgs_data$value, ff_nov23_match$g4_investigator_networks.g41_name_of_organisation)]

ff_nov23_orgs_data$network <- ifelse(ff_nov23_orgs_data$new_name %in% c("", "NA", "0", "Not a network"), FALSE, TRUE)

# create variable enprema. match ff_nov23_orgs_data$new_name and lookup ff_nov23_char$new_name
ff_nov23_orgs_data$enprema <- ff_nov23_char$enprEMA[match(ff_nov23_orgs_data$new_name, ff_nov23_char$new_name)]

# create variable ff_nov23_trials$enprema 
ff_nov23_trials$enprema <- sapply(ff_nov23_trials$a2_eudract_number, function(x){
  any(ff_nov23_orgs_data$enprema[ff_nov23_orgs_data$a2_eudract_number == x])
})

ff_nov23_trials$network <- sapply(ff_nov23_trials$a2_eudract_number, function(x){
  any(ff_nov23_orgs_data$network[ff_nov23_orgs_data$a2_eudract_number == x])
})


## g41_reported VARIABLE ----

ff_nov23_orgs_data$g41_reported <- ifelse(ff_nov23_orgs_data$new_name == "NA", FALSE, TRUE)

ff_nov23_trials$g41_reported <- sapply(ff_nov23_trials$a2_eudract_number, function(x) {
  any(ff_nov23_orgs_data$g41_reported[ff_nov23_orgs_data$a2_eudract_number == x])
})

# Fill NAs with FALSE (assuming that NA here indicates that the eudract number was not found in the orgs_data dataframe)
ff_nov23_trials$g41_reported[is.na(ff_nov23_trials$g41_reported)] <- FALSE



# NETWORK COUNTS ----

ff_nov23_orgs_data <- merge(ff_nov23_orgs_data, ff_nov23_trials[ , c('a2_eudract_number', 'final_inclusion')], by='a2_eudract_number')




# Data manipulation using dplyr
df <- ff_nov23_orgs_data %>%
  filter(final_inclusion == TRUE) %>%
  group_by(new_name) %>% 
  summarise(trials_involved = n_distinct(a2_eudract_number))

# merge df with ff_nov23_char using new_name
ff_nov23_char <- merge(ff_nov23_char, df, by = "new_name", all.x = TRUE)


# ENPREMA -----

# count rows in ff_nov23_trials where enprema == TRUE and final_inclusion == TRUE
ff_nov23_trials_enprema <- ff_nov23_trials %>%
  filter(enprema == TRUE & final_inclusion == TRUE) %>%
  group_by(enprema) %>%
  summarise(n = n_distinct(a2_eudract_number))





# SUMMARY TABLES ----


## table 1 ----

table_one <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>% 
  select(network, status, sponsor, multinational, multicentric, phase, subjects, e13_dic, n_protocols_EU, n_protocols, subjects_dic, f111_in_utero_dic, f112_preterm_newborn_infants_up_to_gestational_age__37_weeks_dic, f113_newborns_027_days_dic, f114_infants_and_toddlers_28_days23_months_dic, f115_children_211years_dic, f116_adolescents_1217_years_dic, f12_adults_1864_years_dic, f13_elderly_65_years_dic, g41_reported, PIP)

table_one_summary <- table_one %>% tbl_summary(
  by = network,
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c(
    "{N_nonmiss}",
    "{median} ({p25}, {p75})",
    "{min}, {max}",
    "{mean} ({sd})"
  ),
  missing = "no",
  label = list(
    status ~ "Status",
    phase ~ "Phase",
    sponsor ~ "Sponsor Type",
    subjects ~ "Enrollment",
    multinational ~ "Multinational",
    multicentric ~ "Multicentric",
    e13_dic ~ "Rare condition"
  )
)  %>% add_overall() %>% bold_labels() # %>% as_gt()  %>%
# gt::gtsave(filename = paste0("table_one_", Sys.Date(), ".docx")) 

## table 1 with all trials and exclusively pediatric trials

### table 1 but filtering out trials with adults ----

table_one_excl_adults <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>% 
  filter(adults == FALSE) %>%
  select(network, status, sponsor, multinational, multicentric, phase, subjects, e13_dic, n_protocols_EU, n_protocols, subjects_dic, f111_in_utero_dic, f112_preterm_newborn_infants_up_to_gestational_age__37_weeks_dic, f113_newborns_027_days_dic, f114_infants_and_toddlers_28_days23_months_dic, f115_children_211years_dic, f116_adolescents_1217_years_dic, f12_adults_1864_years_dic, f13_elderly_65_years_dic, g41_reported, PIP) 

table_one_excl_adults_summary <- table_one_excl_adults %>% tbl_summary(
  by = network,
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c(
    "{N_nonmiss}",
    "{median} ({p25}, {p75})",
    "{min}, {max}",
    "{mean} ({sd})"
  ),
  missing = "no",
  label = list(
    status ~ "Status",
    phase ~ "Phase",
    sponsor ~ "Sponsor Type",
    subjects ~ "Enrollment",
    multinational ~ "Multinational",
    multicentric ~ "Multicentric",
    e13_dic ~ "Rare condition"
  )
)  %>% add_overall() %>% bold_labels() # %>%   as_gt() %>%
# gt::gtsave(filename = paste0("table_one_excl_adults", Sys.Date(), ".docx")) 

## merge tables 1 and 1_excl_adults using tbl_merge() ----

# Merge gtsummary tables
table_one_mergd_summary <- tbl_merge(
  tbls = list(table_one_summary, table_one_excl_adults_summary),
  tab_spanner = c("**All trials**", "**Exclusively pediatric trials**")
) 
# Save table to Word
table_one_mergd_summary %>% as_gt() %>%
  gt::gtsave(filename = paste0("table_one_mergd_summary", Sys.Date(), ".docx"))




## table therapeutic area all trials ----

table_area <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>% 
  select(network, e112_therapeutic_area)

table_area_summary <- table_area %>% tbl_summary(
  by = network,
  type = all_continuous() ~ "continuous2",
  # statistic = list(
  #   all_continuous() ~ "{mean} ({sd})",
  #   all_categorical() ~ "{n} / {N} ({p}%)"
  # ),
  missing = "no",
  label = list(
    e112_therapeutic_area ~ "Therapeutic Area"),
  sort = all_categorical() ~ "frequency"
)  %>% add_overall() %>% bold_labels() # %>% as_gt() %>%
# gt::gtsave(filename = paste0("table_area_", Sys.Date(), ".docx"))


## table therapeutic area exclusively pediatric trials ----

table_area_excl_adults <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>% 
  filter(adults == FALSE) %>%
  select(network, e112_therapeutic_area)

table_area_excl_adults_summary <- table_area_excl_adults %>% tbl_summary(
  by = network,
  type = all_continuous() ~ "continuous2",
  # statistic = list(
  #   all_continuous() ~ "{mean} ({sd})",
  #   all_categorical() ~ "{n} / {N} ({p}%)"
  # ),
  missing = "no",
  label = list(
    e112_therapeutic_area ~ "Therapeutic Area"),
  sort = all_categorical() ~ "frequency"
) %>% add_overall() %>% bold_labels() # %>% as_gt() %>%
# gt::gtsave(filename = paste0("table_area_excl_adults_", Sys.Date(), ".docx"))

## merge tables therapeutic area all trials and exclusively pediatric trials using tbl_merge() ----

# Merge gtsummary tables
table_area_mergd <- tbl_merge(
  tbls = list(table_area_summary, table_area_excl_adults_summary),
  tab_spanner = c("**All trials**", "**Exclusively pediatric trials**")
)
# Save table to Word
table_area_mergd %>% as_gt() %>%
  gt::gtsave(filename = paste0("table_area_mergd_", Sys.Date(), ".docx"))





## table networks summary ----

table_networks <- ff_nov23_char %>%
  select(network, commercial_academic, national_international, pediatric_focus, disease_specific, established, scopus, trials_involved)

table_networks %>% tbl_summary(
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c(
    "{N_nonmiss}",
    "{median} ({p25}, {p75})",
    "{min}, {max}",
    "{mean} ({sd})"
  ),
  missing = "no",
  label = list(
  )
) %>% bold_labels() %>% as_gt() %>%
  gt::gtsave(filename = paste0("table_networks_", Sys.Date(), ".docx")) 

## table networks list ----

# Get data for networks and trials
networks <- ff_nov23_char %>%
  select(new_name, trials_involved) %>%
  arrange(desc(trials_involved))

# Load the necessary library
library(officer)
library(flextable)

# Generate flextable object
network_flextable <- flextable(networks)

# Create a document
doc <- read_docx() 

# Add the table to the document
doc <- body_add_flextable(doc, network_flextable)

# Save the document
print(doc, target = paste0("networks_list_", Sys.Date(), ".docx")) 




## summary table of trial duration by network ----

table_duration <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>%
  # filter out trials with adults
  filter(adults == FALSE) %>%
  select(network, trial_duration) %>%
  # clear negative values
  mutate(trial_duration = ifelse(trial_duration < 0, NA, trial_duration)) %>%
  # convert to months
  mutate(trial_duration = trial_duration / 30.437)


table_duration_summary <- table_duration %>% tbl_summary(
  by = network,
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c(
    "{N_nonmiss}",
    "{median} ({p25}, {p75})",
    "{min}, {max}",
    "{mean} ({sd})"
  ),
  missing = "no",
  # label = list(
  #   duration ~ "Trial duration (years)"),
  sort = all_categorical() ~ "frequency"
) %>% add_overall() %>% bold_labels() %>% as_gt() %>%
  gt::gtsave(filename = paste0("table_duration_", Sys.Date(), ".docx"))





# FIGURES ----

## time-series figure ----


# Filter data for years up to 2022
# ff_nov23_trials_filtered <- ff_nov23_trials %>% filter(year <= 2022)
ff_nov23_trials_filtered <- ff_nov23_trials %>% filter(year >= 2004, year <= 2022)


# Keep only final_inclusion is TRUE
ff_nov23_trials_filtered <- ff_nov23_trials_filtered %>% dplyr::filter(final_inclusion == TRUE)

# Calculate counts
summary_data <- ff_nov23_trials_filtered %>%
  group_by(year, network) %>%
  summarise(counts = n())

# Replace "TRUE" and "FALSE" with "Sim" and "Não"
summary_data$network <- ifelse(summary_data$network == TRUE, "Sim", "Não")

# Create a stacked histogram
ggplot(summary_data, aes(x=year, y=counts, fill=network)) + 
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x= "Ano de autorização", y = "") +
  scale_fill_manual(values = c("gray", "black")) +
  guides(fill = guide_legend(title = "Envolvimento de redes"))+
  scale_y_continuous(breaks = seq(0, 750, by = 50), expand = expansion(mult = c(0, 0.1)))

# Save figure to vector file
ggsave(filename = paste0("figure_time_series_", Sys.Date(), ".svg"), width = 6, height = 3.5, units = "in")


# duration figures ----

# Plot histogram of trial duration with two colours (network and non-network)

table_duration <- ff_nov23_trials %>%
  filter(final_inclusion == TRUE) %>%
  # filter out trials with adults
  filter(adults == FALSE) %>%
  select(network, trial_duration) %>%
  # clear negative values
  mutate(trial_duration = ifelse(trial_duration < 0, NA, trial_duration)) %>%
  # convert to months
  mutate(trial_duration = trial_duration / 30.437)

ggplot(table_duration, aes(x = trial_duration, fill = network)) +
  geom_histogram(bins = 100) +
  labs(x = "Trial duration (months)", y = "Count", title = "Exclusively pediatric trials") +
  scale_fill_discrete(name = "Network", labels = c("No", "Yes"))





# SAVE DATA TO CSV SHEETS ----

# write csv for ff_nov23_data and ff_nov23_trials with today date in file name
readr::write_csv(ff_nov23_data, paste0("tfm-FF-data_", Sys.Date(), ".csv"))
readr::write_csv(ff_nov23_trials, paste0("tfm-FF-trials_", Sys.Date(), ".csv"))



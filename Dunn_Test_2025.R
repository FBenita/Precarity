# --- FULL ANALYSIS SCRIPT: TESTING 'COMMUNITY ACTIVITIES' VARIABLE ---

# --- 0. Setup ---
rm(list = ls())
graphics.off()

# --- 1. Load All Necessary Libraries ---
library(readxl)
library(tidyverse)
library(readr)
library(dunn.test) 
library(lme4)      
library(broom.mixed) 
library(broom)     

# --- PART I: LOAD AND CLEAN ALL RAW DATA ---

# --- a) Load and Clean COVID-19 Time-Series Data ---
df_covid_raw <- read_excel("C:/Users/L03565094/Dropbox/Francisco/Papers_SUTD/Sharing_Economy/Simulation/Platform/Covid/Code2021/Long2/Covid_Jakarta_and_population.xlsx", sheet = "Cases_Mar2020_to_Jun2021")
pop <- read_excel("C:/Users/L03565094/Dropbox/Francisco/Papers_SUTD/Sharing_Economy/Simulation/Platform/Covid/Code2021/Long2/Covid_Jakarta_and_population.xlsx", sheet = "population")

df_weekly <- df_covid_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% as.Date(c("2020-03-28", "2020-04-04", "2020-04-11", "2020-04-18", "2020-04-25", "2020-05-02", "2020-05-09", "2020-05-16", "2020-05-23", "2020-05-30", "2020-06-06", "2020-06-13", "2020-06-20", "2020-06-27", "2020-07-04", "2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01", "2020-08-08", "2020-08-15", "2020-08-22", "2020-08-29", "2020-09-05", "2020-09-12", "2020-09-19", "2020-09-26", "2020-10-03", "2020-10-10", "2020-10-17", "2020-10-24", "2020-10-31", "2020-11-07", "2020-11-14", "2020-11-21", "2020-11-28", "2020-12-05", "2020-12-12", "2020-12-19", "2020-12-26", "2021-01-02", "2021-01-09", "2021-01-16", "2021-01-23", "2021-01-30", "2021-02-06", "2021-02-13", "2021-02-20", "2021-02-27", "2021-03-06", "2021-03-13", "2021-03-20", "2021-03-27", "2021-04-03", "2021-04-10", "2021-04-17", "2021-04-24")))

df_covid_cleaned <- df_weekly %>%
  group_by(ID_KEL) %>%
  arrange(date) %>%
  mutate(positive_lag = lag(positive, n = 1, default = NA)) %>%
  mutate(diff_positive = positive - positive_lag) %>%
  left_join(select(pop, ID_KEL, total_pop), by = "ID_KEL") %>%
  mutate(positive_new_per_week_per_1000 = diff_positive / (total_pop / 1000)) %>%
  ungroup()

# --- b) Load and Prepare Raw Survey Data ---
survey_data <- read_csv("C:/Users/L03565094/Dropbox/Francisco/Papers_SUTD/Sharing_Economy/Simulation/Platform/Covid/Datasets/Jakarta_survey_covid_paper_raw_data.csv")

survey_prepared <- survey_data %>%
  rename(district = area) %>%
  mutate(
    district = factor(district),
    
    # <<< UPDATED SOCIALIZATION BELLWETHER >>>
    socialization_bellwether = case_when(
      `Q14_Do_you_participate_in_community_activities?` == "Tidak" ~ 0,
      `Q14_Do_you_participate_in_community_activities?` == "Ya" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Infrastructure and Mobility bellwethers remain the same
    infrastructure_bellwether = case_when(
      `Q36_4_The_neighbourhood_has_good_quality_health_care_facilities` == "Tidak Setuju" ~ 2, `Q36_4_The_neighbourhood_has_good_quality_health_care_facilities` == "Kadang-Kadang" ~ 3, `Q36_4_The_neighbourhood_has_good_quality_health_care_facilities` == "Setuju" ~ 4, `Q36_4_The_neighbourhood_has_good_quality_health_care_facilities` == "Sangat Setuju" ~ 5, TRUE ~ NA_real_),
    mobility_bellwether = case_when(
      `Q20_Time_to_reach_my_workplace` == "1 -14 menit" ~ 1, `Q20_Time_to_reach_my_workplace` == "15 -30 menit" ~ 2, `Q20_Time_to_reach_my_workplace` == "31 – 60 menit" ~ 3, `Q20_Time_to_reach_my_workplace` == "1-2 jam" ~ 4, `Q20_Time_to_reach_my_workplace` == "Lebih dari 2 jam" ~ 5, TRUE ~ NA_real_)
  )

output_directory <- "C:/Users/L03565094/Dropbox/Francisco/Papers_SUTD/Sharing_Economy/Simulation/Platform/Covid/Results2025/"
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

bellwether_questions <- c("socialization_bellwether", "infrastructure_bellwether", "mobility_bellwether")
for (question in bellwether_questions) {
  
  # Print header for clarity
  cat("\n\n--- Analysis for:", question, "---\n")
  
  # Run and print the Kruskal-Wallis test
  kruskal_result <- kruskal.test(as.formula(paste(question, "~ district")), data = survey_prepared)
  print(kruskal_result)
  
  # Run and print the Dunn's test
  dunn_result <- dunn.test::dunn.test(x = survey_prepared[[question]], g = survey_prepared$district, method = "bonferroni")
  print(dunn_result)
  
  # (Code to export CSVs is omitted here for brevity but is in your full script)
}

# --- PART III: TIME-SERIES ANALYSIS (LMM) ---
district_scores <- survey_prepared %>%
  group_by(district) %>%
  summarise(
    socialization_score = mean(socialization_bellwether, na.rm = TRUE),
    infrastructure_score = mean(infrastructure_bellwether, na.rm = TRUE),
    mobility_score = mean(mobility_bellwether, na.rm = TRUE)
  )

model_data <- df_covid_cleaned %>%
  mutate(
    district_name_survey = case_when(
      nama_kecamatan == "CENGKARENG" ~ "Cengkareng",
      nama_kelurahan %in% c("BENDUNGAN HILIR", "PETAMBURAN") ~ "Benhil",
      nama_kelurahan %in% c("RAWA BADAK SELATAN", "TUGU SELATAN", "KELAPA GADING BARAT") ~ "Tanah Merah",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(district_name_survey), !is.na(positive_new_per_week_per_1000)) %>%
  mutate(week = as.integer(format(date, "%W"))) %>%
  left_join(district_scores, by = c("district_name_survey" = "district"))

if(nrow(model_data) > 0) {
  # Run separate models for each dimension
  
  cat("\n\n--- MODEL 1: SOCIALIZATION ---\n")
  model_socialization <- lmer(positive_new_per_week_per_1000 ~ socialization_score + (1 | week), data = model_data)
  print(summary(model_socialization))
  
  cat("\n\n--- MODEL 2: INFRASTRUCTURE ---\n")
  model_infrastructure <- lmer(positive_new_per_week_per_1000 ~ infrastructure_score + (1 | week), data = model_data)
  print(summary(model_infrastructure))
  
  cat("\n\n--- MODEL 3: MOBILITY ---\n")
  model_mobility <- lmer(positive_new_per_week_per_1000 ~ mobility_score + (1 | week), data = model_data)
  print(summary(model_mobility))
  
} else {
  print("Error: The 'model_data' dataframe is still empty.")
}


#The name of the areas in the column "nama_kelurahan" as follows:
  
#  if nama_kelurahan="CENGKARENG BARAT" then the area is "Cengkareng"
#if nama_kelurahan="RAWA BADAK SELATAN" or nama_kelurahan="TUGU SELATAN" 
#or nama_kelurahan="KELAPA GADING BARAT" then the area is "Tanah Merah".LAstly 
#if nama_kelurahan="PETAMBURAN" or nama_kelurahan="BENDUNGAN HILIR" then the area is "Benhil"

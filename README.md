# Pipeline-Assignment_NFS1218
Group project: Building an A-Z Reproducible Pipeline for Precision Nutrition Analysis

# Exploring Missingness & Data Cleaning for Growth Dataset.R

'''
packages <- c("tidyverse", "naniar", "skimr", "ggplot2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
'''

# 2  Data loading 
'''
data <- read_csv("~/Desktop/NFS 2026/NFS1218/Data Analysis Pipeline/mock_precision_growth_dataset(updated).csv")
'''
# Inspect structure
glimpse(data)
summary(data)

# 3  Missingness exploration 

# 3.1 Percent missing per variable

missing_summary <- data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary)

# 3.2 Visual inspection of missing data
vis_miss(data)

# Missingness combinations
gg_miss_upset(data)

###Add the Visual Table from Queenie, all the variables were missing at random, variables of interest for our research question were not missing###

# 4  Distribution inspection 

# 4.1 zBMI distribution

if ("WHO_zBMI_birth" %in% names(data)) {
  
  ggplot(data, aes(x = WHO_zBMI_birth)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of zBMI at birth")
}
### WHO_zBMI_birth was normally distributed and this histrogram points out any potential outliers that we will investigate in the next step###

# 4.2 maternal BMI distribution

if ("Maternal_BMI" %in% names(data)) {
  
  ggplot(data, aes(x = Maternal_BMI)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal BMI")
}
### Maternal_BMI was normally distributed and this histogram points out any potential outliers that we will investigate in the next step###

# 4.3 Maternal Fasting glucose distribution

if ("Fasting_glucose" %in% names(data)) {
  
  ggplot(data, aes(x = Fasting_glucose)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal fasting glucose")
}
###Fasting_glucose was normally distributed and this histogram points out any potential outliers that we will investigate in the next step###

# 5  Identifying implausible values 

# 5.1 Implausible gestational age values

if ("Gestational_age_weeks" %in% names(data)) {
  
  implausible_gest_age <- data %>%
    filter(Gestational_age_weeks < 22 | Gestational_age_weeks > 44)
  
  print(implausible_gest_age)
}
### Cut-offs were selected based off of criteria and the results indicated 1 participant as an outlier/implausible value (participant ID 96 with a gestational age of 44.8)###

# 5.2 Implausible maternal BMI values

if ("Maternal_BMI" %in% names(data)) {
  
  implausible_mBMI <- data %>%
    filter(Maternal_BMI < 15 | Maternal_BMI > 40)
  
  print(implausible_mBMI)
}
###cut-offs were selected based off of criteria and the results indicated 6 participants as an outlier/implausible value (participant ID 58 = 14.5 kg/m^2; participant ID 65 = 41.4 kg/m^2; participant ID 131 = 13.6 kg/m^2; participant ID 162 = 14.9 kg/m^2; participant ID 233 = 12.5 kg/m^2; participant ID 255 = 12.7 kg/m^2###

# 5.3 Implausible maternal fasting glucose values

if ("Fasting_glucose" %in% names(data)) {
  
  implausible_fglucose <- data %>%
    filter(Fasting_glucose < 2.5 | Fasting_glucose > 12)
  
  print(implausible_fglucose)
}
###cut-offs were selected based off of criteria and the results indicated zero potential participants as outliers/implausible values###

# 5.4 WHO-style plausibility cut-offs for zBMI at birth

# WHO commonly flags z-scores < -5 or > +5 as implausible

if ("WHO_zBMI_birth" %in% names(data)) {
  
  implausible_atbirth <- data %>%
    filter(WHO_zBMI_birth < -5 | WHO_zBMI_birth > 5)
  
  print(implausible_atbirth)
}
###cut-offs were selected based off of criteria and the results indicated 12 participants as an outlier/implausible value (participnat ID 2 = -6.4; participant ID 13 = -5.59; participnat ID 33 = -5.2; participant ID 42 = -5.4; participant ID 112 = -5.6; participant ID 140 = +5.3; participant ID 142 = -5,4; participant ID 200 = -5.1; participant ID 226 = -6.1; participant ID 249 = -6.5; participant ID 278 = -6.5; participant ID 298 = -6.1###

# 6  Data cleaning 

clean_data <- data

# 6.1 Remove implausible gestational age
if ("Gestational_age_weeks" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Gestational_age_weeks >= 22 & Gestational_age_weeks <= 44)
}
###from this a total of 1 participant was removed from the dataset, new sample size is N=299 ###

# 6.2 Remove implausible maternal BMI
if ("Maternal_BMI" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Maternal_BMI >= 15 & Maternal_BMI <= 40)
}
###from this a total of 6 participants were removed from the dataset, new sample size is N=293###

# 6.3 Remove implausible zBMI at birth

if ("WHO_zBMI_birth" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(WHO_zBMI_birth >= -5 & WHO_zBMI_birth <= 5)
}
###from this a total of 12 participants were removed from the dataset, new sample size is N=281###
###since in section 5.3 there were no potential outliers/implausible values for Fasting_glucose we did not have to remove any implausible values, therefore the cleaned dataset sample size is N = 281####

# 7  Post-cleaning diagnostics  

# Compare sample size
cat("Original N:", nrow(data), "\n")
cat("Cleaned N:", nrow(clean_data), "\n")

# Recalculate missingness after cleaning
missing_summary_clean <- clean_data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary_clean)

###ADD Table###
# Save cleaned dataset
write_csv(clean_data, "clean_precision_growth_dataset_03.22.csv")

# Exploring Missingness & Data Cleaning for Growth Dataset.R

packages <- c("tidyverse", "naniar", "skimr", "ggplot2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =    2  Data loading  =========================================================

data <- read_csv("~/Desktop/NFS 2026/NFS1218/Data Analysis Pipeline/mock_precision_growth_dataset(updated).csv")

# Inspect structure
glimpse(data)
summary(data)

# =====   3  Missingness exploration  ===========================================

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


# ======    4  Distribution inspection  =========================================

# 4.1 zBMI distribution

if ("WHO_zBMI_birth" %in% names(data)) {
  
  ggplot(data, aes(x = WHO_zBMI_birth)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of zBMI at birth")
}

# maternal BMI distribution

if ("Maternal_BMI" %in% names(data)) {
  
  ggplot(data, aes(x = Maternal_BMI)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal BMI")
}

# Maternal Fasting glucose distribution

if ("Fasting_glucose" %in% names(data)) {
  
  ggplot(data, aes(x = Fasting_glucose)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal fasting glucose")
}

# =====    5  Identifying implausible values  ===================================

# 5.1 Implausible gestational age values

if ("Gestational_age_weeks" %in% names(data)) {
  
  implausible_gest_age <- data %>%
    filter(Gestational_age_weeks < 22 | Gestational_age_weeks > 44)
  
  print(implausible_gest_age)
}

# 5.2 Implausible maternal BMI values

if ("Maternal_BMI" %in% names(data)) {
  
  implausible_mBMI <- data %>%
    filter(Maternal_BMI < 15 | Maternal_BMI > 40)
  
  print(implausible_mBMI)
}

# 5.2 Implausible maternal fasting glucose values

if ("Fasting_glucose" %in% names(data)) {
  
  implausible_fglucose <- data %>%
    filter(Fasting_glucose < 2.5 | Fasting_glucose > 12)
  
  print(implausible_fglucose)
}

# 5.3 WHO-style plausibility cut-offs for zBMI


# WHO commonly flags z-scores < -5 or > +5 as implausible

if ("WHO_zBMI_birth" %in% names(data)) {
  
  implausible_atbirth <- data %>%
    filter(WHO_zBMI_birth < -5 | WHO_zBMI_birth > 5)
  
  print(implausible_atbirth)
}


# =====    6  Data cleaning  ===================================================

clean_data <- data

# Remove implausible gestational age
if ("Gestational_age_weeks" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Gestational_age_weeks >= 22 & Gestational_age_weeks <= 44)
}

# Remove implausible maternal BMI
if ("Maternal_BMI" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Maternal_BMI >= 15 & Maternal_BMI <= 40)
}

# Remove implausible zBMI

if ("WHO_zBMI_birth" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(WHO_zBMI_birth >= -5 & WHO_zBMI_birth <= 5)
}


# =====    7  Post-cleaning diagnostics  =======================================

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

# Save cleaned dataset
write_csv(clean_data, "clean_precision_growth_dataset_03.22.csv")

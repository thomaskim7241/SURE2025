library(tidyverse)

dataset_full <- read.csv("analytic_data2025_v2.csv", header = FALSE)
dataset_clean <- read.csv("analytic_data2025_v2.csv", skip = 1)
colnames(dataset_clean) <- dataset_full[1,]

dataset_clean <- dataset_clean |>
  janitor::clean_names()

data_ADD <- dataset_clean |>
  mutate(logit_ADD = log((alcohol_impaired_driving_deaths_raw_value + 0.001) / (1 - alcohol_impaired_driving_deaths_raw_value + 0.001)))

#huber model 
more_variables <- MASS::rlm(
  logit_ADD ~ percent_rural_raw_value + unemployment_raw_value + severe_housing_cost_burden_raw_value + percent_65_and_older_raw_value +
    percent_female_raw_value + percent_american_indian_or_alaska_native_raw_value + percent_disability_functional_limitations_raw_value +
    median_household_income_raw_value + high_school_completion_raw_value + primary_care_physicians_raw_value + 
    uninsured_children_raw_value + school_funding_adequacy_raw_value + broadband_access_raw_value +
    library_access_raw_value +
    mental_health_providers_raw_value + percent_non_hispanic_white_raw_value,
  data = data_ADD
)

summary(more_variables)
partial_residual <- crPlots(more_variables)
plot(partial_residual)
vif(more_variables)

#running on dataset_clean with drug od
dataset_clean$drug_overdose_deaths_raw_value
drug_od_huber <- MASS::rlm(
  drug_overdose_deaths_raw_value ~ percent_rural_raw_value + unemployment_raw_value + severe_housing_cost_burden_raw_value + percent_65_and_older_raw_value +
    percent_female_raw_value + percent_american_indian_or_alaska_native_raw_value + percent_disability_functional_limitations_raw_value +
    median_household_income_raw_value + high_school_completion_raw_value + primary_care_physicians_raw_value + 
    uninsured_children_raw_value + school_funding_adequacy_raw_value + broadband_access_raw_value +
    library_access_raw_value +
    mental_health_providers_raw_value + percent_non_hispanic_white_raw_value,
  data = dataset_clean
)
summary(drug_od_huber)

#checking drug od to see if the numbers are skewed
summary(dataset_clean$drug_overdose_deaths_raw_value)
hist(dataset_clean$drug_overdose_deaths_raw_value, breaks = 30)

#log function applied to drug od
dataset_scaled$log_drug_od <- log(dataset_clean$drug_overdose_deaths_raw_value)

drug_od_huber_scaled<- MASS::rlm(
  log_drug_od ~ percent_rural_raw_value + unemployment_raw_value + severe_housing_cost_burden_raw_value + percent_65_and_older_raw_value +
    percent_female_raw_value + percent_american_indian_or_alaska_native_raw_value + percent_disability_functional_limitations_raw_value +
    median_household_income_raw_value + high_school_completion_raw_value + primary_care_physicians_raw_value + 
    uninsured_children_raw_value + school_funding_adequacy_raw_value + broadband_access_raw_value +
    library_access_raw_value +
    mental_health_providers_raw_value + percent_non_hispanic_white_raw_value,
  data = dataset_scaled
)
summary(drug_od_huber_scaled)



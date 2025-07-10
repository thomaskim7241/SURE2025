library(tidyverse)
library(factoextra)
library(ggplot2)
library(ggmosaic)
library(maps)
library(scales)
library(biscale)
library(cowplot)
library(usmap)
library(GGally)
library(car)
library(broom)
library(dplyr)
setwd("~/Documents/Rstudios/SURE 2025/SURE2025/Kim")
df <- read.csv("analytic_data2025_v2.csv")
df_state <- df %>%
  filter(County.FIPS.Code == "000" & 
           State.FIPS.Code != "00" & 
           Name != "District of Columbia") %>% 
  select(Name, 
         X..Rural.raw.value, 
         Unemployment.raw.value,
         X..Below.18.Years.of.Age.raw.value, 
         Severe.Housing.Problems.raw.value,
         Alcohol.Impaired.Driving.Deaths.raw.value,
         Drug.Overdose.Deaths.raw.value) %>% 
  rename(
    State = Name,
    Rural = X..Rural.raw.value,
    Unemployment = Unemployment.raw.value,
    Youth = X..Below.18.Years.of.Age.raw.value,
    HousingProblems = Severe.Housing.Problems.raw.value,
    ADD =  Alcohol.Impaired.Driving.Deaths.raw.value,
    Drug_OD = Drug.Overdose.Deaths.raw.value
  ) %>%
  mutate(across(c(Rural, Unemployment, Youth, HousingProblems, ADD,
                  Drug_OD), as.numeric),
         State = as.factor(State))

view(df_state)

df_state %>% 
  select(-State) %>% 
  ggpairs()

# Apply logit transformation with small adjustment to avoid 0 or 1
df_state <- df_state %>%
  mutate(
    logit_ADD = logit(ADD, adjust = 0.001),
    logit_Drug_OD = logit(Drug_OD, adjust = 0.001)
  )

# Build models using logit-transformed outcomes
model_logit_ADD <- lm(logit_ADD ~ Rural + Unemployment + Youth + HousingProblems, data = df_state)
summary(model_logit_ADD)

model_logit_Drug_OD <- lm(logit_Drug_OD ~ Rural + Unemployment + Youth + HousingProblems, data = df_state)
summary(model_logit_Drug_OD)

# Predict and back-transform to rates using inverse logit
df_state <- df_state %>%
  mutate(
    Pred_logit_ADD = plogis(predict(model_logit_ADD)),
    Pred_logit_Drug_OD = plogis(predict(model_logit_Drug_OD))
  )


# Plot Actual vs. Predicted for Alcohol Death Rate (ADD)
ggplot(df_state, aes(x = Pred_logit_ADD, y = ADD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Actual vs. Predicted: Alcohol Death Rates (Logit Model)",
       x = "Predicted Alcohol Death Rate",
       y = "Actual Alcohol Death Rate") +
  theme_minimal()

# Plot Actual vs. Predicted for Drug Overdose Death Rate (Drug_OD)
ggplot(df_state, aes(x = Pred_logit_Drug_OD, y = Drug_OD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Actual vs. Predicted: Drug Overdose Rates (Logit Model)",
       x = "Predicted Drug Overdose Rate",
       y = "Actual Drug Overdose Rate") +
  theme_minimal()

# Tidy models for coefficient plotting
coefs_logit_ADD <- tidy(model_logit_ADD) %>% mutate(Model = "Alcohol Deaths (Logit)")
coefs_logit_Drug <- tidy(model_logit_Drug_OD) %>% mutate(Model = "Drug Overdose Deaths (Logit)")

# Combine coefficient tables
coefs_all_logit <- bind_rows(coefs_logit_ADD, coefs_logit_Drug)

# Plot Coefficient Estimates for Both Models
ggplot(coefs_all_logit %>% filter(term != "(Intercept)"), 
       aes(x = estimate, y = reorder(term, estimate), color = Model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Comparison of Predictors for Alcohol vs. Drug Deaths (Logit Models)",
       x = "Coefficient Estimate (Logit Scale)",
       y = "Predictor") +
  theme_minimal()

# Residual Plots for Model Diagnostics
plot(model_logit_ADD, which = 1, main = "Residuals vs Fitted: Alcohol Deaths (Logit Model)")
plot(model_logit_Drug_OD, which = 1, main = "Residuals vs Fitted: Drug Overdose (Logit Model)")

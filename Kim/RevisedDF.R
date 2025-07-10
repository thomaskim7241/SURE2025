library(tidyverse)
library(factoextra)
library(ggplot2)
library(ggmosaic)
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

# Apply logit transformation only to ADD (with small adjustment to avoid 0 or 1)
df_state <- df_state %>%
  mutate(
    logit_ADD = logit(ADD, adjust = 0.001)
  )

# Build models
model_logit_ADD <- lm(logit_ADD ~ Rural + Unemployment + Youth + HousingProblems, data = df_state)
summary(model_logit_ADD)

model_Drug_OD <- lm(Drug_OD ~ Rural + Unemployment + Youth + HousingProblems, data = df_state)
summary(model_Drug_OD)

# Predict values
df_state <- df_state %>%
  mutate(
    Pred_logit_ADD = plogis(predict(model_logit_ADD)),       # Back-transform ADD predictions
    Pred_Drug_OD = predict(model_Drug_OD)                    # Keep Drug_OD as is
  )

# Plot Actual vs. Predicted for Alcohol Death Rate (ADD - Logit Model)
ggplot(df_state, aes(x = Pred_logit_ADD, y = ADD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Actual vs. Predicted: Alcohol Death Rates (Logit Model)",
       x = "Predicted Alcohol Death Rate",
       y = "Actual Alcohol Death Rate") +
  theme_minimal()

# Plot Actual vs. Predicted for Drug Overdose Deaths (No Logit)
ggplot(df_state, aes(x = Pred_Drug_OD, y = Drug_OD)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Actual vs. Predicted: Drug Overdose Deaths",
       x = "Predicted Drug Overdose Deaths",
       y = "Actual Drug Overdose Deaths") +
  theme_minimal()

# Tidy models for coefficient plotting
coefs_logit_ADD <- tidy(model_logit_ADD) %>% mutate(Model = "Alcohol Deaths (Logit)")
coefs_Drug <- tidy(model_Drug_OD) %>% mutate(Model = "Drug Overdose Deaths")

# Combine coefficient tables
coefs_all <- bind_rows(coefs_logit_ADD, coefs_Drug)

# Plot Coefficients for Both Models
ggplot(coefs_all %>% filter(term != "(Intercept)"), 
       aes(x = estimate, y = reorder(term, estimate), color = Model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Comparison of Predictors for Alcohol vs. Drug Deaths",
       x = "Coefficient Estimate",
       y = "Predictor") +
  theme_minimal()

# Residual Plot for ADD (Logit Model)
plot(model_logit_ADD, which = 1, main = "Residuals vs Fitted: Alcohol Deaths (Logit Model)")

# Residual Plot for Drug OD (Linear Model)
plot(model_Drug_OD, which = 1, main = "Residuals vs Fitted: Drug Overdose Deaths")

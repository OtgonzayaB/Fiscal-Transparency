# Loading libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(car)
library(lubridate)

# Import data
regression_full <- read_excel("full-da-crises.xlsx")
no_crisis <- read_excel("no-crisis-da.xlsx")

# Model 1
model_1 <- lm(RP_change ~ OBI + Credit_Rating + Macro_var, data = regression_full)
summary(model_1)
vif(model_1)

# Model 2
regression_full <- regression_full %>%
  mutate(FX_group = case_when(
    FX_regime %in% c(1, 2) ~ "Flexible",
    FX_regime %in% c(3, 4, 5, 8, 6, 7) ~ "Managed"))


regression_full$FX_group <- factor(regression_full$FX_group)
regression_full$FX_group <- relevel(factor(regression_full$FX_group), ref = "Flexible")

model_2<- lm(RP_change ~ OBI + Credit_Rating + Macro_var + KAOPEN, data = regression_full) 
summary(model_2)
vif(model_2)

# Model 3
model_3<- lm(RP_change ~ OBI + Credit_Rating + Macro_var + FX_group + KAOPEN, data = regression_full) 
summary(model_3)
vif(model_3)

# AIC,BIC test
AIC(model_1,model_2,model_3)
BIC(model_1,model_2,model_3)

# Robustness checks
## R1 - Year

model_with_year <- lm(RP_change ~  OBI + Credit_Rating + Macro_var + FX_group + KAOPEN + factor(year),
                      data = regression_full)
summary(model_with_year)
vif(model_with_year)

## R2 - Region
model_with_region <- lm(RP_change ~ OBI + Credit_Rating + Macro_var + FX_group + KAOPEN + factor(region),
                        data = regression_full)
summary(model_with_region)
vif(model_with_region)


## R3 - Risk Premium Peak Delay
model_delay <- lm(RP_peak_delay ~ OBI + Credit_Rating + Macro_var+ FX_group + KAOPEN,
                  data = regression_full)
summary(model_delay) 
vif(model_delay)


## R4 - Crises Year OBI
model_crises_obi<- lm(RP_change ~ crisis_year_obi + Credit_Rating + Macro_var + FX_group + KAOPEN, data = regression_full) 
summary(model_crises_obi)
vif(model_crises_obi)


## R5 - Quadratic model

# Center the OBI variable
regression_full$OBI_c <- with(regression_full, OBI - mean(OBI))

model_qd<- lm(RP_change ~ OBI + I(OBI_c^2) + Credit_Rating + Macro_var + FX_group + KAOPEN, data = regression_full) 
summary(model_qd)
vif(model_qd)


## R6 - Stable times
no_crisis <- no_crisis %>%
  mutate(FX_group = case_when(
    FX_regime %in% c(1, 2) ~ "Flexible",
    FX_regime %in% c(3, 4, 5, 8, 6, 7) ~ "Managed"
  ))

# Treat as factor
no_crisis$FX_group <- factor(no_crisis$FX_group)
no_crisis$FX_group <- relevel(factor(no_crisis$FX_group), ref = "Flexible")


model_no_crisis <- lm(RP_level ~ OBI + Credit_Rating + Macro_var + FX_group + KAOPEN, data = no_crisis) 
summary(model_no_crisis) 
vif(model_no_crisis) 


## R7 - FXgroup and OBI interaction term

regression_full <- regression_full %>%
  mutate(OBI_group = ifelse(OBI > 50, "High_OBI", "Low_OBI"),
         OBI_group = factor(OBI_group))

regression_full$FX_group <- factor(regression_full$FX_group)
regression_full$FX_group <- relevel(regression_full$FX_group, ref = "Flexible")


model_int <- lm(RP_change ~ OBI + OBI_group + FX_group + OBI_group:FX_group + Credit_Rating + Macro_var + KAOPEN,
                  data = regression_full)

summary(model_int)
vif(model_int)












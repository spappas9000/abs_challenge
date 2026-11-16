library(lme4)
library(car)
library(caret)
library(pROC)
library(MLmetrics)
library(splines)
library(DHARMa)
library(tidyverse)
source("data.R")

## NEW MODELS
# First: P(X|y_t = 0 & y_call = 1, w)
# mod3_good_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod3_good_challenge_hitter, "./models/mod3_good_challenge_hitter.RDS")
mod3 <- read_rds("./models/mod3_good_challenge_hitter.RDS")

summary(mod3)

## 1. Try some interaction effects with challenges_remaining
# I tried bat_score_diff*challenges_remaining which was significant, and miss_dist*challenges_remaining which was not.
## 2. Add random effect for hitting team
# Surprisingly had ~0 variance and caused singularity

# Looking at the spline:
newdata <- data.frame(
  delta = 0,
  bat_score_diff = 0,
  challenges_remaining = 1,
  miss_dist = seq(0, 0.35, 0.01)
)

pred <- predict(mod3, newdata, re.form = NA, type = "response")

plot(newdata$miss_dist, pred)

# Testing for overdispersion using DHARMa
testDispersion(mod3)
# Dispersion = 1.0443, p < 2.2*e^[-16]
set.seed(1989)
residuals_sim <- simulateResiduals(fittedModel = mod3, plot = TRUE)
plotResiduals(residuals_sim, quantreg = TRUE)

# mod4_bad_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# Try this with the interaction btwn bat_score_diff*challenges_remaining
# saveRDS(mod4_bad_challenge_hitter, "./models/mod4_bad_challenge_hitter.RDS")

mod4 <- readRDS("./models/mod4_bad_challenge_hitter.RDS")

# Umpire_hp is a zero-variance term for this model
vif(mod4)
summary(mod4)

newdata <- data.frame(
  delta = 0,
  bat_score_diff = 0,
  challenges_remaining = 1,
  miss_dist = seq(-0.8, 0.35, 0.01)
)

pred <- predict(mod4, newdata, re.form = NA, type = "response")

plot(newdata$miss_dist, pred)

testDispersion(mod4)
# Dispersion = 1.0539, p-value = 0.016
set.seed(1989)
residuals_sim2 <- simulateResiduals(fittedModel = mod4, plot = TRUE)
plotResiduals(residuals_sim2, quantreg = TRUE)

## Catcher Models
# mod5_good_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(zone %in% c(1:9)),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod5_good_challenge_catcher, "./models/mod5_good_challenge_catcher.RDS")

mod5 <- readRDS("./models/mod5_good_challenge_catcher.RDS")

vif(mod5)
summary(mod5)

testDispersion(mod5)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim3 <- simulateResiduals(fittedModel = mod5, plot = TRUE)
plotResiduals(residuals_sim3, quantreg = TRUE)

# mod6_bad_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(zone > 9),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod6_bad_challenge_catcher, "./models/mod6_bad_challenge_catcher.RDS")

mod6 <- readRDS("./models/mod6_bad_challenge_catcher.RDS")
# Look practically at what these models mean
VarCorr(mod6)

vif(mod6)
summary(mod6)

testDispersion(mod6)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim4 <- simulateResiduals(fittedModel = mod6, plot = TRUE)
plotResiduals(residuals_sim4, quantreg = TRUE)

## Do a model where we take the challenge model and add an indicator for truth for hitters and catchers

# Seeing which of these two matters more
# Hitters
mod_sal <- glm(challenge_hitter ~ scale(log(salary)), family = "binomial",
               data = modeldata_hitter)
mod_serv <- glm(challenge_hitter ~ scale(service_time), family = "binomial",
                data = modeldata_hitter)

AIC(mod_sal, mod_serv)
# salary has AIC 28757.36, service has 28745.25

# Catchers
mod_sal2 <- glm(challenge_catcher ~ scale(log(salary)), family = "binomial",
                data = modeldata_catcher)
mod_serv2 <- glm(challenge_catcher ~ scale(service_time), family = "binomial",
                 data = modeldata_catcher)
AIC(mod_sal2, mod_serv2)
# salary has AIC 35639.3, service has 35639.38

# Now with our full models
# mod_salmer <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_salmer, "./models/mod7_good_hitter_salary.RDS")
mod7 <- read_rds("./models/mod7_good_hitter_salary.RDS")

# mod_servmer <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_servmer, "./models/mod8_good_hitter_serv.RDS")
mod8 <- read_rds("./models/mod8_good_hitter_serv.RDS")

AIC(mod7, mod8)
# Salary AIC 8075.346, Service time AIC 8073.707
BIC(mod7, mod8)
# Salary BIC 8215.516, Service time BIC 8213.878

# Bad challenge hitter
# mod_salmer_bad <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_salmer_bad, "./models/mod9_bad_hitter_salary.RDS")
mod9 <- read_rds("./models/mod9_bad_hitter_salary.RDS")

# mod_servmer_bad <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_servmer_bad, "./models/mod10_bad_hitter_serv.RDS")
mod10 <- read_rds("./models/mod10_bad_hitter_serv.RDS")

AIC(mod9, mod10)
BIC(mod9, mod10)
# Salary AIC of 13232.09, Service time AIC of 13228.33
# Salary BIC of 13403.41, service time BIC of 13399.65

# Catcher models
# mod_salmer_catch <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_salmer_catch, "./models/mod11_good_catcher_salary.RDS")
mod11 <- read_rds("./models/mod11_good_catcher_salary.RDS")

# mod_servmer_catch <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_servmer_catch, "./models/mod12_good_catcher_serv.RDS")
mod12 <- read_rds("./models/mod12_good_catcher_serv.RDS")

AIC(mod11, mod12)
BIC(mod11, mod12)
# Salary AIC: 9120.787; Service AIC: 9118.648
# Salary BIC: 9250.138; Service BIC: 9247.998

# mod_salmer_catch_bad <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_salmer_catch_bad, "./models/mod13_bad_catcher_salary.RDS")
mod13 <- read_rds("./models/mod13_bad_catcher_salary.RDS")

# mod_servmer_catch_bad <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod_servmer_catch_bad, "./models/mod14_bad_catcher_serv.RDS")
mod14 <- read_rds("./models/mod14_bad_catcher_serv.RDS")

AIC(mod13, mod14)
BIC(mod13, mod14)
# Salary AIC: 11786.41; Service AIC: 11789.11
# Salary BIC: 11968.59; Service BIC: 11971.29

# Comparing against base models
# Good challenge hitter
AIC(mod3, mod7, mod8)
BIC(mod3, mod7, mod8)
# Service time model for good challenge hitter is best across both AIC and BIC

# Bad challenge hitter
AIC(mod4, mod9, mod10)
BIC(mod4, mod9, mod10)
# Service time model best AIC, base model best BIC

# Good challenge catcher
AIC(mod5, mod11, mod12)
BIC(mod5, mod11, mod12)
# Service time model for good challenge catcher has best AIC, base model best BIC

# Bad challenge catcher
AIC(mod6, mod13, mod14)
BIC(mod6, mod13, mod14)
# Salary model for bad challenge catcher has best AIC, base model best BIC

# WAR models:
# 2 additional models for each outcome:
# Total WAR and Offensive WAR (up to 2026) for hitting models
# Total WAR and Defensive WAR (up to 2026) for catching models

# Good challenge hitter
mod3_good_challenge_hitter <- glmer(
  challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
    (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
  data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
)
saveRDS(mod3_good_challenge_hitter, "./models/mod3_good_challenge_hitter.RDS")
mod3 <- read_rds("./models/mod3_good_challenge_hitter.RDS")
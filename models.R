library(lme4)
library(car)
library(caret)
library(pROC)
library(MLmetrics)
library(splines)
library(DHARMa)
library(tidyverse)
source("data.R")

# Re-running Steven's models with our new variables (for paper)
# mod1_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# mod2_challenge_hitter_sal <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# mod3_challenge_hitter_serv <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# mod4_challenge_hitter_cwar <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(cumwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# mod5_challenge_hitter_bwar <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(bwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# mod6_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     (1|fielder_2) + (1|pitcher) + (1|batter) + (1|umpire_hp),
#   data = modeldata_catcher,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# 
# mod7_challenge_catcher_sal <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(log(salary)) + (1|fielder_2) + (1|pitcher) + (1|batter) + (1|umpire_hp),
#   data = modeldata_catcher,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# 
# mod8_challenge_catcher_serv <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(service_time) + (1|fielder_2) + (1|pitcher) + (1|batter) + (1|umpire_hp),
#   data = modeldata_catcher,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# 
# mod9_challenge_catcher_war <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(cumwar_career) + (1|fielder_2) + (1|pitcher) + (1|batter) + (1|umpire_hp),
#   data = modeldata_catcher,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# 
# mod10_challenge_catcher_fwar <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta +scale(bat_score_diff)*challenges_remaining +
#     scale(fwar_career) + (1|fielder_2) + (1|pitcher) + (1|batter) + (1|umpire_hp),
#   data = modeldata_catcher,
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )

## NEW MODELS

# ------------- Good Challenge - Hitter ----------------------------------------

# First: P(X|y_t = 0 & y_call = 1, w)
# mod11_good_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod11_good_challenge_hitter, "./models/mod11_good_challenge_hitter.RDS")
mod11_good_challenge_hitter <- read_rds("./models/mod11_good_challenge_hitter.RDS")

# Looking at the spline:
# newdata <- data.frame(
#   delta = 0,
#   bat_score_diff = 0,
#   challenges_remaining = 1,
#   miss_dist = seq(0, 0.35, 0.01)
# )
# 
# pred <- predict(mod11_good_challenge_hitter, newdata, re.form = NA, type = "response")
# 
# plot(newdata$miss_dist, pred)

# mod12_good_challenge_hitter_sal <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod12_good_challenge_hitter_sal, "./models/mod12_good_challenge_hitter_sal.RDS")
mod12_good_challenge_hitter_sal <- read_rds("./models/mod12_good_challenge_hitter_sal.RDS")

# mod13_good_challenge_hitter_serv <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), 
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod13_good_challenge_hitter_serv, "./models/mod13_good_challenge_hitter_serv.RDS")
mod13_good_challenge_hitter_serv <- read_rds("./models/mod13_good_challenge_hitter_serv.RDS")

# mod14_good_challenge_hitter_war <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(cumwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), 
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod14_good_challenge_hitter_war, "./models/mod14_good_challenge_hitter_war.RDS")
mod14_good_challenge_hitter_war <- read_rds("./models/mod14_good_challenge_hitter_war.RDS")

# mod15_good_challenge_hitter_bwar <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(bwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), 
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod15_good_challenge_hitter_bwar, "./models/mod15_good_challenge_hitter_bwar.RDS")
mod15_good_challenge_hitter_bwar <- read_rds("./models/mod15_good_challenge_hitter_bwar.RDS")

AIC(mod11_good_challenge_hitter, mod12_good_challenge_hitter_sal, mod13_good_challenge_hitter_serv,
    mod14_good_challenge_hitter_war, mod15_good_challenge_hitter_bwar)

BIC(mod11_good_challenge_hitter, mod12_good_challenge_hitter_sal, mod13_good_challenge_hitter_serv,
    mod14_good_challenge_hitter_war, mod15_good_challenge_hitter_bwar)

# ------------- Bad Challenge - Hitter -----------------------------------------

# mod16_bad_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod16_bad_challenge_hitter, "./models/mod16_bad_challenge_hitter.RDS")
mod16_bad_challenge_hitter <- read_rds("./models/mod16_bad_challenge_hitter.RDS")

# Plot the spline
# newdata <- data.frame(
#   delta = 0,
#   bat_score_diff = 0,
#   challenges_remaining = 1,
#   miss_dist = seq(-0.8, 0.35, 0.01)
# )
# 
# pred <- predict(mod4, newdata, re.form = NA, type = "response")
# 
# plot(newdata$miss_dist, pred)

# mod17_bad_challenge_hitter_sal <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod17_bad_challenge_hitter_sal, "./models/mod17_bad_challenge_hitter_sal.RDS")
mod17_bad_challenge_hitter_sal <- read_rds("./models/mod17_bad_challenge_hitter_sal.RDS")

# mod18_bad_challenge_hitter_serv <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod18_bad_challenge_hitter_serv, "./models/mod18_bad_challenge_hitter_serv.RDS")
mod18_bad_challenge_hitter_serv <- read_rds("./models/mod18_bad_challenge_hitter_serv.RDS")

# mod19_bad_challenge_hitter_war <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(cumwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod19_bad_challenge_hitter_war, "./models/mod19_bad_challenge_hitter_war.RDS")
mod19_bad_challenge_hitter_war <- read_rds("./models/mod19_bad_challenge_hitter_war.RDS")

# mod20_bad_challenge_hitter_bwar <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     + scale(bwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod20_bad_challenge_hitter_bwar, "./models/mod20_bad_challenge_hitter_bwar.RDS")
mod20_bad_challenge_hitter_bwar <- read_rds("./models/mod20_bad_challenge_hitter_bwar.RDS")

AIC(mod16_bad_challenge_hitter, mod17_bad_challenge_hitter_sal, mod18_bad_challenge_hitter_serv,
    mod19_bad_challenge_hitter_war, mod20_bad_challenge_hitter_bwar)

BIC(mod16_bad_challenge_hitter, mod17_bad_challenge_hitter_sal, mod18_bad_challenge_hitter_serv,
    mod19_bad_challenge_hitter_war, mod20_bad_challenge_hitter_bwar)

# ------------- Good Challenge - Catcher ---------------------------------------

# mod21_good_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(zone %in% c(1:9)),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod21_good_challenge_catcher, "./models/mod21_good_challenge_catcher.RDS")
mod21_good_challenge_catcher <- read_rds("./models/mod21_good_challenge_catcher.RDS")

# mod22_good_challenge_catcher_sal <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod22_good_challenge_catcher_sal, "./models/mod22_good_challenge_catcher_sal.RDS")
mod22_good_challenge_catcher_sal <- read_rds("./models/mod22_good_challenge_catcher_sal.RDS")

# mod23_good_challenge_catcher_serv <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod23_good_challenge_catcher_serv, "./models/mod23_good_challenge_catcher_serv.RDS")
mod23_good_challenge_catcher_serv <- read_rds("./models/mod23_good_challenge_catcher_serv.RDS")

# mod24_good_challenge_catcher_war <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(cumwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod24_good_challenge_catcher_war, "./models/mod24_good_challenge_catcher_war.RDS")
mod24_good_challenge_catcher_war <- read_rds("./models/mod24_good_challenge_catcher_war.RDS")

# mod25_good_challenge_catcher_fwar <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(fwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod25_good_challenge_catcher_fwar, "./models/mod25_good_challenge_catcher_fwar.RDS")
mod25_good_challenge_catcher_fwar <- read_rds("./models/mod25_good_challenge_catcher_fwar.RDS")

AIC(mod21_good_challenge_catcher, mod22_good_challenge_catcher_sal, mod23_good_challenge_catcher_serv,
    mod24_good_challenge_catcher_war, mod25_good_challenge_catcher_fwar)

BIC(mod21_good_challenge_catcher, mod22_good_challenge_catcher_sal, mod23_good_challenge_catcher_serv,
    mod24_good_challenge_catcher_war, mod25_good_challenge_catcher_fwar)

# ------------- Bad Challenge - Catcher ----------------------------------------

# mod26_bad_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(zone > 9),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod26_bad_challenge_catcher, "./models/mod26_bad_challenge_catcher.RDS")
mod26_bad_challenge_catcher <- read_rds("./models/mod26_bad_challenge_catcher.RDS")

# mod27_bad_challenge_catcher_sal <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(log(salary)) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod27_bad_challenge_catcher_sal, "./models/mod27_bad_challenge_catcher_sal.RDS")
mod27_bad_challenge_catcher_sal <- read_rds("./models/mod27_bad_challenge_catcher_sal.RDS")

# mod28_bad_challenge_catcher_serv <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining + 
#     scale(service_time) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod28_bad_challenge_catcher_serv, "./models/mod28_bad_challenge_catcher_serv.RDS")
mod28_bad_challenge_catcher_serv <- read_rds("./models/mod28_bad_challenge_catcher_serv.RDS")

# mod29_bad_challenge_catcher_war <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(cumwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod29_bad_challenge_catcher_war, "./models/mod29_bad_challenge_catcher_war.RDS")
mod29_bad_challenge_catcher_war <- read_rds("./models/mod29_bad_challenge_catcher_war.RDS")

# mod30_bad_challenge_catcher_fwar <- glmer(
#   challenge_catcher ~ miss_dist + miss_dir + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(fwar_career) + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(miss_dist > 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod30_bad_challenge_catcher_fwar, "./models/mod30_bad_challenge_catcher_fwar.RDS")
mod30_bad_challenge_catcher_fwar <- read_rds("./models/mod30_bad_challenge_catcher_fwar.RDS")

AIC(mod26_bad_challenge_catcher, mod27_bad_challenge_catcher_sal, mod28_bad_challenge_catcher_serv,
    mod29_bad_challenge_catcher_war, mod30_bad_challenge_catcher_fwar)

BIC(mod26_bad_challenge_catcher, mod27_bad_challenge_catcher_sal, mod28_bad_challenge_catcher_serv,
    mod29_bad_challenge_catcher_war, mod30_bad_challenge_catcher_fwar)

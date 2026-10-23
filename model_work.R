library(lme4)
library(car)
library(caret)
library(pROC)
library(MLmetrics)
library(splines)
library(DHARMa)
source("data.R")

## ---- Hitting Models ----

# Predicting the probability that a hitter challenges by the plate location
mod0_call_change_hitter <- glm(
  challenge_hitter ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj,
  data = modeldata_hitter,
  family = "binomial"
)

summary(mod0_call_change_hitter)

set.seed(1989)

k_folds <- 5

folds <- createFolds(modeldata_hitter$challenge_hitter, k = k_folds, list = TRUE, returnTrain = FALSE)

glm_results_hitter <- tibble()

for (p in 1:25) {
  
  for (i in 1:k_folds) {
    
    test_indices <- folds[[i]]
    
    train <- modeldata_hitter[-test_indices, ]
    test  <- modeldata_hitter[test_indices, ]
    
    z_terms <- paste0("ns(plate_z, ", 1:p, ")", collapse = " + ")
    x_terms <- paste0("ns(plate_x, ", 1:p, ")", collapse = " + ")
    
    model_formula <- as.formula(
      paste(
        "challenge_hitter ~",
        z_terms, "+",
        x_terms, "+",
        "delta + inning + bat_score_diff + challenges_remaining"
      )
    )
    
    mod <- glm(
      model_formula,
      data = train,
      family = "binomial"
    )
    
    pred <- predict(
      mod,
      newdata = test,
      type = "response"
    )
    
    fold_auc <- as.numeric(
      auc(test$challenge_hitter, pred)
    )
    
    glm_results_hitter <- bind_rows(
      glm_results_hitter,
      tibble(
        fold = i,
        polynomial_order = p,
        modeltype = paste0("Polynomial", p),
        auc = fold_auc,
        coef = list(
          enframe(
            coef(mod),
            name = "var",
            value = "estimate"
          )
        )
      )
    )
  }
}

glm_results_hitter %>% group_by(modeltype) %>% reframe(auc = mean(auc)) %>% arrange(desc(auc))

mean(glm_results_hitter$auc)

print(paste("Area under the curve for each glm fold:", paste(round(glm_results_hitter$auc, 3), collapse = ", ")))

mod1_call_change_hitter_red <- glm(
  challenge_hitter ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj + delta + inning + bat_score_diff + challenges_remaining +
    I(plate_z_adj^3) + I(plate_x_adj^3),
  data = modeldata_hitter,
  family = "binomial"
)

mod1_call_change_hitter <- glm(
  challenge_hitter ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining,
  data = modeldata_hitter,
  family = "binomial"
)

anova(mod1_call_change_hitter_red, mod1_call_change_hitter)

summary(mod1_call_change_hitter)

mod2_call_change_hitter <- glmer(
  challenge_hitter ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining +
                     (1|batter) + (1|fielder_2) + (1|bat_team) + (1|def_team) + (1|umpire_hp),
  data = modeldata_hitter,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

vif(mod2_call_change_hitter)

summary(mod2_call_change_hitter)

hitter_re <- ranef(mod2_call_change_hitter, condVar = TRUE)

hitter_re_id <- hitter_re$batter

hitter_post_var <- attr(hitter_re_id, "postVar")

hitter_re_exploration <- data.frame(
  batter = as.numeric(rownames(hitter_re_id)),
  estimate = hitter_re_id[, "(Intercept)"],
  se = sqrt(hitter_post_var[1, 1, ])
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  filter(upper < 0 & lower < 0 | upper > 0 & lower > 0) %>%
  left_join(playerid, by = c("batter" = "key_mlbam"))

AIC(mod1_call_change_hitter, mod2_call_change_hitter)

null.id <- -2 * logLik(mod1_call_change_hitter) + 2 * logLik(mod2_call_change_hitter)

pchisq(as.numeric(null.id), df = 1, lower.tail = FALSE)

## ---- Catching Models ----

mod0_call_change_catcher <- glm(
  challenge_catcher ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj,
  data = modeldata_catcher,
  family = "binomial"
)

summary(mod0_call_change_catcher)

mod1_call_change_catcher_red <- glm(
  challenge_catcher ~ I(plate_z_adj^3) + I(plate_x_adj^3) + I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj + delta + inning + 
    bat_score_diff + challenges_remaining + I(plate_z_adj^3) + I(plate_x_adj^3),
  data = modeldata_catcher,
  family = "binomial"
)

mod1_call_change_catcher <- glm(
  challenge_catcher ~ plate_z_adj + plate_x_adj + delta + bat_score_diff + inning * challenges_remaining + I(plate_z_adj^2) + 
    I(plate_x_adj^2) + I(plate_z_adj^3) + I(plate_x_adj^3) + I(plate_z_adj^4) + I(plate_x_adj^4),
  data = modeldata_catcher,
  family = "binomial"
)

anova(mod1_call_change_catcher_red, mod1_call_change_catcher)

summary(mod1_call_change_catcher)

set.seed(1989)

k_folds <- 5

folds <- createFolds(modeldata_catcher$challenge_catcher, k = k_folds, list = TRUE, returnTrain = FALSE)

glm_results <- tibble()

for (p in 1:6) {
  
  for (i in 1:k_folds) {
    
    test_indices <- folds[[i]]
    
    train <- modeldata_catcher[-test_indices, ]
    test  <- modeldata_catcher[test_indices, ]
    
    z_terms <- paste0("ns(plate_z_adj, ", 1:p, ")", collapse = " + ")
    x_terms <- paste0("ns(plate_x_adj, ", 1:p, ")", collapse = " + ")
    
    model_formula <- as.formula(
      paste(
        "challenge_catcher ~",
        z_terms, "+",
        x_terms, "+",
        "delta + inning + bat_score_diff + challenges_remaining"
      )
    )
    
    mod <- glm(
      model_formula,
      data = train,
      family = "binomial"
    )
    
    pred <- predict(
      mod,
      newdata = test,
      type = "response"
    )
    
    fold_auc <- as.numeric(
      auc(test$challenge_catcher, pred)
    )
    
    glm_results <- bind_rows(
      glm_results,
      tibble(
        fold = i,
        polynomial_order = p,
        modeltype = paste0("Polynomial", p),
        auc = fold_auc,
        coef = list(
          enframe(
            coef(mod),
            name = "var",
            value = "estimate"
          )
        )
      )
    )
  }
}

glm_results %>% group_by(modeltype) %>% reframe(auc = mean(auc)) %>% arrange(desc(auc))

print(paste("Area under the curve for each glm fold:", paste(round(glm_results$auc, 3), collapse = ", ")))

p <- fitted(mod1_call_change_catcher)

summary(p)

challengeprob <- predict(mod1_call_change_catcher, newdata = modeldata_catcher, type = "response")

cbind(modeldata_catcher, challengeprob) %>%
  view()

modeldata_catcher %>%
  mutate(
    challengeprob = predict(mod1_call_change_catcher, type = "response")
  ) %>%
  view()

mod2_call_change_catcher <- glmer(
  challenge_catcher ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining +
                      (1|fielder_2) + (1|batter) + (1|bat_team) + (1|def_team) + (1|umpire_hp),
  data = modeldata_catcher,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

catcherpred <- predict(mod2_call_change_catcher, newdata = modeldata_catcher, type = "response")

catcherfinalset <- cbind(modeldata_catcher, catcherpred)

summary(mod2_call_change_catcher)

catcher_re <- ranef(mod2_call_change_catcher, condVar = TRUE)

catcher_re_id <- catcher_re$fielder_2

catcher_post_var <- attr(catcher_re_id, "postVar")

catcher_re_exploration <- data.frame(
  catcher = as.numeric(rownames(catcher_re_id)),
  estimate = catcher_re_id[, "(Intercept)"],
  se = sqrt(catcher_post_var[1, 1, ])
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  filter(upper < 0 & lower < 0 | upper > 0 & lower > 0) %>%
  left_join(playerid, by = c("catcher" = "key_mlbam"))

## NEW MODELS
# First: P(X|y_t = 0 & y_call = 1, w)
# mod3_good_challenge_hitter <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff)*challenges_remaining +
#     (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod3_good_challenge_hitter, "mod3_good_challenge_hitter.RDS")
mod3_good_challenge_hitter <- read_rds("mod3_good_challenge_hitter.RDS")

summary(mod3_good_challenge_hitter)

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

pred <- predict(mod3_good_challenge_hitter, newdata, re.form = NA, type = "response")

plot(newdata$miss_dist, pred)

# Testing for overdispersion using DHARMa
testDispersion(mod3_good_challenge_hitter)
# Dispersion = 1.0443, p < 2.2*e^[-16]
set.seed(1989)
residuals_sim <- simulateResiduals(fittedModel = mod3_good_challenge_hitter, plot = TRUE)
plotResiduals(residuals_sim, quantreg = TRUE)

good_hitter_re <- ranef(mod3_good_challenge_hitter, condVar = TRUE)

good_hitter_re_id <- good_hitter_re$batter

good_hitter_post_var <- attr(good_hitter_re_id, "postVar")

good_hitter_re_exploration <- data.frame(
  batter = as.numeric(rownames(good_hitter_re_id)),
  estimate = good_hitter_re_id[, "(Intercept)"],
  se = sqrt(good_hitter_post_var[1, 1, ])
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  # filter(upper < 0 & lower < 0 | upper > 0 & lower > 0) %>%
  left_join(playerid, by = c("batter" = "key_mlbam"))

## SALARY AND SERVICE YEARS MODELS
# mod3_good_challenge_hitter_salary <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff)*challenges_remaining +
#     scale(salary) + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod3_good_challenge_hitter_salary, "mod3_good_challenge_hitter_salary.RDS")
mod3_good_challenge_hitter_salary <- read_rds("mod3_good_challenge_hitter_salary.RDS")

summary(mod3_good_challenge_hitter_salary)

sd(modeldata_hitter$salary)

# mod3_good_challenge_hitter_service <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff)*challenges_remaining +
#     service_years + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_hitter |> filter(miss_dist > 0), # Now that we have miss_dist, we don't need to use zone to filter
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod3_good_challenge_hitter_service, "mod3_good_challenge_hitter_service.RDS")
mod3_good_challenge_hitter_service <- read_rds("mod3_good_challenge_hitter_service.RDS")

summary(mod3_good_challenge_hitter_service)

sd(modeldata_hitter$service_years)

## Based on these models, I think service time looks a lot better as a predictor.
## eda_work 300:330 show a similar story

mod4_bad_challenge_hitter <- glmer(
  challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff) +
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
  data = modeldata_hitter |> filter(miss_dist <= 0),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# saveRDS(mod4_bad_challenge_hitter, "mod4_bad_challenge_hitter.RDS")

mod4_bad_challenge_hitter <- readRDS("mod4_bad_challenge_hitter.RDS")

# Umpire_hp is a zero-variance term for this model
vif(mod4_bad_challenge_hitter)
summary(mod4_bad_challenge_hitter2)

# I am less likely to challenge if I have more challenges
# Can we find the challenges when players have them to burn?

# mod4_bad_challenge_hitter_salary <- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff) +
#     scale(salary) + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod4_bad_challenge_hitter_salary, "mod4_bad_challenge_hitter_salary.RDS")
mod4_bad_challenge_hitter_salary <- read_rds("mod4_bad_challenge_hitter_salary.RDS")

summary(mod4_bad_challenge_hitter_salary)
# Salary is insignificant

# mod4_bad_challenge_hitter_service<- glmer(
#   challenge_hitter ~ ns(miss_dist, 2) + delta + scale(bat_score_diff) +
#     service_years + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_hitter |> filter(miss_dist <= 0),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod4_bad_challenge_hitter_service, "mod4_bad_challenge_hitter_service.RDS")
mod4_bad_challenge_hitter_service <- read_rds("mod4_bad_challenge_hitter_service.RDS")

summary(mod4_bad_challenge_hitter_service)
# Service significant

newdata <- data.frame(
  delta = 0,
  bat_score_diff = 0,
  challenges_remaining = 1,
  miss_dist = seq(-0.8, 0.35, 0.01)
)

pred <- predict(mod4_bad_challenge_hitter, newdata, re.form = NA, type = "response")

plot(newdata$miss_dist, pred)

testDispersion(mod4_bad_challenge_hitter)
# Dispersion = 1.0539, p-value = 0.016
set.seed(1989)
residuals_sim2 <- simulateResiduals(fittedModel = mod4_bad_challenge_hitter, plot = TRUE)
plotResiduals(residuals_sim2, quantreg = TRUE)

## Catcher Models
# mod5_good_challenge_catcher <- glmer(
#   challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
#     challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(zone %in% c(1:9)),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod5_good_challenge_catcher, "mod5_good_challenge_catcher.RDS")

mod5_good_challenge_catcher <- readRDS("mod5_good_challenge_catcher.RDS")

vif(mod5_good_challenge_catcher)
summary(mod5_good_challenge_catcher)

newdata <- data.frame(
  delta = 0,
  bat_score_diff = 0,
  challenges_remaining = 1,
  miss_dist = seq(0, 0.75, 0.01)
)

pred <- predict(mod5_good_challenge_catcher, newdata, re.form = NA, type = "response")

plot(newdata$miss_dist, pred)

testDispersion(mod5_good_challenge_catcher)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim3 <- simulateResiduals(fittedModel = mod5_good_challenge_catcher, plot = TRUE)
plotResiduals(residuals_sim3, quantreg = TRUE)

# mod5_good_challenge_catcher_salary <- glmer(
#   challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
#     scale(salary) + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(zone %in% c(1:9)),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod5_good_challenge_catcher_salary, "mod5_good_challenge_catcher_salary.RDS")

mod5_good_challenge_catcher_salary <- read_rds("mod5_good_challenge_catcher_salary.RDS")

summary(mod5_good_challenge_catcher_salary)
# Salary is insignificant

# mod5_good_challenge_catcher_service <- glmer(
#   challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
#     service_years + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
#   data = modeldata_catcher |> filter(zone %in% c(1:9)),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod5_good_challenge_catcher_service, "mod5_good_challenge_catcher_service.RDS")

mod5_good_challenge_catcher_service <- read_rds("mod5_good_challenge_catcher_service.RDS")

summary(mod5_good_challenge_catcher_service)
# Service time is also insignificant here

mod6_bad_challenge_catcher <- glmer(
  challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
  data = modeldata_catcher |> filter(zone > 9),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# saveRDS(mod6_bad_challenge_catcher, "mod6_bad_challenge_catcher.RDS")

mod6_bad_challenge_catcher <- readRDS("mod6_bad_challenge_catcher.RDS")
VarCorr(mod6_bad_challenge_catcher)

vif(mod6_bad_challenge_catcher)
summary(mod6_bad_challenge_catcher)

testDispersion(mod6_bad_challenge_catcher)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim4 <- simulateResiduals(fittedModel = mod6_bad_challenge_catcher, plot = TRUE)
plotResiduals(residuals_sim4, quantreg = TRUE)

# mod6_bad_challenge_catcher_salary <- glmer(
#   challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
#     scale(salary) + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(zone > 9),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod6_bad_challenge_catcher_salary, "mod6_bad_challenge_catcher_salary.RDS")

mod6_bad_challenge_catcher_salary <- read_rds("mod6_bad_challenge_catcher_salary.RDS")

summary(mod6_bad_challenge_catcher_salary)
# Salary is significant

# mod6_bad_challenge_catcher_service <- glmer(
#   challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) +
#     service_years + challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
#   data = modeldata_catcher |> filter(zone > 9),
#   family = "binomial",
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )
# saveRDS(mod6_bad_challenge_catcher_service, "mod6_bad_challenge_catcher_service.RDS")

mod6_bad_challenge_catcher_service <- read_rds("mod6_bad_challenge_catcher_service.RDS")

summary(mod6_bad_challenge_catcher_service)
# Service time is insignificant

# Looking at whether or not we need the splines at the degree they're at
mod3_data <- modeldata_hitter |> filter(zone > 9)

mod3_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_hitter)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod3_fits <- list(
  linear = glm(challenge_hitter ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod3_data)
)
for (k in 2:6) {
  mod3_fits[[paste0("ns", k)]] <- update(mod3_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

# To go with the simplest model possible, BIC gives a better idea of that
sapply(mod3_fits, BIC)

#ns2 for mod3

mod4_data <- modeldata_hitter |> filter(miss_dist <= 0)

mod4_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_hitter)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod4_fits <- list(
  linear = glm(challenge_hitter ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod4_data)
)
for (k in 2:6) {
  mod4_fits[[paste0("ns", k)]] <- update(mod4_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod4_fits, BIC)
# ns2 again

# Catcher models
mod5_data <- modeldata_catcher |> filter(zone %in% c(1:9))

mod5_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_catcher)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod5_fits <- list(
  linear = glm(challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod5_data)
)
for (k in 2:6) {
  mod5_fits[[paste0("ns", k)]] <- update(mod5_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod5_fits, BIC) # The linear term wins out here.

mod6_data <- modeldata_catcher |> filter(zone > 9)

mod6_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_catcher)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod6_fits <- list(
  linear = glm(challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod6_data)
)
for (k in 2:6) {
  mod6_fits[[paste0("ns", k)]] <- update(mod6_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod6_fits, BIC) # The linear term wins out here as well.


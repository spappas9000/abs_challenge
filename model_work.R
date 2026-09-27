library(lme4)
library(httr)
library(jsonlite)
library(httr2)
library(car)
library(caret)
library(pROC)
library(MLmetrics)
library(splines)
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
mod3_good_challenge_hitter <- glmer(
  challenge_hitter ~ ns(miss_dist, 2) + delta + bat_score_diff + 
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
  data = modeldata_hitter |> filter(zone > 9),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
)

# Look at what the spline looks like
# Had to remove plate location, because we want to filter the data down to pitches 
# that weren't actually a strike --> derivative of x-z location

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

vif(mod3_good_challenge_hitter)

summary(mod3_good_challenge_hitter)

# Testing for overdispersion using DHARMa
testDispersion(mod3_good_challenge_hitter)
# Dispersion = 1.0443, p < 2.2*e^[-16]
set.seed(1989)
residuals_sim <- simulateResiduals(fittedModel = mod3_good_challenge_hitter, plot = TRUE)

plotResiduals(residuals_sim, quantreg = TRUE)
plotQQunif(residuals_sim)
confint.merMod(mod3_good_challenge_hitter, level = 0.95, devmatchtol = 1e-4)

isSingular(mod3_good_challenge_hitter)

mod4_bad_challenge_hitter <- glmer(
  challenge_hitter ~ ns(miss_dist, 2) + delta + bat_score_diff + 
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
  data = modeldata_hitter |> filter(miss_dist <= 0),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# Umpire_hp is a zero-variance term for this model
vif(mod4_bad_challenge_hitter)
summary(mod4_bad_challenge_hitter)

testDispersion(mod4_bad_challenge_hitter)
# Dispersion = 1.0539, p-value = 0.016
set.seed(1989)
residuals_sim2 <- simulateResiduals(fittedModel = mod4_bad_challenge_hitter, plot = TRUE)
plotResiduals(residuals_sim2, quantreg = TRUE)
plotQQunif(residuals_sim2)

## Catcher Models
mod5_good_challenge_catcher <- glmer(
  challenge_catcher ~ ns(miss_dist, 2) + delta + bat_score_diff + 
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2) + (1|umpire_hp),
  data = modeldata_catcher |> filter(zone %in% c(1:9)),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

vif(mod5_good_challenge_catcher)
summary(mod5_good_challenge_catcher)

testDispersion(mod5_good_challenge_catcher)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim3 <- simulateResiduals(fittedModel = mod5_good_challenge_catcher, plot = TRUE)
plotResiduals(residuals_sim3, quantreg = TRUE)
plotQQunif(residuals_sim3)

mod6_bad_challenge_catcher <- glmer(
  challenge_catcher ~ ns(miss_dist, 2) + delta + bat_score_diff + 
    challenges_remaining + (1|batter) + (1|pitcher) + (1|fielder_2),
  data = modeldata_catcher |> filter(zone > 9),
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# This model needed to have bobyqa's maxfun expanded to 2e5 instead of the default

VarCorr(mod6_bad_challenge_catcher)

vif(mod6_bad_challenge_catcher)
summary(mod6_bad_challenge_catcher)

testDispersion(mod6_bad_challenge_catcher)
# Dispersion = 1.012, p-value = 0.472
set.seed(1989)
residuals_sim4 <- simulateResiduals(fittedModel = mod6_bad_challenge_catcher, plot = TRUE)
plotResiduals(residuals_sim4, quantreg = TRUE)
plotQQunif(residuals_sim4)
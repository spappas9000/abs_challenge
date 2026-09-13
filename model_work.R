library(lme4)
library(tidyverse)
library(baseballr)
library(httr)
library(jsonlite)
library(tidyverse)
library(httr2)
library(car)
library(caret)
library(pROC)
library(MLmetrics)

modeldata <- rbind(challenge, statcastlist_26, fill = T) %>%
  filter(description %in% c("blocked_ball", "ball", "called_strike", "pitchout"),
         !des %in% c("Cedric Mullins called out on strikes. Cedric Mullins to 1st. Passed ball by catcher Mickey Gasper.",
                    "Jacob Young called out on strikes. Jacob Young to 1st. Passed ball by catcher Gabriel Moreno."),
         grepl("catcher interference", des) == F,
         plate_x <= 1.666 & plate_x >= -1.666,
         plate_z <= 4.5 & plate_z >= 0.5) %>%
  distinct(game_pk, at_bat_number, pitch_number, .keep_all = T) %>%
  mutate(
    count = paste0(balls, "-", strikes),
    challenge = ifelse(is.na(challenge), 0, challenge),
    challenge_hitter = ifelse(is.na(challenge_hitter), 0, challenge_hitter),
    challenge_catcher = ifelse(is.na(challenge_catcher), 0, challenge_catcher),
    call_change_hitter = ifelse(is.na(call_change_hitter), 0, call_change_hitter),
    call_change_catcher = ifelse(is.na(call_change_catcher), 0, call_change_catcher),
    plate_z_adj = plate_z - ((sz_top - sz_bot)/2 + sz_bot),
    plate_x_adj = case_when(stand == "L" & plate_x >= 0 ~ -abs(plate_x),
                            stand == "L" & plate_x < 0 ~ abs(plate_x),
                            stand == "R" & plate_x >= 0 ~ abs(plate_x),
                            stand == "R" & plate_x < 0 ~ -abs(plate_x)),
    pfx_x_adj = case_when(p_throws == "R" & stand == "R" & pfx_x < 0 ~ -abs(pfx_x),
                          p_throws == "R" & stand == "L" & pfx_x >= 0 ~ -abs(pfx_x),
                          p_throws == "L" & stand == "R" & pfx_x < 0 ~ -abs(pfx_x),
                          p_throws == "L" & stand == "L" & pfx_x >= 0 ~ -abs(pfx_x),
                          .default = abs(pfx_x)),
    pitch_class = case_when(
      pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
      pitch_type %in% c("CU", "KC", "ST", "SL", "CS", "SV", "KN") ~ "Breaking",
      .default = NA
    ),
    bat_team = ifelse(inning_topbot == "Top", away_team, home_team),
    def_team = ifelse(inning_topbot == "Top", home_team, away_team),
    def_score_diff = ifelse(away_team == def_team, bat_score_diff * -1, bat_score_diff),
    on_1b_ind = ifelse(!is.na(on_1b), 1, 0),
    on_2b_ind = ifelse(!is.na(on_2b), 1, 0),
    on_3b_ind = ifelse(!is.na(on_1b), 1, 0),
    description_ind = ifelse(description == "called_strike", "strike", "ball"),
    hand_match = case_when(
      stand == "L" & p_throws == "L" | stand == "R" & p_throws == "R" ~ 1,
      .default = 0
    )
  ) %>%
  left_join(winprobabilities2, by = c("on_1b_ind", "on_2b_ind", "on_3b_ind", "balls", "strikes", "outs_when_up", "description_ind")) %>%
  mutate(delta = abs(delta_run_exp_2.x - delta_run_exp_2.y))

game_pks <- unique(modeldata$game_pk)

df <- data.frame()

for (game_id in game_pks){
  url <- paste0('http://statsapi.mlb.com/api/v1.1/game/', game_id, '/feed/live')
  response <- GET(url)
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  umps <- json_data$liveData$boxscore$officials$official
  roles <- json_data$liveData$boxscore$officials$officialType
  
  umps <- cbind(umps, roles) %>%
    filter(roles == "Home Plate") %>%
    mutate(game_pk = game_id) %>%
    select(game_pk, fullName)
  
  df <- rbind(df, umps)
}

modeldata_hitter <- modeldata %>%
  filter(description == "called_strike" | challenger == "Hitter", !(challenge_catcher == 1 & description == "called_strike")) %>%
  mutate(challenge_hitter_lost = challenge_hitter == 1 & call_change_hitter == 0) %>%
  arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, bat_team) %>%
  mutate(
    challenges_remaining = pmax(2 + (inning > 9) - lag(cumsum(challenge_hitter_lost), default = 0), 0) # ADJUST CODE FOR EXTRA INNINGS
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(df %>% rename(umpire_hp = fullName), by = "game_pk")

modeldata_catcher <- modeldata %>%
  filter(description != "called_strike" | challenger == "Catcher", !(challenge_hitter == 1 & description == "ball")) %>%
  mutate(challenge_catcher_lost = challenge_catcher == 1 & call_change_catcher == 0) %>%
  arrange(game_pk, def_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, def_team) %>%
  mutate(
    challenges_remaining = pmax(2 - lag(cumsum(challenge_catcher_lost), default = 0), 0) # ADJUST CODE FOR EXTRA INNINGS
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(df %>% rename(umpire_hp = fullName), by = "game_pk")

## ---- Hitting Models ----

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

mean(glm_results$auc)

print(paste("Area under the curve for each glm fold:", paste(round(glm_results$auc, 3), collapse = ", ")))

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





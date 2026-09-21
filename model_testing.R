library(tidyverse)
library(pROC)
library(splines)
library(caret)

set.seed(1989)

k_folds <- 5

folds <- createFolds(modeldata_hitter$challenge_hitter, k = k_folds, list = TRUE, returnTrain = FALSE)

glm_results_hitter <- tibble()

for (p in 1:15) {
  
  for (i in 1:k_folds) {
    
    test_indices <- folds[[i]]
    
    train <- modeldata_hitter[-test_indices, ]
    test  <- modeldata_hitter[test_indices, ]
    
    z_terms <- paste0("ns(plate_z_adj, ", 1:p, ")", collapse = " + ")
    x_terms <- paste0("ns(plate_x_adj, ", 1:p, ")", collapse = " + ")
    
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

set.seed(1989)

k_folds <- 5

folds <- createFolds(modeldata_catcher$challenge_catcher, k = k_folds, list = TRUE, returnTrain = FALSE)

glm_results_catcher <- tibble()

for (p in 1:15) {
  
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
    
    glm_results_catcher <- bind_rows(
      glm_results_catcher,
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

glm_results_catcher %>% group_by(modeltype) %>% reframe(auc = mean(auc)) %>% arrange(desc(auc))

## Other Model Testing

set.seed(1989)

k_folds <- 5

folds <- createFolds(modeldata_hitter$challenge_hitter, k = k_folds, list = TRUE, returnTrain = FALSE)

glm_results_hitter_2 <- tibble()

for (i in 1:k_folds) {
  
  test_indices <- folds[[i]]
  
  train <- modeldata_hitter[-test_indices, ]
  test  <- modeldata_hitter[test_indices, ]

  mod <- glmer(
    challenge_hitter ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining +
      (1|fielder_2) + (1|batter) + (1|bat_team) + (1|def_team) + (1|umpire_hp),
    data = train,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa")
  )
  
  pred <- predict(
    mod,
    newdata = test,
    type = "response"
  )
  
  fold_auc <- as.numeric(
    auc(test$challenge_hitter, pred)
  )
  
  glm_results_hitter_2 <- bind_rows(
    glm_results_hitter_2,
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

glm_results_hitter_2 %>% group_by(modeltype) %>% reframe(auc = mean(auc)) %>% arrange(desc(auc))

auc(modeldata_catcher$challenge_catcher, predict(mod2_call_change_catcher, newdata = modeldata_catcher, type = "response"))

auc(modeldata_hitter$challenge_hitter, predict(mod2_call_change_hitter, newdata = modeldata_hitter, type = "response"))

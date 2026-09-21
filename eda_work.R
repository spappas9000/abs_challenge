library(tidyverse)
library(baseballr)
library(data.table)
library(arrow)
source("data.R")

table(challenge$challenger)/nrow(challenge)

challenge %>% 
  group_by(challenger) %>%
  reframe(
    success = length(which(call_change == 1)),
    total = n(),
    success_p = success/total
  )

summary(aov(call_change ~ challenger, data = challenge))

TukeyHSD(aov(call_change ~ challenger, data = challenge))

challenge %>%
  group_by(pitch_class) %>%
  reframe(
    success = length(which(call_change == 1)),
    total = n(),
    success_p = success/total
  ) %>%
  arrange(desc(success_p))

summary(aov(call_change ~ pitch_class, data = challenge))

challenge %>%
  group_by(inning) %>%
  reframe(
    success = length(which(call_change == 1)),
    total = n(),
    success_p = success/total
  ) %>%
  arrange(-desc(inning))

summary(aov(call_change_hitter ~ inning, data = modeldata))

summary(aov(call_change_hitter ~ factor(inning), data = modeldata))

challenge %>%
  group_by(challenger, pitch_class) %>%
  reframe(
    success = length(which(call_change == 1)),
    total = n(),
    success_p = success/total
  ) %>%
  arrange(desc(success_p))

summary(aov(call_change ~ challenger + pitch_class, data = challenge))

challenge %>%
  group_by(challenger, inning) %>%
  reframe(
    success = length(which(call_change == 1)),
    total = n(),
    success_p = success/total
  ) %>%
  arrange(desc(success_p))

summary(aov(call_change ~ challenger + inning, data = challenge))

summary(aov(call_change ~ challenger + factor(inning), data = challenge))

summary(aov(call_change ~ challenger + inning + pitch_class, data = challenge))

modeldata2 %>%
  filter(outs_when_up == 1, !is.na(on_1b), !is.na(on_2b), !is.na(on_3b), home_team == bat_team, description == "ball", 
         count == "0-0", home_score_diff == 2) %>%
  arrange(-desc(inning)) %>%
  select(outs_when_up, on_1b, on_2b, on_3b, home_team, bat_team, description, count, inning, home_score_diff, delta_home_win_exp, delta_run_exp) %>%
  view()

modeldata2 %>%
  filter(!is.na(on_1b), !is.na(on_2b), !is.na(on_3b), count == "3-2", description != "called_strike") %>%
  view()

# Sanity check: did any call being overturned have a delta of 0?
length(which(runexpectancies2$delta == 0))/nrow(runexpectancies2)

modeldata2 %>%
  mutate(on_1b_ind = ifelse(!is.na(on_1b), 1, 0), on_2b_ind = ifelse(!is.na(on_2b), 1, 0), on_3b_ind = ifelse(!is.na(on_3b), 1, 0)) %>%
  filter(on_1b_ind == 1, on_2b_ind == 1, on_3b_ind == 0, outs_when_up == 0, balls == 3, strikes == 2,
         description == "called_strike") %>%
  view()

modeldata2 %>%
  select(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description_ind, description_ind.y.y, delta) %>%
  view()
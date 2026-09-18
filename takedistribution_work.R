
library(tidyverse)

mlbdata <- readRDS("takedistribution/statcastdata.rds") %>%
  mutate(bat_team = ifelse(inning_topbot == "Top", away_team, home_team),
         def_team = ifelse(inning_topbot == "Top", home_team, away_team)) %>%
  filter(!description %in% c("automatic_ball", "automatic_strike")) %>%
  arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, bat_team, inning) %>%
  mutate(outs_on_play = lead(outs_when_up) - outs_when_up,
         outs_on_play = ifelse(is.na(outs_on_play), 3 - outs_when_up, outs_on_play)) %>%
  ungroup() %>%
  group_by(game_pk, bat_team) %>%
  arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  mutate(outs = cumsum(outs_on_play)) %>%
  ungroup()

takefunction <- function(b, s, o) {
  
  remaining <- mlbdata %>%
    arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
    group_by(game_pk, bat_team) %>%
    mutate(is_strike = description %in% c("called_strike"),
           strikes_total = sum(is_strike),
           strikes_upto = cumsum(is_strike),
           strikesremaining = strikes_total - strikes_upto,
           is_ball = description %in% c("ball", "blocked_ball", "pitchout"),
           balls_total = sum(is_ball),
           balls_upto = cumsum(is_ball),
           ballsremaining = balls_total - balls_upto) %>%
    ungroup() %>%
    filter(outs == o,
           balls == b,
           strikes == s) %>%
    select(strikesremaining, ballsremaining)
  
  return(remaining)
  
}

remaining <- takefunction(b = 2, s = 1, o = 4)


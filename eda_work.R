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
  select(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description, description.y.y, delta) %>%
  view()

# Miss distance stuff
# Zone agreement:
strike <- c(1:9)
ball <- c(11:14)
modeldata_hitter |> 
  filter(miss_dist >= 0 & challenge_hitter == 1 & call_change_hitter == 0 &
           zone %in% strike) |> 
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()

modeldata_catcher |> 
  filter(miss_dist <= 0 & challenge_catcher == 1 & call_change_catcher == 0 &
           zone %in% ball) |> 
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()

# ABS agreement:
# True balls that were called strikes and not overturned
modeldata_hitter |> 
  filter(miss_dist >= 0 & challenge_hitter == 1 & call_change_hitter == 0) |>
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()
# Two pitches: Will review footage
# 06-16-26: Tyler Phillips (MIA) vs. Alec Bohm (PHI), Inn. 1, 1 out, 0-1 count
#           Nicked the zone by the smallest hair.
#           Throw em out

# 05-30-26: Ryan Weathers (NYY) vs. Tyler Soderstrom (ATH), Inn. 4, 0 out, 2-0 count
#           This one is very weird: The pitch appeared to be a ball according to 
#           the K-Zone and looks like a ball on Savant's own pitch chart, yet the 
#           call was upheld. On top of this, the ABS visualization didn't even show 
#           up on the Sacramento video board or on the broadcast.
#           Throw this one out.

# True strikes that were called balls and not overturned
modeldata_catcher |> 
  filter(miss_dist <= 0 & challenge_catcher == 1 & call_change_catcher == 0) |>
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()

# Nothing here.

# Flipping ABS agreement. Pitches that were true strikes but were overturned
modeldata_hitter |> 
  filter(miss_dist <= 0 & challenge_hitter == 1 & call_change_hitter == 1) |>
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()

# 06-26-26: Jhoan Duran (PHI) vs. Jared Young (NYM), Inn. 9, 2 out, 0-1
#           Another strange pitch here: Appeared to be a strike according to the K-Zone
#           but on Pitch3D is a ball (for Jared Young specifically)
#           

# Pitches that were true balls but were overturned
modeldata_catcher |> 
  filter(miss_dist >= 0 & challenge_catcher == 1 & call_change_catcher == 1) |>
  select(game_date, player_name, home_team, away_team, outs_when_up, inning, inning_topbot,
         pitch_name, count) |> 
  view()

# 06-03-26: Cody Laweryson (MIN) vs. Chase Meidroth (CWS), Inn. 8, 1 out, 1-2
#           This was a strike that barely nicked the bottom of the zone on the pitcher's left
#           side. 


# Are there differences in the probability of challenging by different pitch types?
# Is a changeup or fastball easier to challenge than any breaking?

# Three group bys: Take the full data, group_by actual ball or strike variable, 
# the call, and then the challenge status
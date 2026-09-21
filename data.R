# THIS SCRIPT IS A MUST-RUN BEFORE EVERY SESSION
# this script initializes all of the data you need for EDA, modeling and 
# visualization.
library(tidyverse)
library(baseballr)
library(data.table)
library(arrow)

# All extra innings are binned with the 10th inning. Setting it as a variable prevents
# drift if we ever want to change this.
extra_inning_cap <- 10

statcastlist_26 <- read_parquet("data/statcastlist_26.parquet")

playerid <- read_csv("data/chadwick_players.csv")

umps <- read_csv("data/home_plate_umps.csv")

challenge = list(
  catcher = read_csv("data/catcher.csv") %>%
    mutate(challenger = "Catcher"),
  hitter = read_csv("data/hitter.csv") %>%
    mutate(challenger = "Hitter"),
  pitcher = read_csv("data/pitcher.csv") %>%
    mutate(challenger = "Pitcher")
) %>%
  rbindlist() %>%
  mutate(
    pitch_class = case_when(
      pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
      pitch_type %in% c("CU", "KC", "ST", "SL", "CS", "SV", "KN") ~ "Breaking",
      .default = NA
    ),
    inning = case_when(
      inning > 9 ~ extra_inning_cap,
      .default = inning
    ),
    challenge = 1,
    challenge_hitter = ifelse(challenger == "Hitter", 1, 0),
    challenge_catcher = ifelse(challenger == "Catcher", 1, 0),
    call_change_hitter = ifelse(challenger == "Hitter" & description == "ball", 1, 0),
    call_change_catcher = ifelse(challenger == "Catcher" & description == "called_strike", 1, 0),
    call_change = ifelse(call_change_hitter == 1 | call_change_catcher == 1, 1, 0)
  )

playerid <- playerid %>%
  mutate(player_name = paste0(name_last, ", ", name_first)) %>%
  select(key_mlbam, player_name)

modeldata <- rbind(challenge, statcastlist_26, fill = T) %>%
  filter(description %in% c("blocked_ball", "ball", "called_strike", "pitchout"),
         !des %in% c("Cedric Mullins called out on strikes. Cedric Mullins to 1st. Passed ball by catcher Mickey Gasper.",
                     "Jacob Young called out on strikes. Jacob Young to 1st. Passed ball by catcher Gabriel Moreno."),
         grepl("catcher interference", des) == F,
         plate_x <= 1.666 & plate_x >= -1.666,
         plate_z <= 4.5 & plate_z >= 0.5) %>%
  distinct(game_pk, at_bat_number, pitch_number, .keep_all = T) %>%
  mutate(
    inning = case_when(
      inning > 9 ~ extra_inning_cap,
      .default = inning
    ),
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
    on_3b_ind = ifelse(!is.na(on_3b), 1, 0),
    description_ind = ifelse(description == "called_strike", "strike", "ball"),
    hand_match = case_when(
      stand == "L" & p_throws == "L" | stand == "R" & p_throws == "R" ~ 1,
      .default = 0
    )
  )

runexpectancies <- modeldata %>%
  mutate(on_1b_ind = ifelse(!is.na(on_1b), 1, 0), on_2b_ind = ifelse(!is.na(on_2b), 1, 0), on_3b_ind = ifelse(!is.na(on_3b), 1, 0),
         delta_runexp = ifelse(home_team == bat_team, delta_run_exp, delta_run_exp * -1), 
         scorediff = ifelse(home_team == bat_team, bat_score_diff, bat_score_diff * -1),
         description_ind = ifelse(description == "called_strike", "strike", "ball")) %>%
  group_by(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description_ind) %>%
  reframe(delta_run_exp_2 = mean(delta_run_exp, na.rm = TRUE), max_runexp = max(delta_run_exp, na.rm = TRUE), min_runexp = min(delta_run_exp, na.rm = TRUE), 
          N = n()) %>%
  mutate(rowid = row_number())

runexpectancies2 <- merge(runexpectancies, runexpectancies, by = c(1:6)) %>%
  filter(rowid.x != rowid.y) %>%
  rename(description_ind = description_ind.x)

modeldata2 <- modeldata %>%
  left_join(runexpectancies2, by = c("on_1b_ind", "on_2b_ind", "on_3b_ind", "balls", "strikes", "outs_when_up", "description_ind")) %>%
  mutate(delta = abs(delta_run_exp_2.x - delta_run_exp_2.y))

chase <- read_csv("data/chase.csv") %>%
  mutate(batter = as.character(player_id)) %>%
  select(batter, 3:5)

modeldata_hitter <- modeldata2 %>%
  filter(description == "called_strike" | challenger == "Hitter", !(challenge_catcher == 1 & description == "called_strike")) %>%
  mutate(challenge_hitter_lost = challenge_hitter == 1 & call_change_hitter == 0) %>%
  arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, bat_team) %>%
  mutate(
    challenges_remaining = pmax(2 + (inning > 9) - lag(cumsum(challenge_hitter_lost), default = 0), 0) # ADJUST CODE FOR EXTRA INNINGS
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(umps %>% rename(umpire_hp = fullName), by = "game_pk")

modeldata_catcher <- modeldata2 %>%
  filter(description != "called_strike" | challenger == "Catcher", !(challenge_hitter == 1 & description == "ball")) %>%
  mutate(challenge_catcher_lost = challenge_catcher == 1 & call_change_catcher == 0) %>%
  arrange(game_pk, def_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, def_team) %>%
  mutate(
    challenges_remaining = pmax(2 - lag(cumsum(challenge_catcher_lost), default = 0), 0) # ADJUST CODE FOR EXTRA INNINGS
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(umps %>% rename(umpire_hp = fullName), by = "game_pk")

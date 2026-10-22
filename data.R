# THIS SCRIPT IS A MUST-RUN BEFORE EVERY SESSION
# this script initializes all of the data you need for EDA, modeling and 
# visualization.
library(tidyverse)
library(baseballr)
library(data.table)
library(arrow)

# Adding distance from the center of the ball to the edges of the zone
# At 1.45, there are 4 challenges where miss distance > 0 that were not overturned
# (zone agreement 100%) compared to 34 at 9/(2*pi) (zone agreement = 99.89%)
ball_radius <- 1.45/12 # The strike zone expands by ~1 baseball, where the center of the ball is ~1/2 a baseball off of the plate.
plate_edge <- 8.5/12 

# All extra innings are binned with the 10th inning. Setting it as a variable prevents
# drift if we ever want to change this.
extra_inning_cap <- 10

statcastlist_26 <- read_parquet("data/statcastlist_26.parquet")

playerid <- read_csv("data/chadwick_players.csv")

contract <- read_csv("data/contract_salary_2026.csv")

umps <- read_csv("data/home_plate_umps.csv")

# Only selecting what we need from contract:
contract <- contract |> 
  select(player_id, player_name, role, position, service_time, salary) |> 
  filter(role != "Pitcher") # We don't care about pitchers

contract |> filter(is.na(service_time)) |> nrow()
# We have 62 players for which service time is not available. This needs to be fixed.

sts <- c(8.114, # Orlando Arcia
         2.087, # Jose Azocar
         1.137, # Jihwan Bae
         0.164, # Luken Baker
         2.050, # Jose Barrero
         2.063, # Jonah Bride
         3.014, # Vidal Brujan
         2.061, # Daz Cameron
         3.078, # Bryan De La Cruz
         9.087, # Elias Diaz
         1.021, # Oliver Dunn
         3.114, # Stuart Fairchild
         3.060, # Aramis Garcia
         4.114, # Eric Haase
         1.103, # P.J. Higgins
         2.057, # Sam Huff
         3.036, # Alex Jackson
         6.000, # Eloy Jimenez
         0.124, # Leo Jimenez
         4.068, # Connor Joe
         1.025, # Daniel Johnson
         1.123, # Corey Julks
         3.035, # Jarred Kelenic
         1.006, # Buddy Kennedy
         9.152, # Max Kepler
         5.090, # Andrew Knizner
         8.159, # Sandy Leon
         6.021, # Nicky Lopez
         1.149, # Vimael Machin
         5.087, # Nick Madrigal
         8.086, # Luke Maile
         6.141, # Trey Mancini
         1.156, # Luis Matos
         1.094, # Curtis Mead
         1.164, # Joey Meneses
         2.133, # Owen Miller
         3.031, # Cristian Pache
         10.092, # Tommy Pham
         5.104, # Josh Rojas
         3.135, # Ben Rortvedt
         1.057, # Ali Sanchez
         1.089, # Brian Serven
         2.070, # Zack Short
         0.088, # Ronny Simon
         4.015, # Jose Siri
         2.150, # Nick Solak
         1.092, # Michael Stefanic
         4.148, # Garrett Stubbs
         6.169, # Rowdy Tellez,
         2.039, # Michael Toglia
         2.144, # Taylor Trammell
         2.120, # Chadwick Tromp
         6.007, # Luis Urias
         3.033, # Andrew Velazquez
         1.101, # Nelson Velazquez
         5.163, # LaMonte Wade Jr
         1.048, # Donovan Walton
         1.032, # Alika Williams
         3.065, # Luke Williams
         1.097, # Weston Wilson
         4.058, # Patrick Wisdom
         1.105 # Brett Wisely
         )

length(sts)

contract$service_time[is.na(contract$service_time)] <- sts

contract <- contract |> 
  mutate(st_years = floor(service_time),
         st_days = round((service_time - st_years) * 1000),
         service_years_calc = st_years + st_days / 172)

contract <- contract |> 
  mutate(service_years = round(service_years_calc, 3)) |> 
  select(-st_years, -st_days, -service_years_calc)

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
    call_change = case_when(
      call_change_catcher == 1 | call_change_hitter == 1 ~ 1,
      .default = 0
    )
  )

playerid <- playerid %>%
  mutate(player_name = paste0(name_last, ", ", name_first)) %>%
  select(key_mlbam, player_name)

modeldata <- rbind(challenge, statcastlist_26, fill = T) %>%
  filter(description %in% c("ball", "called_strike"),
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
      pitch_type %in% c("CH", "FS", "FO", "SC", "KN", "EP") ~ "Offspeed",
      pitch_type %in% c("CU", "KC", "ST", "SL", "CS", "SV") ~ "Breaking",
      .default = NA
    ),
    bat_team = ifelse(inning_topbot == "Top", away_team, home_team),
    def_team = ifelse(inning_topbot == "Top", home_team, away_team),
    def_score_diff = ifelse(away_team == def_team, bat_score_diff * -1, bat_score_diff),
    on_1b_ind = ifelse(!is.na(on_1b), 1, 0),
    on_2b_ind = ifelse(!is.na(on_2b), 1, 0),
    on_3b_ind = ifelse(!is.na(on_3b), 1, 0),
    hand_match = case_when(
      stand == "L" & p_throws == "L" | stand == "R" & p_throws == "R" ~ 1,
      .default = 0
    )
  )

runexpectancies <- modeldata %>%
  mutate(on_1b_ind = ifelse(!is.na(on_1b), 1, 0), on_2b_ind = ifelse(!is.na(on_2b), 1, 0), on_3b_ind = ifelse(!is.na(on_3b), 1, 0),
         delta_runexp = ifelse(home_team == bat_team, delta_run_exp, delta_run_exp * -1), 
         scorediff = ifelse(home_team == bat_team, bat_score_diff, bat_score_diff * -1)) %>%
  group_by(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description) %>%
  reframe(delta_run_exp_2 = mean(delta_run_exp, na.rm = TRUE), max_runexp = max(delta_run_exp, na.rm = TRUE), 
          min_runexp = min(delta_run_exp, na.rm = TRUE), N = n()) %>%
  mutate(rowid = row_number())

runexpectancies2 <- merge(runexpectancies, runexpectancies, by = c(1:6)) %>%
  filter(rowid.x != rowid.y) %>%
  rename(description = description.x)

modeldata2 <- modeldata %>%
  left_join(runexpectancies2, by = c("on_1b_ind", "on_2b_ind", "on_3b_ind", "balls", "strikes", "outs_when_up", "description")) %>%
  mutate(delta = abs(delta_run_exp_2.x - delta_run_exp_2.y),
         half_zone_ht = (sz_top - sz_bot)/2,
         x_edge_dist = abs(plate_x_adj) - plate_edge,
         z_edge_dist = abs(plate_z_adj) - half_zone_ht,
         min_edge_dist = case_when(
           x_edge_dist > 0 & z_edge_dist > 0 ~ sqrt(x_edge_dist^2 + z_edge_dist^2), # Corner: The shortest distance here will be diagonal
           x_edge_dist > 0 & z_edge_dist <= 0 ~ x_edge_dist, # Inside/outside
           x_edge_dist <= 0 & z_edge_dist > 0 ~ z_edge_dist, # High/low
           x_edge_dist <= 0 & z_edge_dist <= 0 & x_edge_dist > z_edge_dist ~ x_edge_dist, # Strike, closer to inner/outer edge
           x_edge_dist <= 0 & z_edge_dist <= 0 & x_edge_dist <= z_edge_dist ~ z_edge_dist # Strike, closer to upper/lower edge
         ),
         miss_dist = min_edge_dist - ball_radius,
         truth = if_else(miss_dist > 0, "ball", "strike"),
         call = case_when(
           call_change_catcher == 1 ~ "ball",
           call_change_hitter == 1 ~ "strike",
           description == "called_strike" ~ "strike",
           description == "ball" ~ "ball"
         ))

chase <- read_csv("data/chase.csv") %>%
  mutate(batter = as.character(player_id)) %>%
  select(batter, 3:5)

modeldata_hitter <- modeldata2 %>%
  filter(description == "called_strike" | challenger == "Hitter", !(challenge_catcher == 1 & description == "called_strike"), !(inning > 9)) %>%
  mutate(challenge_hitter_lost = challenge_hitter == 1 & call_change_hitter == 0) %>%
  arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, bat_team) %>%
  mutate(
    challenges_remaining = pmax(2 - lag(cumsum(challenge_hitter_lost), default = 0), 0)
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(umps %>% rename(umpire_hp = fullName), by = "game_pk") |> 
  # Pulling out the ABS disagreements
  filter(!(miss_dist <= 0 & challenge_hitter == 1 & call_change_hitter == 1) | !(miss_dist >= 0 & challenge_hitter == 1 & call_change_hitter == 0)) |> 
  left_join(contract, by = c("batter" = "player_id"))

modeldata_catcher <- modeldata2 %>%
  filter(description != "called_strike" | challenger == "Catcher", !(challenge_hitter == 1 & description == "ball"), !(inning > 9)) %>%
  mutate(challenge_catcher_lost = challenge_catcher == 1 & call_change_catcher == 0) %>%
  arrange(game_pk, def_team, -desc(at_bat_number), -desc(pitch_number)) %>%
  group_by(game_pk, def_team) %>%
  mutate(
    challenges_remaining = pmax(2 - lag(cumsum(challenge_catcher_lost), default = 0), 0)
  ) %>%
  ungroup() %>%
  filter(challenges_remaining != 0) %>%
  left_join(umps %>% rename(umpire_hp = fullName), by = "game_pk") |> 
  # Pulling out the ABS disagreements
  filter(!(miss_dist >= 0 & challenge_catcher == 1 & call_change_catcher == 1)) |> 
  left_join(contract, by = c("fielder_2" = "player_id"))

rm(statcastlist_26)
rm(umps)
rm(challenge)
rm(modeldata)
rm(runexpectancies)
rm(runexpectancies2)
rm(chase)
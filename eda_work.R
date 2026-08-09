
install_github("BillPetti/baseballr")

install.packages("baseballr")

library(tidyverse)
library(baseballr)
library(data.table)

statcast_bind_rows <- function(start_date, end_date, player_type) {
  
  start <- as.Date(start_date)
  end <- as.Date(end_date) - 1
  range <- seq(start, end, "5 days")
  range_offset <- seq(start + 5, end + 5, "5 days")
  
  if (range_offset[length(range_offset)] != end + 1) {
    range_offset[length(range_offset)] <- end + 1
  }
  
  stats_list <- map2_df(range, range_offset, function(x, y) {
    baseballr::statcast_search(
      start_date = x,
      end_date = y,
      player_type = player_type
    )
  })
  
  return(stats_list)
  
}

statcastlist_26 = list(
  april26 = statcast_bind_rows(start_date = "2026-03-25", end_date = "2026-04-30", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 4,
      across(where(is.character), ~ na_if(., ""))
    ),
  may26 = statcast_bind_rows(start_date = "2026-05-01", end_date = "2026-05-31", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 5,
      across(where(is.character), ~ na_if(., ""))
    ),
  june26 = statcast_bind_rows(start_date = "2026-06-01", end_date = "2026-06-30", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 6,
      across(where(is.character), ~ na_if(., ""))
    ),
  july26_1 = statcast_bind_rows(start_date = "2026-07-01", end_date = "2026-07-12", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 7,
      across(where(is.character), ~ na_if(., ""))
    ),
  july26_2 = statcast_bind_rows(start_date = "2026-07-16", end_date = "2026-07-31", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 7,
      across(where(is.character), ~ na_if(., ""))
    )
) %>% 
  rbindlist()

challenge = list(
  catcher = read_csv("data/catcher.csv") %>%
    mutate(challenger = "Catcher"),
  hitter = read_csv("data/hitter.csv") %>%
    mutate(challenger = "Hitter")
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
      inning > 9 ~ 10,
      .default = inning
    ),
    challenge_hitter = ifelse(challenger == "Hitter", 1, 0),
    challenge_catcher = ifelse(challenger == "Catcher", 1, 0),
    call_change_hitter = ifelse(challenger == "Hitter" & description == "ball", 1, 0),
    call_change_catcher = ifelse(challenger == "Catcher" & description == "called_strike", 1, 0)
  )

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

summary(aov(call_change ~ inning, data = challenge))

summary(aov(call_change ~ factor(inning), data = challenge))

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

chase <- read_csv("data/chase.csv") %>%
  mutate(batter = as.character(player_id)) %>%
  select(batter, 3:5)

modeldata %>%
  filter(outs_when_up == 1, !is.na(on_1b), !is.na(on_2b), !is.na(on_3b), home_team == bat_team, description == "ball", 
         count == "0-0", home_score_diff == 2) %>%
  arrange(-desc(inning)) %>%
  select(outs_when_up, on_1b, on_2b, on_3b, home_team, bat_team, description, count, inning, home_score_diff, delta_home_win_exp, delta_run_exp) %>%
  view()

modeldata %>%
  filter(!is.na(on_1b), !is.na(on_2b), !is.na(on_3b), count == "3-2", description != "called_strike") %>%
  view()

winprobabilities <- modeldata %>%
  mutate(on_1b_ind = ifelse(!is.na(on_1b), 1, 0), on_2b_ind = ifelse(!is.na(on_2b), 1, 0), on_3b_ind = ifelse(!is.na(on_3b), 1, 0),
         delta_runexp = ifelse(home_team == bat_team, delta_run_exp, delta_run_exp * -1), 
         scorediff = ifelse(home_team == bat_team, bat_score_diff, bat_score_diff * -1),
         description_ind = ifelse(description == "called_strike", "strike", "ball")) %>%
  group_by(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description_ind) %>%
  reframe(delta_run_exp_2 = mean(delta_run_exp), max_runexp = max(delta_run_exp), min_runexp = min(delta_run_exp), 
          N = n()) %>%
  mutate(rowid = row_number()) %>%
  view()

winprobabilities2 <- merge(winprobabilities, winprobabilities, by = c(1:6)) %>%
  filter(rowid.x != rowid.y) %>%
  rename(description_ind = description_ind.x) %>%
  view()

length(which(winprobabilities2$delta == 0))/nrow(winprobabilities2)

modeldata %>%
  mutate(on_1b_ind = ifelse(!is.na(on_1b), 1, 0), on_2b_ind = ifelse(!is.na(on_2b), 1, 0), on_3b_ind = ifelse(!is.na(on_3b), 1, 0)) %>%
  filter(on_1b_ind == 1, on_2b_ind == 1, on_3b_ind == 0, outs_when_up == 0, balls == 3, strikes == 2,
         description == "called_strike") %>%
  view()

modeldata %>%
  select(on_1b_ind, on_2b_ind, on_3b_ind, outs_when_up, balls, strikes, description_ind, description_ind.y.y, delta) %>%
  view()
  

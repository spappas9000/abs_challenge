
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
    statcast_search(
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
  may26 = statcast_bind_rows(start_date = "2026-05-01", end_date = "2026-05-14", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 4,
      across(where(is.character), ~ na_if(., ""))
    )
) %>% 
  rbindlist()

challenge = list(
  catcher = read_csv("data/catcher.csv") %>%
    mutate(challenger = "Catcher"),
  pitcher = read_csv("data/pitcher.csv") %>%
    mutate(challenger = "Pitcher"),
  hitter = read_csv("data/hitter.csv") %>%
    mutate(challenger = "Hitter")
) %>%
  rbindlist() %>%
  mutate(
    call_change = case_when(
      challenger == "Catcher" ~ ifelse(description == "called_strike", 1, 0),
      challenger == "Pitcher" ~ ifelse(description == "called_strike", 1, 0),
      challenger == "Hitter" ~ ifelse(description == "ball", 1, 0)
    ),
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
    challenge_catcher = ifelse(challenger == "Catcher", 1, 0)
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

## ---- Modeling ----

library(lme4)

modeldata <- rbind(challenge, statcastlist_26, fill = T) %>%
  filter(!description %in% c("swinging_strike", "hit_into_play", "foul_tip", "foul", "foul_bunt", 
                             "missed_bunt")) %>%
  distinct(game_pk, at_bat_number, pitch_number, .keep_all = T) %>%
  mutate(
    count = paste0(balls, "-", strikes),
    challenge_hitter = ifelse(is.na(challenge_hitter), 0, challenge_hitter),
    challenge_catcher = ifelse(is.na(challenge_catcher), 0, challenge_catcher),
    call_change = ifelse(is.na(call_change), 0, call_change),
    plate_z_adj = plate_z - ((sz_top - sz_bot)/2 + sz_bot),
    plate_x_adj = case_when(stand == "L" & plate_x >= 0 ~ -abs(plate_x),
                            stand == "L" & plate_x < 0 ~ abs(plate_x),
                            stand == "R" & plate_x >= 0 ~ abs(plate_x),
                            stand == "R" & plate_x < 0 ~ -abs(plate_x)),
    bat_team = ifelse(inning_topbot == "Top", away_team, home_team),
    def_team = ifelse(inning_topbot == "Top", home_team, away_team),
  )

mod0_call_change <- glm(
  is_challenge ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj,
  data = modeldata,
  family = "binomial"
)

mod1_call_change_hitter <- glmer(
  challenge_hitter ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj + (1|batter) +
                     (1|bat_team) + inning + bat_score_diff + count,
  data = modeldata,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

mod1_call_change_catcher <- glmer(
  challenge_catcher ~ I(plate_z_adj^2) + I(plate_x_adj^2) + plate_z_adj + plate_x_adj + (1|fielder_2) + 
    (1|def_team),
  data = modeldata,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa")
)

re <- ranef(mod1_call_change_hitter, condVar = TRUE)

hitter_re <- re$batter

post_var <- attr(hitter_re, "postVar")

re_exploration <- data.frame(
  batter = rownames(hitter_re),
  estimate = hitter_re[, "(Intercept)"],
  se = sqrt(post_var[1, 1, ])
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )
  left_join(chase, by = "batter")

anova(mod1_call_change, mod0_call_change)

summary(mod0_call_change)

summary(mod1_call_change_hitter)

re_exploration %>%
  filter(total_pitches >= 100) %>%
  ggplot(aes(x = pitch_percent, y = lower)) +
  geom_point() +
  geom_smooth(method = "lm")

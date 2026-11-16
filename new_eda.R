library(lme4)
library(car)
library(caret)
library(pROC)
library(MLmetrics)
library(splines)
library(DHARMa)
library(tidyverse)
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
hit_chal_ps <- modeldata_hitter |> 
  filter(!is.na(pitch_class)) |> 
  group_by(pitch_class) |> 
  summarise(
    n = n(),
    n_opp = sum(miss_dist > 0),
    p_opp = n_opp / n,
    n_chal = sum(challenge_hitter),
    p_chal = n_chal / n,
    n_good = sum(challenge_hitter & miss_dist > 0),
    p_good = n_good / n_chal,
    n_bad = n_chal - n_good,
    p_bad = 1 - p_good
  )

hit_total <- hit_chal_ps |> 
  summarise(pitch_class = "Total", across(c(n, n_chal, n_good, n_bad), sum)) |> 
  mutate(
    p_chal = n_chal / n,
    p_good = n_good / n_chal,
    p_bad  = n_bad / n_chal
  )

hit_chal_ps |> 
  select(-n_opp, -p_opp) |> 
  bind_rows(hit_total) |> 
  gt(rowname_col = "pitch_class") |> 
  tab_header("Observed Challenge Probabilities by Pitch Class - Hitters",
             subtitle = "C = Challenge, GC = Good Challenge, BC = Bad Challenge") |> 
  fmt_number(columns = c(p_chal, p_good, p_bad), decimals = 3) |> 
  fmt_integer(columns = c(n, n_chal, n_good, n_bad)) |> 
  cols_label(n = "N", n_chal = "C", p_chal = "P(C)", n_good = "GC", p_good = "P(GC)", 
             n_bad = "BC", p_bad = "P(BC)") |> 
  tab_stubhead("Pitch Class") |> 
  tab_style(
    style = list(cell_text(weight = "bold"), cell_borders(sides = "top", weight = px(2))),
    locations = list(cells_body(rows = pitch_class == "Total"),
                     cells_stub(rows = pitch_class == "Total"))
  )

cat_chal_ps <- modeldata_catcher |> 
  filter(!is.na(pitch_class)) |> 
  group_by(pitch_class) |> 
  summarise(
    n = n(),
    n_opp = sum(miss_dist <= 0),
    p_opp = n_opp / n,
    n_chal = sum(challenge_catcher),
    p_chal = n_chal / n,
    n_good = sum(challenge_catcher & miss_dist <= 0),
    p_good = n_good / n_chal,
    n_bad = n_chal - n_good,
    p_bad = 1 - p_good
  )

cat_total <- cat_chal_ps |> 
  summarise(pitch_class = "Total", across(c(n, n_chal, n_good, n_bad), sum)) |> 
  mutate(
    p_chal = n_chal / n,
    p_good = n_good / n_chal,
    p_bad  = n_bad / n_chal
  )

cat_chal_ps |> 
  select(-n_opp, -p_opp) |> 
  bind_rows(cat_total) |> 
  gt(rowname_col = "pitch_class") |> 
  tab_header("Observed Challenge Probabilities by Pitch Class - Catchers",
             subtitle = "C = Challenge, GC = Good Challenge, BC = Bad Challenge") |> 
  fmt_number(columns = c(p_chal, p_good, p_bad), decimals = 3) |> 
  fmt_integer(columns = c(n, n_chal, n_good, n_bad)) |> 
  cols_label(n = "N", n_chal = "C", p_chal = "P(C)", p_good = "P(B|C)", 
             n_bad = "BC", p_bad = "P(BC)") |> 
  tab_stubhead("Pitch Class") |> 
  tab_style(
    style = list(cell_text(weight = "bold"), cell_borders(sides = "top", weight = px(2))),
    locations = list(cells_body(rows = pitch_class == "Total"),
                     cells_stub(rows = pitch_class == "Total"))
  )
# Is a changeup or fastball easier to challenge than any breaking?

# Three group bys: Take the full data, group_by actual ball or strike variable, 
# the call, and then the challenge status
challenge_table <- modeldata2 |> 
  filter(!is.na(pitch_class)) |> 
  group_by(pitch_class, truth, call, challenge) |> 
  summarise(n = n(), .groups = "drop_last") |> 
  mutate(pct = n / sum(n))

modeldata_hitter |> 
  summarise(
    n = n(),
    yes = sum(challenge_hitter),
    p_chal = yes/n,
    no = n - yes,
    s_true = sum(truth == "strike"),
    b_true = sum(truth == "ball"),
    q1 = sum(challenge_hitter & truth == "ball"),
    q2 = sum(challenge_hitter & truth == "strike"),
    q3 = sum(!challenge_hitter & truth == "ball"),
    q4 = sum(!challenge_hitter & truth == "strike"),
    good_chal = q1/yes,
    bad_chal = 1 - good_chal
  ) |> 
  select(n, yes, good_chal, bad_chal)

modeldata_catcher |> 
  summarise(
    n = n(),
    yes = sum(challenge_catcher),
    no = n - yes,
    s_true = sum(truth == "strike"),
    b_true = sum(truth == "ball"),
    q1 = sum(challenge_catcher & truth == "strike"),
    q2 = sum(challenge_catcher & truth == "ball"),
    q3 = sum(!challenge_catcher & truth == "strike"),
    q4 = sum(!challenge_catcher & truth == "ball")
  )


## Looking at challenges
modeldata_hitter |> 
  filter(miss_dist <= 0) |>
  group_by(challenges_remaining, inning) |> 
  summarise(n = n(),
            p = sum(challenge_hitter)/n) |> 
  ggplot(aes(x = inning, y = p, color = as.factor(challenges_remaining))) +
  geom_line()

# We want to find how many times a team has challenged in each game
# Challenges happen more later in the game and also when a team only has one challenge_remaining

# NO BINS

## Salary and Service Time EDA
salary <- modeldata_hitter |> 
  group_by(batter) |> 
  summarise(salary = mean(salary), p_chal = mean(challenge_hitter), n = n()) |> 
  filter(n >= 120)

ggplot(salary, aes(x = salary, y = p_chal)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Salary", y = "P(challenge_hitter)") +
  theme_minimal()

st <- modeldata_hitter |> 
  group_by(batter) |> 
  summarise(service = mean(service_years), p_chal = mean(challenge_hitter), n = n())

ggplot(st, aes(x = log(service), y = p_chal)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Service time (years)", y = "P(challenge_hitter)") +
  theme_minimal()

ggplot(st, aes(x = log(service))) + geom_density()

modeldata_hitter |> 
  distinct(player_name.y, .keep_all = TRUE) |> 
  ggplot(aes(x = service_years, y = log(salary))) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = c(3, 6), linetype = "dashed") +
  labs(x = "Service time (years)", y = "log-Salary") + 
  theme_minimal()

### SPLINE STUFF
# Looking at whether or not we need the splines at the degree they're at
mod3_data <- modeldata_hitter |> filter(zone > 9)

mod3_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_hitter)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod3_fits <- list(
  linear = glm(challenge_hitter ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod3_data)
)
for (k in 2:6) {
  mod3_fits[[paste0("ns", k)]] <- update(mod3_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

# To go with the simplest model possible, BIC gives a better idea of that
sapply(mod3_fits, BIC)

#ns2 for mod3

mod4_data <- modeldata_hitter |> filter(miss_dist <= 0)

mod4_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_hitter)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod4_fits <- list(
  linear = glm(challenge_hitter ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod4_data)
)
for (k in 2:6) {
  mod4_fits[[paste0("ns", k)]] <- update(mod4_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod4_fits, BIC)
# ns2 again

# Catcher models
mod5_data <- modeldata_catcher |> filter(zone %in% c(1:9))

mod5_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_catcher)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod5_fits <- list(
  linear = glm(challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod5_data)
)
for (k in 2:6) {
  mod5_fits[[paste0("ns", k)]] <- update(mod5_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod5_fits, BIC) # The linear term wins out here.

mod6_data <- modeldata_catcher |> filter(zone > 9)

mod6_data |> 
  mutate(bin = cut_number(miss_dist, 25)) |> 
  group_by(bin) |> 
  summarise(miss_dist = mean(miss_dist), p = mean(challenge_catcher)) |> 
  ggplot(aes(miss_dist, qlogis(p))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(y = "Observed log-odds of challenge")

mod6_fits <- list(
  linear = glm(challenge_catcher ~ miss_dist + delta + scale(bat_score_diff) + challenges_remaining,
               family = binomial, data = mod6_data)
)
for (k in 2:6) {
  mod6_fits[[paste0("ns", k)]] <- update(mod6_fits$linear, . ~ . - miss_dist + ns(miss_dist, k))
}

sapply(mod6_fits, BIC) # The linear term wins out here as well.

modeldata_hitter |> 
  mutate(service_time_rd = round(service_time, 0)) |> 
  group_by(service_time_rd) |> 
  summarise(mn = mean(challenge_hitter)) |> 
  ggplot(aes(x = service_time_rd, y = mn)) +
  geom_point() + 
  geom_line() + 
  geom_smooth()

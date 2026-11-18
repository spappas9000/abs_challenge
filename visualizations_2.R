library(tidyverse)
library(lme4)
library(gt)
dir.create("plots", showWarnings = FALSE)

# Replacement for broom.mixed::tidy(): fixed effects, Wald SEs, p-values, 95% CIs
tidy_glmer <- function(m) {
  b  <- fixef(m)
  se <- sqrt(diag(as.matrix(vcov(m))))
  tibble(term = names(b), estimate = unname(b), std.error = unname(se)) |>
    mutate(statistic = estimate / std.error,
           p.value   = 2 * pnorm(-abs(statistic)),
           conf.low  = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}

# Table of every model file
cells <- tibble(
  cell  = c("Hitter, Should", "Hitter, Should Not", "Catcher, Should", "Catcher, Should Not"),
  stem  = c("good_challenge_hitter", "bad_challenge_hitter", "good_challenge_catcher", "bad_challenge_catcher"),
  start = c(11, 16, 21, 26),
  wtag  = c("bwar", "bwar", "fwar", "fwar")
)
models <- cells |>
  crossing(tibble(i = 0:4, measure = c("Base", "Salary", "Service Time", "Career WAR", "Bat/Field WAR"))) |>
  mutate(suffix = c("", "_sal", "_serv", "_war", "")[i + 1],
         suffix = if_else(i == 4, paste0("_", wtag), suffix),
         file   = paste0("mod", start + i, "_", stem, suffix)) |>
  filter(file.exists(file.path("models", paste0(file, ".RDS"))))

# Load each model once
fits <- models |>
  mutate(res = map(file, \(f) {
    m <- readRDS(file.path("models", paste0(f, ".RDS")))
    out <- list(tidy = tidy_glmer(m), AIC = AIC(m), BIC = BIC(m))
    rm(m); gc(); out
  }))
coefs   <- fits |> mutate(tidy = map(res, "tidy")) |> select(cell, measure, tidy) |> unnest(tidy)
fit_tab <- fits |> transmute(cell, measure, AIC = map_dbl(res, "AIC"), BIC = map_dbl(res, "BIC"))

# The four base models
mod11 <- readRDS("models/mod11_good_challenge_hitter.RDS")
mod16 <- readRDS("models/mod16_bad_challenge_hitter.RDS")
mod21 <- readRDS("models/mod21_good_challenge_catcher.RDS")
mod26 <- readRDS("models/mod26_bad_challenge_catcher.RDS")
base  <- list(`Hitter, Should` = mod11, `Hitter, Should Not` = mod16,
              `Catcher, Should` = mod21, `Catcher, Should Not` = mod26)


coefs |> filter(cell == "Hitter, Should", measure == "Service Time", str_detect(term, "service"))


# SLIDE 10: Model Frameworks (sample-size table)
tab_n <- imap_dfr(base, ~ tibble(Subset = .y, Opportunities = nobs(.x),
                                 Challenged = sum(getME(.x, "y")))) |>
  mutate(`% challenged` = Challenged / Opportunities) |>
  gt() |>
  fmt_number(c(Opportunities, Challenged), decimals = 0) |>
  fmt_percent(`% challenged`, decimals = 1) |>
  tab_options(table.font.size = 16)
tab_n          # screenshot from the Viewer pane

# SLIDE 11a: Model Results (game state)
gs <- coefs %>% 
  filter(measure == "Base", term %in% c("delta", "challenges_remaining")) |>
  mutate(k = if_else(term == "delta", 0.1, 1),
         across(c(estimate, conf.low, conf.high), ~ exp(.x * k)),
         term = recode(term, delta = "+0.1 runs at stake",
                       challenges_remaining = "+1 challenge remaining"))

p_gs <- ggplot(gs, aes(estimate, fct_rev(cell), color = conf.low > 1 | conf.high < 1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1, size = 0.8) +
  geom_text(aes(label = sprintf("%.2f", estimate)), vjust = -1.2, color = "black", size = 4) +
  facet_wrap(~ term, scales = "free_x") +
  scale_color_manual(values = c(`TRUE` = "#4D1434", `FALSE` = "grey60"), guide = "none") +
  labs(x = "Odds ratio (95% CI)", y = NULL) +
  theme_minimal(base_size = 15)
p_gs
ggsave("plots/game_state_effects.png", p_gs, width = 11, height = 4.5, dpi = 300)

# Model Results (seniority)
sen <- coefs |>
  filter(str_detect(term, "salary|service|war_career")) |>
  mutate(across(c(estimate, conf.low, conf.high), exp),
         measure = factor(measure, levels = c("Salary", "Service Time", "Career WAR", "Bat/Field WAR")))

p_sen <- ggplot(sen, aes(estimate, fct_rev(measure), color = conf.low > 1 | conf.high < 1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1, size = 0.8) +
  geom_text(aes(label = sprintf("%.2f", estimate)), vjust = -1.2, color = "black", size = 3.8) +
  facet_wrap(~ cell, nrow = 1) +
  scale_color_manual(values = c(`TRUE` = "#4D1434", `FALSE` = "grey60"), guide = "none") +
  labs(x = "Odds ratio per 1 SD increase (95% CI)", y = NULL) +
  theme_minimal(base_size = 14)
p_sen
ggsave("plots/seniority_effects.png", p_sen, width = 14, height = 4.5, dpi = 300)

# : Model Results (model comparison)
tab_fit <- fit_tab |>
  group_by(cell) |>
  mutate(`ΔAIC vs base` = AIC - AIC[measure == "Base"],
         `ΔBIC vs base` = BIC - BIC[measure == "Base"]) |>
  ungroup() |>
  select(-AIC, -BIC) |>
  gt(groupname_col = "cell", rowname_col = "measure") |>
  fmt_number(starts_with("Δ"), decimals = 1) |>
  tab_options(table.font.size = 14)
tab_fit        # screenshot from the Viewer pane

# Combining Simulation and Model Results
# (a) League success rate when a challenge is made
p_lg <- tibble(
  Role = c("Hitter", "Catcher"),
  good = c(sum(getME(mod11, "y")), sum(getME(mod21, "y"))),
  bad  = c(sum(getME(mod16, "y")), sum(getME(mod26, "y")))
) |> mutate(p_lg = good / (good + bad))
p_lg           # expect about 0.48 for hitters and 0.59 for catchers

# (b) Player success probability
names <- read_csv("data/contract_salary_2026.csv", show_col_types = FALSE) |>
  transmute(player = as.character(player_id), player_name)

player_pind <- function(good, bad, id) {
  summ <- function(m, tag) tibble(player = as.character(m@frame[[id]]),
                                  y = getME(m, "y"), p = fitted(m)) |>
    group_by(player) |>
    summarise("{tag}_ch" := sum(y), "{tag}_exp" := sum(p))
  full_join(summ(good, "good"), summ(bad, "bad"), by = "player") |>
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
           challenges  = good_ch + bad_ch,
           p_ind_raw   = good_ch / challenges,
           p_ind_model = good_exp / (good_exp + bad_exp)) |>
    left_join(names, by = "player")
}
pind <- bind_rows(Hitter  = player_pind(mod11, mod16, "batter"),
                  Catcher = player_pind(mod21, mod26, "fielder_2"), .id = "Role") |>
  filter(challenges >= 10)

# Figure
p_pind <- ggplot(pind, aes(p_ind_model)) +
  geom_histogram(binwidth = 0.025, fill = "#4D1434", color = "white") +
  geom_vline(data = p_lg, aes(xintercept = p_lg), linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Role, ncol = 1) +
  labs(x = "Player success probability when challenging (P_ind)", y = "Players",
       caption = "Dashed line = league rate (p_lg). Players with 10+ challenges.") +
  theme_minimal(base_size = 15)
ggsave("plots/pind_distribution.png", p_pind, width = 8, height = 6, dpi = 300)

# Top and bottom 3 per role
pind |> group_by(Role) |> arrange(desc(p_ind_model)) |>
  slice(c(1:3, (n() - 2):n())) |>
  select(Role, player_name, challenges, p_ind_raw, p_ind_model)

write_csv(pind, "plots/player_pind.csv")
# Player dataset for the ABS challenge project
#
# Builds one row per MLB player for the season with experience info from the
# MLB Stats API and salary / service time from the salary sheet.
# Run from the abs_challenge project root (same as data.R and get_statcast.R).
# Inputs:  data/MLB-Salaries.xls.csv, data/salary_overrides.csv
# Output:  data/players_2026.csv (and data/salary_unmatched.csv)
#
# player_id is the MLBAM ID, which matches `batter` and `fielder_2` in the
# Statcast data, so this file joins directly onto the model data.

library(tidyverse)
library(jsonlite)

season     <- 2026
salary_csv <- "data/MLB-Salaries.csv"
api        <- "https://statsapi.mlb.com/api/v1"

data_dir  <- "data"
cache_dir <- file.path(data_dir, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)


# Helpers 

# Download a file once and keep it in data/cache/ so reruns don't hit the API
cached_json <- function(url, file) {
  path <- file.path(cache_dir, file)
  if (!file.exists(path)) {
    download.file(url, path, quiet = TRUE, mode = "wb")
    Sys.sleep(1)
  }
  fromJSON(path, simplifyVector = FALSE)
}

# Put names in a common format so the two sources match,
# e.g. "Guerrero Jr., Vladimir" and "Vladimir Guerrero Jr." -> "vladimir guerrero"
clean_name <- function(x) {
  x <- str_remove_all(x, "\\*")
  x <- if_else(str_detect(x, ","),
               paste(str_trim(str_remove(x, "^[^,]*,")), str_extract(x, "^[^,]*")),
               x)
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_remove_all("[^a-z ]") |>
    str_remove_all("\\b(jr|sr|ii|iii|iv|[a-z])\\b") |>   # suffixes and middle initials
    str_squish()
}

# Innings pitched come as "45.2" = 45 innings and 2 outs
ip_to_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  floor(x) + round((x %% 1) * 10) / 3
}


# MLB players

people <- cached_json(paste0(api, "/sports/1/players?season=", season),
                      paste0("sports_players_", season, ".json"))$people

field <- function(...) map_chr(people, \(p) as.character(pluck(p, ..., .default = NA)))

players <- tibble(
  player_id   = as.integer(field("id")),
  player_name = field("fullName"),
  first_name  = coalesce(field("useName"), field("firstName")),
  last_name   = field("lastName"),
  position    = field("primaryPosition", "abbreviation"),
  bats        = field("batSide", "code"),
  throws      = field("pitchHand", "code"),
  birth_date  = as.Date(field("birthDate")),
  age         = as.integer(field("currentAge")),
  debut_date  = as.Date(field("mlbDebutDate"))
) |>
  distinct(player_id, .keep_all = TRUE)


# Career at-bats and innings before this season (for rookie status)

batches <- split(players$player_id, ceiling(seq_along(players$player_id) / 100))
hydrate <- URLencode("stats(group=[hitting,pitching],type=[yearByYear])", reserved = TRUE)

career <- map(batches, \(ids) {
  cached_json(paste0(api, "/people?personIds=", paste(ids, collapse = ","), "&hydrate=", hydrate),
              paste0("stats_", min(ids), "_", max(ids), ".json"))$people
}, .progress = TRUE) |>
  list_flatten() |>
  map(\(p) {
    map(p$stats, \(s) {
      map(s$splits, \(sp) {
        if (!is.null(sp$sport$id) && sp$sport$id != 1) return(NULL)   # MLB seasons only
        tibble(player_id = as.integer(p$id),
               group     = s$group$displayName %||% NA_character_,
               year      = as.integer(sp$season),
               ab        = as.numeric(sp$stat$atBats %||% 0),
               ip        = ip_to_num(sp$stat$inningsPitched %||% "0"))
      }) |> list_rbind()
    }) |> list_rbind()
  }) |>
  list_rbind() |>
  filter(year < season) |>
  group_by(player_id) |>
  summarise(prior_ab = sum(ab[group == "hitting"]),
            prior_ip = sum(ip[group == "pitching"]),
            .groups  = "drop")


# 3. Salary sheet 
# Columns: Player ("Last, First", * = salary discounted for deferrals),
# Pos'n, MLS (service time as years.days), 2026 (salary)

salaries <- read_csv(salary_csv, col_types = cols(.default = "c")) |>
  set_names(c("sheet_name", "sheet_pos", "mls", "salary")) |>
  mutate(
    salary_deferred = str_detect(sheet_name, "\\*"),
    salary          = na_if(parse_number(salary), 0),       # $0 = no 2026 salary listed
    service_time    = suppressWarnings(as.numeric(mls)),
    service_years   = floor(service_time) + round((service_time %% 1) * 1000) / 172,  # 172 days = 1 year
    sheet_pitcher   = str_detect(sheet_pos, "hp"),
    name_key        = clean_name(sheet_name),
    last_key        = clean_name(str_extract(sheet_name, "^[^,]*")),
    first_key       = clean_name(str_remove(sheet_name, "^[^,]*,"))
  ) |>
  select(-mls)


# 4. Match salaries to MLB players
# The sheet has no player IDs, so names are matched in passes. Each pass only
# accepts a match when it points to exactly one player on each side; when two
# players share a name, pitcher vs. hitter is used to tell them apart.
#   Pass 1: full name                      ("juan soto")
#   Pass 2: last name + first initial      ("soto j"; catches Mike/Michael, Zach/Zack)
#   Pass 3: data/salary_overrides.csv     (sheet_name, sheet_pos, player_id) for anything left

players <- players |>
  left_join(career, by = "player_id") |>
  mutate(name_key    = clean_name(player_name),
         initial_key = paste(clean_name(last_name), str_sub(clean_name(first_name), 1, 1)),
         is_pitcher  = position == "P")

salaries <- salaries |>
  mutate(row = row_number(),
         initial_key = paste(last_key, str_sub(first_key, 1, 1)))

match_on <- function(sheet_rows, mlb_rows, key) {
  sheet_rows |>
    inner_join(select(mlb_rows, player_id, all_of(key), is_pitcher),
               by = key, relationship = "many-to-many") |>
    filter(n() == 1 | sheet_pitcher == is_pitcher, .by = all_of(key)) |>
    filter(n() == 1, .by = player_id) |>
    filter(n() == 1, .by = row)
}

pass1 <- match_on(salaries, players, "name_key")
pass2 <- match_on(filter(salaries, !row %in% pass1$row),
                  filter(players, !player_id %in% pass1$player_id), "initial_key")

matches <- bind_rows(pass1 |> mutate(match_type = "full name"),
                     pass2 |> mutate(match_type = "last name + initial"))

if (file.exists("data/salary_overrides.csv")) {
  overrides <- read_csv("data/salary_overrides.csv", col_types = cols(player_id = "i", .default = "c")) |>
    inner_join(salaries, by = c("sheet_name", "sheet_pos")) |>
    mutate(match_type = "manual")
  matches <- matches |>
    filter(!row %in% overrides$row, !player_id %in% overrides$player_id) |>
    bind_rows(overrides)
}

matched_rows <- matches$row
matches <- select(matches, player_id, salary, salary_deferred,
                  service_time, service_years, match_type)

# Anything with a salary that still didn't match, with a hint for fixing it by hand
unmatched <- salaries |>
  filter(!row %in% matched_rows, !is.na(salary)) |>
  mutate(possible_matches = map2_chr(last_key, first_key, \(last, first) {
    # MLB players with the same last name and first initial; "none" usually
    # means he didn't play in MLB this season, so he isn't in the pitch data
    hit <- clean_name(players$last_name) == last &
      str_sub(clean_name(players$first_name), 1, 1) == str_sub(first, 1, 1)
    if (!any(hit)) return("none")
    paste0(players$player_name[hit], " (", players$player_id[hit], ")", collapse = "; ")
  })) |>
  select(sheet_name, sheet_pos, salary, service_time, possible_matches) |>
  arrange(desc(salary))
write_csv(unmatched, file.path(data_dir, "salary_unmatched.csv"))
message(nrow(unmatched), " salaried players still unmatched -> data/salary_unmatched.csv")
print(count(matches, match_type))


# 5. Final dataset

players <- players |>
  left_join(matches, by = "player_id") |>
  mutate(
    across(c(prior_ab, prior_ip), \(x) replace_na(x, 0)),
    salary_imputed = is.na(salary), # TRUE = no salary found, filled with the minimum
    salary         = coalesce(salary, 780000),
    role = case_when(position == "C"   ~ "Catcher",
                     position == "P"   ~ "Pitcher",
                     position == "TWP" ~ "Two-way",
                     TRUE              ~ "Position player"),
    years_since_debut = season - as.integer(format(debut_date, "%Y")),
    
    # MLB rookie rule: no more than 130 AB and 50 IP before this season
    rookie = prior_ab <= 130 & prior_ip <= 50,
    
    # Rookies missing from the salary sheet are assumed to have no service time
    service_imputed = is.na(service_time) & rookie,
    service_time    = if_else(service_imputed, 0, service_time),
    service_years   = if_else(service_imputed, 0, service_years),
    
    experience = case_when(rookie                 ~ "rookie",
                           years_since_debut <= 3 ~ "early (1-3 yrs)",
                           years_since_debut <= 6 ~ "mid (4-6 yrs)",
                           years_since_debut >  6 ~ "veteran (7+ yrs)"),
    
    # Contract stage from service time when the sheet has it, otherwise
    # estimated from years since debut. Super Two players aren't separated out.
    years_for_stage = coalesce(service_years, years_since_debut),
    contract_stage  = case_when(years_for_stage < 3 ~ "pre-arbitration",
                                years_for_stage < 6 ~ "arbitration-eligible",
                                years_for_stage >= 6 ~ "free-agent eligible"),
    contract_stage_source = if_else(is.na(service_years), "debut year", "service time")
  ) |>
  select(player_id, player_name, role, position, bats, throws, age, birth_date,
         debut_date, years_since_debut, prior_ab, prior_ip, rookie, experience,
         service_time, service_years, service_imputed, contract_stage, contract_stage_source,
         salary, salary_deferred, salary_imputed, match_type)

write_csv(players, file.path(data_dir, paste0("contract_salary_", season, ".csv")))

stopifnot(!anyDuplicated(players$player_id))
message(nrow(players), " players written; ", sum(!players$salary_imputed), " salaries from the sheet, ",
        sum(players$salary_imputed), " filled with 780000; ",
        sum(is.na(players$service_time)), " still missing service time")
print(count(players, role, salary_imputed))


# Joining to the model data

# players <- read_csv("data/players_2026.csv")
# add_prefix <- function(df, prefix) rename_with(df, \(x) paste0(prefix, x), -player_id)
#
# modeldata_hitter <- modeldata_hitter |>
#   mutate(batter = as.integer(batter)) |>
#   left_join(add_prefix(players, "bat_"), by = c("batter" = "player_id"))
#
# modeldata_catcher <- modeldata_catcher |>
#   mutate(fielder_2 = as.integer(fielder_2)) |>
#   left_join(add_prefix(players, "catch_"), by = c("fielder_2" = "player_id"))


view(players)
view(unmatched)

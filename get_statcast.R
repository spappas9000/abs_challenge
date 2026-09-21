# get_statcast.R
# Pretty self-explanatory, this script gets statcast data and writes it out to a 
# .parquet file (490.5MB -> 99.7MB)
#
# IF YOU DON'T HAVE baseballr:
# run `remotes::install_github("BillPetti/baseballr")`

library(tidyverse)
library(baseballr)
library(data.table)
library(arrow)
library(httr)
library(jsonlite)

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
    ),
  aug26 = statcast_bind_rows(start_date = "2026-08-01", end_date = "2026-08-31", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 8,
      across(where(is.character), ~ na_if(., ""))
    ),
  sept26 = statcast_bind_rows(start_date = "2026-09-01", end_date = "2026-09-09", player_type = "pitcher") %>%
    distinct() %>%
    mutate(
      month = 9,
      across(where(is.character), ~ na_if(., ""))
    )
) %>% 
  rbindlist()

write_parquet(statcastlist_26, "data/statcastlist_26.parquet")

game_pks <- unique(statcastlist_26$game_pk)

df <- data.frame()
for (game_id in game_pks){
  url <- paste0('http://statsapi.mlb.com/api/v1.1/game/', game_id, '/feed/live')
  response <- GET(url)
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  umps <- json_data$liveData$boxscore$officials$official
  roles <- json_data$liveData$boxscore$officials$officialType
  
  umps <- cbind(umps, roles) %>%
    filter(roles == "Home Plate") %>%
    mutate(game_pk = game_id) %>%
    select(game_pk, fullName)
  
  df <- rbind(df, umps)
}

write_csv(df, "data/home_plate_umps.csv")

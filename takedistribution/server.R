
library(tidyverse)
library(shiny)
library(shinydashboard)

mlbdata <- readRDS("statcastdata.rds") %>%
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

server = function(input, output, session) {
  
  hitterfunction = reactive({
    mlbdata %>%
      arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
      group_by(game_pk, bat_team) %>%
      mutate(is_strike = description %in% c("called_strike"),
             strikes_total = length(which(description %in% c("called_strike"))),
             strikes_upto = cumsum(is_strike),
             strikesremaining = strikes_total - strikes_upto) %>%
      ungroup() %>%
      filter(outs == input$Outhit_input,
             balls == input$Ballhit_input,
             strikes == input$Strikehit_input)
  })
  
  catcherfunction = reactive({
    mlbdata %>%
      arrange(game_pk, bat_team, -desc(at_bat_number), -desc(pitch_number)) %>%
      group_by(game_pk, bat_team) %>%
      mutate(is_ball = description %in% c("ball", "blocked_ball", "pitchout"),
             balls_total = length(which(description %in% c("ball", "blocked_ball", "pitchout"))),
             balls_upto = cumsum(is_ball),
             ballsremaining = balls_total - balls_upto) %>%
      ungroup() %>%
      filter(outs == input$Outcatch_input,
             balls == input$Ballcatch_input,
             strikes == input$Strikecatch_input)
  })
  
  output$hitplot = renderPlot({
    
    hitterfunction() %>%
      ggplot(aes(x = strikesremaining)) +
      geom_bar() +
      labs(
        title = "Density of Strikes Remaining",
        subtitle = "Data from MLB Advanced Media during the 2026 MLB season up to Wednesday, September 9",
        x = "Strikes Remaining"
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
  })
  
  output$catchplot = renderPlot({
    
    catcherfunction() %>%
      ggplot(aes(x = ballsremaining)) +
      geom_bar() +
      labs(
        title = "Density of Balls Remaining",
        subtitle = "Data from MLB Advanced Media during the 2026 MLB season up to Wednesday, September 9",
        x = "Balls Remaining"
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
  })
  
}

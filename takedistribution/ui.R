
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rsconnect)

print(getwd())
print(list.files())
print(list.files(recursive = TRUE))

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

ui = dashboardPage(
  skin = "purple",
  dashboardHeader(title = span("Distribution of Remaining Takes", style = "font-size: 12px; font-weight: bold")),
  dashboardSidebar(
    minified = F,
    sidebarMenu(
      menuItem("Hitters", tabName = "hitplot", icon = icon("baseball-bat-ball")),
      menuItem("Catchers", tabName = "catchplot", icon = icon("baseball"))
    )
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "hitplot",
        
        fluidRow(
          
          column(
            width = 4,
            numericInput(
              inputId = "Outhit_input",
              label = "Select Outs in Game",
              min = min(mlbdata$outs),
              max = max(mlbdata$outs),
              value = min(mlbdata$outs)
            )
          ),
          
          column(
            width = 4,
            numericInput(
              inputId = "Ballhit_input",
              label = "Select Balls in PA",
              min = min(mlbdata$balls),
              max = max(mlbdata$balls),
              value = min(mlbdata$balls)
            )
          ),
          
          column(
            width = 4,
            numericInput(
              inputId = "Strikehit_input",
              label = "Select Strikes in PA",
              min = min(mlbdata$strikes),
              max = max(mlbdata$strikes),
              value = min(mlbdata$strikes)
            )
          )
          
        ),
        
        plotOutput("hitplot")
      ),
      
      tabItem(
        tabName = "catchplot",
        
        fluidRow(
          
          column(
            width = 4,
            numericInput(
              inputId = "Outcatch_input",
              label = "Select Outs in Game",
              min = min(mlbdata$outs),
              max = max(mlbdata$outs),
              value = min(mlbdata$outs)
            )
          ),
          
          column(
            width = 4,
            numericInput(
              inputId = "Ballcatch_input",
              label = "Select Balls in PA",
              min = min(mlbdata$balls),
              max = max(mlbdata$balls),
              value = min(mlbdata$balls)
            )
          ),
          
          column(
            width = 4,
            numericInput(
              inputId = "Strikecatch_input",
              label = "Select Strikes in PA",
              min = min(mlbdata$strikes),
              max = max(mlbdata$strikes),
              value = min(mlbdata$strikes)
            )
          )
          
        ),
        
        plotOutput("catchplot")
      )
    )
  )
)

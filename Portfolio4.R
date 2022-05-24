# title: "Portfolio 2"
# author: "Logan O'Brien"
# date: "3/13/2022"

library(shiny)
library(tidyverse)
library(plotly)

data <- read_csv('spreadspoke_scores.csv')
team_data <- read_csv('nfl_teams.csv')

#Adding Team IDs to the Main Dataset, Changing SuperBowl for Consistency
for (i in 1:nrow(data)){
  data$home_id[i] = (team_data$team_id[team_data$team_name == data$team_home[i]])
  data$away_id[i] = (team_data$team_id[team_data$team_name == data$team_away[i]])
  if(data$schedule_week[i] == "Superbowl"){
    data$schedule_week[i] = "SuperBowl"
  }
}

#Cleaning Data and Creating New Variables
data <- data %>%
  filter(schedule_season >= 2000 & spread_favorite != 0) %>% 
  mutate(total_pts = score_home + score_away,
         spread = score_home - score_away, 
         winner_id = case_when(
           spread > 0 ~ home_id,
           TRUE ~ away_id),
         fav_win = team_favorite_id == winner_id,
         over_hit = case_when(
           total_pts >= over_under_line ~ 'Over Hit',
           TRUE ~ 'Over Not Hit'),
         spread_hit = case_when(
           fav_win == TRUE & abs(spread) > abs(spread_favorite) ~ 'Spread Covered',
           TRUE ~ 'Spread Not Covered')) %>%
  select(-starts_with("weather")) %>% 
  drop_na()

team_data <- team_data %>% 
  filter(team_name %in% data$team_home)

plot_spread_data <- function(df){
  p <- ggplot(df)+
    geom_histogram(aes(spread_favorite, fill=over_hit))+
    facet_wrap(~spread_hit)+
    labs(title = "Favored Team vs The Over/Under and Spread",
         x = "Favored Team Spread",
         y = "Number of Games", 
         fill = "Over/Under Line Covered")+
    scale_x_reverse()+
    scale_fill_manual(values=c("deepskyblue3","firebrick2"))
  ggplotly(p)
}

plot_num_data <- function(df){
  p <- ggplot(df)+
    geom_point(aes(spread_favorite, over_under_line, color=over_hit))+
    facet_wrap(~spread_hit)+
    labs(x = "Favored Team Spread",
         y = "Over/Under Line", 
         color = "Over/Under Line Covered")+
    scale_x_reverse()+
    scale_color_manual(values=c("deepskyblue3","firebrick2"))
  ggplotly(p)
}

generate_stats <- function(df){
  nrow(df[df$spread_hit == TRUE,])
  nrow(df[df$spread_hit == FALSE,])
}

ui <- fluidPage(
    titlePanel(h2("Analyzing Favored NFL Teams Since 2000", align = "center")),
    h4("Logan O'Brien", align = "center"),
    br(),
    selectInput("fav_team", "Select The Favored Team", team_data$team_name, multiple = FALSE),
    br(),
    textOutput("hit"),
    br(),
    textOutput("not_hit"),
    br(),
    plotlyOutput('spread_plot'),
    br(),
    plotlyOutput('num_plot'),
    br(),
    h4("Check Out More Data on This Favored Team Below", align = "center"),
    tags$ul(
      tags$li(tags$b("Season"), " - The NFL season which this game was played."),
      tags$li(tags$b("Home Team"), " - The NFL Team playing at their home stadium."),
      tags$li(tags$b("Away Team"), " - The NFL team playing on the road."),
      tags$li(tags$b("Winner"), " - The winner of the NFL Game"),
      tags$li(tags$b("Spread Hit"), " - Whether the favored team covered the spread(outscored opponent by at least the given spread)"),
      tags$li(tags$b("Over Hit"), " - Whether the combined score of the game covered the over/under mark")
    ),
    dataTableOutput("team_dt")
)

server <- function(input, output) {
  sample <- reactive({
    data %>% 
      filter((team_favorite_id == team_data$team_id[team_data$team_name == input$fav_team]))
  })
  out_sample <- reactive({
    data %>% 
      filter((team_favorite_id == team_data$team_id[team_data$team_name == input$fav_team])) %>% 
      select(Season = schedule_season,
             `Home Team` = team_home,
             `Away Team` = team_away,
             `Winner` = winner_id,
             `Spread Hit` = spread_hit,
             `Over Hit` = over_hit)
  })
  
  output$spread_plot <- renderPlotly(plot_spread_data(sample()))
  output$num_plot <- renderPlotly(plot_num_data(sample()))
  
  output$hit <- renderText({paste("The",
                                 input$fav_team,
                                 "covered the spread",
                                 100*round(nrow(sample()[sample()$spread_hit == "Spread Covered",]) / nrow(sample()),3),
                                 "% of games since 2000")})
  output$not_hit <- renderText({paste("The",
                                       input$fav_team,
                                       "covered the over/under line",
                                        100*round(nrow(sample()[sample()$over_hit == "Over Hit",]) / nrow(sample()),3),
                                       "% of games since 2000")})
  output$team_dt <- renderDataTable({out_sample()})
}

shinyApp(ui = ui, server = server)

library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(shinycustomloader)
source("source/stats.R")
playerNames <- as.character(players$playerID)
names(playerNames) <- players$playerName
library(shiny)

# Define UI for application that draws a histogram
ui<-navbarPage(title="NBA Season",inverse = TRUE,
               tabPanel("Player Performance",
                        sidebarPanel(id="sidebar",
                                     withLoader(uiOutput("player_photo"), type = "html"),
                                     selectInput(
                                       "playerID", "Player:",
                                       choices = playerNames
                                     ),
                                     actionButton("select_player", "Send It"), width = 2
                        )
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Stats",withLoader(DT::dataTableOutput("stats"), type = "html"), withLoader(DT::dataTableOutput("stats_avg"), type = "html"), 
                                      fluidRow(column(3, uiOutput("yearid")), column(3, uiOutput("mp_pct"))), 
                                      withLoader(plotOutput("percentile_graph"), type = "html"))
                 ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  player_id <- reactiveVal(playerNames[1])
  
  observeEvent(input$select_player, {
    new_player= input$playerID
    player_id(new_player)
  })
  
  observe({
    updateSelectInput(session, 'playerID', "Player:", choices = playerNames, player_id())
  })
  output$player_photo<-renderUI(tags$img(src = paste("https://d2cwpp38twqe55.cloudfront.net/req/201811081/images/players/",player_id(),".jpg", sep = ""), height=285, width=205,alt = "photo")
  )
  output$stats<-DT::renderDataTable({
    player_df<-stats_players %>% filter(playerID == player_id()) %>% ungroup() %>%
      select(year, age, teamid, pos, mp_pct, usg_pct, ts_pct, fg3a_per_fga_pct, 
             ast_pct, orb_pct, drb_pct, tov_pct, stl_pct, blk_pct,
             bpm, vorp, raw_bpm, adj_war) %>% mutate(
               mp_pct = format(round(mp_pct*100, digits = 1), nsmall = 1),
               usg_pct = format(round(usg_pct, digits = 1), nsmall = 1), 
               ts_pct = format(round(ts_pct, digits = 3), nsmall = 1), 
               fg3a_per_fga_pct = format(round(fg3a_per_fga_pct, digits = 3), nsmall = 1), 
               ast_pct = format(round(ast_pct, digits = 1), nsmall = 1), 
               orb_pct = format(round(orb_pct, digits = 1), nsmall = 1), 
               drb_pct = format(round(drb_pct, digits = 1), nsmall = 1), 
               tov_pct = format(round(tov_pct, digits = 1), nsmall = 1), 
               stl_pct = format(round(stl_pct, digits = 1), nsmall = 1), 
               blk_pct = format(round(blk_pct, digits = 1), nsmall = 1), 
               bpm = format(round(bpm, digits = 1), nsmall = 1), 
               vorp = format(round(vorp, digits = 1), nsmall = 1), 
               raw_bpm = format(round(raw_bpm, digits = 3), nsmall = 1),
               adj_war = format(round(adj_war, digits = 3), nsmall = 1)
             )
    DT::datatable(player_df, rownames = FALSE, options = list(dom = 't',ordering = F, pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  output$stats_avg<-DT::renderDataTable({
    position<-stats_players %>% group_by(playerID) %>% filter(playerID == player_id() & year == max(year)) %>% summarise(
      year = max(year), 
      pos = pos[1]
    )
    player_df<-stats_players  %>% filter(year == position$year & pos == position$pos ) %>% 
      select(playerID, year, age, teamid, pos, mp, mp_pct, usg_pct, ts_pct, fg3a_per_fga_pct, 
             ast_pct, orb_pct, drb_pct, tov_pct, stl_pct, blk_pct,
             bpm, vorp, raw_bpm, adj_war) %>% group_by(pos) %>% summarise(
               year = max(year), 
               age = '-',
               teamid = '-', 
               mp_pct = format(round(weighted.mean(mp_pct*100, mp), digits = 1), nsmall = 1),
               usg_pct = format(round(weighted.mean(usg_pct, mp), digits = 1), nsmall = 1), 
               ts_pct = format(round(weighted.mean(ts_pct, mp, na.rm = T), digits = 3), nsmall = 3), 
               fg3a_per_fga_pct = format(round(weighted.mean(fg3a_per_fga_pct, mp, na.rm = T), digits = 3), nsmall = 3), 
               ast_pct = format(round(weighted.mean(ast_pct, mp), digits = 1), nsmall = 1), 
               orb_pct = format(round(weighted.mean(orb_pct, mp), digits = 1), nsmall = 1), 
               drb_pct = format(round(weighted.mean(drb_pct, mp), digits = 1), nsmall = 1), 
               tov_pct = format(round(weighted.mean(tov_pct, mp, na.rm = T), digits = 1), nsmall = 1), 
               stl_pct = format(round(weighted.mean(stl_pct, mp), digits = 1), nsmall = 1), 
               blk_pct = format(round(weighted.mean(blk_pct, mp), digits = 1), nsmall = 1), 
               bpm = format(round(weighted.mean(bpm, mp), digits = 1), nsmall = 1), 
               vorp = format(round(weighted.mean(vorp, mp), digits = 1), nsmall = 1), 
               raw_bpm = format(round(weighted.mean(raw_bpm, mp, na.rm = T), digits = 3), nsmall = 3),
               adj_war = format(round(weighted.mean(adj_war, mp, na.rm = T), digits = 3), nsmall = 3)
             )
    DT::datatable(player_df, rownames = FALSE, options = list(dom = 't',ordering = F, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  output$yearid <- renderUI({
    selectInput("year_input", "Season:", choices = stats_players$year[stats_players$playerID == player_id()], selected = max(stats_players$year[stats_players$playerID == player_id()]))
  })
  output$mp_pct <- renderUI({
    selectInput("mp_pct_input", "MP:", choices = seq(0, 1, by = 0.1), selected = mean(seq(0, 1, by = 0.1)))
  })
  output$percentile_graph <- renderPlot({
    w_stats_players <- stats_players %>% group_by(year, playerName, playerID, age) %>% summarise(
      pos = pos[1],
      mp_w = sum(mp, na.rm = T),
      mp_pct = weighted.mean(mp_pct, mp, na.rm = T),
      adj_war = sum(adj_war, na.rm = T), 
      raw_bpm = weighted.mean(raw_bpm, mp, na.rm = T),
      vorp = sum(vorp, na.rm = T), 
      bpm = weighted.mean(bpm, mp, na.rm = T),
      ast_pct = weighted.mean(ast_pct, mp, na.rm = T), 
      fg3a_per_fga_pct = weighted.mean(fg3a_per_fga_pct, mp, na.rm = T), 
      ts_pct = weighted.mean(ts_pct, mp, na.rm = T), 
      usg_pct = weighted.mean(usg_pct, mp, na.rm = T), 
      tov_pct = weighted.mean(tov_pct, mp, na.rm = T), 
      orb_pct = weighted.mean(orb_pct, mp, na.rm = T),
      drb_pct = weighted.mean(drb_pct, mp, na.rm = T), 
      stl_pct = weighted.mean(stl_pct, mp, na.rm = T), 
      blk_pct = weighted.mean(blk_pct, mp, na.rm = T)
    )
    minute_req <- w_stats_players %>% filter(playerID != player_id() & mp_pct >= input$mp_pct_input & year == input$year_input)
    player_chosen <- w_stats_players %>% filter(playerID == player_id() & year == input$year_input)
    w_stats_players <- rbind(minute_req, player_chosen)
    jbut<-w_stats_players %>% 
      select(playerID, year, age, pos, usg_pct, ts_pct, fg3a_per_fga_pct, 
             ast_pct, orb_pct, drb_pct, tov_pct, stl_pct, blk_pct, raw_bpm, adj_war,
             bpm, vorp) %>% group_by(year, pos) %>% mutate(
               usg_pct = round(cume_dist(usg_pct)*100), 
               ts_pct = round(cume_dist(ts_pct)*100), 
               fg3a_per_fga_pct = round(cume_dist(fg3a_per_fga_pct)*100), 
               ast_pct = round(cume_dist(ast_pct)*100), 
               orb_pct = round(cume_dist(orb_pct)*100), 
               drb_pct = round(cume_dist(drb_pct)*100), 
               tov_pct = round(cume_dist(tov_pct)*100), 
               stl_pct = round(cume_dist(stl_pct)*100), 
               blk_pct = round(cume_dist(blk_pct)*100), 
               bpm = round(cume_dist(bpm)*100), 
               vorp = round(cume_dist(vorp)*100), 
               raw_bpm = round(cume_dist(raw_bpm)*100),
               adj_war = round(cume_dist(adj_war)*100)
             ) %>% filter(playerID == player_id()) %>% ungroup() %>% select(year, usg_pct, ts_pct, fg3a_per_fga_pct, 
                                                                               ast_pct, orb_pct, drb_pct, tov_pct, stl_pct, blk_pct,
                                                                               bpm, vorp, raw_bpm, adj_war)
    playerGraph<-melt(jbut, id.vars = "year")
    
    playerGraph %>% ggplot(aes(x = variable, y = value, fill = value)) + 
      geom_bar(stat = 'identity') + theme_bw() +
      scale_fill_gradient2(low = 'red', mid = 'orange', high = 'yellow', midpoint = 50) + 
      xlab("Category") + ylab("Percentile") + ggtitle("Percentile Graph")+ geom_hline(aes(yintercept = 50), colour = "red") + scale_y_continuous(breaks = seq(0, 100, by = 10))
    
  })
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)


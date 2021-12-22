

navbarPage(theme = shinytheme("flatly"),
  title = "Football Data",
  tabPanel(
    "Team Analysis",
    fluidPage(
      fluidRow(
        column(6,
               h2("Team Form Analysis"), 
               h5("Data based on most recent Matches"), offset = 3, align = "center")
      ),
      fluidRow(hr()),
      fluidRow(
        tabsetPanel(
          hr(), 
          tabPanel("Eng Prem",
                   fluidRow(
                     column(8,
                            selectInput("select_engprem_team", "Select Team", choices = c("", engpremTeams), width = "40%"),
                            actionButton("get_engprem_team_data", "Get Data", width = "40%", align = "center"),
                            hr(),
                            h3(textOutput("engprem_team")),
                            h3("SRS:"),
                            h2(textOutput("engprem_team_srs") %>% withSpinner()),
                            align = "center", offset = 2)
                     
                   ),
                   fluidRow(hr()),
                   fluidRow(
                     column(6,
                     tabsetPanel(
                     tabPanel(h4("Home"),
                              column(12,
                                     h3("Last 5 Home Matches"),
                                     h4("Home Goal Data"),
                                     gt_output("engprem_h_goal_data"),
                                     h4("W - D - L"),
                                     h3(textOutput("engprem_h_wdl_win") %>% withSpinner()),
                                     gt_output("engprem_last_h_five"),
                                     align = "center")
                     ),
                     tabPanel(h4("Away"),
                              column(12,
                                     h3("Last 5 Away Match data"),
                                     h4("Away Goal Data"),
                                     gt_output("engprem_a_goal_data"),
                                     h4("W - D - L"),
                                     h3(textOutput("engprem_a_wdl_win") %>% withSpinner()),
                                     gt_output("engprem_last_a_five"),
                                     align = "center")
                     ),
                     tabPanel(h4("Last 10 (H&A)"),
                              column(12,
                                     hr(),
                                     h3("Match Table - Last 10 matches (H&A)"),
                                     gt_output("engprem_last_ten"),
                                     align = "center")
                     )
                   ), align = "center"),
                   column(6,
                     h3("Goal Data for Team"),
                     gt_output("eng_prem_team_goal_data"),
                     p("Based on previous games as indicated"),
                     align = "center")
                   ),
                   fluidRow(hr())
                   ),
          tabPanel("Eng Champ",
                   fluidRow(
                     column(8,
                            selectInput("select_engchamp_team", "Select Team", choices = c("", engchampTeams), width = "40%"),
                            actionButton("get_engchamp_team_data", "Get Data", width = "40%", align = "center"),
                            hr(),
                            h3(textOutput("engchamp_team")),
                            h3("SRS:"),
                            h2(textOutput("engchamp_team_srs") %>% withSpinner()),
                            align = "center", offset = 2),
                     column(6,
                            h3("Last 5 Home Matches"),
                            h4("Home Goal Data"),
                            gt_output("engchamp_h_goal_data"),
                            h4("W - D - L"),
                            h3(textOutput("engchamp_h_wdl_win") %>% withSpinner()),
                            gt_output("engchamp_last_h_five"),
                            align = "center"),
                     column(6,
                            h3("Last 5 Away Match data"),
                            h4("Away Goal Data"),
                            gt_output("engchamp_a_goal_data"),
                            h4("W - D - L"),
                            h3(textOutput("engchamp_a_wdl_win") %>% withSpinner()),
                            gt_output("engchamp_last_a_five"),
                            align = "center"),
                     column(8,
                            hr(),
                            h3("Match Table - Last 10 matches (H&A)"),
                            gt_output("engchamp_last_ten"),
                            align = "center", offset = 2)
                   ),
                   fluidRow(hr())
                   
          ),
          tabPanel("Belgium",
                 fluidRow(
                   column(8,
                          selectInput("select_belgium_team", "Select Team", choices = c("", belgiumTeams), width = "40%"),
                          actionButton("get_belgium_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("belgium_team")),
                          h3("SRS:"),
                          h2(textOutput("belgium_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("belgium_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("belgium_h_wdl_win") %>% withSpinner()),
                          gt_output("belgium_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("belgium_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("belgium_a_wdl_win") %>% withSpinner()),
                          gt_output("belgium_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("belgium_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("France",
                 fluidRow(
                   column(8,
                          selectInput("select_france_team", "Select Team", choices = c("", franceTeams), width = "40%"),
                          actionButton("get_france_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("france_team")),
                          h3("SRS:"),
                          h2(textOutput("france_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("france_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("france_h_wdl_win") %>% withSpinner()),
                          gt_output("france_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Matches"),
                          h4("Away Goal Data"),
                          gt_output("france_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("france_a_wdl_win") %>% withSpinner()),
                          gt_output("france_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("france_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),tabPanel("Germany",
                   fluidRow(
                     column(8,
                            selectInput("select_germany_team", "Select Team", choices = c("", germanyTeams), width = "40%"),
                            actionButton("get_germany_team_data", "Get Data", width = "40%", align = "center"),
                            hr(),
                            h3(textOutput("germany_team")),
                            h3("SRS:"),
                            h2(textOutput("germany_team_srs") %>% withSpinner()),
                            align = "center", offset = 2),
                     
                   ),
                   fluidRow(hr()),
                   tabsetPanel(
                     tabPanel("Last 5 Home",
                              fluidRow(
                                column(8,
                                       h3("Last 5 Home Matches"),
                                       h4("Home Goal Data"),
                                       gt_output("germany_h_goal_data"),
                                       h4("W - D - L"),
                                       h3(textOutput("germany_h_wdl_win") %>% withSpinner()),
                                       gt_output("germany_last_h_five"),
                                       align = "center", offset = 2)
                              )),
                     tabPanel("Last 5 Away",
                              fluidRow(
                                column(8,
                                       h3("Last 5 Away Matches"),
                                       h4("Away Goal Data"),
                                       gt_output("germany_a_goal_data"),
                                       h4("W - D - L"),
                                       h3(textOutput("germany_a_wdl_win") %>% withSpinner()),
                                       gt_output("germany_last_a_five"),
                                       align = "center", offset = 2)
                              )),
                     tabPanel("Last 10 Matches",
                              fluidRow(
                                column(8,
                                       hr(),
                                       h3("Match Table - Last 10 matches (H&A)"),
                                       gt_output("germany_last_ten"),
                                       align = "center", offset = 2)
                              ))
                   ),
                   fluidRow(hr())
                   
        ),
        tabPanel("Greece",
                 fluidRow(
                   column(8,
                          selectInput("select_greece_team", "Select Team", choices = c("", greeceTeams), width = "40%"),
                          actionButton("get_greece_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("greece_team")),
                          h3("SRS:"),
                          h2(textOutput("greece_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("greece_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("greece_h_wdl_win") %>% withSpinner()),
                          gt_output("greece_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("greece_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("greece_a_wdl_win") %>% withSpinner()),
                          gt_output("greece_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("greece_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Italy",
                 fluidRow(
                   column(8,
                          selectInput("select_italy_team", "Select Team", choices = c("", italyTeams), width = "40%"),
                          actionButton("get_italy_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("italy_team")),
                          h3("SRS:"),
                          h2(textOutput("italy_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("italy_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("italy_h_wdl_win") %>% withSpinner()),
                          gt_output("italy_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("italy_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("italy_a_wdl_win") %>% withSpinner()),
                          gt_output("italy_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("italy_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Netherlands",
                 fluidRow(
                   column(8,
                          selectInput("select_neth_team", "Select Team", choices = c("", nethTeams), width = "40%"),
                          actionButton("get_neth_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("neth_team")),
                          h3("SRS:"),
                          h2(textOutput("neth_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("neth_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("neth_h_wdl_win") %>% withSpinner()),
                          gt_output("neth_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("neth_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("neth_a_wdl_win") %>% withSpinner()),
                          gt_output("neth_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("neth_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Portugal",
                 fluidRow(
                   column(8,
                          selectInput("select_portugal_team", "Select Team", choices = c("", portugalTeams), width = "40%"),
                          actionButton("get_portugal_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("portugal_team")),
                          h3("SRS:"),
                          h2(textOutput("portugal_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("portugal_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("portugal_h_wdl_win") %>% withSpinner()),
                          gt_output("portugal_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("portugal_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("portugal_a_wdl_win") %>% withSpinner()),
                          gt_output("portugal_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("portugal_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Scotland",
                 fluidRow(
                   column(8,
                          selectInput("select_scotprem_team", "Select Team", choices = c("", scotpremTeams), width = "40%"),
                          actionButton("get_scotprem_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("scotprem_team")),
                          h3("SRS:"),
                          h2(textOutput("scotprem_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("scotprem_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("scotprem_h_wdl_win") %>% withSpinner()),
                          gt_output("scotprem_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("scotprem_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("scotprem_a_wdl_win") %>% withSpinner()),
                          gt_output("scotprem_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("scotprem_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Spain",
                 fluidRow(
                   column(8,
                          selectInput("select_spain_team", "Select Team", choices = c("", spainTeams), width = "40%"),
                          actionButton("get_spain_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("spain_team")),
                          h3("SRS:"),
                          h2(textOutput("spain_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("spain_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("spain_h_wdl_win") %>% withSpinner()),
                          gt_output("spain_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("spain_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("spain_a_wdl_win") %>% withSpinner()),
                          gt_output("spain_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("spain_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
        tabPanel("Turkey",
                 fluidRow(
                   column(8,
                          selectInput("select_turkey_team", "Select Team", choices = c("", turkeyTeams), width = "40%"),
                          actionButton("get_turkey_team_data", "Get Data", width = "40%", align = "center"),
                          hr(),
                          h3(textOutput("turkey_team")),
                          h3("SRS:"),
                          h2(textOutput("turkey_team_srs") %>% withSpinner()),
                          align = "center", offset = 2),
                   column(6,
                          h3("Last 5 Home Matches"),
                          h4("Home Goal Data"),
                          gt_output("turkey_h_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("turkey_h_wdl_win") %>% withSpinner()),
                          gt_output("turkey_last_h_five"),
                          align = "center"),
                   column(6,
                          h3("Last 5 Away Match data"),
                          h4("Away Goal Data"),
                          gt_output("turkey_a_goal_data"),
                          h4("W - D - L"),
                          h3(textOutput("turkey_a_wdl_win") %>% withSpinner()),
                          gt_output("turkey_last_a_five"),
                          align = "center"),
                   column(8,
                          hr(),
                          h3("Match Table - Last 10 matches (H&A)"),
                          gt_output("turkey_last_ten"),
                          align = "center", offset = 2)
                 ),
                 fluidRow(hr())
                 
        ),
      ), type = c("pills")
      )
      )),
  tabPanel(
    "Match Analysis",
    fluidPage(
      fluidRow(
        column(6,
               h2("Match Analysis"), 
               h5("Head to Head anlysis - refer code from Prediction"),
               p("To do - Tabs for each league"),
               offset = 3, align = "center")
      )
      
    )
  ),
  tabPanel(
    title = "League Goals",
    fluidPage(
      fluidRow(
        column(6,
               h2("League Goal Overview"), 
               p("To do: Match overviews, plots"),
               offset = 3, align = "center")
      ),
      fluidRow(hr()),
      fluidRow(
        column(6, 
               selectInput("selected_league", "League", choices = NULL, selected = ""), offset = 3, align = "center")
      ),
      fluidRow(hr(),
               column(12,
                      actionButton("get_league_data", label = "Get Data & Update", width = "50%"),
                      align = "center")),
      fluidRow(hr()),
      fluidRow(
        column(10, style = "background-color:#15488F; color:white;",
               h4(textOutput("league_name")),
               h4(strong("Goal Data")), align = "center", offset = 1)
      ),
      fluidRow(),
      fluidRow(
        column(6, 
               h4("First Half Goals"),
               h3(textOutput("all_fhg") %>% withSpinner()),
               hr(),
               p("Home Goals:"),
               h5(textOutput("fh_home")),
               p("Away Goals:"),
               h5(textOutput("fh_away")),
               p("To Do: Plot"),
               align = "center"),
        column(6, 
               h4("Second Half Goals"),
               h3(textOutput("all_shg") %>% withSpinner()),
               hr(),
               p("Home Goals:"),
               h5(textOutput("sh_home")),
               p("Away Goals:"),
               h5(textOutput("sh_away")),
               p("To Do: Plot"),
               align = "center")
      )
    )
  ),
  tabPanel(
    "Matches Today",
    fluidPage(
      fluidRow(
        column(6,
               h2("Today's Matches"), 
               hr(),
               p("Odds obtained from FlashScore"),
               offset = 3, align = "center")
      ),
      fluidRow(hr()),
      fluidRow(
        column(10,
               gt_output("matches_today"),
               hr(),
               downloadButton("downloadMatches", "Download"),
               hr(),
               p("Odds obtained from FlashScore"),
               offset = 1, align = "center")
      )
    )
  ),
  tabPanel(
    "SPI Ratings",
    fluidPage(
      fluidRow(
        column(6,
               h2("Team SPI Ratings per League"),
               offset = 3, align = "center")
      ),
      fluidRow(
        tabsetPanel(
          br(),
          tabPanel("Eng Prem",
                   fluidRow(
                     column(10,
                            gt_output("spi_engprem_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Eng Champ",
                   fluidRow(
                     column(10,
                            gt_output("spi_engchamp_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Belgium",
                   fluidRow(
                     column(10,
                            gt_output("spi_belgium_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("France",
                   fluidRow(
                     column(10,
                            gt_output("spi_france_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Germany",
                   fluidRow(
                     column(10,
                            gt_output("spi_germany_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Greece",
                   fluidRow(
                     column(10,
                            gt_output("spi_greece_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Italy",
                   fluidRow(
                     column(10,
                            gt_output("spi_italy_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Netherlands",
                   fluidRow(
                     column(10,
                            gt_output("spi_neth_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Portugal",
                   fluidRow(
                     column(10,
                            gt_output("spi_portugal_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Scotland",
                   fluidRow(
                     column(10,
                            gt_output("spi_scotprem_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Spain",
                   fluidRow(
                     column(10,
                            gt_output("spi_spain_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Turkey",
                   fluidRow(
                     column(10,
                            gt_output("spi_turkey_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          )
      )
      
    ),
    fluidRow(
      hr(),
      column(12,
             h4("Data Accreditation:"),
             p(tags$a(href='https://data.fivethirtyeight.com/#soccer-spi', 'FiveThirtyEight', target="_blank"), " - ", tags$a(href='http://creativecommons.org/licenses/by/4.0/', 'Creative Commons Attribution 4.0 International license', target="_blank")), align = "center"),
      br()
    )
  )
  ),
  tabPanel(
    "Goals (O/U) Analysis",
    fluidPage(
      fluidRow(
        column(6,
               h2("Over/Under Goals Analysis per League"),
               offset = 3, align = "center")
      ),
      fluidRow(
        tabsetPanel(
          br(),
          tabPanel("Eng Prem",
                   fluidRow(
                     column(10,
                            gt_output("goals_engprem_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Eng Champ",
                   fluidRow(
                     column(10,
                            gt_output("goals_engchamp_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Belgium",
                   fluidRow(
                     column(10,
                            gt_output("goals_belgium_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("France",
                   fluidRow(
                     column(10,
                            gt_output("goals_france_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Germany",
                   fluidRow(
                     column(10,
                            gt_output("goals_germany_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Greece",
                   fluidRow(
                     column(10,
                            gt_output("goals_greece_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Italy",
                   fluidRow(
                     column(10,
                            gt_output("goals_italy_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Netherlands",
                   fluidRow(
                     column(10,
                            gt_output("goals_neth_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Portugal",
                   fluidRow(
                     column(10,
                            gt_output("goals_portugal_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Scotland",
                   fluidRow(
                     column(10,
                            gt_output("goals_scotprem_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Spain",
                   fluidRow(
                     column(10,
                            gt_output("goals_spain_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          ),
          tabPanel("Turkey",
                   fluidRow(
                     column(10,
                            gt_output("goals_turkey_gt"),
                            hr(),
                            offset = 1, align = "center")
                   )
          )
        )
        
      ),
      fluidRow(
        hr(),
        # column(12,
        # h4("Data Accreditation:"),
        # p(tags$a(href='https://data.fivethirtyeight.com/#soccer-spi', 'FiveThirtyEight', target="_blank"), " - ", tags$a(href='http://creativecommons.org/licenses/by/4.0/', 'Creative Commons Attribution 4.0 International license', target="_blank")), align = "center"),
      br()
    )
  )
),
  collapsible = TRUE
)

function(input, output, session) {
  # Team Data Processing
  # Eng Prem
  
  league_data_engprem <<- render_gt(leagueengpremTblGT)
  
  output$engprem_team <- renderText(input$select_engprem_team)
  output$engprem_team_srs <- renderText(EngpremTeamNameRating)
  # Subset into Team H and A Data
  
  engpremHTeamdata <- reactive({
    engprem %>%
      filter(HomeTeam == input$engprem_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  engpremATeamdata <- reactive({
    engprem %>%
      filter(AwayTeam == input$engprem_team)
  })
  
  engpremTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_engprem_team",
                    choices = c("", engpremTeams))
  observeEvent(input$select_engprem_team,
               {
                 engpremTeam$a <- input$select_engprem_team
                 EngpremTeamName <<- engpremTeam$a
               })
  
  
  
  # Get Home Data
  output$engprem_h_wdl_win <- renderText({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    source("teamAnalysis/engpremAnalysis.R")
    engprem_h_wdl_win <- engpremHWDL
  })
  # Get SRS
  output$engprem_team_srs <- renderText({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    
    engprem_team_srs <- EngpremTeamNameRating
  })
  # Get Away Data
  output$engprem_a_wdl_win <- renderText({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_a_wdl_win <- engpremAWDL
  })  
  
  
  
  # GT Last H Five: engprem_last_h_five
  output$engprem_last_h_five <- render_gt({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_last_h_five <- engpremHDataTbl
  })
  
  # GT Last A Five: engprem_last_a_five
  output$engprem_last_a_five <- render_gt({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_last_a_five <- engpremADataTbl
  })
  # Home Goal Data
  output$engprem_h_goal_data <- render_gt({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_h_goal_data <<- engpremHGoalDataTbl
  })
  # Away Goal Data
  output$engprem_a_goal_data <- render_gt({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_a_goal_data <<- engpremAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$engprem_last_ten <- render_gt({
    print(input$get_engprem_team_data)
    
    if(input$get_engprem_team_data == 0) {
      return()
    }
    engprem_last_ten <- engpremLastTenTbl
  })
  
  # Eng Champ
  
  output$engchamp_team <- renderText(input$select_engchamp_team)
  
  # Subset into Team H and A Data
  
  engchampHTeamdata <- reactive({
    engchamp %>%
      filter(HomeTeam == input$engchamp_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  engchampATeamdata <- reactive({
    engchamp %>%
      filter(AwayTeam == input$engchamp_team)
  })
  
  engchampTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_engchamp_team",
                    choices = c("", engchampTeams))
  observeEvent(input$select_engchamp_team,
               {
                 engchampTeam$a <- input$select_engchamp_team
                 EngchampTeamName <<- engchampTeam$a
               })
  
  # Get Home Data
  output$engchamp_h_wdl_win <- renderText({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    source("teamAnalysis/engchampAnalysis.R")
    engchamp_h_wdl_win <- engchampHWDL
  })
  # Get SRS
  output$engchamp_team_srs <- renderText({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    
    engchamp_team_srs <- EngchampTeamNameRating
  })
  # Get Away Data
  output$engchamp_a_wdl_win <- renderText({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_a_wdl_win <- engchampAWDL
  })  
  
  # GT Last H Five: engchamp_last_h_five
  output$engchamp_last_h_five <- render_gt({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_last_h_five <- engchampHDataTbl
  })
  
  # GT Last A Five: engchamp_last_a_five
  output$engchamp_last_a_five <- render_gt({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_last_a_five <- engchampADataTbl
  })
  
  # Home Goal Data
  output$engchamp_h_goal_data <- render_gt({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_h_goal_data <<- engchampHGoalDataTbl
  })
  # Away Goal Data
  output$engchamp_a_goal_data <- render_gt({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_a_goal_data <<- engchampAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$engchamp_last_ten <- render_gt({
    print(input$get_engchamp_team_data)
    
    if(input$get_engchamp_team_data == 0) {
      return()
    }
    engchamp_last_ten <- engchampLastTenTbl
  })
  
  # Belgium
  
  output$belgium_team <- renderText(input$select_belgium_team)
  
  # Subset into Team H and A Data
  
  belgiumHTeamdata <- reactive({
    belgium %>%
      filter(HomeTeam == input$belgium_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  belgiumATeamdata <- reactive({
    belgium %>%
      filter(AwayTeam == input$belgium_team)
  })
  
  belgiumTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_belgium_team",
                    choices = c("", belgiumTeams))
  observeEvent(input$select_belgium_team,
               {
                 belgiumTeam$a <- input$select_belgium_team
                 BelgiumTeamName <<- belgiumTeam$a
               })
  
  # Get Home Data
  output$belgium_h_wdl_win <- renderText({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    source("teamAnalysis/belgiumAnalysis.R")
    belgium_h_wdl_win <- belgiumHWDL
  })
  # Get SRS
  output$belgium_team_srs <- renderText({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    
    belgium_team_srs <- BelgiumTeamNameRating
  })
  # Get Away Data
  output$belgium_a_wdl_win <- renderText({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_a_wdl_win <- belgiumAWDL
  })  
  
  # GT Last H Five: belgium_last_h_five
  output$belgium_last_h_five <- render_gt({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_last_h_five <- belgiumHDataTbl
  })
  # Home Goal Data
  output$belgium_h_goal_data <- render_gt({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_h_goal_data <<- belgiumHGoalDataTbl
  })
  # Away Goal Data
  output$belgium_a_goal_data <- render_gt({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_a_goal_data <<- belgiumAGoalDataTbl
  })
  
  
  
  # GT Last A Five: belgium_last_a_five
  output$belgium_last_a_five <- render_gt({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_last_a_five <- belgiumADataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$belgium_last_ten <- render_gt({
    print(input$get_belgium_team_data)
    
    if(input$get_belgium_team_data == 0) {
      return()
    }
    belgium_last_ten <- belgiumLastTenTbl
  })
  
  # France
  
  output$france_team <- renderText(input$select_france_team)
  
  # Subset into Team H and A Data
  
  franceHTeamdata <- reactive({
    france %>%
      filter(HomeTeam == input$france_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  franceATeamdata <- reactive({
    france %>%
      filter(AwayTeam == input$france_team)
  })
  
  franceTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_france_team",
                    choices = c("", franceTeams))
  observeEvent(input$select_france_team,
               {
                 franceTeam$a <- input$select_france_team
                 FranceTeamName <<- franceTeam$a
               })
  
  # Get Home Data
  output$france_h_wdl_win <- renderText({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    source("teamAnalysis/franceAnalysis.R")
    france_h_wdl_win <- franceHWDL
  })
  # Get SRS
  output$france_team_srs <- renderText({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    
    france_team_srs <- FranceTeamNameRating
  })
  # Get Away Data
  output$france_a_wdl_win <- renderText({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_a_wdl_win <- franceAWDL
  })  
  
  # GT Last H Five: france_last_h_five
  output$france_last_h_five <- render_gt({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_last_h_five <- franceHDataTbl
  })
  
  # GT Last A Five: france_last_a_five
  output$france_last_a_five <- render_gt({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_last_a_five <- franceADataTbl
  })
  # Home Goal Data
  output$france_h_goal_data <- render_gt({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_h_goal_data <<- franceHGoalDataTbl
  })
  # Away Goal Data
  output$france_a_goal_data <- render_gt({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_a_goal_data <<- franceAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$france_last_ten <- render_gt({
    print(input$get_france_team_data)
    
    if(input$get_france_team_data == 0) {
      return()
    }
    france_last_ten <- franceLastTenTbl
  })
  
  
  # Germany
  
  output$germany_team <- renderText(input$select_germany_team)
  
  # Subset into Team H and A Data
  
  germanyHTeamdata <- reactive({
    germany %>%
      filter(HomeTeam == input$germany_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  germanyATeamdata <- reactive({
    germany %>%
      filter(AwayTeam == input$germany_team)
  })
  
  germanyTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_germany_team",
                    choices = c("", germanyTeams))
  observeEvent(input$select_germany_team,
               {
                 germanyTeam$a <- input$select_germany_team
                 GermanyTeamName <<- germanyTeam$a
               })
  
  # Get Home Data
  output$germany_h_wdl_win <- renderText({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    source("teamAnalysis/germanyAnalysis.R")
    germany_h_wdl_win <- germanyHWDL
  })
  # Get SRS
  output$germany_team_srs <- renderText({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    
    germany_team_srs <- GermanyTeamNameRating
  })
  
  # Get Away Data
  output$germany_a_wdl_win <- renderText({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_a_wdl_win <- germanyAWDL
  })  
  
  # GT Last H Five: germany_last_h_five
  output$germany_last_h_five <- render_gt({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_last_h_five <- germanyHDataTbl
  })
  
  # GT Last A Five: germany_last_a_five
  output$germany_last_a_five <- render_gt({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_last_a_five <- germanyADataTbl
  })
  
  # Home Goal Data
  output$germany_h_goal_data <- render_gt({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_h_goal_data <<- germanyHGoalDataTbl
  })
  # Away Goal Data
  output$germany_a_goal_data <- render_gt({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_a_goal_data <<- germanyAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$germany_last_ten <- render_gt({
    print(input$get_germany_team_data)
    
    if(input$get_germany_team_data == 0) {
      return()
    }
    germany_last_ten <- germanyLastTenTbl
  })
  
  # Greece
  
  output$greece_team <- renderText(input$select_greece_team)
  
  # Subset into Team H and A Data
  
  greeceHTeamdata <- reactive({
    greece %>%
      filter(HomeTeam == input$greece_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  greeceATeamdata <- reactive({
    greece %>%
      filter(AwayTeam == input$greece_team)
  })
  
  greeceTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_greece_team",
                    choices = c("", greeceTeams))
  observeEvent(input$select_greece_team,
               {
                 greeceTeam$a <- input$select_greece_team
                 GreeceTeamName <<- greeceTeam$a
               })
  
  # Get Home Data
  output$greece_h_wdl_win <- renderText({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    source("teamAnalysis/greeceAnalysis.R")
    greece_h_wdl_win <- greeceHWDL
  })
  # Get SRS
  output$greece_team_srs <- renderText({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    
    greece_team_srs <- GreeceTeamNameRating
  })
  # Get Away Data
  output$greece_a_wdl_win <- renderText({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_a_wdl_win <- greeceAWDL
  })  
  
  # GT Last H Five: greece_last_h_five
  output$greece_last_h_five <- render_gt({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_last_h_five <- greeceHDataTbl
  })
  
  # GT Last A Five: greece_last_a_five
  output$greece_last_a_five <- render_gt({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_last_a_five <- greeceADataTbl
  })
  
  # Home Goal Data
  output$greece_h_goal_data <- render_gt({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_h_goal_data <<- greeceHGoalDataTbl
  })
  # Away Goal Data
  output$greece_a_goal_data <- render_gt({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_a_goal_data <<- greeceAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$greece_last_ten <- render_gt({
    print(input$get_greece_team_data)
    
    if(input$get_greece_team_data == 0) {
      return()
    }
    greece_last_ten <- greeceLastTenTbl
  })
  
  
  # Italy
  
  output$italy_team <- renderText(input$select_italy_team)
  
  # Subset into Team H and A Data
  
  italyHTeamdata <- reactive({
    italy %>%
      filter(HomeTeam == input$italy_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  italyATeamdata <- reactive({
    italy %>%
      filter(AwayTeam == input$italy_team)
  })
  
  italyTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_italy_team",
                    choices = c("", italyTeams))
  observeEvent(input$select_italy_team,
               {
                 italyTeam$a <- input$select_italy_team
                 ItalyTeamName <<- italyTeam$a
               })
  
  # Get Home Data
  output$italy_h_wdl_win <- renderText({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    source("teamAnalysis/italyAnalysis.R")
    italy_h_wdl_win <- italyHWDL
  })
  # Get SRS
  output$italy_team_srs <- renderText({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    
    italy_team_srs <- ItalyTeamNameRating
  })
  
  # Get Away Data
  output$italy_a_wdl_win <- renderText({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_a_wdl_win <- italyAWDL
  })  
  
  # GT Last H Five: italy_last_h_five
  output$italy_last_h_five <- render_gt({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_last_h_five <- italyHDataTbl
  })
  
  # GT Last A Five: italy_last_a_five
  output$italy_last_a_five <- render_gt({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_last_a_five <- italyADataTbl
  })
  
  # Home Goal Data
  output$italy_h_goal_data <- render_gt({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_h_goal_data <<- italyHGoalDataTbl
  })
  # Away Goal Data
  output$italy_a_goal_data <- render_gt({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_a_goal_data <<- italyAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$italy_last_ten <- render_gt({
    print(input$get_italy_team_data)
    
    if(input$get_italy_team_data == 0) {
      return()
    }
    italy_last_ten <- italyLastTenTbl
  })
  
  # Netherlands
  
  output$neth_team <- renderText(input$select_neth_team)
  
  # Subset into Team H and A Data
  
  nethHTeamdata <- reactive({
    neth %>%
      filter(HomeTeam == input$neth_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  nethATeamdata <- reactive({
    neth %>%
      filter(AwayTeam == input$neth_team)
  })
  
  nethTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_neth_team",
                    choices = c("", nethTeams))
  observeEvent(input$select_neth_team,
               {
                 nethTeam$a <- input$select_neth_team
                 NethTeamName <<- nethTeam$a
               })
  
  # Get Home Data
  output$neth_h_wdl_win <- renderText({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    source("teamAnalysis/nethAnalysis.R")
    neth_h_wdl_win <- nethHWDL
  })
  # Get SRS
  output$neth_team_srs <- renderText({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    
    neth_team_srs <- NethTeamNameRating
  })
  # Get Away Data
  output$neth_a_wdl_win <- renderText({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_a_wdl_win <- nethAWDL
  })  
  
  # GT Last H Five: neth_last_h_five
  output$neth_last_h_five <- render_gt({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_last_h_five <- nethHDataTbl
  })
  
  # GT Last A Five: neth_last_a_five
  output$neth_last_a_five <- render_gt({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_last_a_five <- nethADataTbl
  })
  
  # Home Goal Data
  output$neth_h_goal_data <- render_gt({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_h_goal_data <<- nethHGoalDataTbl
  })
  # Away Goal Data
  output$neth_a_goal_data <- render_gt({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_a_goal_data <<- nethAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$neth_last_ten <- render_gt({
    print(input$get_neth_team_data)
    
    if(input$get_neth_team_data == 0) {
      return()
    }
    neth_last_ten <- nethLastTenTbl
  })
  
  # Portugal
  
  output$portugal_team <- renderText(input$select_portugal_team)
  
  # Subset into Team H and A Data
  
  portugalHTeamdata <- reactive({
    portugal %>%
      filter(HomeTeam == input$portugal_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  portugalATeamdata <- reactive({
    portugal %>%
      filter(AwayTeam == input$portugal_team)
  })
  
  portugalTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_portugal_team",
                    choices = c("", portugalTeams))
  observeEvent(input$select_portugal_team,
               {
                 portugalTeam$a <- input$select_portugal_team
                 PortugalTeamName <<- portugalTeam$a
               })
  
  # Get Home Data
  output$portugal_h_wdl_win <- renderText({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    source("teamAnalysis/portugalAnalysis.R")
    portugal_h_wdl_win <- portugalHWDL
  })
  # Get SRS
  output$portugal_team_srs <- renderText({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    
    portugal_team_srs <- PortugalTeamNameRating
  })
  # Get Away Data
  output$portugal_a_wdl_win <- renderText({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_a_wdl_win <- portugalAWDL
  })  
  
  # GT Last H Five: portugal_last_h_five
  output$portugal_last_h_five <- render_gt({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_last_h_five <- portugalHDataTbl
  })
  
  # GT Last A Five: portugal_last_a_five
  output$portugal_last_a_five <- render_gt({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_last_a_five <- portugalADataTbl
  })
  
  # Home Goal Data
  output$portugal_h_goal_data <- render_gt({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_h_goal_data <<- portugalHGoalDataTbl
  })
  # Away Goal Data
  output$portugal_a_goal_data <- render_gt({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_a_goal_data <<- portugalAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$portugal_last_ten <- render_gt({
    print(input$get_portugal_team_data)
    
    if(input$get_portugal_team_data == 0) {
      return()
    }
    portugal_last_ten <- portugalLastTenTbl
  })
  
  # Scotprem
  
  output$scotprem_team <- renderText(input$select_scotprem_team)
  
  # Subset into Team H and A Data
  
  scotpremHTeamdata <- reactive({
    scotprem %>%
      filter(HomeTeam == input$scotprem_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  scotpremATeamdata <- reactive({
    scotprem %>%
      filter(AwayTeam == input$scotprem_team)
  })
  
  scotpremTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_scotprem_team",
                    choices = c("", scotpremTeams))
  observeEvent(input$select_scotprem_team,
               {
                 scotpremTeam$a <- input$select_scotprem_team
                 ScotpremTeamName <<- scotpremTeam$a
               })
  
  # Get Home Data
  output$scotprem_h_wdl_win <- renderText({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    source("teamAnalysis/scotpremAnalysis.R")
    scotprem_h_wdl_win <- scotpremHWDL
  })
  # Get SRS
  output$scotprem_team_srs <- renderText({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    
    scotprem_team_srs <- ScotpremTeamNameRating
  })
  # Get Away Data
  output$scotprem_a_wdl_win <- renderText({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_a_wdl_win <- scotpremAWDL
  })  
  
  # GT Last H Five: scotprem_last_h_five
  output$scotprem_last_h_five <- render_gt({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_last_h_five <- scotpremHDataTbl
  })
  
  # GT Last A Five: scotprem_last_a_five
  output$scotprem_last_a_five <- render_gt({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_last_a_five <- scotpremADataTbl
  })
  
  # Home Goal Data
  output$scotprem_h_goal_data <- render_gt({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_h_goal_data <<- scotpremHGoalDataTbl
  })
  # Away Goal Data
  output$scotprem_a_goal_data <- render_gt({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_a_goal_data <<- scotpremAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$scotprem_last_ten <- render_gt({
    print(input$get_scotprem_team_data)
    
    if(input$get_scotprem_team_data == 0) {
      return()
    }
    scotprem_last_ten <- scotpremLastTenTbl
  })
  
  # Spain
  
  output$spain_team <- renderText(input$select_spain_team)
  
  # Subset into Team H and A Data
  
  spainHTeamdata <- reactive({
    spain %>%
      filter(HomeTeam == input$spain_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  spainATeamdata <- reactive({
    spain %>%
      filter(AwayTeam == input$spain_team)
  })
  
  spainTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_spain_team",
                    choices = c("", spainTeams))
  observeEvent(input$select_spain_team,
               {
                 spainTeam$a <- input$select_spain_team
                 SpainTeamName <<- spainTeam$a
               })
  
  # Get Home Data
  output$spain_h_wdl_win <- renderText({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    source("teamAnalysis/spainAnalysis.R")
    spain_h_wdl_win <- spainHWDL
  })
  # Get SRS
  output$spain_team_srs <- renderText({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    
    spain_team_srs <- SpainTeamNameRating
  })
  # Get Away Data
  output$spain_a_wdl_win <- renderText({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_a_wdl_win <- spainAWDL
  })  
  
  # GT Last H Five: spain_last_h_five
  output$spain_last_h_five <- render_gt({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_last_h_five <- spainHDataTbl
  })
  
  # GT Last A Five: spain_last_a_five
  output$spain_last_a_five <- render_gt({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_last_a_five <- spainADataTbl
  })
  
  # Home Goal Data
  output$spain_h_goal_data <- render_gt({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_h_goal_data <<- spainHGoalDataTbl
  })
  # Away Goal Data
  output$spain_a_goal_data <- render_gt({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_a_goal_data <<- spainAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$spain_last_ten <- render_gt({
    print(input$get_spain_team_data)
    
    if(input$get_spain_team_data == 0) {
      return()
    }
    spain_last_ten <- spainLastTenTbl
  })
  
  # Turkey
  
  output$turkey_team <- renderText(input$select_turkey_team)
  
  # Subset into Team H and A Data
  
  turkeyHTeamdata <- reactive({
    turkey %>%
      filter(HomeTeam == input$turkey_team)  %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  })
  
  turkeyATeamdata <- reactive({
    turkey %>%
      filter(AwayTeam == input$turkey_team)
  })
  
  turkeyTeam <- reactiveValues()
  
  updateSelectInput(session,
                    "select_turkey_team",
                    choices = c("", turkeyTeams))
  observeEvent(input$select_turkey_team,
               {
                 turkeyTeam$a <- input$select_turkey_team
                 TurkeyTeamName <<- turkeyTeam$a
               })
  
  # Get Home Data
  output$turkey_h_wdl_win <- renderText({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    source("teamAnalysis/turkeyAnalysis.R")
    turkey_h_wdl_win <- turkeyHWDL
  })
  # Get SRS
  output$turkey_team_srs <- renderText({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    
    turkey_team_srs <- TurkeyTeamNameRating
  })
  # Get Away Data
  output$turkey_a_wdl_win <- renderText({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_a_wdl_win <- turkeyAWDL
  })  
  
  # GT Last H Five: turkey_last_h_five
  output$turkey_last_h_five <- render_gt({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_last_h_five <- turkeyHDataTbl
  })
  
  # GT Last A Five: turkey_last_a_five
  output$turkey_last_a_five <- render_gt({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_last_a_five <- turkeyADataTbl
  })
  
  # Home Goal Data
  output$turkey_h_goal_data <- render_gt({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_h_goal_data <<- turkeyHGoalDataTbl
  })
  # Away Goal Data
  output$turkey_a_goal_data <- render_gt({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_a_goal_data <<- turkeyAGoalDataTbl
  })
  
  # Get Last 10 Matches (GT)
  output$turkey_last_ten <- render_gt({
    print(input$get_turkey_team_data)
    
    if(input$get_turkey_team_data == 0) {
      return()
    }
    turkey_last_ten <- turkeyLastTenTbl
  })
  
  #League Data
  output$league_name <- renderText(input$selected_league)
  
  league <- reactiveValues()
  
  updateSelectInput(session,
                    "selected_league",
                    choices = c("", mainLeagues))
  observeEvent(input$selected_league,
               {
                 league$a <- input$selected_league
                 LeagueName <<- league$a
               })
  
  
  
  
  first_half_goals <- reactive({
    allLeagueGoalData %>%
      filter(LeagueName == input$selected_league)
  })
  second_half_goals <- reactive({
    allLeagueGoalData %>%
      filter(LeagueName == input$selected_league)
  })
  full_time_goals <- reactive({
    allLeagueGoalData %>%
      filter(LeagueName == input$selected_league)
  })
  
  output$all_fhg <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    all_fhg <- first_half_goals() %>% select(htAllGoals) %>% as.character()
  })
  output$fh_home <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    fh_home <- full_time_goals() %>% select(htHGoals) %>% as.character()
  })
  output$fh_away <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    fh_away <- full_time_goals() %>% select(htAGoals) %>% as.character()
  })
  
  
  output$all_shg <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    all_shg <- second_half_goals() %>% select(shAllGoals) %>% as.character()
  })
  
  output$sh_home <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    sh_home <- full_time_goals() %>% select(shHGoals) %>% as.character()
  })
  output$sh_away <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    sh_away <- full_time_goals() %>% select(shAGoals) %>% as.character()
  })
  
  output$all_ftg <- renderText({
    print(input$get_league_data)
    
    if(input$get_league_data == 0) {
      return()
    }
    
    all_ftg <- full_time_goals() %>% select(ftAllGoals) %>% as.character()
  })
  
  
  # Get Matches
  output$matches_today <- render_gt({
    matches_today <- matchesTableGT
  })
  output$downloadMatches <- downloadHandler(
    filename = function() {
      paste0("dailyMatches-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(matchesTable, file, row.names = FALSE)
    }
  )
  
  # GT League SPIs
  output$spi_engprem_gt <- render_gt({
    spi_engprem_gt <- spiengpremGT
  })
  output$spi_engchamp_gt <- render_gt({
    spi_engchamp_gt <- spiengchampGT
  })
  output$spi_belgium_gt <- render_gt({
    spi_belgium_gt <- spibelgiumGT
  })
  output$spi_france_gt <- render_gt({
    spi_france_gt <- spifranceGT
  })
  output$spi_germany_gt <- render_gt({
    spi_germany_gt <- spigermanyGT
  })
  output$spi_greece_gt <- render_gt({
    spi_greece_gt <- spigreeceGT
  })
  output$spi_italy_gt <- render_gt({
    spi_italy_gt <- spiitalyGT
  })
  output$spi_neth_gt <- render_gt({
    spi_neth_gt <- spinethGT
  })
  output$spi_portugal_gt <- render_gt({
    spi_portugal_gt <- spiportugalGT
  })
  output$spi_scotprem_gt <- render_gt({
    spi_scotprem_gt <- spiscotpremGT
  })
  output$spi_spain_gt <- render_gt({
    spi_spain_gt <- spispainGT
  })
  output$spi_turkey_gt <- render_gt({
    spi_turkey_gt <- spiturkeyGT
  })
  
  # Leagues Goal Analysis Tables
  output$goals_engprem_gt <- render_gt({
    
    goals_engprem_gt <- allTeamsengpremGoalDataFGT
  })
  output$goals_engchamp_gt <- render_gt({
    goals_engchamp_gt <- allTeamsengchampGoalDataFGT
  })
  output$goals_belgium_gt <- render_gt({
    goals_belgium_gt <- allTeamsbelgiumGoalDataFGT
  })
  output$goals_france_gt <- render_gt({
    goals_france_gt <- allTeamsfranceGoalDataFGT
  })
  output$goals_germany_gt <- render_gt({
    goals_germany_gt <- allTeamsgermanyGoalDataFGT
  })
  output$goals_greece_gt <- render_gt({
    goals_greece_gt <- allTeamsgreeceGoalDataFGT
  })
  output$goals_italy_gt <- render_gt({
    goals_italy_gt <- allTeamsitalyGoalDataFGT
  })
  output$goals_neth_gt <- render_gt({
    goals_neth_gt <- allTeamsnethGoalDataFGT
  })
  output$goals_portugal_gt <- render_gt({
    goals_portugal_gt <- allTeamsportugalGoalDataFGT
  })
  output$goals_scotprem_gt <- render_gt({
    goals_scotprem_gt <- allTeamsscotpremGoalDataFGT
  })
  output$goals_spain_gt <- render_gt({
    goals_spain_gt <- allTeamsspainGoalDataFGT
  })
  output$goals_turkey_gt <- render_gt({
    goals_turkey_gt <- allTeamsturkeyGoalDataFGT
  })
  
  
  
  
  # Administration
  output$last_admin_update <- renderText({
    print(input$admin_process)
    
    if(input$admin_process == 0) {
      return()
    }
    source("leagueData.R")
    source("leagueGoalProcessing.R")
    last_admin_update <- Sys.Date()
  })
  
}
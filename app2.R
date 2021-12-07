library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(gt)
library(readxl)

#Import Data
Big_Dance_CSV <- read.csv("Big_Dance_CSV.csv")
teams <- read_excel("teams.xlsx")

#Clean Data



Big_Dance_Seeds <- Big_Dance_CSV %>%
  rename(Seed_1 = Seed.1, Team_1 = Team.1, Score_1 = Score.1)%>%
  mutate("high seed" = case_when(Seed < Seed_1 ~ Seed,
                                 Seed_1 < Seed ~ Seed_1,
                                 Seed == Seed_1 ~ Seed),
         "low seed" = case_when(Seed > Seed_1 ~ Seed,
                                Seed_1 > Seed ~ Seed_1,
                                Seed == Seed_1 ~ Seed),
         "high seed team" = case_when(Seed < Seed_1 ~ Team,
                                      Seed_1 < Seed ~ Team_1,
                                      Seed == Seed_1 ~ Team),
         "low seed team" = case_when(Seed > Seed_1 ~ Team,
                                     Seed_1 > Seed ~ Team_1,
                                     Seed == Seed_1 ~ Team_1),
         "high seed score" = case_when(Seed < Seed_1 ~ Score,
                                       Seed_1 < Seed ~ Score_1,
                                       Seed == Seed_1 ~ Score),
         "low seed score" = case_when(Seed > Seed_1 ~ Score,
                                      Seed_1 > Seed ~ Score_1,
                                      Seed == Seed_1 ~ Score_1),
         "winning score" = case_when(Score > Score_1 ~ Score,
                                     Score_1 > Score ~ Score_1),
         "losing score" = case_when(Score < Score_1 ~ Score,
                                    Score_1 < Score ~ Score_1),
         "high seed win" = case_when(`high seed score` > `low seed score` ~ 1,
                                     `high seed score` < `low seed score` ~ 0),
         "round name" = case_when(Round == 1 ~ "Round of 64",
                                  Round == 2 ~ "Round of 32",
                                  Round == 3 ~ "Sweet 16",
                                  Round == 4 ~ "Elite 8",
                                  Round == 5 ~ "Final 4",
                                  Round == 6 ~ "Championship")) %>% 
  select(-Team, -Team_1, -Score, -Score_1, -Seed, -Seed_1)

conference <- left_join(Big_Dance_Seeds, teams)
conferences <- left_join(conference, teams, by= c("low seed team" = "high seed team"))
conferences <- conferences%>%
  rename("High Seed Conference" = Conference.x, "Low Seed Conference" = Conference.y)


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("NCAA March Madness", theme = shinytheme("lumen"),
             tabPanel("Matchup Finder", fluid = TRUE, icon = icon("basketball-ball"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Desired Matchup"),
                          fluidRow(column(8,
                                          selectInput(inputId = "seed1",
                                                      label = "Team Seed:",
                                                      choices = c(1:16),
                                                      selected = 1),
                                          hr(),
                                          selectInput(inputId = "seed2", 
                                                      label = "Opponent Seed:",
                                                      choices = c(1:16), 
                                                      selected = 1)
                                          
                          ),
                          ),
                          hr(),
                          sliderInput(inputId = "year",
                                      label = "Select Year Range",
                                      min = 1985,
                                      max = 2021,
                                      value = c(1985, 2021),
                                      width = "220px"),
                          hr(),
                        ),
                        mainPanel(
                          fluidRow(
                            column(12,
                            )),
                          hr(),
                          textOutput(outputId = "text"),
                          hr(),
                          br(),
                          tableOutput(outputId = "matchups"),
                          hr(),
                          fluidRow(column(7,
                          ),
                          ),
                          br(),
                          fluidRow((dataTableOutput(outputId = "bigdata")))
                        )
                      )
             ),
             navbarMenu("Team/Conference Statistics", icon = icon("chart-bar"),
                        # Team Statistics Panel
                        tabPanel("Team Statistics", fluid = TRUE, icon = icon("bars"),
                                 titlePanel("Team Statistics"),
                                 fluidRow(
                                   column(6,
                                          selectizeInput(inputId = "SchoolSelectA",
                                                         label = "Select Schools (Max 2)",
                                                         choices = sort(c(Big_Dance_Seeds$`high seed team`, Big_Dance_Seeds$`low seed team`)),
                                                         multiple = TRUE,
                                                         options = list(maxItems = 2, placeholder = 'Enter school name',
                                                                        onInitialize = I('function() { this.setValue(""); }'))
                                          ),
                                   ),
                                   column(6,
                                          checkboxGroupInput(inputId = "RoundSelect",
                                                             label = "Select Round:",
                                                             choices = unique(conferences$`round name`),
                                                             selected = unique(conferences$`round name`))
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(6,
                                          tableOutput(outputId = "SchoolHistory1"),
                                          hr(),
                                          tableOutput("SchoolHistory2")
                                   ),
                                 )  
                        ),
                        #Conference Statistics Panel
                        tabPanel("Conference Statistics", fluid = TRUE, icon = icon("bolt"),
                                 titlePanel("Conference Statistics"),
                                 fluidRow(
                                   column(6,
                                          selectizeInput(inputId = "conferenceSelect",
                                                         label = "Select Conference (Max 2)",
                                                         choices = sort(c(conferences$`High Seed Conference`, Big_Dance_Seeds$`Low Seed Conference`)),
                                                         multiple = TRUE,
                                                         options = list(maxItems = 2, placeholder = 'Enter Conference',
                                                                        onInitialize = I('function() { this.setValue(""); }'))
                                          ),
                                   ),
                                   column(6,
                                          checkboxGroupInput(inputId = "RoundSelect2",
                                                             label = "Select Round:",
                                                             choices = unique(conferences$`round name`),
                                                             selected = unique(conferences$`round name`))
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(12,
                                          tableOutput(outputId = "conferenceHistory1"),
                                          hr(),
                                          tableOutput("conferenceHistory2")
                                   ),
                                 )  
                        )),
             navbarMenu("Spread Comparisons", icon = icon("chart-bar"),
                        tabPanel("Spread Comparision Between Seeds", fluid = TRUE,
                                 tags$style(button_color_css),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     titlePanel("Desired Matchup"),
                                     fluidRow(column(8,
                                                     selectInput(inputId = "seed1_2",
                                                                 label = "Team Seed:",
                                                                 choices = c(1:16),
                                                                 selected = 1),
                                                     hr(),
                                                     selectInput(inputId = "seed2_2", 
                                                                 label = "Opponent Seed:",
                                                                 choices = c(1:16), 
                                                                 selected = 1)
                                                     
                                     ),
                                     ),
                                     hr(),
                                     sliderInput(inputId = "year_2",
                                                 label = "Select Year Range",
                                                 min = 1985,
                                                 max = 2021,
                                                 value = c(1985, 2021),
                                                 width = "220px"),
                                     hr(),
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(12,
                                       )),
                                     hr(),
                                     br(),
                                     fluidRow((dataTableOutput(outputId = "seedsTable"))),
                                     hr(),
                                     br(),
                                     fluidRow((plotOutput(outputId = "seedsSpreadHist", brush = "plot_hover"))),
                                     hr(),
                                     br(),
                                     fluidRow((dataTableOutput(outputId = "moviestable")))
                                   )
                                 )
                        ),
                        tabPanel("Spread Comparison Between Conferences", fluid = TRUE,
                                 tags$style(button_color_css),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     titlePanel("Desired Conferences"),
                                     fluidRow(column(8,
                                                     selectInput(inputId = "conference1",
                                                                 label = "Team Conference:",
                                                                 choices = sort(c(conferences$`High Seed Conference`,conferences$`Low Seed Conference`))),
                                                     hr(),
                                                     selectInput(inputId = "conference2", 
                                                                 label = "Opponent Conference:",
                                                                 choices = sort(c(conferences$`High Seed Conference`,conferences$`Low Seed Conference`)))
                                                     
                                     ),
                                     ),
                                     hr(),
                                     sliderInput(inputId = "year_3",
                                                 label = "Select Year Range",
                                                 min = 1985,
                                                 max = 2021,
                                                 value = c(1985, 2021),
                                                 width = "220px"),
                                     hr(),
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(12,
                                       )),
                                     hr(),
                                     br(),
                                     fluidRow((dataTableOutput(outputId = "conferencesTable"))),
                                     hr(),
                                     br(),
                                     hr(),
                                     br()
                                   )
                                 )
                        ),
                        tabPanel("Spread Comparisons Between Seeds/Conferences", fluid = TRUE,
                                 titlePanel("Division I School Types"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Select which Gender(s) to plot
                                     checkboxGroupInput(inputId = "GenderDI",
                                                        label = "Select Gender(s):",
                                                        choices = c("Male" = "M", "Female" = "F"),
                                                        selected = "M"),
                                     # Select which Region(s) to plot
                                     checkboxGroupInput(inputId = "RegionDI",
                                                        label = "Select Region:",
                                                        choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                        selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific", "Alaska", "Hawaii")),
                                     # Set Top X Rank
                                     sliderInput(inputId = "RankDI",
                                                 label = "Top Times Range:",
                                                 min = 1, max = 3500,
                                                 value = c(1,250)),
                                     # Set school rank
                                     sliderInput(inputId = "School_RankDI",
                                                 label = "School Rank",
                                                 min = 1,
                                                 max = 250,
                                                 value = c(1,250))
                                   ),
                                   mainPanel(
                                     withSpinner(plotOutput(outputId = "barplotDI")),
                                     textOutput(outputId = "description_DI")
                                     #plotOutput(outputId = "scatterplotDI")
                                   )
                                 )
                        ),
                        tabPanel("Spread Comparison by Seeds per Round", fluid = TRUE,
                                 tags$style(button_color_css),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     titlePanel("Desired Matchup"),
                                     fluidRow(column(8,
                                                     selectInput(inputId = "seed1_4",
                                                                 label = "Team Seed:",
                                                                 choices = c(1:16),
                                                                 selected = 1),
                                                     hr(),
                                                     selectInput(inputId = "seed2_4", 
                                                                 label = "Opponent Seed:",
                                                                 choices = c(1:16), 
                                                                 selected = 1)
                                                     
                                     ),
                                     ),
                                     hr(),
                                     selectInput(inputId = "round",
                                                 label = "Round:",
                                                 choices = Big_Dance_Seeds$`round name`,
                                                 selected = Big_Dance_Seeds$`round name`[0]),
                                     hr(),
                                     sliderInput(inputId = "year_4",
                                                 label = "Select Year Range",
                                                 min = 1985,
                                                 max = 2021,
                                                 value = c(1985, 2021),
                                                 width = "220px"),
                                     hr(),
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(12,
                                       )),
                                     hr(),
                                     br(),
                                     fluidRow((dataTableOutput(outputId = "seedsRoundTable"))),
                                     hr(),
                                     br(),
                                     fluidRow((plotOutput(outputId = "seedsSpreadHist2", brush = "plot_hover"))),
                                     hr(),
                                     br(),
                                     fluidRow((dataTableOutput(outputId = "moviestable2")))
                                   )
                                 )
                        ),
                        tabPanel("Spread Comparisons by Seeds/Conferences per Round", fluid = TRUE,
                                 titlePanel("Division III School Types"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Select which Gender(s) to plot
                                     checkboxGroupInput(inputId = "GenderDIII",
                                                        label = "Select Gender(s):",
                                                        choices = c("Male" = "M", "Female" = "F"),
                                                        selected = "M"),
                                     # Select which Region(s) to plot
                                     checkboxGroupInput(inputId = "RegionDIII",
                                                        label = "Select Region:",
                                                        choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                        selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific", "Alaska", "Hawaii")),
                                     # Set Top X Rank
                                     sliderInput(inputId = "RankDIII",
                                                 label = "Top Times Range:",
                                                 min = 1, max = 3500,
                                                 value = c(1,250)),
                                     # Set school rank
                                     sliderInput(inputId = "School_RankDIII",
                                                 label = "School Rank",
                                                 min = 1,
                                                 max = 250,
                                                 value = c(1,250))
                                   ),
                                   mainPanel(
                                     withSpinner(plotOutput(outputId = "barplotDIII")),
                                     textOutput(outputId = "description_DIII")
                                   )
                                 )
                        )
             ),
             # Simulator Tab
             tabPanel("Simulator", icon = icon("random"),
                      titlePanel("Tournament Simulator"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Desired Matchup"),
                          fluidRow(column(8,
                                          selectInput(inputId = "seed1_8",
                                                      label = "Team Seed:",
                                                      choices = c(1:16),
                                                      selected = 1),
                                          hr(),
                                          selectInput(inputId = "seed2_8", 
                                                      label = "Opponent Seed:",
                                                      choices = c(1:16), 
                                                      selected = 1)
                                          
                          ),
                          ),
                          hr(),
                          selectInput(inputId = "round5",
                                      label = "Round:",
                                      choices = Big_Dance_Seeds$`round name`,
                                      selected = Big_Dance_Seeds$`round name`[0]),
                          hr(),
                          sliderInput(inputId = "year_8",
                                      label = "Select Year Range",
                                      min = 1985,
                                      max = 2021,
                                      value = c(1985, 2021),
                                      width = "220px"),
                          hr(),
                        ),
                        mainPanel(
                          fluidRow(
                            column(12,
                            )),
                          hr(),
                          br(),
                          fluidRow((dataTableOutput(outputId = "seedsRoundTeable"))),
                          hr(),
                          br(),
                          fluidRow((plotOutput(outputId = "seedsSpreadHist3", brush = "plot_hover"))),
                          hr(),
                          br(),
                          fluidRow((dataTableOutput(outputId = "moviestable3")))
                        )
                      )
             )
  )
)


# Define server
server <- function(input, output, session) {
  
  #Data Table
  
  output$matchups <- renderTable(Big_Dance_Seeds %>%
                                   filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | 
                                            `high seed` %in% input$seed1 & `low seed` %in% input$seed2,
                                          Year >= input$year[1],
                                          Year <= input$year[2])%>%
                                   summarise(`# of games` = n(),
                                             `win %` = mean(`high seed win`)) %>%
                                   mutate("# of wins" = `win %` * `# of games`) %>% 
                                   select(`# of wins`, `# of games`, `win %`))
  
  output$bigdata <- renderDataTable(Big_Dance_Seeds%>%
                                      filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | `high seed` %in% input$seed1 & `low seed` %in% input$seed2, Year >= input$year[1], Year <= input$year[2]) %>% 
                                      select(Year, `round name`, `high seed`, `high seed team`, `low seed`, `low seed team`, `high seed score`, `low seed score`))
  
  output$text <- renderText({
    if (input$seed1 == input$seed2){
      "These teams are the same seed."
    }
    else{
      paste("The ", case_when(as.numeric(input$seed1) < as.numeric(input$seed2) ~ as.numeric(input$seed1),
                              as.numeric(input$seed1) > as.numeric(input$seed2) ~ as.numeric(input$seed2)), " seed has beaten the ", case_when(as.numeric(input$seed1) > as.numeric(input$seed2) ~ as.numeric(input$seed1),
                                                                                                                                               as.numeric(input$seed1) < as.numeric(input$seed2) ~ as.numeric(input$seed2)), " seed ", as.numeric(
                                                                                                                                                 Big_Dance_Seeds %>%
                                                                                                                                                   filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | `high seed` %in% input$seed1 & `low seed` %in% input$seed2, Year >= input$year[1], Year <= input$year[2]) %>%
                                                                                                                                                   na.omit() %>% 
                                                                                                                                                   summarise(`win %` = round(mean(`high seed win`) * 100,2))), "% of the time between ", input$year[1], " and ", input$year[2], sep = "")
    }
  })
  
  output$win_names <- renderText({
    paste("The ", case_when(as.numeric(input$seed1) < as.numeric(input$seed2) ~ as.numeric(input$seed1),
                            as.numeric(input$seed1) > as.numeric(input$seed2) ~ as.numeric(input$seed2)), " seed has beaten the ", case_when(as.numeric(input$seed1) > as.numeric(input$seed2) ~ as.numeric(input$seed1),
                                                                                                                                             as.numeric(input$seed1) < as.numeric(input$seed2) ~ as.numeric(input$seed2)), " Seed Wins")
  })
  output$seedsTable <- renderDataTable({
    if (input$seed1_2 == input$seed2_2){
      DT::datatable(data = Big_Dance_Seeds %>%
                      filter(
                        `low seed` %in% input$seed1_2 & `high seed` %in% input$seed2_2 | 
                          `high seed` %in% input$seed1_2 & `low seed` %in% input$seed2_2,
                        Year >= input$year_2[1],
                        Year <= input$year_2[2])%>%
                      mutate(Difference = abs(`high seed score` - `low seed score`))%>%
                      summarise(Games = n(), winningScore = round(mean(`winning score`),2), losingScore = round(mean(`losing score`),2), avgDiff = round(mean(Difference),2)))
    }
    else{
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_2 & `high seed` %in% input$seed2_2 | 
                 `high seed` %in% input$seed1_2 & `low seed` %in% input$seed2_2,
               Year >= input$year_2[1],
               Year <= input$year_2[2])%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        group_by(`high seed win`)%>%
        summarise(Games = n(), highSeedScore = round(mean(`high seed score`),2), lowSeedScore = round(mean(`low seed score`),2), avgDiff = round(mean(Difference),2))
    }
  })
  
  output$seedsSpreadHist <- renderPlot({
    if (input$seed1_2 == input$seed2_2){
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_2 & `high seed` %in% input$seed2_2 | 
                 `high seed` %in% input$seed1_2 & `low seed` %in% input$seed2_2,
               Year >= input$year_2[1],
               Year <= input$year_2[2])%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()
    }
    else{
      win_names = c(`0` = paste(case_when(as.numeric(input$seed1_2) > as.numeric(input$seed2_2) ~ as.numeric(input$seed1_2),
                                          as.numeric(input$seed1_2) < as.numeric(input$seed2_2) ~ as.numeric(input$seed2_2)), " Seed Wins"), 
                    `1` = paste(case_when(as.numeric(input$seed1_2) < as.numeric(input$seed2_2) ~ as.numeric(input$seed1_2),
                                          as.numeric(input$seed1_2) > as.numeric(input$seed2_2) ~ as.numeric(input$seed2_2)), " Seed Wins"))
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_2 & `high seed` %in% input$seed2_2 | 
                 `high seed` %in% input$seed1_2 & `low seed` %in% input$seed2_2,
               Year >= input$year_2[1],
               Year <= input$year_2[2])%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()+
        facet_wrap(~ `high seed win`, ncol = 1, labeller = as_labeller(win_names), scales='free')
    }
  })
  
  output$moviestable <- renderDataTable({
    brushedPoints(Big_Dance_Seeds %>%
                    filter(`low seed` %in% input$seed1_2 & `high seed` %in% input$seed2_2 | 
                             `high seed` %in% input$seed1_2 & `low seed` %in% input$seed2_2,
                           Year >= input$year_2[1],
                           Year <= input$year_2[2])%>%
                    mutate(Difference = abs(`high seed score` - `low seed score`)), brush = input$plot_hover) %>%
      select(`high seed team`, `low seed team`, Difference)
  })
  
  
  output$seedsRoundTable <- renderDataTable({
    if (input$seed1_4 == input$seed2_4){
      DT::datatable(data = Big_Dance_Seeds %>%
                      filter(
                        `low seed` %in% input$seed1_4 & `high seed` %in% input$seed2_4 | 
                          `high seed` %in% input$seed1_4 & `low seed` %in% input$seed2_4,
                        Year >= input$year_4[1],
                        Year <= input$year_4[2],
                        `round name` == input$round)%>%
                      mutate(Difference = abs(`high seed score` - `low seed score`))%>%
                      summarise(Games = n(), winningScore = round(mean(`winning score`),2), losingScore = round(mean(`losing score`),2), avgDiff = round(mean(Difference),2)))
    }
    else{
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_4 & `high seed` %in% input$seed2_4 | 
                 `high seed` %in% input$seed1_4 & `low seed` %in% input$seed2_4,
               Year >= input$year_4[1],
               Year <= input$year_4[2],
               `round name` == input$round)%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        group_by(`high seed win`)%>%
        summarise(Games = n(), highSeedScore = mean(`high seed score`), lowSeedScore = mean(`low seed score`), avgDiff = mean(Difference))
    }
  })
  
  output$seedsSpreadHist2 <- renderPlot({
    if (input$seed1_4 == input$seed2_4){
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_4 & `high seed` %in% input$seed2_4 | 
                 `high seed` %in% input$seed1_4 & `low seed` %in% input$seed2_4,
               Year >= input$year_4[1],
               Year <= input$year_4[2],
               `round name` == input$round)%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()
    }
    else{
      win_names = c(`0` = "Low Seed Win", `1` = "High Seed Win")
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1_4 & `high seed` %in% input$seed2_4 | 
                 `high seed` %in% input$seed1_4 & `low seed` %in% input$seed2_4,
               Year >= input$year_4[1],
               Year <= input$year_4[2],
               `round name` == input$round)%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()+
        facet_wrap(~ `high seed win`, ncol = 1, labeller = as_labeller(win_names))
    }
  })
  
  output$conferencesTable <- renderDataTable({req(input$conference1)
    req(input$conference2)
    conferences%>%
      filter(`Low Seed Conference` %in% input$conference1 & `High Seed Conference` %in% input$conference2 | 
               `High Seed Conference` %in% input$conference1 & `Low Seed Conference` %in% input$conference2,
             Year >= input$year_3[1],
             Year <= input$year_3[2])%>%
      mutate(Difference = abs(`high seed score` - `low seed score`))%>%
      group_by(`high seed win`)%>%
      summarise(Games = n(), highSeedScore = round(mean(`high seed score`),2), lowSeedScore = round(mean(`low seed score`),2), avgDiff = round(mean(Difference),2))
  })
  
  #tables for team comparisons
  output$SchoolHistory1 <- renderTable({req(input$SchoolSelectA[1])
    req(input$RoundSelect)
    Big_Dance_Seeds %>% filter(`high seed team` == input$SchoolSelectA[1] | `low seed team` == input$SchoolSelectA[1], `round name` %in% input$RoundSelect) %>% 
      mutate("Win" = case_when((`high seed team` == input$SchoolSelectA[1] & `high seed score` > `low seed score`) | (`low seed team` == input$SchoolSelectA[1] & `low seed score` > `high seed score`) ~ 1,
                               (`high seed team` == input$SchoolSelectA[1] & `high seed score` < `low seed score`) | (`low seed team` == input$SchoolSelectA[1] & `low seed score` < `high seed score`) ~ 0),
             "Tournaments" = case_when((`high seed team` == input$SchoolSelectA[1] & Round == 1 | `low seed team` == input$SchoolSelectA[1] & Round == 1) ~ 1,
                                       TRUE ~ 0),
             "Championship" = case_when((`high seed team` == input$SchoolSelectA[1] & `high seed score` > `low seed score` & Round == 6) | (`low seed team` == input$SchoolSelectA[1] & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                        TRUE ~ 0),
             "Champion" = case_when((`high seed team` == input$SchoolSelectA[1] & `high seed score` > `low seed score` & Round == 5) | (`low seed team` == input$SchoolSelectA[1] & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                    TRUE ~ 0),
             "Final Four" = case_when((`high seed team` == input$SchoolSelectA[1] & `high seed score` > `low seed score` & Round == 4) | (`low seed team` == input$SchoolSelectA[1] & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                      TRUE ~ 0),
             "PF" = case_when(`high seed team` == input$SchoolSelectA[1] ~ `high seed score`,
                              `low seed team` == input$SchoolSelectA[1] ~ `low seed score`),
             "PA" = case_when(`high seed team` == input$SchoolSelectA[1] ~ `low seed score`,
                              `low seed team` == input$SchoolSelectA[1] ~ `high seed score`)) %>% 
      summarise("Tournament Appearances" = sum(Tournaments),
                "Games Played" = n(),
                "win%" = mean(Win),
                "# of wins" = `Games Played` * `win%`,
                "# of losses" = `Games Played` - `# of wins`,
                "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
                "Championships Won" = mean(Championship) * n(),
                "Championships Made" = mean(Champion) * n(),
                "Final Fours" = mean(`Final Four`) * n(),
                "Average Points Scored" = round(mean(PF),2),
                "Average Points Allowed" = round(mean(PA),2)) %>% 
      mutate("Team" = input$SchoolSelectA[1]) %>% 
      select(Team, `Tournament Appearances`, `Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`) %>% 
      gt()
  })
  output$SchoolHistory2 <- renderTable({req(input$SchoolSelectA[2])
    req(input$RoundSelect)
    Big_Dance_Seeds %>% filter(`high seed team` == input$SchoolSelectA[2] | `low seed team` == input$SchoolSelectA[2], `round name` %in% input$RoundSelect) %>% 
      mutate("Win" = case_when((`high seed team` == input$SchoolSelectA[2] & `high seed score` > `low seed score`) | (`low seed team` == input$SchoolSelectA[2] & `low seed score` > `high seed score`) ~ 1,
                               (`high seed team` == input$SchoolSelectA[2] & `high seed score` < `low seed score`) | (`low seed team` == input$SchoolSelectA[2] & `low seed score` < `high seed score`) ~ 0),
             "Tournaments" = case_when((`high seed team` == input$SchoolSelectA[2] & Round == 1 | `low seed team` == input$SchoolSelectA[2] & Round == 1) ~ 1,
                                       TRUE ~ 0),
             "Championship" = case_when((`high seed team` == input$SchoolSelectA[2] & `high seed score` > `low seed score` & Round == 6) | (`low seed team` == input$SchoolSelectA[2] & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                        TRUE ~ 0),
             "Champion" = case_when((`high seed team` == input$SchoolSelectA[2] & `high seed score` > `low seed score` & Round == 5) | (`low seed team` == input$SchoolSelectA[2] & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                    TRUE ~ 0),
             "Final Four" = case_when((`high seed team` == input$SchoolSelectA[2] & `high seed score` > `low seed score` & Round == 4) | (`low seed team` == input$SchoolSelectA[2] & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                      TRUE ~ 0),
             "PF" = case_when(`high seed team` == input$SchoolSelectA[2] ~ `high seed score`,
                              `low seed team` == input$SchoolSelectA[2] ~ `low seed score`),
             "PA" = case_when(`high seed team` == input$SchoolSelectA[2] ~ `low seed score`,
                              `low seed team` == input$SchoolSelectA[2] ~ `high seed score`)) %>% 
      summarise("Tournament Appearances" = sum(Tournaments),
                "Games Played" = n(),
                "win%" = mean(Win),
                "# of wins" = `Games Played` * `win%`,
                "# of losses" = `Games Played` - `# of wins`,
                "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
                "Championships Won" = mean(Championship) * n(),
                "Championships Made" = mean(Champion) * n(),
                "Final Fours" = mean(`Final Four`) * n(),
                "Average Points Scored" = round(mean(PF),2),
                "Average Points Allowed" = round(mean(PA),2)) %>%
      mutate("Team" = input$SchoolSelectA[2]) %>% 
      select(Team, `Tournament Appearances` ,`Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`) %>% 
      gt() %>% 
      tab_header(title = md("Historical Record"))
  })
  
  #tables for conference comparisons
  
  output$conferenceHistory1 <- renderTable({req(input$conferenceSelect[1])
    req(input$RoundSelect2)
    conferences %>% filter(`High Seed Conference` == input$conferenceSelect[1] | `Low Seed Conference` == input$conferenceSelect[1], `round name` %in% input$RoundSelect2) %>% 
      mutate("Win" = case_when((`High Seed Conference` == input$conferenceSelect[1] & `high seed score` > `low seed score`) | (`Low Seed Conference` == input$conferenceSelect[1] & `low seed score` > `high seed score`) ~ 1,
                               (`High Seed Conference` == input$conferenceSelect[1] & `high seed score` < `low seed score`) | (`Low Seed Conference` == input$conferenceSelect[1] & `low seed score` < `high seed score`) ~ 0),
             "Tournaments" = case_when((`High Seed Conference` == input$conferenceSelect[1] & Round == 1 | `Low Seed Conference` == input$conferenceSelect[1] & Round == 1) ~ 1,
                                       TRUE ~ 0),
             "Championship" = case_when((`High Seed Conference` == input$conferenceSelect[1] & `high seed score` > `low seed score` & Round == 6) | (`Low Seed Conference` == input$conferenceSelect[1] & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                        TRUE ~ 0),
             "Champion" = case_when((`High Seed Conference` == input$conferenceSelect[1] & `high seed score` > `low seed score` & Round == 5) | (`Low Seed Conference` == input$conferenceSelect[1] & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                    TRUE ~ 0),
             "Final Four" = case_when((`High Seed Conference` == input$conferenceSelect[1] & `high seed score` > `low seed score` & Round == 4) | (`Low Seed Conference` == input$conferenceSelect[1] & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                      TRUE ~ 0),
             "PF" = case_when(`High Seed Conference` == input$conferenceSelect[1] ~ `high seed score`,
                              `Low Seed Conference` == input$conferenceSelect[1] ~ `low seed score`),
             "PA" = case_when(`High Seed Conference` == input$conferenceSelect[1] ~ `low seed score`,
                              `Low Seed Conference` == input$conferenceSelect[1] ~ `high seed score`)) %>% 
      summarise("Tournament Appearances" = sum(Tournaments),
                "Games Played" = n(),
                "win%" = mean(Win),
                "# of wins" = `Games Played` * `win%`,
                "# of losses" = `Games Played` - `# of wins`,
                "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
                "Championships Won" = mean(Championship) * n(),
                "Championships Made" = mean(Champion) * n(),
                "Final Fours" = mean(`Final Four`) * n(),
                "Average Points Scored" = round(mean(PF),2),
                "Average Points Allowed" = round(mean(PA),2)) %>% 
      mutate("Conference" = input$conferenceSelect[1]) %>% 
      select(Conference, `Tournament Appearances` ,`Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`) %>% 
      gt()
  })
  
  output$conferenceHistory2 <- renderTable({req(input$conferenceSelect[2])
    req(input$RoundSelect2)
    conferences %>% filter(`High Seed Conference` == input$conferenceSelect[2] | `Low Seed Conference` == input$conferenceSelect[2], `round name` %in% input$RoundSelect2) %>% 
      mutate("Win" = case_when((`High Seed Conference` == input$conferenceSelect[2] & `high seed score` > `low seed score`) | (`Low Seed Conference` == input$conferenceSelect[2] & `low seed score` > `high seed score`) ~ 1,
                               (`High Seed Conference` == input$conferenceSelect[2] & `high seed score` < `low seed score`) | (`Low Seed Conference` == input$conferenceSelect[2] & `low seed score` < `high seed score`) ~ 0),
             "Tournaments" = case_when((`High Seed Conference` == input$conferenceSelect[2] & Round == 1 | `Low Seed Conference` == input$conferenceSelect[2] & Round == 1) ~ 1,
                                       TRUE ~ 0),
             "Championship" = case_when((`High Seed Conference` == input$conferenceSelect[2] & `high seed score` > `low seed score` & Round == 6) | (`Low Seed Conference` == input$conferenceSelect[2] & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                        TRUE ~ 0),
             "Champion" = case_when((`High Seed Conference` == input$conferenceSelect[2] & `high seed score` > `low seed score` & Round == 5) | (`Low Seed Conference` == input$conferenceSelect[2] & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                    TRUE ~ 0),
             "Final Four" = case_when((`High Seed Conference` == input$conferenceSelect[2] & `high seed score` > `low seed score` & Round == 4) | (`Low Seed Conference` == input$conferenceSelect[2] & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                      TRUE ~ 0),
             "PF" = case_when(`High Seed Conference` == input$conferenceSelect[2] ~ `high seed score`,
                              `Low Seed Conference` == input$conferenceSelect[2] ~ `low seed score`),
             "PA" = case_when(`High Seed Conference` == input$conferenceSelect[2] ~ `low seed score`,
                              `Low Seed Conference` == input$conferenceSelect[2] ~ `high seed score`)) %>% 
      summarise("Tournament Appearances" = sum(Tournaments),
                "Games Played" = n(),
                "win%" = mean(Win),
                "# of wins" = `Games Played` * `win%`,
                "# of losses" = `Games Played` - `# of wins`,
                "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
                "Championships Won" = mean(Championship) * n(),
                "Championships Made" = mean(Champion) * n(),
                "Final Fours" = mean(`Final Four`) * n(),
                "Average Points Scored" = round(mean(PF),2),
                "Average Points Allowed" = round(mean(PA),2)) %>% 
      mutate("Conference" = input$conferenceSelect[2]) %>% 
      select(Conference, `Tournament Appearances` ,`Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`) %>% 
      gt()
  })
  
  # Simulator tab output
  
  output$ProbabilityShow <- renderText({
    if(input$seed1_8 == input$seed2_8) {
      "These teams are the same seed, so this will be a 50/50 matchup"
    }
    else {
      paste("The ", case_when(as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed1_8,
                              as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed2_8), " seed will have a ", Team_Probability(),
            "% chance of beating the ", case_when(as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed1_8,
                                                  as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed2_8), " seed", sep = "")
    }
  })
  
  # BigTop100_finder <- reactive({
  #   req(input$DivisionFinder)
  #   req(input$RegionFinder)
  #   req(input$School_TypeFinder)
  #   req(input$GenderFinder)
  #   req(input$EventFinder)
  #   req(Input$School_Rank)
  #   filter(BigTop100, Division %in% input$DivisionFinder) %>%
  #     filter(Region %in% input$RegionFinder) %>%
  #     filter(Event %in% input$EventFinder) %>%
  #     filter(Time >= TimeFinderDF()$Time[1], Time <= TimeFinderDF()$Time[2]) %>%
  #     filter(Sex %in% input$GenderFinder) %>%
  #     filter(Type %in% input$School_TypeFinder) %>%
  #     filter(Y2019 >= input$School_RankFinder[1], Y2019 <= input$School_RankFinder[2]) %>%
  #     filter(RankInEvent_Team >= input$RankOnTeam[1], RankInEvent_Team <= input$RankOnTeam[2]) %>%
  #     group_by(Team, Event) %>%
  #     dplyr::mutate(Entries = n()) %>%
  #     dplyr::mutate(MinTime = mmss_format(min(Time))) %>%
  #     dplyr::mutate(MaxTime = mmss_format(max(Time)))
  # 
  # })
  # 
  # fiftystatesCAN_Finder <- reactive({
  #   req(input$RegionFinder)
  #   filter(fiftystatesCAN, GeoRegion %in% input$RegionFinder)
  # })
  # 
  # uniquecities_Finder <- reactive({
  #   req(input$RegionFinder)
  #   filter(uniquecities, Region %in% input$RegionFinder) %>%
  #     filter(Team %in% BigTop100_finder()$Team)
  # })
  # 
  # output$scatterplotFinder <- renderPlot({
  #   input$EnterTimes
  #   input$show_NamesFinder
  #   input$GenderFinder
  #   input$DivisionFinder
  #   input$RegionFinder
  #   input$RankOnTeam
  #   input$School_TypeFinder
  #   input$School_RankFinder
  #   isolate({
  #     if (length(BigTop100_finder()$Address) == 0) {
  #       ggplot() +
  #         geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
  #         coord_quickmap() +
  #         theme_void() +
  #         ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
  #         theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
  #     } else {
  #       ggplot() +
  #         geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
  #         geom_point(data = uniquecities_Finder(), aes(x = lon, y = lat, alpha = 0.8)) +
  #         {if(input$show_NamesFinder == "School Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(Team)))} +
  #         {if(input$show_NamesFinder == "City Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(City)))} +
  #         coord_quickmap() +
  #         guides(fill = FALSE) +
  #         geom_point(data = BigTop100_finder(), aes(x = lon, y = lat, color = Division, shape = Sex), alpha = 0.5) +
  #         theme_void() +
  #         labs(color = "Division", shape = "Gender"
  #              #, title = pretty_plot_title()
  #         ) +
  #         {if(length(input$DivisionFinder) <= 1) scale_color_manual(guide = "none", values = c("DI" = "#1E90FF", "DII" = "#FF8D1E", "DIII" = "#20FF1E"))} +
  #         {if(length(input$DivisionFinder) > 1)
  #           scale_color_manual(values = c("DI" = "blue", "DII" = "red", "DIII" = "green"))} +
  #           {if(length(input$GenderFinder) <= 1) scale_shape_manual(guide = "none", values = c("M" = "circle", "F" = "triangle"))} +
  #           {if(length(input$GenderFinder) > 1)
  #             scale_shape_manual(values = c("M" = "circle", "F" = "triangle"))} +
  #         theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  #         theme(plot.title = element_text(hjust=0.5, face = "bold")) +
  #         theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  #         guides(alpha = FALSE) +
  #         theme(legend.text = element_text(size = 12),
  #               legend.title = element_text(size = 15)) +
  #         theme(plot.background = element_rect(
  #           color = "white"
  #         ))
  # 
  #     }
  #   })
  # })
  # 
  # user_clickFinder <- reactiveValues()
  # reactive({
  #   user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(BigTop100), nrow = 1))
  #   names(user_clickFinder$DT) <- colnames(BigTop100)
  # })
  # 
  # observeEvent(input$click_plotFinder, {
  #   add_row <-     nearPoints(BigTop100_finder(), input$click_plotFinder, xvar = "lon", yvar = "lat", threshold = 5)
  #   user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  # })
  # 
  # brushFinder <- reactive({
  #   req(length(user_clickFinder$DT) > 1)
  #   user_clickFinder$DT
  # })
  # 
  # observeEvent({
  #   input$FinderClear
  #   #input$EnterTimes
  # },{
  #   user_clickFinder$DT <- NULL
  # })
  # 
  # output$schoolstableFinder<-DT::renderDataTable({
  # 
  #   DT::datatable(unique(brushFinder()[,c("Name", "Class", "X.swim_time", "Team", "Relative_RankInEvent_Team", "Division", "Address", "Y2019", "Type", "Time")]),
  #                 colnames = c("Sort" = "Time", "Time" = "X.swim_time", "US News School Ranking" = "Y2019", "School Type" = "Type", "Swimmer Rank In Event On Team" = "Relative_RankInEvent_Team"),
  #                 rownames = FALSE,
  #                 options = list(order = list(9, 'asc'),
  #                                columnDefs = list(list(visible=FALSE, targets=c(9)),
  #                                                  list(className = "dt-center", targets = 1:7),
  #                                                  list(classname = "dt-right", targets = 8))
  #                 ))
  # 
  # })
  # 
  # #Program Comparisons
  # 
  # BigTop100_SchoolComp <- reactive({
  #   req(input$SchoolCompGender)
  #   req(input$SchoolSelectA)
  #   req(input$SchoolCompRace)
  #   filter(BigTop100, Sex %in% input$SchoolCompGender) %>%
  #     filter(Event %in% input$SchoolCompRace) %>%
  #     filter(Team %in% input$SchoolSelectA | Team %in% input$SchoolSelectB)
  # 
  # })
  # reactive({
  #   BigTop100_SchoolComp$Time <- as.numeric(format(BigTop100_SchoolComp()$Time, nsmall = 2))
  # })
  # 
  # output$SchoolCompPlotEvent <- renderPlot({
  #   ggplot(data = BigTop100_SchoolComp(), aes(y = Time, x = Team, color = Team)) +
  #     geom_boxplot(outlier.shape = NA) +
  #     geom_jitter(position = position_jitter(width = 0.05), alpha = 0.8) +
  #     scale_color_manual(values=c("#1E90FF", "#20FF1E", "#FF8D1E", "#FD1EFF")) +
  #     theme_minimal() +
  #     labs(x = NULL, y = NULL) +
  #     theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
  #     theme(plot.title = element_text(hjust=0.5, face = "bold")) +
  #     theme(legend.position="none") +
  #     scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
  #     theme(legend.text = element_text(size = 12),
  #           legend.title = element_text(size = 15),
  #           axis.text.x = element_text(size = 15),
  #           axis.text.y = element_text(size = 15))
  # 
  # 
  # })
  # 
  # output$SchoolCompDT<-DT::renderDataTable({
  #   DT::datatable(BigTop100_SchoolComp()[,c("Name", "Team", "X.swim_time", "Class", "Rank", "Division", "Time")],
  #                 colnames = c("Sort" = "Time", "Time" = "X.swim_time"),
  #                 rownames = FALSE,
  #                 options = list(order = list(6, 'asc'),
  #                                columnDefs = list(list(visible=FALSE, targets=6),
  #                                                  list(className = "dt-center", targets = 1:5)
  #                                                  #list(className = "dt-right", targets = 5)
  #                                ))
  # 
  #   )
  # })
  # 
  # 
  #   else if(input$TuitionType == "No"){
  #     DT::datatable(unique(BigTop100_SchoolComp()[,c("Team", "Type", "Y2019", "Tuition_Out", "Enrollment", "Public")]),
  #                   colnames = c("US News Ranking" = "Y2019", "Tuition" = "Tuition_Out"),
  #                   rownames = FALSE,
  #                   options = list(order = list(0, 'asc'),
  #                                  dom = 't',
  #                                  list(columnDefs = list(list(className = "dt-center", targets = 1:5)))
  #                   ))
  #   }
  # })
  # 
  # #Division Comparisons
  # 
  # BigTop100_subsetACA_DI <- reactive({
  #   req(input$GenderDI)
  #   req(input$RegionDI)
  #   req(input$RankDI)
  #   filter(BigTop100, Division == "DI") %>%
  #     filter(Sex %in% input$GenderDI) %>%
  #     filter(Region %in% input$RegionDI) %>%
  #     filter(Rank >= input$RankDI[1], Rank <= input$RankDI[2]) %>%
  #     filter(Y2019 >= input$School_RankDI[1], Y2019 <= input$School_RankDI[2]) %>%
  #     group_by(Team) %>%
  #     dplyr::mutate('No. of Top Times' = n())
  # })
  # 
  # BigTop100_subsetACA_DII <- reactive({
  #   req(input$GenderDII)
  #   req(input$RegionDII)
  #   req(input$RankDII)
  #   filter(BigTop100, Division == "DII") %>%
  #     filter(Sex %in% input$GenderDII) %>%
  #     filter(Region %in% input$RegionDII) %>%
  #     filter(Rank >= input$RankDII[1], Rank <= input$RankDII[2]) %>%
  #     filter(Y2019 >= input$School_RankDII[1], Y2019 <= input$School_RankDII[2]) %>%
  #     group_by(Team) %>%
  #     dplyr::mutate('No. of Top Times' = n())
  # })
  # 
  # BigTop100_subsetACA_DIII <- reactive({
  #   req(input$GenderDIII)
  #   req(input$RegionDIII)
  #   req(input$RankDIII)
  #   filter(BigTop100, Division == "DIII") %>%
  #     filter(Sex %in% input$GenderDIII) %>%
  #     filter(Region %in% input$RegionDIII) %>%
  #     filter(Rank >= input$RankDIII[1], Rank <= input$RankDIII[2]) %>%
  #     filter(Y2019 >= input$School_RankDIII[1], Y2019 <= input$School_RankDIII[2]) %>%
  #     group_by(Team) %>%
  #     dplyr::mutate('No. of Top Times' = n())
  # })
  # 
  # BigTop100_DivCompA <- reactive({
  #   req(input$DivCompGenderA)
  #   req(input$DivCompRankA)
  #   req(input$DivCompRaceA)
  #   filter(BigTop100, Sex %in% input$DivCompGenderA) %>%
  #   filter(Rank >= input$DivCompRankA[1], Rank <= input$DivCompRankA[2]) %>%
  #   filter(Event %in% input$DivCompRaceA)
  # })
  # reactive({
  # BigTop100_DivCompA$Time <- as.numeric(format(BigTop100_DivCompA()$Time, nsmall = 2))
  # })
  # 
  # 
  # output$barplotDI <- renderPlot({
  #   ggplot() +
  #   geom_bar(data = BigTop100_subsetACA_DI(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
  #   labs(y = "Percent", x = "Divison") +
  #   coord_polar("y", start=0) +
  #   scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
  #   theme_void()
  # })
  # 
  # output$description_DI <- renderText({
  #   paste0("Division I is primarily made of national universities, with a sizable subset of regional universities.
  #          There are relatively few colleges.")
  # })
  # 
  # output$barplotDII <- renderPlot({
  #   ggplot() +
  #     geom_bar(data = BigTop100_subsetACA_DII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
  #     labs(y = "Percent", x = "Divison") +
  #     scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
  #     coord_polar("y", start=0) +
  #     theme_void()
  # })
  # 
  # output$description_DII <- renderText({
  #   paste0("Division II is primarily made of regional universities, with a national universities as the second largest component.
  #          There are relatively few national or regional colleges.")
  # })
  # 
  # output$barplotDIII <- renderPlot({
  #   ggplot() +
  #     geom_bar(data = BigTop100_subsetACA_DIII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
  #     labs(y = "Percent", x = "Divison") +
  #     scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
  #     coord_polar("y", start=0) +
  #     theme_void()
  # })
  # 
  # output$description_DIII <- renderText({
  #   paste0("Division III is primarily made of national universities and national liberal arts colleges.
  #         Regional universities and colleges are a smaller component")
  # })
  # 
  # output$DivCompPlotA <- renderPlot({
  #     ggplot(data = BigTop100_DivCompA(), aes(y = Time, x = Division, color = Division)) +
  #     geom_violin() +
  #     geom_jitter(position = position_jitter(width = 0.08), alpha = 0.5, size = 3) +
  #     theme_minimal() +
  #     labs(x = NULL, y = NULL) +
  #     scale_color_manual(values = c("DI" = "#1E90FF", "DII" = "#FF8D1E", "DIII" = "#20FF1E")) +
  #     theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
  #     theme(plot.title = element_text(hjust=0.5, face = "bold")) +
  #     theme(legend.position="none") +
  #     scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
  #     theme(legend.text = element_text(size = 12),
  #           legend.title = element_text(size = 15),
  #           axis.text.x = element_text(size = 15),
  #           axis.text.y = element_text(size = 15))
  # })
  # 
  # #using brush plot
  # 
  # brushDiv <- reactive({
  #   user_brushDiv <- input$brush_plotDiv
  #   brushedPoints(BigTop100_DivCompA(), user_brushDiv, xvar = "Division", yvar =
  #                   "Time")
  # })
  # 
  # observeEvent(input$DivCompClear, {
  #   brushDiv <- NULL
  # })
  # 
  # #using click plot
  # 
  # # user_clickDiv <- reactiveValues()
  # # reactive({
  # #   user_clickDiv$DT <- data.frame(matrix(0, ncol = ncol(BigTop100_DivCompA()), nrow = 1))
  # #   names(user_clickDiv$DT) <- colnames(BigTop100_DivCompA())
  # # })
  # #
  # # observeEvent(input$click_plotDiv, {
  # #   add_row <-     nearPoints(BigTop100_DivCompA(), input$click_plotDiv, xvar = "Division", yvar = "Time", threshold = 8)
  # #   user_clickDiv$DT <- rbind(add_row, user_clickDiv$DT)
  # # })
  # #
  # # brushDiv <- reactive({
  # #   req(length(user_clickDiv$DT) > 1)
  # #   user_clickDiv$DT
  # # })
  # #
  # # observeEvent(input$DivCompClear, {
  # #   user_clickDiv$DT <- NULL
  # # })
  # 
  # output$DivCompTable<-DT::renderDataTable({
  #   DT::datatable(unique(brushDiv()[,c("Name", "Team", "X.swim_time", "Rank", "Division", "Time")]),
  #                 colnames = c("Sort" = "Time", "Time" = "X.swim_time", "Rank In Division" = "Rank"),
  #                 rownames = FALSE,
  #                 options = list(order = list(5, 'asc'),
  #                                columnDefs = list(list(visible=FALSE, targets=c(5)),
  #                                                  list(className = "dt-center", targets = 1:5)
  #                                ))
  #   )
  # })
  
  session$onSessionEnded(stopApp)
}
# Run the application
shinyApp(ui = ui, server = server)
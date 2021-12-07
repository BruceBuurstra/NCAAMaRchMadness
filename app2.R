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
             navbarMenu("Seed/Team/Conference Statistics", icon = icon("chart-bar"),
                        tabPanel("Seed Statistics", fluid = TRUE, icon = icon("list-ol"),
                                 titlePanel("Seed Statistics"),
                                 fluidRow(
                                   column(6,
                                          selectInput(inputId = "seed_2", 
                                                      label = "Seed:",
                                                      choices = c(1:16), 
                                                      selected = 1)
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
                                          tableOutput(outputId = "seedHistory")
                                   )
                                 )
             ),
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
                               checkboxGroupInput(inputId = "RoundSelect2",
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
                               checkboxGroupInput(inputId = "RoundSelect3",
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
                               tableOutput("conferenceHistory2"),
                        )
                      ),
                      hr(),
                      fluidRow(textOutput(outputId = "helpConferenceText2")),
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
                                     fluidRow((plotOutput(outputId = "seedsSpreadHist"))),
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
                                     fluidRow((tableOutput(outputId = "conferencesTable"))),
                                     hr(),
                                     fluidRow(textOutput(outputId = "conferenceText")),
                                     hr(),
                                     br(),
                                     fluidRow((plotOutput(outputId = "conferenceSpreadHist")))
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
                                     fluidRow((plotOutput(outputId = "seedsSpreadHist2"))),
                                     hr()
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
                          br(),
                          actionButton(inputId = "Randomize", label = "Simulate Matchup"),                          
                          hr(),
                        ),
                        mainPanel(
                          fluidRow(
                            column(12,
                            )),
                          hr(),
                          br(),
                          fluidRow(textOutput(outputId = "ProbabilityShow")),
                          hr(),
                          br(),
                          fluidRow(textOutput(outputId = "RunRandom")),
                          hr()
                        )
                      )
             ),
             tabPanel("Upset Insight", icon = icon("eraser"),
                      titlePanel("Upsets"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Upsets"),
                          fluidRow(column(8,
                                          sliderInput(inputId = "year_5",
                                                      label = "Select Year Range",
                                                      min = 1985,
                                                      max = 2021,
                                                      value = c(1985, 2021),
                                                      width = "220px"),
                                          column(8,
                                                 checkboxGroupInput(inputId = "RoundSelect4",
                                                                    label = "Select Round:",
                                                                    choices = unique(conferences$`round name`),
                                                                    selected = unique(conferences$`round name`))
                                          )
                                          
                          ),
                          ),
                          hr(),
                        ),
                        mainPanel(
                          fluidRow(
                            column(12,
                            )),
                          hr(),
                          br(),
                          fluidRow((tableOutput(outputId = "upsetsTable"))),
                          hr(),
                          br(),
                          fluidRow(plotOutput(outputId = "upsetsHistogram")),
                          hr(),
                          br(),
                          fluidRow((tableOutput(outputId = "upsetsText"))),
                          hr(),
                        )
                      )
             )
             )
)


# Define server
server <- function(input, output, session) {
  
  #Data Table
  
  output$matchups <- renderTable({
    if (input$seed1 == input$seed2)
    {
      Big_Dance_Seeds %>%
          filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | 
                   `high seed` %in% input$seed1 & `low seed` %in% input$seed2,
                 Year >= input$year[1],
                 Year <= input$year[2])%>%
          summarise(`# of games` = n())%>%
          select(`# of games`)
    }
    else
    {
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | 
                 `high seed` %in% input$seed1 & `low seed` %in% input$seed2,
               Year >= input$year[1],
               Year <= input$year[2])%>%
        summarise(`# of games` = n(),
                  `win %` = mean(`high seed win`)) %>%
        mutate("# of wins" = as.integer(`win %` * `# of games`)) %>% 
        select(`# of wins`, `# of games`, `win %`)
    }
  }
  )
  
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
  
  output$conferencesTable <- renderTable({req(input$conference1)
    req(input$conference2)
    if(input$conference1 == input$conference2)
    {
      conferences%>%
        filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
                 `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2,
               Year >= input$year_3[1],
               Year <= input$year_3[2])%>%
        mutate("Conf Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1,
                                      `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1,
                                      TRUE ~ 0),
               "Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `winning score`,
                                        `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `winning score`,
                                        TRUE ~ `losing score`),
               "Opp Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `losing score`,
                                            `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `losing score`,
                                            TRUE ~ `winning score`))%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        summarise(Games = n(), `Team Conference Score` = round(mean(`Conf Score`),2), `Opponent Conference Score` = round(mean(`Opp Conf Score`),2), `Average Difference` = round(mean(Difference),2))
    }
    else
    {
      conferences%>%
        filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
               `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2,
               Year >= input$year_3[1],
               Year <= input$year_3[2])%>%
        mutate("Team Conference Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1,
                                    `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1,
                                    TRUE ~ 0),
             "Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `winning score`,
                                      `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `winning score`,
                                      TRUE ~ `losing score`),
             "Opp Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `losing score`,
                                          `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `losing score`,
                                          TRUE ~ `winning score`))%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        group_by(`Team Conference Win`)%>%
        summarise(Games = n(), `Team Conference Score` = round(mean(`Conf Score`),2), `Opponent Conference Score` = round(mean(`Opp Conf Score`),2), `Average Difference` = round(mean(Difference),2))
    }
   }
   )
    
  output$conferenceText <- renderText({
    req(input$conference1)
    req(input$conference2)
    if (input$conference1 == input$conference2){
      "These are the same conferences."
    }
    else{
      paste("The ", input$conference1 , " conference has beaten the ", input$conference2, " conference ", as.numeric(conferences%>%
                                                                                                                       filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
                                                                                                                                `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2,
                                                                                                                              Year >= input$year_3[1],
                                                                                                                              Year <= input$year_3[2])%>%
                                                                                                                       mutate("Conf Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1L,
                                                                                                                                                     `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1L,
                                                                                                                                                     TRUE ~ 0L))%>%
                                                                                                                       summarise(`win %` = round(mean(`Conf Win`) * 100,2))), "% of the time  between ", input$year_3[1], " and ", input$year_3[2], sep = "")
    }
  })
  
  output$conferenceSpreadHist <- renderPlot({
    req(input$conference1)
    req(input$conference2)
    if (input$conference1 == input$conference2){
      conferences%>%
        filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
                 `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2,
               Year >= input$year_3[1],
               Year <= input$year_3[2])%>%
        mutate("Conf Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1,
                                      `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1,
                                      TRUE ~ 0),
               "Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `winning score`,
                                        `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `winning score`,
                                        TRUE ~ `losing score`),
               "Opp Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `losing score`,
                                            `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `losing score`,
                                            TRUE ~ `winning score`))%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()
    }
    else{
      win_names = c(`0` = "Opponent Conference Win", `1` = "Team Conference Win")
      conferences%>%
        filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
                 `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2,
               Year >= input$year_3[1],
               Year <= input$year_3[2])%>%
        mutate("Team Conference Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1,
                                      `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1,
                                      TRUE ~ 0),
               "Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `winning score`,
                                        `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `winning score`,
                                        TRUE ~ `losing score`),
               "Opp Conf Score" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ `losing score`,
                                            `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ `losing score`,
                                            TRUE ~ `winning score`))%>%
        mutate(Difference = abs(`high seed score` - `low seed score`))%>%
        ggplot(aes(Difference))+
        geom_histogram()+
        facet_wrap(~ `Team Conference Win`, ncol = 1, labeller = as_labeller(win_names))
    }
  })
  
  #tables for team comparisons
  output$SchoolHistory1 <- renderTable({req(input$SchoolSelectA[1])
    req(input$RoundSelect2)
    Big_Dance_Seeds %>% filter(`high seed team` == input$SchoolSelectA[1] | `low seed team` == input$SchoolSelectA[1], `round name` %in% input$RoundSelect2) %>% 
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
    req(input$RoundSelect2)
    Big_Dance_Seeds %>% filter(`high seed team` == input$SchoolSelectA[2] | `low seed team` == input$SchoolSelectA[2], `round name` %in% input$RoundSelect2) %>% 
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
    req(input$RoundSelect3)
    conferences %>% filter(`High Seed Conference` == input$conferenceSelect[1] | `Low Seed Conference` == input$conferenceSelect[1], `round name` %in% input$RoundSelect3) %>% 
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
    req(input$RoundSelect3)
    conferences %>% filter(`High Seed Conference` == input$conferenceSelect[2] | `Low Seed Conference` == input$conferenceSelect[2], `round name` %in% input$RoundSelect3) %>% 
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
  
  output$helpConferenceText <- renderText({
    "Note: The Conferences do not reflect Teams that have changed Conferences in the past and only represent Teams and their current Conferences."
  })
  
  output$helpConferenceText2 <- renderText({
    "Note: The Conferences do not reflect Teams that have changed Conferences in the past and only represent Teams and their current Conferences."
  })
  
  #Seed History Table
  output$seedHistory <- renderTable({
    req(input$seed_2)
    req(input$RoundSelect)
    Big_Dance_Seeds %>% filter(`high seed` == input$seed_2 | `low seed` == input$seed_2, `round name` %in% input$RoundSelect) %>% 
         mutate("Win" = case_when((`high seed` == input$seed_2 & `high seed score` > `low seed score`) | (`low seed` == input$seed_2 & `low seed score` > `high seed score`) ~ 1,
                                  (`high seed` == input$seed_2 & `high seed score` < `low seed score`) | (`low seed` == input$seed_2 & `low seed score` < `high seed score`) ~ 0),
                "Championship" = case_when((`high seed` == input$seed_2 & `high seed score` > `low seed score` & Round == 6) | (`low seed` == input$seed_2 & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                           TRUE ~ 0),
                "Champion" = case_when((`high seed` == input$seed_2 & `high seed score` > `low seed score` & Round == 5) | (`low seed` == input$seed_2 & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                       TRUE ~ 0),
                "Final Four" = case_when((`high seed` == input$seed_2 & `high seed score` > `low seed score` & Round == 4) | (`low seed` == input$seed_2 & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                         TRUE ~ 0),
                "PF" = case_when(`high seed` == input$seed_2 ~ `high seed score`,
                                 `low seed` == input$seed_2 ~ `low seed score`),
                "PA" = case_when(`high seed` == input$seed_2 ~ `low seed score`,
                                 `low seed` == input$seed_2 ~ `high seed score`)) %>% 
         summarise("Games Played" = n(),
                   "win%" = mean(Win),
                   "# of wins" = `Games Played` * `win%`,
                   "# of losses" = `Games Played` - `# of wins`,
                   "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
                   "Championships Won" = mean(Championship) * n(),
                   "Championships Made" = mean(Champion) * n(),
                   "Final Fours" = mean(`Final Four`) * n(),
                   "Average Points Scored" = round(mean(PF),2),
                   "Average Points Allowed" = round(mean(PA),2)) %>% 
          mutate("Seed" = input$seed_2)%>%
          select(Seed, `Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`)%>%
          gt()
  })
  
  output$upsetsTable <- renderTable({
    req(input$RoundSelect4)
    Big_Dance_Seeds%>%
      filter(`high seed` != `low seed`, 
             `round name` %in% input$RoundSelect4,
             Year >= input$year_5[1],
             Year <= input$year_5[2])%>%
      mutate(Upset = case_when(`low seed score` > `high seed score` ~ 1,
                               TRUE ~ 0))%>%
      group_by(Year)%>%
      summarise(Upset = sum(Upset))%>%
      summarise(Min = min(Upset), `1Q` = summary(Upset)[["1st Qu."]] , Median = median(Upset),`3Q` = summary(Upset)[["3rd Qu."]], Median = median(Upset), Max = max(Upset), Mean = mean(Upset), StandDev = sd(Upset))
  })
  
  output$upsetsHistogram <- renderPlot({
    req(input$RoundSelect4)
    Big_Dance_Seeds%>%
      filter(`high seed` != `low seed`, 
             `round name` %in% input$RoundSelect4,
             Year >= input$year_5[1],
             Year <= input$year_5[2])%>%
      mutate(Upset = case_when(`low seed score` > `high seed score` ~ 1,
                               TRUE ~ 0))%>%
      group_by(Year)%>%
      summarise(Upset = sum(Upset))%>%
      ggplot(aes(x = Upset))+
      labs(title = "Total Upsets per Year based off Rounds Selected")+
      geom_histogram(binwidth = 1)
  })
  
  output$upsetsText <- renderText({
    req(input$RoundSelect4)
    paste("For this round, or these rounds, there has been an upset ", as.numeric(
    Big_Dance_Seeds%>%
      filter(`high seed` != `low seed`, 
             `round name` %in% input$RoundSelect4,
             Year >= input$year_5[1],
             Year <= input$year_5[2])%>%
      mutate(Upset = case_when(`low seed score` > `high seed score` ~ 1,
                               TRUE ~ 0))%>%
      group_by(Year)%>%
      summarise(Upset = mean(Upset))%>%
      summarise(`Upset%` = round(mean(Upset)*100, 2))), "% of the time between ", input$year_5[1], " and ", input$year_5[2], sep = "")
  })
  
  # Simulator tab output
  
  output$ProbabilityShow <- renderText({
    if(input$seed1_8 == input$seed2_8) {
      "These teams are the same seed, so this will be a 50/50 matchup"
    }
    else {
      paste("The ", case_when(as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed1_8,
                              as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed2_8), " seed will have a ", round(Team_Probability() * 100, 2),
            "% chance of beating the ", case_when(as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed1_8,
                                                  as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed2_8), " seed", sep = "")
    }
  })
  observeEvent(input$Randomize,{
    Number <- runif(1, min = 0, max = 1)
    output$RunRandom <- renderText({ if(input$seed1_8 == input$seed2_8) {
      "These are the same seed, we can't really help you here"
      }
      else if(Number < as.numeric(Team_Probability())) {
      paste("The ", case_when(as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed1_8,
                              as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed2_8,
                              TRUE ~ input$seed1_8), " seed will win against the ", case_when(as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed1_8,
                                                                                              as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed2_8,
                                                                                              TRUE ~ input$seed2_8), " seed")
    } 
    else {
      paste("The ", case_when(as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed1_8,
                              as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed2_8,
                              TRUE ~ input$seed2_8), " seed will win against the ", case_when(as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed1_8,
                                                                                              as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed2_8,
                                                                                              TRUE ~ input$seed1_8), " seed")
    }  
                                        
      })
  })
  
  Team_Probability <- reactive({
    Big_Dance_Seeds %>%
      na.omit()%>%
      group_by(`high seed`, `low seed`)%>%
      summarise(`win %` = mean(`high seed win`), wins = sum(`high seed win`), games = n()) %>%
      select(`high seed`, `low seed`,`win %`, wins, games) %>%
      mutate(prob = case_when(`high seed`==`low seed` ~.5,
                              TRUE ~ `win %`),
             new_wins = case_when(wins == 0 ~ wins +2,
                                  wins == games ~ wins +2,
                                  TRUE ~ wins), 
             new_games = case_when(wins == 0 ~ games +4L,
                                   wins == games ~ games +4L,
                                   TRUE ~ games),
             new_prob = case_when(`high seed`==`low seed` ~.5,
                                  TRUE ~ new_wins/new_games)) %>%
      filter(`high seed` == case_when(as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed1_8,
                                      as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed2_8,
                                      TRUE ~ input$seed1_8),
             `low seed` == case_when(as.numeric(input$seed1_8) > as.numeric(input$seed2_8) ~ input$seed1_8,
                                     as.numeric(input$seed1_8) < as.numeric(input$seed2_8) ~ input$seed2_8,
                                     TRUE ~ input$seed2_8)) %>% ungroup() %>% select(new_prob)
  })
  
  
  session$onSessionEnded(stopApp)
}
# Run the application
shinyApp(ui = ui, server = server)



library(tidyverse)
library(readr)
library(shiny)
library(gt)
library(gtExtras)
Big_Dance_CSV <- read_csv("Big_Dance_CSV.csv")
shinyApp(
  ui = fluidPage(
    selectInput("seed1", "Team Seed:",
                c(1:16)),
    selectInput("seed2", "Opponent Seed:",
                c(1:16)),
    sliderInput(inputId = "year",
                label = "Start Year:",
                min = 1985,
                max = 2021,
                value = 1985),
    print("hello world"),
    
    tableOutput("data"),
    textOutput("text"),
  ),
  server = function(input, output) {
    output$data <- renderTable({
      Big_Dance_Seeds %>%
        filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | `high seed` %in% input$seed1 & `low seed` %in% input$seed2, Year >= input$year) %>%
        na.omit() %>% 
        summarise(`# of games` = n(),
                  `win %` = mean(`high seed win`)) %>%
        mutate("# of wins" = `win %` * `# of games`) %>% 
        select(`# of wins`, `# of games`, `win %`) %>% 
        gt() 
    }, bordered = T, rownames = FALSE)
    
    output$text <- renderText(paste("The ", case_when(input$seed1 < input$seed2 ~ as.numeric(input$seed1),
                                                      input$seed1 > input$seed2 ~ as.numeric(input$seed2)), " seed has beaten the ", case_when(input$seed1 > input$seed2 ~ as.numeric(input$seed1),
                                                                                                                                               input$seed1 < input$seed2 ~ as.numeric(input$seed2)), " seed ", as.numeric(
                                                                                                                                                 Big_Dance_Seeds %>%
                                                                                                                                                   filter(`low seed` %in% input$seed1 & `high seed` %in% input$seed2 | `high seed` %in% input$seed1 & `low seed` %in% input$seed2, Year >= input$year) %>%
                                                                                                                                                   na.omit() %>% 
                                                                                                                                                   summarise(`win %` = mean(`high seed win`) * 100)), "% of the time since ", input$year, sep = ""))
  }
)
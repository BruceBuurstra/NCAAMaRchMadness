library(tidyverse)
library(readr)
library(shiny)
library(gt)
library(gtExtras)
Big_Dance_CSV <- read_csv("Big_Dance_CSV.csv")
Big_Dance_Seeds <- Big_Dance_CSV %>% 
  mutate("high seed" = case_when(Seed < Seed_1 ~ Seed,
                                 Seed_1 < Seed ~ Seed_1,
                                 Seed == Seed_1 ~ Seed)) %>% 
  mutate("low seed" = case_when(Seed > Seed_1 ~ Seed,
                                 Seed_1 > Seed ~ Seed_1,
                                 Seed == Seed_1 ~ Seed))
shinyApp(
  ui = fluidPage(
    selectInput("seed1", "Team Seed:",
                c(1:16)),
    selectInput("seed2", "Opponent Seed:",
                c(1:16)),
    sliderInput(inputId = "start year",
                label = "Number of bins:",
                min = 1985,
                max = 2021,
                value = 1985),
    print("hello world"),
    tableOutput("data"), 
  ),
  server = function(input, output) {
    output$data <- renderTable({
      Big_Dance_CSV %>%
        filter(`Seed` %in% input$seed1 & `Seed_1` %in% input$seed2 | `Seed_1` %in% input$seed1 & `Seed` %in% input$seed2, Year >= input$`start year`) %>% 
        gt()
    }, rownames = TRUE)
  }
)

library(tidyverse)
library(shinythemes)
library(readr)
Big_Dance_CSV <- read_csv("Big_Dance_CSV.csv")
shinyApp(
  ui = fluidPage(
    navbarPage("NCAA Swimming", theme = shinytheme("lumen")),
    selectInput("seed1", "Team Seed:",
                c(1:16)),
    selectInput("seed2", "Opponent Seed:",
                c(1:16)),
    print("hello people"),
    tableOutput("data"), 
  ),
  server = function(input, output) {
    output$data <- renderTable({
      Big_Dance_CSV %>%
        filter(`Seed` %in% input$seed1 & `Seed_1` %in% input$seed2 | `Seed_1` %in% input$seed1 & `Seed` %in% input$seed2)
    }, rownames = TRUE)
  }
)

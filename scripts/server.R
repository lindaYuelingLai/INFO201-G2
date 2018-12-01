library(shiny)
library(dplyr)
library(ggplot2)
shooting_data <- read.csv("shootings_data.csv", stringsAsFactors = FALSE)

server <- function(input, output) {
  # get the subset for the selected variable
  race_data <- reactive({
    shooting_data %>% group_by(race) %>% summarise(n = n()) %>% arrange(desc(n))
  })
}

shinyServer(server)
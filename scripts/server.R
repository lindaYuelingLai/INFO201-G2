library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  shooting_data <- read.csv("shootings_data.csv", stringsAsFactors = FALSE)
}

shinyServer(server)
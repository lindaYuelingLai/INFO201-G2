library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  shooting_data <- read.csv("shootings_data.csv", stringsAsFactors = FALSE)
  
  # return bar graph for the specified race, grouped by state
  race_by_state <- function(the_race) {
    if (the_race == "W") title <- "White"
    if (the_race == "B") title <- "Black"
    if (the_race == "H") title <- "Hispanic"
    if (the_race == "A") title <- "Asian"
    if (the_race == "N") title <- "Native American"
    race_data <- filter(shooting_data, race == the_race)
    race_data <- race_data %>% group_by(state) %>% summarise(n = n()) %>% arrange(desc(n))
    result_plot <- ggplot(race_data, aes(race_data, x = state, y = n)) +
      geom_bar(stat="identity", width = 1) +
      labs(
        title = paste0("Fatal Shootings by State, ", title),
        x = "State",
        y = "Reports"
      ) +
      theme(axis.text.x=element_text(size=rel(1), angle=90))
    return(result_plot)
  }
  
  # get the subset for the selected variable
  race_data <- reactive({
    shooting_data %>% group_by(race) %>% summarise(n = n()) %>% arrange(desc(n))
  })
}

shinyServer(server)
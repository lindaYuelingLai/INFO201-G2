library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  shooting_data <- read.csv("../shootings_data.csv", stringsAsFactors = FALSE)
  
  # return bar graph for the specified race, grouped by state
  output$statePlot <- renderPlot({
    race_by_state <- function(the_race) {
      title <- ""
      race_data <- shooting_data
      if (the_race == "all") {
        title <- "All Races"
      } else {
        if (the_race == "W") title <- "White"
        if (the_race == "B") title <- "Black"
        if (the_race == "H") title <- "Hispanic"
        if (the_race == "A") title <- "Asian"
        if (the_race == "N") title <- "Native American"
        race_data <- filter(race_data, race == the_race)
      }
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
    race_by_state(input$race)
  })
  
  # get the subset for the selected variable
  race_data <- reactive({
    shooting_data %>% group_by(race) %>% summarise(n = n()) %>% arrange(desc(n))
  })
  
  race_by_armed <- function(the_race) {
    title <- ""
    race_data <- shooting_data
    if (the_race == "all") {
      title <- "All Races"
    } else {
      if (the_race == "W") title <- "White"
      if (the_race == "B") title <- "Black"
      if (the_race == "H") title <- "Hispanic"
      if (the_race == "A") title <- "Asian"
      if (the_race == "N") title <- "Native American"
      race_data <- filter(race_data, race == the_race)
    }
    armed_data <- filter(race_data, race == the_race)
    armed_data[armed_data != "unarmed" & armed_data != "gun"] <- "other"
    armed_data <- armed_data%>% group_by(armed) %>% summarise(n = n()) %>% arrange(desc(n))
    result_plot <- ggplot(armed_data, aes(armed_data, x = armed, y = n)) +
      geom_bar(stat="identity", width = 1) +
      labs(
        title = paste0("Fatal Shootings by Armed / Unarmed, ", title),
        x = "Armed?",
        y = "Reports"
      ) +
      theme(axis.text.x=element_text(size=rel(1), angle=90))
    return(result_plot)
  }
  
  race_by_mental_illness <- function(the_race) {
    title <- ""
    race_data <- shooting_data
    if (the_race == "all") {
      title <- "All Races"
    } else {
      if (the_race == "W") title <- "White"
      if (the_race == "B") title <- "Black"
      if (the_race == "H") title <- "Hispanic"
      if (the_race == "A") title <- "Asian"
      if (the_race == "N") title <- "Native American"
      race_data <- filter(race_data, race == the_race)
    }
    mi_data <- filter(race_data, race == the_race)
    mi_data <- mi_data%>% group_by(signs_of_mental_illness) %>% summarise(n = n()) %>% arrange(desc(n))
    result_plot <- ggplot(mi_data, aes(signs_of_mental_illness, x = signs_of_mental_illness, y = n)) +
      geom_bar(stat="identity", width = 1) +
      labs(
        title = paste0("Fatal Shootings by Perceived Mental Illness: ", title),
        x = "Mental Illness",
        y = "Reports"
      ) +
      theme(axis.text.x=element_text(size=rel(1), angle=90))
    return(result_plot)
  }
  
  # get the subset for the selected variable
  race_data <- reactive({
    shooting_data %>% group_by(race) %>% summarise(n = n()) %>% arrange(desc(n))
  })
  
  race_by_flee <- function(the_race) {
    if (the_race == "W") title <- "White"
    if (the_race == "B") title <- "Black"
    if (the_race == "H") title <- "Hispanic"
    if (the_race == "A") title <- "Asian"
    if (the_race == "N") title <- "Native American"
    mi_data<- filter(shooting_data, race == the_race)
    mi_data <- mi_data%>% group_by(flee) %>% summarise(n = n()) %>% arrange(desc(n))
    result_plot <- ggplot(mi_data, aes(flee, x = flee, y = n)) +
      geom_bar(stat="identity", width = 1) +
      labs(
        title = paste0("Fatal Shootings by Fleeing: ", title),
        x = "Fleeing",
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
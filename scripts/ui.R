library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Fatal Police Shootings in the US"),
  
  sidebarLayout(
    sidebarPanel(
      
    )
  ),
  mainPanel(
    plotOutput("")
  )
)

shinyUI(ui)
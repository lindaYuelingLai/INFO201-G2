library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Fatal Police Shootings in the US"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("race", h3("Select a race:"),
                   c("All" = "all","White" = "white","Black" = "black","Asian"="asian",
                     "Native American"="native","Hispanic"="hispanic"),
      selectInput("factors", h3("Other factors describing victim:"), 
                  c("Armed"="arm","Signs of Mental Illness"="mental","Threat Level"="threat","Fleeing"="flee"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      )
    )
  ),
  mainPanel(
  plotOutput("distPlot")
  )
)

shinyUI(ui)
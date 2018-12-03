library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Fatal Police Shootings in the US"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("race", h3("Select a race:"),
                   c("All" = "all","White" = "W","Black" = "B","Asian"="A",
                     "Native American"="N","Hispanic"="H")),
      selectInput("factors", h3("Other factors describing victim:"), 
                  c("Armed"="armed","Signs of Mental Illness"="mental","Threat Level"="threat","Fleeing"="flee"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      ),
    mainPanel(
      plotOutput("statePlot")
    )
  )
)

shinyUI(ui)
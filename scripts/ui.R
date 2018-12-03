library(shiny)
library(dplyr)
library(ggplot2)

ui <- pageWithSidebar(
  # Application title
  headerPanel("Fatal Police Shootings in the US"),
  
  sidebarPanel(
    radioButtons("race", h3("Select a race:"),
                 c("All" = "all","White" = "W","Black" = "B","Asian"="A",
                   "Native American"="N","Hispanic"="H")),
    
    br(),
    
    selectInput("factors", h3("Other factors describing victim:"), 
                c("Armed"="armed","Signs of Mental Illness"="mental","Threat Level"="threat","Fleeing"="flee"), 
                selected = NULL, multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL)
  ),
  
  # Show a tabset that includes a summary, two plots
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", textOutput("summary")),
      tabPanel("Plot (race as a variable)", plotOutput("statePlot"), plotOutput("factorsPlot"))
    )
  )
)

shinyUI(ui)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

ui <- navbarPage(title = "GROUP AF3",
             theme = shinytheme("flatly"),
             tabPanel("Overview",
                      mainPanel(
                        tags$h1("Overview of the Data"),
                        tags$h3(strong("Intro")),
                        tags$p("Our dataset encompasses fatal shootings by the police force in the U.S., 
                          and we have grouped the data by race to see if we can find a correlation 
                          between police and racism, particularly towards people of color.  Police 
                          violence has become a nationwide issue after the 2014 death of Michael 
                          Brown and spurred movements, such as Black Lives Matter."),
                        tags$p("Our data comes from Washington Post's gathering of news reports, police 
                               departments, and social media. It documented police shootings since January 1, 
                               2015 and gathers present updates. We downloaded the dataset on December 1, 2018, 
                               so our data relates to shootings only up until that date.
                               We have filtered our dataset based on 
                               racial groups and the following factors:"),
                        tags$ul(
                          tags$li("Armed / Unarmed"),
                          tags$li("Perceived Mental Illness"),
                          tags$li("Perceived Threat Level"),
                          tags$li("Fleeing / Not Fleeing")
                        ),
                        tags$p(HTML(paste0("The link to the dataset can be found ", 
                                      a(href="https://www.washingtonpost.com/graphics/national/police-shootings-2016/", 
                                        "here"), "."))),
                        tags$h3(strong("Audience")),
                        tags$p("In choosing who our audience would be, we considered the current political
                               and social climate in the United States. So, our target audience is anyone 
                               who may be unaware of systemic racism and how it directly affects the 
                               lives of certain minorities. Specifically, we are aiming to educate 
                               those who benefit from white privilege or who are fortunate enough not 
                               to have experienced such prejudice regarding skin color. In displaying 
                               data on race and fatal police shootings, one can see how even in the 
                               eyes of the law, racism can come into play. Seeing this data makes it 
                               more tangible and allows for viewers to see the facts and statistics 
                               behind a prevalent issue that is very easy to look past if you are not 
                               someone directly affected by it"),
                        tags$h3(strong("Questions")),
                        tags$p("In discovering this information, a goal of ours is that our audience 
                               will better understand the magnitude of this problem and will continue 
                               to spread awareness about it through educating others. In doing so, 
                               society can gradually become more socially aware of systemic 
                               racism and we can work to end racism as a whole. Our audience hopes to 
                               learn more about the following questions regarding this topic:"),
                        tags$ul(
                          tags$li("Who is being disproportionately targeted in police brutality and killing incidents?"),
                          tags$li("What was the incident (a crime, attack, intent, or mere interaction) within 
                                  these situations? Does the data tell us about police brutality or self-defense?"),
                          tags$li("What can the data tell us about police and civilian relationships depending on race?")
                        )
                      )),
             tabPanel("Bar Plots",
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
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Bar Plots", plotOutput("statePlot"), plotOutput("factorsPlot")),
                          tabPanel("Plot Info", textOutput("summary"))
                        )
                      )),
             tabPanel("Other Visuals",
                      headerPanel("Fatal Police Shootings in the US"),
                      sidebarPanel(
                        h4("About the Graphs"),
                        helpText("summary info about pie chart here? 
                                 + map graph with dots where shootings have occurred?")
                      ),
                      mainPanel(
                        plotOutput("racePlot")
                      )),
             tabPanel("Summary",
                      mainPanel(
                        tags$h1("Summary"),
                        tags$h3(strong("Data Analytics Hypotheses")),
                        tags$p("We were surprised to see graphs with more shootings of white people 
                               than shootings of people of color, given the movements and protests 
                               that are fighting against disproportionate police violence against POC."),
                        tags$p("We hypothesized a few causes for these results:"),
                        tags$ul(
                          tags$li(strong("Population:"),
                                  tags$ul(
                                  tags$li("Greater number of total white people in certain areas, 
                                  leading to the higher likelihood of shooting a white person")
                                  )),
                          tags$li(strong("Unreported:"),
                                  tags$ul(
                                  tags$li("More police activity is being reported by body cameras, 
                                  but a lot of our data comes from self-reporting within the police force. 
                                  Who is reporting? Are all police reporting?")
                                  )),
                          tags$li(strong("Media Reporting / Risk:"), 
                                  tags$ul(
                                  tags$li("With large broadcasting on national and social 
                                  media platforms, did police becoming hesitant to actually shoot people 
                                  of color at risk of being seen as racist?")
                                  ))
                        ),
                        tags$h3(strong("Research")),
                        tags$p(HTML(paste0("This ", a(href="https://www.nytimes.com/2016/07/12/upshot/surprising-new-evidence-shows-bias-in-police-use-of-force-but-not-in-shootings.html",
                                                 "NY Times article"), 
                                      " states an interesting concept that aligns with our data. Although police brutality
                                      disproportionally affects black people more, 'brutality' only includes police using hands,
                                      pushing people into walls, using handcuffs, drawing weapons, pushing people to the 
                                      ground, pointing weapons, and using pepper spray, but NOT shooting to kill the subjects."))),
                        tags$ul(
                          tags$li("In the first dataset from police reports, black people are 17-25% more  
                                  likely than white people to encounter this treatment."),
                          tags$li("In the second dataset from civilian reports, black people are 170-305% 
                                  more likely to encounter gun pointing, handcuffs, grabbing, kicked, or 
                                  subject to a stun gun. The 305% more likely category refers to having 
                                  a gun pointed.")
                        ),
                        tags$p(HTML(paste0("This ", a(href="https://www.washingtonpost.com/news/post-nation/wp/2016/07/11/arent-more-white-people-than-black-people-killed-by-police-yes-but-no/?utm_term=.bec825db615c", 
                                          "Washington Post article"), ", 'Aren't more white people killed 
                                          than black people killed by police? Yes, but no.' directly 
                                          references the dataset we used and answers the same questions 
                                          were thinking."))),
                        tags$ul(
                          tags$li("Data scientists note on our dataset that comparing just white people 
                                  and black people by the police is 'statistically dubious.' Affirming 
                                  one of our hypotheses, data analytics must adjust for racial 
                                  distribution per population."),
                          tags$li("White people make up 62% of the U.S. population, but 49% of those 
                                  are killed by police officers. Black people make up 13% of the 
                                  population, but 24% account for those fatally shot by police.",
                                  tags$ul(
                                    tags$li("Because of the population distributions, researchers 
                                            conclude that unarmed black Americans are 5x as likely 
                                            to be shot and killed by police officers than white Americans.")
                                  )
                                  )
                        ),
                        tags$h3(strong("Conclusion")),
                        tags$p("Looking solely at our charts and numbers alone, white deaths by 
                               police shootings are the highest. However, this does not accurately 
                               represent the proportions by which people are shot and killed by 
                               police according to their race. Therefore, factoring in our population
                               as a whole, we can conclude that people of color are typically more
                               targeted by police than their white peers.")
                      ))
  )

shinyUI(ui)
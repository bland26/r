fluidPage(
  tabsetPanel(
    tabPanel("Polar", fluid = TRUE,
      pageWithSidebar(
                headerPanel('Scouting Report'),
                sidebarPanel(width = 4,
                             
                             checkboxGroupInput(inputId = "teams",
                                                label = 'Choose up to 10 teams:', choiceNames = c(scoutData$robot),
                                                choiceValues = c(scoutData$robot),
                                                selected = scoutData %>% filter(robot %in% 8044 )
                                                ,inline=TRUE),
                             submitButton("Update filters")
                ),
                mainPanel(
                    column(8, plotlyOutput("plot1", width = 900, height=700)
                           

                    )

                )
            )
    ),
    tabPanel("Heatmap", fluid = TRUE,
             pageWithSidebar(
               headerPanel('Heatmap'),
               sidebarPanel(width = 3,
                            selectInput(inputId = 'team', label = 'List of Teams:',scoutData$robot, selected = scoutData %>% filter(robot %in% 8044 ) ),
                            
                            
                            submitButton("Update filters")
               ),
               mainPanel(
                 column(8, plotlyOutput("heatAuto", width = 900, height=300)),
                 column(5, plotlyOutput("heatTele", width = 900, height=300))
                 ))),
    tabPanel("Match Record", fluid = TRUE,
             pageWithSidebar(
               headerPanel('Matches'),
               sidebarPanel(width = 3,
                            selectInput("a","List of Teams:",oppData, selected = 8044
                                        ),
                            
                            submitButton("Update filters")
                   ),
               mainPanel(
               column(8, plotOutput("mr", width = 900, height = 700))
                   
               ))),
    tabPanel("Comments", fluid = TRUE,
             pageWithSidebar(
               headerPanel('Scout Comments'),
               sidebarPanel(width = 3,
                            selectInput(inputId = 'comTeam', label = 'List of Teams:',comData$robot, selected = comData %>% filter(robot %in% 8044 ) ),
                            
                            
                            submitButton("Update filters")
               ),
               mainPanel(
                  htmlOutput("com", inline = FALSE))
             ))
   )
)

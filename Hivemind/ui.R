fluidPage(
  titlePanel("Hivemind"),
  tabsetPanel(
    tabPanel("Full Season",
        tabsetPanel(
          tabPanel("Teams", fluid = TRUE,
                   
                  tableOutput("teamlist")
                   
                   ),
          tabPanel("Polar", fluid = TRUE,
                   pageWithSidebar(
                     headerPanel('Scouting Report'),
                     sidebarPanel(width = 4,
                                  checkboxGroupInput(inputId = "teams",
                                                     label = 'Choose up to 10 teams:', choiceNames = c(scoutData$robot),
                                                     choiceValues = c(scoutData$robot),
                                                     selected = 8044
                                                     
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
                                  selectInput(inputId = 'team', label = 'List of Teams:',scoutData$robot, 
                                              selected = 8044
                                              
                                              ),
                                  
                                  
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
                                  selectInput(inputId = 'comTeam', label = 'List of Teams:',comData$robot, 
                                              
                                              selected = comData %>% filter(robot %in% 8044 ) 
                                              ),
                                  
                                  
                                  submitButton("Update filters")
                     ),
                     mainPanel(
                        htmlOutput("com", inline = FALSE))
                   ))
         )),
    tabPanel("Magnolia",
             tabsetPanel(
               tabPanel("Teams", fluid = TRUE,

                        tableOutput("teamlistM")

               ),
               tabPanel("Polar", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Scouting Report'),
                          sidebarPanel(width = 4,
                                       checkboxGroupInput(inputId = "teamsM",
                                                          label = 'Choose up to 10 teams:', choiceNames = c(scoutDataM$robot),
                                                          choiceValues = c(scoutDataM$robot),
                                                          selected = 8044

                                                          ,inline=TRUE),
                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotlyOutput("plotM", width = 900, height=700)


                            )

                          )
                        )
               ),
               tabPanel("Heatmap", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Heatmap'),
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'teamM', label = 'List of Teams:',scoutDataM$robot,
                                                   selected = 8044

                                       ),


                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotlyOutput("heatAutoM", width = 900, height=300)),
                            column(5, plotlyOutput("heatTeleM", width = 900, height=300))
                          ))),
               tabPanel("Match Record", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Matches'),
                          sidebarPanel(width = 3,
                                       selectInput("aM","List of Teams:",oppDataM, selected = 8044
                                       ),

                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotOutput("mrM", width = 900, height = 700))

                          ))),
               tabPanel("Comments", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Scout Comments'),
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'comTeamM', label = 'List of Teams:',comDataM$robot,

                                                   selected = comDataM %>% filter(robot %in% 8044 )
                                       ),


                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            htmlOutput("comM", inline = FALSE))
                        ))
             )),
    tabPanel("Bayou",
             tabsetPanel(
               tabPanel("Teams", fluid = TRUE,

                        tableOutput("teamlistB")

               ),
               tabPanel("Polar", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Scouting Report'),
                          sidebarPanel(width = 4,
                                       checkboxGroupInput(inputId = "teamsB",
                                                          label = 'Choose up to 10 teams:', choiceNames = c(scoutDataB$robot),
                                                          choiceValues = c(scoutDataB$robot),
                                                          selected = 8044

                                                          ,inline=TRUE),
                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotlyOutput("plotB", width = 900, height=700)


                            )

                          )
                        )
               ),
               tabPanel("Heatmap", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Heatmap'),
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'teamB', label = 'List of Teams:',scoutDataB$robot,
                                                   selected = 8044

                                       ),


                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotlyOutput("heatAutoB", width = 900, height=300)),
                            column(5, plotlyOutput("heatTeleB", width = 900, height=300))
                          ))),
               tabPanel("Match Record", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Matches'),
                          sidebarPanel(width = 3,
                                       selectInput("aB","List of Teams:",oppDataB, selected = 8044
                                       ),

                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            column(8, plotOutput("mrB", width = 900, height = 700))

                          ))),
               tabPanel("Comments", fluid = TRUE,
                        pageWithSidebar(
                          headerPanel('Scout Comments'),
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'comTeamB', label = 'List of Teams:',comDataB$robot,

                                                   selected = comDataB %>% filter(robot %in% 8044 )
                                       ),


                                       submitButton("Update filters")
                          ),
                          mainPanel(
                            htmlOutput("comB", inline = FALSE))
                        ))
             ))
    )
)

fluidPage(theme = shinytheme("slate"),
          titlePanel(title=div(img(src='  https://tse3.mm.bing.net/th/id/OIP.DZVqJx3X9XGAGmXd-gqOdgAAAA?rs=1&pid=ImgDetMain',height = 50, width = 50), "Hivemind")),
          tabsetPanel(
            tabPanel("Scouting",
                     tabsetPanel(
                       tabPanel("Teams", fluid = TRUE,
                                
                                tableOutput("teamlistW")
                                
                       ),
                       tabPanel("Match Schedule", fluid = TRUE,
                                
                                tableOutput("msW")
                                
                       ),
                       tabPanel("Match Record", fluid = TRUE,
                                pageWithSidebar(
                                  headerPanel('Matches'),
                                  sidebarPanel(width = 3,
                                               selectInput("aW","List of Teams:",oppDataW, selected = 8044
                                               ),
                                               
                                               submitButton("Update filters")
                                  ),
                                  mainPanel(
                                    column(8, plotlyOutput("mrW", width = 900, height = 900))
                                    
                                  ))),
                       tabPanel("Polar", fluid = TRUE,
                                pageWithSidebar(
                                  headerPanel('Scouting Report'),
                                  sidebarPanel(width = 4,
                                               checkboxGroupInput(inputId = "teamsW",
                                                                  label = 'Choose up to 10 teams:', choiceNames = c(scoutDataW$robot),
                                                                  choiceValues = c(scoutDataW$robot),
                                                                  selected = 8044
                                                                  
                                                                  ,inline=TRUE),
                                               submitButton("Update filters")
                                  ),
                                  mainPanel(
                                    column(8, plotlyOutput("plotW", width = 900, height=700)
                                           
                                           
                                    )
                                    
                                  )
                                )
                       ),
                       tabPanel("Score Bar", fluid = TRUE,
                                
                                plotlyOutput("scoreChart")
                                
                       ),
                       tabPanel("Pit Scout", fluid = TRUE,
                                pageWithSidebar(
                                  headerPanel('Pit'),
                                  sidebarPanel(width = 3,
                                               selectInput(inputId = 'pitTeamW', label = 'List of Teams:',pitDataW$Team,
                                                           
                                                           selected = pitDataB %>% filter(Team %in% 8044 )
                                               ),
                                               
                                               submitButton("Update filters")
                                  ),
                                  mainPanel(
                                    column(8, htmlOutput("pitW", width = 900, height = 700))
                                    
                                  ))),
                       tabPanel("Comments", fluid = TRUE,
                                pageWithSidebar(
                                  headerPanel('Scout Comments'),
                                  sidebarPanel(width = 3,
                                               selectInput(inputId = 'comTeamW', label = 'List of Teams:',comDataW$robot,
                                                           
                                                           selected = comDataW %>% filter(robot %in% 8044 )
                                               ),
                                               
                                               
                                               submitButton("Update filters")
                                  ),
                                  mainPanel(
                                    htmlOutput("comW", inline = FALSE))
                                ))
                     )),
                       tabPanel("Next Match", fluid = TRUE,
                                pageWithSidebar(
                                  headerPanel(''),
                                  sidebarPanel(width = 1,
                                               selectInput(inputId = 'nmTeamW', label = 'Match:', nmDataW$match_number
                                                           
                                                           
                                               ),
                                               
                                               
                                               submitButton("Submit")
                                  ),
                                  mainPanel(
                                    fluidRow(column(6,h4("Alliance"),
                                                    plotlyOutput("nmPlot2W")),
                                             column(6,h4("Opponents"),
                                                    plotlyOutput("nmPlot3W"))),
                                    
                                    fluidRow(column(6,
                                                    plotlyOutput("nmPlot4W")),
                                             column(6,plotlyOutput("nmPlot5W")))
                                    
                                  )
                                ))
                     )),
            
            
          ))

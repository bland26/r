
fluidPage(
    pageWithSidebar(
                headerPanel('Scouting Report'),
                sidebarPanel(width = 4,
                             selectInput('year', 'List of Teams:',paste(data$team,"-",data$name)),


                             checkboxGroupInput(inputId = "teams",
                                                label = 'Choose up to 10 teams:', choiceNames = c(data$team),
                                                choiceValues = c(data$team),
                                                selected = data %>% filter(team == 8044)
                                                ,inline=TRUE),
                             submitButton("Update filters")
                ),
                mainPanel(
                    column(8, plotlyOutput("plot1", width = 800, height=700)
                           

                    )

                )
            )
   )

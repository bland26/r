
function(input, output, session) {

    selectedData1 <- reactive({
        scoutData %>%
            filter(robot %in% input$teams )
    })
    
    selectedData2 <- reactive({
        selectedData1() %>%
            select(6,10,14,15,17:19) 
    })
    


    # Combine the selected variables into a new data frame
    output$plot1 <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = 'toself'
        ) %>%
            add_trace(
                r = as.matrix(selectedData2()[1,]) ,
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1()[1,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[2,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[2,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[3,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[3,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[4,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[4,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[5,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[5,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[6,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[6,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[7,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[7,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[8,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[8,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[9,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[9,4]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[10,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score", "Links (Alliance)"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[10,4]
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(-0.25,1)
                    )
                ),
                
                showlegend=TRUE
                
                
            )
        
    })
    selectedData3 <- reactive({
        scoutData %>%
            filter(robot %in% input$team )
    })
    
    at <- reactive({input$time})
    
    
    selectedData4 <- reactive({selectedData1 %>%
            select(7:9)
    })
    selectedData5 <- reactive({selectedData1 %>%
            select(11:13)
    })
    output$heatAuto <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = scoutData3, type = "heatmap",
        )%>%
            layout(title = "Auto")
    })
    output$heatTele <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = scoutData3, type = "heatmap",
        ) %>%
            layout(title = "Tele")
    })
    
    
   
    
    winData <- reactive({
        getTeamMatchesCustom(2023,input$a)
    })
    
    output$mr <- renderPlot({
        ggplot(winData(), aes(x = Alliance_Score, y = Opponent_Score)) + 
            geom_point(aes(color = factor(win),shape = factor(str_trunc(name, 20)), size = 1))+ 
            xlim(10,200) + ylim(10,200) + labs (shape = "Event") + labs (color = "Results")
    })
}

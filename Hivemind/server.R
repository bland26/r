function(input, output, session) {

    selectedData1 <- reactive({
        scoutData %>%
            filter(robot %in% input$teams )
    })
    
    selectedData2 <- reactive({
        selectedData1() %>%
            select(2:7) 
    })
    
    oprSelect <- reactive({
        oprData %>%
            filter(name %in% input$teams)
    })
    
    #selectedData3 <- reactive({
        #selectedData2() %>% mutate("opr" = oprSelect()[2])
    #})
    


    # Combine the selected variables into a new data frame
    output$plot1 <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = 'toself'
        ) %>%
            add_trace(
                r = as.matrix(selectedData2()[1,]) ,
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1()[1,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[2,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[2,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[3,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[3,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[4,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[4,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[5,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[5,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[6,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[6,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[7,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[7,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[8,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[8,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[9,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[9,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[10,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[10,1]
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(-0.1,1)
                    )
                ),
                
                showlegend=TRUE
                
                
            )
        
    })

    


    
    scoutData2 <- reactive({
        ahigh <- scoutData %>% filter(robot %in% input$team) %>% select(8:16) 
        amid  <- scoutData %>% filter(robot %in% input$team) %>% select(17:25) 
        alow  <- scoutData %>% filter(robot %in% input$team) %>% select(26:34) 
        c(alow,amid,ahigh)
        })
    scoutData3 <- reactive({
        thigh <- scoutData %>% filter(robot %in% input$team) %>% select(35:43) 
        tmid  <- scoutData %>% filter(robot %in% input$team) %>% select(44:52) 
        tlow  <- scoutData %>% filter(robot %in% input$team) %>% select(53:61) 
        c(tlow,tmid,thigh)
        })
    

    output$heatAuto <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData2()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
        )%>%
            layout(title = "Auto")
    })
    output$heatTele <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData3()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
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
    
    output$com <- renderUI({ 
       comOut <- comData %>% filter(robot %in% input$comTeam)
       comOut <- comOut %>% select(-1)
       comOut <- comOut %>% unite("comment", na.rm = TRUE)
       comOut <- paste(comOut, sep = '<br/>')
       comOut <- gsub("c\\(", "",comOut)
       comOut <- gsub("\"", "" , comOut)
       comOut <- gsub(")", "", comOut)
       comOut <- gsub(",", "<br><br>", comOut)
       comOut <- gsub("_",", ", comOut)
       comOut <- gsub(":,",":", comOut)
       HTML(comOut)
    
    })
}


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
    
    selectedData3 <- reactive({
        selectedData2() %>% mutate("opr" = oprSelect()[[2]])
    })
    
    



    # Combine the selected variables into a new data frame
    output$plot1 <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = 'toself'
        ) %>%
            add_trace(
                r = as.matrix(selectedData3()[1,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1()[1,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[2,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[2,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[3,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[3,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[4,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[4,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[5,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[5,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[6,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[6,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[7,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[7,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[8,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[8,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[9,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[9,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3()[10,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
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
            xlim(0,200) + ylim(0,200) + labs (shape = "Event") + labs (color = "Results")
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
    
    selectedData1M <- reactive({
        scoutDataM %>%
            filter(robot %in% input$teamsM )
    })
    
    selectedData2M <- reactive({
        selectedData1M() %>%
            select(2:7) 
    })
    
    oprSelectM <- reactive({
        oprDataM %>%
            filter(name %in% input$teamsM)
    })
    
    selectedData3M <- reactive({
        selectedData2M() %>% mutate("opr" = oprSelectM()[[2]])
    })
    
    
    
    
    
    # Combine the selected variables into a new data frame
    output$plotM <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = 'toself'
        ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[1,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1M()[1,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[2,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[2,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[3,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[3,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[4,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[4,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[5,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[5,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[6,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[6,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[7,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[7,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[8,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[8,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[9,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[9,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3M()[10,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1M()[10,1]
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
    
    
    
    
    
    scoutData2M <- reactive({
        ahighM <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(8:16) 
        amidM  <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(17:25) 
        alowM  <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(26:34) 
        c(alowM,amidM,ahighM)
    })
    scoutData3M <- reactive({
        thighM <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(35:43) 
        tmidM  <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(44:52) 
        tlowM  <- scoutDataM %>% filter(robot %in% input$teamM) %>% select(53:61) 
        c(tlowM,tmidM,thighM)
    })
    
    
    output$heatAutoM <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData2M()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
        )%>%
            layout(title = "Auto")
    })
    output$heatTeleM <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData3M()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
        ) %>%
            layout(title = "Tele")
    })
    
    winDataM <- reactive({
        getTeamMatchesCustom(2023,input$aM)
    })
    
    output$mrM <- renderPlot({
        ggplot(winDataM(), aes(x = Alliance_Score, y = Opponent_Score)) + 
            geom_point(aes(color = factor(win),shape = factor(str_trunc(name, 20)), size = 1))+ 
            xlim(0,200) + ylim(0,200) + labs (shape = "Event") + labs (color = "Results")
    })
    
    output$comM <- renderUI({ 
        comOut <- comDataM %>% filter(robot %in% input$comTeamM)
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
    
    selectedData1B <- reactive({
        scoutDataB %>%
            filter(robot %in% input$teamsB )
    })
    
    selectedData2B <- reactive({
        selectedData1B() %>%
            select(2:7) 
    })
    
    oprSelectB <- reactive({
        oprDataB %>%
            filter(name %in% input$teamsB)
    })
    
    selectedData3B <- reactive({
        selectedData2B() %>% mutate("opr" = oprSelectB()[[2]])
    })
    
    
    
    
    
    # Combine the selected variables into a new data frame
    output$plotB <- renderPlotly({
        
        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = 'toself'
        ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[1,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1B()[1,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[2,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[2,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[3,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[3,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[4,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[4,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[5,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[5,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[6,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[6,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[7,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[7,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[8,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[8,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[9,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[9,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData3B()[10,]),
                theta = c("Auto Movement", "Auto Node Score", "Tele Node Score"
                          , "Charge Station Tele", "Driver Skill", "Collaboration","OPR"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1B()[10,1]
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
    
    
    
    
    
    scoutData2B <- reactive({
        ahighB <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(8:16) 
        amidB  <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(17:25) 
        alowB  <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(26:34) 
        c(alowB,amidB,ahighB)
    })
    scoutData3B <- reactive({
        thighB <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(35:43) 
        tmidB  <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(44:52) 
        tlowB  <- scoutDataB %>% filter(robot %in% input$teamB) %>% select(53:61) 
        c(tlowB,tmidB,thighB)
    })
    
    
    output$heatAutoB <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData2B()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
        )%>%
            layout(title = "Auto")
    })
    output$heatTeleB <- renderPlotly({
        plot_ly(
            x = c(1:9), y = c("Low","Mid", "High"),
            z = matrix(as.double(scoutData3B()), nrow = 3, ncol = 9, byrow = TRUE), type = "heatmap",
        ) %>%
            layout(title = "Tele")
    })
    
    winDataB <- reactive({
        getTeamMatchesCustom(2023,input$aB)
    })
    
    output$mrB <- renderPlot({
        ggplot(winDataB(), aes(x = Alliance_Score, y = Opponent_Score)) + 
            geom_point(aes(color = factor(win),shape = factor(str_trunc(name, 20)), size = 1))+ 
            xlim(0,200) + ylim(0,200) + labs (shape = "Event") + labs (color = "Results")
    })
    
    output$comB <- renderUI({ 
        comOut <- comDataB %>% filter(robot %in% input$comTeamB)
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
    
    output$teamlist <- renderTable({oppName3})
    output$teamlistM <- renderTable({oppName})
    output$teamlistB <- renderTable({oppName2})
}

data <- read.csv("Rtesting - Magnolia Regional.csv")

function(input, output, session) {
    


    selectedData1 <- reactive({
        data %>%
            filter(team %in% input$teams )
    })
    
    selectedData2 <- reactive({
        selectedData1() %>%
            select(4:17) 
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
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData1()[1,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[2,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[2,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[3,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[3,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[4,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[4,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[5,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[5,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[6,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[6,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[7,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[7,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[8,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[8,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[9,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[9,1]
            ) %>%
            add_trace(
                r = as.matrix(selectedData2()[10,]),
                theta = c("ACH","ACM","ACL","ACuH","ACuM","ACuL","AChL",
                          "TCH","TCM","TCL","TCuH","TCuM","TCuL","TChL"),
                showlegend = TRUE,
                mode = "markers",
                visible="legendonly",
                name = selectedData1()[10,1]
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(-1,5)
                    )
                ),
                
                showlegend=TRUE
                
                
            )
        
    })
    
}





# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {

  updateSelectInput(session, "variable", choices=colnames(faithful))
  
  output$view <- renderDataTable({ faithful })
  
  output$summary <- renderText({    paste("Selected ", input$variable)     }) 
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    if (input$variable != "")
    {
    x    <- faithful[, input$variable]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    }

  })
  
  output$distPlot2 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 1]
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  observe({
    
  updateSliderInput(session, "bins2", value=input$bins+5)
  })

})

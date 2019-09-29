#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #make Sure to use the names you give inputs!
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        print(input$bins)
        bins <- seq(min(x), max(x), length.out = input$Unit + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Some Graph Title")

    })
    
    output$distPlot2 <- renderLeaflet({
        
        m <- leaflet() %>% 
            setView(lng = -73.98, lat = 40.75, zoom = 10)%>%
            addProviderTiles(providers$CartoDB.Positron)
        return(m)

    })


})

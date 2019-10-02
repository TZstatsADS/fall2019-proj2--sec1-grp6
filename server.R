library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(formattable)
library(RColorBrewer)
library(leaflet)

df<- read.csv("C:/Users/Summer/Desktop/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

server=function(input, output) {
    
    Grade=df$GRADE
    
    output$NYC_Restaurants=renderDataTable(
        datatable(merge(trans_time[,c('zip',paste('X',input$'zip_input',sep=""))],df%>%filter(df$'GRADE' == input$'Grade' & df$'CUISINE.DESCRIPTION' == input$'speech1'),  
                        by='zip'))%>%formatStyle('zip',color='white',target='row',backgroundColor='black'), 
        options = list(pageLength=5, scrollX = TRUE, scrollY = TRUE
        ))
    output$mymap2 <- renderLeaflet({
        m <- leaflet(data=df) %>%
            addTiles() %>%
            setView(lng=-73.98928, lat=40.75042 , zoom=12)%>%
            addProviderTiles("Stamen.Toner")%>%
            addMarkers(lng = ~ (df%>%filter(df$'GRADE' == input$Grade & df$'CUISINE.DESCRIPTION' == input$'speech1') )$Longitude,
                             lat = ~(df%>%filter(df$'GRADE' == input$Grade & df$'CUISINE.DESCRIPTION' == input$'speech1'))$Latitude,
                             stroke = FALSE, fillOpacity = 0.5
            )
        
        m
    })
    
}
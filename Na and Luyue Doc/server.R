library(shiny)
library(shinythemes)
library(DT)
library(varhandle)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(leaflet)
library(dplyr)
library(rgdal)
library(Rcpp)
df<- read.csv("C:/Users/Summer/Desktop/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

server=function(input, output) {
    
    output$NYC_Restaurants=renderDataTable(
        datatable(df%>%filter((df$'GRADE'  %in%  input$variable) & df$'CUISINE.DESCRIPTION' == input$'speech1' & df$'BORO' == input$'speech2'  
                        ))%>%formatStyle('ZIPCODE',color='white',target='row',backgroundColor='black'),
        options = list(pageLength=5, scrollX = TRUE, scrollY = TRUE
        ))
    output$mymap2 <- renderLeaflet({
        m <- leaflet(data=df) %>%
            addTiles() %>%
            setView(lng=-73.98928, lat=40.75042 , zoom=12)%>%
            addProviderTiles("Stamen.Toner")%>%
            addMarkers(lng = ~ (df%>%filter((df$'GRADE'  %in%  input$variable) & df$'CUISINE.DESCRIPTION' == input$'speech1' & df$'BORO' == input$'speech2') )$Longitude,
                             lat = ~(df%>%filter((df$'GRADE'  %in%  input$variable) & df$'CUISINE.DESCRIPTION' == input$'speech1'& df$'BORO' == input$'speech2'))$Latitude,
                             
            )
        
        m
    })
    
}
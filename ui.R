

library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(leaflet)
#Import dataset
df<- read.csv("C:/Users/Summer/Desktop/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
#Clean the missing Data
df %<>% filter(`BORO` != '0')
df %<>% filter(`ZIPCODE` != 'N/A')


# Define UI for application 
shinyUI(fluidPage(

    
    titlePanel("Na and Luyue's Template"),
    tabPanel("Find Your Restaurant",
             selectInput("speech1","Category:" ,df$CUISINE.DESCRIPTION
             ),
             selectInput("variable", "Grade:",
                         c("A" = "A","B" = "B", "C"="C", "G"="G","N"="N","P"="P","Z"="Z"), multiple=TRUE),
             textInput('zip_input', "Zip:", value='10025'),
             checkboxGroupInput("map_select", "Critical?",
                                c("Y" = '1',
                                  "N" = '2',
                                  "I don't know" = '3'
                                )),
             absolutePanel(top=20,left=380,width=1000,height=3,tags$head(tags$style("#NYC_Restaurants {white-space: nowrap;}")),
                           dataTableOutput("NYC_Restaurants"),leafletOutput("mymap2",height = 450))
    )
   

))

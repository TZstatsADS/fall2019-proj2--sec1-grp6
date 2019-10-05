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
#Import dataset
df<- read.csv("C:/Users/ajkra/OneDrive/Documents/GitHub/fall2019-proj2--sec1-grp6/data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
#Clean the missing Data
df %<>% filter(`BORO` != '0')
df %<>% filter(`ZIPCODE` != 'N/A')
#list cuisine out
cuisine <- sort(unique(df$CUISINE.DESCRIPTION))
cuisine <- cuisine[cuisine != "Not Listed/Not Applicable"]

#list borough out
borough <- sort(unique(df$BORO))

# Define UI for application 
shinyUI(fluidPage(

    tabPanel("Find Your Restaurant",
             selectInput("speech1","Cuisine Type:" ,cuisine
             ),
             selectInput("speech2","Borough:" ,borough
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

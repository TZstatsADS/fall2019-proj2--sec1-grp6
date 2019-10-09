
library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(leaflet)
library(Rcpp)
library(varhandle)
library(gpclib)

##load data
data_raw_1 <- fread('../data/Raw Data 1.csv')
data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

nbhd <- fread('../data/nbhd.csv')
allNbhd <- nbhd$NEIGHBORHOOD

##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')

#########################################################

#All listed cuisines
allCuisines <- sort(unique(data_raw$`CUISINE DESCRIPTION`))
allCuisines <- allCuisines[allCuisines != "Not Listed/Not Applicable"]
cuisine <- allCuisines
#All listed boroughs
allBoros <- unique(data_raw$BORO)

#Main UI
shinyUI(
  fluidPage(theme = shinytheme("darkly"),
  
  navbarPage(title = p(class="h","Tasty & Safety"),
             
             ###OVerview tab
             tabPanel("Compare",
                    fluidRow(
                      
                      ##side bar controls
                      column(2,
                             selectInput("cuisine1","Cuisine Type 1" ,c('All',allCuisines),multiple = TRUE,selected = 'All'),
                             selectInput("boro1","Borough 1" ,c('All',allBoros),multiple = TRUE,selected = 'All'),
                             br(),
                             selectInput("cuisine2","Cuisine Type 2" ,c('All',allCuisines),multiple = TRUE,selected = 'All'),
                             selectInput("boro2","Borough 2" ,c('All',allBoros),multiple = TRUE,selected = 'All'),
                             br(),
                             br(),
                             br(),
                             br(),
                             sliderInput("slider1", label='Display Number '
                                         ,min=5,max=20,value=10),
                             
                             # textInput('zip_input', "Zip:", value='10027'),
                             checkboxGroupInput("critFlag", "Severity",
                                                c("Critical" = 'Y',
                                                  "Non-Critical" = 'N'),selected = c('Y','N'))
                      ),
                      
                      ##Tabset
                      column(10,
                             tabsetPanel(
                               ##table and barchart
                               tabPanel("Top Violations", 
                                        dataTableOutput("top_vio_table1")
                                        # ,plotlyOutput("top_vio_bar1",height = "auto",width = "auto")
                                        ,dataTableOutput("top_vio_table2")
                               )
                               ##Inpsection score distribution
                               ,tabPanel("Inspection Score" ,
                                         plotOutput("score_hist",height = '1000px')
                               ),tabPanel("Inspection Grade" ,
                                          br(),
                                          br(),
                                          plotlyOutput("grade_pie",height = '1000px')
                               )
                             )
                      )
                    )
             ),
             
             ##Map Tab
             tabPanel("Map",
                      fluidRow(
                        
                        ##side bar controls
                        column(2,
                               selectInput("boromap","Select Borough" ,c('All',allBoros) ,selected = 'All'),
                               br(),
                               selectInput("cuisinemap","Select Cuisine" ,c('All',allCuisines) ,selected = 'All')
        
                        ),
                        
                        ##Tabset
                        column(10, 
                               tabsetPanel(
                                 tabPanel("Map",
                                   #Print out map with slider and underneath a datatable
                                   leafletOutput("nycmap", height = '700px')
                                 ),
                                 
                                 tabPanel("Data Table",

                                    dataTableOutput("map_data_table", height = '400px')

                                 )
                               
                               )
                        )
                      )
             ),
             
             ##Individual Restaurant Info
             tabPanel("Find Your Restaurant",
                      fluidRow(
                        
                        ##side bar controls
                        column(2,                  
                          selectInput("speech1","Cuisine Type:" ,c('All',allCuisines)
                          ),
                          selectInput("speech2","Borough:" ,c('All',allBoros)
                          ),
                          selectInput("variable", "Grade:",
                                      c("A" = "A","B" = "B", "C"="C", "G"="G","N"="N","P"="P","Z"="Z"), multiple=TRUE),
                          selectInput("nbhd","Neighborhood:" ,c('All',allNbhd),selected = 'All'
                          )
                         
                          
                        ),
                        column(10, 
                               leafletOutput("mymap2",height = '300px')
                               ,
                               dataTableOutput("NYC_Restaurants")
                        )
                      )
             )
  )
  )
)

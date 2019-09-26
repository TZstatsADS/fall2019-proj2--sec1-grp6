

library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)


##load data
data_raw_1 <- fread('../data/Raw Data 1.csv')
data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')





#########################################################

#All listed cuisines
allCuisines <- sort(unique(data_raw$`CUISINE DESCRIPTION`))
allCuisines <- allCuisines[allCuisines != "Not Listed/Not Applicable"]

#All listed boroughs
allBoros <- unique(data_raw$BORO)





# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = shinytheme("darkly"),
  
  navbarPage(title = p(class="h","Tasty & Safety"),
             
             ###OVerview tab
             tabPanel("Overview",
                    fluidRow(
                      
                      ##side bar controls
                      column(2,
                             selectInput("cuisine1","Cuisine Type:" ,c('All',allCuisines),multiple = TRUE,selected = 'All'),
                             selectInput("boro1","Borough:" ,c('All',allBoros),multiple = TRUE,selected = 'All'),
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
                                        dataTableOutput("top_vio_table")
                                        ,plotlyOutput("top_vio_bar",height = "auto",width = "auto")
                               )
                               ##Inpsection score distribution
                               ,tabPanel("Inspection Score" 

                               )
                             )
                      )
                    )
             ),
             
             ##Map Tab
             tabPanel("Map"
             ),
             
             ##Individual Restaurant Info
             tabPanel("Restaurant Lookup"
             )
  )
  )
)

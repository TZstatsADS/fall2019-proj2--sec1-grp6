#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    navbarPage("Whole Project Title",id='first_map',
               
           tabPanel("Tab 1",

                    #Pot Graph, MUST BE UNIQUW
                    plotOutput("distPlot"),


                    #absolutePanel makes the panel on the right, Draggable panel witht the top, left, right ... give the panel its boundaries where it can go
                    #width and height gives the panel its actual size (it can be cut off from top bottom left right)

                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 0, right = 40, bottom = "auto",
                                  width = 330, height = "auto",

                                  #The Title on the Panel

                                  titlePanel("Panel"),


                                  #The Select Input from premade options

                                  selectInput("Select_name", label = h3("Select Something"),
                                              choices = list("Choice 1","Choice 2", "Choice 3"), selected = "Choice 2"),


                                  #The Slider for hours

                                  sliderInput("Unit", "Slider_name", label = "Slide to pick something",
                                              min = 0, max = 23, value = 8, step=1),


                                  #Checkboxes (what you call it, what people see, should it be checked already)
                                  #True means is checked already

                                  checkboxInput("Check1_name", "Checkbox1", TRUE),
                                  checkboxInput("Check2_name", "Checkbox2", FALSE),

                                  #A button that does something (what you call it, what people see)
                                  actionButton("button_name", "I'm a button"),

                                  radioButtons("Radio_button_name", label = "Radio Button Somthing",
                                               choices = list("Radio Choice 1" = "RC1", "Radio Choice 2" = "RC2","Radio Choice 3" = "RC3"),
                                               selected = "RC1")

                    )



           ),
           
           
           
           
           
           tabPanel("Tab 2",

                    
                    
                    
                    leafletOutput("distPlot2"),
                    
                    #absolutePanel makes the panel on the right, Draggable panel witht the top, left, right ... give the panel its boundaries where it can go
                    #width and height gives the panel its actual size (it can be cut off from top bottom left right)

                    absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 0, right = 40, bottom = "auto",
                                  width = 330, height = "auto",

                                  #The Title on the Panel

                                  h3("some title"),


                                  #The Select Input from premade options

                                  selectInput("Select_name2", label = h3("Select Something"),
                                              choices = list("Choice 1","Choice 2", "Choice 3"), selected = "Choice 2"),


                                  #The Slider for hours

                                  sliderInput("Unit2", "Slider_name2", label = "Slide to pick something",
                                              min = 0, max = 23, value = 8, step=1),


                                  #Checkboxes (what you call it, what people see, should it be checked already)
                                  #True means is checked already

                                  checkboxInput("Check1_name2", "Checkbox1", TRUE),
                                  checkboxInput("Check2_name2", "Checkbox2", FALSE),

                                  #A button that does something (what you call it, what people see)
                                  actionButton("button_name2", "I'm a button"),

                                  radioButtons("Radio_button_name2", label = "Radio Button Somthing",
                                               choices = list("Radio Choice 12" = "RC12", "Radio Choice 22" = "RC22","Radio Choice 32" = "RC32"),
                                               selected = "RC12")

                    )



           )
    )
    
   
))

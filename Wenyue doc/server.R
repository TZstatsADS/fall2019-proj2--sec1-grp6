library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)


# data_raw <- fread('../data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv')
# a <- data_raw[1:200000,]
# b <- data_raw[200001:nrow(data_raw),]

# write_csv(a,'../data/Raw Data 1.csv')
# write_csv(b,'../data/Raw Data 2.csv')

data_raw_1 <- fread('../data/Raw Data 1.csv')
data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')





#########################################################

vio_map <- unique(data_raw[,c('VIOLATION DESCRIPTION','VIOLATION CODE','CRITICAL FLAG')])


shinyServer(function(input, output) {
  
  ##top violation barchart
  output$top_vio_bar <- renderPlotly({

    if ('All' %in% input$cuisine1){
      cuisineFilter <- unique(data_raw$`CUISINE DESCRIPTION`)
    }else{
      cuisineFilter <- input$cuisine1
    }
    if ('All' %in% input$boro1){
      boroFilter <- unique(data_raw$BORO)
    }else{
      boroFilter <- input$boro1
    }


    data_sub <- data_raw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% input$critFlag,
                                    `VIOLATION DESCRIPTION` != "")

    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Code, Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)

    x_names <- vio_count$Code[1:input$slider1]
    x_names <- factor(x_names,levels = rev(x_names))
    plot_ly(
      x = vio_count$`# of Cases`[1:input$slider1],
      y = x_names,
      text = substr(paste0(vio_count$`# of Cases`[1:input$slider1], ", ",vio_count$`Description`[1:input$slider1]),1,150),
      hoverinfo = 'text',
      name = "Bigram",
      type = "bar",
      orientation = 'h',
      marker = list(color = 'rgb(158,202,225)',
                    line = list(color = 'rgb(8,48,107)',
                                width = 1.5))) %>% 
    layout(paper_bgcolor = 'azure1',
           plot_bgcolor = 'rgba(245, 246, 249, 1)',
           showlegend = FALSE)
    
  })
  
  ##top violation datatable
  output$top_vio_table <-  renderDataTable({
    if ('All' %in% input$cuisine1){
      cuisineFilter <- unique(data_raw$`CUISINE DESCRIPTION`)
    }else{
      cuisineFilter <- input$cuisine1
    }
    if ('All' %in% input$boro1){
      boroFilter <- unique(data_raw$BORO)
    }else{
      boroFilter <- input$boro1
    }

    data_sub <- data_raw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% input$critFlag,
                                    `VIOLATION DESCRIPTION` != "")
    
    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Code, Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)
    
    
    datatable(vio_count[1:input$slider1,],options = list(pageLength=min(input$slider1,10), scrollX = TRUE, scrollY = TRUE)) %>%
      formatStyle(1:ncol(vio_count),color = 'black')
  })

})



library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(formattable)
library(RColorBrewer)



data_raw_1 <- fread('../data/Raw Data 1.csv')
data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')





#########################################################

vio_map <- unique(data_raw[,c('VIOLATION DESCRIPTION','VIOLATION CODE','CRITICAL FLAG')])

########filter function for convenience##################
filterByCuisineBorough <- function(dataRaw, cuisine,borough,critFlag){
  if ('All' %in% cuisine){
    cuisineFilter <- unique(dataRaw$`CUISINE DESCRIPTION`)
  }else{
    cuisineFilter <- cuisine
  }
  if ('All' %in% borough){
    boroFilter <- unique(dataRaw$BORO)
  }else{
    boroFilter <- borough
  }
  
  dataRaw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% critFlag,
                                  `VIOLATION DESCRIPTION` != "")
}
########f################################################

# input <- list('cuisine1' = 'French','boro1' = 'Manhattan','cuisine2' = 'Chinese','boro2' = 'Manhattan','critFlag' = 'Y')

shinyServer(function(input, output) {
  
  ##top violation barchart1
  # output$top_vio_bar1 <- renderPlotly({
  # 
  #   if ('All' %in% input$cuisine1){
  #     cuisineFilter <- unique(data_raw$`CUISINE DESCRIPTION`)
  #   }else{
  #     cuisineFilter <- input$cuisine1
  #   }
  #   if ('All' %in% input$boro1){
  #     boroFilter <- unique(data_raw$BORO)
  #   }else{
  #     boroFilter <- input$boro1
  #   }
  # 
  # 
  #   data_sub <- data_raw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% input$critFlag,
  #                                   `VIOLATION DESCRIPTION` != "")
  # 
  #   vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
  #     arrange(desc(`# of Cases`))
  #   vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
  #                         ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
  #     select(Code, Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)
  # 
  #   x_names <- vio_count$Code[1:input$slider1]
  #   x_names <- factor(x_names,levels = rev(x_names))
  #   plot_ly(
  #     x = vio_count$`# of Cases`[1:input$slider1],
  #     y = x_names,
  #     text =  substr(paste0(vio_count$`# of Cases`[1:input$slider1], ", ",vio_count$`Description`[1:input$slider1]),1,150),
  #     hoverinfo = 'text',
  #     name = "Bigram",
  #     type = "bar",
  #     orientation = 'h',
  #     marker = list(color = 'rgb(158,202,225)',
  #                   line = list(color = 'rgb(8,48,107)',
  #                               width = 1.5))) %>% 
  #   layout(paper_bgcolor = 'azure1',
  #          plot_bgcolor = 'rgba(245, 246, 249, 1)',
  #          showlegend = FALSE)
  #   
  # })
  
  ##top violation datatable1
  output$top_vio_table1 <-  renderDataTable({
  
    data_sub <- filterByCuisineBorough(data_raw,input$cuisine1,input$boro1,input$critFlag)
    
    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)

    
    formattable::as.datatable(formattable(vio_count[1:input$slider1,], align = c("l",rep("c", ncol(vio_count) - 1)),
                                          list(`Description` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                                               `# of Cases` = color_bar("#FA614B",),
                                               `Critical` = formatter("span", style = ~ style(color = "black", font.weight = "bold")))),
                              options = list(columnDefs = list(list(className = 'dt-center', targets = 2:3))),
                              caption = htmltools::tags$caption(htmltools::tags$span("Top Violations for "),
                                                                htmltools::tags$span(paste(paste(input$cuisine1,collapse=', '), 'Cuisines'), style="color:red;"),
                                                                htmltools::tags$span("In Boroughs: "),
                                                                htmltools::tags$span(paste(paste(input$boro1,collapse = ', ')), style="color:green;"))) %>%
      formatStyle(1:ncol(vio_count),color = 'black') %>% 
      formatStyle(2:ncol(vio_count),border = '1px solid #ddd')
  })
  
  ##top violation datatable2
  output$top_vio_table2 <-  renderDataTable({
    data_sub <- filterByCuisineBorough(data_raw,input$cuisine2,input$boro2,input$critFlag)
    
    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)
    
    
    formattable::as.datatable(formattable(vio_count[1:input$slider1,], align = c("l",rep("c", ncol(vio_count) - 1)),
                                          list(`Description` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                                               `# of Cases` = color_bar("#FA614B",),
                                               `Critical` = formatter("span", style = ~ style(color = "black", font.weight = "bold")))),
                              options = list(columnDefs = list(list(className = 'dt-center', targets = 2:3))),
                              caption = htmltools::tags$caption(htmltools::tags$span("Top Violations for "),
                                      htmltools::tags$span(paste(paste(input$cuisine2,collapse=', '), 'Cuisines'), style="color:red;"),
                                      htmltools::tags$span("In Boroughs: "),
                                      htmltools::tags$span(paste(paste(input$boro2,collapse = ', ')), style="color:green;"))) %>%
      formatStyle(1:ncol(vio_count),color = 'black') %>% 
      formatStyle(2:ncol(vio_count),border = '1px solid #ddd')
    

  })
  
  
  ###histogram for inspection scores
  output$score_hist <- renderPlot({
    
    data_sub <- data_raw %>% mutate(SCORE = replace(SCORE,ACTION == "No violations were recorded at the time of this inspection.",0)) %>% filter(!is.na(SCORE)) 
    data_sub_1 <- filterByCuisineBorough(data_sub,input$cuisine1,input$boro1,input$critFlag)
    data_sub_2 <- filterByCuisineBorough(data_sub,input$cuisine2,input$boro2,input$critFlag)
    
    

    
    score_1 <- data_sub_1 %>% group_by(`CAMIS`,`INSPECTION DATE`) %>% summarise(Score = mean(SCORE)) %>% group_by(`CAMIS`) %>% summarise(Score = mean(Score)) %>% transmute(Category = paste0(paste(input$cuisine1,collapse = ', '),' cuisines in ',paste(input$boro1,collapse = ', ')),Score = Score) 
    score_2 <- data_sub_2 %>% group_by(`CAMIS`,`INSPECTION DATE`) %>% summarise(Score = mean(SCORE)) %>% group_by(`CAMIS`) %>% summarise(Score = mean(Score)) %>% transmute(Category = paste0(paste(input$cuisine2,collapse = ', '),' cuisines in ',paste(input$boro2,collapse = ', ')),Score = Score) 
    
    hist_data <- rbind(score_1,score_2)
    hist_data$Category <- factor(hist_data$Category,levels =  unique(hist_data$Category))
    mean_data <- hist_data %>% group_by(Category) %>% summarise(Mean = mean(Score))
    
    # hist_plot <- ggplot(hist_data, aes(x=Score, fill=Category, color=Category)) +
    #   geom_histogram(position="identity",binwidth = 1,alpha = 0.5) +
    #   geom_vline(data=mean_data, aes(xintercept=Mean, color=Category),
    #              linetype="dashed",size = 1.1,show.legend = FALSE) +
    #   theme(legend.title=element_blank(),legend.position = 'top',axis.title.x = element_text(face = 'bold'),axis.title.y = element_text(face = 'bold'))
    # 
    # hist_plot + geom_text(aes(x = mean_data$Mean[1]+1.5,y = diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9,label = round(mean_data$Mean[1],1)),show.legend = FALSE,colour = 'black') +
    #   geom_text(aes(x = mean_data$Mean[2]+1.5,y = diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9,label = round(mean_data$Mean[2],1)),show.legend = FALSE,colour = 'black')
    # 
    hist_plot<-ggplot(hist_data, aes(x=Score))+geom_histogram(aes(x = Score,fill = Category), binwidth = 1)+facet_grid(Category ~ .,scales = 'free_y') +
      geom_vline(data=mean_data, aes(xintercept=Mean,color = Category),linetype="dashed",size = 1,show.legend = FALSE) + scale_fill_brewer(palette = "Pastel1", name = "Category") +
      theme(legend.title=element_blank(),legend.position = 'top',axis.title.x = element_text(face = 'bold'),axis.title.y = element_text(face = 'bold'))
    
    
    if(input$cuisine1==input$cuisine2 & input$boro1==input$boro2){
      hist_yrange <- diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9
    }else{
      hist_yrange <- c(diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range),diff(ggplot_build(hist_plot)$layout$panel_params[[2]]$y.range))*0.9
    }
    
    hist_plot + geom_text(data = mean_data,aes(x = Mean+2,y =hist_yrange ,label = round(mean_data$Mean,1),color = Category),show.legend = FALSE)
      
  })
  
  
  ##pie chart for grades
  output$grade_pie <- renderPlotly({
    
    data_sub <- data_raw %>% filter(!GRADE %in% c("","G")) ##filtering out empty grades and G (we do not know what G means, and it has less than 5 observations anyway)
    data_sub_1 <- filterByCuisineBorough(data_sub,input$cuisine1,input$boro1,input$critFlag)
    data_sub_2 <- filterByCuisineBorough(data_sub,input$cuisine2,input$boro2,input$critFlag)
    
    
    ##get the latest(most recent) restaurant grade for each filter
    grade_1 <- data_sub_1 %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>% slice(n()) %>% transmute(Category = paste0(paste(input$cuisine1,collapse = ', '),' cuisines in ',paste(input$boro1,collapse = ', ')),Grade = GRADE) 
    grade_2 <- data_sub_2 %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>% slice(n()) %>% transmute(Category = paste0(paste(input$cuisine2,collapse = ', '),' cuisines in ',paste(input$boro2,collapse = ', ')),Grade = GRADE) 

    
    pie_data <- rbind(grade_1,grade_2)
    pie_data$Category <- factor(pie_data$Category,levels =  unique(pie_data$Category))
    
    
    
    
    plot_ly(textposition = 'inside',textinfo = 'label+percent',marker = list(colors = brewer.pal(8,"Set2"),line = list(color = '#FFFFFF', width = 1)),type = 'pie') %>%
      add_pie(data = count(grade_1[,-1], Grade), labels = ~Grade, values = ~n,
              name = grade_1$Category[1], domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
      add_pie(data = count(grade_2[,-1], Grade,sort = TRUE), labels = ~Grade, values = ~n,
              name = grade_2$Category[1], domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
      layout(paper_bgcolor='#E2E2E2',legend = list(
        font = list(
          family = "sans-serif",
          size = 12,
          color = "#000"),
        bgcolor = "#gray",
        bordercolor = "black",
        borderwidth = 2),annotations = list(text = sprintf("<b>%s</b>", paste(grade_1$Category[1],paste(rep(" ",90),collapse = ''),grade_2$Category[1])),  x = 0.5, y = 1,showarrow=FALSE))
    

    
    
    
  })
  

})



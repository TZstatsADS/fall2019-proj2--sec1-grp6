library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(formattable)
library(RColorBrewer)
library(leaflet)
library(tigris)
library(rgdal)

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
##### Making a Zipcode List for map Boundaries ##########
## data from https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm# 

bronx_area.nbhd=c("Central Bronx", "Bronx Park and Fordham",
                 "High Bridge and Morrisania", "Hunts Point and Mott Haven",
                 "Kingsbridge and Riverdale", "Northeast Bronx",
                 "Southeast Bronx")

bronx_zip.nbhd=list(1:length(bronx_area.nbhd))
bronx_zip.nbhd[[1]]=c(10453, 10457, 10460)
bronx_zip.nbhd[[2]]=c(10458, 10467, 10468)
bronx_zip.nbhd[[3]]=c(10451, 10452, 10456)
bronx_zip.nbhd[[4]]=c(10454, 10455, 10459, 10474)
bronx_zip.nbhd[[5]]=c(10463, 10471)
bronx_zip.nbhd[[6]]=c(10466, 10469, 10470, 10475)
bronx_zip.nbhd[[7]]=c(10461, 10462,10464, 10465, 10472, 10473)


brook_area.nbhd =c("Central Brooklyn", "Southwest Brooklyn",
                   "Borough Park", "Canarsie and Flatlands",
                   "Southern Brooklyn", "Northwest Brooklyn",
                   "Flatbush", "East New York and New Lots", "Greenpoint",
                   "Sunset Park", "Bushwick and Williamsburg")

brook_zip.nbhd=list(1:length(brook_area.nbhd))
brook_zip.nbhd[[1]]=c(11212, 11213, 11216, 11233, 11238)
brook_zip.nbhd[[2]]=c(11209, 11214, 11228)
brook_zip.nbhd[[3]]=c(11204, 11218, 11219, 11230)
brook_zip.nbhd[[4]]=c(11234, 11236, 11239)
brook_zip.nbhd[[5]]=c(11223, 11224, 11229, 11235)
brook_zip.nbhd[[6]]=c(11201, 11205, 11215, 11217, 11231)
brook_zip.nbhd[[7]]=c(11203, 11210, 11225, 11226)
brook_zip.nbhd[[8]]=c(11207, 11208)
brook_zip.nbhd[[9]]=c(11211, 11222)
brook_zip.nbhd[[10]]=c(11220, 11232)
brook_zip.nbhd[[11]]=c(11206, 11221, 11237)


man_area.nbhd=c("Central Harlem", "Chelsea and Clinton",
                "East Harlem", "Gramercy Park and Murray Hill",
                "Greenwich Village and Soho", "Lower Manhattan",
                "Lower East Side", "Upper East Side", "Upper West Side",
                "Inwood and Washington Heights")

man_zip.nbhd=list(1:length(man_area.nbhd))
man_zip.nbhd[[1]]=c(10026, 10027, 10030, 10037, 10039)
man_zip.nbhd[[2]]=c(10001, 10011, 10018, 10019, 10020)
man_zip.nbhd[[3]]=c(10036, 10029, 10035)
man_zip.nbhd[[4]]=c(10010, 10016, 10017, 10022)
man_zip.nbhd[[5]]=c(10012, 10013, 10014)
man_zip.nbhd[[6]]=c(10004, 10005, 10006, 10007, 10038, 10280)
man_zip.nbhd[[7]]=c(10002, 10003, 10009)
man_zip.nbhd[[8]]=c(10021, 10028, 10044, 10065, 10075, 10128)
man_zip.nbhd[[9]]=c(10023, 10024, 10025)
man_zip.nbhd[[10]]=c(10031, 10032, 10033, 10034, 10040)


queens_area.nbhd=c("Northeast Queens", "North Queens",
                   "Central Queens", "Jamaica",
                   "Northwest Queens", "West Central Queens",
                   "Rockaways", "Southeast Queens", "Southwest Queens",

                                      "West Queens")
queens_zip.nbhd=list(1:length(queens_area.nbhd))
queens_zip.nbhd[[1]]=c(11361, 11362, 11363, 11364)
queens_zip.nbhd[[2]]=c(11354, 11355, 11356, 11357, 11358, 11359, 11360)
queens_zip.nbhd[[3]]=c(11365, 11366, 11367)
queens_zip.nbhd[[4]]=c(11412, 11423, 11432, 11433, 11434, 11435, 11436)
queens_zip.nbhd[[5]]=c(11101, 11102, 11103, 11104, 11105, 11106)
queens_zip.nbhd[[6]]=c(11374, 11375, 11379, 11385)
queens_zip.nbhd[[7]]=c(11691, 11692, 11693, 11694, 11695, 11697)
queens_zip.nbhd[[8]]=c(11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429)
queens_zip.nbhd[[9]]=c(11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421)
queens_zip.nbhd[[10]]=c(11368, 11369, 11370, 11372, 11373, 11377, 11378)


staten_area.nbhd=c("Port Richmond", "South Shore",
                   "Stapleton and St. George", "Mid-Island")

staten_zip.nbhd=list(1:length(staten_area.nbhd))
staten_zip.nbhd[[1]]=c(10302, 10303, 10310)
staten_zip.nbhd[[2]]=c(10306, 10307, 10308, 10309, 10312)
staten_zip.nbhd[[3]]=c(10301, 10304, 10305)
staten_zip.nbhd[[4]]=c(10314)


nyc_area.nbhd=c(bronx_area.nbhd, brook_area.nbhd
                , man_area.nbhd, queens_area.nbhd
                , staten_area.nbhd)

zip.nbhd= c(bronx_zip.nbhd, brook_zip.nbhd
            , man_zip.nbhd, queens_zip.nbhd 
            ,staten_zip.nbhd)


########################################################

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
  
  #Make nycmap, using longitude and latitude and zoom to find nyc
  #Adding tiles (Providers$ The tile design you like)
  output$nycmap <- renderLeaflet({
    
    m <- leaflet() %>% 
      setView(lng = -73.98, lat = 40.75, zoom = 10)%>%
      addProviderTiles(providers$CartoDB.Positron)
    return(m)
    
  })
  
  
  output$nycmap2 <- renderLeaflet({
  
    
    # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
    NYCzipcodes <- readOGR("../data/ZIP_CODE_040114.shp",
                           #layer = "ZIP_CODE", 
                           verbose = FALSE)
    
    #get the ZIPCODE and SCORE columns from data_raw and them get rid of blanks and NA rows
    sample_data = data_raw[,c("ZIPCODE", "SCORE")]
    sample_data[sample_data==""] <- NA
    sample_data <- na.omit(sample_data)
    
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% sample_data$ZIPCODE)
    

    # ----- Transform to EPSG 4326 - WGS84 (required) and to get spacial data
    subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
    
   
 
    
    map_data <- geo_join(subdat, sample_data , "ZIPCODE", "ZIPCODE")

    pic1 <- leaflet(map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")

    pic1 <- pic1 %>%
      addPolygons(fillColor="red", color = 'red', weight = 1
      ,fillOpacity = .6)
  })
  
  ## Panel 3: leaflet
  output$nycmap3 <- renderLeaflet({
    
    
    #get the ZIPCODE and SCORE columns from data_raw and them get rid of blanks and NA rows
    sample_data = data_raw[,c("ZIPCODE", "SCORE")]
    sample_data[sample_data==""] <- NA
    sample_data <- na.omit(sample_data)
    
    # From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
    NYCzipcodes <- readOGR("../data/ZIP_CODE_040114.shp",
                           #layer = "ZIP_CODE", 
                           verbose = FALSE)
    
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% sample_data$ZIPCODE)
    
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
    
    # ----- save the data slot
    subdat_data=subdat@data[,c("ZIPCODE", "POPULATION")]
    subdat.rownames=rownames(subdat_data)
    
    subdat_data.df=
      subdat_data%>%left_join(sample_data, by=c("ZIPCODE" = "ZIPCODE"))
    rownames(subdat_data)=subdat.rownames
    
    
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
    # Create a continuous palette function
    pal <- colorNumeric(
      palette = "Blues",
      domain = subdat$POPULATION
    )
    
    leaflet(subdat) %>%
      addTiles()%>%
      addPolygons(
        stroke = T, weight=1,
        fillOpacity = 0.6,
        color = ~pal(POPULATION)
      )
  })

})



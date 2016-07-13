#These need to be in the server file so that they are only run once.
library(rgdal)
require(GISTools)
df.sf_crime <- read.csv("../SFPD.csv")
source("helper.R")

df.sf_crime$Times <- sapply(strsplit(as.character(df.sf_crime$Time),":"),function(v) { as.numeric(v[1])  + as.numeric(v[2])/60})
df.sf_crime$Dates <- as.Date(df.sf_crime$Date,format="%m/%d/%Y")
topct <- tail(row.names(sort(table(df.sf_crime$Category))),20)
df.sf_crime = df.sf_crime[df.sf_crime$Category %in%  topct,]
df.sf_crime$Category = factor(df.sf_crime$Category)

df_neigh <- spTransform(readOGR("..",'planning_neighborhoods') ,CRS("+proj=longlat +ellps=WGS84"))
df_water <- spTransform(readOGR("..",'phys_waterbodies') ,CRS("+proj=longlat +ellps=WGS84"))
    
    
shinyServer(function(input, output) {
        
      dataInput <- reactive({ createsub(df.sf_crime, input) })
      
      dataType <- reactive({input$plot})
        
      output$map <- renderPlot(mapfunc(dataInput(),df_neigh,df_water,dataType()))
      
      
      output$click_info <- renderPrint({
        nearPoints(df.sf_crime, input$plot1_click, "X", "Y")
      })
      
      output$brush_info <- renderPrint({
        brushedPoints(df.sf_crime, input$plot1_brush, "X", "Y")
      })
      
      output$downloadData <- downloadHandler(
        filename = "df.sf_crime sub",
        content = function(file) {
          write.csv(dataInput(), file)
        })
        
      
      })



df.sf_crime <- read.csv("data/SFPD.csv")
df.sf_crime$Times <- sapply(strsplit(as.character(df.sf_crime$Time),":"),function(v){ as.numeric(v[1])+as.numeric(v[2])/60 })
df.sf_crime$Month <- factor(month.name[as.numeric(substr(df.sf_crime$Date,1,2))],levels=month.name)

source('helper.R')

shinyServer(
  function(input, output){
    
    output$plot1 <- renderPlot({
      graphvar1(df.sf_crime[,input$vars])
    }, height = 400, width = 800)
    
    output$sum11 <- renderText({
      numsum1(df.sf_crime[,input$vars])
    })
    output$sum12 <- renderText({
      numsum12(df.sf_crime[,input$vars])
    })
    
    output$plot2 <- renderPlot({
      graphvar2(df.sf_crime[,input$vars1],df.sf_crime[,input$vars2])
    }, height = 600, width = 800)
    
    output$sum2 <- renderPrint({
      numsum2(df.sf_crime[,input$vars1],df.sf_crime[,input$vars2])
    })
  })
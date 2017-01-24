library(dplyr)
library(ggplot2)
library(shiny)
library(DBI)
library(RPostgreSQL)
library(qcc)
library(DT)
library(shinydashboard)


shinyServer(function(input, output){
  
  b6data <- reactive({
    flights.df %>%
      filter(Origin == input$airport)
  })
  
  # histogram
  
  output$hist <- renderPlot({
    b6data =  filter(flights.df, Origin == input$airport)
    hist(b6data$DepDelayMinutes, col = 'blue', main = 'JetBlue delayed flights', xlab = 'Flight delay (min)')
  })
  
  
  # boxplot
  output$boxplot <- renderPlot({
    b6data = filter(flights.df, Origin == input$airport)
    ggplot(b6data, aes(x = UniqueCarrier, y = DepDelayMinutes))+
      stat_boxplot(geom = "errorbar")+
      geom_boxplot()+
      labs(x =  "JetBlue", y = "Departure Delay")
    
  })
  
  # statistics
  
  output$stats <- renderPrint({
    
    b6data = filter(flights.df, Origin == input$airport)
    b6data1 <- b6data$DepDelayMinutes 
    
    cat(
      paste("Count      =", length(b6data1)), "\n",
      paste("Sum        =", sum(b6data1)), "\n",
      paste("Min        =", min(b6data1)), "\n",
      paste("Max        =", max(b6data1)), "\n",
      paste("Min        =", min(b6data1)), "\n",
      paste("Mean       =", mean(b6data1)), "\n",
      paste("Median     =", median(b6data1)),"\n",
      paste("SDev       =", sd(b6data1)), "\n",
      paste("Variance   =", var(b6data1)), "\n"
    )
   
  
  })
  
  
  # x bar
  
  output$xbar <- renderPlot({
    b6data = filter(flights.df, Origin == input$airport)
    b6data1 <- b6data$DepDelayMinutes 
    qcc(b6data1, type = "xbar.one", nsigmas = 3)
    
  })
  


  
  #  process capabilities
  
  output$pc <- renderPlot({
    b6data = filter(flights.df, Origin == input$airport)
    b6data1 <- b6data$DepDelayMinutes 
    qx <- qcc(b6data1, type = "xbar.one", nsigmas = 3)
    process.capability(qx, spec.limits = c(input$lcl, input$usl))
    
  })
  
  # raw data
  
  output$data <- renderPrint({b6data()})
    
  })
  

  


  
  

  

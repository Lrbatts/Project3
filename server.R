library(shiny)
library(shinydashboard)
library(psych)
library(ggplot2)
library(tidyverse)

shinyServer(function(input,output){
  
  mjdata <- read_csv("mjdata.csv")
  
  output$mj_img <- renderImage({
    list(src="www/michael-jordan-png-10671.png",
         width="100%",
         height=330)
},deleteFile=F)
  
  output$numsum <- renderTable({
    if (input$var1=="Points"){
      describe(mjdata$PTS)
    } else if(input$var1=="Rebounds"){
      describe(mjdata$TRB)
    } else if(input$var1=="Assists"){
      describe(mjdata$AST)
    } else if(input$var1=="Game Score"){
      describe(mjdata$GmSc)
    } else {
      describe(mjdata$FG_PCT)
    }
  })
  
  output$corr <- renderTable({
    if (input$var1=="Points"){
      tab <- matrix(c(cor(mjdata$Win,mjdata$PTS),cov(mjdata$Win,mjdata$PTS)),ncol=2)
      colnames(tab) <- c("Correlation","Covariance")
      tab <- as.table(tab)
      tab
    } else if (input$var1=="Rebounds"){
      tab <- matrix(c(cor(mjdata$Win,mjdata$TRB),cov(mjdata$Win,mjdata$TRB)),ncol=2)
      colnames(tab) <- c("Correlation","Covariance")
      tab <- as.table(tab)
      tab
    } else if (input$var1=="Assists"){
      tab <- matrix(c(cor(mjdata$Win,mjdata$AST),cov(mjdata$Win,mjdata$AST)),ncol=2)
      colnames(tab) <- c("Correlation","Covariance")
      tab <- as.table(tab)
      tab
    } else if (input$var1 == "Game Score"){
      tab <- matrix(c(cor(mjdata$Win,mjdata$GmSc),cov(mjdata$Win,mjdata$GmSc)),ncol=2)
      colnames(tab) <- c("Correlation","Covariance")
      tab <- as.table(tab)
      tab
    } else {
      tab <- matrix(c(cor(mjdata$Win,mjdata$FG_PCT),cov(mjdata$Win,mjdata$PTS)),ncol=2)
      colnames(tab) <- c("Correlation","Covariance")
      tab <- as.table(tab)
      tab
    }
  })
  
  output$hist <- renderPlot({
    g <- ggplot(mjdata)
    if (input$var1=="Points"){
    g + geom_histogram(aes(x=PTS))
    } else if(input$var1=="Assists"){
      g + geom_histogram(aes(x=AST))
    } else if(input$var1=="Rebounds"){
      g + geom_histogram(aes(x=TRB))
    } else if(input$var1=="Game Score"){
      g + geom_histogram(aes(x=GmSc))
    } else {
      g + geom_histogram(aes(x=FG_PCT))
    }
  })
  
  output$box <- renderPlot({
    g <- ggplot(mjdata)
    if (input$var1=="Points"){
      g + geom_boxplot(aes(x=PTS))
    } else if(input$var1=="Assists"){
      g + geom_boxplot(aes(x=AST))
    } else if(input$var1=="Rebounds"){
      g + geom_boxplot(aes(x=TRB))
    } else if(input$var1=="Game Score"){
      g + geom_boxplot(aes(x=GmSc))
    } else {
      g + geom_boxplot(aes(x=FG_PCT))
    }
  })
  
  scatterdata <- reactive({
    mjdata[,c(input$var2,input$var3)]
  })
  output$scatter <- renderPlot({
    plot(scatterdata())
  })
  
  output$mj <- renderDataTable({
    mjdata
  })
      })
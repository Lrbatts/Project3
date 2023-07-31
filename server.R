library(shiny)
library(shinydashboard)
library(psych)
library(ggplot2)
library(tidyverse)
library(caret)
library(tree)
library(randomForest)
shinyServer(function(input,output,session){
  
  mjdata <- read_csv("mjdata.csv")
  vars <- mjdata %>% select("PTS","TRB","AST","GmSc","FG_PCT","Win") 
  vars$Win <- as.factor(vars$Win)
  
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
  
  inputdata <- eventReactive(input$submit,{
    vars
  })
  
  modelinput <- eventReactive(input$submit,{
    if (is.null(input$selectvar)) {
      dt <- vars
    }
    else{
      dt <- vars[, c(input$selectvar,'Win')]
    }
  })
  
  datasplit <- eventReactive(input$submit,{
    input$train / 100
  })
  
  set.seed(123)
  trainIndex <- eventReactive(input$submit,{
    sample(1:nrow(modelinput()),
           datasplit() * nrow(modelinput()))
  })
  
  mjtrain <- eventReactive(input$submit,{
    tmptrain <- modelinput()
    tmptrain[trainIndex(),]
  })
  
  mjtest <- eventReactive(input$submit,{
    tmptest <- modelinput()
    tmptest[-trainIndex(),]
  })
  
  f <- eventReactive(input$submit,{
    as.formula(paste('Win' ,"~."))
  })
  
  logfit <- eventReactive(input$submit,{
    train(f(), data=mjtrain(), 
                  method="glm",
                  family="binomial",
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="cv",number=5))
  })
  
  
  output$logmodel <- renderPrint(
    logfit())
   
  output$logsum <- renderPrint(
    summary(logfit())
  )
  
  logpred <- reactive({
    predict(logfit(),newdata=mjtest())
  })
  
  output$logtest <- renderPrint({
    tmptest <- mjtest()
    postResample(logpred(), obs = tmptest$Win)
  }
  )
  
  fittree <- eventReactive(input$submit,{
    tree(f(),data=mjtrain(),split="deviance")
  }) 
  
  output$treestat <- renderPrint(
    fittree()
  )
  output$treemodel <- renderPrint(
    summary(fittree())
  )
  
  treepred <- reactive({
    predict(fittree(),newdata=mjtest())
  })
  
  output$treetest <- renderPrint({
    tmptest <- mjtest()
    postResample(treepred(), obs = tmptest$Win)
  }
  )

  rfmodel <- eventReactive(input$submit,{
    randomForest(f(),data=mjtrain(),mtry=ncol(mjtrain())/3,ntree=200,importance=TRUE)
  })
  
  output$rfmodel <- renderPrint({
    rfmodel()
  })

  output$rfplot <- renderPrint({
    importance(rfmodel())
  })
  
  rfpred <- reactive({
    predict(rfmodel(),newdata=mjtest())
  })
  
  output$rftest <- renderPrint({
    tmptest <- mjtest()
    postResample(rfpred(), obs = tmptest$Win)
  }
  )

  preddata <- reactive({
    data.frame(PTS=input$pts,TRB=input$trb,AST=input$ast,GmSc=input$gmsc,FG_PCT=input$fgpct/100)
  })
  
  output$predict <- renderPrint({
    if(input$modeltype=="Logistic Regression"){
      predict(logfit(),newdata=preddata())
    } else if(input$modeltype=="Classification Tree"){
      predict(fittree(),newdata=preddata())
    } else {
      predict(rfmodel(),newdata=preddata())
    }
  })
  
  output$mj <- renderDataTable({
    mjdata
  })
  
  output$downloadData <- downloadHandler( 
    filename = function(){
      paste("mjdataset.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(mjdata, file, row.names = FALSE)
    }) 

  })

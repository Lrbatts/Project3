library(shiny)
library(shinydashboard)
library(psych)
library(ggplot2)
library(tidyverse)
library(caret)
library(tree)
library(randomForest)

#Create shiny server#
shinyServer(function(input,output,session){
  
  mjdata <- read_csv("mjdata.csv")
  vars <- mjdata %>% select("PTS","TRB","AST","GmSc","FG_PCT","Win") 
  vars$Win <- as.factor(vars$Win)
  
  output$mj_img <- renderImage({
    list(src="www/michael-jordan-png-10671.png",
         width="100%",
         height=330)
},deleteFile=F) #Renders Michael Jordan PNG image from web#
  
  #Create numeric summary based on which variable is selected
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
  
  #Creates correlation table depending on which variable is selected#
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
  
  #Creates histogram of data depending on which variable is selected
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
  
  #Creates boxplot of data depending on which variable is selected#
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
  
  #Create reactive data for scatter plot to react based on whcih scatter variables are selected
  scatterdata <- reactive({
    mjdata[,c(input$var2,input$var3)]
  })
  #Renders scatter plot based on reactive scatter data
  output$scatter <- renderPlot({
    plot(scatterdata())
  })
  
  inputdata <- eventReactive(input$submit,{
    vars
  })
  #Creates model reactive input that only changes when submit button pressed and changes what variables are used in fit
  modelinput <- eventReactive(input$submit,{
    if (is.null(input$selectvar)) {
      dt <- vars
    }
    else{
      dt <- vars[, c(input$selectvar,'Win')]
    }
  })
  #Creates reactive data for whenever the train slider is changed and submit button pressed#
  datasplit <- eventReactive(input$submit,{
    input$train / 100
  })
  #set seed for reproducability#
  set.seed(123)
  #Creates reactive training index using reactive datasplit for training model if submit pressed#
  trainIndex <- eventReactive(input$submit,{
    sample(1:nrow(modelinput()),
           datasplit() * nrow(modelinput()))
  })
  #Creates reactive training data
  mjtrain <- eventReactive(input$submit,{
    tmptrain <- modelinput()
    tmptrain[trainIndex(),]
  })
  #Creates reactive test data#
  mjtest <- eventReactive(input$submit,{
    tmptest <- modelinput()
    tmptest[-trainIndex(),]
  })
  #Creates reactive formula for models#
  f <- eventReactive(input$submit,{
    as.formula(paste('Win' ,"~."))
  })
  #Creates reactive logistic fit using caret package#
  logfit <- eventReactive(input$submit,{
    train(f(), data=mjtrain(), 
                  method="glm",
                  family="binomial",
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="cv",number=5))
  })
  
  #Prints logistic fit statistics#
  output$logmodel <- renderPrint(
    logfit())
  #Print logistic summary#
  output$logsum <- renderPrint(
    summary(logfit())
  )
  #Creates reactive logistic prediction
  logpred <- reactive({
    predict(logfit(),newdata=mjtest())
  })
  #Prints comparison to test set for logistic regression#
  output$logtest <- renderPrint({
    tmptest <- mjtest()
    postResample(logpred(), obs = tmptest$Win)
  }
  )
  #create reactive classification tree model#
  fittree <- eventReactive(input$submit,{
    tree(f(),data=mjtrain(),split="deviance")
  }) 
  #prints fit statistics for tree#
  output$treestat <- renderPrint(
    fittree()
  )
  #Print summary for tree#
  output$treemodel <- renderPrint(
    summary(fittree())
  )
  #Creates reactive prediction for classification tree#
  treepred <- reactive({
    predict(fittree(),newdata=mjtest())
  })
  #Prints comparison to test data for tree#
  output$treetest <- renderPrint({
    tmptest <- mjtest()
    postResample(treepred(), obs = tmptest$Win)
  }
  )
#Create reactive random forest model#
  rfmodel <- eventReactive(input$submit,{
    randomForest(f(),data=mjtrain(),mtry=ncol(mjtrain())/3,ntree=200,importance=TRUE)
  })
#Print fit statistic for reactive rf model#  
  output$rfmodel <- renderPrint({
    rfmodel()
  })
#Prints importance for selected variables for rf model#
  output$rfplot <- renderPrint({
    importance(rfmodel())
  })
#Creates reactive prediction on test set for rf model
  rfpred <- reactive({
    predict(rfmodel(),newdata=mjtest())
  })
  #Prints comparison to test set for rf#
  output$rftest <- renderPrint({
    tmptest <- mjtest()
    postResample(rfpred(), obs = tmptest$Win)
  }
  )
#Creates reactive data set based on input variables#
  preddata <- reactive({
    data.frame(PTS=input$pts,TRB=input$trb,AST=input$ast,GmSc=input$gmsc,FG_PCT=input$fgpct/100)
  })
  #Prints prediction based on which model is selected using reactive new data#
  output$predict <- renderPrint({
    if(input$modeltype=="Logistic Regression"){
      predict(logfit(),newdata=preddata())
    } else if(input$modeltype=="Classification Tree"){
      predict(fittree(),newdata=preddata())
    } else {
      predict(rfmodel(),newdata=preddata())
    }
  })
  #Prints original dataset#
  output$mj <- renderDataTable({
    mjdata
  })
  #Creates download button to download data set as csv#
  output$downloadData <- downloadHandler( 
    filename = function(){
      paste("mjdataset.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(mjdata, file, row.names = FALSE)
    }) 

  })

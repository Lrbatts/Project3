library(shinydashboard)
library(shiny)
library(tidyverse)
mjdata <- read_csv("mjdata.csv")
vars <- mjdata %>% select("PTS","TRB","AST","GmSc","FG_PCT","Win")
vars$Win <- as.factor(vars$Win)
shinyUI(fluidPage(
  dashboardPage(
  dashboardHeader(title = "Project 3 Landon Batts"),
  dashboardSidebar(
  sidebarMenu(
    menuItem("About",tabName="about"),
    menuItem("Data Exploration",tabName="dataexplore"),
    menuItem("Modeling",tabName="model"),
    menuItem("Data",tabName="data")
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName="about",
              fluidRow(
              box(title="Purpose",
      p(style="text-align: justify; font-size=25px",
        "The purpose of this app is to analyze game-by-game statistics of Michael Jordan and analyze what statistics lead to wins in the NBA. In the Data Exploration tab, you will be able to produce various numeric and graphical summaries of the data in order to analyze potential trends or relationships in the data. In the Modeling tab, you will be able to fit multiple types of learning models including multiple linear regression, regression trees, and a random forest model to determine which statistics led to wins for Michael Jordan. After fitting different models, you will then be able to use a model to make predictions of wins based on given statistics. Finally, in the data page, you will be able to actually scroll through the data set as well as subset it and save it yourself.")),
      box(title="Dataset",
          p(style="text-align: justify; font-size=25px", 
            "The dataset used for this app comes from", a(href="https://sports-statistics.com/sports-data/sports-data-sets-for-data-modeling-visualization-predictions-machine-learning/", "sports-statistics.com"), "a website with a wide variety of datasets from almost any sport designed to be able to be used for modeling and visualization. The data is Micheal Jordan regular season statistics for every game in his career. This includes his age at the time, whether they won the game, as well various statistics including points, rebounds, assists, field goal percentage, and more"),
          br(),
          imageOutput("mj_img")
          )
    )
  ),
      tabItem(tabName="dataexplore",
              sidebarLayout(
                sidebarPanel(
                radioButtons("numtype","Type of Numeric Summary", choices=c("One Variable Summary","Correlation/Covariance with Win"),selected="One Variable Summary"),
                radioButtons("graphtype", "Type of Graphical Summary", choices=c("Histogram","Boxplot","Scatter Plot"),selected="Histogram"),
                selectInput("var1","Variable",choices=c("Points","Rebounds","Assists","Game Score","Field Goal Pct")),
                conditionalPanel(condition="input.graphtype=='Scatter Plot'", 
                                 selectInput("var2","Scatter X",names(vars))),
                conditionalPanel(condition="input.graphtype=='Scatter Plot'", 
                                 selectInput("var3","Scatter Y",names(vars)))),
              
              mainPanel(
                conditionalPanel(condition="input.numtype=='One Variable Summary'", tableOutput("numsum")),
                conditionalPanel(condition="input.numtype=='Correlation/Covariance with Win'", tableOutput("corr")),
                conditionalPanel(condition="input.graphtype=='Histogram'", plotOutput("hist")),
                conditionalPanel(condition="input.graphtype=='Boxplot'", plotOutput("box")),
                conditionalPanel(condition="input.graphtype=='Scatter Plot'", plotOutput("scatter"))
              ))
),
      tabItem(tabName = "model",
              mainPanel(
              tabsetPanel(id="tabID",type="tabs",
                                      tabPanel("Modeling Info",fluidRow(
                                      box(title="Logistic Model for Wins",
                                          p("Description of logistic model")),
                                      box(title="Regression Tree",
                                          p("Description of regression tree")),
                                      box(title="Random Forest Model",
                                          p("Description of Random Forest")))),
                                      tabPanel("Model Fitting", sidebarPanel(sliderInput("train", label = h3("Train/Test Split %"),min = 0, max = 100,value = 80),
                                                                                selectInput("selectvar", label = "Select variables",choices = names(vars[-6]), multiple = TRUE,selected = names(vars[-6])),
                                                                             actionButton("submit","Submit Fit")),
                                                  mainPanel(tabsetPanel(id="tabset",
                                               tabPanel("Logistic Regression Model", fluidRow(box(title="Fit Statistics",verbatimTextOutput("logmodel")), box(title="Model Summary", verbatimTextOutput("logsum")))),
                                               tabPanel("Classification Tree", fluidRow(box(title="Fit Statistics", verbatimTextOutput("treestat")), box(title="Model Summary", verbatimTextOutput("treemodel")))),
                                               tabPanel("Random Forest Model", fluidRow(box(title="Fit Statistics",verbatimTextOutput("rfmodel")), box(title="Variable Importance", verbatimTextOutput("rfplot"))))))),
                                      tabPanel("Prediction")))
              ),
tabItem(tabName="data",
        mainPanel(renderDataTable("mj"))))
)
)
))



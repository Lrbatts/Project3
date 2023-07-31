library(shinydashboard)
library(shiny)
library(tidyverse)

#Read in data#
mjdata <- read_csv("mjdata.csv")
#select variables of interes
vars <- mjdata %>% select("PTS","TRB","AST","GmSc","FG_PCT","Win")
#make wins a factor variable
vars$Win <- as.factor(vars$Win)
#Begin UI#
shinyUI(fluidPage(
  dashboardPage(
  dashboardHeader(title = "Project 3 Landon Batts"),
  dashboardSidebar(
  sidebarMenu(
    menuItem("About",tabName="about"),
    menuItem("Data Exploration",tabName="dataexplore"),
    menuItem("Modeling",tabName="model"),
    menuItem("Data",tabName="data")
  )), #Creates side menu with each tab for app#
  dashboardBody(
    tabItems(
      tabItem(tabName="about",
              fluidRow(
              box(title="Purpose", 
      p(style="text-align: justify; font-size=25px",
        "The purpose of this app is to analyze game-by-game statistics of Michael Jordan and analyze what statistics lead to wins in the NBA. In the Data Exploration tab, you will be able to produce various numeric and graphical summaries of the data in order to analyze potential trends or relationships in the data. In the Modeling tab, you will be able to fit multiple types of learning models including logistic regression, classification trees, and a random forest model to determine which statistics lead to wins for Michael Jordan. Summary statistics are supplied for each model you fit to help you make a decision as to the best model to use. After selecting a model, you will then be able to use it to make predictions of the probability that his team won based on different combinations of statistics. A win is represented by a 1 and a loss is represented by a 0. Finally, in the data page, you will be able to actually scroll through the data set as well as subset it and save it yourself.")), #Creates box in about page describing purpose of app#
      box(title="Dataset",
          p(style="text-align: justify; font-size=25px", 
            "The dataset used for this app comes from", a(href="https://sports-statistics.com/sports-data/sports-data-sets-for-data-modeling-visualization-predictions-machine-learning/", "sports-statistics.com"), "a website with a wide variety of datasets from almost any sport designed to be able to be used for modeling and visualization. The data is Michael Jordan regular season statistics for every game in his career. This includes his age at the time, whether they won the game, as well various statistics including points, rebounds, assists, field goal percentage, and more. For this app, I have chosen to focus on points, rebounds, assists, game score, and field goal percentage as those are popular statistics thought to help a team win a game. Game score is a formula that takes other statistics and weighs them accordingly to produce a new statistic as to that players impact on the game."), 
          br(),
          imageOutput("mj_img")
          ) #Creates new box in about page describing data set and providing image of Michael Jordan
    )
  ),
      tabItem(tabName="dataexplore",
              sidebarLayout(
                sidebarPanel(
                radioButtons("numtype","Type of Numeric Summary", choices=c("One Variable Summary","Correlation/Covariance with Win"),selected="One Variable Summary"), #creates buttons to select type of numeric summary#
                radioButtons("graphtype", "Type of Graphical Summary", choices=c("Histogram","Boxplot","Scatter Plot"),selected="Histogram"), #creates buttons to select type of graphical summary#
                selectInput("var1","Variable",choices=c("Points","Rebounds","Assists","Game Score","Field Goal Pct")), #Creates input allows you to select what variable of interest for numerical and graphical summaries#
                conditionalPanel(condition="input.graphtype=='Scatter Plot'", 
                                 selectInput("var2","Scatter X",names(vars))),
                conditionalPanel(condition="input.graphtype=='Scatter Plot'",  
                                 selectInput("var3","Scatter Y",names(vars)))), #Creates two conditional panels if scatter plot selected so all variables of interest are able to be used#
              
              mainPanel(
                conditionalPanel(condition="input.numtype=='One Variable Summary'", tableOutput("numsum")), #outputs object numsum if one variable summary selected#
                conditionalPanel(condition="input.numtype=='Correlation/Covariance with Win'", tableOutput("corr")), # creates object corr if Correlation with win is selected#
                conditionalPanel(condition="input.graphtype=='Histogram'", plotOutput("hist")), #creates object hist if histogram is selected#
                conditionalPanel(condition="input.graphtype=='Boxplot'", plotOutput("box")), #creates object box if boxplot is selected#
                conditionalPanel(condition="input.graphtype=='Scatter Plot'", plotOutput("scatter")) #creates object scatter if scatter plot is selected#
              ))
), #Now create model page#
      tabItem(tabName = "model",
              mainPanel(
              tabsetPanel(id="tabID",type="tabs",
                                      tabPanel("Modeling Info",tabsetPanel(
                                        tabPanel("Logistic Regression Model", box(title="Logistic Model for Wins", withMathJax(),
                                          p("A Generalized Linear model (GLM) is a type of linear model for analyzing the regression line of a dataset in which it allows for responses from non-normal distributions unlike simple linear regression and also it allowfor both categorical and quantitative predicotr variabels. More specifically a logistic regression model is a type of GLM designed for datasets with a response variable that is measured as either a success (1) or a failure (0). It is modeling the average number of successes for a given x. Since we are attempting to model the probability of a win given a variety of statistics for Michael Jordan, this model is appropriate. Now, beta1 represents a change in the log-odds of success which a great way to represent data with a response such as win. Th emajor benefit is that there is no assumptions about the distributions as they can be non-normal. The major drawback is an assumption of linearity between dependent and independent variables. The probability of a success is modeled in basic logistic regression with the formula", helpText('$$\\frac{e^(\beta_0+\beta_1)}{1+e^(\beta_0+\beta_1)}$$')),width=12)), #Creates description of logistic regression if logistic tab is selected#
                                        tabPanel("Classification Tree", box(title="Classification Tree",
                                          p("A classification tree follows the tree based model in which the predictor space is split up into regions and different predictions are made for each region. A classification tree has the goal of predicting group membership meaning for a given region, the most prevalent class as prediction. In our case, our classes are a win or a loss. It fits a series of flat planes and finds the mean for a given region and automatically accounts for interaction. Benefits of using trees include easy interpretation and built-in variable slection. A potential drawback is that many trees are grown through splitting and they generally need to be pruned to not overfit the data which can increase bias. Also, small changes in data can drastically affect the tree."),width=12)), #Creates description of classification trees if the classification tab is selected#
                                        tabPanel("Random Forest Model", box(title="Random Forest Model",
                                          p("A random forest model is a method of creating multiple trees from bootstrap sampling creating many samples and then averaging results. instead of using all predictors for each bootstrap sample and tree fit, a random subset of predictors is selected for each. This is beneficial if a really strong predictor exists, every bootstrap tree will likely use it for the first split and it makes the bagged tree predcitons more correlated leading to a smaller reduction in variance. By randomly selecting predictors, a good predicotr won't dominate tree fits. The benefits of random forest models are that they are efficient with large data,accurate, and work well with non-linear data. Potential drawbacks include slow training as a random subset is made each time."),width=12)))), #Creates description of random forest models if tab selected
                                      tabPanel("Model Fitting", sidebarPanel(sliderInput("train", label = h3("Train/Test Split %"),min = 0, max = 100,value = 80), #creates slider to select the training split percentage#
                                                                                selectInput("selectvar", label = "Select variables",choices = names(vars[-6]), multiple = TRUE,selected = names(vars[-6])), #Creates another variable selection where multiple can be selected for fitting models
                                                                             actionButton("submit","Submit Fit")), #Creates button to submit the model when ready#
                                                  mainPanel(tabsetPanel(id="tabset",
                                               tabPanel("Logistic Regression Model", fluidRow(box(title="Fit Statistics",verbatimTextOutput("logmodel")), box(title="Model Summary", verbatimTextOutput("logsum")),box(title="Compare To Test", verbatimTextOutput("logtest")))), #Creates output statistics for logistic model including fit, summary, and comparison to test set#
                                               tabPanel("Classification Tree", fluidRow(box(title="Fit Statistics", verbatimTextOutput("treestat")), box(title="Model Summary", verbatimTextOutput("treemodel")),box(title="Compare To Test", verbatimTextOutput("treetest")))), #Creates output statistics for classification trees including fit, summary, and comparison to test set#
                                               tabPanel("Random Forest Model", fluidRow(box(title="Fit Statistics",verbatimTextOutput("rfmodel")), box(title="Variable Importance", verbatimTextOutput("rfplot")),box(title="Compare To Test", verbatimTextOutput("rftest"))))))), #Creates output statistics for random forest model including fit, summary, and comparison to test set#
                                      tabPanel("Prediction", sidebarLayout(
                                        sidebarPanel(radioButtons("modeltype","Select Model Type",choices=c("Logistic Regression","Classification Tree","Random Forest"),selected="Logistic Regression"), #Creates button to select type of model for prediction#
                                        numericInput("pts","Points",min=0,max=100,value=30), #Creates numeric input to select a number of points to predict on#
                                        numericInput("trb","Rebounds",min=0,max=20,value=6), #Creates numeric input to select a number of rebounds to predict on#
                                        numericInput("ast","Assists",min=0,max=20,value=5), #Creates numeric input to select a number of assists to predict on#
                                        numericInput("gmsc","Game Score",min=0,max=70,value=35), #Creates numeric input to select a game score to predict on#
                                        numericInput("fgpct","Field Goal Percent",min=0,max=100,value=50)), #Creates numeric input to select a field goal percentage to predict on#
                                        mainPanel(verbatimTextOutput("predict"))) #Outputs printed predict object#
                                      )))),
tabItem(tabName="data",
        dataTableOutput("mj"),
        downloadButton("downloadData", "Download"))))))
) #Creates data page with a data table output and a download button#





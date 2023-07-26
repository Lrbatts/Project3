library(shinydashboard)
library(shiny)

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
  )
)
)
)
))


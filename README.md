# Project 3

### App Description
The purpose of this app is to analyze game-by-game statistics of Michael Jordan and analyze what statistics lead to wins in the NBA. In the Data Exploration tab, you will be able to produce various numeric and graphical summaries of the data in order to analyze potential trends or relationships in the data. In the Modeling tab, you will be able to fit multiple types of learning models including logistic regression, classification trees, and a random forest model to determine which statistics lead to wins for Michael Jordan. Summary statistics are supplied for each model you fit to help you make a decision as to the best model to use. After selecting a model, you will then be able to use it to make predictions of the probability that his team won based on different combinations of statistics. A win is represented by a 1 and a loss is represented by a 0. Finally, in the data page, you will be able to actually scroll through the data set as well as subset it and save it yourself.

###Packages Needed
1. `shinydashboard` - Package allowing creation of pages within the app and even tabs within the pages
2. `shiny` - Essential package to create and run applications
3. `tidyverse` - Package to help read in and manipulate data
4. `psych` - Package used to create one variable numeric summaries
5. `ggplot2` - Package to help create plots and graphical summaries
6. `caret` - Package allowing fit of numerous generalized models and easy training of data
7. `tree` - Package to create classification trees and summaries
8. `randomForest` - Package to create randomforest models and summaries

#### Code to Install Packages
```{r install, eval=FALSE}
install.packages(c("shinydashboard","shiny","psyche","tidyverse","ggplot2","caret","tree","randomForest"))
```
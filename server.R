library(shiny)
library(shinydashboard)

shinyServer(function(input,output){
  output$mj_img <- renderImage({
    list(src="www/michael-jordan-png-10671.png",
         width="100%",
         height=330)
},deleteFile=F)
})
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
library(shiny)
library(data.table)
library(stringi)
library(magrittr)
shinyServer(function(input, output) {
         tri <- fread("../trigrampredictiontable.csv")
         setkey(tri,uni,bi)
       bi <- fread("../bigrampredictiontable.csv")
       setkey(bi,uni)
       # x <- reactive({
       #         a = input$firstWords %>% stri_extract_last_words() %>% stri_trans_tolower()
       #         })
       #  z <- "be"
       y <- reactive({
             
                 steve <- input$firstWords %>% stri_trans_tolower() %>% 
                         stri_extract_all_words() %>% unlist()
               
                 if(length(steve) >= 2){
       tripred <- doclistTDMs[[3]][.(steve[length(steve)-1],steve[length(steve)])]$tri
                         
                         if(!is.na(tripred)){
                                 tripred
                         }else{
                                 doclistTDMs[[2]][steve[length(steve)]]$bi
                         }
                 }
          #       paste(steve[length(steve)-1],steve[length(steve)])
        })
        output$prediction <- renderText(y())
})

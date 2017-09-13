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
library(googledrive)
library(RMySQL)
convertWord <- function(predictor,conversionTable){
        conversionTable[predictor,which=TRUE]
}
shinyServer(function(input, output) {
        capstoneDb <- dbConnect(MySQL(),user="capstone", dbname = "capstone",
                                host="localhost",password = "posTrooF!2")
        zipname<-"conversionTable.zip"
        drive_download(zipname)
        unzip(zipname)
         conversiontable <- fread("./uniTDM.csv")
          setkey(tri,uni,bi)
       # bi <- fread("../bigrampredictiontable.csv")
       # setkey(bi,uni)
       
       y <- reactive({
             
                 input <- input$firstWords %>% stri_trans_tolower() %>% 
                         stri_extract_all_words() %>% unlist()
                 wordIndices <- c(input[length(input)-1],input[length(input)]) %>%
                         sapply(convertWord,conversiontable)
               
                 if(length(input) >= 2){
       #tripred <- tri[.(input[length(input)-1],input[length(input)])]$tri
                         selector <- "select tri from triTDM where uni ="
                         query <- paste(selector,wordIndices[1],"and bi =",wordIndices[2])
                        tripred <- dbGetQuery(capstoneDb, query)$tri %>% IndextoWord()
                         #if(!is.na(tripred)){
                                 tripred
                         #}else{
                          #       doclistTDMs[[2]][input[length(input)]]$bi
                         #}
                 }
          #       paste(input[length(input)-1],input[length(input)])
        })
        output$prediction <- renderText(y())
})

# drawing on the power of 800 MB of documents drawn from commonly accessed internet sources, the app
# uses the most frequent bi-,tri- and quadra-grams to predict your next word.

#Simply type into the text box and have a word suggested for you.

#The token frequency database was created using the speed of the data.table package to allow
#the use of the entire corpus






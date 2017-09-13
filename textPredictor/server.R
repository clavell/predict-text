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
source("./Shinyfunctions.R")
shinyServer(function(input, output) {
     
        output$loading <- renderText(loadingText)

#requirements for final version
      
                unzip("./TDMsforShinynoSum.zip")
                filenames <- fread("filenames.csv")$filenames
                x = 4
                TDM <- lapply(filenames[1:x],fread)
                lapply(TDM,TDMsetkey)
      
#if uncommented, below can restore tables to original
        # lapply(TDM[2:4],wordifyTable,TDM[[1]])
        # lapply(TDM,TDMsetkey)
        
        # capstoneDb <- dbConnect(MySQL(),user="capstone", dbname = "capstone",
        #                         host="localhost",password = "posTrooF!2")
        
        
################################        
       y <- reactive({

                 input <- input$firstWords %>% stri_trans_tolower() %>%
                         stri_extract_all_words() %>% unlist()

                   wordIndices <- input %>% lapply(WordtoIndex,conversionTable = TDM[[1]])

                  multiPred(TDM[2:x],wordIndices,TDM[[1]])  %>%
                                 predictionchooser(TDM[[1]])

                
                  
#below  is some code that could be used if a mysql database was being used
                         
                        #  selector <- "select tri from triTDM where uni ="
                        #  query <- paste(selector,wordIndices[1],"and bi =",wordIndices[2])
                        # tripred <- dbGetQuery(capstoneDb, query)$tri %>% IndextoWord(TDMs[[1]])

       
      })
        output$prediction <- renderText(y())
})

# drawing on the power of 800 MB of documents drawn from commonly accessed internet sources, the app
# uses the most frequent bi-,tri- and quadra-grams to predict your next word.

#Simply type into the text box and have a word suggested for you.

#The token frequency database was created using the speed of the data.table package to allow
#the use of the entire corpus






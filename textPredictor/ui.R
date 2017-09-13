#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            h3("Please enjoy 20 - 30 seconds of solitude while loading.."),
       textInput("firstWords",
                   "Input your text here",
                 "Enter your text while you wait!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h3(textOutput("prediction"))
        # textOutput("prediction")
    )
  )
))

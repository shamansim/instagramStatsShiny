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
  titlePanel("Instagram Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Got to", a("pictaram.com", href = "http://www.pictaram.com", target="_blank"), "and look for your user page. Copy and paste here your usename/id from the last part of the URL (example: shamansim/1945339775)"),
      textInput("userid", label = "", value = "shamansim/1945339775"),
      actionButton("launch", "I want my graph!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("progression")
    )
  )
))

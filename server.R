#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  contentprogression <- eventReactive(input$launch, {
    withProgress(message = 'Processing...', value = 0, {
      incProgress(1/5, detail = "1/5 collecting pages URL (can be long if many posts but <2min)")
      
      firstpageURL <- paste0("http://www.pictaram.com/user/", input$userid)
      
      extractNextPageURL <- function(pageURL){
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Step 1/5", value = 0)
        if(length(grep(pattern = "NEXT PAGE", x = readLines(pageURL, warn = F))) == 0) {
          return (pageURL)
        }
        else {
          progress$inc(1, detail = paste("Found", pageURL))
          nextURLraw <- readLines(pageURL, warn = F)[grep(pattern = "NEXT PAGE", x = readLines(pageURL, warn = F))]
          return(c(pageURL, extractNextPageURL(strsplit(nextURLraw, "\"") %>% unlist %>% extract(2))))
        }
      }
      
      pagesURL <- extractNextPageURL(firstpageURL)
      
      incProgress(1/5, detail = "2/5 downloading web pages (can be long if many posts but <5min)")
      pagesContent <- vector(mode = "character", length = 0)
      
      progress2 <- shiny::Progress$new()
      on.exit(progress2$close())
      progress2$set(message = "Step 2/5", value = 0)
      for(j in 1:length(pagesURL)){
        progress2$inc(1/length(pagesURL), detail = paste("Downloading", pagesURL[j]))
        pagesContent <- c(pagesContent, readLines(pagesURL[j], warn = F))
      }

      concatenatedPage <- unlist(pagesContent)
      
      incProgress(1/5, detail = "3/5 processing datas")
      # time
      timedate <- '<i class="fa fa-clock-o"></i>([^<]*)</span>' %>%
        grep(concatenatedPage, value=TRUE) %>%
        gsub('<[^>]*>', "", .) %>%
        gsub('  ', "", .) %>%
        strptime("%I:%M%p %m/%d/%Y")
      
      # comments
      comments <- '<i class="fa fa-comments-o"></i>' %>%
        grep(concatenatedPage, value=TRUE) %>%
        gsub('<[^>]*>', "", .) %>%
        gsub(' ', "", .) %>%
        as.integer
      
      # likes
      likes <- '<i class="fa fa-heart"></i>' %>%
        grep(concatenatedPage, value=TRUE) %>%
        gsub('<[^>]*>', "", .) %>%
        gsub(' ', "", .) %>%
        as.integer
      
      incProgress(1/5, detail = "4/5 calculating basic statistics")
      nbPosts <- length(timedate)
      averageLikes <- mean(likes)
      averageComments <- mean(comments)
      
      # grather everything
      DataClean <- data.frame(time = timedate, likes = likes, comments = comments) %>%
        arrange(time)
      
      # graph
      incProgress(1/5, detail = "5/5 generating graph")
      subtitle <- paste("Number of posts:", nbPosts, ";", "Average nb of likes:", round(averageLikes, 1), ";", "Average nb of comments:", round(averageComments, 1))
      username <- unlist(strsplit(input$userid, "/"))[1]
      todaysdate <-  format(Sys.time(), "%d/%m/%Y %H:%M")
      titleprogression <- paste0("Instagram progression of @", username, " (", todaysdate, ")")
      
      ggplot(DataClean, aes(time, likes)) +
        geom_smooth(method = "loess") +
        geom_point(aes(size = comments)) +
        scale_size_continuous(range = c(2, 11)) +
        theme_minimal() +
        labs(x = "", y = "Number of likes") +
        ggtitle(bquote(atop(
          .(titleprogression),
          atop(italic(.(subtitle)))
        ))) +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_x_datetime(breaks = date_breaks("1 month"), labels = date_format("%m/%Y"))
    })
  })
  
  output$progression <- renderPlot({
    contentprogression()
  })
  
})

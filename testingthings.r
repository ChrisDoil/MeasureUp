#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyr)
library(ggplot2)

myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTnuFK5HWAcKoNGHlXqMAT8JPF6koSDLl-fwduRdU1Cu7Nin_zlfo7wDxGHxIWOTQzZOZH0pvIQZ1WL/pub?gid=0&single=true&output=csv"

measureup <- read.csv(url(myurl), header = TRUE)
# head(measureup)

measureup_cat <- gather(measureup, key = "category", value = "score", "TAV", "A&D", "IAM", "RM", "C&PKI", "T&T", "Overall")




# Define UI for application that draws table
ui <- fluidPage(
  hr(),
  # Application title
  column(3, offset = 5, titlePanel("Measure Up Scores")
  ),
  
  
  
  br(),
  br(),
  br(),
  
  
  fluidRow(
    column(1, align = "center",
           
           selectInput("nameChoice", "Name", c("All", measureup$Name)
           )
           
           
    ),
    
    br(),
    br(),
    br(),
    br(),
    h4("Summary Statistics", align = "center"),
    
    verbatimTextOutput("stats"),
    align = "center"
  ),
  
  plotOutput("distPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if (input$nameChoice != "All") {
      x    <- measureup_cat[which(measureup_cat$Name == input$nameChoice), ]
      
      myplot <- ggplot(data = x, aes(x$score, y = x$Attempt, fill = x$Name))
      
      
    } else {
      
      x    <- measureup_cat
      
      myplot <- ggplot(data = x, aes(x$score, y = x$Attempt)
      )
      
    }
    
    # draw the histogram with the specified number of bins
    
    myplot <- myplot + 
      geom_point(aes(colour = factor(x$Name))) +
      facet_grid(vars(x$category)) + 
      ylab(NULL) +
      xlab("score") +
      labs(fill = "Name") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    
    myplot
  })
  
  
  
  output$stats <- renderPrint({
    
    if (input$nameChoice != "All") {
      x    <- measureup[which(measureup$Name == input$nameChoice), ]
    } else {
      x    <- measureup
    }
    
    summary(x[, 3:9])
  })
}


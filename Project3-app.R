#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load Relevant Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(mdsr)
library(mosaic)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(fontawesome)
library(DT)
library(shinyjs)
# Define UI for shiny app
ui <- fluidPage(
    #Set up for font awesome (gives UPS logo)
    tags$script(src = "https://kit.fontawesome.com/cc3b2bd0f4.js"),
    #Title for Page
    titlePanel(   tags$div(
      tags$i(class = "fa-brands fa-ups"),
      tags$span("Hub Buck Earning Simulator")
    )),
    #Set theme for UI
    #shinythemes::themeSelector(),
    theme = shinytheme("superhero"),
    #Button to run the function
    actionButton("Run" , "Run" , icon=tags$i(class="fa-solid fa-boxes-packing")),
    #Side Bar
    sidebarLayout(
    #Side Bar Panel
    sidebarPanel(
      #Help Text
      helpText("Get an estimation for how many HubBucks you might earn in a given number of work days"),
      #Run time Estimate
      useShinyjs(),
      htmlOutput("Progress"),
      textOutput("Estimate"),
      #Sliders
      progressBar(id = "Intervals", value = 100 , display_pct = TRUE),
      sliderInput("Intervals" , "Intervals" , min = 0 , max = 500 , value = 1),
      sliderInput("DaysWorked" , "Days Worked" , min = 0 , max = 365 , value = 12*5),
      sliderInput("Packages" , "Packages through facility per day" , min = 0 , max = 250000 , value = 120000),
      sliderInput("Salt" , "Salt Rate" , min = 0 , max = .05 , value = .0001),
      sliderInput("Misload" , "Rate of bad packages" , min = 0 , max = 1/2000 , value = 1/4620),
      sliderInput("Catch" , "Chance to Catch a bad package" , min = 0 , max = 1 , value = .95),
    ),
    #Main Panel
    mainPanel(
      plotOutput("Histogram"),
      verbatimTextOutput("summary")
    )
    
  )   
)

# Define server logic
server <- function(input, output, session){
  ##Load in Function:
  hubBucks <- function(intervals  , trials , Misload , Salt , chanceToCatch , packCount){
    int_count = 0
    downCount = 0
    totalCount <- vector(length = intervals)
    while(int_count <= intervals){
      int_count = int_count + 1
      count = 0
      hold = vector(length=trials)
      caught = vector(length=trials)
      while(count <= trials){
        count2 = 1
        packagesForDay <- 1
        packagesForDay <- seq(packagesForDay , packCount , 1)
        while(count2 <= packCount){
          count2 = count2 + 1
          if(rbinom(1 , 1 , Salt) == 1){
            packagesForDay[count2] = 0
          }
          if(rbinom(1 , 1 , Misload) == 1){
            packagesForDay[count2] = 0
            packagesForDay[count2]
          }
        }
        myFlow <- rpois(5 , 300)
        myFlow <- sum(myFlow)
        myPackages <- sample(packagesForDay  , myFlow, replace = FALSE)
        count = count + 1
        badToMe <- length(myPackages[myPackages == 0])
        hold[count] = badToMe
      }
      caught = 0
      for(package in hold){
        if(package == 0 & rbinom(1 , 1 , chanceToCatch) == 1){
          caught = caught + 1
        }
      }
      caught = caught %/% 3
      count2 = 0
      hubBucksCount = vector(length=caught)
      while(count2 < caught){
        count2 = count2 + 1
        if(rbinom(1 , 1 , .5) == 1){
          hubBucksCount[count2] = 5
        }else{
          hubBucksCount[count2] = 10
        }
      }
      print(int_count)
      phrase <- paste("<B>" , int_count - 1 , "</B>")
      rv$outputText = paste0( phrase , "<B> intervals done </B>")
      shinyjs::html(id = 'Progress', rv$outputText)
      updater <- 100/(intervals + 1)
      downCount <- downCount + updater
      updateProgressBar(session = session, id = "Intervals", value = downCount)
      totalCount[int_count] = sum(hubBucksCount)
    }
    return(totalCount)
  }
  #Setup reactive values
  outputData <- reactiveVal()
  bins <- reactiveVal()
  daysWork <- reactiveVal()
  rv <- reactiveValues(outputText = '')
  #Give Estimated Run Time for Simulation , this is pretty bad granted
  output$Estimate <- renderText(paste("Estimated run time:" , 
                                      floor((input$Intervals * input$DaysWorked) / 60) , 
                                      "minute(s)"))
  #Run Function on Button Press
  observeEvent(input$Run , {
      rv$outputText = paste( "<B> 0 </B>" , "<B> intervals done </B>")
      shinyjs::html(id = 'Progress', rv$outputText)

      buckData <- hubBucks(input$Intervals ,
                    input$DaysWorked,
                    input$Misload,
                    input$Salt,
                    input$Catch,
                    input$Packages)
  
      outputData(as.data.frame(buckData))
      bins(input$Intervals / 2)
      daysWork(input$DaysWorked)
  })
  #Output summary of function output:
  output$summary <- renderPrint({
    req(outputData())
    summary(outputData())
    
  })
  #Output for Histogram;
  output$Histogram <- renderPlot({
    req(outputData())
    ggplot(outputData() , aes(x = buckData)) +
      geom_histogram(binwidth = bins() , color = "black" , fill = "gray") +
      xlab(paste("Hub Bucks Earned in", 
                 daysWork() , 
                 "days worked"))
  })
}
  
  
#Run the application 
shinyApp(ui = ui, server = server)


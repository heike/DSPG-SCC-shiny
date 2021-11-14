library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(rlang)

## import dataset

levy <- read.csv("levy_all.csv")


ui <- fluidPage(
  fluidRow(
    numericInput(
      inputId = "numGroups",
      label = "Number of Groups",
      value = 4
    ),
    actionButton("action", "Get Table"),
    
    tableOutput("table1")
  )
)


server <- function(input, output) {
  ##values <- reactiveValues(groupTable = NULL)
  
  data_output <- eventReactive(input$action, {
    groupTable <- data.frame(Group = 1:input$numGroups, NumberofCities = 1:input$numGroups, GroupPopulation = 1:input$numGroups)
    
    citypop <- levy %>%
        filter(Year == 2019) %>%
        select(Population) %>%
        arrange(Population)
    
    totalPop <- sum(citypop$Population)
    
    threshold <- totalPop / input$numGroups
      
    cityTracker <- 1
    
    for (i in 2:input$numGroups - 1) {
      currentPop <- 0
      currNumCities <- 0
      
      while (currentPop < threshold & cityTracker <= 100) {
        currentPop <- currentPop + citypop[cityTracker,1]
        currNumCities <- currNumCities + 1
        cityTracker <- cityTracker + 1
      }
      
      groupTable[i,2] <- currNumCities
      groupTable[i,3] <- currentPop
      
    }
    
    groupTable[input$numGroups, 2] <- 100 - cityTracker
    
    if (groupTable[input$numGroups, 2] == 0) {
      groupTable[input$numGroups, 3] = 0
      print("1 or more cities excluded")
    }
    else {
      groupTable[input$numGroups, 3] <- totalPop - sum(groupTable$GroupPopulation)
    }
    
    groupTable
    })
  
  output$table1 <- renderTable(data_output())
  
}

shinyApp(ui = ui, server = server)

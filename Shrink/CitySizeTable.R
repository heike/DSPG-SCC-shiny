library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(rlang)

## import dataset

levy <- read.csv("levy_all.csv")

citypop <- levy %>%
  filter(Year == 2019) %>%
  select(CITY.NAME, Population, CITY.SIZE) %>%
  arrange(Population)

totalPop <- sum(citypop$Population)


ui <- fluidPage(
  fluidRow(
    numericInput(
      inputId = "numGroups",
      label = "Number of Groups",
      value = 4
    ),
    submitButton("Get Table"),
    
    tableOutput(outputId = "table1")
  )
)


server <- function(input, output) {
  
  values <- reactiveValues()
  values$groupTable <- data.frame(Group = integer(), NumberofCities = integer(), GroupPopulation = integer())
  
  newEntry <- observe({
    threshold <- totalPop / input$numGroups
    
    cityTracker <- 1
    
    ## add first n - 1 rows
    for (i in 1:input$numGroups - 1) {
      currentPop <- 0
      currNumCities <- 0
      
      while (currentPop < threshold) {
        currentPop =+ citypop[cityTracker,2]
        currNumCities =+ 1
        cityTracker =+ 1
      }
      
      newRow <- isolate(c(i, currNumCities, currentPop))
      isolate(values$groupTable <- rbind(values$groupTable, newRow))
    }
    
    ## add last row using deduction
    lastRow <- isolate(c(input$numGroups, 100 - sum(groupTable$NumberofCities), totalPop - sum(groupTable$GroupPopulation)))
    isolate(values$groupTable <- rbind(values$groupTable, lastRowRow))
  })
  
  output$table1 < renderTable({values$groupTable})
  
}

shinyApp(ui = ui, server = server)

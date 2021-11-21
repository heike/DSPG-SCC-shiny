library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(rlang)

## import dataset

levy <- read.csv("levy_all.csv")

ui <- wellPanel(
  fluidPage(
    flowLayout(
      htmlOutput("g1label"),
      htmlOutput("g1bot"),
      numericInput("g1upper", "Enter Group 1 Upper Bound", 0)
    ),
    
    flowLayout(
      htmlOutput("g2label"),
      htmlOutput("g2bot"),
      numericInput("g2upper", "Enter Group 2 Upper Bound", 1)
    ),
    
    flowLayout(
      htmlOutput("g3label"),
      htmlOutput("g3bot"),
      numericInput("g3upper", "Enter Group 3 Upper Bound", 2)
    ),
    
    flowLayout(
      htmlOutput("g4label"),
      htmlOutput("g4bot"),
      htmlOutput("g4upper")
    )
  ),
  
  mainPanel(
    fluidRow(
      actionButton("action", "Generate Table"),
      dataTableOutput("table1")
    )
  )
)


  
server <- function(input, output) {
  output$g1label <- renderText({paste("<b>Group 1 Lower Bound:</b>")})
  output$g1bot <- renderText({paste("<b><p style=font-size:20px>0</p></b>")})
  
  output$g2label <- renderText({paste("<b>Group 2 Lower Bound:</b>")})
  output$g2bot <- renderText({paste("<b><p style=font-size:20px>", input$g1upper[1] + 1, "</p></b>")})
  
  output$g3label <- renderText({paste("<b>Group 3 Lower Bound:</b>")})
  output$g3bot <- renderText({paste("<b><p style=font-size:20px>", input$g2upper[1] + 1, "</p></b>")})
  
  output$g4label <- renderText({paste("<b>Group 4 Lower Bound:</b>")})
  output$g4bot <- renderText({paste("<b><p style=font-size:20px>", input$g3upper[1] + 1, "</p></b>")})
  output$g4upper <- renderText({paste("<b><p style=font-size:20px>25000</p></b>")})
  
  
  data_output <- eventReactive(input$action, {
    sizeTable <- levy %>%
      filter(Year == 2019) %>%
      select(CITY.NAME, Population) %>%
      mutate(`City Size` = ifelse(Population <= input$g1upper, 
                                "Group 1",
                                ifelse(Population <= input$g2upper,
                                       "Group 2",
                                       ifelse(Population <= input$g3upper,
                                              "Group 3",
                                              "Group 4")))) %>%
      group_by(`City Size`) %>%
      summarise(`Number of Cities` = n(),
                `Group Population` = sum(Population))
    
    sizeTable
  })
  
  output$table1 <- renderDataTable(data_output())
}


shinyApp(ui = ui, server = server)








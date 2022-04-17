library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(tidyr)
library(dplyr)
library(rlang)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(maps)
library(shinyjs)
library(fresh)

# use the theme
source("theme.R")

jscode <- "var referer = document.referrer;
           var n = referer.includes('economic');
           var x = document.getElementsByClassName('logo');
           if (n != true) {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/events/dspg2021/postersessions\">' +
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2021 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a>';
                              
           } else {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a>';
           }"

# Drive Time Analysis ----------------------------------------------------------------
all_cities <- read.csv("Total_City_Population_by_Year-with-estimate_new.csv")

# Levy Rates --------------------------------------------------------------

#Base Dataset 2006 - 2020
levy <- read.csv("levy_all.csv")

levy <- levy %>%
  rename(`Job Quality` = QOLjobs_i14,
         `Medical Quality` = QOLmed_i14,
         `K-12 Quality` = QOLk12_i14,
         `Housing Quality` = QOLhousing_i14,
         `Governmental Services Quality` = QOLgovt_i14,
         `Child Services Quality` = QOLchildsrv_i14,
         `Senior Services Quality` = QOLseniorsrv_i14)

levy <- levy %>% mutate(
  CITY.SIZE = factor(CITY.SIZE),
  CITY.SIZE = reorder(CITY.SIZE, Population, median, na.rm=TRUE)
)


all_iowa <- read.csv("Current Data/levy-rates-2006-2020_ALL_IOWA.csv") %>%
  filter(Levy.Rate > 0) %>%
  mutate(Prop.Tax.Revenue = (Adjusted.Amount * Levy.Rate) / 1000,
         Per.Capita.Revenue = Prop.Tax.Revenue / Population,
         Per.Capita.Valuation = Adjusted.Amount / Population)


# Dotplot of Iowa ----------------------------------------------------

## set the rows to be used as metrics - all continuous numerical values; will end up being the size of dots
metrics <- c(
  "Population",
  "Levy.Rate",
  "Adjusted.Amount",
  "Per.Capita.Revenue",
  "Per.Capita.Valuation",
  "Fiscal.Effort",
  "Fiscal.Capacity",
  "miles50",
  "min50",
  "miles10",
  "min10",
  "miles25",
  "min25",
  "Job Quality",
  "Medical Quality",
  "K-12 Quality",
  "Housing Quality",
  "Governmental Services Quality",
  "Child Services Quality",
  "Senior Services Quality"
)

## set the rows to be used as colors of dots - if factor use discrete, if continuous use continuous
fills <- c(
  "CITY.SIZE",
  "hub50",
  "hub25",
  "Population",
  "Levy.Rate",
  "Adjusted.Amount",
  "Per.Capita.Revenue",
  "Per.Capita.Valuation",
  "Fiscal.Effort",
  "Fiscal.Capacity",
  "miles50",
  "min50",
  "miles10",
  "min10",
  "miles25",
  "min25",
  "Job Quality",
  "Medical Quality",
  "K-12 Quality",
  "Housing Quality",
  "Governmental Services Quality",
  "Child Services Quality",
  "Senior Services Quality"
)

levy_qol <- levy %>%
  select(Year, CITY.NAME, longitude, latitude, all_of(fills), all_of(metrics)) %>%
  filter(Year >= 2010,
         Year <= 2016)

RV <- reactiveValues()


# Measures of Crop Diversity  ---------------------------------------------------------

crop_mets <- read_csv("crop-metrics.csv") %>%
  mutate(crop = str_to_title(crop),
         crop = str_replace_all(crop, "_", " ")) %>%
  filter(year == 2014)

div_mets <- read.csv("crop-diversity_metrics.csv") %>%
  filter(year == 2014) %>%
  mutate(ag_prop_all = value) 


# Quality of Life by Metrics ------------------------------------------------

ds <- read.csv("correlation-dash.csv")

ds$CITY.SIZE <- factor(ds$CITY.SIZE,
                       levels = c("RURAL",
                                  "RURAL PLUS",
                                  "URBAN CLUSTER",
                                  "MICROPOLITAN"))
levels(ds$CITY.SIZE)[4] = "NANO- and MICROPOLITAN"

ds <- ds %>%
  select(-X) %>%
  rename(
    Jobs = QOLjobs_i14,
    `Medical Services` = QOLmed_i14,
    `Local Schools` = QOLk12_i14,
    Housing = QOLhousing_i14,
    `Local Government Services` = QOLgovt_i14,
    `Child Care` = QOLchildsrv_i14,
    `Senior Services` = QOLseniorsrv_i14,
    `Ag Land Prop` = Proportion.of.land.dedicated.to.ag,
    `Unique Crops` = Number.of.unique.crops.grown,
    `Shannon Diversity` = Shannon.s.Diversity.Index,
    `Shannon Evenness` = Shannon.s.Evenness.Index,
    `Simpson Diversity` = Simpson.s.Diveristy.Index..Chance,
    `Simpson Evenness` = Simpson.s.Evenness.Index,
    `QoL Average` = QoL.Average
  )

indices <- c(
  "Fiscal.Capacity",
  "Fiscal.Effort",
  "Ag Land Prop",
  "Unique Crops",
  "Shannon Diversity",
  "Shannon Evenness",
  "Simpson Diversity",
  "Simpson Evenness",
  "miles50",
  "min50",
  "miles10",
  "min10",
  "miles25",
  "min25"
)

qol_metrics <- c(
  "Jobs",
  "Medical Services",
  "Local Schools",
  "Housing",
  "Local Government Services",
  "Child Care",
  "Senior Services",
  "QoL Average"
)

# Shiny Header --------------------------------------------------------------

header <- dashboardHeader(title = tags$a(href='http://ruralshrinksmart.org',
                               tags$img(src='http://ruralshrinksmart.org/wp-content/uploads/2020/12/bckss-29.png')))

# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  ### sidebar dropdown
  
  dropdown(div(id = 'my_numinput', 
               h4(HTML("<b>Set the slider range to your desired population values, starting with the first range:</b>")),
               sliderInput("range1", "Rural:", min=0, max=2500, value=c(0,800), step = 50),
               sliderInput("range2", "Rural Plus:", min=0, max=7500, value=c(800, 2500), step = 100),
               sliderInput("range3", "Urban Cluster:", min=0, max=20000, value=c(2500, 5000), step = 100),
               sliderInput("range4", "Nano- and Micropolitan:", min=0, max=25000, value=c(5000, 25000), step = 100),
               selectInput(
                 inputId = "fiscalButtons",
                 label = h4(HTML("<b>Choose Grouping for Fiscal Metrics</b>")),
                 choices = list("Shrink Smart Cities" = "shrink",
                                "All of Iowa" = "all")
               ),
               selectInput(inputId = "chooseCities",
                           label = h4(HTML("<b>Choose Cities to Highlight</b>")),
                           choices = c(unique(levy$CITY.NAME)),
                           multiple = TRUE)),
           
           actionButton("action1", strong("APPLY CHANGES")),
           actionButton("actionReset", strong("RESET LIMITS TO DEFAULT")),
           
           tags$style(type = "text/css", "#my_numinput {color: black}"),
           width = 450,
           animate = TRUE,
           label = "Options",
           icon = icon("cog", lib = "glyphicon"),
           size = "sm"),
  
  sidebarMenu(
    id = "tabs",
    menuItem(
      tabName = "overview",
      text = "Project Overview",
      icon = icon("info-circle")
    ),
    menuItem(
      text = "Dotplot of Iowa",
      tabName = "Dotplot",
      icon = icon("far fa-map")
    ),
    menuItem(
      text = "Levy Rates of Cities",
      tabName = "LevyCity",
      icon = icon("fas fa-chart-line")
    ),
    menuItem(
      text = "City Comparison",
      tabName = "Compare",
      icon = icon("chart-bar")
    ),
    menuItem(
      text = "Drive Time Analysis",
      tabName = "DriveTime",
      icon = icon("car")
    ),
    menuItem(
      text = "Crop Diversity Map",
      tabName = "CropDiversity",
      icon = icon("map")
    ),
    menuItem(
      text = " Crop Diversity Measures",
      tabName = "CropMeasures",
      icon = icon("seedling")
    ),
    menuItem(
      text = "Quality of Life by Metrics",
      tabName = "QoLMetrics",
      icon = icon("heart")
    ),
    menuItem(
      tabName = "team",
      text = "The Shrink Smart Team",
      icon = icon("user-friends")
      )
    )
  )


# Body --------------------------------------------------------------------

body <- dashboardBody(
  useShinyjs(),
  use_theme(mytheme),
  
  # Overview Tab ------------------------------------------------------------
  
  tabItems(
    
    
    tabItem(tabName = "overview",
            fluidRow(
              box(
                title = "Project Overview",
                closable = FALSE,
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("2021 DSPG Project Iowa Shrink Smart Communities"),
                h2("Project Description"),
                p(
                  "While the state of Iowa has seen a gentle increase of about 0.4% in population, many of Iowa's smaller, more rural communities  have experienced significant population declines over the last few decades. "
                ),
                HTML(
                  "<p>The <a href='https://smalltowns.soc.iastate.edu/iowa-small-town-poll/'>Iowa Small Town Poll</a> has been tracking quality of life in small towns all acros Iowa since 1994 and has established that a shrinking population is not automatically associated with a loss in the quality of life.</p>"
                ),
                p(
                  "Our project focuses on factors affecting the perception of quality of life in small and shrinking rural communities in Iowa.  "
                ),
                div(
                  align = "right",
                  HTML(
                    "<p>
                      <em>
                        <strong>Fig.1 </strong>
                          Change in population in Iowa's communities between 2010 and 2019. Red indicates a population loss of 2.5% or more, blue indicates a gain of 2.5% or more. The size of dots is indicative of population size.
                      </em>
                    </p>"
                  ),
                  img(
                    src = 'Iowa-population-change-2010-19.png',
                    width = "45%",
                    align = "right"
                  )
                ),
                div(
                  plotlyOutput("plotly_loss", width = "45%", height = "300px"),
                  HTML(
                    "<p><em><strong>Fig.2 </strong>Boxplots of percent change in population between 2006 and 2019 in the rural towns of the Small Town Survey. Almost 75% percent of all the rural communities have experienced a loss in population, while only about half of the slightly larger communities (rural plus) have experienced a loss. </em></p>"
                  )
                ),
                h2("Project Goals"),
                p(
                  "The goal of our project is to help communities focus their limited resources on improving quality of life rather than using scarce resources to try to grow (as this is unlikely in most towns)."
                ),
                h2("Our Approach"),
                p(
                  "The team has built a community information ecosystem that makes use of publicly available data and links it to some proprietary data sets to help communities understand, utilize, and collect new data about their towns and peer communities. The ecosystem uses statistical modelling and cutting-edge visualization strategies to make data more accessible to stakeholders in this community including city staff, local leaders, and the public."
                ),
                h2("Ethical Considerations"),
                p(
                  "The data used to create our ecosystem has been through the use of publicly available data. Sources include:"
                ),
                
                HTML("<ul>
                        <li><a href='https://console.developers.google.com/apis'>Google Developer API</a></li>
                        <li><a href='https://dom.iowa.gov/'>Iowa Department of Management</a></li>
                        <li><a href='https://smalltowns.soc.iastate.edu/iowa-small-town-poll/'>Iowa Small Town Poll</a></li>
                        <li><a href='https://www.tandfonline.com/doi/abs/10.1080/10106049.2011.562309'>Monitoring US agriculture: the US Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer Program - <i>C. Boryan, Z.Yang, R. Mueller, M. Craig</i></a></li>
                        <li><a href='https://data.census.gov/cedsci/'>United States 2010 Decennial Census</a></li>
                        <li><a href='https://nassgeodata.gmu.edu/CropScape/'>United States Department of Agriculture</a></li>
                      </ul>"
                ),
                h2("Sponsor"),
                p(
                  "This project is sponsored by Kimberly Zarecor with ISU Smart & Connected Communities and the National Science Foundation."
                  ),
                img(src = '1024px-NSF_logo.png',
                    width = "5%",
                    align = "right")
                )
              )
            ),
    
    # Dotplot of Iowa ---------------------------------------------------------
    
    tabItem(
      tabName = "Dotplot",
      titlePanel(title = "Small towns of Iowa"),
      fluidRow(box(
        width = 10,
        p(
          "The dot map of Iowa below shows all communities that are part of the Small Town Survey.  Each dot corresponds to one of the 99 communities of the survey."
        ),
        HTML(
          "The default dot color option is city size which is determined by population of the community.<br>
          <p><strong>Rural</strong> cities have a population of fewer than <strong>800</strong> residents.<br>
           Cities classified as <strong>Rural Plus</strong> have <strong>between 800 and 2,499</strong> residents. <br>
           <strong>Urban Cluster</strong> communities are defined as having <strong>between 2,500 and 4,999</strong> residents. <br>
          <strong>Nano- and Micropolitan</strong> cities have a population of <strong>more than 5000</strong>.</p>"),
        p(
          "Selecting a variable in the dropdown menu on the left will change the size of dots to represent the values according to the variable. Smaller sized dots represent a lower value, while bigger dots show higher values.  "
        ),
        p("Hover over points for additional information on each community."),
        HTML(
          "<p align='right'>Source: <em>Population data from the American Community Survey (ACS) US Census Bureau 2010 – 2016.</em></p>"
        )
      )),
      fluidRow(
        box(
          width = 3,
          selectInput("Year2", "Select Year:",
                      choices = levels(factor(levy_qol$Year))),
          varSelectInput("metric", "Select Dot Size:",
                         data = levy_qol[metrics]),
          varSelectInput("fills", "Select Dot Color:",
                         data = levy_qol[fills])
        ),
        box(
          plotlyOutput("mapPlot", width = 0.75 * 1100, height = 0.75 * 700),
          status = "primary"
        )
      )
    ),
    
    # Levy Plot ---------------------------------------------------------------
    
    tabItem(
      tabName = "LevyCity",
      titlePanel(title = "Levy Rates in Iowa 2006 - 2020"),
      fluidRow(box(
        width = 12,
        p(
          "A levy rate is another term for tax rate. In Iowa the General Fund Levy for all cities is capped at $8.10, if the total levy rate is higher than that there is a specific purpose, such as; debt repayment, city improvements, and emergencies."
        ),
        p(
          "The line graph is a tool to compare tax rates with other communities of like sizes. How high or low a levy/tax rate is can be an indicator of fiscal health of a community."
        ),
        p(
          "Hovering over with cursor shows year, city name, and it's value. Source: Iowa Department of Management 2006 - 2020."
        ),
        HTML(
          "<p align='right'>Source: <em>Levy Rates data from Iowa Department of Management 2006 – 2020.</em></p>"
        )
      )),
      fluidRow(
        box(
          width = 4,
          selectInput(
            "which_cities",
            "Choose a City",
            selected = "All",
            choices = c("All", unique(levy$CITY.NAME)),
            multiple = TRUE
          ),
          selectInput(
            "which_variable",
            "Choose a dependent variable",
            selected = "Adjusted.Amount",
            choices = c(
              "Population",
              "Adjusted.Amount",
              "Levy.Rate",
              "Prop.Tax.Revenue",
              "Fiscal.Capacity",
              "Fiscal.Effort"
            )
          ),
          multiple = FALSE
        ),
        box(plotlyOutput("plotly", height = 1000))
      )
    ),
    
    # City Comparison ---------------------------------------------------------
    
    tabItem(
      tabName = "Compare",
      titlePanel(title = "Compare Fiscal Capacity and Fiscal Effort"),
      fluidRow(box(
        width = 12,
        p(
          "The bar graph below focuses on fiscal capacity and fiscal effort. Cities with higher fiscal capacity and lower fiscal effort tend to be more economically comfortable, while cities with lower fiscal capacity and higher fiscal effort typically have more economic stress."
        ),
        p(
          "Fiscal Capacity is shown in pink and fiscal effort is shown in blue. The graphs can be adjusted by year."
        ),
        p(" By selecting two cities in the dropdown menu, the bar graph will adjust to a side-by-side comparison of a city's fiscal capacity and effort."),
        HTML(
          "<p align='right'>Source: <em>Fiscal Effort and Capacity derived from data by the Iowa Department of Management 2006 – 2020.</em></p>"
        )
      )),
      fluidRow(
        box(
          width = 4,
          selectInput("Year", "Select Year:",
                      choices = unique(levy$Year)),
          selectInput("City1", "Select City 1:",
                      choices = levels(factor(levy$CITY.NAME))),
          selectInput("City2", "Select City 2:",
                      choices = levels(factor(levy$CITY.NAME)))
        ),
        box(
          plotOutput("ComparePlot"),
          width = 6,
          status = "primary"
        )
      )
    ),
    
    # Drive Time Analysis -------------------------------------------------------------
    
    tabItem(
      tabName = "DriveTime",
      titlePanel(title = "Drive Times from each of the 99 Communities to the closest Metropolitan Statistical Area (MSA)"),
      fluidRow(box(
        width = 12,
        p(
          "This drive time analysis focuses on a necessity of rural Iowans: cars. Drive times are featured from the 99 communities to the closest Metropolitan Statistical Area (MSA) to gain an understanding of how long an Iowan from one of the 99 Communities has to drive in order access certain resources. "
        ),
        p(
          "Each colored point corresponds to a community in Iowa. Hubs are sized according to their population.
            Filled dots indicate that a community is within 50 mins driving distance to the closest MSA or micro MSA. "
        ),
        p("Hover over points for additional information on each community."),
        HTML(
          "<p align='right'>Source: <em>(micro)MSA defined by the US Census Bureau based on the 2010 Decennial Census.</em></p>"
        ),
        HTML(
          "<p align='right'> <em>Drive times between locations are estimated and accessed through the Google developer API.</em></p>"
        )
      )),
      fluidRow(
        box(
          width = 3,
          selectInput("msa", "Select Hub Size:",
                      choices = c("Metropolitan Statistical Area (MSA)", "Micropolitan Statistical Area (µMSA)"))
        ),
        box(
          title = (""),
          width = 9,
          side = "right",
     #     tabPanel("Economic Hubs"),

          plotlyOutput("LevyMapPlotly", height = 700)
        )
       )
    ),
    # ArcGis Crop Diversity Map ---------------------------------------------------------------
    
    tabItem(
      tabName = "CropDiversity",
      titlePanel(title = "Map of Crops Grown in Iowa for 2020"),
      fluidRow(box(
        width = 12,
        p(
          "The ArcGIS map below shows the type of crops grown across Iowa."
        ),
        p(
          "On the left is a legend of all the crops shown on the graph. Each blue circle represent a radius of five miles around one of the 99 communities.
          The most popular crops grown are corn and soybeans in yellow and dark green respectively. "
        ),
        p("Zoom in and out to gain a more descriptive picture.")
      )),
      fluidRow(mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel("Arc",
                 br(),
                 htmlOutput("arcFrame"))
      )))
    ),
    
        
    # Measures of Crop Diversity ----------------------------------------------
    
    tabItem(
      tabName = "CropMeasures",
      titlePanel(title = "Measures for Crop Diversity"),
      fluidRow(box(
        width = 12,
        p(
          "The lollipop chart and map of Iowa are shown to gain a further understanding of crop diversity in the 99 communities."
        ),
        p(
          "The left showcases a lollipop chart that displays the type of crops grown in a 5 mile radius around the city selected.
          On the right displays a dot map, with each dot representing one of the 99 communites. Different plots can be selected by choosing a crop diversity metric which changes color by light to dark as highest to lowest."
        ),
        p(
          "Begin by choosing a city or clicking on a dot on the map. Each dot is interactive and provides further information by hovering over with cursor."
        )
      )),
      fluidRow(column(width = 4,
                      box(
                        width = NULL,
                        selectInput(
                          "which_city_cropdiv",
                          "Choose a City",
                          selected = "Monroe",
                          choices = c(unique(crop_mets$city_name)),
                          multiple = FALSE
                        )
                      )),
               column(
                 width = 4,
                 offset = 2,
                 box(
                   width = NULL,
                   selectInput(
                     "which_divmet",
                     "Choose a Diversity Metric",
                     selected = "Shannon's Evenness Index",
                     choices = c(unique(div_mets$div_metric_nice)),
                     multiple = FALSE
                   )
                 )
               )),
      fluidRow(column(
        width = 6,
        box(
          plotlyOutput("plotly_cropdiv", height = 500),
          width = NULL,
          status = "primary"
        )
      ),
      column(
        width = 6,
        box(
          plotlyOutput("plotly_divmet", width = 900, height = 500),
          width = NULL,
          status = "primary"
        )
      ))
    ),
    
    
    # Quality of Life by Metrics ----------------------------------------------
    
    tabItem(
      tabName = "QoLMetrics",
      titlePanel(title = "Quality of Life by Metrics"),
      fluidRow(box(
        width = 12,
        p(
          "The Scatterplot below displays a dot for each of the communities in the Small Town Survey." 
        ),
        p(
          "Select a demographic for the community that will be plotted on the X-axis and a Quality of Life Index to plot on the Y-axis. Year only applies when plotting Fiscal Capacity or Fiscal Effort. 
          The Pearson-Spearman correlation between the two variables is then displayed along with a line of best fit."
        ),
        p("Hover over the dots for more details on which community is being plotted and the values of the selected metric and index."),
        HTML(
          "<p align='right'>Source: <em>MSA defined by the US Census Bureau based on the 2010 Decennial Census.</em></p>"
        ),
        HTML(
          "<p align='right'> <em>Fiscal Metrics by Iowa Department of Management 2010 - 2016.</em></p>"
        )
      )),
      fluidRow(
        box(
          width = 4,
          selectInput("Year3", "Select Year:",
                      choices = levels(factor(ds$Year))),
          varSelectInput("fiscal", "Select Community Demographic:",
                         data = ds[indices]),
          varSelectInput("qol", "Select Quality of Life (QoL) Index:",
                         data = ds[qol_metrics]),
          textOutput("qolCorrelation")
        ),
        box(
          plotlyOutput(
            "scatPlot", width = 800, height = 700),
          plotlyOutput(
            "scatPlot2", width = 800, height = 700))
      )
    ),
    
    
    # Team Tab ----------------------------------------------------------------
    
    tabItem(tabName = "team",
            fluidRow(
              box(
                title = "Our Team",
                closable = FALSE,
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                h2("DSPG Team Members"),
                fluidRow(
                  box(
                    width = 2,
                    img(
                      src = 'laailah.png',
                      width = "150px",
                      height = "150px"
                    ),
                    p("Laailah Ali, DSPG Young Scholar, Economics")
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'max.png',
                      width = "150px",
                      height = "150px"
                    ),
                    p("Max Ruehle, DSPG Young Scholar, Statistics")
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'jack.png',
                      width = "150px",
                      height = "150px"
                    ),
                    p("Jack Studier, DSPG Young Scholar, Urban Planning")
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'amanda.png',
                      width = "150px",
                      height = "150px"
                    ),
                    p("Amanda Rae, DSPG Graduate Fellow, Sociology")
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'gina.png',
                      width = "150px",
                      height = "150px"
                    ),
                    HTML("<p><a href='https://www.agron.iastate.edu/people/virginia-gina-nichols'>Dr. Gina Nichols</a>, DSPG Advisor, Agronomy")
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'heike.png',
                      width = "150px",
                      height = "150px"
                    ),
                    HTML(
                      "<p><a href='https://www.stat.iastate.edu/people/heike-hofmann'>Dr. Heike Hofmann</a>, Professor of Statistics and Professor in Charge of the Data Science Program</p>"
                    )
                  )
                ),
                h2("Project Sponsors and Advisors"),
                fluidRow(
                  box(
                    width = 2,
                    img(
                      src = 'kimberly.png',
                      width = "150px",
                      height = "150px"
                    ),
                    HTML(
                      "<p><a href='https://www.design.iastate.edu/faculty/zarecor/'>Dr. Kimberly Zarecor</a>, Professor of Architecture, Principal Investiagator</p>"
                    )
                  ),
                  box(
                    width = 2,
                    img(
                      src = 'biswa.png',
                      width = "150px",
                      height = "150px"
                    ),
                    HTML(
                      "<p><a href='https://www.design.iastate.edu/faculty/bdas/'>Dr. Biswa Das</a>, Associate Professor, Community and Regional Planning Director of Graduate Education, Community and Regional Planning, Co-Principal Investigator</p>"
                    )
                  )),
                h2("Acknowledgements"),
                p(
                  "This material is based upon work supported by the National Science Foundation under Grant Number #1736718. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation."
                ),
                img(src = '1024px-NSF_logo.png',
                    width = "5%",
                    align = "right")
              )
            ))
  )
)

# ui ----------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  levy_react <- reactive({
    levy
  })
  
  observe({
    RV$levy <- levy_react()
  })
  
  levy_qol_react <- reactive({
    levy_qol
  })
  
  observe({
    RV$levy_qol <- levy_qol_react()
  })
  
  all_iowa_react <- reactive({
    all_iowa
  })
  
  observe({
    RV$all_iowa <- all_iowa_react()
  })
  
  ds_react <- reactive({
    ds
  })
  
  observe({
    RV$ds <- ds_react()
  })
  
  
  levy_sub <- reactive({
    cities <- input$which_cities
    if ("All" %in% cities)
      cities <- unique(RV$levy$CITY.NAME)
    RV$levy %>% #filter(Tax.Type == "TOTALS") %>%
      filter(CITY.NAME %in% cities)
  })
  
  observe({
    RV$levy_sub <- levy_sub
  })
  
  y <- reactive({
    RV$levy_sub()[, input$which_variable]
  })
  
  observe({
    RV$y <- y
  })
  
  myTest <- reactive({
    cities2 <- input$chooseCities
    RV$ds %>%
      filter(CITY.NAME %in% cities2)
  })
  
  observe({
    RV$myTest <- myTest()
  })
  
  
  # Population Loss in Rural Iowa ---------------------------------------------------------
  
  output$plotly_loss <- renderPlotly({
    foo <- levy %>%
      filter(CITY.SIZE %in% c("RURAL", "RURAL PLUS")) %>%
      filter(Year <= 2019) %>%
      group_by(CITY.SIZE, CITY.NAME) %>% nest() %>%
      summarize(
        m = data %>% purrr::map(
          .f = function(d)
            lm(Population ~ Year, d)
        ),
        population = data %>% purrr::map_dbl(
          .f = function(d)
            d$Population[d$Year == 2006]
        )
      )
    
    foo <- foo %>% mutate(rate = m %>% purrr::map_dbl(
      .f = function(mod)
        coef(mod)[2]
    ))
    
    
    gg <-
      foo %>% ggplot(aes(
        x = CITY.SIZE,
        y = rate / population * 100,
        label = CITY.NAME
      )) + geom_boxplot() + coord_flip() +
      ylab("Percent Population Change") + xlab("") + theme_stata()
    
    ggplotly(gg)
  })
  
  # Sidebar City Limit Change -----------------------------------------------
  
  observe({
    updateSliderInput(session, "range2", value = c(input$range1[2], 2500),
                      min = input$range1[2], max = 7500, step = 100)})
  
  observe({
    updateSliderInput(session, "range3", value = c(input$range2[2], 5000),
                      min = input$range2[2], max = 20000, step = 100)})
  
  observe({
    updateSliderInput(session, "range4", value = c(input$range3[2], 25000),
                      min = input$range3[2], max = 25000, step = 100)})
  
  # Reset sliders button ----------------------------------------------
  
  observeEvent(input$actionReset, {
    updateSliderInput(session, "range1", value = c(0, 800),
                      min = 0, max = 2500, step = 50)
    
    updateSliderInput(session, "range2", value = c(800, 2500),
                      min = 800, max = 7500, step = 100)
    
    updateSliderInput(session, "range3", value = c(2500, 5000),
                      min = 2500, max = 20000, step = 100)
    
    updateSliderInput(session, "range4", value = c(5000, 25000),
                      min = 5000, max = 25000, step = 100)
    
  })
  
  # Apply changes button ---------------------------------------------
  
  observeEvent(input$action1, {
    
    ## Remove Fiscal Capacity and Effort in Levy Set as it will be changed with the rest of this code
    RV$levy <- RV$levy %>%
      select(-c(Fiscal.Effort, Fiscal.Capacity))
    
    ## First change Levy ranges
    RV$levy <- RV$levy %>% mutate(CITY.SIZE = ifelse(Population < input$range1[2], 
                                     "RURAL", 
                                     ifelse((Population < input$range2[2] & Population >= input$range2[1]),
                                            "RURAL PLUS", 
                                            ifelse((Population < input$range3[2] & Population >= input$range3[1]), 
                                                   "URBAN CLUSTER",
                                                   "NANO- and MICROPOLITAN"))))
    
    RV$levy$CITY.SIZE <- factor(RV$levy$CITY.SIZE, levels = c("RURAL", "RURAL PLUS", "URBAN CLUSTER", "NANO- and MICROPOLITAN"))
    
    ## Now change All-iowa ranges
    RV$all_iowa <- RV$all_iowa %>% mutate(CITY.SIZE = ifelse(Population < input$range1[2], 
                                                     "RURAL", 
                                                     ifelse((Population < input$range2[2] & Population >= input$range2[1]),
                                                            "RURAL PLUS", 
                                                            ifelse((Population < input$range3[2] & Population >= input$range3[1]), 
                                                                   "URBAN CLUSTER",
                                                                   "NANO- and MICROPOLITAN"))))
    
    RV$all_iowa$CITY.SIZE <- factor(RV$all_iowa$CITY.SIZE, levels = c("RURAL", "RURAL PLUS", "URBAN CLUSTER", "NANO- and MICROPOLITAN"))
    
    ## Now, depending on the SelectInput edit the fiscal metrics accordingly
    
    if (input$fiscalButtons == "shrink") {
      RV$levy <- RV$levy %>% select(-c(Group.Per.Capita.Revenue,
                                       Group.Tax.Valuation,
                                       Group.Per.Capita.Valuation,
                                       Group.Levy.Rate))
      
      levy_groups_PCRev <- RV$levy %>%
        filter(!is.na(Levy.Rate)) %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Population = sum(Population),
                  Group.Per.Capita.Revenue = sum(Per.Capita.Revenue) / Group.Population) %>%
        select(CITY.SIZE, Year, Group.Per.Capita.Revenue)
      
      RV$levy <- left_join(RV$levy, levy_groups_PCRev, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      levy_groups_TRRev <- RV$levy %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Population = sum(Population),
                  Group.Tax.Valuation = sum(Adjusted.Amount),
                  Group.Per.Capita.Valuation = Group.Tax.Valuation / Group.Population) %>%
        select(CITY.SIZE, Year, Group.Tax.Valuation, Group.Per.Capita.Valuation) 
      
      RV$levy <- left_join(RV$levy, levy_groups_TRRev, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      levy_groups_PGLR <- RV$levy %>%
        filter(!is.na(Levy.Rate)) %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Levy.Rate = 1000 * sum(Prop.Tax.Revenue) / sum(Adjusted.Amount))
      
      RV$levy <- left_join(RV$levy, levy_groups_PGLR, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      RV$levy <- RV$levy %>%
        mutate(Fiscal.Capacity = (Per.Capita.Valuation / Group.Per.Capita.Valuation) * 100)
      
      RV$levy <- RV$levy %>%
        mutate(Fiscal.Effort = (Per.Capita.Revenue / (Per.Capita.Valuation * Group.Levy.Rate)) * 100000)
    }
    
    if (input$fiscalButtons == "all") {
      RV$all_iowa <- RV$all_iowa %>% select(CITY.NAME, 
                                            Year, 
                                            Population, 
                                            Levy.Rate,
                                            Adjusted.Amount, 
                                            CITY.SIZE, 
                                            Prop.Tax.Revenue, 
                                            Per.Capita.Revenue, 
                                            Per.Capita.Valuation)
      
      levy_groups_PCRev <- RV$all_iowa %>%
        filter(!is.na(Levy.Rate)) %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Population = sum(Population),
                  Group.Per.Capita.Revenue = sum(Per.Capita.Revenue) / Group.Population) %>%
        select(CITY.SIZE, Year, Group.Per.Capita.Revenue)
      
      RV$all_iowa <- left_join(RV$all_iowa, levy_groups_PCRev, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      levy_groups_TRRev <- RV$all_iowa %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Population = sum(Population),
                  Group.Tax.Valuation = sum(Adjusted.Amount),
                  Group.Per.Capita.Valuation = Group.Tax.Valuation / Group.Population) %>%
        select(CITY.SIZE, Year, Group.Tax.Valuation, Group.Per.Capita.Valuation) 
      
      RV$all_iowa <- left_join(RV$all_iowa, levy_groups_TRRev, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      levy_groups_PGLR <- RV$all_iowa %>%
        filter(!is.na(Levy.Rate)) %>%
        group_by(CITY.SIZE, Year) %>%
        summarise(Group.Levy.Rate = 1000 * sum(Prop.Tax.Revenue) / sum(Adjusted.Amount))
      
      RV$all_iowa <- left_join(RV$all_iowa, levy_groups_PGLR, by = c("Year" = "Year", "CITY.SIZE" = "CITY.SIZE"))
      
      RV$all_iowa <- RV$all_iowa %>%
        mutate(Fiscal.Capacity = (Per.Capita.Valuation / Group.Per.Capita.Valuation) * 100)
      
      RV$all_iowa <- RV$all_iowa %>%
        mutate(Fiscal.Effort = (Per.Capita.Revenue / (Per.Capita.Valuation * Group.Levy.Rate)) * 100000)
      
      ## merge levy with all_iowa for only fiscal cap and fiscal eff
      RV$levy <- RV$levy %>%
        left_join(RV$all_iowa %>% select(CITY.NAME, Year, Fiscal.Capacity, Fiscal.Effort), 
                  by = c("CITY.NAME" = "CITY.NAME", "Year" = "Year"))
    }
    
    ## create the new levy_qol set from the reactive levy set
    RV$levy_qol <- RV$levy %>%
      select(Year, CITY.NAME, longitude, latitude, all_of(fills), all_of(metrics)) %>%
      filter(Year >= 2010,
             Year <= 2016)
    
    ## join new new data onto the ds set from the reactive levy set
    RV$ds <- RV$ds %>%
      select(-c(Fiscal.Capacity,
                Fiscal.Effort,
                CITY.SIZE)) %>%
      left_join(RV$levy %>% select(CITY.NAME, Year, CITY.SIZE, Fiscal.Capacity, Fiscal.Effort), 
                by = c("CITY.NAME" = "CITY.NAME",
                       "Year" = "Year"))
  }, ignoreNULL = TRUE)
  
  
  
  
  
  
  # Dotplot of Iowa ---------------------------------------------------------
  
  output$mapPlot <- renderPlotly({
    tryCatch(
      expr = {
        ggplotly(
          map_data("county") %>% filter(region == "iowa") %>%
            ggplot(aes(x = long, y = lat)) +
            geom_path(aes(group = group), colour = "grey30") +
            geom_point(
              data = (RV$levy_qol %>% filter(Year == input$Year2) %>% distinct()),
              aes(
                x = longitude,
                y = latitude,
                size = !!input$metric,
                color = !!input$fills,
                text = paste("City:", CITY.NAME, "\n",!!input$fills)
              )
            ) +
            theme_map() +
            scale_color_viridis_d(begin = 0, end = 0.8) +
            theme(legend.text = element_text(size = 12),
                  legend.position = "top"),
          tooltip = c("text", "size")
        ) %>%
          layout(legend = list(
            x = 0.95 ,
            y = 0.1,
            face = "bold",
            size = 15,
            itemsizing = "constant"
            )
          )
      },
      error = function(e) {
        ggplotly(
          map_data("county") %>% filter(region == "iowa") %>%
            ggplot(aes(x = long, y = lat)) +
            geom_path(aes(group = group), colour = "black") +
            geom_point(
              data = (RV$levy_qol %>% filter(Year == input$Year2) %>% distinct()),
              aes(
                x = longitude,
                y = latitude,
                size = !!input$metric,
                color = !!input$fills,
                text = paste("City:", CITY.NAME, "\n",!!input$fills)
              )
            ) +
            theme_map() +
            scale_color_viridis_c(begin = 0, end = 0.8) +
            theme(
              legend.title = element_text(size = 12, face = "bold"),
              legend.text = element_text(size = 10, face = "bold")
            ),
          tooltip = c("text", "size")
        )
      }
    )
  })
  
  # Levy Plot ---------------------------------------------
  
  output$plotly <- renderPlotly({
    RV$levy_sub() %>%
      ggplot(aes(x = Year, y = RV$y())) +
      geom_line(aes(group = CITY.NAME)) +
      facet_grid(CITY.SIZE ~ ., scales = "free_y") +
      ylab(input$which_variable) +
      theme_stata()
    print(plotly::ggplotly())
  })
  
  # City Comparison  ---------------------------------------------------------
  
  output$ComparePlot <- renderPlot({
    RV$levy %>%
      filter(Year == input$Year,
             CITY.NAME == input$City1 |
               CITY.NAME == input$City2) %>%
      select(CITY.NAME, Year, Fiscal.Effort, Fiscal.Capacity) %>%
      pivot_longer(3:4, names_to = "Type", values_to = "Value") %>%
      ggplot(aes(x = CITY.NAME,
                 y = Value,
                 fill = Type)) +
      geom_col(color = "black", position = "dodge") +
      scale_fill_brewer(palette = "Pastel1") +
      xlab("City") +
      ylab("Index Value") +
      theme_stata() +
      geom_text(aes(x = CITY.NAME, 
                    y = Value + 5,
                    label = format(Value,
                                   scientific = FALSE,
                                   digits = 3)),
                position = position_dodge(.9),
                size = 6)
  })
  
  # Drive Time Analysis -------------------------------------
  
  output$LevyMapPlotly <- renderPlotly({
    gg1 <- RV$levy %>%
      mutate(`Commute ≥ 50 mins` = min50 >= 50) %>%
      filter(Year == 2020) %>%
      ggplot(aes(x = longitude, y = latitude)) +
      geom_path(aes(x = long,
                    y = lat,
                    group = subregion),
                data = map_data("county") %>% filter(region == "iowa"),
                colour = "grey70") +
      geom_point(aes(size = Population,
                     colour = MSA),
                 data = all_cities %>% filter(Population >= 50000),
                 alpha = 0.4) +
      geom_point(aes(
        label = CITY.NAME,
        colour = MSA,
        shape = `Commute ≥ 50 mins`
      ),
      size = 3) +
      theme_bw() +
      scale_size(range = c(5, 10), guide = NULL) +
      scale_shape_manual("50 min or more Commute", values = c(19, 1)) +
      scale_colour_brewer("Hub and influence region", palette = "Paired") +
      ggthemes::theme_map() +
      theme(legend.position = "none")
    
    gg2 <- RV$levy %>%
      mutate(`Commute ≥ 50 mins` = hub_min >= 50) %>%
      filter(Year == 2020) %>%
      ggplot(aes(x = longitude, y = latitude)) +
      geom_path(aes(x = long,
                    y = lat,
                    group = subregion),
                data = map_data("county") %>% filter(region == "iowa"),
                colour = "grey70") +
      geom_point(aes(size = Population,
                     colour = microMSA),
                 data = all_cities %>% filter(Population >= 10000),
                 alpha = 0.4) +
      geom_point(aes(
        label = CITY.NAME,
        colour = microMSA,
        shape = `Commute ≥ 50 mins`
      ),
      size = 3) +
      theme_bw() +
      scale_size(range = c(5, 10), guide = NULL) +
      scale_shape_manual("50 min or more Commute", values = c(19, 1)) +
      ggthemes::theme_map() +
      theme(legend.position = "none")
    
    if (input$msa == "Micropolitan Statistical Area (µMSA)")
      print(plotly::ggplotly(gg2))
    else 
      print(plotly::ggplotly(gg1))
  })
  
  # ArcGIS Crop Diversiy Map -----------------------------------------------------
  
  output$arcFrame <- renderUI({
    HTML(
      '
    <style>
      .embed-container {
        position: relative;
        padding-bottom: 80%;
        height: 0;
        max-width: 100%;
      }
    </style>
    <iframe
        width="2000"
        height="1000"
        frameborder="0"
        scrolling="no"
        marginheight="0"
        marginwidth="0"
        title="provPrepTest"
        src="https://www.arcgis.com/apps/mapviewer/index.html?webmap=5c70b9b3088c4b8d885417ddee5bf4ee
">
    </iframe>
'
    )
  })
  
  # Measures of Crop Diversity -------------------------------------------------
  
  #Crop Diversity Lollipop
  crop_sub <- reactive({
    crop_mets %>%
      filter(city_name == input$which_city_cropdiv)
  })
  output$plotly_cropdiv <- renderPlotly({
    ggplotly(
      crop_sub() %>%
        filter(crop_prop > 0) %>%
        arrange(crop_prop) %>%
        mutate(crop = fct_inorder(crop)) %>%
        ggplot(aes(crop, crop_prop)) +
        geom_point(aes(
          size = crop_prop,
          text = paste(round(crop_prop * 100, 1), "%")
        )) +
        geom_segment(aes(
          xend = crop, y = 0, yend = crop_prop
        )) +
        coord_flip() +
        scale_y_continuous(
          labels = scales::label_percent(),
          limits = c(0, 1)
        ) +
        labs(y = "Percentage of Ag Land",
             x = NULL) +
        theme_stata() +
        theme(axis.text.y = element_text(angle = 0)),
      tooltip = c("text")
    )
  })
  
  #Landscape Diversity Map
  observe({
    d <- event_data("plotly_click", source = "plotly_divmet")
    if (!is.null(d)) {
      which_city = as.character(unlist(div_sub()[d$pointNumber + 1, "city_name"]))
      
      updateSelectInput(session, "which_city_cropdiv", selected = which_city)
    }
  })
  
  
  div_sub <- reactive({
    div_mets %>%
      filter(div_metric_nice == input$which_divmet)
  })
  
  output$plotly_divmet <- renderPlotly({
    ggplotly(
      source = "plotly_divmet",
      map_data("county") %>% filter(region == "iowa") %>%
        ggplot(aes(x = long, y = lat)) +
        geom_path(aes(group = group), colour = "grey30") +
        geom_point(
          data = div_sub(),
          size = 6,
          aes(
            x = longitude,
            y = latitude,
            color = value,
            text = paste(
              "City name:",
              city_name,
              "\n",
              "2020 Population:",
              round(population, 0),
              "\n",
              "Land in Ag:",
              round(ag_prop_all * 100, 0),
              "%",
              "\n",
              "Value:",
              round(value, 2)
            )
          )
        ) +
        labs(color = str_wrap(input$which_divmet), 5) +
        theme_map() +
        scale_color_viridis_c(option = "inferno") +
        theme(legend.text = element_text(size = 12)),
      tooltip = c("text", "size")
    ) %>%
      layout(legend = list(
        x = -0.25 ,
        y = 1,
        face = "bold"
      ))
  })
  
  # Quality of Life by Metrics ----------------------------------------------
  output$qolCorrelation <- renderText({
    subds <- RV$ds %>%
      filter(Year == input$Year3)
    correlation <- cor(
      x = subds %>% select(input$fiscal),
      y = subds %>% select(input$qol),
      use = "pairwise"
    )
    
    sprintf(
      "The Pearson-Spearman correlation between the selected variables is %.4f.",
      correlation
    )
  })
  
  
  
  
  output$scatPlot <- renderPlotly({
    myds <- RV$myTest %>% filter(Year == input$Year3)
    
    
    if (nrow(myds) == 0) {
      g <- ggplotly(
        RV$ds %>%
          filter(Year == input$Year3) %>%
          ggplot(
            aes(
              x = !!input$fiscal,
              y = !!input$qol,
              color = CITY.SIZE,
              text = paste("City:", CITY.NAME)
            )
          ) +
          geom_point() +
          scale_color_viridis_d(begin = 0, end = 0.8) +
          theme(legend.text = element_text(size = 12)) +
          theme_stata() +
          geom_smooth(
            aes(group = 1),
            colour = "grey70",
            method = "lm",
            se = FALSE
          ),
        tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x = -0.25,
                             y = 1,
                             face = "bold"))
    }
    else{
      g <- ggplotly(
        RV$ds %>%
          filter(Year == input$Year3) %>%
          ggplot(
            aes(
              x = !!input$fiscal,
              y = !!input$qol,
              color = CITY.SIZE,
              text = paste("City:", CITY.NAME)
            )
          ) +
          geom_point() +
          geom_point(data = myds, aes(x = !!input$fiscal, y = !!input$qol, size = 5), show.legend = F) +
          scale_color_viridis_d(begin = 0, end = 0.8) +
          theme(legend.text = element_text(size = 12)) +
          theme_stata() +
          scale_alpha(guide = 'none') +
          geom_smooth(
            aes(group = 1),
            colour = "grey70",
            method = "lm",
            se = FALSE
          ),
        tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x = -0.25,
                             y = 1,
                             face = "bold"))
    }
    
    g
  })
}

shinyApp(ui = ui, server = server)
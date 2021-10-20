library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(ggiraph)
library(cowplot)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(DT)
library(stats)
library(ggthemes)
library(magrittr)

ui <- shinydashboard::dashboardPage(skin="red",
                    
  shinydashboard::dashboardHeader(title = 'NBA Draft Dashboard'),
  
  shinydashboard::dashboardSidebar(width = "260",
                   sliderInput("year", label="Draft Year(s):", min=1997, value=c(1997,2015), max=2015, sep="", width="260"),
                   selectInput("filter", label="Display up to which pick?", filters, selected=30, width="260"),
                   selectizeInput("teams", label="Team(s):", 
                                  nba_display$team_abbreviation %>% unique() %>% str_sort(), 
                                  selected = "ATL", multiple = TRUE),
                   hr(style = "border-top: 1px solid #f4f0ec"),
                   actionButton("apply", label="Apply", icon("filter", lib = "glyphicon"))),
  
  shinydashboard::dashboardBody(
    useShinyjs(),
              tabsetPanel(
                          tabPanel("Ages", hr(), 
                                   column(8, girafeOutput("Age", width="100%")), 
                                   fluidRow(offset = 5, 
                                            valueBoxOutput("ageBox"), 
                                            infoBoxOutput("meanAge"), 
                                            infoBoxOutput("medianAge"))), 
                         
                          
                           tabPanel("Height and Weight", hr(),
                                   column(8, girafeOutput("cosie", width="100%")),
                                   fluidRow(offset = 5,
                                            valueBoxOutput("hwBox"),
                                            infoBoxOutput("tallest"),
                                            infoBoxOutput("shortest"),
                                            infoBoxOutput("heaviest"),
                                            infoBoxOutput("lightest")),
                                   fluidRow(column(8, box(DTOutput("cosieTable"), 
                                                          collapsed = TRUE, 
                                                          collapsible = TRUE, 
                                                          width = NULL, 
                                                          status = "info",
                                                          solidHeader = TRUE,
                                                          title = "Selected contents datatable")))),
                          
                          
                          tabPanel("Countries",
                                   hr(),
                                   fluidRow(valueBoxOutput("mapBox", width=3), 
                                            infoBoxOutput("USdrafts", width=3), 
                                            box(helpText(h4(p("Note: Please click on apply filters on the sidebar",
                                                              "to see changes. This map uses eventReactive to",
                                                              "improve performance."))),
                                                status = "info",
                                                solidHeader = TRUE)),
                                   column(10, girafeOutput("Map"))),
                          
                          
                          tabPanel("Colleges", 
                                   hr(),
                                   column(6, girafeOutput("College"),
                                          box(sliderInput("collegefilter",
                                                          label = "Amount of colleges",
                                                          min=1, value=18, max=36),
                                              status = "primary",
                                              solidHeader = TRUE,
                                              width = NULL)),
                                   fluidRow(column(6, box(DTOutput("collegeTable"),
                                                          collapsed = TRUE,
                                                          collapsible = TRUE,
                                                          width = NULL,
                                                          status = "info",
                                                          solidHeader = TRUE,
                                                          title = "Selected contents datatable")))),
                          
                         
                          navbarMenu("More", icon = icon("info-sign", lib="glyphicon"),
                                     tabPanel("About",
                                              h1(p("About")),
                                              hr(),
                                              h5(p("This dashboard is meant to be a fun interactive experience to learn some surface level facts about the NBA drafts. 
                                                   The original dataset was found here on ", a("Kaggle. ", href = "https://www.kaggle.com/justinas/nba-players-data"),
                                                   br(),
                                                   "I can be reached at bchen7899@gmail.com if you have any questions. Thank you!"))),
                                     tabPanel("Download filtered csv",
                                              hr(),
                                              downloadButton("allDataDownload"),
                                              hr(style="border-top: 1px solid #000000"),
                                              DTOutput("allData"))
                                    )
                                  
                         )
                )
  
)

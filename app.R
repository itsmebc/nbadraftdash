library(shiny)
library(shinydashboard)
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
library(shinyjs)

source('ui.R', local=TRUE)
source('server.R', local=TRUE)

shinyApp(
  ui=ui,
  server=server
)

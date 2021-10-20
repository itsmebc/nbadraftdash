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

#import csv file
nba = read.csv(file="all_seasons.csv")

world = ne_countries(scale = "small", returnclass = "sf")['name_long']
world = setNames(world, c("country", "geometry"))

nba_display <- nba %>% 
  transform(draft_year = as.numeric(draft_year),
            draft_number = as.integer(draft_number),
            draft_round = as.numeric(draft_round),
            age = as.numeric(age)) %>%
  distinct(player_name, .keep_all = TRUE) %>%
  mutate(player_weight = round(player_weight, 2),
         player_height = round(player_height, 2))

nba_display$college = gsub("'","", as.character(nba_display$college))

filters = c(1:45) %>% as.numeric()

server <- (function(input, output, session) {
  
  textSizeTheme = function(textX, textY, titleX, titleY){
                        theme(axis.text.x = element_text(size=textX),
                        axis.text.y = element_text(size=textY),
                        axis.title.x = element_text(size=titleX),
                        axis.title.y = element_text(size=titleY))
  }
  
  filtering = reactive({
    nba_display %>%
      filter(draft_year %in% c(input$year[1]:input$year[2]),
             draft_number <= as.numeric(input$filter),
             team_abbreviation %in% c(input$teams))
  })
  
  filteringCountries = reactive({
    nba_countries %>%
      filter(draft_year %in% c(input$year[1]:input$year[2]),
             draft_number <= as.numeric(input$filter),
             team_abbreviation %in% c(input$teams))
  })
  
  
  
# age -----------------------------------------------------------------------------------------------------------------------  
  
  output$Age <- renderGirafe({
    age = ggplot(data.frame(filtering() %>%
                              count(., vars = factor(age))), aes(x = vars, y = n, tooltip = n, data_id = vars)) +
      geom_col_interactive(aes(tooltip=paste0("Count: ", n), data_id = heightweight()$age)) + 
      labs(x = "Age", y = "Count") + 
      theme_fivethirtyeight() +
      textSizeTheme(16,16,24,24)
    
    girafe(ggobj = age, width_svg=16, height_svg = 12)
  })
  
  output$ageBox <- renderValueBox({
    valueBox(filtering() %>%
             filter(age %in% c(input$Age_selected)) %>%
             count(), 
             "Total players selected", color = "yellow")
  })
  
  output$medianAge <- renderInfoBox({
    infoBox("Median age of selected", 
            filtering() %>%
              filter(age %in% c(input$Age_selected)) %>%
              summarize(medianAge = median(age)) %>%
              round(2),
            color = "red")
  })
  
  output$meanAge <- renderInfoBox({
    infoBox("Average age of selected", 
            filtering() %>%
              filter(age %in% c(input$Age_selected)) %>%
              summarize(meanAge = mean(age)) %>%
              round(2),
            color = "blue")
  })
  
  
  
  
# height and weight -------------------------------------------------------------------------------------------------------------------  
  
  heightweight = reactive({
    filtering() %>%
      group_by(age) %>%
      summarize(
        height = mean(player_height),
        weight = mean(player_weight)) %>%
      as_tibble()
  })
  
  # wbox = reactive({ggobj = w})
  # hbox = reactive({ggobj = h})
  
  w <- reactive({
    ggplot(filtering(), aes(x=factor(age), y=player_weight)) +
      geom_boxplot_interactive(tooltip = paste0("Mean: ", filtering() %>%
                                 aggregate(player_weight ~ age, ., mean) %>%
                                 tibble() %>%
                                 .$player_weight %>% round(digits=2)), 
                               data_id = heightweight()$age) +
      labs(x = "Age", y = "Weight") +
      theme_fivethirtyeight() +
      textSizeTheme(8,8,12,12) 
  })
  
  h <- reactive({
    ggplot(filtering(), aes(x=factor(age), y=player_height)) +
      stat_boxplot() +
      geom_boxplot_interactive(tooltip = paste0("Mean: ", filtering() %>%
                                 aggregate(player_height ~ age, ., mean) %>%
                                 tibble() %>%
                                 .$player_height %>% round(digits=2)), 
                               data_id = heightweight()$age) +
      labs(x = "Age", y = "Height") + 
      theme_fivethirtyeight() +
      textSizeTheme(8,8,12,12)
  })
  
  output$cosie = renderGirafe({
    girafe(ggobj = plot_grid(h(),w()), width_svg = 6, height_svg = 4) %>%
      girafe_options(opts_hover(css="fill:orange;"))
  })
  
  cosieTableData = reactive(
    filtering() %>%
      filter(age %in% c(input$cosie_selected))
  )
  
  output$cosieTable = renderDT(
   cosieTableData() %>%
     select(player_name, age, player_weight, player_height) %>%
     data_frame() %>%
     arrange(age) %>%
     data.table::setDT()
  )
  
  output$tallest <- renderInfoBox({
    infoBox("Tallest player selected", 
            cosieTableData() %>% 
              .[which.max(.$player_height),] %>% 
               tidyr::unite(player, player_name, player_height, sep=", ") %>% 
               select(player), 
             color = "blue")})
  
  output$shortest <- renderInfoBox({
    infoBox("Shortest player selected",
            cosieTableData() %>% .[which.min(.$player_height),] %>% 
              tidyr::unite(player, player_name, player_height, sep=", ") %>% 
              select(player),
            color = "blue")})
  
  output$heaviest <- renderInfoBox({
    infoBox("Heaviest player selected",
            cosieTableData() %>% .[which.max(.$player_weight),] %>% 
              tidyr::unite(player, player_name, player_weight, sep=", ") %>% 
              select(player),
            color = "red")})
  
  output$lightest <- renderInfoBox({
    infoBox("Lightest player selected",
            cosieTableData() %>% .[which.min(.$player_weight),] %>% 
              tidyr::unite(player, player_name, player_weight, sep=", ") %>% 
              select(player),
            color = "red")})
  
  output$hwBox <- renderValueBox({
    valueBox(cosieTableData() %>%
               count(), 
             "Total players selected", color = "yellow")
  })
  
  
  
# countries --------------------------------------------------------------------------------------------------------------  
  
  oldNames = c("Bosnia", "Bosnia & Herzegovina", "Cabo Verde", "Congo", "England", "Great Britain", "Russia", 
               "Scotland", "Serbia and Montenegro", "South Korea", "St. Vincent & Grenadines", "Sudan (UK)", 
               "U.S. Virgin Islands", "US Virgin Islands", "USA", "USSR", "Yugoslavia")
  newNames = c("Bosnia and Herzegovina", "Bosnia and Herzegovina", "Cape Verde", "Democratic Republic of the Congo", 
               "United Kingdom", "United Kingdom", "Russian Federation", "United Kingdom", "Serbia", 
               "Republic of Korea", "Saint Vincent and the Grenadines", "Sudan", 
               "United States Virgin Islands", "United States Virgin Islands", "United States", 
               "Russian Federation", "Bosnia and Herzegovina")
  nba_countries = nba_display
  nba_countries$country = plyr::mapvalues(nba_countries$country, from=oldNames, to=newNames)
  
  joinYears = eventReactive(input$apply, {
    filteringCountries() %>%
      count(country)
  })
  
  joinedYears = reactive({
    world$n = joinYears()$n[match(world$country, joinYears()$country)]
    world
  })
  
  output$Map = renderGirafe({
    map = ggplot(data = joinedYears()) +
      geom_sf_interactive(aes(fill = n, tooltip = paste0(country, ": ", n))) +
      scale_fill_gradient(low="#fe7f9c", high="#300000", trans = "log10") +
      theme_fivethirtyeight()
      
    
    girafe(ggobj = map, width = 1000, height = 500)
  })
  
  output$mapBox <- renderValueBox({
    valueBox(filteringCountries() %>% summarize(n_distinct(country)), 
             "Countries drafted", color = "yellow")
  })
  
  output$USdrafts <- renderInfoBox({
    infoBox("From USA",
            filteringCountries() %>% filter(country == "United States")%>% count(), color = "red")
  })
  
  output$countryLine <- renderPlot({
    countryline = ggplot(data = 
                         filteringCountries() %>% 
                         select(draft_year, country) %>%
                         group_by(draft_year) %>%
                         count(country), aes(x=draft_year, y=country, group=country)) +
      geom_line()
  })
  
  
  
# colleges -----------------------------------------------------------------------
  output$College <- renderGirafe({
    collegeBar <- ggplot(data = filtering() %>%
                           select(draft_year, college) %>%
                           count(college) %>%
                           arrange(desc(n)) %>%
                           mutate(college = reorder(college, n)) %>%
                           slice(1:input$collegefilter), aes(x=college, y=n))+
      geom_col_interactive(aes(fill=college, tooltip = n, data_id = college)) +
      theme_fivethirtyeight() +
      theme(legend.position = "NONE") +
      labs(y = "College",
           title = "How many draftees from this college?",
           subtitle = sprintf("Top %s college(s), %s to %s", input$collegefilter, input$year[1], input$year[2])) +
      coord_flip()
    
    girafe(ggobj = collegeBar)
  })
  
  output$collegeTable <- renderDT({
    filtering() %>%
      select(draft_year, college, draft_number, player_name) %>%
      filter(college %in% input$College_selected)
  })

  
  
# more --------------------------------------------------------------------------------------------------
  output$allData <- renderDT({
    datatable(filtering(), extensions = 'Responsive', width = "100%")
  })
  
  output$allDataDownload = downloadHandler('nbaDraftFiltered.csv', content = function(x) {
    write.csv(filtering(), x)
  })
  
})
  
  
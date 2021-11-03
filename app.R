#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(RColorBrewer)
library(maps)
library(tigris)
library(leaflet)
library(stringr)
options(tigris_use_cache = TRUE)

discr_vars <- c('Insecticide', 'Fungicide', 'Fertilizer', 'Herbicide', 'Other')
fill_vars <- c('hormone_disruptor','carcinogen','bee_toxins','neurotoxins','developmental_or_reproductive_toxins')
dat <- read.csv("strawb.csv", header = TRUE)
state_vars <- c('California','Florida', 'Oregon', 'Washington')
dat$state <- str_to_title(dat$state)
dat$type <- str_to_title(dat$type)


ui <- dashboardPage(
  dashboardHeader(title = "Strawberry dashboard"),
  dashboardSidebar( sidebarMenu(
    menuItem("Toxicity Level", tabName = "toxicity", icon = icon("dashboard")),
    menuItem("Maps", tabName = "maps", icon = icon("th")),
    menuItem("Trends in Appliactions", tabName = "trends", icon = icon("calendar")) ) ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "toxicity",
              fluidRow(
                 h2('The use frequncy of different toxicity level of chemical',align = "center"),
                 box(plotlyOutput("barplot", width = "100%", height = "600px"),width = 9),
                
                    box( title = "Inputs", width = 3,status="primary",
                  # Boxes need to be put in a row (or column)
                  selectInput(inputId = "x",
                              label = "Type of chemicals:",
                              choices = discr_vars ,
                              multiple =TRUE,
                              selected = "Insecticide"),
                  # Select variable for fill 
                  selectInput(inputId = "fill",
                              label = "Toxicity type:",
                              choices = fill_vars ,
                              selected = fill_vars[1])),
                
                  box(title = "Facet?", width = 3, status="primary",
                  selectInput("Facet", label = h3("Facet?"), 
                              choices = list("Year" = "Year", "State" = 'state', 'No' = 'FALSE'), 
                              selected = 'No'))
              )

              ),
      tabItem(tabName = "maps",
              h2("Map of strawberries with chemical",align = "center"),
              box(title = 'Inputs', width = 4,status="primary",
              selectInput(inputId = "States",
                          label = "States:",
                          choices = state_vars ,
                          multiple =TRUE,
                          selected = c("California",'Florida')),
              selectInput(inputId = "Years",
                          label = "Year:",
                          multiple = TRUE,
                          choices = c(2016,2018,2019) ,
                          selected = c(2016,2018,2019))),
              box(title = 'Choose types of chemical',width = 4,status="primary",
              selectInput(inputId = "Types",
                          label = "Chemical type:",
                          choices = discr_vars ,
                          selected = 'Insecticide')),
              box(title = 'Choose types of measurements',width = 4,status="primary",
                  selectInput(inputId = "measurements",
                              label = "Measurements:",
                              choices = list("Measure in LB" = " APPLICATIONS ,  MEASURED IN LB", 
                                             "Meaure in LB/Acer/Application" = " APPLICATIONS ,  MEASURED IN LB / ACRE / APPLICATION", 
                                             "Meaure in LB/Acer/Year" = " APPLICATIONS ,  MEASURED IN LB / ACRE / YEAR",
                                             "Mearue in number" = " APPLICATIONS ,  MEASURED IN NUMBER") ,
                              selected = 'Measure in LB')),
              leafletOutput("my_leaf")
      ),
      tabItem(tabName = "trends",
              h2("Trend of applications of chemical, Measured in LB/Acre/Application", align = "center"),
              box(plotlyOutput("linechart", width = "100%", height = "400px"),width = 8),
              box(title = 'Inputs', width = 4,status="primary",
                  selectInput(inputId = "Type",
                              label = "Type of chemicals:",
                              choices = discr_vars,
                              selected = "Insecticide"),
                  selectInput(inputId = "State",
                              label = "States:",
                              choices = state_vars ,
                              multiple =TRUE,
                              selected = "California"))
              
      
    ))
    

))



server <- function(input, output) {
  

  output$barplot <- renderPlotly({
    
    dat_filter <- reactive({
      dat %>%
        filter(type %in% input$x & measurements %in% ' APPLICATIONS ,  MEASURED IN LB')
    })
    fill <- input$fill
    basic_p <- ggplot(dat_filter()) +
      geom_bar(aes(x =  type , fill = !!rlang::sym(input$fill) ), position = "fill") + 
      labs(fill = input$fill) + 
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 45, size = 8)) 
    if(input$Facet == 'FALSE'){
      basic_p
    }else{ 
      if(input$Facet == 'Year'){
      basic_p + facet_grid(. ~ Year) 
      }else{
        basic_p + facet_grid(. ~ state) 
    }}
      
})
  
  
  output$my_leaf <- renderLeaflet({
    newDf <- reactive({
      dat %>% filter(measurements %in% input$measurements & state %in% input$States & type %in% input$Types & Year %in% input$Years  ) %>%
        group_by(state) %>%
        summarise(sum = round(mean(value),2))
    })
    
    mapStates <- map("state", fill = TRUE, plot = FALSE)
    statesUsed <- states(cb=T)
    states_merged <- reactive({
      geo_join(statesUsed, newDf(), "NAME", "state", how = 'inner')
    })
    
    pal <- reactive({colorNumeric(
      palette = "Blues",
      domain = states_merged()$sum)})
    
    leaflet() %>% addTiles() %>%
    setView(lng = -99, lat = 40, zoom = 4) %>%
      addPolygons(data = states_merged(),
                  fillColor = ~ pal()(sum),
                  fillOpacity = 0.5,
                   weight = 0.2,
                  smoothFactor = 0.2,
                   label = paste('Average application:',states_merged()$sum, input$measurements)) %>%
       addLegend(position = "bottomleft",
                pal = pal(), values = states_merged()$sum,
                title = paste('average application of',input$Types),
                opacity = 1) 
    })
  
  output$linechart <- renderPlotly({
    
    dat_sub <- reactive({
      dat %>%
        filter(type %in% input$Type & state %in% input$State & measurements == ' APPLICATIONS ,  MEASURED IN LB / ACRE / APPLICATION' & value > 0) %>%
        group_by(Year, state) %>%
        summarise(mean = mean(value))
    })
    
    validate(
      need(nrow(dat_sub()) > 0, "Data insufficient for plot!")
    )
    
     ggplot(dat_sub(), aes(x =  Year, y = mean, color = state )) +
      geom_point() +
      geom_line() +
      theme(axis.text.x = element_text(angle = 45, size = 8))
  
    
  })
  
}

shinyApp(ui = ui, server = server)

###############################################.
## ScotPHO - Life expectancy - Council Area ----
###############################################.

# Code to create shiny chart of trends in life expectancy and healthy life expectancy by Council Area 
# This is published in the following section of the ScotPHO website: 
# Population Dynamics > Deaths and life expectancy > Data > Council Area

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny) #shiny apps

# Data file
ca_trend <- readRDS("data/le_hle_ca.rds")

# Use for selection of areas
council_list <- sort(unique(ca_trend$council))

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
ui <- fluidPage(style="width: 650px; height: 500px; ",
                div(style= "width:100%",
                    h4("Chart 1. Life expectancy and healthy life expectancy at birth by Council Area"),
                    div(style = "width: 50%; float: left;",
                        selectInput("measure", label = "Select a measure type",
                                    choices = c("Life expectancy", "Healthy life expectancy"), 
                                    selected = "Life expectancy"))),
                
                div(style = "width: 50%; float: left;",
                    selectInput("council", label = "Select Council Area", 
                                choices = council_list,
                                selected = "Scotland")),
                
                div(style= "width:100%; float: left;", #Main panel
                    plotlyOutput("chart", width = "100%", height = "350px"),
                    p("Note: y-axis does not start at zero"),
                    p("2020-2022 Life expectancy estimates are provisional"),
                    p("Publication of 2020-2022 Healthy life expectancy delayed until 2024"),
                    p(div(style = "width: 25%; float: left;", #Footer
                          HTML("Source: <a href='https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy' target='_blank'>NRS</a>")),
                      div(style = "width: 25%; float: left;",
                          downloadLink('download_data', 'Download data')))
                )
) # fluidPage

############################.
## Server ----
############################.
server <- function(input, output) {
  
  output$chart <- renderPlotly({
    
    #Data for Council Area line
    data_ca <- ca_trend %>% filter(council == input$council & measure == input$measure)
     
    
    # Information to be displayed in tooltip
    tooltip <- c(paste0(input$council, " - ", data_ca$sex, "<br>",
                        "Time period (3 year average): ", data_ca$year, "<br>",
                        input$measure, " (years): ", data_ca$value, "<br>"))
    
    # y-axis title
    yaxistitle <- paste0(input$measure, " (years)")
    
    # Define line colours
    pal <- c('#9B4393', '#1E7F84')
    
    # Define number of lines on chart
    num <- length(unique(data_ca$sex))
    
    # set number of ticks depending on measure selected
    if (input$measure == "Life expectancy") 
      
    {tick_freq <- 2}
    
    else {tick_freq <- 1}
    
      
    # Buttons to remove from plot
    bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                         'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                         'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')
    
    
    plot <- plot_ly(data = data_ca, x=~year, y = ~value, 
                    color= ~sex, colors = pal[1:num], 
                    type = "scatter", mode = 'lines+markers', 
                    symbol= ~sex, symbols = list('circle','square'), marker = list(size= 8),
                    width = 650, height = 350,
                    text=tooltip, hoverinfo="text")  %>%  

      
      # Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = yaxistitle, 
                          #rangemode="tozero", 
                          fixedrange=TRUE), 
             xaxis = list(
               title = list(text = "3 year average", standoff=20),
               dtick = tick_freq,
               fixedrange=TRUE#, 
               #tickangle = 0
             ),
             font = list(family = 'Arial, sans-serif'), #font
             margin = list(pad = 4, t = 50), #margin-paddings
             hovermode = 'false',  # to get hover compare mode as default
             legend = list(orientation = "h", x=0, y=1.2)) %>% 
      config(displayModeBar= T, displaylogo = F, editable =F, modeBarButtonsToRemove = bttn_remove) 
    # taking out plotly logo and collaborate button
    
  }) 
  
  
  # Allow user to download data
  output$download_data <- downloadHandler(
    filename =  'le_and_hle_ca_data.csv', 
    content = function(file) {
      write.csv(ca_trend, file, row.names=FALSE) })
  
} # end of server

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END
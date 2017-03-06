require(shiny)
require(dplyr)
require(ggplot2)
require(maps)
require(ggmap)

data("state.map")

# Read in the data
df <- read.csv(file="data/smallaca.csv", header = FALSE, skip = 1, sep = ",", strip.white = TRUE)

# add region column and convert state names to lowercase
df$region  = tolower(df$V1)

# get state map data and merge with insurance data
states = map_data('state')
mapData = merge(df, states, by = 'region') 


ui <- fluidPage(
  
  # Application title
  titlePanel("ACA Impact across United States"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "columns",
                  label = "Explore:",
                  choices = c("Uninsured Rate 2010", "Uninsured Rate 2015", "Uninsured Rate Change (2010-2015)"),
                  selected= "Uninsured Rate 2010")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rolePlot")
    )
  )
)

server <- function(input, output) {
  
  output$rolePlot <- renderPlot({
    
    if(input$columns == "Uninsured Rate 2010") {
      column = mapData$V2
      legendTitle = "Uninsured Rate"
      gtitle="Insurance Coverage across States 2010"
    } else if (input$columns == "Uninsured Rate 2015") {
      column = mapData$V3
      legendTitle = "Uninsured Rate"
      gtitle="Insurance Coverage across States 2015"
    } else {
      column = mapData$V4
      legendTitle = "Uninsured Rate (decrease)"
      gtitle="Insurance Coverage Change across States 2010-2015"
    }
    
    ggplot(mapData, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = cut_number(column, 8))) +
      geom_path(colour = 'white') + labs(title = gtitle) +
      scale_fill_brewer(legendTitle) + coord_map()
  })
}

shinyApp(ui = ui, server = server)

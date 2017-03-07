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
  h2("Intro"),
  p("The purpose of our research project is to help people better understand the impact of the Affordable Care 
Act (Obamacare) on health insurance for United States citizens and analyze how future health law policy can change
health coverage for many. Since Obamacare was signed into law in 2010, millions of people have benefitted that 
wouldn't otherwise be able to. Many of these people were unemployed or had low paying jobs. Some couldn't afford 
health insurance due to pre-existing health conditions. Now, more than 16 million Americans have obtained health 
insurance coverage  which is more than ever before. Given the wider range of health care options, more people can 
afford healthcare in the United States. With this said, we want to analyze data that allows us to seek insight into 
how the changes of health law (existing and future) impacts the lives of most Americans."),
  br(),
  p("This map below introduces the uninsured rates of change by state before and after the ACA was implemented. 
    As you can see, the states with a lighter blue shade have a lower rate of change for the amount of people uninsured 
    between 2010 and 2015. The darker blue states have the highest rates of change for uninsured people. It seems that 
    the amount of people uninsured living in the west coast has decreased the most in the country with Oregon, California 
    and Nevada with substantial differences of almost a 10% decrease. The midwestern states including North Dakota, 
    South Dakota, Wyoming and Nebrasksa have had the smallest uninsured rate change at around 2%. With 8 different rate 
    change levels, it is very intuitive to understand the rates of change among each state. To say the least, I think this 
    visualization proves to help people better understand the impact of the ACA in a geographical sense. This heat map 
    displaying rate of change is an effective means of doing so."),
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
      gtitle="Insurance Coverage across United States in 2010"
    } else if (input$columns == "Uninsured Rate 2015") {
      column = mapData$V3
      legendTitle = "Uninsured Rate"
      gtitle="Insurance Coverage across United States in 2015"
    } else {
      column = mapData$V4
      legendTitle = "Uninsured Rate (decrease)"
      gtitle="Insurance Coverage Change across United States from 2010-2015"
    }
    
    ggplot(mapData, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = cut_number(column, 8))) +
      geom_path(colour = 'white') + labs(title = gtitle) +
      scale_fill_brewer(legendTitle) + coord_map()
  })
}

shinyApp(ui = ui, server = server)

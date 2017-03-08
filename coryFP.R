require(shiny)
require(dplyr)
require(ggplot2)
require(maps)
require(ggmap)

data("state.map")

# Read in the data
df <- read.csv(file="data/datACA.csv", header = FALSE, skip = 1, sep = ",", strip.white = TRUE)

# add region column and convert state names to lowercase
df$region  = tolower(df$V1)

# get state map data and merge with insurance data
states = map_data('state')
mapData = merge(df, states, by = 'region') 


ui <- fluidPage(
  
  # Application title
  titlePanel("ACA Impact across United States"),
  h2("Intro"),
  p("The purpose of this visualization is to share different insight using a compilation of State Data on 
    the Affordable Care Act deriving from ASPE  and the Department of Health & Human Services. With this dataset, there 
    are valuable answers to questions that many people are unaware of. There is alot of untapped knowledge to be gained 
    and this interactive tool helps explore those potentials."),
  br(),
  p("Thes map-based visualization below shows uninsured rates for 2010, 2015 and the uninsured rate change between the two 
    periods. From viewing the rate in 2010, Nevada and Texas have the highest uninsured rates in the US at around 20%. 
    This means that roughly 20% of the people living in Nevada and Texas are without insurance. The Midwestern states, 
    specifically Wisconsin and Minneosota, have the lowest rate of people without insurance in the United States. In 2015, 
    the uninsured rate for each and every state lowered. It may be difficult to tell because the scale changes between time 
    periods. Oklahoma, Texas and Georgia have the highest uninsured rates in 2015 and have the same color shade as the highest
    rates in 2010 so you really need to pay attention to the scale. If you switch between the two time periods, you can
    estimate manually but its too time consuming and isnt ideal for conducting data analysis."), 
  br(),
  p("So to really understand the difference between the two periods, the third filter to show rate of change is essential to
    understanding the implications of the ACA. As you can see, the states with a lighter blue shade have a lower rate of change 
    for the amount of people uninsured between 2010 and 2015. The darker blue states have the highest rates of change for uninsured 
    people. It seems that the amount of people uninsured living in the west coast has decreased the most in the country with 
    Oregon, California and Nevada with substantial differences of almost a 10% decrease. The midwestern states including North 
    Dakota, South Dakota, Wyoming and Nebrasksa have had the smallest uninsured rate change at around 2%. With 8 different rate 
    change levels, it is very intuitive to understand the rates of change among each state. To say the least, I think this 
    visualization proves to help people better understand the impact of the ACA in a geographical sense. This heat map 
    displaying rate of change is an effective means of doing so."),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "columns",
                  label = "Explore:",
                  choices = c("Uninsured Rate 2010", "Uninsured Rate 2015", "Uninsured Rate Change (2010-2015)", "Family Premium Rate Change (2010 - 2015)"),
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
    } else if (input$columns == "Uninsured Rate Change (2010-2015)") {
      column = mapData$V4
      legendTitle = "Uninsured Rate (decrease)"
      gtitle="Insurance Coverage Change across United States from 2010-2015"
    } else {
      column = mapData$V17
      legendTitle = "Family Premium Rate (increase)"
      gtitle = "Family Premium Insurance Price Change across United States from 2010-2015"
    }
    
    ggplot(mapData, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = cut_number(column, 8))) +
      geom_path(colour = 'white') + labs(title = gtitle) +
      scale_fill_brewer(legendTitle) + coord_map()
  })
}

shinyApp(ui = ui, server = server)


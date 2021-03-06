require(shiny)
require(dplyr)
require(ggplot2)
require(maps)
require(ggmap)
require(rsconnect)

data("state.map")

# Read in the data
# rename columns as V1,V2 and so on to ease merging data process below.
df <- read.csv(file="data/datACA.csv", header = FALSE, skip = 1, sep = ",", strip.white = TRUE)

# add region column and convert state names to lowercase
df$region  = tolower(df$V1)

# get state map data and merge with insurance data
states = map_data('state')
mapData = merge(df, states, by = 'region') 


ui <- fluidPage(
  
  # Application title
  titlePanel("ACA Impact across United States"),
  h3("Introduction"),
  p("The purpose of this visualization is to share different insight using a compilation of State Data on 
    the Affordable Care Act deriving from ASPE and the Department of Health & Human Services (https://aspe.hhs.gov/compilation-state-data-affordable-care-act). 
    I sliced and diced  the columns I envisioned myself using and renamed them for more simplicity. With this dataset, there are valuable 
    answers to questions that many people are unaware of. There is alot of untapped knowledge to be gained and this 
    interactive tool helps explore those potentials. Since Obama signed the ACA into law on March 23, 2010, health 
    care access has certainly changed. Soon, you will learn more about the results the ACA has created for millions of 
    American lives using this tool. Questions I focused on answering using this dataset include:"),
  p("* How has the Affordable Care Act change the rate percentages of US citizens by State with health insurance?"),
  p("* Which states experienced the smallest/largest decline in their uninsured rate?"),
  p("* Which states have the smallest/largest uninsured rate?"),
  p("* Which states have the smallest/largest annual growth rates in family premiums?"),
  br(),
  h4("Findings: Uninsured Rate Percentages of 2010 & 2015"),
  p("This map-based visualization below shows uninsured rates for 2010, 2015 and the uninsured rate change between the two 
    periods. From viewing the rate in 2010, Nevada and Texas have the highest uninsured rates in the US at around 22%. 
    This means that roughly 22% of the people living in Nevada and Texas are without insurance. This is concerning and raises
    an additional question of why are these two states the outliers in this category. What are the driving forces behind this? 
    Additional things to think about are uninsured children, looking at the raw data I noticed that Texas had most uninsured children
    as well. This is interesting because in raw numbers one might expect the most populous states to have the highest totals
    but Texas has fewer residents than California. The Midwestern states, specifically Wisconsin and Minnesota, have the lowest 
    rate of people without insurance in the United States. In 2015, the uninsured rate for each and every state lowered. It may be 
    difficult to tell because the scale changes between time periods. Oklahoma, Texas and Georgia have the highest uninsured rates in 
    2015 and have the same color shade as the highest rates in 2010 so you really need to pay attention to the scale. If you switch between 
    the two time periods, you can estimate manually but its too time consuming and isnt ideal for conducting data analysis."), 
  br(),
  h4("Findings: Percentage Point Decrease in Uninsured Rate from 2010 to 2015"),
  p("So to really understand the difference between the two periods, the third filter to show rate of change is essential to
    understanding the implications of the ACA. As you can see, there is a decreasing trend of uninsured people for each state 
    on average. This is a very promising effect of the ACA and it is proving to have helped the United States as a whole. The states 
    with a lighter blue shade have a lower rate of change for the amount of people uninsured between 2010 and 2015. The darker blue 
    states have the highest rates of change for uninsured people. It seems that the amount of people uninsured living in the west 
    coast has decreased the most in the country with Oregon, California and Nevada with substantial differences of almost a 10% uninsured percentage decrease. 
    The midwestern states including North Dakota, South Dakota, Wyoming and Nebraska have had the smallest uninsured rate change at 
    around 2%. With 8 different rate change levels, it is very intuitive to understand the rates of change among each state. To say 
    the least, I think this visualization proves to help people better understand the impact of the ACA in a geographical sense. This 
    heat map displaying rate of change is an effective means of doing so."),
  br(),
  h4("Findings: Average Annual Growth in Family Premiums for Employer Coverage from 2010 to 2015"),
  p("This area of focus is additional insight worth mentioning because it direclty impacts the opportunity in terms of insurance
    affordability. Notably, there are 7 states (Idaho, Montana, Nevada, Missouri, Georgia, New York, and New Hampshire) that have had the highest level of an 
    average annual growth rate percentage increase of about 5.5 - 8.0 % growth/year in the past 5 years as of 2015.  I decided to dig deeper into why 
    this may be the case for New York (one of the 7 highest growth rate states) and found an interesting case as well. You can expect 
    New York to have high insurance costs due to the high cost of living. But to make matters premiums more expensive, Gov. Andrew Cuomo
    signed a bill enabling pregnat women to enroll in an insurance plan at any time. New York is the only state in the country that requires
    policyholders to accept pregnant women at any time. It is interesting to note that my home state of Washington is one of 5 states in the 
    country with less than 3% annual growth rate. As far as the average growth rate for the United States as a whole, it seems that 
    most of the annual growth rates from family premiums are increasing at about 4%. Will all of the pro-effects originating from the 
    creation of the ACA, this insight resonates alot with people that are against the plan. Since premiums are increasing for most people,
    it is certainly a tradeoff that some people may have a very challenging time accepting."),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "columns",
                  label = "Explore:",
                  choices = c("Uninsured Rate 2010", "Uninsured Rate 2015", "Uninsured Rate Change (2010-2015)",
                              "Family Premium Rate Change (2010 - 2015)"),
                  selected= "Uninsured Rate 2010"),
      em("Above, you can select between four different explorative subtopics - to gain more knowledge about the effects of the ACA on 
      health insurance coverage.")
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
      legendTitle = "Uninsured Rate %"
      gtitle="Insurance Coverage across United States in 2010"
    } else if (input$columns == "Uninsured Rate 2015") {
      column = mapData$V3
      legendTitle = "Uninsured Rate %"
      gtitle="Insurance Coverage across United States in 2015"
    } else if (input$columns == "Uninsured Rate Change (2010-2015)") {
      column = mapData$V4
      legendTitle = "Uninsured Rate (% decrease)"
      gtitle="Insurance Coverage Change across United States from 2010-2015"
    } else {
      column = mapData$V17
      legendTitle = "Family Premium Rate (% increase)"
      gtitle = "Family Premium Insurance Price Change across United States from 2010-2015"
    } 
    
    ggplot(mapData, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = cut_number(column, 8))) +
      geom_path(colour = 'white') + labs(title = gtitle) +
      scale_fill_brewer(legendTitle) + coord_map()
  })
}

shinyApp(ui = ui, server = server)


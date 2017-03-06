library(shiny)
library(HSAUR)
library(dplyr)
library(ggplot2)

data <- (womensrole)

data.men <- data %>%
  filter(sex =="Male")

data.women <- data %>%
  filter(sex == "Female")

ui <- fluidPage(
  
  # Application title
  titlePanel("Women's Role in Society"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gender",
                  label = "Gender:",
                  choices = c("Men", "Women", "Both"),
                  selected = "Both"),
      
      selectInput(inputId = "thoughts",
                  label = "Who:",
                  choices = c("Agree", "Disagree"),
                  selected= "Agree"),
      
      sliderInput("integer", inputId = "education",
                  label = "Education level",
                  min = 0, max = 20, value = 20, step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rolePlot")
    )
  )
)

server <- function(input, output) {
  
  output$rolePlot <- renderPlot({
    
    if(input$gender == "Men") {
      plot.data <- data.men
    } else if (input$gender == "Women") {
      plot.data <- data.women
    } else {
      plot.data <- data
    }
    
    plot.data <- plot.data %>%
      filter(education <= input$education)
    
    if(input$thoughts == "Agree") {
      y = plot.data$agree
    } else {
      y = plot.data$disagree
    }
    
    ggplot(plot.data, aes(x=education, y=y, color=factor(sex))) + geom_point()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

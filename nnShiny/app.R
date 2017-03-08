# Nick Nordale

library(ggplot2)
library(gridExtra)
library(dplyr)
library(shiny)
library(rsconnect)
require(maps)

us <- map_data("state")

aca <- read.csv('aca.csv')
a.abbreviations <- aca$State
aca$region <- tolower(state.name[match(a.abbreviations, state.abb)])

private <- read.csv('private.csv')
p.abbreviations <- private$StateCode
private$region <- tolower(state.name[match(p.abbreviations, state.abb)])

# value = aca, rate = private
tot <- inner_join(aca, private, 
                  by = c('State' = 'StateCode', 
                         'Metal.Level' = 'MetalLevel', 
                         'age' = 'aca_age', 
                         'year' = 'BusinessYear', 
                         'region' = 'region'), 
                  suffix = c(".aca", ".p"))

tot.map <- full_join(tot, us, by = 'region')

tot$diff <- tot$rate - tot$value

state.level <- tot %>% group_by(region) %>% 
  summarise(avg_aca = mean(value), avg_private = mean(rate), diff = mean(rate) - mean(value))

state.level.map <- full_join(state.level, us, by = 'region')

ui <- fluidPage(
  
  selectInput(inputId = "type",
              label = "Heath Insurance Type (aca, private",
              choices = c('ACA', 'Private'),
              selected = 'ACA'),
  
  plotOutput(outputId = "main_plot", width = '800px', height = '400px')
)

server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    if (input$type == 'ACA') {
      metric = 'avg_aca'
    } else if (input$type == 'Private') {
      metric = 'avg_private'
    }
    
    ggplot(state.level.map, aes(map_id = region)) + 
      geom_map(aes_string(fill = metric), map = us) + 
      expand_limits(x = state.level.map$long, y = state.level.map$lat)
  })
}

shinyApp(ui = ui, server = server)

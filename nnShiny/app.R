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

state.age <- tot %>% group_by(region, age) %>% 
  summarise(avg_aca = mean(value), avg_private = mean(rate), diff = mean(rate) - mean(value))

state.level.map <- full_join(state.level, us, by = 'region')

ui <- fluidPage(
  
  titlePanel("ACA Pricing Analysis"),
  p("The following visualizations display data from 2014-2016 on the average premium costs of individual health care plans for various states and ages."),
  br(),
  
  mainPanel(
    fluidRow(
      selectInput(inputId = "type",
                  label = "Heath Insurance Type",
                  choices = c('ACA', 'Private'),
                  selected = 'ACA'),
      
      plotOutput(outputId = "main_plot", width = '800px', height = '400px'),
      br(),
      p("We found some very interesting results from looking at the average premium costs of individual healthcare plans for various 
        states and ages. Virginia was a stark outlier compared to the other states. It has by 
        far the highest average premiums for ACA plans of any state. It was the only state whose 
        average premiums for ACA plans was actually higher that the private market plans. That said, 
        because it was such an anomaly, we have concerns about the data quality and collections methods 
        for Virginia. Further validation will need to be done in analyzing this insight."),
      p("Another interesting observation we made during this analysis was the inconsistency of states 
        average costs in relation to the other states for each type of insurance. In other words, 
        the states with the cheapest private health insurance, did not necessarily have the cheapest 
        average ACA insurance. For example, Virginia’s average private cost is relatively normal compared 
        to its extremely high ACA cost. Similarly, Utah has the highest average private costs, but they 
        have a fairly normal average ACA cost. This may be worth digging into more thoroughly in the future.")
    ),
    br(),
    br(),
    
    fluidRow(
      column(
        6,
        h4("Highest Avg Savings"),
        selectInput(inputId = "table.age",
                    label = "Age",
                    choices = c('All', 'Child', '21', '27', '30', '40', '50', '60'),
                    selected = 'All'),
        
        tableOutput('top.10.diff')
      ),
      column(
        6,
        h4("Lowest Avg Savings"),
        selectInput(inputId = "table.age2",
                    label = "Age",
                    choices = c('All', 'Child', '21', '27', '30', '40', '50', '60'),
                    selected = 'All'),
        
        tableOutput('bottom.10.diff')
      )
    ),
    width = 10
  )
)

server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    if (input$type == 'ACA') {
      metric = 'avg_aca'
    } else if (input$type == 'Private') {
      metric = 'avg_private'
    }
    
    if (input$table.age == 'All') {
      ages = c(10, 21, 27, 30, 40, 50, 60)
    } else if (input$table.age == 'Child') {
      ages = c(10)
    } else if (input$table.age == '21-26') {
      ages = c(21)
    } else if (input$table.age == '27-29') {
      ages = c(27)
    } else if (input$table.age == '30-39') {
      ages = c(30)
    } else if (input$table.age == '40-49') {
      ages = c(40)
    } else if (input$table.age == '50-59') {
      ages = c(50)
    } else if (input$table.age == '60+') {
      ages = c(60)
    }
    
    if (input$table.age2 == 'All') {
      ages = c(10, 21, 27, 30, 40, 50, 60)
    } else if (input$table.age == 'Child') {
      ages = c(10)
    } else if (input$table.age == '21-26') {
      ages = c(21)
    } else if (input$table.age == '27-29') {
      ages = c(27)
    } else if (input$table.age == '30-39') {
      ages = c(30)
    } else if (input$table.age == '40-49') {
      ages = c(40)
    } else if (input$table.age == '50-59') {
      ages = c(50)
    } else if (input$table.age == '60+') {
      ages = c(60)
    }
    
    output$top.10.diff <- renderTable(
      state.age %>% 
        filter(age %in% ages) %>%
        group_by(region) %>% 
        summarise(avg_aca = mean(avg_aca), avg_private = mean(avg_private), diff = mean(avg_private) - mean(avg_aca)) %>%
        arrange(-diff) %>% 
        top_n(10) %>% 
        mutate('State' = sub("(.)", "\\U\\1", region, perl=TRUE), 'Avg ACA Savings' = round(diff, 2)) %>% 
        mutate('Avg ACA Savings' = paste0('$', `Avg ACA Savings`)) %>%
        select(one_of(c('State', 'Avg ACA Savings'))),
      spacing = 'm', 
      striped = TRUE,  
      width = '100%', 
      align = 'c'
    )
    
    output$bottom.10.diff <- renderTable(
      state.age %>% 
        filter(age %in% ages) %>%
        group_by(region) %>% 
        summarise(avg_aca = mean(avg_aca), avg_private = mean(avg_private), diff = mean(avg_private) - mean(avg_aca)) %>%
        arrange(diff) %>% 
        top_n(-10) %>% 
        mutate('State' = sub("(.)", "\\U\\1", region, perl=TRUE), 'Avg ACA Savings' = round(diff, 2)) %>% 
        mutate('Avg ACA Savings' = paste0('$', `Avg ACA Savings`)) %>%
        select(one_of(c('State', 'Avg ACA Savings'))),
      spacing = 'm', 
      striped = TRUE,  
      width = '100%', 
      align = 'c'
    )
    
    ggplot(state.level.map, aes(map_id = region)) + 
      geom_map(aes_string(fill = metric), map = us) + 
      expand_limits(x = state.level.map$long, y = state.level.map$lat) + 
      guides(fill = guide_legend(title = "Average Individual Premium"))
  })
}

shinyApp(ui = ui, server = server)

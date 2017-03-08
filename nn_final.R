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
#aca.map <- full_join(aca, us, by = 'region')

private <- read.csv('private.csv')
p.abbreviations <- private$StateCode
private$region <- tolower(state.name[match(p.abbreviations, state.abb)])
#private.map <- full_join(private, us, by = 'region')

# value = aca, rate = private
tot <- inner_join(aca, private, 
                  by = c('State' = 'StateCode', 
                         'Metal.Level' = 'MetalLevel', 
                         'age' = 'aca_age', 
                         'year' = 'BusinessYear', 
                         'region' = 'region'), 
                  suffix = c(".aca", ".p"))

tot.map <- full_join(tot, us, by = 'region')

ggplot(tot.map, aes(map_id = region)) + 
  geom_map(aes(fill = value), map = us) + 
  expand_limits(x = tot.map$long, y = tot.map$lat)

ggplot(tot.map, aes(map_id = region)) + 
  geom_map(aes(fill = rate), map = us) + 
  expand_limits(x = tot.map$long, y = tot.map$lat)

tot$diff <- tot$rate - tot$value

state.level <- tot %>% group_by(region) %>% 
  summarise(avg_aca = mean(value), avg_private = mean(rate), diff = mean(rate) - mean(value))

state.level.map <- full_join(state.level, us, by = 'region')

ggplot(state.level.map, aes(map_id = region)) + 
  geom_map(aes(fill = avg_aca), map = us) + 
  expand_limits(x = state.level.map$long, y = state.level.map$lat)

ggplot(state.level.map, aes(map_id = region)) + 
  geom_map(aes(fill = avg_private), map = us) + 
  expand_limits(x = state.level.map$long, y = state.level.map$lat)

ggplot(state.level.map, aes(map_id = region)) + 
  geom_map(aes(fill = diff), map = us) + 
  expand_limits(x = state.level.map$long, y = state.level.map$lat)








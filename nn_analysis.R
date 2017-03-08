# Nick Nordale
#library(plyr)
#library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(dplyr)

# write.csv(prac, file = "foo.csv", row.names = FALSE)

private.rates <- read.csv('health-insurance-marketplace/Rate.csv', stringsAsFactors = FALSE)

drop.cols <- c('RatingAreaId', 'RowNumber', 'FederalTIN', 
               'ImportDate', 'RateEffectiveDate', 'RateExpirationDate', 
               'Tobacco', 'IndividualTobaccoRate', 'Couple', 'PrimarySubscriberAndOneDependent', 
               'PrimarySubscriberAndTwoDependents', 'PrimarySubscriberAndThreeOrMoreDependents', 
               'CoupleAndOneDependent', 'CoupleAndTwoDependents', 'CoupleAndThreeOrMoreDependents',
               'IssuerId', 'IssuerId2', 'SourceName', 'VersionNum')

private.rates <- private.rates %>% select(-one_of(drop.cols))
private.rates <- private.rates %>% filter(Age != 'Family Option')
private.rates <- distinct(private.rates)
#private.rates <- private.rates %>% group_by(BusinessYear, StateCode, PlanId, Tobacco, Age) %>% summarise_each(funs(mean))
private.rates$aca_age <- ifelse(private.rates$Age >= 21 & private.rates$Age < 27, 21, 
                                ifelse(private.rates$Age >= 27 & private.rates$Age < 30, 27, 
                                       ifelse(private.rates$Age >= 30 & private.rates$Age < 40, 30, 
                                              ifelse(private.rates$Age >= 40 & private.rates$Age < 50, 40, 
                                                     ifelse(private.rates$Age >= 50 & private.rates$Age < 60, 50, 
                                                            ifelse(private.rates$Age >= 60, 60, 
                                                                   ifelse(private.rates$Age == '65 and over', 60, 10)))))))

private.rates <- private.rates %>% select(-one_of(c('Age')))

rate.sum <- private.rates %>% group_by(BusinessYear, StateCode, PlanId, aca_age) %>% summarise(mean.rate = mean(IndividualRate))

plan.attributes <- read.csv('health-insurance-marketplace/PlanAttributes.csv', stringsAsFactors = FALSE)
plan.attributes <- plan.attributes %>% select(-one_of(drop.cols))
plan.attributes[is.na(plan.attributes)] <- ""
plan.attributes[plan.attributes == 'Not Applicable'] <- ""
plan.attributes <- plan.attributes[, colSums(plan.attributes != "") != 0]
plan.attributes <- distinct(plan.attributes)
plan.attributes <- plan.attributes %>% filter(MetalLevel != 'Low' & MetalLevel != 'High')

metal.levels <- distinct(plan.attributes %>% select(one_of(c("MetalLevel", "StandardComponentId"))))
private <- inner_join(rate.sum, metal.levels, by = c("PlanId" = "StandardComponentId"))

private <- private %>% select(-one_of(c('PlanId')))
private <- private %>% group_by(BusinessYear, StateCode, aca_age, MetalLevel) %>% summarise(rate = mean(mean.rate))

#rm(private.rates, plan.attributes)

#write.csv(plan.attributes, file = "pa.csv", row.names = FALSE)

# StandardComponentId
# dim(plan.attributes %>% select(one_of(c("Metal"))))

#private <- left_join(private.rates, plan.attributes, 
#                     by = c("BusinessYear" = "BusinessYear", "StateCode" = "StateCode", 
#                            "IssuerId" = "IssuerId", "SourceName" = "SourceName", 
#                            "VersionNum" = "VersionNum", "IssuerId2" = "IssuerId2", "PlanId" = "StandardComponentId"))



#private2 <- inner_join(private.rates, plan.attributes)

##########################################################################################

df14 <- read.csv('aca_14.csv', stringsAsFactors = FALSE)
df15 <- read.csv('aca_15.csv', stringsAsFactors = FALSE)
df16 <- read.csv('aca_16.csv', stringsAsFactors = FALSE)
df16$State <- df16$State.Code
df16$Plan.ID..standard.component. <- df16$Plan.ID..Standard.Component.

aca.cols = c("State", "Metal.Level", "Premium.Child", 
             "Premium.Adult.Individual.Age.21", "Premium.Adult.Individual.Age.27", 
             "Premium.Adult.Individual.Age.30", "Premium.Adult.Individual.Age.40", 
             "Premium.Adult.Individual.Age.50", "Premium.Adult.Individual.Age.60")

df14 <- select(df14, one_of(aca.cols))
df15 <- select(df15, one_of(aca.cols))
df16 <- select(df16, one_of(aca.cols))

df14[df14 == ''] <- NA
df14[df14 == ' '] <- NA
df14 <- na.omit(df14)
df15[df15 == ''] <- NA
df15[df15 == ' '] <- NA
df15 <- na.omit(df14)
df16[df16 == ''] <- NA
df16[df16 == ' '] <- NA
df16 <- na.omit(df14)

toNumeric <- function(arg1) {
  return(as.numeric(sub('\\,', '', as.character(sub('\\$','', as.character(arg1))))))
}

df14 <- df14 %>% group_by(State, Metal.Level) %>% mutate_each(funs(toNumeric))
df15 <- df15 %>% group_by(State, Metal.Level) %>% mutate_each(funs(toNumeric))
df16 <- df16 %>% group_by(State, Metal.Level) %>% mutate_each(funs(toNumeric))

df14[is.na(df14)] <- 0
df15[is.na(df15)] <- 0
df16[is.na(df16)] <- 0

df14 <- df14 %>% group_by(State, Metal.Level) %>% summarise_each(funs(mean))
df15 <- df15 %>% group_by(State, Metal.Level) %>% summarise_each(funs(mean))
df16 <- df16 %>% group_by(State, Metal.Level) %>% summarise_each(funs(mean))

df14 <- df14 %>% rename('10' = Premium.Child) %>% 
  rename('21' = Premium.Adult.Individual.Age.21) %>% 
  rename('27' = Premium.Adult.Individual.Age.27) %>% 
  rename('30' = Premium.Adult.Individual.Age.30) %>% 
  rename('40' = Premium.Adult.Individual.Age.40) %>% 
  rename('50' = Premium.Adult.Individual.Age.50) %>%
  rename('60' = Premium.Adult.Individual.Age.60)

df15 <- df15 %>% rename('10' = Premium.Child) %>% 
  rename('21' = Premium.Adult.Individual.Age.21) %>% 
  rename('27' = Premium.Adult.Individual.Age.27) %>% 
  rename('30' = Premium.Adult.Individual.Age.30) %>% 
  rename('40' = Premium.Adult.Individual.Age.40) %>% 
  rename('50' = Premium.Adult.Individual.Age.50) %>%
  rename('60' = Premium.Adult.Individual.Age.60)

df16 <- df16 %>% rename('10' = Premium.Child) %>% 
  rename('21' = Premium.Adult.Individual.Age.21) %>% 
  rename('27' = Premium.Adult.Individual.Age.27) %>% 
  rename('30' = Premium.Adult.Individual.Age.30) %>% 
  rename('40' = Premium.Adult.Individual.Age.40) %>% 
  rename('50' = Premium.Adult.Individual.Age.50) %>%
  rename('60' = Premium.Adult.Individual.Age.60)

df14 <- melt(data = df14, id.vars = c('State', 'Metal.Level'))
df15 <- melt(data = df15, id.vars = c('State', 'Metal.Level'))
df16 <- melt(data = df16, id.vars = c('State', 'Metal.Level'))

df14$year <- '2014'
df15$year <- '2015'
df16$year <- '2016'

df14 <- transform(df14, variable = as.numeric(variable))
df15 <- transform(df15, variable = as.numeric(variable))
df16 <- transform(df16, variable = as.numeric(variable))

aca <- bind_rows(df14, df15, df16)

##########################################################################################



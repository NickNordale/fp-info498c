# Nick Nordale
#library(plyr)
#library(tidyr)
#library(reshape2)
library(ggplot2)
library(gridExtra)
library(dplyr)

# write.csv(prac, file = "foo.csv", row.names = FALSE)

private.rates <- read.csv('health-insurance-marketplace/Rate.csv', stringsAsFactors = FALSE)

drop.cols <- c('RatingAreaId', 'RowNumber', 'FederalTIN', 
               'ImportDate', 'RateEffectiveDate', 'RateExpirationDate', 
               'Tobacco', 'IndividualTobaccoRate', 'Couple', 'PrimarySubscriberAndOneDependent', 
               'PrimarySubscriberAndTwoDependents', 'PrimarySubscriberAndThreeOrMoreDependents', 
               'CoupleAndOneDependent', 'CoupleAndTwoDependents', 'CoupleAndThreeOrMoreDependents')

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
                                                                   ifelse(private.rates$Age == '65 and over', 60, 'Child')))))))


plan.attributes <- read.csv('health-insurance-marketplace/PlanAttributes.csv', stringsAsFactors = FALSE)
plan.attributes <- plan.attributes %>% select(-one_of(drop.cols))
plan.attributes[is.na(plan.attributes)] <- ""
plan.attributes[plan.attributes == 'Not Applicable'] <- ""
plan.attributes <- plan.attributes[, colSums(plan.attributes != "") != 0]
plan.attributes <- distinct(plan.attributes)
plan.attributes <- plan.attributes %>% filter(MetalLevel != 'Low' & MetalLevel != 'High')

private <- inner_join(private.rates, plan.attributes, by = c("PlanId" = "StandardComponentId"))
rm(private.rates, plan.attributes)

#write.csv(plan.attributes, file = "pa.csv", row.names = FALSE)

# StandardComponentId

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

aca.cols = c("Plan.ID..standard.component.", "State", "Metal.Level", "Premium.Child", 
             "Premium.Adult.Individual.Age.21", "Premium.Adult.Individual.Age.27", 
             "Premium.Adult.Individual.Age.30", "Premium.Adult.Individual.Age.40", 
             "Premium.Adult.Individual.Age.50", "Premium.Adult.Individual.Age.60")

df14 <- select(df14, one_of(aca.cols))
df15 <- select(df15, one_of(aca.cols))
df16 <- select(df16, one_of(aca.cols))

toNumeric <- function(arg1) {
  return(as.numeric(sub('\\,', '', as.character(sub('\\$','', as.character(arg1))))))
}

df14 <- df14 %>% group_by(Plan.ID..standard.component., State, Metal.Level) %>% mutate_each(funs(toNumeric))
df15 <- df15 %>% group_by(Plan.ID..standard.component., State, Metal.Level) %>% mutate_each(funs(toNumeric))
df16 <- df16 %>% group_by(Plan.ID..standard.component., State, Metal.Level) %>% mutate_each(funs(toNumeric))

df14[is.na(df14)] <- 0
df15[is.na(df15)] <- 0
df16[is.na(df16)] <- 0



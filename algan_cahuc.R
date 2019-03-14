library(countrycode)
library(dplyr)

setwd('~/documents/algan_cahuc')
data <- read.csv('algan_cahuc.csv', header=F, stringsAsFactors=F)
names(data) <- c('country', 'trust')
data$region<-countrycode(data$country,origin='country.name', destination = 'region')
data %>% na.omit() -> data
data

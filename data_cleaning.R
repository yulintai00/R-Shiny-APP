## This file is for dataset...

## Loading libraries
library(dplyr)
library(ggplot2)

## Setting working directory
getwd()
setwd('~/Desktop/1. grad(PurdueBAIM)/Lectures/1.Summer_21/MGMT590(R)/project/dataset')

## Loading each dataset and combine them into a one dataset
red <- read.csv(file='Red.csv', header=T, sep=',')
rose <- read.csv(file='Rose.csv', header=T, sep=',')
sparkling <- read.csv(file='Sparkling.csv', header=T, sep=',')
varieties <- read.csv(file='Varieties.csv', header=T, sep=',')
white <- read.csv(file='White.csv', header=T, sep=',')

red$Type <- "Red"
rose$Type <- "Rose"
sparkling$Type <- "Sparkling"
white$Type <- "White"

wine <- rbind(red, rose, sparkling, white)

## Making new dataset for autocomplete text input
wine <- wine %>% 
  filter(Year != 'N.V.')
names(wine)[7] <- "Price"
wine$NumberOfRatings <- as.integer(wine$NumberOfRatings)
wine$Year <- as.integer(wine$Year)
wine$Price <- as.numeric(wine$Price)
wine$Rating <- as.numeric(wine$Rating)

wine <- wine %>%
  mutate(name_winery_region = paste0(wine[,'Name'],sep=" - ",wine[,'Winery'],sep=', ',wine[,'Region']))
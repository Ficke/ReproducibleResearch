# Author: Adam Ficke
# Date: 2/18/21

#load libraries
install.packages("data.table")
install.packages("R.utils")

library(tidyverse)
library(data.table)
library(R.utils)

#Read in data
df <-
        fread("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")
colnames(df)

#Q1 Across the United States, which types of events (as indicated in the 
#EVTYPE are most harmful with #respect to population health?

#convert to dataframe 
df <- as.data.frame(df)

#summarize for fatalities by EVTYPE 
fatality_summary <- df %>%
        group_by(EVTYPE) %>%
        summarise(fatalities = sum(FATALITIES)) %>%
        arrange(desc(fatalities))

#summarize for injuries by EVTYPE 
injury_summary <- df %>%
        group_by(EVTYPE) %>%
        summarise(fatalities = sum(INJURIES)) %>%
        arrange(desc(fatalities))

#summarize for total health outcomes by EVTYPE 
health_summary <- df %>%
        mutate(HEALTH = INJURIES + FATALITIES) %>%
        group_by(EVTYPE) %>%
        summarise(TOT_HEALTH = sum(HEALTH)) %>%
        arrange(desc(TOT_HEALTH))




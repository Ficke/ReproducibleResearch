# Author: Adam Ficke
# Date: 2/18/21

#load libraries
install.packages("data.table")
library(tidyverse)
library(data.table)
library(R.utils)
library(lubridate)

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
        summarise(Injuries_Fatalities = sum(HEALTH)) %>%
        arrange(desc(Injuries_Fatalities))
health_summary

#plot the response

health_plot <- health_summary %>%
        top_n(10) %>%
        ggplot(aes(x=reorder(EVTYPE,-Injuries_Fatalities),y=Injuries_Fatalities)) + 
        geom_col() +
        ggtitle("Health Impacts by Event Type") + 
        theme(plot.title = element_text(hjust=0.5)) + 
        theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 0.5))
health_plot

#look at it by year
health_summary <- df %>%
        mutate(year = year(mdy_hms(BGN_DATE))) %>% 
        group_by(year) %>%
        summarise(Injuries_Fatalities = sum(c(FATALITIES,INJURIES)),n=n()) %>%
        mutate(health_per_event = Injuries_Fatalities/n) %>%
        arrange(desc(health_per_event))


#show the number of events by year for the top 5 deadliest events

#first calculate the top 5 deadliest events
TOP_5 <- df %>%
        group_by(EVTYPE) %>%
        summarise(Injuries_Fatalities = sum(c(INJURIES,FATALITIES))) %>%
        arrange(desc(Injuries_Fatalities)) %>% 
        top_n(5)

#now show the number of events for each over time for the top 5 

incidence_time <- df %>% 
        mutate(year = year(mdy_hms(BGN_DATE))) %>% 
        group_by(year,EVTYPE) %>%
        filter(EVTYPE %in% TOP_5[[1]]) %>%
        count() %>% 
        ggplot(aes(x = year, y=n, group = EVTYPE, color=EVTYPE)) + 
        geom_line() + 
        ggtitle("Incidence of Dedliest Weather Events by Year")
        
incidence_time

#health events over time for top 5

health_time <- df %>% 
        mutate(year = year(mdy_hms(BGN_DATE))) %>% 
        group_by(year,EVTYPE) %>%
        filter(EVTYPE %in% TOP_5[[1]]) %>%
        summarise(Injuries_Deaths = sum(c(FATALITIES,INJURIES))) %>% 
        ggplot(aes(x = year, y=Injuries_Deaths, group = EVTYPE, color=EVTYPE)) + 
        geom_line() + 
        ggtitle("Injuries and Deaths of Dedliest Weather Events by Year")

health_time

install.packages("gridExtra")
library(gridExtra)

grid.arrange(incidence_time,health_time)


year_plot <- health_summary %>%
        ggplot(aes(x=year,y=Injuries_Fatalities)) + 
        geom_col() + 
        theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 0.5))
year_plot

#Across the United States, which types of events 
#have the greatest economic consequences?

#summarize for PROPDMG by EVTYPE 
PROPDMG_summary <- df %>%
        group_by(EVTYPE) %>%
        summarise(PROPDMG = sum(PROPDMG)) %>%
        arrange(desc(PROPDMG))

#summarize for CROPDMG by EVTYPE 
CROPDMG_summary <- df %>%
        group_by(EVTYPE) %>%
        summarise(CROPDMG = sum(CROPDMG)) %>%
        arrange(desc(CROPDMG))

#summarize for total damage outcomes by EVTYPE 
damange_summary <- df %>%
        mutate(DAMAGE = PROPDMG + CROPDMG) %>%
        group_by(EVTYPE) %>%
        summarise(DAMAGE = sum(DAMAGE)) %>%
        arrange(desc(DAMAGE))

damange_summary

damage_plot <- damange_summary %>%
        top_n(10) %>%
        ggplot(aes(x=reorder(EVTYPE,-DAMAGE),y=DAMAGE)) + 
        geom_col() + 
        theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 0.5)) +
        ggtitle("Property Damage Impacts by Event Type") + 
        theme(plot.title = element_text(hjust=0.5))
        
damage_plot


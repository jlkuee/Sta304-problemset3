#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/sunyiyun/Desktop/sta304/ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(registration,
         vote_2016,
         vote_intention,
         vote_2020,
         employment,
         foreign_born,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age,
         weight)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

reduced_data$age<-as.numeric(reduced_data$age)



reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
 mutate(vote_biden = 
                 ifelse(vote_2020 =="Joe Biden", 1, 0))
  

reduced_data <- 
  reduced_data %>% 
  filter(registration == "Registered") %>%
  filter(education != "Completed some graduate, but no degree") 

reduced_data <- na.omit(reduced_data) 
  
reduced_data$education[reduced_data$education=="Other post high school vocational training"]<-"High school graduate"

otherandpaci<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Korean)","Asian (Other)","Asian (Filipino)",
              "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")

reduced_data<-reduced_data %>% 
  mutate(race = case_when(race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% otherandpaci ~"Other asian or pacific islander",
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native"
  )) 


reduced_data <- na.omit(reduced_data)  
  
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")




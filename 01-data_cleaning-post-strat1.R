#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/sunyiyun/Desktop/sta304/ps3")
raw_datac <- read_dta("usa_00003.dta")


# Add the labels
raw_datac <- labelled::to_factor(raw_datac)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_datac <- 
  raw_datac %>% 
  select(statefip,
         sex, 
         age, 
         race, 
         educd,
         hhincome,
         perwt)
         



#### What's next? ####


reduced_datac$age<-as.numeric(reduced_datac$age)


reduced_datac <- reduced_datac %>%
  filter(educd != "n/a") %>%
  filter(hhincome != "9999999") %>%  
  filter(hhincome >= 0) %>%
  filter(age >= 18)


reduced_datac <- na.omit(reduced_datac)


reduced_datac <- reduced_datac %>%
  rename(gender = sex) %>%
  rename(race_ethnicity = race)



reduced_datac$gender<-ifelse(reduced_datac$gender=="female","Female","Male")



gradeless3<-c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade11<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
edu.high<-c("ged or alternative credential","regular high school diploma")
edu.coll<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")
reduced_datac<-reduced_datac %>% 
  mutate(education = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~"Doctorate degree",
                            educd =="master's degree" ~ "Masters degree",
                            educd=="professional degree beyond a bachelor's degree" ~ "Masters degree",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.) ",
                            educd %in% gradeless3 ~"3rd Grade or less",
                            educd %in% grade8~"Middle School - Grades 4 - 8",
                            educd %in% grade11~"Completed some high school",
                            educd %in% edu.high~"High school graduate",
                            educd %in% edu.coll~"Completed some college, but no degree"
  )) 


reduced_datac<-reduced_datac %>% 
  mutate(state = case_when(statefip=="alabama"~"AL",
                           statefip=="alaska"~"AK",
                           statefip=="arizona"~"AZ",
                           statefip=="arkansas"~"AR",
                           statefip=="california"~"CA",
                           statefip=="colorado"~"CO",
                           statefip=="connecticut"~"CT",
                           statefip=="delaware"~"DE",
                           statefip=="district of columbia"~"DC",
                           statefip=="florida"~"FL",
                           statefip=="georgia"~"GA",
                           statefip=="hawaii"~"HI",
                           statefip=="idaho"~"ID",
                           statefip=="illinois"~"IL",
                           statefip=="indiana"~"IN",
                           statefip=="iowa"~"IA",
                           statefip=="kansas"~"KS",
                           statefip=="kentucky"~"KY",
                           statefip=="louisiana"~"LA",
                           statefip=="maine"~"ME",
                           statefip=="maryland"~"MD",
                           statefip=="massachusetts"~"MA",
                           statefip=="michigan"~"MI",
                           statefip=="minnesota"~"MN",
                           statefip=="mississippi"~"MS",
                           statefip=="missouri"~"MO",
                           statefip=="montana"~"MT",
                           statefip=="nebraska"~"NE",
                           statefip=="nevada"~"NV",
                           statefip=="new hampshire"~"NH",
                           statefip=="new jersey"~"NJ",
                           statefip=="new mexico"~"NM",
                           statefip=="new york"~"NY",
                           statefip=="north carolina"~"NC",
                           statefip=="north dakota"~"ND",
                           statefip=="ohio"~"OH",
                           statefip=="oklahoma"~"OK",
                           statefip=="oregon"~"OR",
                           statefip=="pennsylvania"~"PA",
                           statefip=="rhode island"~"RI",
                           statefip=="south carolina"~"SC",
                           statefip=="south dakota"~"SD",
                           statefip=="tennessee"~"TN",
                           statefip=="texas"~"TX",
                           statefip=="utah"~"UT",
                           statefip=="vermont"~"VT",
                           statefip=="virginia"~"VA",
                           statefip=="washington"~"WA",
                           statefip=="west virginia"~"WV",
                           statefip=="wisconsin"~"WI",
                           statefip=="wyoming"~"WY")) 


reduced_datac<-reduced_datac %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 



reduced_datac<-reduced_datac %>% 
  mutate(race = case_when(race_ethnicity=="white"~"White",
                          race_ethnicity=="chinese"~"Chinese",
                          race_ethnicity=="black/african american/negro"~"Black, or African American",
                          race_ethnicity=="two major races"~"Other race",
                          race_ethnicity=="other race, nec"~"Other race",
                          race_ethnicity=="japanese"~"Japanese",
                          race_ethnicity=="american indian or alaska native"~"American Indian or Alaska Native",
                          race_ethnicity=="three or more major races"~"Other race",
                          race_ethnicity=="other asian or pacific islander"~"Other asian or pacific islander"
  )) 



reduced_datac <- 
  reduced_datac %>%
  count(state,gender,age,race,education,household_income)  



# Saving the census data as a csv file in my
# working directory
write_csv(reduced_datac, "census_data.csv")



         
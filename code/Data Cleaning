#STAT260 FINAL GROUP PROJECT

#READING DATA SETS: selecting and renaming columns

library(foreign)
library(tidyverse)
library(haven)
library(fstcore)
library(fst)

#using sample data sets

#2024
brfss2024_sample<-read.csv("brfss2024_sample.csv")
dim(brfss2024_sample)
brfss2024_sample<-as_tibble(brfss2024_sample)
d_2024 <- brfss2024_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2024, month = brfss2024_sample$IMONTH, day = brfss2024_sample$IDAY))

#2023
brfss2023_sample<-read.csv("brfss2023_sample.csv")
dim(brfss2023_sample)
brfss2023_sample<-as_tibble(brfss2023_sample)
d_2023 <- brfss2023_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2023, month = brfss2023_sample$IMONTH, day = brfss2023_sample$IDAY))

#2022
brfss2022_sample<-read.csv("brfss2022_sample.csv")
dim(brfss2022_sample)
brfss2022_sample<-as_tibble(brfss2022_sample)
d_2022 <- brfss2022_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE1, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2022, month = brfss2022_sample$IMONTH, day = brfss2022_sample$IDAY))

#2021
brfss2021_sample<-read.csv("brfss2021_sample.csv")
dim(brfss2021_sample)
brfss2021_sample<-as_tibble(brfss2021_sample)
d_2021 <- brfss2021_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2021, month = brfss2021_sample$IMONTH, day = brfss2021_sample$IDAY))

#2020
brfss2020_sample<-read.csv("brfss2020_sample.csv")
dim(brfss2020_sample)
brfss2020_sample<-as_tibble(brfss2020_sample)
d_2020 <- brfss2020_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2020, month = brfss2020_sample$IMONTH, day = brfss2020_sample$IDAY))

#2019
brfss2019_sample<-read.csv("brfss2019_sample.csv")
dim(brfss2019_sample)
brfss2019_sample<-as_tibble(brfss2019_sample)
d_2019 <- brfss2019_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7) %>% mutate(date = make_date(year = 2019, month = brfss2019_sample$IMONTH, day = brfss2019_sample$IDAY))

#2018
brfss2018_sample<-read.csv("brfss2018_sample.csv")
dim(brfss2018_sample)
brfss20218_sample<-as_tibble(brfss2018_sample)
d_2018 <- brfss2018_sample %>% select(state=X_STATE, sex=SEX1, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT6) %>% mutate(date = make_date(year = 2018, month = brfss2018_sample$IMONTH, day = brfss2018_sample$IDAY))

#combining years into one data set
d_combined<- bind_rows(d_2018, d_2019, d_2020, d_2021, d_2022, d_2023, d_2024)
d_combined <- as_tibble(d_combined)
dim(d_combined)


#CLEANING VARIABLES

#vaccine 
d_combined$vaccine_DKorR <- ifelse(d_combined$vaccine==7 | d_combined$vaccine==9, 1, 0) #saving 'don't know' and 'refused' responses
d_combined <- d_combined %>% mutate(vaccine = case_match(vaccine, 1 ~ "yes", 2 ~ "no", 7 ~ NA, 9 ~ NA))

#state
state_dict <- c(
  "1" = "Alabama",
  "2" = "Alaska",
  "4" = "Arizona",
  "5" = "Arkansas",
  "6" = "California",
  "8" = "Colorado",
  "9" = "Connecticut",
  "10" = "Delaware",
  "11" = "Washington D.C.",
  "12" = "Florida",
  "13" = "Georgia",
  "15" = "Hawaii",
  "16" = "Idaho",
  "17" = "Illinois",
  "18" = "Indiana",
  "19" = "Iowa",
  "20" = "Kansas",
  "21" = "Kentucky",
  "22" = "Louisiana",
  "23" = "Maine",
  "24" = "Maryland",
  "25" = "Massachusetts",
  "26" = "Michigan",
  "27" = "Minnesota",
  "28" = "Mississippi",
  "29" = "Missouri",
  "30" = "Montana",
  "31" = "Nebraska",
  "32" = "Nevada",
  "33" = "New Hampshire",
  "34" = "New Jersey",
  "35" = "New Mexico",
  "36" = "New York",
  "37" = "North Carolina",
  "38" = "North Dakota",
  "39" = "Ohio",
  "40" = "Oklahoma",
  "41" = "Oregon",
  "42" = "Pennsylvania",
  "44" = "Rhode Island",
  "45" = "South Carolina",
  "46" = "South Dakota",
  "47" = "Tennessee",
  "48" = "Texas",
  "49" = "Utah",
  "50" = "Vermont",
  "51" = "Virginia",
  "53" = "Washington",
  "54" = "West Virginia",
  "55" = "Wisconsin",
  "56" = "Wyoming",
  "66" = "Guam",
  "72" = "Puerto Rico",
  "78" = "US Virgin Islands"
)

d_combined <- d_combined %>% mutate(state = state_dict[state])

#sex
d_combined <- d_combined %>% mutate(sex = case_match(sex, 1 ~ "male", 2 ~ "female"))

#race
d_combined <- d_combined %>% mutate(race = case_match(race, 1 ~ "White", 2 ~ "Black", 3 ~ "American Indian/Alaskan Native", 4 ~ "Asian", 5 ~ "Pacific Islander", 6 ~ "Other", 7 ~ "Multiracial", 8 ~ "Hispanic", 9 ~ NA))

#age
d_combined <- d_combined %>% mutate(age = case_match(age, 1 ~ "18-24", 2 ~ "25-29", 3 ~ "30-34", 4 ~ "35-39", 5 ~ "40-44", 6 ~ "45-49", 7 ~ "50-54", 8 ~ "55-59", 9 ~ "60-64", 10 ~ "65-69", 11 ~ "70-74", 12 ~ "75-79", 13 ~ "80+", 14 ~ NA))	


#FINAL DATA SET

#saving data file
write.csv(d_combined, "CleanData.csv", row.names=FALSE)

#summarizing NAs

summary(d_combined$state) #328 NAs
summary(d_combined$sex) #0 NAs
summary(d_combined$race) #154 NAs
summary(d_combined$age) #138 NAs
summary(d_combined$vaccine) #570 NAs
summary(d_combined$date) #0 NAs

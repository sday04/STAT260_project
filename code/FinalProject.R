#STAT260 FINAL GROUP PROJECT

library(foreign)
library(tidyverse)
library(haven)
library(fstcore)
library(fst)
library(ggplot2)
library(splines)
library(ggfun)
library(randomForest)
library(party)
library(tree)
library(lubridate)

###DATA CLEANING###

#reading data sets: randomly selecting 10000/year; selecting and renaming columns

set.seed(8)

#2024 data 

#only for first time:
#brfss2024<-read.xport("data/LLCP2024.XPT") 
#brfss2024<-write_fst(brfss2024, "data/brfss2024")
#read in faster after first time:
brfss2024<-read_fst("data/brfss2024") 
brfss2024_sample <- brfss2024[sample(nrow(brfss2024), 10000), ]
dim(brfss2024_sample) #10000   302

#2023 data 

#only for first time:
#brfss2023<-read.xport("data/LLCP2024.XPT") 
#brfss2023<-write_fst(brfss2023, "data/brfss2023")
#read in faster after first time:
brfss2023<-read_fst("data/brfss2023") #use this to read in faster after first time
brfss2023_sample <- brfss2023[sample(nrow(brfss2023), 10000), ]
dim(brfss2023_sample) # 10000   351

#2022 data 

#only for first time:
#brfss2022<-read.xport("data/LLCP2022.XPT") 
#brfss2022<-write_fst(brfss2022, "data/brfss2022")
#read in faster after first time:
brfss2022<-read_fst("data/brfss2022") #use this to read in faster after first time
brfss2022_sample <- brfss2022[sample(nrow(brfss2022), 10000), ]
dim(brfss2022_sample) # 10000   329

#2021 data 

#only for first time:
#brfss2021<-read.xport("data/LLCP2021.XPT") 
#brfss2021<-write_fst(brfss2021, "data/brfss2021")
#read in faster after first time:
brfss2021<-read_fst("data/brfss2021") #use this to read in faster after first time
brfss2021_sample <- brfss2021[sample(nrow(brfss2021), 10000), ]
dim(brfss2021_sample) #10000   304

#2020 data 

#only for first time:
#brfss2020<-read.xport("data/LLCP2020.XPT") 
#brfss2020<-write_fst(brfss2020, "data/brfss2020")
#read in faster after first time:
brfss2020<-read_fst("data/brfss2020") #use this to read in faster after first time
brfss2020_sample <- brfss2020[sample(nrow(brfss2020), 10000), ]
dim(brfss2020_sample) # 10000  280

#2019 data

#only for first time:
#brfss2019<-read.xport("data/LLCP2019.XPT") 
#brfss2019<-write_fst(brfss2019, "data/brfss2019")
#read in faster after first time:
brfss2019<-read_fst("data/brfss2019") #use this to read in faster after first time
brfss2019_sample <- brfss2019[sample(nrow(brfss2019), 10000), ]
dim(brfss2019_sample) # 10000   342

#2018 data 

#only for first time:
#brfss2018<-read.xport("data/LLCP2018.XPT") 
#brfss2018<-write_fst(brfss2018, "data/brfss2018")
#read in faster after first time:
brfss2018<-read_fst("data/brfss2018") #use this to read in faster after first time
brfss2018_sample <- brfss2018[sample(nrow(brfss2018), 10000), ]
dim(brfss2018_sample) # 10000   276


# exporting the yearly datasets
write.csv(brfss2018_sample, "shared_datasets/brfss2018_sample.csv")
write.csv(brfss2019_sample, "shared_datasets/brfss2019_sample.csv")
write.csv(brfss2020_sample, "shared_datasets/brfss2020_sample.csv")
write.csv(brfss2021_sample, "shared_datasets/brfss2021_sample.csv")
write.csv(brfss2022_sample, "shared_datasets/brfss2022_sample.csv")
write.csv(brfss2023_sample, "shared_datasets/brfss2023_sample.csv")
write.csv(brfss2024_sample, "shared_datasets/brfss2024_sample.csv")


#reading in sample data sets; selecting and renaming columns

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


#cleaning variables

#vaccine
d_combined <- d_combined %>% mutate(vaccine = case_match(vaccine, 1 ~ "yes", 2 ~ "no", 7 ~ "DK/R/M", 9 ~ "DK/R/M"))

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
d_combined <- d_combined %>% mutate(race = case_match(race, 1 ~ "White", 2 ~ "Black", 3 ~ "American Indian/Alaskan Native", 4 ~ "Asian", 5 ~ "Pacific Islander", 6 ~ "Other", 7 ~ "Multiracial", 8 ~ "Hispanic", 9 ~ "DK/R/M"))

#age
d_combined <- d_combined %>% mutate(age = case_match(age, 1 ~ "18-24", 2 ~ "25-29", 3 ~ "30-34", 4 ~ "35-39", 5 ~ "40-44", 6 ~ "45-49", 7 ~ "50-54", 8 ~ "55-59", 9 ~ "60-64", 10 ~ "65-69", 11 ~ "70-74", 12 ~ "75-79", 13 ~ "80+", 14 ~ "DK/R/M"))	

#adding region variable
d_combined<-d_combined %>% mutate(region = case_match(state, "Alabama"~"south", "Alaska"~"west", "Arizona"~"west", "Arkansas"~"south", "California"~"west", "Colorado"~"west", "Conneticut"~"northeast", "Delaware"~"northeast", "Florida"~"south", "Georgia"~"south", "Hawaii"~"west", "Idaho"~"west", "Illinois"~"midwest", "Indiana"~"midwest", "Iowa"~"midwest", "Kansas"~"midwest", "Kentucky"~"south", "Louisiana"~"south", "Maine"~"northeast", "Maryland"~"south", "Massachusetts"~"northeast", "Michigan"~"midwest", "Minnesota"~"midwest", "Mississippi"~"south", "Missouri"~"midwest", "Montana"~"west", "Nebraska"~"midwest", "Nevada"~"west", "New Hampshire"~"northeast","New Jersey"~"northeast", "New Mexico"~"west", "New York"~"northeast", "North Carolina"~"south", "North Dakota"~"midwest", "Ohio"~"midwest", "Oklahoma"~"south", "Oregon"~"west", "Pennsylvania"~"northeast", "Rhode Island"~"northeast", "South Carolina"~"south", "South Dakota"~"midwest", "Tennessee"~"south", "Texas"~"south", "Utah"~"west", "Vermont"~"northeast", "Virginia"~"south", "Washington"~"west", "West Virginia"~"south", "Wisconsin"~"midwest", "Wyoming"~"west", "Guam"~"territory", "Puerto Rico"~"territory", "US Virgin Islands"~"territory", "Washington D.C."~"south"))

#summarizing NAs
summary(d_combined$state) #3290 NAs
summary(d_combined$sex) #21 NAs
summary(d_combined$race) #1587 NAs
summary(d_combined$age) #1282 NAs
summary(d_combined$vaccine) #5087 NAs
summary(d_combined$date) #1 NAs

#dropping one row with no date
d_combined<- d_combined %>% drop_na(date)

#adding season indicator 
d_combined<- d_combined %>% mutate (date=as.Date(date), season = case_when(
					month(date) %in% c(12, 1, 2) ~ "winter", 
					month(date) %in% c(3, 4, 5) ~ "spring", 
					month(date) %in% c(6, 7, 8) ~ "summer", 
					month(date) %in% c(9, 10, 11) ~"fall"))
					
#converting all NAs to "DK/R/M"
d_combined<- d_combined %>% mutate(across(everything(), ~replace_na(.x, "DK/R/M")))


#final data set

dim(d_combined) # 69,999 rows x 8 columns

#saving data file
write.csv(d_combined, "CleanData.csv", row.names=FALSE)





###GRAPHICS###

d<-read.csv("CleanData.csv")
d<-as_tibble(d)

#vaccine rates
vaccine_counts<- table(d$vaccine)
vaccine_rates<- prop.table(vaccine_counts)
vaccine_rates

d <- CleanData
library(tidyverse)
library(ggplot2)
library(splines)
library(ggfun)

d<-as_tibble(d)

#Overall vaccination rate
d %>%
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(
    Vaccinated = mean(vaccine == "yes", na.rm = TRUE),
    `Non-Vaccinated` = mean(vaccine == "no", na.rm = TRUE),
    `Don't know/Refused` = mean(vaccine == "DK/R", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(month)) +
  geom_point(aes(y = Vaccinated, color = "Vaccinated")) +
  geom_xspline(aes(y = Vaccinated, color = "Vaccinated"), shape = -1) +
  geom_point(aes(y = `Non-Vaccinated`, color = "Non-Vaccinated")) +
  geom_xspline(aes(y = `Non-Vaccinated`, color = "Non-Vaccinated"), shape = -1) +
  geom_point(aes(y = `Don't know/Refused`, color = "Don't know/Refused")) +
  geom_xspline(aes(y = `Don't know/Refused`, color = "Don't know/Refused"), shape = -1) +
  scale_color_manual(
    values = c(
      "Vaccinated" = "green3",
      "Non-Vaccinated" = "red",
      "Don't know/Refused" = "blue"),
    breaks = c(
      "Vaccinated",
      "Non-Vaccinated",
      "Don't know/Refused")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Response",
       title = "U.S. flu vaccination rates 2018–2024")

#Vaccinate rate by sex
#Vaccinated
d %>%	mutate(date = as.Date(date)) %>%
  filter(sex != "DK/R/M") %>%
  group_by(sex, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = sex)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Sex",
       title = "Flu vaccination rates by sex 2018-2024")

#Non-vaccinated
d %>%	mutate(date = as.Date(date)) %>%
  filter(sex != "DK/R/M") %>%
  group_by(sex, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "no")) %>%
  ggplot(aes(month, rate, color = sex)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Sex",
       title = "Flu vaccination rates by sex 2018-2024")

#DK/R/M
d %>%	mutate(date = as.Date(date)) %>%
  filter(sex != "DK/R/M") %>%
  group_by(sex, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "DK/R/M")) %>%
  ggplot(aes(month, rate, color = sex)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Sex",
       title = "Flu DK/R/M rates by sex 2018-2024")

#Vaccination rate by race
#Vaccinated
d %>%	mutate(date = as.Date(date),
             race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other"),"Other", race)) %>%
  filter(race != "DK/R/M") %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n  within the past 12 months",
       color = "Race",
       title = "Flu vaccination rates by race 2018-2024")

#Non-vaccinated
d %>%	mutate(date = as.Date(date),
             race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other"),"Other", race)) %>%
  filter(race != "DK/R/M") %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "no")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Race",
       title = "Flu non-vaccination rates by race 2018-2024")

#DK/R/M
d %>%	mutate(date = as.Date(date),
             race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other"),"Other", race)) %>%
  filter(race != "DK/R/M") %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "DK/R/M")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Race",
       title = "Flu DK/R/M rates by race 2018-2024")

#By year
d %>%	mutate(date = as.Date(date),
             race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other", "DK/R/M"),"Other", race)) %>%
  filter(race != "DK/R/M") %>%
  group_by(race, year = floor_date(date, "year")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(year, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Race",
       title = "Flu vaccination rates by race 2018-2024")

# Vaccination rate by age
#Vaccinated
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(age != "DK/R/M") %>%
  group_by(age, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = age)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x= "Age group",
       y= "Percent of survey respondents\n within the past 12 months",
       color = "Age",
       title = "Flu vaccination rates by age 2018-2024")

#Non-vaccinated
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(age != "DK/R/M") %>%
  group_by(age, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "no", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = age)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x= "Age group",
       y= "Percent of survey respondents\n within the past 12 months",
       color = "Age",
       title = "Flu non-vaccination rates by age 2018-2024")

#DK/R/M
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(age != "DK/R/M") %>%
  group_by(age, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "DK/R/M", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = age)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x= "Age group",
       y= "Percent of survey respondents\n within the past 12 months",
       color = "Age",
       title = "Flu DK/R/M rates by age 2018-2024")

#Age barplot
d %>% 
  filter(age != "DK/R/M") %>%
  group_by(age) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=age,y=rate)) +
  geom_col() +
  ylim(c(0,1))+
  labs(x = "Age Group",
       y = "Percent of survey respondents",
       title = "Flu vaccination rates by age 2018-2024")

#Vaccination rate by region
#Vaccinated
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(region != "DK/R/M") %>%
  group_by(region, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = region)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey Month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Region of the U.S.",
       title = "Flu vaccination rates by region 2018-2024")

#Non-vaccinated
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(region != "DK/R/M") %>%
  group_by(region, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "no", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = region)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey Month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Region of the U.S.",
       title = "Flu non-vaccination rates by region 2018-2024")

#DK/R/M
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(region != "DK/R/M") %>%
  group_by(region, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "DK/R/M", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = region)) +
  geom_point() +
  geom_xspline(shape=-1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey Month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Region of the U.S.",
       title = "Flu DK/R/M rates by region 2018-2024")

#By year
d %>% 
  mutate(date = as.Date(date)) %>%
  filter(region != "DK/R/M") %>%
  group_by(region, year = floor_date(date, "year")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = rate, color = region)) +
  geom_point() +
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey Month",
       y = "Percent of survey respondents\n within the past 12 months",
       color = "Region of the U.S.",
       title = "Flu vaccination rates by region 2018-2024")

#Region barplot
d %>% 
  filter(region != "DK/R/M") %>%
  group_by(region) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=reorder(region,rate),y=rate)) +
  geom_col() +
  ylim(c(0,1))+
  labs(x = "Region",
       y = "Percent of survey respondents",
       title = "Flu vaccination rates by region 2018-2024")

#State and region barplot
d %>% 
  filter(region != "DK/R/M") %>%
  group_by(state, region) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=reorder(state,rate),y=rate, fill=region)) +
  geom_col() +
  ylim(c(0,1)) +
  labs(x = "State",
       y = "Percent of survey respondents",
       title = "Flu vaccination rates by region 2018-2024",
       fill = "Region") +
  guides(x = guide_axis(angle = 45))

#Vaccination rate by season
#Season barplot
d %>% 
  group_by(season) %>% 
  summarise(rate=mean(vaccine == "no", na.rm = TRUE)) %>% 
  ggplot(aes(x=season,y=rate)) +
  geom_col() +
  ylim(c(0,1))+
  labs(x = "Survey Season",
       y = "Percent of survey respondents",
       title = "Flu vaccination rates by season 2018-2024")

# Exploring Interactions: Age and Sex
view(d)

d %>% 
  group_by(sex, age) %>% 
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = rate, fill = sex)) +
  geom_col(position = "dodge") +
  ylim(c(0,1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(
    x = "Age of Respondent",
    y = "% survey respondents vaccinated \nwithin the last 12 months",
    title = "Flu vaccination rates 2018-2024 by age and sex"
  )
d %>% 
  group_by(sex, age) %>% 
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(age, rate, color = sex, group = sex)) +
  geom_line()+
  geom_point() +
  ylim(c(0,1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(
    x = "Age of Respondent",
    y = "% survey respondents vaccinated \nwithin the last 12 months",
    title = "Flu vaccination rates 2018-2024 by age and sex"
  )


# Exploring Interactions: Age and Race
view(d)

d %>% 
  mutate(date = as.Date(date),
         race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other"),"Other", race)) %>% 
  group_by(race, age) %>% 
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(age, rate, color = race, group = race)) +
  geom_line()+
  geom_point() +
  ylim(c(0,1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(
    x = "Age of Respondent",
    y = "% survey respondents vaccinated \nwithin the last 12 months",
    title = "Flu vaccination rates 2018-2024 by age and race"
  )

    
    
    
    
###MODELS###

#random forest 
set.seed(1)

d <- d %>%
  mutate(across(where(is.character), as.factor))

rFout<-randomForest(vaccine~state + sex + race + age + region + season, data=d,
                    ntree=600, importance=TRUE)
rFout$importance 

#tree
d$vaccine<-as.factor(d$vaccine)
d$sex<-as.factor(d$sex)
d$age<-as.factor(d$age)
d$race<-as.factor(d$race)
d$region<-as.factor(d$region)
d$date<-as.factor(d$date)
d$season<-as.factor(d$season)

tree<-ctree(vaccine~sex+age+race+state+region+season, data=d, control = ctree_control(maxdepth = 3))
plot(tree)

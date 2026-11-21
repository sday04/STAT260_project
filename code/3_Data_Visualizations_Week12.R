View(CleanData)
library(tidyverse)
library(ggplot2)
library(splines)
library(ggfun)

d<-read.csv("CleanData.csv")
d<-as_tibble(d)
d$date <- as.Date(d$date)

#Vaccine rates
d %>%
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>%
  ggplot(aes(month, rate)) +
  geom_point(color = "palevioletred4") +
  geom_xspline(shape=-0.4, color = "palevioletred4")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates 2018-2024")

#Vaccine rates by sex
d %>%	mutate(date = as.Date(date)) %>%
  group_by(sex, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = sex)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by sex 2018-2024")

#Vaccine rates by race
#All race categories
d %>%	mutate(date = as.Date(date)) %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by race 2018-2024")

#No "other" or "pacific islander"
d %>%	mutate(date = as.Date(date)) %>% filter(race %in% c("American Indian/Alaskan Native", "Asian", "Black", "Hispanic", "Multiracial", "White")) %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by race 2018-2024")

#"American Indian/Alaskan Native," "Multiracial", "Pacific Islander" all coded as "Other"
d %>%	mutate(date = as.Date(date),
             race = ifelse(race %in% c("American Indian/Alaskan Native", "Multiracial", "Pacific Islander", "Other"),"Other", race)) %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by race 2018-2024")

#Vaccine rates by region and state
table(d$region)
d$region[d$region == "Northeast"] <- "northeast"

d %>% 
  group_by(region) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=reorder(region,rate),y=rate)) +
  geom_col(fill="lightblue") +
  ylim(c(0,1))+
  labs(x = "Region",
       y = "Overall Vaccination Rate",
       title = "U.S. flu vaccination rates by region 2018-2024")

d %>% 
  group_by(state, region) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=reorder(state,rate),y=rate, fill=region)) +
  geom_col() +
  ylim(c(0,1)) +
  labs(x = "State",
       y = "Overall Vaccination Rate",
       title = "U.S. flu vaccination rates by state 2018-2024",
       fill = "Region") +
  guides(x = guide_axis(angle = 45))

d %>% 
  mutate(date = as.Date(date)) %>%
  group_by(region, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = rate, color = region)) +
  geom_point() +
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey Month",
       y = "Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by region 2018-2024",
       color = "Region of United States")

# Vaccine rates by age
d %>% 
  group_by(age) %>% 
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x=reorder(age,rate),y=rate)) +
  geom_col(fill="thistle4") +
  ylim(c(0,1))+
  labs(x = "Age Group",
       y = "Percent of survey respondents vaccinated between 2018 and 2024",
       title = "U.S. flu vaccination rates by age groups 2018-2024")

d %>% 
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month"), age) %>%
  summarise(rate=mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(month, rate, color = age)) +
  ylim(c(0,1)) +
  geom_point() +
  geom_xspline(shape=-0.4) + 
  labs(x="Survey Month",
       y="Percent of survey respondents vaccinated within the last 12 months",
       title = "U.S. flu vaccination rates by age groups 2018-2024")

# Running random forest to see which predictors are important
library(randomForest)
set.seed(1)

d <- d %>%
  mutate(across(where(is.character), as.factor))

rFout<-randomForest(vaccine~state + sex + race + age + region, data=d,
                    ntree=600, importance=TRUE)
rFout$importance # age is most important predictor

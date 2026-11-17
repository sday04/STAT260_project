View(CleanData)
library(tidyverse)
library(ggplot2)
library(splines)
library(ggfun)

#GRAPHICS

d<-read.csv("CleanData.csv")
d<-as_tibble(d)

#vaccine rates by sex
 d %>%	mutate(date = as.Date(date)) %>%
  group_by(sex, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = sex)) +
  geom_point()+
  geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months", title = "Flu vaccination rates by sex 2018-2024")

#vaccine rates by race

#all race categories
 d %>%	mutate(date = as.Date(date)) %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
   geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months", title = "Flu vaccination rates by race 2018-2024")

#no "other" or "pacific islander"
 d %>%	mutate(date = as.Date(date)) %>% filter(race %in% c("American Indian/Alaskan Native", "Asian", "Black", "Hispanic", "Multiracial", "White")) %>%
  group_by(race, month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes")) %>%
  ggplot(aes(month, rate, color = race)) +
  geom_point()+
   geom_xspline(shape=-0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents vaccinated within the last 12 months", title = "Flu vaccination rates by race 2018-2024")

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
       y = "Percent of survey respondents\n vaccinated within the last 12 months", title = "Flu vaccination rates by race 2018-2024")

#vaccine rates all
d %>%
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>%
  ggplot(aes(month, rate)) +
  geom_point(color = "palevioletred4") +
  geom_line(color = "palevioletred4") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n vaccinated within the last 12 months")

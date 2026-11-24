library(tidyverse)
library(ggplot2)
library(splines)
library(ggfun)

d<-read.csv("CleanData.csv")
d<-as_tibble(d)
d$date <- as.Date(d$date)

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

table(d$race)
  
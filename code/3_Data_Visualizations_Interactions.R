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
  group_by(race, age) %>% 
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = rate, fill = race)) +
  geom_col(position = "dodge", color="black", linewidth = 0.2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(
    x = "Age of Respondent",
    y = "% survey respondents vaccinated \nwithin the last 12 months",
    title = "Flu vaccination rates 2018-2024 by age and race"
  )

  
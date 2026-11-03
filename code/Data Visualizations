dim(CleanData)
head(CleanData)
library(tidyverse)

CleanData %>%
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>%
  ggplot(aes(month, rate)) +
  geom_col(fill = "pink4") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Survey month",
       y = "Percent of survey respondents\n vaccinated within the last 12 months")

CleanData %>%
  filter(!is.na(race), !is.na(sex)) %>%
  group_by(race, sex) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(race, rate), y = rate, fill = sex)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Vaccination rate by race and sex between 2018 and 2024",
       x = "Race", y = "Vaccination rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("female" = "palevioletred", "male" = "steelblue"))

CleanData %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  summarise(rate = mean(vaccine == "yes", na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, rate), y = rate)) +
  geom_col(fill = "salmon4") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Vaccination rate by state between 2018 and 2024",
    x = "State", y = "Vaccination Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

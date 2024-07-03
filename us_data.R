## preparing global environment
install.packages('tidyverse')
library(tidyverse)
library(lubridate)
## installing cleaning packages
install.packages("tidyr")
library(tidyr)
install.packages("skimr")
library(skimr)
install.packages("here")
library(here)
install.packages("janitor")
library(janitor)
## import data
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
## install ggplot for vizuals
install.packages("ggplot2")
library(ggplot2)
## view data 
head("owd.covid.data")
str('owde.covid.data')
summary("owds.covid.data")
## Since this is my first project, I want to work with the US data only
## creating a dataframe for the US stats only
## found out that datframe is loaded incorrectly without headers
## reloaded with headers
print(names(owid.covid.data))
## making sure my locations exist
if ("location" %in% names(owid.covid.data)) {
  us_data <- owid.covid.data %>% filter(location == "United States")
  head(us_data)
} else {
  print("The 'location' column is missing.")
}
## now filtering for US data
us_data <- owid.covid.data %>% 
      filter(location == "United States")
## view dataset
View(us_data) 
## Next I want to find total number of repoprted cases
latest_date <- max(us_data$date, na.rm = TRUE)
total_cases_latest_date <- max(us_data$total_cases[us_data$date == latest_date], na.rm = TRUE)
print(paste("Total cases as of", latest_date, ":", total_cases_latest_date))

## To compare the data I am creating three different line graphs for death over time, vaccines over time and hospitalizations over time
library(ggplot2)

# Plotting COVID-19 deaths over time with a specific line color
ggplot(us_data, aes(x = date, y = total_deaths)) +
  geom_line(color = "red") +
  labs(title = "COVID-19 Deaths Over Time in the US",
       x = "Date",
       y = "Total Deaths") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2020-01-01", "2024-12-31"))) +
  theme_minimal() +
  annotate("text", x = as.Date("2020-01-05"), y = Inf, label = "Start: 2020-01-05", vjust = -2, color = "purple") +
  annotate("text", x = as.Date("2024-06-16"), y = Inf, label = "End: 2024-06-16", vjust = -2, color = "purple")
## having errors with date format, checking to see format
print(class(us_data$date))
## It was character 
## Formating date
us_data$date <- as.Date(us_data$date, format = "%Y-%m-%d")
print(class(us_data$date))
## Rerun ggplot for Deaths over time
ggplot(us_data, aes(x = date, y = total_deaths)) +
  geom_line(color = "red") +
  labs(title = "COVID-19 Deaths Over Time in the US",
       x = "Date",
       y = "Total Deaths") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2020-01-01", "2024-12-31"))) +
  theme_minimal() +
  annotate("text", x = as.Date("2020-01-05"), y = Inf, label = "Start: 2020-01-05", vjust = -2, color = "purple") +
  annotate("text", x = as.Date("2024-06-16"), y = Inf, label = "End: 2024-06-16", vjust = -2, color = "purple")
## now I want to do vaccines 
ggplot(us_data, aes(x = date, y = people_vaccinated)) +
  geom_line(color = "green") +
  labs(title = "COVID-19 Vaccines Over Time in the US",
       x = "Date",
       y = "Total Vaccines") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2020-01-01", "2024-12-31"))) +
  theme_minimal() +
  annotate("text", x = as.Date("2020-01-05"), y = Inf, label = "Start: 2020-01-05", vjust = -2, color = "purple") +
  annotate("text", x = as.Date("2024-06-16"), y = Inf, label = "End: 2024-06-16", vjust = -2, color = "purple")
## looking for correlation of benefits from vaccines, now will do hospitalizations
ggplot(us_data, aes(x = date, y = hosp_patients)) +
  geom_line(color = "blue") +
  labs(title = "COVID-19 Hospitalizations Over Time in the US",
       x = "Date",
       y = "Total Hospitalizations") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2020-01-01", "2024-12-31"))) +
  theme_minimal() +
  annotate("text", x = as.Date("2020-01-05"), y = Inf, label = "Start: 2020-01-05", vjust = -2, color = "purple") +
  annotate("text", x = as.Date("2024-06-16"), y = Inf, label = "End: 2024-06-16", vjust = -2, color = "purple")
## I want to see if their is a correlation between hospitalizations, and deaths before and after vaccines, I am going to calculate
## starting with weekly changes
##loading dplyr
library(dplyr)
correlation_matrix <- cor(us_data[, c("weekly_deaths", "weekly_hosp", "weekly_vaccinations")], use = "complete.obs")
## cant compute because one varibles standard deviation is 0 have to look at another way
## checking for constant values
summary(us_data$weekly_deaths) # deaths had a standard deviation of 0
summary(us_data$weekly_hosp)
summary(us_data$weekly_vaccinations)
## Trying a different way
## creating weekly data set for correlation
weekly_data <- us_data %>%
  mutate(week = floor_date(date, unit = "week")) %>%  # Round down date to the nearest week
  group_by(week) %>%
  summarize(
    weekly_deaths = sum(total_deaths, na.rm = TRUE),
    weekly_hosp = sum(hosp_patients, na.rm = TRUE),
    weekly_vaccinations = sum(total_vaccinations, na.rm = TRUE),
    .groups = 'drop'
  )
## vizualize weekly data
ggplot(weekly_data, aes(x = week))+
  geom_line(aes(y = weekly_deaths, color = "deaths"))+
  geom_line(aes(y = weekly_hosp, color = "Hospitalizations"))+
  geom_line(aes(y = weekly_vaccinations, color = "Vaccinations"))+
  scale_color_manual(values = c("Deaths" = "red", "Hopitalizations" = "blue", "Vaccinations" = "green"))+
  labs(title = "Weekly COVID-19 Deaths, Hoispitalizations, and Vaccinations",
       x = "Week",
       y = "Count",
       color = "Indicator")+
  theme_minimal()
## trying another vizualization
install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45) 
## creating a summary statistic for the weekly data
summary_stats <- summary(weekly_data)
print(summary_stats)
## Calculate correlation_matrix
correlation_matrix <- cor(weekly_data[, c("weekly_deaths", "weekly_hosp", "weekly_vaccinations")], use = "complete.obs")
print(correlation_matrix)
## results are in cunclusive more models are needed and futhrer exploration


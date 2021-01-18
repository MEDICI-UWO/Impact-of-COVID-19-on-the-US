##############################################################################################################################
# Author: EYSC
# Date Modified: August 6, 2020
# 
# Estimate the mid-year population for the USA and its states for the year 2020 for each given age cohort
##############################################################################################################################

library(dplyr)
library(tidyr)

# Read in data supplied by CDC Wonder
data <- read.csv('Data/mid-year_pop_1990-2019.csv', stringsAsFactors = FALSE)

# Sum male and female population together to get total population (for each age cohort)
temp <- data %>%
  group_by(State, Age.Group, Yearly.July.1st.Estimates) %>%
  summarise(
    Population = sum(Population)
  ) %>%
  ungroup() %>%
  mutate(
    Gender = 'Total'
  ) %>%
  select(Age.Group, State, Gender, Yearly.July.1st.Estimates, Population)

# Add total population data frame to existing data frame (which only contains male and female population)
data <- rbind(data, temp) %>%
  select(State, Age.Group, Gender, Yearly.July.1st.Estimates, Population) %>%
  rename(
    Year = Yearly.July.1st.Estimates
  ) %>%
  arrange(State, Age.Group, Gender, Year)

rm(temp)

# Rearrange data frame to regroup age cohort (from intervals of 5 years to intervals of 10 years)
data.spread <- spread(data, Age.Group, Population)

data.10.year.cohort <- data.spread %>%
  select(State, Gender, Year, '< 1 year', '1-4 years', '85+ years')
data.10.year.cohort[, '5-14 years'] <- data.spread$`5-9 years` + data.spread$`10-14 years`
data.10.year.cohort[, '15-24 years'] <- data.spread$`15-19 years` + data.spread$`20-24 years`
data.10.year.cohort[, '25-34 years'] <- data.spread$`25-29 years` + data.spread$`30-34 years`
data.10.year.cohort[, '35-44 years'] <- data.spread$`35-39 years` + data.spread$`40-44 years`
data.10.year.cohort[, '45-54 years'] <- data.spread$`45-49 years` + data.spread$`50-54 years`
data.10.year.cohort[, '55-64 years'] <- data.spread$`55-59 years` + data.spread$`60-64 years `
data.10.year.cohort[, '65-74 years'] <- data.spread$`65-69 years` + data.spread$`70-74 years`
data.10.year.cohort[, '75-84 years'] <- data.spread$`75-79 years` + data.spread$`80-84 years`

data.10.year.cohort <- gather(data.10.year.cohort, "Age.Cohort", "Population", 4:14)

data.10.year.cohort <- data.10.year.cohort %>%
  select(State, Age.Cohort, Gender, Year, Population) %>%
  arrange(State, Age.Cohort, Gender, Year)

rm(data)
rm(data.spread)

predict.df <- data.frame(Year = 2020)

states <- unique(data.10.year.cohort$State)
genders <- unique(data.10.year.cohort$Gender)
age.cohorts <- unique(data.10.year.cohort$Age.Cohort)
age.cohorts <- age.cohorts[c(1, 2, 7, 3, 4, 5, 6, 8, 9, 10, 11)]

# Predict population for the USA and for each state for each age cohort (10 year interval) using a cubic regression
pop.matrix <- NULL
for (state in states) {
  for (gender in genders) {
    for (age in age.cohorts) {
      df <- data.10.year.cohort %>%
        filter(
          State == state,
          Gender == gender,
          Age.Cohort == age
        )
      reg <- lm(Population~poly(Year, 3), data = df)
      p <- predict(reg, predict.df, interval = 'prediction')
      pop.matrix <- rbind(pop.matrix, c(state, gender, age, p))
    }
  }
}
colnames(pop.matrix) <- c('State', 'Gender', 'Age.Cohort', 'fit', 'lwr', 'upr')

pop.est.df <- as.data.frame(pop.matrix, stringsAsFactors = FALSE)

write.csv(pop.est.df, 'Data/mid_year_pop_est.csv', row.names = FALSE)

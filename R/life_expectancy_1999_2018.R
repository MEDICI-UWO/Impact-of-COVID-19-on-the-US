##############################################################################################################################
# Author: EYSC
# Date Modified: January 13, 2021
# 
# Compute life expectancy at birth form 1999-2019 for the USA and states using abridged life table
##############################################################################################################################

library(dplyr)
library(tidyr)

# Read in source file which contains functions that calculate life expectancy
source('R/life_table_functions.R')

# Read number of deaths data (from 1999-2019), sourced from CDC Wonder
deaths <- read.csv('Data/Deaths per State (filled).csv')
deaths <- deaths %>%
  rename(
    Age.Cohort = Ten.Year.Age.Groups
  ) %>%
  select(State, Gender, Year, Age.Cohort, Deaths)
deaths.usa <- read.csv('Data/USA Deaths.csv')
deaths.usa <- deaths.usa %>%
  rename(
    Age.Cohort = Ten.Year.Age.Groups
  ) %>%
  filter(Age.Cohort != 'Not Stated') %>%
  mutate(
    State = 'United States'
  ) %>%
  select(State, Gender, Year, Age.Cohort, Deaths)
deaths.usa.total <- deaths.usa %>%
  filter(Age.Cohort != 'Not Stated') %>%
  group_by(Year, Age.Cohort) %>%
  summarise(
    Deaths = sum(Deaths)
  ) %>%
  mutate(
    State = 'United States',
    Gender = 'Total'
  )
deaths.usa <- rbind(deaths.usa, deaths.usa.total)

deaths <- rbind(deaths, deaths.usa)

age.group <- unique(deaths$Age.Cohort)
states <- unique(deaths$State)
gender <- unique(deaths$Gender)
years <- unique(deaths$Year)

# Read in population data (source: CDC Wonder)
population <- read.csv('Data/mid-year_pop_1990-2019.csv', stringsAsFactors = FALSE)
population <- population %>%
  rename(
    Age.Cohort = Age.Group,
    Year = Yearly.July.1st.Estimates
  ) 

# Reorganize age cohorts so that it is separated by 10 year intervals rather than 5
pop.spread <- spread(population, Age.Cohort, Population)
pop.10.year <- pop.spread %>%
  select(State, Gender, Year, '< 1 year', '1-4 years', '85+ years')
pop.10.year[, '5-14 years'] <- pop.spread$`5-9 years` + pop.spread$`10-14 years`
pop.10.year[, '15-24 years'] <- pop.spread$`15-19 years` + pop.spread$`20-24 years`
pop.10.year[, '25-34 years'] <- pop.spread$`25-29 years` + pop.spread$`30-34 years`
pop.10.year[, '35-44 years'] <- pop.spread$`35-39 years` + pop.spread$`40-44 years`
pop.10.year[, '45-54 years'] <- pop.spread$`45-49 years` + pop.spread$`50-54 years`
pop.10.year[, '55-64 years'] <- pop.spread$`55-59 years` + pop.spread$`60-64 years `
pop.10.year[, '65-74 years'] <- pop.spread$`65-69 years` + pop.spread$`70-74 years`
pop.10.year[, '75-84 years'] <- pop.spread$`75-79 years` + pop.spread$`80-84 years`

population <- gather(pop.10.year, "Age.Cohort", "Population", 4:14)

temp <- population %>%
  group_by(State, Age.Cohort, Year) %>%
  summarise(
    Population = sum(Population)
  ) %>%
  ungroup() %>%
  mutate(
    Gender = 'Total'
  ) %>%
  select(State, Gender, Year, Age.Cohort, Population)

population <- rbind(population, temp)

n <- c(1, 4, rep(10, 8), 10000)

# calculate life expectancy for each state for total population and for each gender separately
for (s in states) {
  life.expectancy <- NULL
  for (y in years) {
    LE.vec <- rep(0, 4)
    names(LE.vec) <- c('Year', 'Total', 'Male', 'Female')
    LE.vec['Year'] <- y
    for (g in gender) {
      d.df <- deaths %>%
        filter(
          State == s,
          Year == y,
          Gender == g
        )
      d.df <- d.df[match(age.group, d.df$Age.Cohort),]
      pop.df <- population %>%
        filter(
          State == s,
          Year == y,
          Gender == g
        )
      pop.df <- pop.df[match(age.group, pop.df$Age.Cohort),]
      LE.vec[g] <- life.table(d.df$Deaths, pop.df$Population, n)[1, 'Years.remaining']
    }
    life.expectancy <- rbind(life.expectancy, LE.vec)
  }
  row.names(life.expectancy) <- NULL
  LE.df <- as.data.frame(life.expectancy, stringsAsFactors = FALSE)
  write.csv(LE.df, paste0('Data/Life Expectancy (1999-2019)/', s, '.csv'), row.names = FALSE)
}


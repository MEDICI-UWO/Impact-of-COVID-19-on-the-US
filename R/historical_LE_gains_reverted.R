##############################################################################################################################
# Author: EYSC
# Date Modified: January 13, 2021
# 
# Calculate number of years reverted in life expectancy gains (using raw life expectancy results)
##############################################################################################################################

library(dplyr)

# Read in life expectancies at birth (calculated using the current number of COVID-19 deaths)
current.LE <- read.csv('Data/Life Expectancy (Direct and Indirect).csv')
current.LE <- current.LE %>%
  filter(Age.Cohort == '< 1 year') %>%
  select(-Age.Cohort)

states <- unique(current.LE$State)
genders <- unique(current.LE$Gender)

# Loop through each state
reverted.year <- NULL
for (s in states) {
  past.LE <- read.csv(paste0('Data/Life Expectancy (1999-2019)/', s, '.csv'), stringsAsFactors = FALSE)
  df <- current.LE %>%
    filter(State == s)
  state.le.df <- NULL
  for (i in 1:nrow(df)) {
    g <- df[i, 'Gender']
    f <- df[i, 'Fit.Bound']
    le <- df[i, 'LE']
    year <- max(past.LE$Year[past.LE[, g] < le], na.rm = TRUE)
    if (year == '-Inf') {
      year <- '<1999'
    }
    state.le.df <- rbind(state.le.df, data.frame(State = s, Gender = g, Fit.Bound = f, Year = year))
  }
  reverted.year <- rbind(reverted.year, state.le.df)
}

# Format for manuscript
reverted.year.total <- reverted.year %>%
  filter(Gender == 'Total') %>%
  select(-Gender) %>%
  spread(Fit.Bound, Year)
names(reverted.year.total) <- c('State', 'total.fit', 'total.lwr', 'total.upr')
reverted.year.male <- reverted.year %>%
  filter(Gender == 'Male') %>%
  select(-Gender) %>%
  spread(Fit.Bound, Year)
names(reverted.year.male) <- c('State', 'male.fit', 'male.lwr', 'male.upr')
reverted.year.female <- reverted.year %>%
  filter(Gender == 'Female') %>%
  select(-Gender) %>%
  spread(Fit.Bound, Year)
names(reverted.year.female) <- c('State', 'female.fit', 'female.lwr', 'female.upr')

reverted.year.table <- merge(reverted.year.total, reverted.year.male, by = 'State')
reverted.year.table <- merge(reverted.year.table, reverted.year.female, by = 'State')

write.csv(reverted.year.table, 'Tables/Life Expectancy (Year Reverted).csv', row.names = FALSE)

historical.gains.reverted <- reverted.year.table
historical.gains.reverted[historical.gains.reverted == '<1999'] <- NA
historical.gains.reverted[, 2:ncol(historical.gains.reverted)] <- sapply(historical.gains.reverted[, 2:ncol(historical.gains.reverted)], as.numeric)
historical.gains.reverted[, 2:ncol(historical.gains.reverted)] <- 2020 - historical.gains.reverted[, 2:ncol(historical.gains.reverted)]
historical.gains.reverted[is.na(historical.gains.reverted)] <- '>21'

write.csv(historical.gains.reverted, 'Tables/Years Lost in Historical LE Gains.csv', row.names = FALSE)

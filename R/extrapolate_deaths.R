library(dplyr)

state.deaths <- read.csv('Data/Deaths per State (filled).csv', stringsAsFactors = FALSE)

age.cohort <- unique(state.deaths$Ten.Year.Age.Groups)
gender <- unique(state.deaths$Gender)

usa.deaths <- read.csv('Data/USA Deaths.csv', stringsAsFactors = FALSE)
usa.deaths <- usa.deaths %>%
  filter(Ten.Year.Age.Groups != 'Not Stated') %>%
  select(-c(Population, Crude.Rate)) %>%
  mutate(State = 'United States')
usa.total <- usa.deaths %>%
  group_by(Year, Ten.Year.Age.Groups) %>%
  summarise(
    Deaths = sum(Deaths)
  ) %>%
  mutate(
    Gender = 'Total',
    State = 'United States'
  )

deaths <- rbind(state.deaths, usa.deaths, usa.total)
states <- unique(deaths$State)

predict.year <- data.frame(Year = c(2020))

death.predict <- NULL
for (s in states) {
  for (g in gender) {
    for (a in age.cohort) {
      temp <- deaths %>%
        filter(
          State == s,
          Gender == g,
          Ten.Year.Age.Groups == a
        )
      r <- lm(Deaths~Year, data = temp)
      p <- predict(r, predict.year, interval = 'predict')
      death.predict <- rbind(death.predict, c(s, g, a, p))
    }
  }
}

death.predict <- as.data.frame(death.predict, stringsAsFactors = FALSE)
names(death.predict) <- c('State', 'Gender', 'Age.Cohort', 'fit', 'lwr', 'upr')
death.predict[, c('fit', 'lwr', 'upr')][death.predict[, c('fit', 'lwr', 'upr')] < 0] <- 0.01

write.csv(death.predict, 'Data/extrapolated_deaths_2020.csv', row.names = FALSE)

library(dplyr)
library(tidyr)
source('R/life_table_functions.R')

est.deaths <- read.csv('Data/extrapolated_deaths_2020.csv')
pop <- read.csv('Data/mid_year_pop_est.csv')
cdc.deaths <- read.csv('Data/Provisional_Deaths_filled.csv')

states <- unique(cdc.deaths$State)
age.cohort <- unique(cdc.deaths$Age.Cohort)
gender <- unique(cdc.deaths$Gender)

n <- n <- c(1, 4, rep(10, 8), 10000)

xs.mortality <- merge(cdc.deaths, est.deaths, by = c('State', 'Gender', 'Age.Cohort'))
xs.mortality <- xs.mortality %>%
  select(-COVID.19.Deaths) %>%
  mutate(
    fit = Total.Deaths - fit,
    lwr = Total.Deaths - lwr,
    upr = Total.Deaths - upr
  ) %>%
  select(-Total.Deaths)
xs.mortality[, 4:6][xs.mortality[, 4:6] < 0] <- 0

xs.mortality.gather <- xs.mortality %>%
  gather(Fit.Bound, XS.Mortality, fit, lwr, upr)

pop.gather <- pop %>%
  gather(Fit.Bound, Population, fit, lwr, upr)

pop.xs.mort <- merge(pop.gather, xs.mortality.gather, by = c('State', 'Gender', 'Age.Cohort', 'Fit.Bound'))
pop.xs.mort <- pop.xs.mort %>%
  mutate(
    pop.xs.mort = round(Population - XS.Mortality/2)
  ) %>%
  select(-c(Population, XS.Mortality))
pop.xs.mort <- pop.xs.mort %>%
  spread(Fit.Bound, pop.xs.mort)

cdc.death.total <- cdc.deaths %>%
  select(-COVID.19.Deaths) %>%
  rename(
    Deaths = Total.Deaths
  )

cdc.death.non.covid <- cdc.deaths %>%
  mutate(
    Deaths = Total.Deaths - COVID.19.Deaths
  ) %>%
  select(-c(COVID.19.Deaths, Total.Deaths))

total.LE <- NULL
for (s in states) {
  temp.pop <- pop.xs.mort %>%
    filter(State == s)
  temp.death <- cdc.death.total %>%
    filter(State == s)
  lt <- cdc.le.summary(temp.death, temp.pop, n, age.cohort)
  LE <- data.frame(State = rep(s, nrow(lt)), lt)
  row.names(LE) <- NULL
  total.LE <- rbind(total.LE, LE)
}
write.csv(total.LE, 'Data/Life Expectancy (Direct and Indirect).csv', row.names = FALSE, na = '')

indirect.LE <- NULL
for (s in states) {
  temp.pop <- pop %>%
    filter(State == s)
  temp.death <- cdc.death.non.covid %>%
    filter(State == s)
  lt <- cdc.le.summary(temp.death, temp.pop, n, age.cohort)
  LE <- data.frame(State = rep(s, nrow(lt)), lt)
  row.names(LE) <- NULL
  indirect.LE <- rbind(indirect.LE, LE)
}
write.csv(indirect.LE, 'Data/Life Expectancy (Indirect).csv', row.names = FALSE)

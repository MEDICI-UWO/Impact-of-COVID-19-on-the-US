library(dplyr)

source('R/life_table_functions.R')

mid.year.pop <- read.csv('Data/mid_year_pop_est.csv')
deaths <- read.csv('Data/extrapolated_deaths_2020.csv')

states <- unique(mid.year.pop$State)
age.cohort <- unique(mid.year.pop$Age.Cohort)
gender <- unique(mid.year.pop$Gender)

n <- n <- c(1, 4, rep(10, 8), 10000)

est.LE <- NULL
for (s in states) {
  temp.pop <- mid.year.pop %>%
    filter(State == s)
  temp.death <- deaths %>%
    filter(State == s)
  lt <- le.summary(temp.death, temp.pop, n, age.cohort)
  LE <- data.frame(State = rep(s, nrow(lt)), lt)
  est.LE <- rbind(est.LE, LE)
}

# est.LE <- est.LE %>%
#   spread(Fit.Bound, LE)

est.LE$Age.Cohort <- factor(est.LE$Age.Cohort, levels = age.cohort)

est.LE <- est.LE %>%
  arrange(State, Gender, Age.Cohort)

# est.LE.birth <- est.LE %>%
#   filter(Age.Cohort == '< 1 year') %>%
#   select(-Age.Cohort)

write.csv(est.LE, 'Data/Life Expectancy Estimate.csv', row.names = FALSE)
# write.csv(est.LE.birth, 'Data/Life Expectancy Estimate (birth).csv', row.names = FALSE)

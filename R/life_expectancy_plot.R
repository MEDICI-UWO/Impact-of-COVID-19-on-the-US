library(ggplot2)
library(dplyr)
library(tidyr)

LE.est <- read.csv('Data/Life Expectancy Estimate.csv')
LE.total <- read.csv('Data/Life Expectancy (Direct and Indirect).csv')
LE.indirect <- read.csv('Data/Life Expectancy (Indirect).csv')

LE.est.birth <- LE.est %>%
  filter(Age.Cohort == '< 1 year') %>%
  select(-Age.Cohort) %>%
  spread(Fit.Bound, LE)

LE.total.birth <- LE.total %>%
  filter(Age.Cohort == '< 1 year') %>%
  select(-Age.Cohort) %>%
  spread(Fit.Bound, LE)

ggplot(LE.est.birth, aes(x = State, y = fit, color = Gender)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Reference Life Expectancy at Birth in the USA', y = 'Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

ggplot(LE.total.birth, aes(x = State, y = fit, color = Gender)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Approximate Life Expectancy at Birth in the USA in 2020', y = 'Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

LE.diff <- merge(LE.est, LE.total, by = c('State', 'Gender', 'Age.Cohort', 'Fit.Bound'))
LE.diff <- LE.diff %>%
  rename(
    LE.est = LE.x,
    LE.direct.indirect = LE.y
  )
LE.diff <- merge(LE.diff, LE.indirect, by = c('State', 'Gender', 'Age.Cohort', 'Fit.Bound'))
LE.diff <- LE.diff %>%
  rename(
    LE.indirect = LE
  )
LE.diff <- LE.diff %>%
  mutate(
    `Direct Effects` = LE.indirect - LE.direct.indirect,
    `Indirect Effects` = LE.est - LE.indirect,
    `Net Effects` = LE.est - LE.direct.indirect
  )
LE.diff <- LE.diff %>%
  select(-c(LE.est, LE.direct.indirect, LE.indirect))
LE.diff <- LE.diff %>%
  gather(Effect, LE.Diff, `Direct Effects`, `Indirect Effects`, `Net Effects`)
# LE.diff <- LE.diff %>%
#   spread(Fit.Bound, LE.Diff)

LE.diff.birth <- LE.diff %>%
  filter(Age.Cohort == '< 1 year', Effect == 'Net Effects') %>%
  select(-Age.Cohort) %>%
  spread(Fit.Bound, LE.Diff)

ggplot(LE.diff.birth, aes(x = State, y = fit, color = Gender)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Years in Life Expectancy at Birth Decreased by Gender in the USA in 2020', y = 'Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Total population
LE.diff.total <- LE.diff %>%
  filter(Age.Cohort == '< 1 year', Gender == 'Total') %>%
  spread(Fit.Bound, LE.Diff)
ggplot(LE.diff.total, aes(x = State, y = fit, color = Effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Decrease in Life Expectancy at Birth due to Direct vs Indirect Effects of COVID-19 \nin the USA in 2020 (Total Population)', y = 'Decrease in Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Female population
LE.diff.female <- LE.diff %>%
  filter(Age.Cohort == '< 1 year', Gender == 'Female') %>%
  spread(Fit.Bound, LE.Diff)
ggplot(LE.diff.female, aes(x = State, y = fit, color = Effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Decrease in Life Expectancy at Birth due to Direct vs Indirect Effects of COVID-19 \nin the USA in 2020 (Female Population)', y = 'Decrease in Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Male population
LE.diff.male <- LE.diff %>%
  filter(Age.Cohort == '< 1 year', Gender == 'Male') %>%
  spread(Fit.Bound, LE.Diff)
ggplot(LE.diff.male, aes(x = State, y = fit, color = Effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Decrease in Life Expectancy at Birth due to Direct vs Indirect Effects of COVID-19 \nin the USA in 2020 (Male Population)', y = 'Decrease in Life Expectancy (years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# All of United States
LE.diff.USA <- LE.diff %>%
  filter(Age.Cohort == '< 1 year', State == 'United States') %>%
  spread(Fit.Bound, LE.Diff)
LE.diff.USA.net <- LE.diff.USA %>%
  filter(Effect == 'Net Effects') %>%
  select(-c(Effect, fit))
LE.diff.USA <- LE.diff.USA %>%
  filter(Effect != 'Net Effects') %>%
  select(-c(lwr, upr))
LE.diff.USA <- merge(LE.diff.USA, LE.diff.USA.net, by = c('State', 'Gender', 'Age.Cohort'))
ggplot(LE.diff.USA, aes(x = Gender, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2,) +
  labs(title = 'Decrease in Life Expectancy at Birth due to Direct vs Indirect Effects of COVID-19 \nin the USA in 2020', y = 'Decrease in Life Expectancy (years)', x = 'Sex') +
  theme_minimal()

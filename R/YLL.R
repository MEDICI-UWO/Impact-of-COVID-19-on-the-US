library(dplyr)
library(tidyr)
library(ggplot2)

est.LE <- read.csv('Data/Life Expectancy Estimate.csv')
cdc.deaths <- read.csv('Data/Provisional_Deaths_filled.csv')
est.deaths <- read.csv('Data/extrapolated_deaths_2020.csv')

states <- unique(cdc.deaths$State)
age.cohort <- unique(cdc.deaths$Age.Cohort)
gender <- unique(cdc.deaths$Gender)

cdc.deaths <- cdc.deaths %>%
  mutate(
    non.COVID.Deaths = Total.Deaths - COVID.19.Deaths
  )

est.deaths <- est.deaths %>%
  gather(Fit.Bound, Est.Deaths, fit, lwr, upr)

xs.mortality <- merge(est.deaths, cdc.deaths, by = c('State', 'Gender', 'Age.Cohort'))
xs.mortality <- xs.mortality %>%
  mutate(
    `Indirect Effects` = round(non.COVID.Deaths - Est.Deaths),
    `Net Effects` = round(Total.Deaths - Est.Deaths)
  ) %>%
  rename(
    `Direct Effects` = COVID.19.Deaths
  ) %>%
  select(-c(non.COVID.Deaths, Est.Deaths, Total.Deaths))

xs.mortality <- xs.mortality %>%
  gather(Effect, Deaths, `Direct Effects`, `Indirect Effects`, `Net Effects`)
xs.mortality$Fit.Bound[xs.mortality$Fit.Bound == 'lwr'] <- 'temp'
xs.mortality$Fit.Bound[xs.mortality$Fit.Bound == 'upr'] <- 'lwr'
xs.mortality$Fit.Bound[xs.mortality$Fit.Bound == 'temp'] <- 'upr'

# Calculate the years of life lost
YLLs <- merge(xs.mortality, est.LE, by = c('State', 'Gender', 'Age.Cohort', 'Fit.Bound'))
YLLs <- YLLs %>%
  mutate(YLL = Deaths*LE) %>%
  select(-c(Deaths, LE))

# Plot total YLLs due to direct effects of COVID-19 by State and Gender
total.YLLs <- YLLs %>%
  filter(State != 'United States', Effect == 'Direct Effects') %>%
  group_by(State, Gender, Fit.Bound) %>%
  summarise(
    YLL = sum(YLL)
  ) %>%
  spread(Fit.Bound, YLL)

ggplot(total.YLLs, aes(x = State, y = fit, color = Gender)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Years of Life Lost in the USA due to Direct Effects of COVID-19', y = 'YLLs (Person years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Plot total YLLs due to net effects of COVID-19 by State and Gender
total.YLLs <- YLLs %>%
  filter(State != 'United States', Effect == 'Net Effects') %>%
  group_by(State, Gender, Fit.Bound) %>%
  summarise(
    YLL = sum(YLL)
  ) %>%
  spread(Fit.Bound, YLL)

ggplot(total.YLLs, aes(x = State, y = fit, color = Gender)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(title = 'Years of Life Lost in the USA due to Direct and Indirect Effects of COVID-19', y = 'YLLs (Person years)', x = 'States') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# YLLs by age cohort in the United States
USA.YLLs <- YLLs %>%
  filter(State == 'United States')

# Plot YLLS due to Direct Effects (only) by age cohort in the United States
USA.YLL.direct <- USA.YLLs %>%
  filter(Effect == 'Direct Effects') %>%
  select(-Effect) %>%
  spread(Fit.Bound, YLL)
USA.YLL.direct$Age.Cohort <- factor(USA.YLL.direct$Age.Cohort, levels = age.cohort)

ggplot(USA.YLL.direct, aes(x = Age.Cohort, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Years of Life Lost due to Direct Effects of COVID-19 by Age Group \nin the USA due to COVID-19', y = 'YLLs (Person years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Plot YLLS due to net effects by age cohort in the United States
USA.YLL.net <- USA.YLLs %>%
  filter(Effect == 'Net Effects') %>%
  select(-Effect) %>%
  spread(Fit.Bound, YLL)
USA.YLL.net$Age.Cohort <- factor(USA.YLL.net$Age.Cohort, levels = age.cohort)

ggplot(USA.YLL.net, aes(x = Age.Cohort, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Years of Life Lost due to Direct and Indirect Effects of COVID-19 by Age Group \nin the USA due to COVID-19', y = 'YLLs (Person years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Plot YLLs (direct vs indirect effects) by age cohort in the United States
USA.YLLs <- USA.YLLs %>%
  spread(Fit.Bound, YLL)

USA.YLLs.total <- USA.YLLs %>%
  filter(Gender == 'Total') %>%
  select(-Gender)
USA.YLLs.total$Age.Cohort <- factor(USA.YLLs.total$Age.Cohort, levels = age.cohort)

ggplot(USA.YLLs.total, aes(x = Age.Cohort, group = Effect)) +
  geom_bar(aes(y = fit, fill = Effect), stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = 0.2, position = position_dodge(.9)) +
  labs(title = 'YLLs due to Direct vs Indirect Effects of COVID-19 in the USA (Total Population)', y = 'YLLs (Person years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

USA.YLLs.female <- USA.YLLs %>%
  filter(Gender == 'Female') %>%
  select(-Gender)
USA.YLLs.female$Age.Cohort <- factor(USA.YLLs.female$Age.Cohort, levels = age.cohort)

ggplot(USA.YLLs.female, aes(x = Age.Cohort, group = Effect)) +
  geom_bar(aes(y = fit, fill = Effect), stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = 0.2, position = position_dodge(.9)) +
  labs(title = 'YLLs due to Direct vs Indirect Effects of COVID-19 in the USA (Female Population)', y = 'YLLs (Person years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

USA.YLLs.male <- USA.YLLs %>%
  filter(Gender == 'Male') %>%
  select(-Gender)
USA.YLLs.male$Age.Cohort <- factor(USA.YLLs.male$Age.Cohort, levels = age.cohort)

ggplot(USA.YLLs.male, aes(x = Age.Cohort, group = Effect)) +
  geom_bar(aes(y = fit, fill = Effect), stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = 0.2, position = position_dodge(.9)) +
  labs(title = 'YLLs due to Direct vs Indirect Effects of COVID-19 in the USA (Male Population)', y = 'YLLs (Person years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

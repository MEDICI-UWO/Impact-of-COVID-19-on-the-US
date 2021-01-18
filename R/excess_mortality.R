library(dplyr)
library(tidyr)
library(ggplot2)

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
    `Indirect Effect` = round(non.COVID.Deaths - Est.Deaths),
    `Net Effect` = round(Total.Deaths - Est.Deaths)
  ) %>%
  rename(
    `Direct Effect` = COVID.19.Deaths
  ) %>%
  select(-c(non.COVID.Deaths, Est.Deaths, Total.Deaths))

xs.mortality <- xs.mortality %>%
  gather(Effect, Deaths, `Direct Effect`, `Indirect Effect`, `Net Effect`)

xs.mortality <- xs.mortality %>%
  spread(Fit.Bound, Deaths)

# total Net Effect by state
total.xs.mortality <- xs.mortality %>%
  gather(Fit.Bound, Excess.Mortality, fit, lwr, upr) %>%
  group_by(State, Gender, Effect, Fit.Bound) %>%
  summarise(
    Excess.Mortality = sum(Excess.Mortality)
  ) %>%
  ungroup()

total.xs.mortality.net <- total.xs.mortality %>%
  filter(Effect == 'Net Effect', State != 'United States') %>%
  spread(Fit.Bound, Excess.Mortality)

ggplot(total.xs.mortality.net, aes(x = State, color = Gender)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  geom_point(aes(y = fit)) +
  labs(title = 'Excess Mortality attributed to the Direct and Indirect Effects of COVID-19 in the USA', y = '# of Deaths', x = 'State') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),
        legend.title = element_blank())

# Direct vs Indirect Effect of COVID-19 in excess mortalities in the United States
USA.xs.mortality <- xs.mortality %>%
  filter(State == 'United States')

USA.xs.mort.total <- USA.xs.mortality %>%
  filter(Gender == 'Total')
USA.xs.mort.total$Age.Cohort <- factor(USA.xs.mort.total$Age.Cohort, levels = age.cohort)
ggplot(USA.xs.mort.total, aes(x = Age.Cohort, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = lwr, ymin = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to Direct vs Indirect Effects of COVID-19 \nfor the Total Population in the USA', y = '# of Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),
        legend.title = element_blank())

USA.xs.mort.female <- USA.xs.mortality %>%
  filter(Gender == 'Female')
USA.xs.mort.female$Age.Cohort <- factor(USA.xs.mort.female$Age.Cohort, levels = age.cohort)
ggplot(USA.xs.mort.female, aes(x = Age.Cohort, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = lwr, ymin = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to Direct vs Indirect Effects of COVID-19 \nfor the Female Population in the USA', y = '# of Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),
        legend.title = element_blank())

USA.xs.mort.male <- USA.xs.mortality %>%
  filter(Gender == 'Male')
USA.xs.mort.male$Age.Cohort <- factor(USA.xs.mort.male$Age.Cohort, levels = age.cohort)
ggplot(USA.xs.mort.male, aes(x = Age.Cohort, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymax = lwr, ymin = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to Direct vs Indirect Effects of COVID-19 \nfor the Male Population in the USA', y = '# of Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),
        legend.title = element_blank())

# Reformat data frames for supplementary material
total.xs.mortality.csv <- total.xs.mortality %>%
  filter(Effect == 'Net Effect') %>%
  select(-Effect)
xs.mortality.female <- total.xs.mortality.csv %>%
  filter(Gender == 'Female') %>%
  spread(Fit.Bound, Excess.Mortality) %>%
  select(State, fit, upr, lwr)
names(xs.mortality.female) <- c('State', 'female.fit', 'female.lwr', 'female.upr')
xs.mortality.male <- total.xs.mortality.csv %>%
  filter(Gender == 'Male') %>%
  spread(Fit.Bound, Excess.Mortality) %>%
  select(State, fit, upr, lwr)
names(xs.mortality.male) <- c('State', 'male.fit', 'male.lwr', 'male.upr')
xs.mortality.total <- total.xs.mortality.csv %>%
  filter(Gender == 'Total') %>%
  spread(Fit.Bound, Excess.Mortality) %>%
  select(State, fit, upr, lwr)
names(xs.mortality.total) <- c('State', 'total.fit', 'total.lwr', 'total.upr')

xs.mortality.csv <- merge(xs.mortality.total, xs.mortality.male, by = 'State')
xs.mortality.csv <- merge(xs.mortality.csv, xs.mortality.female, by = 'State')

write.csv(xs.mortality.csv, 'Tables/Total excess mortalities (per state).csv', row.names = FALSE)

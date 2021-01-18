library(plyr)
library(dplyr)
library(tidyr)

covid.death <- read.csv('Data/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv', stringsAsFactors = FALSE)

covid <- covid.death %>%
  select(State, Sex, Age.group, COVID.19.Deaths, Total.Deaths)

state <- unique(covid$State)
state <- state[-c(35, 54)]
age.cohort <- unique(covid$Age.group)
age.cohort <- age.cohort[-c(3, 7, 9, 12)]
gender <- unique(covid$Sex)

covid <- covid %>%
  filter(
    State != 'Puerto Rico',
    Age.group %in% age.cohort
  )

total.deaths <- covid %>%
  filter(Age.group == 'All Ages')

total.covid.deaths <- total.deaths %>%
  select(-Total.Deaths) %>%
  spread(Sex, COVID.19.Deaths) %>%
  mutate(
    Total = Female + Male,
    Difference = `All Sexes` - Total
  ) %>%
  select(-c(Unknown, Total)) %>%
  rename(Unknown = Difference) %>%
  gather(Sex, COVID.19.Deaths, `All Sexes`, Female, Male, Unknown)
  
total.total.deaths <- total.deaths %>%
  select(-COVID.19.Deaths) %>%
  spread(Sex, Total.Deaths) %>%
  mutate(
    Total = Female + Male,
    Difference = `All Sexes` - Total
  ) %>%
  select(-c(Unknown, Total)) %>%
  rename(Unknown = Difference) %>%
  gather(Sex, Total.Deaths, `All Sexes`, Female, Male, Unknown)

total.deaths <- merge(total.covid.deaths, total.total.deaths, by = c('State', 'Age.group', 'Sex'))

cohort.deaths <- covid %>%
  filter(Age.group != 'All Ages')

cohort.covid.deaths <- cohort.deaths %>%
  select(-Total.Deaths) %>%
  arrange(State, Sex, Age.group)

missing.covid.summary <- cohort.covid.deaths %>%
  filter(is.na(COVID.19.Deaths)) %>%
  select(-COVID.19.Deaths)

missing.covid <- missing.covid.summary %>%
  group_by(State, Sex) %>%
  summarise(N = n()) %>%
  filter(N == 1) %>%
  select(-N)

missing.covid.summary <- merge(missing.covid.summary, missing.covid, by = c('State', 'Sex'))

cohort.covid.sum <- cohort.covid.deaths %>%
  group_by(State, Sex) %>%
  summarise(COVID.19.Deaths = sum(COVID.19.Deaths, na.rm = TRUE))

missing.covid.summary <- merge(missing.covid.summary, cohort.covid.sum, by = c('State', 'Sex'))
missing.covid.summary <- merge(missing.covid.summary, total.covid.deaths, by = c('State', 'Sex'))
missing.covid.summary <- missing.covid.summary %>%
  mutate(Difference = COVID.19.Deaths.y - COVID.19.Deaths.x) %>%
  select(-c(COVID.19.Deaths.x, COVID.19.Deaths.y, Age.group.y)) %>%
  rename(Age.group = Age.group.x)

cohort.covid.deaths$COVID.19.Deaths[!is.na(match(paste(cohort.covid.deaths$State, cohort.covid.deaths$Sex, cohort.covid.deaths$Age.group), paste(missing.covid.summary$State, missing.covid.summary$Sex, missing.covid.summary$Age.group)))] <- missing.covid.summary$Difference

# View(cohort.covid.deaths)

cohort.total.deaths <- cohort.deaths %>%
  select(-COVID.19.Deaths) %>%
  arrange(State, Sex, Age.group)

missing.total.summary <- cohort.total.deaths %>%
  filter(is.na(Total.Deaths)) %>%
  select(-Total.Deaths)

missing.total <- missing.total.summary %>%
  group_by(State, Sex) %>%
  summarise(N = n()) %>%
  filter(N == 1) %>%
  select(-N)

missing.total.summary <- merge(missing.total.summary, missing.total, by = c('State', 'Sex'))

cohort.total.sum <- cohort.total.deaths %>%
  group_by(State, Sex) %>%
  summarise(Total.Deaths = sum(Total.Deaths, na.rm = TRUE))

missing.total.summary <- merge(missing.total.summary, cohort.total.sum, by = c('State', 'Sex'))
missing.total.summary <- merge(missing.total.summary, total.total.deaths, by = c('State', 'Sex'))
missing.total.summary <- missing.total.summary %>%
  mutate(Difference = Total.Deaths.y - Total.Deaths.x) %>%
  select(-c(Total.Deaths.x, Total.Deaths.y, Age.group.y)) %>%
  rename(Age.group = Age.group.x)

cohort.total.deaths$Total.Deaths[!is.na(match(paste(cohort.total.deaths$State, cohort.total.deaths$Sex, cohort.total.deaths$Age.group), paste(missing.total.summary$State, missing.total.summary$Sex, missing.total.summary$Age.group)))] <- missing.total.summary$Difference

# View(cohort.total.deaths)

cohort.deaths <- merge(cohort.covid.deaths, cohort.total.deaths, by = c('State', 'Sex', 'Age.group'))

# Fill in missing data by age group (compare to deaths by age group in the US)
usa <- cohort.deaths %>%
  filter(State == 'United States')

usa.covid <- usa %>%
  select(-Total.Deaths)

usa.total <- usa %>%
  select(-COVID.19.Deaths)

states <- cohort.deaths %>%
  filter(State != 'United States')

# Fill in missing covid death data by age group
states.covid <- states %>%
  select(-Total.Deaths)

states.covid.summary <- states.covid %>%
  group_by(Sex, Age.group) %>%
  summarise(
    COVID.19.Deaths = sum(COVID.19.Deaths, na.rm = TRUE)
  )

states.covid.diff <- merge(usa.covid, states.covid.summary, by = c('Sex', 'Age.group'))
states.covid.diff <- states.covid.diff %>%
  mutate(
    Difference = COVID.19.Deaths.x - COVID.19.Deaths.y
  ) %>%
  select(-c(COVID.19.Deaths.x, COVID.19.Deaths.y, State))

missing.states.covid <- states.covid %>%
  filter(is.na(COVID.19.Deaths)) %>%
  group_by(Sex, Age.group) %>%
  summarise(
    N = n()
  ) %>%
  filter(N == 1) %>%
  select(-N)

states.covid.diff <- merge(states.covid.diff, missing.states.covid, by = c('Sex', 'Age.group'))

missing.states.covid <- states.covid %>%
  filter(is.na(COVID.19.Deaths)) %>%
  select(-COVID.19.Deaths)

states.covid.diff <- merge(states.covid.diff, missing.states.covid, by = c('Sex', 'Age.group'))
states.covid.diff <- states.covid.diff %>%
  select(State, Sex, Age.group, Difference) %>%
  arrange(State, Sex, Age.group)

cohort.deaths$COVID.19.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(states.covid.diff$State, states.covid.diff$Sex, states.covid.diff$Age.group)))] <- states.covid.diff$Difference

# Fill in missing total death data by age group
states.total <- states %>%
  select(-COVID.19.Deaths)

states.total.summary <- states.total %>%
  group_by(Sex, Age.group) %>%
  summarise(
    Total.Deaths = sum(Total.Deaths, na.rm = TRUE)
  )

states.total.diff <- merge(usa.total, states.total.summary, by = c('Sex', 'Age.group'))
states.total.diff <- states.total.diff %>%
  mutate(Difference = Total.Deaths.x - Total.Deaths.y) %>%
  select(-c(Total.Deaths.x, Total.Deaths.y, State))

missing.states.total <- states.total %>%
  filter(is.na(Total.Deaths)) %>%
  group_by(Sex, Age.group) %>%
  summarise(
    N = n()
  ) %>%
  filter(N == 1) %>%
  select(-N)

states.total.diff <- merge(states.total.diff, missing.states.total, by = c('Sex', 'Age.group'))

missing.states.total <- states.total %>%
  filter(is.na(Total.Deaths)) %>%
  select(-Total.Deaths)

states.total.diff <- merge(states.total.diff, missing.states.total, by = c('Sex', 'Age.group'))
states.total.diff <- states.total.diff %>%
  select(State, Sex, Age.group, Difference) %>%
  arrange(State, Sex, Age.group)

cohort.deaths$Total.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(states.total.diff$State, states.total.diff$Sex, states.total.diff$Age.group)))] <- states.total.diff$Difference

# Need to fill in missing data that we cannot figure out logically here
missing.data <- cohort.deaths[!complete.cases(cohort.deaths),]

temp <- cohort.deaths %>%
  group_by(State, Sex) %>%
  summarise(
    COVID.19.Deaths = sum(COVID.19.Deaths, na.rm = TRUE), 
    Total.Deaths = sum(Total.Deaths, na.rm = TRUE)
  )

filtered.total.deaths <- total.deaths %>%
  filter(Sex != 'All Sexes') %>%
  select(-Age.group)

total.deaths.compare <- merge(temp, filtered.total.deaths, by = c('State', 'Sex'))
total.deaths.compare <- total.deaths.compare %>%
  mutate(
    COVID.19.Deaths = COVID.19.Deaths.y - COVID.19.Deaths.x,
    Total.Deaths = Total.Deaths.y - Total.Deaths.x
  ) %>%
  select(-c(COVID.19.Deaths.x, COVID.19.Deaths.y, Total.Deaths.x, Total.Deaths.y))

missing.data.diff <- merge(missing.data, total.deaths.compare, by = c('State', 'Sex'))

missing.data.covid <- missing.data.diff[is.na(missing.data.diff$COVID.19.Deaths.x),]
missing.data.covid <- missing.data.covid %>%
  select(-c(Total.Deaths.x, Total.Deaths.y)) %>%
  rename(
    COVID.19.Deaths = COVID.19.Deaths.x,
    Difference = COVID.19.Deaths.y
  )

missing.covid.summary <- missing.data.covid %>%
  group_by(State, Sex) %>%
  summarise(
    N = n()
  )
temp <- total.deaths.compare %>%
  select(-Total.Deaths)
missing.covid.summary <- merge(missing.covid.summary, temp, by = c('State', 'Sex'))

known.value.covid <- missing.covid.summary %>%
  filter(N == COVID.19.Deaths)

missing.covid.summary <- anti_join(missing.covid.summary, known.value.covid, by = c('State', 'Sex'))

known.value.covid <- merge(known.value.covid, missing.data.covid, by = c('State','Sex'))
known.value.covid <- known.value.covid %>%
  mutate(
    COVID.19.Deaths = 1
  ) %>%
  select(State, Sex, Age.group, COVID.19.Deaths)
cohort.deaths$COVID.19.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(known.value.covid$State, known.value.covid$Sex, known.value.covid$Age.group)))] <- known.value.covid$COVID.19.Deaths

ratio.covid <- NULL
for (i in 1:nrow(missing.covid.summary)) {
  s <- missing.covid.summary[[i, 'State']]
  g <- missing.covid.summary[[i, 'Sex']]
  temp <- missing.data.covid %>%
    filter(State == s & Sex == g)
  ag <- temp$Age.group
  ratio <- usa %>%
    filter(
      Sex == g,
      Age.group %in% ag
    ) %>%
    select(-c(State, Total.Deaths)) %>%
    mutate(
      State = s,
      ratio = COVID.19.Deaths/sum(COVID.19.Deaths)
    ) %>%
    select(State, Sex, Age.group, ratio)
  ratio.covid <- rbind(ratio.covid, ratio)
}

missing.data.covid <- merge(missing.data.covid, ratio.covid, by = c('State', 'Sex', 'Age.group'))
missing.data.covid <- missing.data.covid %>%
  mutate(
    COVID.19.Deaths = round(Difference*ratio)
  )

revise.missing.covid <- missing.data.covid %>%
  filter(COVID.19.Deaths == 0) %>%
  select(-c(Age.group, COVID.19.Deaths, Difference, ratio))
revise.missing.covid <- unique(revise.missing.covid[, c('State', 'Sex')])
revise.missing.covid <- merge(revise.missing.covid, missing.data.covid, by = c('State', 'Sex'))

revise.covid.summary <- revise.missing.covid %>%
  group_by(State, Sex) %>%
  summarise(N = n())

for (i in 1:nrow(revise.covid.summary)) {
  s <- revise.covid.summary[[i, 'State']]
  g <- revise.covid.summary[[i, 'Sex']]
  if (nrow(revise.missing.covid[revise.missing.covid$State == s & revise.missing.covid$Sex == g,]) == 2) {
    revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths != 0] <- revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths != 0] - 1
    revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths == 0] <- 1
  }
  else {
    n <- nrow(revise.missing.covid[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths == 0,])
    value <- max(revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g])
    revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths == value] <- revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths == value] - n
    revise.missing.covid$COVID.19.Deaths[revise.missing.covid$State == s & revise.missing.covid$Sex == g & revise.missing.covid$COVID.19.Deaths == 0] <- 1
  }
}
revise.missing.covid <- revise.missing.covid %>%
  arrange(State, Sex, Age.group)
cohort.deaths$COVID.19.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(revise.missing.covid$State, revise.missing.covid$Sex, revise.missing.covid$Age.group)))] <- revise.missing.covid$COVID.19.Deaths

missing.data.covid <- anti_join(missing.data.covid, revise.missing.covid, by = c('State', 'Sex', 'Age.group'))
cohort.deaths$COVID.19.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(missing.data.covid$State, missing.data.covid$Sex, missing.data.covid$Age.group)))] <- missing.data.covid$COVID.19.Deaths

missing.data.total <- missing.data.diff[is.na(missing.data.diff$Total.Deaths.x),]
missing.data.total <- missing.data.total %>%
  select(-c(COVID.19.Deaths.x, COVID.19.Deaths.y)) %>%
  rename(
    Total.Deaths = Total.Deaths.x,
    Difference = Total.Deaths.y
  )

missing.total.summary <- missing.data.total %>%
  group_by(State, Sex) %>%
  summarise(
    N = n()
  )
temp <- total.deaths.compare %>%
  select(-COVID.19.Deaths)
missing.total.summary <- merge(missing.total.summary, temp, by = c('State', 'Sex'))

known.value.total <- missing.total.summary %>%
  filter(N == Total.Deaths)

missing.total.summary <- anti_join(missing.total.summary, known.value.total, by = c('State', 'Sex'))

known.value.total <- merge(known.value.total, missing.data.total, by = c('State','Sex'))
known.value.total <- known.value.total %>%
  mutate(
    Total.Deaths = 1
  ) %>%
  select(State, Sex, Age.group, Total.Deaths)
cohort.deaths$Total.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(known.value.total$State, known.value.total$Sex, known.value.total$Age.group)))] <- known.value.total$Total.Deaths

ratio.total <- NULL
for (i in 1:nrow(missing.total.summary)) {
  s <- missing.total.summary[[i, 'State']]
  g <- missing.total.summary[[i, 'Sex']]
  temp <- missing.data.total %>%
    filter(State == s & Sex == g)
  ag <- temp$Age.group
  ratio <- usa %>%
    filter(
      Sex == g,
      Age.group %in% ag
    ) %>%
    select(-c(State, COVID.19.Deaths)) %>%
    mutate(
      State = s,
      ratio = Total.Deaths/sum(Total.Deaths)
    ) %>%
    select(State, Sex, Age.group, ratio)
  ratio.total <- rbind(ratio.total, ratio)
}

missing.data.total <- merge(missing.data.total, ratio.total, by = c('State', 'Sex', 'Age.group'))
missing.data.total <- missing.data.total %>%
  mutate(
    Total.Deaths = round(Difference*ratio)
  )

revise.missing.total <- missing.data.total %>%
  filter(Total.Deaths == 0) %>%
  select(-c(Age.group, Total.Deaths, Difference, ratio))
revise.missing.total <- unique(revise.missing.total[, c('State', 'Sex')])
revise.missing.total <- merge(revise.missing.total, missing.data.total, by = c('State', 'Sex'))

revise.total.summary <- revise.missing.total %>%
  group_by(State, Sex) %>%
  summarise(N = n())

for (i in 1:nrow(revise.total.summary)) {
  s <- revise.total.summary[[i, 'State']]
  g <- revise.total.summary[[i, 'Sex']]
  if (nrow(revise.missing.total[revise.missing.total$State == s & revise.missing.total$Sex == g,]) == 2) {
    revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$COVID.19.Deaths != 0] <- revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$Total.Deaths != 0] - 1
    revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.covid$Total.Deaths == 0] <- 1
  }
  else {
    n <- nrow(revise.missing.total[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$Total.Deaths == 0,])
    value <- max(revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g])
    revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$Total.Deaths == value] <- revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$Total.Deaths == value] - n
    revise.missing.total$Total.Deaths[revise.missing.total$State == s & revise.missing.total$Sex == g & revise.missing.total$Total.Deaths == 0] <- 1
  }
}
revise.missing.total <- revise.missing.total %>%
  arrange(State, Sex, Age.group)
cohort.deaths$Total.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(revise.missing.total$State, revise.missing.total$Sex, revise.missing.total$Age.group)))] <- revise.missing.total$Total.Deaths

missing.data.total <- anti_join(missing.data.total, revise.missing.total, by = c('State', 'Sex', 'Age.group'))
cohort.deaths$Total.Deaths[!is.na(match(paste(cohort.deaths$State, cohort.deaths$Sex, cohort.deaths$Age.group), paste(missing.data.total$State, missing.data.total$Sex, missing.data.total$Age.group)))] <- missing.data.total$Total.Deaths

# Calculate total state count
state.data <- cohort.deaths %>%
  filter(State != 'United States') %>%
  group_by(State, Age.group) %>%
  summarise(
    COVID.19.Deaths = sum(COVID.19.Deaths),
    Total.Deaths = sum(Total.Deaths)
  ) %>%
  mutate(
    Sex = 'All Sexes'
  ) %>%
  select(State, Sex, everything())

cohort.deaths <- rbind(cohort.deaths, state.data) %>%
  filter(Sex != 'Unknown') %>%
  arrange(State, Sex, Age.group, COVID.19.Deaths, Total.Deaths)

# Merge New York and New York City
new.york <- cohort.deaths %>%
  filter(State == 'New York')
new.york.city <- cohort.deaths %>%
  filter(State == 'New York City') %>%
  select(Sex, Age.group, COVID.19.Deaths, Total.Deaths)
new.york <- merge(new.york, new.york.city, by = c('Sex', 'Age.group'))
new.york <- new.york %>%
  mutate(
    COVID.19.Deaths = COVID.19.Deaths.x + COVID.19.Deaths.y,
    Total.Deaths = Total.Deaths.x + Total.Deaths.y
  ) %>%
  select(State, Sex, Age.group, COVID.19.Deaths, Total.Deaths)

states <- cohort.deaths %>%
  filter(State != 'New York' & State != 'New York City')
states <- rbind(states, new.york)

states$State <- factor(states$State, levels = state)
states$Age.group <- factor(states$Age.group, levels = age.cohort)
states$Sex <- factor(states$Sex, levels = gender)

states <- states %>%
  select(State, Sex, Age.group, COVID.19.Deaths, Total.Deaths) %>%
  arrange(State, Sex, Age.group)

# Rename column names and some entries
states$Age.group <- revalue(states$Age.group, c('Under 1 year' = '< 1 year', '85 years and over' = '85+ years'))
states$Sex <- revalue(states$Sex, c('All Sexes' = 'Total'))
names(states) <- c('State', 'Gender', 'Age.Cohort', 'COVID.19.Deaths', 'Total.Deaths')

write.csv(states, 'Data/Provisional_Deaths_filled.csv', row.names = FALSE, na = '')

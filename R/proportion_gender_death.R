library(dplyr)
library(tidyr)

# Data on number of deaths by state, age group, gender, and year
deaths <- read.csv('Data/Deaths per State.csv', stringsAsFactors = FALSE)
deaths <- deaths %>%
  select(State, Ten.Year.Age.Groups, Gender, Year, Deaths)

# Get age cohort breakdown
age.cohort <- unique(deaths$Ten.Year.Age.Groups)
age.cohort <- age.cohort[1:length(age.cohort)-1]

# Data on number of deaths by state and year
yearly.deaths <- read.csv('Data/Death per State (Total no age group).csv', stringsAsFactors = FALSE)
yearly.deaths <- yearly.deaths %>%
  select(State, Year, Deaths)

# Data on number of deaths by state, age group, and year
total.deaths <- read.csv('Data/Deaths per State (total).csv', stringsAsFactors = FALSE)
total.deaths <- total.deaths %>%
  select(State, Ten.Year.Age.Groups, Year, Deaths)

# Data on number of deaths by state, gender, and year
deaths.by.gender <- read.csv('Data/Death per State (Gender only).csv', stringsAsFactors = FALSE)
deaths.by.gender <- deaths.by.gender %>%
  select(State, Gender, Year, Deaths)

deaths.spread <- deaths %>%
  spread(Gender, Deaths)

# Fill in suppressed data for data fields in which only one data (either female or male) is missing
merge.deaths <- merge(deaths.spread, total.deaths, by = c('State', 'Ten.Year.Age.Groups', 'Year'))
merge.deaths <- merge.deaths %>%
  rename(
    Total = Deaths
  ) %>%
  filter(Ten.Year.Age.Groups != 'Not Stated')
merge.deaths[merge.deaths == 'Suppressed'] <- NA

merge.deaths[, c('Female', 'Male', 'Total')] <- sapply(merge.deaths[, c('Female', 'Male', 'Total')], as.integer)

merge.deaths$Female[is.na(merge.deaths$Female) & !is.na(merge.deaths$Male)] <- merge.deaths$Total[is.na(merge.deaths$Female) &!is.na(merge.deaths$Male)] - merge.deaths$Male[is.na(merge.deaths$Female) &!is.na(merge.deaths$Male)]
merge.deaths$Male[is.na(merge.deaths$Male) & !is.na(merge.deaths$Female)] <- merge.deaths$Total[is.na(merge.deaths$Male) & !is.na(merge.deaths$Female)] - merge.deaths$Female[is.na(merge.deaths$Male) & !is.na(merge.deaths$Female)]

# Fill in suppressed data for male and female deaths if total number of deaths is equal to 18 (this means that the values for male and female deaths has to be 9)
merge.deaths$Female[merge.deaths$Total == 18] <- 9
merge.deaths$Male[merge.deaths$Total == 18] <- 9

# Fill in suppressed "total" data (to the best of our ability)
# Assume that any difference less than 10 is the value for our missing data value (will be a slight overestimation)
yearly.deaths.merge <- merge.deaths %>%
  select(-c(Male, Female)) %>%
  group_by(State, Year) %>%
  summarise(
    Total = sum(Total, na.rm = TRUE)
  )
yearly.deaths.merge <- merge(yearly.deaths.merge, yearly.deaths, by = c('State', 'Year'))
yearly.deaths.merge <- yearly.deaths.merge %>%
  mutate(
    Difference = Deaths - Total
  ) %>%
  select(-c(Deaths, Total))

missing.deaths <- merge.deaths[is.na(merge.deaths$Total),]
missing.deaths <- missing.deaths %>%
  select(-c(Female, Male, Total))
missing.deaths <- merge(missing.deaths, yearly.deaths.merge, by = c('State', 'Year'))
missing.deaths <- missing.deaths %>%
  filter(Difference < 10) %>%
  rename(Total = Difference)
temp <- missing.deaths %>%
  group_by(State, Year) %>%
  summarise(N = n()) %>%
  filter(N == 1) %>%
  select(-N)
missing.deaths <- merge(missing.deaths, temp, by = c('State', 'Year'))

merge.deaths$Total[!is.na(match(paste(merge.deaths$State, merge.deaths$Ten.Year.Age.Groups, merge.deaths$Year), paste(missing.deaths$State, missing.deaths$Ten.Year.Age.Groups, missing.deaths$Year)))] <- missing.deaths$Total

# Data frame that calculated total number of male and female deaths based on the information that we have so far
temp <- merge.deaths %>%
  group_by(State, Year) %>%
  summarise(
    Female = sum(Female, na.rm = TRUE),
    Male = sum(Male, na.rm = TRUE)
  ) %>%
  gather(Gender, Deaths, Female, Male)
  
# Find difference between calculated total and data given by the CDC
death.diff <- merge(temp, deaths.by.gender, by = c('State', 'Year', 'Gender'))
death.diff <- death.diff %>%
  mutate(Difference = Deaths.y - Deaths.x)
death.diff <- death.diff %>%
  select(-c(Deaths.y, Deaths.x))

# Find states age groups with missing data
missing.data <- merge.deaths[!complete.cases(merge.deaths),]

# Filter out states that provides total amounts
data.w.total.calc <- missing.data %>%
  filter(!is.na(Total))
data.w.total <- data.w.total.calc %>%
  select(-Total) %>%
  gather(Gender, Deaths, Female, Male)
data.w.total <- merge(data.w.total, death.diff, by = c('State', 'Year', 'Gender'))
# Add male and female difference to see if total matches CDC data
data.w.total <- data.w.total %>%
  select(-Deaths) %>%
  rename(Deaths = Difference) %>%
  spread(Gender, Deaths) %>%
  mutate(
    Total = Female + Male
  )
data.w.total <- merge(data.w.total, data.w.total.calc %>% select(-c(Female, Male)), by = c('State', 'Ten.Year.Age.Groups', 'Year'))
# If sum of differences matches CDC data, then accept these as the numbers for male and female deaths
data.w.total <- data.w.total %>%
  filter(Total.x == Total.y)
data.w.total <- data.w.total %>%
  select(-c(Total.x, Total.y))

# Fill in missing data that matches CDC data
merge.deaths$Female[!is.na(match(paste(merge.deaths$State, merge.deaths$Ten.Year.Age.Groups, merge.deaths$Year), paste(data.w.total$State, data.w.total$Ten.Year.Age.Groups, data.w.total$Year)))] <- data.w.total$Female
merge.deaths$Male[!is.na(match(paste(merge.deaths$State, merge.deaths$Ten.Year.Age.Groups, merge.deaths$Year), paste(data.w.total$State, data.w.total$Ten.Year.Age.Groups, data.w.total$Year)))] <- data.w.total$Male

# New dataframe of data points that has a value for Total, but not for Male and Female
missing.gender.data <- merge.deaths[!complete.cases(merge.deaths) & !is.na(merge.deaths$Total),]

death.age.prop <- merge.deaths %>%
  mutate(
    Female.prop = Female/Total,
    Male.prop = Male/Total
  ) %>%
  group_by(Ten.Year.Age.Groups) %>%
  summarise(
    Female.prop = mean(Female.prop, na.rm = TRUE),
    Male.prop = mean(Male.prop, na.rm = TRUE)
  )

missing.gender.data <- merge(missing.gender.data, death.age.prop, by = 'Ten.Year.Age.Groups')

missing.gender.data$Female <- round(missing.gender.data$Total*missing.gender.data$Female.prop)
missing.gender.data$Male <- round(missing.gender.data$Total*missing.gender.data$Male.prop)

missing.gender.data <- missing.gender.data %>%
  arrange(State, Ten.Year.Age.Groups, Year)

merge.deaths$Female[!is.na(match(paste(merge.deaths$State, merge.deaths$Ten.Year.Age.Groups, merge.deaths$Year), paste(missing.gender.data$State, missing.gender.data$Ten.Year.Age.Groups, missing.gender.data$Year)))] <- missing.gender.data$Female
merge.deaths$Male[!is.na(match(paste(merge.deaths$State, merge.deaths$Ten.Year.Age.Groups, merge.deaths$Year), paste(missing.gender.data$State, missing.gender.data$Ten.Year.Age.Groups, missing.gender.data$Year)))] <- missing.gender.data$Male

# write filled-in data to file
merge.deaths.gather <- merge.deaths %>%
  gather(Gender, Deaths, Female, Male, Total)
merge.deaths.gather$Ten.Year.Age.Groups <- factor(merge.deaths.gather$Ten.Year.Age.Groups, levels = age.cohort)
merge.deaths.gather <- merge.deaths.gather  %>%
  arrange(State, Year, Gender, Ten.Year.Age.Groups)

write.csv(merge.deaths.gather, 'Data/Deaths per State (filled).csv', row.names = FALSE, na = '')

# # Find total number of deaths each year for male, female and total population
# sum.deaths <- merge.deaths%>%
#   group_by(State, Year) %>%
#   summarise(
#     Total = sum(Total, na.rm = TRUE)
#   )
# 
# # Calculate proportion of deaths for each gender and age group from total population
# death.prop <- merge.deaths %>%
#   select(-Total) %>%
#   gather(Gender, Deaths, Female, Male)
# death.prop <- merge(death.prop, sum.deaths, by = c('State', 'Year'))
# death.prop <- death.prop %>%
#   mutate(
#     Proportion = Deaths/Total
#   )
# # Take average of proportion from past 5 years
# proportion.avg <- death.prop %>%
#   filter(Year > 2014) %>%
#   group_by(State, Ten.Year.Age.Groups, Gender) %>%
#   summarise(
#     avg.prop = mean(Proportion, na.rm = TRUE)
#   )
# 
# prop.sum <- proportion.avg %>%
#   group_by(State) %>%
#   summarise(
#     total.prop = sum(avg.prop, na.rm = TRUE)
#   )
# 
# # Find # of deaths for each age cohort and gender in January 2020
# jan.deaths <- read.csv('Data/VSRR_-_State_and_National_Provisional_Counts_for_Live_Births__Deaths__and_Infant_Deaths.csv', stringsAsFactors = FALSE)
# jan.deaths <- jan.deaths %>%
#   filter(Year == 2020, Month == 'January', Period == 'Monthly', Indicator == 'Number of Deaths', !(State %in% c('UNITED STATES', 'PUERTO RICO')))
# 
# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), substring(s, 2),
#         sep="", collapse=" ")
# }
# 
# temp <- sapply(jan.deaths$State, tolower)
# jan.deaths$State <- sapply(temp, simpleCap)
# 
# jan.deaths$State[jan.deaths$State == 'District Of Columbia'] <- 'District of Columbia'
# 
# jan.deaths <- jan.deaths %>%
#   select(State, Data.Value)
# 
# jan.deaths.age.gender <- merge(proportion.avg, jan.deaths, by = c('State'))
# jan.deaths.age.gender <- jan.deaths.age.gender %>%
#   mutate(
#     Deaths = round(avg.prop*Data.Value)
#   )
# jan.deaths.output <- jan.deaths.age.gender %>%
#   select(State, Ten.Year.Age.Groups, Gender, Deaths)
# jan.deaths.output$Ten.Year.Age.Groups <- factor(jan.deaths.output$Ten.Year.Age.Groups, levels = age.cohort)
# usa.jan.deaths <- jan.deaths.output %>%
#   group_by(Gender, Ten.Year.Age.Groups) %>%
#   summarise(Deaths = sum(Deaths)) %>%
#   mutate(State = 'United States')
# jan.deaths.output <- rbind(jan.deaths.output, usa.jan.deaths)
# jan.deaths.output <- jan.deaths.output %>%
#   arrange(State, Gender, Ten.Year.Age.Groups)
# 
# write.csv(jan.deaths.output, 'Data/usa_jan_deaths.csv', row.names = FALSE)

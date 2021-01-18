##############################################################################################################################
# Author: EYSC
# Date Modified: January 8, 2021
# 
# Source file of functions regarding life tables
##############################################################################################################################

life.table <- function(death, pop, n) {
  # Calculates abridged life table
  # Inputs:
  #   death: vector of number of deaths for each age cohort
  #   pop: vector of population for each age cohort
  #   n: vector of number of years that each age cohort lasts
  # Output:
  #   data frame of abridged life table
  df <- data.frame('Deaths' = death, 'Population' = pop)
  df <- df %>%
    mutate(
      Mortality.rate = Deaths/Population,
      Proportion.who.die = 1 - exp(-n*Mortality.rate)
    )
  Starting.newborns <- numeric(length(n))
  Starting.newborns[1] <- 100000
  Deaths.in.cohort <- numeric(length(n))
  Deaths.in.cohort[1] <- df[1, ]$Proportion.who.die*Starting.newborns[1]
  for (i in 2:length(n)) {
    Starting.newborns[i] <- Starting.newborns[i-1]-Deaths.in.cohort[i-1]
    Deaths.in.cohort[i] <- df[i, ]$Proportion.who.die*Starting.newborns[i]
  }
  df$Starting.newborns <- Starting.newborns
  df$Deaths.in.cohort <- Deaths.in.cohort
  df <- df %>%
    mutate(
      Years.of.life.cohort = Deaths.in.cohort/Mortality.rate
    )
  Total.years.lived <- numeric(length(n))
  for (i in 1:length(n)) {
    Total.years.lived[i] <- sum(df$Years.of.life.cohort[i:length(n)])
  }
  df$Total.years.lived <- Total.years.lived
  df$Years.remaining <- df$Total.years.lived/df$Starting.newborns
  return(df)
}

# summary.le <- function(death.df, population.df, n, age.cohort) {
#   # Calculates the life expectancy (fitted value, lower and upper bound) at birth for each gender
#   # Inputs:
#   #   death.df: data frame of estimated number of deaths that will occur, need to have columns "Gender", "fit", "lwr", and "upr" 
#   #   population.df: data frame of estimated mid-year population, need to have columns "Gender", "fit", "lwr", and "upr" 
#   #   n: vector of number of years each corresponding age.cohort spans
#   #   age.cohort: vector (string) of names of age cohort in the correct order
#   # Output:
#   #   Matrix of life expectancy (fit, lwr, upr) for the given gender(s)
#   pop.genders <- unique(population.df$Gender)
#   death.genders <- unique(death.df$Gender)
#   if (all(pop.genders %in% death.genders)) {
#     genders <- pop.genders
#   }
#   else {
#     stop('Genders do not match between population and death data frame.')
#   }
#   lex.table <- NULL
#   for (g in genders) {
#     d.df <- death.df %>%
#       filter(Gender == g)
#     d.df <- d.df[match(age.cohort, d.df$Age.Cohort),]
#     pop.df <- population.df %>%
#       filter(Gender == g)
#     pop.df <- pop.df[match(age.cohort, pop.df$Age.Cohort),]
#     fit.est <- life.table(d.df$fit, pop.df$fit, n)[1, 'Years.remaining']
#     lwr.est <- life.table(d.df$upr, pop.df$lwr, n)[1, 'Years.remaining']
#     upr.est <- life.table(d.df$lwr, pop.df$upr, n)[1, 'Years.remaining']
#     lex.table <- rbind(lex.table, c(fit.est, lwr.est, upr.est))
#   }
#   rownames(lex.table) <- genders
#   colnames(lex.table) <- c('fit', 'lwr', 'upr')
#   return(lex.table)
# }

le.summary <- function(death.df, population.df, n, age.cohort) {
  # Calculates the life expectancy (fitted value, lower and upper bound) for all age cohorts for each gender
  # Inputs:
  #   death.df: data frame of estimated number of deaths that will occur, need to have columns "Gender", "fit", "lwr", and "upr" 
  #   population.df: data frame of estimated mid-year population, need to have columns "Gender", "fit", "lwr", and "upr" 
  #   n: vector of number of years each corresponding age.cohort spans
  #   age.cohort: vector (string) of names of age cohort in the correct order
  # Output:
  #   Matrix of life expectancy (fit, lwr, upr) for the given gender(s)
  pop.genders <- unique(population.df$Gender)
  death.genders <- unique(death.df$Gender)
  if (all(pop.genders %in% death.genders)) {
    genders <- pop.genders
  }
  else {
    stop('Genders do not match between population and death data frame.')
  }
  lex.table <- NULL
  for (g in genders) {
    d.df <- death.df %>%
      filter(Gender == g)
    d.df <- d.df[match(age.cohort, d.df$Age.Cohort),]
    pop.df <- population.df %>%
      filter(Gender == g)
    pop.df <- pop.df[match(age.cohort, pop.df$Age.Cohort),]
    fit.est <- life.table(d.df$fit, pop.df$fit, n)[, 'Years.remaining']
    fit.est <- cbind(Gender = rep(g, length(fit.est)), Age.Cohort = age.cohort, Fit.Bound = rep('fit', length(fit.est)), LE = fit.est)
    lwr.est <- life.table(d.df$upr, pop.df$lwr, n)[, 'Years.remaining']
    lwr.est <- cbind(Gender = rep(g, length(lwr.est)), Age.Cohort = age.cohort, Fit.Bound = rep('lwr', length(lwr.est)), LE = lwr.est)
    upr.est <- life.table(d.df$lwr, pop.df$upr, n)[, 'Years.remaining']
    upr.est <- cbind(Gender = rep(g, length(upr.est)), Age.Cohort = age.cohort, Fit.Bound = rep('upr', length(upr.est)), LE = upr.est)
    lex.table <- rbind(lex.table, fit.est, lwr.est, upr.est)
  }
  return(lex.table)
}

cdc.le.summary <- function(death.df, population.df, n, age.cohort) {
  # Calculates the life expectancy (fitted value, lower and upper bound) for all age cohorts for each gender
  # Inputs:
  #   death.df: data frame of the number of deaths reported by the CDC, need to have columns "Gender", "Deaths"
  #   population.df: data frame of estimated mid-year population, need to have columns "Gender", "fit", "lwr", and "upr" 
  #   n: vector of number of years each corresponding age.cohort spans
  #   age.cohort: vector (string) of names of age cohort in the correct order
  # Output:
  #   Matrix of life expectancy (fit, lwr, upr) for the given gender(s)
  pop.genders <- unique(population.df$Gender)
  death.genders <- unique(death.df$Gender)
  if (all(pop.genders %in% death.genders)) {
    genders <- pop.genders
  }
  else {
    stop('Genders do not match between population and death data frame.')
  }
  lex.table <- NULL
  for (g in genders) {
    d.df <- death.df %>%
      filter(Gender == g)
    d.df <- d.df[match(age.cohort, d.df$Age.Cohort),]
    pop.df <- population.df %>%
      filter(Gender == g)
    pop.df <- pop.df[match(age.cohort, pop.df$Age.Cohort),]
    fit.est <- life.table(d.df$Deaths, pop.df$fit, n)[, 'Years.remaining']
    fit.est <- cbind(Gender = rep(g, length(fit.est)), Age.Cohort = age.cohort, Fit.Bound = rep('fit', length(fit.est)), LE = fit.est)
    lwr.est <- life.table(d.df$Deaths, pop.df$lwr, n)[, 'Years.remaining']
    lwr.est <- cbind(Gender = rep(g, length(lwr.est)), Age.Cohort = age.cohort, Fit.Bound = rep('lwr', length(lwr.est)), LE = lwr.est)
    upr.est <- life.table(d.df$Deaths, pop.df$upr, n)[, 'Years.remaining']
    upr.est <- cbind(Gender = rep(g, length(upr.est)), Age.Cohort = age.cohort, Fit.Bound = rep('upr', length(upr.est)), LE = upr.est)
    lex.table <- rbind(lex.table, fit.est, lwr.est, upr.est)
  }
  return(lex.table)
}
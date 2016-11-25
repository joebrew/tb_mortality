# Attach packages
library(readxl)
library(tidyr)
library(readr)
library(dplyr)

# Read in data
for (i in c('a', 'b', 'c', 'd')){
  x <- read_excel(paste0('data/File ', i, ' excel WHO_pg.xlsx'))
  x <- x[names(x) != '']
  assign(paste0('who_', i),
         x)
}

# Combine horizontally
who <-
  who_a %>%
  left_join(who_b, by = 'iso3') %>%
  left_join(who_c, by = 'iso3') %>%
  left_join(who_d, by = 'iso3')

# Pre-pend all column names with "who"
for (j in 1:ncol(who)){
  if(names(who)[j] != 'iso3'){
    names(who)[j] <- paste0('who_', names(who)[j])
  }
}

# Replace all periods with underscores
names(who) <- gsub('.', '_', names(who), fixed = TRUE)

# Get the country linkage name
linkage <- read_csv('data/ISO_Country_Link.csv')
who <- left_join(who,
                linkage,
                by = 'iso3') %>%
  rename(country = `COUNTRY NAME`)

# Reorder column names
who <- who[,unique(c('country', 'iso3', 'country_number', names(who)))]

# Remove all the other junk
rm(who_a, who_b, who_c, who_d, linkage, x, i, j)

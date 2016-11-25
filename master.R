# Format all data
source('format_who.R')
source('format_ihme.R')
source('format_population_data.R')

# Combine all data
linkage <- read_csv('data/ISO_Country_Link.csv') %>%
  rename(country_name = `COUNTRY NAME`)

df <- linkage %>%
  left_join(who) %>%
  left_join(ihme) %>%
  left_join(population)

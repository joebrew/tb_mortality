# Format all data
if('formatted_data.RData' %in% dir('data')){
  load('data/formatted_data.RData')
} else {
  source('format_who.R')
  source('format_ihme.R')
  source('format_population_data.R')
  save.image('data/formatted_data.RData')
}

# Combine all data
linkage <- read_csv('data/ISO_Country_Link.csv') %>%
  rename(country_name = `COUNTRY NAME`)
df <- linkage %>% 
  full_join(who) %>%
  full_join(ihme) %>%
  full_join(population)

# Remove those rows for which there is no country_number
# ie they do not appear in either dataset
df <- df %>% filter(!is.na(country_number))

# Write csv
write_csv(df, 'data/combined_data.csv')

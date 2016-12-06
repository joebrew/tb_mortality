library(readr)
library(dplyr)
options(scipen = '999')
# Format all data
if('formatted_data.RData' %in% dir('data')){
  load('data/formatted_data.RData')
} else {
  source('format_population_data.R')
  source('format_who.R')
  source('format_ihme.R')
  save.image('data/formatted_data.RData')
}

# Read in the linkage file
linkage <- read_csv('data/ISO_Country_Link.csv') %>%
  rename(country_name = `COUNTRY NAME`)

# Combine all data
df <- who %>%
  left_join(linkage,
            by = 'iso3') %>%
  left_join(ihme,
            by = 'country_number') %>%
  left_join(population,
            by = 'country_number')

# Remove those rows for which there is no country_number
# ie they do not appear in either dataset
df <- df %>% filter(!is.na(country_number))

# Clean up names
names(df) <- 
  gsub('mort', 'deaths', names(df))

# Remove se and lo/hi
df <- df[,!grepl('_lo|_hi|_se', names(df))]

# Remove commas from country names
df$country_name <- gsub(',', '', df$country_name)
df <- df[,unique(c('country_name', 'country_number', 'iso3',
                   'who_g_whoregion', 'who_year', names(df)))]


# Write csv
write_csv(df, 'data/combined_data.csv')

# Create a regional view
# region <- data.frame(region = sort(unique(df$who_g_whoregion)))
# these_names <- names(df)
# for (j in names(df)){
#   
# }


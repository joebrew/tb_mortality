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

# Remove all population columns from WHO
df <- df[,!grepl('pop', names(df))]

# Fix one pesky name
names(df)[names(df) == 'who_g_whoregion'] <- 'who_region'

# Remove the year
df$who_year <- NULL

# Standardize order of name elements
# source_indicator_sex_age_hiv_ratenumber
standardize <- function(x){
  new_name <- x
  x_split <- unlist(strsplit(x, split = '_'))
  if(x_split[1] %in% c('ihme', 'who') &
     x != 'who_region'){
    # Source
    source <- x_split[1]
    
    # Indicator
    indicator <- 'deaths'
    
    # Sex
    sex <- x_split[x_split %in% c('m', 'f', 'both')]
    if(length(sex) == 0){
      sex <- 'both'
    }
    
    # Age
    age <- x_split[x_split %in% c('014', '15plus', 'allages')]
    if(length(age) == 0){
      age <- 'allages'
    }
    
    # Hiv
    hiv <- x_split[x_split %in% c('h', 'nh')]
    if(length(hiv) == 0){
      hiv <- 'nh'
    }
    
    # rate_number
    rate_number <- x_split[x_split %in% c('number', 'rate')]
    if(length(rate_number) == 0){
      rate_number <- 'number'
    }
    
    # Combine together
    new_name <- 
      paste0(c(source,
             indicator,
             sex,
             age,
             hiv,
             rate_number),
             collapse = '_')
  }
  return(new_name)
}

for (j in 1:ncol(df)){
  the_old_name <- names(df)[j]
  the_new_name <- standardize(the_old_name)
  if(the_old_name == the_new_name){
    message(paste0('Did not change variable ', the_old_name))
  } else {
    message(paste0('CHANGED variable ', 
                   the_old_name,
                   ' to ',
                   the_new_name))
    names(df)[j] <- 
      the_new_name
  }
}

# Write csv
write_csv(df, 'data/combined_data.csv')

# Create a regional view
# region <- data.frame(region = sort(unique(df$who_g_whoregion)))
# these_names <- names(df)
# for (j in names(df)){
#   
# }


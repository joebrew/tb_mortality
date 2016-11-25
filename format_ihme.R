# Attach packages
library(readxl)
library(tidyr)
library(readr)
library(dplyr)

# Read in data
ihme <- read_csv('data/IHME DATASET 24112016.csv')

# Re-organize so that the data are in a 
# one row per country format, with columns inidcating other metrics
# (ie, go from long to wide)

# This would result in the following number of columns

# Get the individual values on which we're spreading the data
location_names <- unique(ihme$location_name)

measure_names <- unique(ihme$measure_name)
sex_names <- unique(ihme$sex_name)
age_names <- unique(ihme$age_name)
cause_names <- unique(ihme$cause_name)
metric_names <- unique(ihme$metric_name)
years <- unique(ihme$year)

# Create new dataframe
wide <- data.frame(location_name = location_names)

for(measure_name in measure_names){
  for(sex_name in sex_names){
    for(age_name in age_names){
      for(cause_name in cause_names){
        for(metric_name in metric_names){
          for(year in years){
            wide[,paste(measure_name, '_',
                        sex_name, '_',
                        age_name, '_',
                        cause_name, '_',
                        metric_name, '_',
                        year, '_')] <- NA
          }
        }
      }
    }
  }
}

n_wide <- nrow(wide)
for (i in 1:n_wide){
  message(paste0('Row ', i, ' of ', n_wide))
  this_location_name <- wide$location_name[i]
  for(measure_name in measure_names){
    for(sex_name in sex_names){
      for(age_name in age_names){
        for(cause_name in cause_names){
          for(metric_name in metric_names){
            for(year in years){
              this_row <-
                ihme[which(ihme$measure_name == measure_name &
                           ihme$sex_name == sex_name &
                           ihme$age_name == age_name &
                           ihme$cause_name == cause_name &
                           ihme$metric_name == metric_name &
                           ihme$year == year &
                           ihme$location_name == this_location_name),]
              this_value <- this_row$val
              if(length(this_value) > 0){
                wide[i,paste(measure_name, '_',
                             sex_name, '_',
                             age_name, '_',
                             cause_name, '_',
                             metric_name, '_',
                             year, '_')] <- this_value
              }
              
            }
          }
        }
      }
    }
  }
}

# Overwrite
ihme <- wide; rm(wide)

# Remove spaces in column names
names(ihme) <- gsub(' ', '', names(ihme))

# Make all names lowercase
names(ihme) <- tolower(names(ihme))

# Remove slashes
names(ihme) <- gsub('/', '_', names(ihme), fixed = TRUE)

# Remove dashes
names(ihme) <- gsub('-', '_', names(ihme), fixed = TRUE)

# Remove pluses
names(ihme) <- gsub('+', '_plus_', names(ihme), fixed = TRUE)

# Remove trailing underscore
names(ihme) <- sub("_$","",names(ihme))

# Prepend with ihme
names(ihme) <- paste0('ihme_', names(ihme))

# Get the country linkage name
ihme$country <- ihme$ihme_location_name

# Remove all incidence related variables
ihme <- ihme[,!grepl('incidence', names(ihme))]

# Reshape age groups to match WHO age groups
# ....

# Get linkage variables
linkage <- read_csv('data/ISO_Country_Link.csv')
ihme <- left_join(ihme,
                linkage %>%
                  rename(country = `COUNTRY NAME`),
                by = 'country')

# Reorder column names
ihme <- ihme[,unique(c('country', 'iso3', 'country_number', names(ihme)))]

# Remove all the excess junk
z <- unlist(lapply(ls(), function(x){class(get(x))[1]}))
files <- ls()
for (i in 1:length(z)){
  if(!z[i] %in% c('data.frame', 'tbl_df')){
    rm(list = files[i], envir = .GlobalEnv)
  }
}
rm(files, i)
rm(linkage, this_row, z)
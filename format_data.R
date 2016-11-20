# Attach packages
library(readxl)
library(tidyr)
library(readr)

# Read in data
df <- read_excel('GBD2015_TB.xlsx')

# Re-organize so that the data are in a 
# one row per country format, with columns inidcating other metrics
# (ie, go from long to wide)

# This would result in the following number of columns

# Get the individual values on which we're spreading the data
location_names <- unique(df$location_name)

measure_names <- unique(df$measure_name)
sex_names <- unique(df$sex_name)
age_names <- unique(df$age_name)
cause_names <- unique(df$cause_name)
metric_names <- unique(df$metric_name)
years <- unique(df$year)

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


for (i in 1:nrow(wide)){
  message(i)
  this_location_name <- wide$location_name[i]
  for(measure_name in measure_names){
    for(sex_name in sex_names){
      for(age_name in age_names){
        for(cause_name in cause_names){
          for(metric_name in metric_names){
            for(year in years){
              this_row <-
                df[which(df$measure_name == measure_name &
                           df$sex_name == sex_name &
                           df$age_name == age_name &
                           df$cause_name == cause_name &
                           df$metric_name == metric_name &
                           df$year == year &
                           df$location_name == this_location_name),]
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
write_csv(wide, 'wide_data.csv')

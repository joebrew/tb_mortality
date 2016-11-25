# Attach packages
library(readxl)
library(tidyr)
library(readr)
library(dplyr)

# Read in data
population <- read_excel('data/Population Estimates IHME_2015.xlsx')

# Make wide
results <- data.frame(country_number = unique(sort(population$location_id)))

# Loop through each row to make wide
sex_names <- sort(unique(population$sex_name))
age_group_names <- sort(unique(population$age_group_name))
for(s in sex_names){
  for(a in age_group_names){
    results[,paste0(s, '_', a)] <- NA
  }
}
for (i in 1:nrow(results)){
  message(i)
  for(s in sex_names){
    for(a in age_group_names){
      results[,paste0(s, '_', a)] <- NA
    }
  }
  this_location <- results$country_number[i]
  x <- population %>%
    filter(sex_name == s,
           age_group_name == a,
           location_id == this_location) %>%
    .$pop
  if(length(x) == 1){
    results[i,paste0(s, '_', a)] <- x
  }
}

# Overwrite
population <- results; rm(results)
rm(a, age_group_names, i, s, sex_names, this_location, x)

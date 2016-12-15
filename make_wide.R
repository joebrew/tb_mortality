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
# Get IHME data for those with a country number
  full_join(ihme,
            by = 'country_number') %>%
  left_join(population,
            by = 'country_number') %>%
  # Create a have_both variable
  mutate(have_who = !is.na(who_mort_h_number),
         have_ihme = !is.na(ihme_deaths_f_allages_h_number)) %>%
  mutate(have_both = have_who & have_ihme)

# # Keep only data for which there is info from both datasets
# df <- df %>%
#   filter(!is.na(ihme_deaths_m_014_h_rate),
#          !is.na(who_mort_h_014_rate))
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

# # Standardize order of name elements
# # source_indicator_sex_age_hiv_ratenumber
# standardize <- function(x){
#   new_name <- x
#   x_split <- unlist(strsplit(x, split = '_'))
#   if(x_split[1] %in% c('ihme', 'who') &
#      x != 'who_region'){
#     # Source
#     source <- x_split[1]
#     
#     # Indicator
#     indicator <- 'deaths'
#     
#     # Sex
#     sex <- x_split[x_split %in% c('m', 'f', 'both')]
#     if(length(sex) == 0){
#       sex <- 'both'
#     }
#     
#     # Age
#     age <- x_split[x_split %in% c('014', '15plus', 'allages')]
#     if(length(age) == 0){
#       age <- 'allages'
#     }
#     
#     # Hiv
#     hiv <- x_split[x_split %in% c('h', 'nh')]
#     if(length(hiv) == 0){
#       hiv <- 'nh'
#     }
#     
#     # rate_number
#     rate_number <- x_split[x_split %in% c('number', 'rate')]
#     if(length(rate_number) == 0){
#       rate_number <- 'number'
#     }
#     
#     # Combine together
#     new_name <- 
#       paste0(c(source,
#              indicator,
#              sex,
#              age,
#              hiv,
#              rate_number),
#              collapse = '_')
#   }
#   return(new_name)
# }

# Standardize per Alberto Garcia Basteiro's naming schema
# who = w
# ihme = i
# nd = number of deaths
# mr = mortality rate
# htb = hiv related to tb
# tb = only tb
albertify <- function(x){
  new_name <- x
  x_split <- unlist(strsplit(x, split = '_'))
  if(x_split[1] %in% c('ihme', 'who')){
    
    # Source
    source <- x_split[1]
    if(source == 'ihme'){
      source <- 'i'
    } else if(source == 'who'){
      source <- 'w'
    } else {
      source <- 'PROBLEMWITHSOURCE'
    }
    
    # # Indicator
    # indicator <- 'deaths'
    
    # Sex
    sex <- x_split[x_split %in% c('m', 'f', 'both')]
    if(length(sex) == 0){
      sex <- 'both'
    }
    
    # Age
    age <- x_split[x_split %in% c('014', '15plus', 'allages')]
    if(length(age) == 0){
      age <- 'all'
    }
    if(age == 'allages'){
      age <- 'all'
    }
    
    # Hiv
    hiv <- x_split[x_split %in% c('h', 'nh')]
    if(length(hiv) == 0){
      hiv <- 'nh'
    }
    if(hiv == 'h'){
      hiv <- 'htb'
    } else {
      hiv <- 'tb'
    }
    
    # rate_number
    rate_number <- x_split[x_split %in% c('number', 'rate')]
    if(length(rate_number) == 0){
      rate_number <- 'number'
    } 
    if(rate_number == 'number'){
      rate_number <- 'nd'
    } else {
      rate_number <- 'mr'
    }
    
    # Combine together
    new_name <- 
      paste0(c(source,
               # indicator,
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
  the_new_name <- albertify(the_old_name)
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

# Make specific changes
names(df)[which(names(df) == 'country_name')] <- 'country'

# Create a prevalence survey variable
df$prevsurvey <- 0
has_prev_survey <- c(
  161, 6, 178, 179, 207, 206,
  11, 180, 10, 12, 15, 38, 182,
  13, 214, 165, 16, 185, 522, 18,
  189, 190, 20, 191, 198)
df$prevsurvey[df$country_number %in% has_prev_survey] <- 1

# Define which countries have high tb
df$hightb <- 0
has_high_tb <- c(
  168, 161, 135, 169, 6, 171, 170, 179,
  11, 163, 180, 10, 210, 194, 15, 184,
  195, 214, 165, 16, 26, 62, 217, 18, 189, 20,
  196, 191, 198)
df$hightb[df$country_number %in% has_high_tb] <- 1

# Define which countries have high mdr
df$highmdr <- 0
has_high_mdr <- c(168, 34, 161, 57, 6, 171, 179, 11,
                  163, 36, 180, 37, 61, 15, 184, 214,
                  165, 123, 16, 26, 7, 62, 187, 18,
                  39, 63, 41, 20, 196, 198)
df$highmdr[df$country_number %in% has_high_mdr] <- 1
# 
# # Create variable ALL TB DEATHS by WHO 
# 
# egen wndalltb = rowtotal (wndtb_all wndhtb_all)
# 
# *Create variable ALL TB DEATHS by IHME (i need to destring first).
# egen indalltb = rowtotal (indtb_all indhtb_all)
# 
# *CREATE ALL TB DEATHS IN CHILDREN AND ADULTS BY IHME
# egen indalltb_call = rowtotal (indtb_call indhtb_call)
# egen indalltb_aall = rowtotal (indtb_aall indhtb_aall)
# 
# * CREATE ALL TB DEATHS IN MALES AND FEMALES BY IHME
# egen indalltb_mall = rowtotal  (indtb_mall indhtb_mall)
# egen indalltb_fall = rowtotal  (indtb_fall indhtb_fall)
# 
# 
# *Para conseguir sumatorio de todos los valores,
# tabstat wndalltb, stat(sum)
# tabstat indalltb , stat(sum)


# Write csv
write_csv(df, 'data/combined_data.csv')

# Create a regional view
# region <- data.frame(region = sort(unique(df$who_g_whoregion)))
# these_names <- names(df)
# for (j in names(df)){
#   
# }


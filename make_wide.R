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
linkage <- data.frame(linkage)

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
df <- data.frame(df)
df$country_name <- as.character(df$country_name)
df$iso3 <- as.character(df$iso3)

# Get missing country names if relevant
for (i in 1:nrow(df)){
  if(is.na(df$country_name[i])){
    df$country_name[i] <-
      linkage$country_name[which(linkage$country_number == df$country_number[i])][1]
    df$iso3[i] <-
      linkage$iso3[which(linkage$country_number == df$country_number[i])][1]
  }
}

# Manually add Taiwan to Western Pacific region
df$who_g_whoregion[df$iso3 == 'TWN'] <- 'WPR'

# Clean up names
names(df) <- 
  gsub('mort', 'deaths', names(df))

# Remove se and lo/hi
df <- df[,!grepl('_lo|_hi|_se', names(df))]

# Remove commas from country names
df$country_name <- gsub(',', '', df$country_name)
df <- data.frame(df)
df <- df[,unique(c('country_name', 'country_number', 'iso3',
                   'who_g_whoregion', 'who_year', names(df)))]

# Remove all population columns from WHO
df <- df[,!grepl('pop', names(df))]

# Fix one pesky name
names(df)[names(df) == 'who_g_whoregion'] <- 'who_region'

# Remove the year
df$who_year <- NULL


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
    rate_number <- x_split[x_split %in% c('number', 'rate', 'incidence')]
    if('incidence' %in% rate_number){
      rate_number <- 'in'
    } else {
      if(length(rate_number) == 0){
        rate_number <- 'number'
      } 
      if(rate_number == 'number'){
        rate_number <- 'nd'
      } else {
        rate_number <- 'mr'
      }
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


okay_to_change <- which(!names(df) %in% 
                          c('country_name',
                            'country_number',
                            'iso3',
                            'who_region') &
                          !grepl('gb_', names(df)))
for (j in okay_to_change){
  if(!grepl('have', names(df)[j])){
    the_old_name <- names(df)[j]
    the_new_name <- albertify(the_old_name)
    # print(the_old_name)
    # print(the_new_name)
    if(the_old_name == the_new_name){
      message(paste0('Did not change variable ', the_old_name))
    } else {
      # messaC          the_new_name))
      names(df)[j] <-
        the_new_name
    }
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

# CREATE NEW VARIABLES
# Use format: source_sex_age_disease_indicator
# Alberto's stata code in comments

# # Create variable ALL TB DEATHS WHO
# egen wndalltb = rowtotal (wndtb_all wndhtb_all)
df$w_both_all_tbtotal_nd <- df$w_both_all_htb_nd + df$w_both_all_tb_nd

# *Create variable ALL TB DEATHS by IHME (i need to destring first).
# egen indalltb = rowtotal (indtb_all indhtb_all)
df$i_both_all_tbtotal_nd <- df$i_both_all_htb_nd + df$i_both_all_tb_nd

# Format variables from Global burden public
# and join to df
source('format_global_burden.R')
df <-
  df %>%
  left_join(gb,
            by = 'iso3')

# Write csv
write_csv(df, 'data/combined_data.csv')

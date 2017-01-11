library(dplyr)
library(readr)

# Read data
outcomes <- read_csv('data/TB_outcomes_2017-01-11.csv')

# Keep only years of interest
outcomes <-
  outcomes %>%
  filter(year == 2014) # 2015 not yet available

# Keep only variables of interest
outcomes <- 
  outcomes %>%
  dplyr::select(iso3, newrel_died, ret_nrel_died,
                ret_nrel_coh,
                newrel_coh)





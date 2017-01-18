library(dplyr)
library(readr)

# Read data
outcomes <- read_csv('data/TB_outcomes_2017-01-11.csv')

# Get an average of last three years
outcomes3 <- outcomes %>%
  filter(year %in% 2012:2014) %>%
  group_by(iso3) %>%
  summarise(newrel_died_avg_2012_to_2014 = mean(newrel_died, na.rm = TRUE), 
            ret_nrel_died_avg_2012_to_2014 = mean(ret_nrel_died, na.rm = TRUE),
            ret_nrel_coh_avg_2012_to_2014 = mean(ret_nrel_coh, na.rm = TRUE),
            newrel_coh_avg_2012_to_2014 = mean(newrel_coh, na.rm = TRUE))

# Keep only years of interest
outcomes <-
  outcomes %>%
  filter(year == 2014) # 2015 not yet available

# Join with the 3 year average
outcomes <- 
  left_join(x = outcomes,
            y = outcomes3,
            by = 'iso3')

# Keep only variables of interest
outcomes <- 
  outcomes %>%
  dplyr::select(iso3, newrel_died, ret_nrel_died,
                ret_nrel_coh,
                newrel_coh,
                newrel_died_avg_2012_to_2014,
                ret_nrel_died_avg_2012_to_2014,
                ret_nrel_coh_avg_2012_to_2014,
                newrel_coh_avg_2012_to_2014)

# Calculate new variable for Alberto
outcomes <- outcomes %>%
  mutate(case_fatality_rate_2014 = 
           (newrel_died + 
              ret_nrel_died ) /  
           ( ret_nrel_coh + 
               newrel_coh ),
         case_fatality_rate_2012_to_2014 = 
           (newrel_died_avg_2012_to_2014 +
           ret_nrel_died_avg_2012_to_2014) / 
           ( ret_nrel_coh_avg_2012_to_2014 +
               newrel_coh_avg_2012_to_2014  )
           )


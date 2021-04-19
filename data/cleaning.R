library(tidyverse)

# read in the data --------------------------------------------------------

csvs <- list.files("inputs", full.names = TRUE)
all_data <- map(csvs, read_csv)
names(all_data) <- str_remove_all(csvs, "(inputs/)|(.csv)")
list2env(all_data, envir = .GlobalEnv)
rm(csvs, all_data)


# race leaders ------------------------------------------------------------

# create df of just the lap leader
lap_leaders <- lap_times %>% 
  filter(position == 1) %>% 
  select(raceId, driverId, lap_num = lap)

# add driver and constructor names
lap_leaders <- lap_leaders %>% 
  left_join(races[, c('raceId', 'year', 'date')], by = 'raceId') %>% 
  left_join(drivers[, c('driverId', 'driverRef')], by = 'driverId') %>% 
  left_join(results[, c('raceId', 'driverId', 'constructorId')], by = c('raceId', 'driverId')) %>% 
  left_join(constructors[, c('constructorId', 'constructorRef')], by = 'constructorId') %>% 
  left_join(names_pretty, 'constructorRef')

# write out
write_csv(lap_leaders, 'data/lap_leaders.csv')

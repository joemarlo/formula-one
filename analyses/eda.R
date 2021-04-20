library(tidyverse)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers.R')

# read in the data
lap_leaders <- read_csv('data/lap_leaders.csv')

# mercedes wins in hybrid era
lap_leaders %>%
  group_by(raceId) %>% 
  filter(lap_num == max(lap_num)) %>% 
  ungroup() %>% 
  filter(year >= 2014) %>% 
  summarize(mean(constructor_pretty == 'Mercedes'))

# changes in race lead
lap_leaders %>%
  group_by(date, raceId) %>% 
  arrange(lap_num) %>% 
  mutate(lead_change = driverRef != lag(driverRef)) %>% 
  summarize(lead_changes = sum(lead_change, na.rm = TRUE),
            .groups = 'drop') %>% 
  arrange(date) %>%
  mutate(mean = zoo::rollmean(lead_changes, k = 60, align = 'right', na.pad = TRUE)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  labs(title = "Changes in race lead",
       subtitle = '60 race (~3 seasons) trailing rolling mean',
       x = NULL,
       y = NULL)
  
# plot all the races 2010+
lap_leaders %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(x = lap_num, y = reorder(raceId, date), fill = constructor_pretty)) +
  geom_tile(color = 'white') +
  scale_y_discrete(labels = NULL) +
  scale_fill_manual(values = team_colors) +
  facet_wrap(~year, ncol = 3, scales = 'free') +
  labs(title = "Mercedes's dominance is reflected by the sea of teal",
       subtitle = 'Color represents constructor lead by race lap',
       caption = 'Data from ergast\nmarlo.works',
       x = 'Lap number',
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
# ggsave('analyses/plots/sequences.png', width = 9, height = 11)

# last 10% of each race
lap_leaders %>% 
  filter(year >= 2010) %>% 
  group_by(raceId) %>% 
  mutate(lap_from_end = max(lap_num) - lap_num,
         percent_complete = lap_num / max(lap_num)) %>% 
  ungroup() %>% 
  filter(percent_complete >= 0.9) %>% 
  mutate(rev_lap = max(lap_num) - lap_from_end,
         rev_lap = rev_lap - min(rev_lap)) %>% 
  ggplot(aes(x = rev_lap, y = reorder(raceId, date), fill = constructor_pretty)) +
  geom_tile(color = 'white') +
  scale_y_discrete(labels = NULL) +
  scale_fill_manual(values = team_colors) +
  facet_wrap(~year, ncol = 3, scales = 'free') +
  labs(title = 'Constructor leader by lap',
       caption = 'Data from ergast\nmarlo.works',
       x = 'Laps from end',
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# entropy of races over time
lap_leaders %>% 
  group_by(year, raceId) %>% 
  summarize(entropy = entropy(driverRef),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = entropy, group = year)) +
  geom_boxplot() +
  labs(title = 'Entropy of race leader over time',
       caption = 'Data from ergast\nmarlo.works',
       x = NULL,
       y = NULL)

# calculate rolling entropy
entropy_rolling <- lap_leaders %>% 
  group_by(date, year, raceId) %>% 
  summarize(entropy = entropy(driverRef),
            .groups = 'drop') %>% 
  arrange(date) %>%
  mutate(mean = zoo::rollmean(entropy, k = 60, align = 'right', na.pad = TRUE))
  
# create summaries of each constructor era
era_ferrari <- lap_leaders %>% 
  filter(year %in% 1999:2004) %>% 
  summarize(xmin = min(date),
            xmax = max(date),
            ymin = min(entropy_rolling$mean, na.rm = TRUE)*1/1.1,
            ymax = max(entropy_rolling$mean, na.rm = TRUE)*1.1)
era_red_bull <- lap_leaders %>% 
  filter(year %in% 2010:2013) %>% 
  summarize(xmin = min(date),
            xmax = max(date),
            ymin = min(entropy_rolling$mean, na.rm = TRUE)*1/1.1,
            ymax = max(entropy_rolling$mean, na.rm = TRUE)*1.1)
era_mercedes <- lap_leaders %>% 
  filter(year %in% 2014:2020) %>% 
  summarize(xmin = min(date),
            xmax = max(date),
            ymin = min(entropy_rolling$mean, na.rm = TRUE)*1/1.1,
            ymax = max(entropy_rolling$mean, na.rm = TRUE)*1.1)

# plot rolling average of entropy
entropy_rolling %>% 
  ggplot() +
  geom_rect(data = era_ferrari,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.3, fill = mute_color("#DC0000")) +
  geom_rect(data = era_red_bull,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.3, fill = mute_color("#0600EF")) +
  geom_rect(data = era_mercedes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.3, fill = mute_color("#00D2BE")) +
  geom_text(data = tibble(x = as.Date(c('2002-01-01', '2012-01-01', '2017-06-01')),
                           y = rep(1.65),
                           label = c("Ferrari", "Red Bull", "Mercedes")),
            aes(x = x, y = y, label = label),
            color = 'grey95', size = 15, angle = -90, fontface = 'bold') +
  geom_line(aes(x = date, y = mean), color = 'grey30', size = 0.9) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  coord_cartesian(xlim = c(as.Date('1999-06-01'), as.Date('2021-05-01'))) +
  labs(title = 'Race excitement falls mid-decade',
       subtitle = 'Entropy of the race lead position. 60 race (~3 seasons) trailing rolling mean.',
       caption = 'Data from ergast\nmarlo.works',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
# ggsave('analyses/plots/entropy.png', width = 9, height = 6)

# entropy of last 10 percent of the race
lap_leaders %>% 
  group_by(raceId) %>% 
  mutate(lap_from_end = max(lap_num) - lap_num,
         percent_complete = lap_num / max(lap_num)) %>% 
  ungroup() %>% 
  filter(percent_complete >= 0.9) %>% 
  group_by(date, year, raceId) %>% 
  summarize(entropy = sequenchr::shannon_entropy(driverRef),
            .groups = 'drop') %>% 
  arrange(date) %>%
  mutate(mean = zoo::rollmean(entropy, k = 60, align = 'right', na.pad = TRUE)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Entropy of the last 10% of the race',
       subtitle = '60 race (~3 seasons) trailing rolling mean',
       caption = 'Data from ergast',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# top 5 most exciting races
top_five <- entropy_rolling %>% 
  slice_max(entropy, n = 5) 
top_five %>% 
  inner_join(lap_leaders, by = 'raceId') %>% 
  # group_by(raceId) %>% filter(lap_num == max(lap_num))
  left_join(tibble(
    raceId = top_five$raceId,
    label = c("1. Raikkonen @ Australia 2013", "2. Kubica @ Canada 2008", "3. Hamilton @ China 2011", "4. Alonso @ Japan 2008", "5. Raikkonen win @ Japan 2005")
  ), by = 'raceId') %>%
  mutate(driver_clean = str_replace_all(driverRef, "_", " "),
         driver_clean = str_to_title(driver_clean)) %>% 
  ggplot(aes(x = lap_num, y = reorder(label, -entropy), fill = driver_clean)) +
  geom_tile(color = 'white') +
  labs(title = "Top five 'most exciting' races by entropy",
       subtitle = '1996-2020. Color represents race leader.',
       caption = 'Data from ergast\nmarlo.works',
       x = 'Lap number',
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
# ggsave('analyses/plots/top_five.png', width = 8, height = 5)

# top 5's changes
lap_leaders %>%
  filter(raceId %in% top_five$raceId) %>% 
  arrange(lap_num) %>% 
  group_by(date, raceId) %>% 
  mutate(lead_change = driverRef != lag(driverRef)) %>% 
  summarize(lead_changes = sum(lead_change, na.rm = TRUE),
            .groups = 'drop')

# top 5 least exciting races
entropy_rolling %>% 
  slice_min(entropy, n = 5)

# how to measure midfield? entropy of each position
source('data/cleaning.R')
entropy_by_position <- lap_times %>% 
  na.omit() %>% 
  group_by(raceId, position) %>% 
  arrange(lap) %>% 
  summarize(entropy = entropy(driverId),
            .groups = 'drop') %>% 
  left_join(distinct(lap_leaders, raceId, year, date), by = c('raceId'))

# plot the entropy by position by race
p_data <- entropy_by_position %>% 
  filter(position < 21) %>%
  arrange(date) %>% 
  mutate(newRaceId = group_indices(., date))
p_labels <- p_data %>% 
  distinct(date, year, newRaceId) %>% 
  group_by(year) %>% 
  mutate(year = ifelse(date == min(date), year, NA)) %>% 
  ungroup()
p_data %>%
  ggplot(aes(x = newRaceId, y = position, fill = entropy)) +
  geom_tile() +
  scale_x_continuous(labels = p_labels$year[!is.na(p_labels$year)], 
                     breaks = which(!is.na(p_labels$year))) +
  labs(title = 'Excitement in the midfield peaked in the 2013 season',
       subtitle = 'Entropy by race position. Higher entropy == more exciting.',
       caption = 'Data from ergast\nmarlo.works',
       x = NULL,
       y = "Position",
       fill = "Entropy") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
# ggsave('analyses/plots/midfield.png', width = 8, height = 5)

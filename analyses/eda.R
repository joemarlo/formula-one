library(tidyverse)
source('analyses/plots/ggplot_settings.R')

# read in the data
lap_leaders <- read_csv('data/lap_leaders.csv')

# plot all the races 2010+
lap_leaders %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(x = lap_num, y = reorder(raceId, date), fill = constructor_pretty)) +
  geom_tile(color = 'white') +
  scale_y_discrete(labels = NULL) +
  scale_fill_manual(values = team_colors) +
  facet_wrap(~year, ncol = 3, scales = 'free') +
  labs(title = 'Race leader by lap',
       caption = 'Data from ergast\nmarlo.works',
       x = 'Lap number',
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
# ggsave('analysis/plots/sequences.png', width = 9, height = 11)

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
  labs(title = 'Race leader by lap',
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
  summarize(entropy = sequenchr::shannon_entropy(driverRef),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = entropy, group = year)) +
  geom_boxplot() +
  labs(title = 'Entropy of race leader over time',
       caption = 'Data from ergast',
       x = NULL,
       y = NULL)
lap_leaders %>% 
  group_by(date, year, raceId) %>% 
  summarize(entropy = sequenchr::shannon_entropy(driverRef),
            .groups = 'drop') %>% 
  mutate(mean = zoo::rollmean(entropy, k = 40, align = 'right', na.pad = TRUE)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Entropy of the race leader',
       subtitle = '40 race (~2 seasons) rolling mean',
       caption = 'Data from ergast',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
# ggsave('analysis/plots/entropy.png', width = 9, height = 5)

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
  mutate(mean = zoo::rollmean(entropy, k = 40, align = 'right', na.pad = TRUE)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Entropy of the last 10% of the race',
       subtitle = '40 race (~2 seasons) rolling mean',
       caption = 'Data from ergast',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# how to measure midfield?
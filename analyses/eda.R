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
       caption = 'Data from ergast\nmarlo.works',
       x = NULL,
       y = NULL)

# calculate rolling entropy
entropy_rolling <- lap_leaders %>% 
  group_by(date, year, raceId) %>% 
  summarize(entropy = sequenchr::shannon_entropy(driverRef),
            .groups = 'drop') %>% 
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
                           y = rep(0.7),
                           label = c("Ferrari", "Red Bull", "Mercedes")),
            aes(x = x, y = y, label = label),
            color = 'grey75', size = 10, angle = -90, fontface = 'bold') +
  geom_line(aes(x = date, y = mean), color = 'grey30', size = 0.9) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  coord_cartesian(xlim = c(as.Date('1999-06-01'), as.Date('2021-05-01'))) +
  labs(title = 'Entropy of the race lead position',
       subtitle = '60 race (~3 seasons) rolling mean',
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
  mutate(mean = zoo::rollmean(entropy, k = 60, align = 'right', na.pad = TRUE)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Entropy of the last 10% of the race',
       subtitle = '60 race (~3 seasons) rolling mean',
       caption = 'Data from ergast',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# how to measure midfield?

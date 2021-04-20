library(tidyverse)
library(TraMineR)
library(sequenchr)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers.R')

# read in the data
lap_leaders <- read_csv('data/lap_leaders.csv')
lap_leaders_wide <- lap_leaders %>%
  dplyr::select(raceId, lap_num, driverRef) %>% 
  pivot_wider(names_from = lap_num, values_from = driverRef)

# define alphabet as all unique states
alphabet <- lap_leaders_wide[,-1] %>% unlist() %>% unique() %>% sort()
labels <- as.factor(unique(lap_leaders$driverRef))

# create state sequence object
lap_leaders_seq <- seqdef(
  data = lap_leaders_wide[, -1], 
  alphabet = alphabet, 
  id = lap_leaders_wide$raceId,
  labels = labels,
  xtstep = 1)

# launch sequenchr
# launch_sequenchr(lap_leaders_seq)

# compute optimal matching distances
dist_mat <- seqdist(lap_leaders_seq, method = "OM", sm = "TRATE", with.missing = TRUE)


# outliers ----------------------------------------------------------------

# hist of distances
hist(dist_mat[lower.tri(dist_mat)])

# which race has the highest average distance? ie the most dissimilar
# most_dissimilar <- lap_leaders_wide[which.max(rowMeans(dist_mat)),]
most_dissimilar <- tibble(raceId = lap_leaders_wide$raceId,
       dist_mean = rowMeans(dist_mat)) %>% 
  slice_max(order_by = dist_mean, n = 10)
  

# clustering --------------------------------------------------------------

# cluster the data
cluster_model <- fastcluster::hclust(as.dist(dist_mat), method = "ward.D2")

# select k
k_range <- c(2, 30)
cluster_stats <- sequenchr::cluster_stats(dist_mat, cluster_model, k_range[1], k_range[2])
cluster_stats$k[which.max(cluster_stats$ch)]
cluster_stats$k[which.max(cluster_stats$silhouette)]

# plot stats
cluster_stats %>% 
  select(k, ch_norm, silhouette_norm) %>% 
  pivot_longer(-k) %>% 
  ggplot(aes(x = k, y = value, color = name)) +
  geom_line()

# create the cluster labels
k <- 18
clusters <- stats::cutree(cluster_model, k = k)
clusters <- tibble(
  raceId = lap_leaders_wide$raceId,
  cluster = clusters
) %>% 
  left_join(lap_leaders[, c('raceId', 'year')]) 

hist(clusters$cluster)

# clusters by modal sequence
lap_leaders %>% 
  left_join(clusters, by = 'raceId') %>% 
  group_by(cluster, lap_num) %>% 
  summarize(driverRef = get_mode(driverRef)) %>% 
  ggplot(aes(x = lap_num, y = cluster, fill = driverRef)) +
  geom_tile(color = 'white') +
  labs(title = 'Modal sequences by cluster')

# distribution of cluster membership by year; could be misleading b/c of smoothing
clusters %>% 
  ggplot(aes(x = cluster, y = as.factor(year))) +
  ggridges::geom_density_ridges() +
  labs(title = 'Distribution of cluster membership by year',
       x = 'Cluster',
       y = NULL)

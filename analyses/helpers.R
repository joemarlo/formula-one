# mute colors to pre-specified level
mute_color <- function(color) scales::muted(color, 70, 100)

# entropy
entropy <- function(x){
  tab <- table(x)
  prop <- tab/sum(tab)
  entropy <- -sum(ifelse(prop > 0, prop * log(prop, base = 2), 
                         0))
  n_changes <- length(rle(x)$lengths)-1
  weighted_entropy <- entropy * n_changes
  return(weighted_entropy)
}

x <- c("A", "B", "A", "B")
entropy(x)
x <- sort(x)
entropy(x)
x <- c("A", "A", "A", "B")
entropy(x)
x <- c("A", "A", "B", "B")
entropy(x)


# create list of the colors by team
team_colors <- c(
  'Red Bull' = '#0600EF',
  'McLaren' = '#FF8700',
  'Ferrari' = '#DC0000',
  'Mercedes' = '#00D2BE',
  'Lotus' = '#FFB800',
  'Force India' = '#F596C8',
  'Williams' = '#005AFF',
  'Renault' = '#FFF500',
  'Toro Rosso' = '#469BFF',
  'Brawn' = '#9ceb4d',
  'Alfa' = '#900000',
  'Alpha Tauri' = '#2B4562',
  'Racing Point' = '#F596C8',
  'Sauber' = '#9B0000'
)

# create list of prettified names
names_pretty <- tribble(
  ~constructorRef, ~constructor_pretty,
  'red_bull', 'Red Bull',
  'mclaren', 'McLaren',
  'ferrari', 'Ferrari',
  'mercedes', 'Mercedes',
  'lotus_f1', 'Lotus',
  'force_india', 'Force India',
  'williams', 'Williams',
  'renault', 'Renault',
  'toro_rosso', 'Toro Rosso',
  'brawn', 'Brawn',
  'alfa', 'Alfa',
  'alphatauri', 'Alpha Tauri',
  'racing_point', 'Racing Point',
  'sauber', 'Sauber'
)
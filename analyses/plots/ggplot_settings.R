require(ggplot2)
require(viridis)

# build custom theme
theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      text = element_text(family = "Helvetica",
                          color = "gray30"),
      plot.caption = element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50'),
      legend.key = element_rect(fill = NA)
    )
}

# set custom theme
theme_set(theme_custom())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_color_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_fill_discrete <- function(...) {
  scale_fill_viridis(..., discrete = TRUE)
}

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

# mute colors to pre-specified level
mute_color <- function(color) scales::muted(color, 70, 100)

library(tidyverse)

script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)

df<-read.csv("ov.csv")
# below find an example with a different dataset


# Create example sample data ----
dfe <- expand.grid(x = 1:5, y = 1:5)
dfe$z <- sample(1:25, nrow(dfe))  # Random values

ggplot(dfe, aes(x, y, fill = z)) +
  geom_tile() +  # Heatmap
  geom_text(aes(label = z), color = "white") +  # Overlay text
  scale_fill_viridis_c() + 
  theme_minimal()


ggplot(dfe, aes(x, y)) +
  geom_tile(aes(fill = z), alpha = 0.8) +  # Slight transparency
  geom_point(size = 4, color = "red") +  # Overlay points
  scale_fill_viridis_c() +
  theme_minimal()

# example with a different mark ----
# Create line segment data
lines <- data.frame(
  x = rep(1:5, each = 5),
  y = rep(1:5, times = 5),
  xend = rep(1:5, each = 5) + runif(25, -0.5, 0.5),  # Tilt variation
  yend = rep(1:5, times = 5) + runif(25, -0.5, 0.5)   # Tilt variation
)

ggplot(dfe, aes(x, y)) +
  geom_tile(aes(fill = z), alpha = 0.8) +  # Heatmap with transparency
  geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), 
               color = "black", size = 1) +  # Tilted lines
  scale_fill_viridis_c() +
  theme_minimal()


# example with nudged points ----
dfe <- expand.grid(x = 1:5, y = 1:5)
dfe$z <- sample(1:25, nrow(dfe))  # Random values
points1 <- dfe
points2 <- dfe
points1$type <- "A"
points2$type <- "B"
points <- rbind(points1, points2)
points1$nudge_x <- -0.2
points1$nudge_y <- -0.2
points2$nudge_x <- 0.2
points2$nudge_y <- 0.2
points <- rbind(points1, points2)

ggplot(dfe, aes(x, y)) +
  geom_tile(aes(fill = z), alpha = 0.8) + 
  geom_point(data = points, aes(x = x + nudge_x, y = y + nudge_y, color = type), size = 3) + 
  scale_fill_viridis_c() +
  theme_minimal()


# example using geom_spoke ----
# Assign random angles between 0 and π/2 (0 to 90 degrees)
points1$angle <- runif(nrow(points1), 0, pi/2)
points2$angle <- runif(nrow(points2), 0, pi/2)

# Assign random radius values and shorten them by 20%
points1$radius <- runif(nrow(points1), 0.25, 0.3) * 0.8  # 20% shorter
points2$radius <- runif(nrow(points2), 0.25, 0.3) * 0.8  # 20% shorter

points <- rbind(points1, points2)

# Compute end points for the spokes
points$x_end <- points$x + points$nudge_x + points$radius * cos(points$angle)
points$y_end <- points$y + points$nudge_y + points$radius * sin(points$angle)

# Plot using geom_spoke() and geom_point() for endpoints
ggplot(dfe, aes(x, y)) +
  geom_tile(aes(fill = z), alpha = 0.8) + 
  geom_spoke(data = points, aes(
    x = x + nudge_x, 
    y = y + nudge_y, 
    angle = angle, 
    radius = radius, 
    color = type
  ), linewidth = 1) + 
  geom_point(data = points, aes(x = x_end, y = y_end, color = type), size = 2) +  # Endpoints
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Spokes with Endpoints")


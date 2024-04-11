### Set-up

# Install package if necessary
install <- FALSE
if (install) {
  #install.packages("remotes")
  remotes::install_github("b-cubed-eu/gcube")
}

# Load packages
library(gcube)     # simulate biodiversity data cubes
library(sf)        # work with spatial objects
library(tidyverse) # data wrangling and visualisation
library(tidyterra) # visualisation spatraster objects

### Input

# Create a polygon to simulate occurrences
polygon <- st_polygon(list(cbind(c(500, 1000, 1000, 600, 200, 100, 500),
                                 c(200, 100, 700, 1000, 900, 500, 200))))

# Visualise
ggplot() +
  geom_sf(data = polygon) +
  theme_minimal()

### Occurrence process

# We generate occurrence points within the polygon using the
# simulate_occurrences() function.
?simulate_occurrences

# Say we want to have 100 occurrences in our plot over 10 years
# You can change the spatial clustering and the trend over time
# We visualise this with the helper functions used in simulate_occurrences()
# The number of occurrences are always drawn from a Poisson distribution
?simulate_timeseries

# 1) If we do not specify a temporal function we draw from a Poisson
# distribution for each time point
n_occurrences_indep <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = NA,
  seed = 123)

# Plot the simulated abundances over time using ggplot2
# We see that the average is close to 100 over time
tibble(
  n_occurrences = n_occurrences_indep,
  time_point = seq_along(n_occurrences_indep)
  ) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
    theme_minimal()

# 2) We can specify a function ourselves, e.g. the internal function
# simulate_random_walk to have a random walk over time and draw from Poisson
n_occurrences_walk <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = simulate_random_walk,
  sd_step = 1,
  seed = 123)

# Plot the simulated abundances over time using ggplot2
# We see that the average randomly goes down
tibble(
  n_occurrences = n_occurrences_walk,
  time_point = seq_along(n_occurrences_walk)
  ) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  theme_minimal()

# 3) We can specify a function ourselves that we created ourselves
# Here is an example for a linear trend
my_own_linear_function <- function(
    initial_average_occurrences = initial_average_occurrences,
    n_time_points = n_time_points,
    coef) {
  # Calculate new average abundances over time
  time <- seq_len(n_time_points) - 1
  lambdas <- initial_average_occurrences + (coef * time)

  # Identify where the lambda values become 0 or lower
  zero_or_lower_index <- which(lambdas <= 0)

  # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
  if (length(zero_or_lower_index) > 0) {
    zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
    lambdas[zero_or_lower_indices] <- 0
  }

  # Return average abundances
  return(lambdas)
}

n_occurrences_linear <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = my_own_linear_function,
  coef = 1,
  seed = 123)

# Plot the simulated abundances over time using ggplot2
# We see that the average slope is indeed close to 1
tibble(
  n_occurrences = n_occurrences_linear,
  time_point = seq_along(n_occurrences_linear)
  ) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  theme_minimal()

# We can also choose the amount of spatial clustering
?create_spatial_pattern

# There are defaults for random and clustered patterns, but you can also choose
# a value yourself

# 1) Lets look at the default where we have no clustering
rs_pattern_random <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = "random",
  seed = 123)

# We see values of high sampling probability randomly distributed
ggplot() +
  geom_spatraster(data = rs_pattern_random) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

# 2) Lets look at the default where we have clustering
rs_pattern_clustered <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = "clustered",
  seed = 123)

# We see values of high sampling probability clustered together
ggplot() +
  geom_spatraster(data = rs_pattern_clustered) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

# 3) Lets change the clustering ourselves
rs_pattern_clustered2 <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = 100,
  seed = 123)

# We see values of high sampling probability in fewer, larger clusters
ggplot() +
  geom_spatraster(data = rs_pattern_clustered2) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()


# We sample from this pattern using a different helper function
?sample_occurrences_from_raster

# If we for example sample 500 occurrences from the last raster, we see
# the sampling is according to the expected pattern
pts_occ_clustered2 <- sample_occurrences_from_raster(
  rs = rs_pattern_clustered2,
  ts = 500,
  seed = 123)

ggplot() +
  geom_spatraster(data = rs_pattern_clustered2) +
  geom_sf(data = pts_occ_clustered2) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()


# Now that we know how the helper functions work, we can generate occurrence
# points within the polygon using the simulate_occurrences() function.
# We can for example sample over 10 time points were we use a random walk over
# time with an initial average number of occurrences equal to 100

occurrences_df <- simulate_occurrences(
  plgn = polygon,
  initial_average_abundance = 100,
  n_time_points = 10,
  temporal_function = simulate_random_walk,
  sd_step = 1,
  spatial_autocorr = "clustered",
  seed = 123)

# This is the number of occurrences we have for each time point
occurrences_df %>%
  st_drop_geometry() %>%
  count(time_point) %>%
  ggplot(aes(x =  time_point, y = n)) +
    geom_point() +
    theme_minimal()

# This spatial distribution of the occurrences for each time point
ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = occurrences_df) +
  facet_wrap(~time_point, nrow = 2) +
  ggtitle("Distribution of occurrences for each time point") +
  theme_minimal()



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
# We can for example sample over 6 time points were we use a random walk over
# time with an initial average number of occurrences equal to 100
occurrences_df <- simulate_occurrences(
  plgn = polygon,
  initial_average_abundance = 100,
  n_time_points = 6,
  temporal_function = simulate_random_walk,
  sd_step = 1,
  spatial_autocorr = "random",
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


### Detection process

# We have our occurrences, but not all occurrences are generally observed.
# The detection of occurrences depends on the detection probability of a species
# and the sampling bias (includes both sampling bias and effort). This process
# can be simulated using the sample_observations() function.
?sample_observations

# Each observation will have a detection probability value (=the same for all
# observations) and a bias weight depending on its spatial distribution. The
# combination of detection probability and bias weight results in a sampling
# probability which is used to decide whether each occurrence is detected or not
# using (rbinom(1, 1, sampling_probability)).

# For bias there are 3 options: "no_bias", "polygon" or "manual".
# 1. With "no_bias", only the detection probability value will decide whether
# an occurrence is observed or not.
# 2. With "polygon", bias weights depend on their location inside or outside a
# given polygon with a certain bias strength. We can visualise this using the
# helper function apply_polygon_sampling_bias()
?apply_polygon_sampling_bias

# Lets say we have a road across our polygon
# Define the road width
road_width <- 50

# Create road points
road_points <- rbind(c(100, 500), c(1000, 500))

# Create road-like polygon within the given polygon
road_polygon <- st_linestring(road_points) %>%
  st_buffer(road_width) %>%
  st_intersection(polygon) %>%
  st_polygon() %>%
  st_sfc() %>%
  st_as_sf() %>%
  rename(geometry = x)

# Plot the result
ggplot() +
  geom_sf(data = polygon, fill = "lightgreen") +
  geom_sf(data = road_polygon) +
  theme_minimal()

# We can say that occurrences on or close to the road have 2x larger probability
# to be detected
occurrence_bias_df1 <- apply_polygon_sampling_bias(
  occurrences_df,
  bias_area = road_polygon,
  bias_strength = 2)

ggplot() +
  geom_sf(data = polygon, fill = "lightgreen") +
  geom_sf(data = road_polygon) +
  geom_sf(data = occurrence_bias_df1,
          aes(colour = factor(round(bias_weight, 3)))) +
  facet_wrap(~time_point, nrow = 2) +
  labs(title = "Distribution of occurrences for each time point",
       colour = "bias_weight") +
  theme_minimal()

# 3. With "manual", bias weights depend on their location inside grid cells
# of a given grid where each cell has its own value.
# We can visualise this using the helper function apply_manual_sampling_bias()
?apply_manual_sampling_bias

# Lets create a grid and give random bias weights to each cell
grid <- st_make_grid(
    polygon,
    n = c(10, 10),
    square = TRUE) %>%
  st_sf()
set.seed(123)
grid$bias_weight <- runif(nrow(grid), min = 0, max = 1)

# Plot the grid
ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = grid, alpha = 0) +
  geom_sf_text(data = grid, aes(label = round(bias_weight, 2))) +
  theme_minimal()


# We use the helper function. We only use time point 1
occurrence_bias_df2 <- apply_manual_sampling_bias(
  occurrences_df %>% dplyr::filter(time_point == 1),
  bias_weights = grid)

# We indeed see higher bias weights for occurrences where with higher values in
# The grid cells
ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = grid, alpha = 0) +
  geom_sf(data = occurrence_bias_df2,
          aes(colour = bias_weight)) +
  geom_sf_text(data = grid, aes(label = round(bias_weight, 2))) +
  theme_minimal()

# Now that we know how the helper functions work, we can simulate the detection
# process using the sample_observations() function.
# We can for example state that our species has a 0.9 detection probability and
# this time we say there is a very small chance to detect it close to the road
detections_df_raw <- sample_observations(
  occurrences_df,
  detection_probability = 0.9,
  sampling_bias = "polygon",
  bias_area = road_polygon,
  bias_strength = 0.1,
  seed = 123)

# We see that a lot of occurrences are detected due to the high detection
# probability, but this is not the case on the road where few are detected.
ggplot() +
  geom_sf(data = polygon, fill = "lightgreen") +
  geom_sf(data = road_polygon) +
  geom_sf(data = detections_df_raw,
          aes(colour = sampling_status)) +
  scale_colour_manual(values = c("blue", "red")) +
  facet_wrap(~time_point, nrow = 2) +
  labs(title = "Distribution of occurrences for each time point") +
  theme_minimal()


# We only keep the detected occurrences
detections_df <- detections_df_raw %>%
  dplyr::filter(sampling_status == "detected")

# We add coordinate uncertainty to the observations.
# This can be done using the add_coordinate_uncertainty() function.
?add_coordinate_uncertainty

# You can add a value for all observations or a vector with a single value for
# each observation.
# Lets add 25 meters of uncertainty to each observation
observations_df <- add_coordinate_uncertainty(
  observations = detections_df,
  coords_uncertainty_meters = 25)

# Created and sf object with uncertainty circles to visualise
buffered_observations <- st_buffer(
  observations_df,
  observations_df$coordinateUncertaintyInMeters)

ggplot() +
  geom_sf(data = polygon, fill = "lightgreen") +
  geom_sf(data = road_polygon) +
  geom_sf(data = buffered_observations,
          fill = alpha("firebrick", 0.3)) +
  geom_sf(data = observations_df, colour = "firebrick", size = 0.8) +
  facet_wrap(~time_point, nrow = 2) +
  labs(title = "Distribution of occurrences for each time point") +
  theme_minimal()


### Grid designation process

# Now we can make a data cube from our observations while taking into account
# the uncertainty. We can create the grid using the grid_designation() function.
?grid_designation

# We also need a grid. Each observation will be designated to a grid cell.
cube_grid <- st_make_grid(
  st_buffer(polygon, 25),
  n = c(20, 20),
  square = TRUE) %>%
  st_sf()

ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = cube_grid, alpha = 0) +
  theme_minimal()

# How does grid designation take coordinate uncertainty into account?
# The default is "uniform" randomisation where a random point within the
# uncertainty circle is taken as the location of the observation. This point is
# then designated to the overlapping grid cell.
# Another option is "normal" where a point is sampled from a bivariate Normal
# distribution with means equal to the observation point and the variance equal
# to (-coordinateUncertaintyInMeters^2) / (2 * log(1 - p_norm)) such that
# p_norm % of all possible samples from this Normal distribution fall within
# the uncertainty circle. This can be visualised by using these helper functions
?sample_from_uniform_circle
?sample_from_binormal_circle

# Lets create a random point with 25 meter coordinate uncertainty.
# We sample 1000 times using uniform and normal randomisation to look at the
# difference between the methods.
point_df <- tibble(
  x = 200,
  y = 500,
  coordinateUncertaintyInMeters = 25) %>%
  st_as_sf(coords = c("x", "y"))

n_sim <- 1000

# Take 1000 samples with uniform randomisation
list_samples_uniform <- vector("list", length = n_sim)
for (i in seq_len(n_sim)) {
  sampled_point_uniform <- sample_from_uniform_circle(point_df)
  sampled_point_uniform$sim <- i
  list_samples_uniform[[i]] <- sampled_point_uniform
}
samples_uniform_df <- do.call(rbind.data.frame, list_samples_uniform)

# Take 1000 samples with normal randomisation
list_samples_normal <- vector("list", length = n_sim)
for (i in seq_len(n_sim)) {
  sampled_point_normal <- sample_from_binormal_circle(point_df, p_norm = 0.95)
  sampled_point_normal$sim <- i
  list_samples_normal[[i]] <- sampled_point_normal
}
samples_normal_df <- do.call(rbind.data.frame, list_samples_normal)


# Visualise the samples
coordinates_uniform_df <- data.frame(st_coordinates(samples_uniform_df))
coordinates_normal_df <- data.frame(st_coordinates(samples_normal_df))
coordinates_point_df <- data.frame(st_coordinates(point_df))

scatter_uniform <- ggplot() +
  geom_point(data = coordinates_uniform_df,
             aes(x = X, y = Y),
             colour = "cornflowerblue") +
  geom_segment(data = coordinates_point_df,
               aes(x = X, xend = X + 25,
                   y = Y, yend = Y),
               linewidth = 1.5, colour = "darkgreen") +
  geom_label(aes(y = 503, x = 212.5, label = "25 m"), colour = "black",
            size = 5) +
  geom_point(data = coordinates_point_df,
             aes(x = X, y = Y),
             color = "firebrick", size = 2) +
  coord_fixed() +
  theme_minimal()

scatter_normal <- ggplot() +
  geom_point(data = coordinates_normal_df,
             aes(x = X, y = Y),
             colour = "cornflowerblue") +
  geom_segment(data = coordinates_point_df,
               aes(x = X, xend = X + 25,
                   y = Y, yend = Y),
               linewidth = 1.5, colour = "darkgreen") +
  geom_label(aes(y = 503, x = 212.5, label = "25 m"), colour = "black",
             size = 5) +
  stat_ellipse(data = coordinates_normal_df,
               aes(x = X, y = Y), level = 0.975, linewidth = 1.5, color = "firebrick") +
  geom_point(data = coordinates_point_df,
             aes(x = X, y = Y),
             color = "firebrick", size = 2) +
  coord_fixed() +
  theme_minimal()

# In the case of uniform randomisation, we see samples everywhere within the
# uncertainty circle.
ggExtra::ggMarginal(scatter_uniform, type = "histogram")

# In the case of normal randomisation, we see some samples outside the
# uncertainty circle. This should be 0.05 (1 - p_norm) %.
# They won't be used for grid designation.
ggExtra::ggMarginal(scatter_normal, type = "histogram")


# Now we know how to use the randomisation in grid_designation()
# By default we use uniform randomisation

# Create occurrence cube for time point 1
occurrence_cube_df <- grid_designation(
  observations_df %>% dplyr::filter(time_point == 1),
  cube_grid,
  seed = 123)

# Get sampled points within uncertainty circle by setting aggregate = FALSE
sampled_points <- grid_designation(
  observations_df %>% dplyr::filter(time_point == 1),
  cube_grid,
  seed = 123,
  aggregate = FALSE)

# Lets visualise were the samples were taken
ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = occurrence_cube_df, alpha = 0) +
  geom_sf_text(data = occurrence_cube_df, aes(label = n)) +
  geom_sf(data = buffered_observations %>% dplyr::filter(time_point == 1),
          fill = alpha("firebrick", 0.3)) +
  geom_sf(data = sampled_points, colour = "blue") +
  geom_sf(data = observations_df %>% dplyr::filter(time_point == 1),
          colour = "firebrick") +
  theme_minimal()

# Visualise minimal coordinate uncertainty
ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = occurrence_cube_df, aes(fill = min_coord_uncertainty),
          alpha = 0.3) +
  geom_sf_text(data = occurrence_cube_df, aes(label = n)) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

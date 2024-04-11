### Set-up

# Install package if necessary
install <- FALSE
if (install) {
  install.packages("remotes")
  remotes::install_github("b-cubed-eu/gcube")
}

# Load packages
library(gcube)     # simulate biodiversity data cubes
library(sf)        # work with spatial objects
library(tidyverse) # data wrangling and visualisation

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
?simulate_occurrences()

# Say we want to have 100 occurrences in our plot over 10 years
# You can change the spatial clustering and the trend over time
# We visualise this with the helper functions used in simulate_occurrences()
# The number of occurrences are always drawn from a Poisson distribution
?simulate_timeseries()

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

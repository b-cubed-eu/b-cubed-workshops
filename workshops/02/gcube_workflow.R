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
polygon <- st_polygon(list(cbind(c(5, 10, 10, 6, 2, 1, 5),
                                 c(2, 1, 7, 10, 9, 5, 2))))

# Visualise
ggplot() +
  geom_sf(data = polygon) +
  theme_minimal()

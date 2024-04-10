### Set-up
# Install package if necessary
install <- FALSE
if (install) {
  install.packages("remotes")
  remotes::install_github("b-cubed-eu/gcube")
}

# Load packages
library(gcube)

# Install or load necessary packages for the Palliative Care Project
# Code developed by Dilara Binoel, Clara Klimaschewski and Josefine Walthaler

# Version 1.0 # 2024-01-05, first commit

install_load_packages <- function(packages) {
  # Check and install required packages
  package.check <- lapply(
    packages,
    FUN = function(package_name) {
      if (!require(package_name, character.only = TRUE)) {
        install.packages(package_name, dependencies = TRUE)
        library(package_name, character.only = TRUE)
      }
    }
  )
}

# Usage example:
# install_load_packages(c("package1", "package2", "package3"))

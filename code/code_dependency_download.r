# The purpose of this script is to install the packages with the dependencies needed

# Load Packages --------------------------------------------
# Set a default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages
install.packages("remotes", repos = "http://cran.us.r-project.org")
remotes::install_version("usethis", version = "2.2.3")
remotes::install_version("REDCapR", version = "1.1.0")
remotes::install_version("ggplot2", version = "3.5.1")
remotes::install_version("lubridate", version = "1.9.3")
remotes::install_version("purr", version = "1.0.2")
remotes::install_version("ggrepel", version = "0.9.5")
remotes::install_version("tidyr", version = "1.3.1")
remotes::install_version("dplyr", version = "1.1.4")
remotes::install_version("digest", version = "0.6.36")
remotes::install_version("bslib", version = "0.7.0")
remotes::install_version("shinythemes", version = "1.2.0")
remotes::install_version("reactable", version = "0.4.4")
remotes::install_version("tidyr", version = "1.3.1")
remotes::install_version("patchwork", version = "1.2.0")


# Load libraries
library(remotes)
library(usethis)
library(REDCapR)
library(ggplot2)
library(lubridate)
library(purrr)
library(ggrepel)
library(dplyr)
library(digest)
library(bslib)
library(shinythemes)
library(reactable)
library(tidyr)
library(patchwork)
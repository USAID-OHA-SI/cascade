<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# cascade <img src="man/figures/logo.png" align="right" height="120" />
The goal of cascade is to automatically generate cascade plots

## Installation

You can install the development version of cascade like so:

``` r
# installing from OHA GH page
 install.packages("devtools")
 devtools::install_github("USAID-OHA-SI/cascade")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cascade)
library(gaglr)


# Setup local paths where msds live
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY20-23_20220812_v1_1_Zambia")

# load msd to be used
  df <- read_msd(file_path)

# populate all the metadata needed for cascade
  get_file_metadata(file_path)

# Check cascades avaible for creation
  plot_name

# Return a cascade data frame (number corresponds to position in char list)
# 1 = Standard cascade
  return_cascade(df, 1)

# Plot the cascade
# You will be 
  return_cascade_plot(df, export = F)

```


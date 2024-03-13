#### Preamble ####
# Purpose: Simulates the CES 2020 data
# Author: Liban Timir
# Date: 12 March 2024 
# Contact: liban.timir@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(555)

num_obs <- 1000

us_political_preferences <- tibble(
  race = sample(0:4, size = num_obs, replace = TRUE),
  faminc_new = sample(0:1, size = num_obs, replace = TRUE),
  support_prob = ((race + faminc_new) / 5),
) |>
  mutate(
    supports_biden = if_else(runif(n = num_obs) < support_prob, "yes", "no"),
    race = case_when(
      race == 0 ~ "White",
      race == 1 ~ "Black",
      race == 2 ~ "Hispanic",
      race == 3 ~ "Asian",
      race == 4 ~ "Other"
    ),
    faminc_new = case_when(
      faminc_new == 0 ~ "Under $30000",
      faminc_new == 1 ~ "$30000 to $49999",
      faminc_new == 2 ~ "$50000 to $99999",
      faminc_new == 3 ~ "$100000 to $199999",
      faminc_new == 4 ~ "$200000 or more"
    )
  ) |>
  select(-support_prob, supports_biden, race, faminc_new)

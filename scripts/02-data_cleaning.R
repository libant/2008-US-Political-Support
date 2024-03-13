#### Preamble ####
# Purpose: Cleans the raw CES 2008 data
# Author: Liban Timir
# Date: 12 March 2024
# Contact: liban.timir@utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(tidyverse)
library(dataverse)

#### Clean data ####
ces2020 <-
  get_dataframe_by_name(
    filename = "CES20_Common_OUTPUT_vv.csv",
    dataset = "10.7910/DVN/E9N6PH",
    server = "dataverse.harvard.edu",
    .f = read_csv
  ) |>
  select(votereg, CC20_410, faminc_new, race)

write_csv(ces2020, "ces2020.csv")

ces2020 <-
  read_csv(
    "ces2020.csv",
    col_types =
      cols(
        "votereg" = col_integer(),
        "CC20_410" = col_integer(),
        "race" = col_integer(),
        "faminc_new" = col_integer()
      )
  )

ces2020 <-
  ces2020 |>
  filter(votereg == 1,
         CC20_410 %in% c(1, 2)) |>
  mutate(
    voted_for = if_else(CC20_410 == 1, "Biden", "Trump"),
    voted_for = as_factor(voted_for),
    faminc_new = case_when(
      faminc_new == 0 ~ "Less than $10000",
      faminc_new == 1 ~ "$10000 to $19999",
      faminc_new == 2 ~ "$20000 to $29999",
      faminc_new == 3 ~ "$30000 to $39999",
      faminc_new == 4 ~ "$40000 to $49999",
      faminc_new == 5 ~ "$50000 to $59999",
      faminc_new == 6 ~ "$60000 to $69999",
      faminc_new == 7 ~ "$70000 to $79999",
      faminc_new == 8 ~ "$80000 to $99999",
      faminc_new == 9 ~ "$100000 to $119999",
      faminc_new == 10 ~ "$120000 to $149999",
      faminc_new == 11 ~ "$150000 to $199999",
      faminc_new == 12 ~ "$200000 to $249999",
      faminc_new == 13 ~ "$250000 to $349999",
      faminc_new == 14 ~ "$350000 to $499999",
      faminc_new == 15 ~ "$500000 or more",
      faminc_new == 16 ~ "Prefer not to say",
    ),
    faminc_new = factor(
      faminc_new,
      levels = c(
        "Less than $10000",
        "$10000 to $19999",
        "$20000 to $29999",
        "$30000 to $39999",
        "$40000 to $49999",
        "$50000 to $59999",
        "$60000 to $69999",
        "$70000 to $79999",
        "$80000 to $99999",
        "$100000 to $119999",
        "$120000 to $149999",
        "$150000 to $199999",
        "$200000 to $249999",
        "$250000 to $349999",
        "$350000 to $499999",
        "$500000 or more",
        "Prefer not to say"
      )
    ),
    race = case_when(
      race == 0 ~ "White",
      race == 1 ~ "Black",
      race == 2 ~ "Hispanic",
      race == 3 ~ "Asian",
      race == 4 ~ "Native American",
      race == 5 ~ "Middle Eastern",
      race == 6 ~ "Two or more races",
      race == 7 ~ "Other"
    ),
    race = factor(
      race,
      levels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian",
        "Native American",
        "Middle Eastern",
        "Two or more races",
        "Other"
      )
    )
  ) |>
  select(voted_for, faminc_new, race)

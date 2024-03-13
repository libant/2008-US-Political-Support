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
library(haven)

#### Clean data ####
ces2008 <-
  get_dataframe_by_name(
    filename = "cces_2008_common.dta",
    dataset = "1902.1/14003",
    server = "dataverse.harvard.edu",
    .f = read_dta
  ) |>
  select(votereg, CC20_410, gender, educ)

write_dta(ces2008, "cces_2008_common.dta")

ces2008 <-
  read_dta(
    "cces_2008_common.dta",
    col_types =
      cols(
        "votereg" = col_integer(),
        "CC20_410" = col_integer(),
        "gender" = col_integer(),
        "educ" = col_integer()
      )
  )

ces2008 <-
  ces2008 |>
  filter(votereg == 1,
         CC20_410 %in% c(1, 2)) |>
  mutate(
    voted_for = if_else(CC20_410 == 1, "Obama", "McCain"),
    voted_for = as_factor(voted_for),
    gender = if_else(gender == 1, "Male", "Female"),
    education = case_when(
      educ == 1 ~ "No HS",
      educ == 2 ~ "High school graduate",
      educ == 3 ~ "Some college",
      educ == 4 ~ "2-year",
      educ == 5 ~ "4-year",
      educ == 6 ~ "Post-grad"
    ),
    education = factor(
      education,
      levels = c(
        "No HS",
        "High school graduate",
        "Some college",
        "2-year",
        "4-year",
        "Post-grad"
      )
    )
  ) |>
  select(voted_for, gender, education)

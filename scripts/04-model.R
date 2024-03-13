#### Preamble ####
# Purpose: Models the political preference of the voter
# Author: Liban Timir
# Date: 12 March 2024
# Contact: Liban Timir
# License: MIT
# Pre-requisites: 01-download_data.R, 02-data_cleaning.R


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
ces2008 <-
  get_dataframe_by_name(
    filename = "cces_2008_common.dta",
    dataset = "1902.1/14003",
    server = "dataverse.harvard.edu",
    .f = read_dta
  ) |>
  select(votereg, CC20_410, gender, educ)

read_dta(ces2008, "cces_2008_common.dta")

### Model data ####
set.seed(555)

ces2008_reduced <- 
  ces2008 |> 
  slice_sample(n = 1000)

political_preferences <-
  stan_glm(
    voted_for ~ gender + education,
    data = ces2008_reduced,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 555
  )

#### Save and read model ####
saveRDS(
  political_preferences,
  file = "political_preferences.rds"
)

political_preferences <-
  readRDS(file = "political_preferences.rds")

#### Results of model ####
modelsummary(
  list(
    "Support Biden" = political_preferences
  ),
  statistic = "mad"
)

modelplot(political_preferences, conf_level = 0.9) +
  labs(x = "90 per cent credibility interval")




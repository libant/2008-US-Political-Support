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
library(dataverse)

#### Read data ####
ces2020 <-
  get_dataframe_by_name(
    filename = "CES20_Common_OUTPUT_vv.csv",
    dataset = "10.7910/DVN/E9N6PH",
    server = "dataverse.harvard.edu",
    .f = read_csv
  ) |>
  select(votereg, CC20_410, faminc_new, race)

write_csv(ces2020, "ces2020.csv")

### Model data ####
set.seed(555)

ces2020_reduced <- 
  ces2020 |> 
  slice_sample(n = 1000)

political_preferences <-
  stan_glm(
    voted_for ~ faminc_new + race,
    data = ces2020_reduced,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 555
  )

saveRDS(
  political_preferences,
  file = "political_preferences.rds"
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




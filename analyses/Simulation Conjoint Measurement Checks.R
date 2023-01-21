##### Analysis Script for "Can we measure effort in cognitive tasks?: Examining the application of Additive Conjoint Measurement and the Rasch model"

# Author: James Steele

##### In this script we examine Coefficient of Reproducibility and also check for cancellation axioms of Additive Conjoint Measurement
### As the latter is quite computationally intensive we only check things for the first trial of each dataset (i.e., first of the 500 simulated responses for each person-item pair)
### Also, in some of the simulations the ConjointChecks function is unable to run due to the matrix sizes produced for checks
  # Note, we do not run the cancellation checks for the datasets which show a Guttman pattern (here defined as a Cr >= 0.9) as we suspect they will be violated ahead of time
  # The actual Guttman response we won't try at all, but we check the Cr for all other datasets
  # Where any are not run these are commented out in the code

##### Note, I've been somewhat lazy with this and was running out of time for the submission deadline 
  # as such I've just copied and pasted the code rather than nesting varying conditions and simulating within loops

### Open required packages
library(tidyverse)
library(ConjointChecks)

### Functions

# To calculate Coefficient of Reproducibility for Guttman scaling
CR <- function(dataset) {
  dataset = as.data.frame(dataset)
  dataset_optimal = dataset[, order(apply(X = dataset, MARGIN = 2, 
                                          FUN = sum), decreasing = TRUE)]
  dataset_optimal = dataset_optimal[order(apply(X = dataset_optimal, 
                                                MARGIN = 1, FUN = sum), decreasing = TRUE), ]
  row_errors = setNames(apply(X = dataset_optimal, MARGIN = 1, 
                              FUN = function(x) {
                                total = sum(x)
                                ideal = c(rep(1, total), rep(0, length(x) - total))
                                sum(x != ideal)
                              }), rownames(dataset_optimal))
  total_errors = sum(row_errors)
  N = prod(dim(dataset_optimal))
  CR = 1 - (total_errors/N)
  CR
}

### Run ConjointChecks quicker
cores <- detectCores()/2

### Load all simulated datasets

load("datasets/oneRM_dat_50_5_narrow")
load("datasets/oneRM_dat_50_10_narrow")
load("datasets/oneRM_dat_50_20_narrow")
load("datasets/oneRM_dat_100_5_narrow")
load("datasets/oneRM_dat_100_10_narrow")
load("datasets/oneRM_dat_100_20_narrow")
load("datasets/oneRM_dat_250_5_narrow")
load("datasets/oneRM_dat_250_10_narrow")
load("datasets/oneRM_dat_250_20_narrow")
load("datasets/oneRM_dat_500_5_narrow")
load("datasets/oneRM_dat_500_10_narrow")
load("datasets/oneRM_dat_500_20_narrow")
load("datasets/oneRM_dat_1000_5_narrow")
load("datasets/oneRM_dat_1000_10_narrow")
load("datasets/oneRM_dat_1000_20_narrow")
load("datasets/oneRM_dat_50_5_wide")
load("datasets/oneRM_dat_50_10_wide")
load("datasets/oneRM_dat_50_20_wide")
load("datasets/oneRM_dat_100_5_wide")
load("datasets/oneRM_dat_100_10_wide")
load("datasets/oneRM_dat_100_20_wide")
load("datasets/oneRM_dat_250_5_wide")
load("datasets/oneRM_dat_250_10_wide")
load("datasets/oneRM_dat_250_20_wide")
load("datasets/oneRM_dat_500_5_wide")
load("datasets/oneRM_dat_500_10_wide")
load("datasets/oneRM_dat_500_20_wide")
load("datasets/oneRM_dat_1000_5_wide")
load("datasets/oneRM_dat_1000_10_wide")
load("datasets/oneRM_dat_1000_20_wide")

### Prepare data in response format for the first trial of each dataset
# Guttman
resp_guttman_50_5_narrow <- oneRM_dat_50_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_50_10_narrow <- oneRM_dat_50_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_50_20_narrow <- oneRM_dat_50_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_5_narrow <- oneRM_dat_100_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_10_narrow <- oneRM_dat_100_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_20_narrow <- oneRM_dat_100_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_5_narrow <- oneRM_dat_250_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_10_narrow <- oneRM_dat_250_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_20_narrow <- oneRM_dat_250_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_5_narrow <- oneRM_dat_500_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_10_narrow <- oneRM_dat_500_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_20_narrow <- oneRM_dat_500_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_5_narrow <- oneRM_dat_1000_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_10_narrow <- oneRM_dat_1000_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_20_narrow <- oneRM_dat_1000_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_50_5_wide <- oneRM_dat_50_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_50_10_wide <- oneRM_dat_50_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_50_20_wide <- oneRM_dat_50_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_5_wide <- oneRM_dat_100_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_10_wide <- oneRM_dat_100_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_100_20_wide <- oneRM_dat_100_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_5_wide <- oneRM_dat_250_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_10_wide <- oneRM_dat_250_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_250_20_wide <- oneRM_dat_250_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_5_wide <- oneRM_dat_500_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_10_wide <- oneRM_dat_500_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_500_20_wide <- oneRM_dat_500_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_5_wide <- oneRM_dat_1000_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_10_wide <- oneRM_dat_1000_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

resp_guttman_1000_20_wide <- oneRM_dat_1000_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_guttman) %>%
  pivot_wider(values_from = "response_guttman", names_from = "item") %>%
  dplyr::select(-person)

# Rasch
resp_rasch_50_5_narrow <- oneRM_dat_50_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_50_10_narrow <- oneRM_dat_50_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_50_20_narrow <- oneRM_dat_50_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_5_narrow <- oneRM_dat_100_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_10_narrow <- oneRM_dat_100_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_20_narrow <- oneRM_dat_100_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_5_narrow <- oneRM_dat_250_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_10_narrow <- oneRM_dat_250_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_20_narrow <- oneRM_dat_250_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_5_narrow <- oneRM_dat_500_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_10_narrow <- oneRM_dat_500_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_20_narrow <- oneRM_dat_500_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_5_narrow <- oneRM_dat_1000_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_10_narrow <- oneRM_dat_1000_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_20_narrow <- oneRM_dat_1000_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_50_5_wide <- oneRM_dat_50_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_50_10_wide <- oneRM_dat_50_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_50_20_wide <- oneRM_dat_50_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_5_wide <- oneRM_dat_100_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_10_wide <- oneRM_dat_100_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_100_20_wide <- oneRM_dat_100_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_5_wide <- oneRM_dat_250_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_10_wide <- oneRM_dat_250_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_250_20_wide <- oneRM_dat_250_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_5_wide <- oneRM_dat_500_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_10_wide <- oneRM_dat_500_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_500_20_wide <- oneRM_dat_500_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_5_wide <- oneRM_dat_1000_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_10_wide <- oneRM_dat_1000_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

resp_rasch_1000_20_wide <- oneRM_dat_1000_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_rasch) %>%
  pivot_wider(values_from = "response_rasch", names_from = "item") %>%
  dplyr::select(-person)

# Rasch high error
resp_raschhigherror_50_5_narrow <- oneRM_dat_50_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_50_10_narrow <- oneRM_dat_50_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_50_20_narrow <- oneRM_dat_50_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_5_narrow <- oneRM_dat_100_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_10_narrow <- oneRM_dat_100_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_20_narrow <- oneRM_dat_100_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_5_narrow <- oneRM_dat_250_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_10_narrow <- oneRM_dat_250_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_20_narrow <- oneRM_dat_250_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_5_narrow <- oneRM_dat_500_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_10_narrow <- oneRM_dat_500_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_20_narrow <- oneRM_dat_500_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_50_5_wide <- oneRM_dat_50_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_50_10_wide <- oneRM_dat_50_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_50_20_wide <- oneRM_dat_50_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_5_wide <- oneRM_dat_100_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_10_wide <- oneRM_dat_100_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_100_20_wide <- oneRM_dat_100_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_5_wide <- oneRM_dat_250_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_10_wide <- oneRM_dat_250_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_250_20_wide <- oneRM_dat_250_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_5_wide <- oneRM_dat_500_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_10_wide <- oneRM_dat_500_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_500_20_wide <- oneRM_dat_500_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_5_wide <- oneRM_dat_1000_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_10_wide <- oneRM_dat_1000_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschhigherror_1000_20_wide <- oneRM_dat_1000_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschhigherror) %>%
  pivot_wider(values_from = "response_raschhigherror", names_from = "item") %>%
  dplyr::select(-person)

# Rasch low error
resp_raschlowerror_50_5_narrow <- oneRM_dat_50_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_50_10_narrow <- oneRM_dat_50_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_50_20_narrow <- oneRM_dat_50_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_5_narrow <- oneRM_dat_100_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_10_narrow <- oneRM_dat_100_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_20_narrow <- oneRM_dat_100_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_5_narrow <- oneRM_dat_250_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_10_narrow <- oneRM_dat_250_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_20_narrow <- oneRM_dat_250_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_5_narrow <- oneRM_dat_500_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_10_narrow <- oneRM_dat_500_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_20_narrow <- oneRM_dat_500_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_50_5_wide <- oneRM_dat_50_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_50_10_wide <- oneRM_dat_50_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_50_20_wide <- oneRM_dat_50_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_5_wide <- oneRM_dat_100_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_10_wide <- oneRM_dat_100_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_100_20_wide <- oneRM_dat_100_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_5_wide <- oneRM_dat_250_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_10_wide <- oneRM_dat_250_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_250_20_wide <- oneRM_dat_250_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_5_wide <- oneRM_dat_500_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_10_wide <- oneRM_dat_500_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_500_20_wide <- oneRM_dat_500_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_5_wide <- oneRM_dat_1000_5_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_10_wide <- oneRM_dat_1000_10_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

resp_raschlowerror_1000_20_wide <- oneRM_dat_1000_20_wide %>% 
  filter(trial == 1) %>%
  dplyr::select(person, item, response_raschlowerror) %>%
  pivot_wider(values_from = "response_raschlowerror", names_from = "item") %>%
  dplyr::select(-person)

### First check the Cr for each dataset
# Guttman
c_r_guttman_50_5_narrow  <- CR(resp_guttman_50_5_narrow)
c_r_guttman_50_10_narrow  <- CR(resp_guttman_50_10_narrow)
c_r_guttman_50_20_narrow  <- CR(resp_guttman_50_20_narrow)
c_r_guttman_100_5_narrow  <- CR(resp_guttman_100_5_narrow)
c_r_guttman_100_10_narrow  <- CR(resp_guttman_100_10_narrow)
c_r_guttman_100_20_narrow  <- CR(resp_guttman_100_20_narrow)
c_r_guttman_250_5_narrow  <- CR(resp_guttman_250_5_narrow)
c_r_guttman_250_10_narrow  <- CR(resp_guttman_250_10_narrow)
c_r_guttman_250_20_narrow  <- CR(resp_guttman_250_20_narrow)
c_r_guttman_500_5_narrow  <- CR(resp_guttman_500_5_narrow)
c_r_guttman_500_10_narrow  <- CR(resp_guttman_500_10_narrow)
c_r_guttman_500_20_narrow  <- CR(resp_guttman_500_20_narrow)
c_r_guttman_1000_5_narrow  <- CR(resp_guttman_1000_5_narrow)
c_r_guttman_1000_10_narrow  <- CR(resp_guttman_1000_10_narrow)
c_r_guttman_1000_20_narrow  <- CR(resp_guttman_1000_20_narrow)

c_r_guttman_50_5_wide  <- CR(resp_guttman_50_5_wide)
c_r_guttman_50_10_wide  <- CR(resp_guttman_50_10_wide)
c_r_guttman_50_20_wide  <- CR(resp_guttman_50_20_wide)
c_r_guttman_100_5_wide  <- CR(resp_guttman_100_5_wide)
c_r_guttman_100_10_wide  <- CR(resp_guttman_100_10_wide)
c_r_guttman_100_20_wide  <- CR(resp_guttman_100_20_wide)
c_r_guttman_250_5_wide  <- CR(resp_guttman_250_5_wide)
c_r_guttman_250_10_wide  <- CR(resp_guttman_250_10_wide)
c_r_guttman_250_20_wide  <- CR(resp_guttman_250_20_wide)
c_r_guttman_500_5_wide  <- CR(resp_guttman_500_5_wide)
c_r_guttman_500_10_wide  <- CR(resp_guttman_500_10_wide)
c_r_guttman_500_20_wide  <- CR(resp_guttman_500_20_wide)
c_r_guttman_1000_5_wide  <- CR(resp_guttman_1000_5_wide)
c_r_guttman_1000_10_wide  <- CR(resp_guttman_1000_10_wide)
c_r_guttman_1000_20_wide  <- CR(resp_guttman_1000_20_wide)

# Rasch
c_r_rasch_50_5_narrow  <- CR(resp_rasch_50_5_narrow)
c_r_rasch_50_10_narrow  <- CR(resp_rasch_50_10_narrow)
c_r_rasch_50_20_narrow  <- CR(resp_rasch_50_20_narrow)
c_r_rasch_100_5_narrow  <- CR(resp_rasch_100_5_narrow)
c_r_rasch_100_10_narrow  <- CR(resp_rasch_100_10_narrow)
c_r_rasch_100_20_narrow  <- CR(resp_rasch_100_20_narrow)
c_r_rasch_250_5_narrow  <- CR(resp_rasch_250_5_narrow)
c_r_rasch_250_10_narrow  <- CR(resp_rasch_250_10_narrow)
c_r_rasch_250_20_narrow  <- CR(resp_rasch_250_20_narrow)
c_r_rasch_500_5_narrow  <- CR(resp_rasch_500_5_narrow)
c_r_rasch_500_10_narrow  <- CR(resp_rasch_500_10_narrow)
c_r_rasch_500_20_narrow  <- CR(resp_rasch_500_20_narrow)
c_r_rasch_1000_5_narrow  <- CR(resp_rasch_1000_5_narrow)
c_r_rasch_1000_10_narrow  <- CR(resp_rasch_1000_10_narrow)
c_r_rasch_1000_20_narrow  <- CR(resp_rasch_1000_20_narrow)

c_r_rasch_50_5_wide  <- CR(resp_rasch_50_5_wide)
c_r_rasch_50_10_wide  <- CR(resp_rasch_50_10_wide)
c_r_rasch_50_20_wide  <- CR(resp_rasch_50_20_wide)
c_r_rasch_100_5_wide  <- CR(resp_rasch_100_5_wide)
c_r_rasch_100_10_wide  <- CR(resp_rasch_100_10_wide)
c_r_rasch_100_20_wide  <- CR(resp_rasch_100_20_wide)
c_r_rasch_250_5_wide  <- CR(resp_rasch_250_5_wide)
c_r_rasch_250_10_wide  <- CR(resp_rasch_250_10_wide)
c_r_rasch_250_20_wide  <- CR(resp_rasch_250_20_wide)
c_r_rasch_500_5_wide  <- CR(resp_rasch_500_5_wide)
c_r_rasch_500_10_wide  <- CR(resp_rasch_500_10_wide)
c_r_rasch_500_20_wide  <- CR(resp_rasch_500_20_wide)
c_r_rasch_1000_5_wide  <- CR(resp_rasch_1000_5_wide)
c_r_rasch_1000_10_wide  <- CR(resp_rasch_1000_10_wide)
c_r_rasch_1000_20_wide  <- CR(resp_rasch_1000_20_wide)

# Rasch high error
c_r_raschhigherror_50_5_narrow  <- CR(resp_raschhigherror_50_5_narrow)
c_r_raschhigherror_50_10_narrow  <- CR(resp_raschhigherror_50_10_narrow)
c_r_raschhigherror_50_20_narrow  <- CR(resp_raschhigherror_50_20_narrow)
c_r_raschhigherror_100_5_narrow  <- CR(resp_raschhigherror_100_5_narrow)
c_r_raschhigherror_100_10_narrow  <- CR(resp_raschhigherror_100_10_narrow)
c_r_raschhigherror_100_20_narrow  <- CR(resp_raschhigherror_100_20_narrow)
c_r_raschhigherror_250_5_narrow  <- CR(resp_raschhigherror_250_5_narrow)
c_r_raschhigherror_250_10_narrow  <- CR(resp_raschhigherror_250_10_narrow)
c_r_raschhigherror_250_20_narrow  <- CR(resp_raschhigherror_250_20_narrow)
c_r_raschhigherror_500_5_narrow  <- CR(resp_raschhigherror_500_5_narrow)
c_r_raschhigherror_500_10_narrow  <- CR(resp_raschhigherror_500_10_narrow)
c_r_raschhigherror_500_20_narrow  <- CR(resp_raschhigherror_500_20_narrow)
c_r_raschhigherror_1000_5_narrow  <- CR(resp_raschhigherror_1000_5_narrow)
c_r_raschhigherror_1000_10_narrow  <- CR(resp_raschhigherror_1000_10_narrow)
c_r_raschhigherror_1000_20_narrow  <- CR(resp_raschhigherror_1000_20_narrow)

c_r_raschhigherror_50_5_wide  <- CR(resp_raschhigherror_50_5_wide)
c_r_raschhigherror_50_10_wide  <- CR(resp_raschhigherror_50_10_wide)
c_r_raschhigherror_50_20_wide  <- CR(resp_raschhigherror_50_20_wide)
c_r_raschhigherror_100_5_wide  <- CR(resp_raschhigherror_100_5_wide)
c_r_raschhigherror_100_10_wide  <- CR(resp_raschhigherror_100_10_wide)
c_r_raschhigherror_100_20_wide  <- CR(resp_raschhigherror_100_20_wide)
c_r_raschhigherror_250_5_wide  <- CR(resp_raschhigherror_250_5_wide)
c_r_raschhigherror_250_10_wide  <- CR(resp_raschhigherror_250_10_wide)
c_r_raschhigherror_250_20_wide  <- CR(resp_raschhigherror_250_20_wide)
c_r_raschhigherror_500_5_wide  <- CR(resp_raschhigherror_500_5_wide)
c_r_raschhigherror_500_10_wide  <- CR(resp_raschhigherror_500_10_wide)
c_r_raschhigherror_500_20_wide  <- CR(resp_raschhigherror_500_20_wide)
c_r_raschhigherror_1000_5_wide  <- CR(resp_raschhigherror_1000_5_wide)
c_r_raschhigherror_1000_10_wide  <- CR(resp_raschhigherror_1000_10_wide)
c_r_raschhigherror_1000_20_wide  <- CR(resp_raschhigherror_1000_20_wide)

# Rasch low error
c_r_raschlowerror_50_5_narrow  <- CR(resp_raschlowerror_50_5_narrow)
c_r_raschlowerror_50_10_narrow  <- CR(resp_raschlowerror_50_10_narrow)
c_r_raschlowerror_50_20_narrow  <- CR(resp_raschlowerror_50_20_narrow)
c_r_raschlowerror_100_5_narrow  <- CR(resp_raschlowerror_100_5_narrow)
c_r_raschlowerror_100_10_narrow  <- CR(resp_raschlowerror_100_10_narrow)
c_r_raschlowerror_100_20_narrow  <- CR(resp_raschlowerror_100_20_narrow)
c_r_raschlowerror_250_5_narrow  <- CR(resp_raschlowerror_250_5_narrow)
c_r_raschlowerror_250_10_narrow  <- CR(resp_raschlowerror_250_10_narrow)
c_r_raschlowerror_250_20_narrow  <- CR(resp_raschlowerror_250_20_narrow)
c_r_raschlowerror_500_5_narrow  <- CR(resp_raschlowerror_500_5_narrow)
c_r_raschlowerror_500_10_narrow  <- CR(resp_raschlowerror_500_10_narrow)
c_r_raschlowerror_500_20_narrow  <- CR(resp_raschlowerror_500_20_narrow)
c_r_raschlowerror_1000_5_narrow  <- CR(resp_raschlowerror_1000_5_narrow)
c_r_raschlowerror_1000_10_narrow  <- CR(resp_raschlowerror_1000_10_narrow)
c_r_raschlowerror_1000_20_narrow  <- CR(resp_raschlowerror_1000_20_narrow)

c_r_raschlowerror_50_5_wide  <- CR(resp_raschlowerror_50_5_wide)
c_r_raschlowerror_50_10_wide  <- CR(resp_raschlowerror_50_10_wide)
c_r_raschlowerror_50_20_wide  <- CR(resp_raschlowerror_50_20_wide)
c_r_raschlowerror_100_5_wide  <- CR(resp_raschlowerror_100_5_wide)
c_r_raschlowerror_100_10_wide  <- CR(resp_raschlowerror_100_10_wide)
c_r_raschlowerror_100_20_wide  <- CR(resp_raschlowerror_100_20_wide)
c_r_raschlowerror_250_5_wide  <- CR(resp_raschlowerror_250_5_wide)
c_r_raschlowerror_250_10_wide  <- CR(resp_raschlowerror_250_10_wide)
c_r_raschlowerror_250_20_wide  <- CR(resp_raschlowerror_250_20_wide)
c_r_raschlowerror_500_5_wide  <- CR(resp_raschlowerror_500_5_wide)
c_r_raschlowerror_500_10_wide  <- CR(resp_raschlowerror_500_10_wide)
c_r_raschlowerror_500_20_wide  <- CR(resp_raschlowerror_500_20_wide)
c_r_raschlowerror_1000_5_wide  <- CR(resp_raschlowerror_1000_5_wide)
c_r_raschlowerror_1000_10_wide  <- CR(resp_raschlowerror_1000_10_wide)
c_r_raschlowerror_1000_20_wide  <- CR(resp_raschlowerror_1000_20_wide)

# Combine all the Cr values
c_r <- as.data.frame(ls()) %>% filter(str_detect(`ls()`, "c_r_"))

values_c_r <- c(c_r_guttman_100_10_narrow,
                c_r_guttman_100_10_wide,
                c_r_guttman_100_20_narrow,
                c_r_guttman_100_20_wide,
                c_r_guttman_100_5_narrow,
                c_r_guttman_100_5_wide,
                c_r_guttman_1000_10_narrow,
                c_r_guttman_1000_10_wide,
                c_r_guttman_1000_20_narrow,
                c_r_guttman_1000_20_wide,
                c_r_guttman_1000_5_narrow,
                c_r_guttman_1000_5_wide,
                c_r_guttman_250_10_narrow,
                c_r_guttman_250_10_wide,
                c_r_guttman_250_20_narrow,
                c_r_guttman_250_20_wide,
                c_r_guttman_250_5_narrow,
                c_r_guttman_250_5_wide,
                c_r_guttman_50_10_narrow,
                c_r_guttman_50_10_wide,
                c_r_guttman_50_20_narrow,
                c_r_guttman_50_20_wide,
                c_r_guttman_50_5_narrow,
                c_r_guttman_50_5_wide,
                c_r_guttman_500_10_narrow,
                c_r_guttman_500_10_wide,
                c_r_guttman_500_20_narrow,
                c_r_guttman_500_20_wide,
                c_r_guttman_500_5_narrow,
                c_r_guttman_500_5_wide,
                c_r_rasch_100_10_narrow,
                c_r_rasch_100_10_wide,
                c_r_rasch_100_20_narrow,
                c_r_rasch_100_20_wide,
                c_r_rasch_100_5_narrow,
                c_r_rasch_100_5_wide,
                c_r_rasch_1000_10_narrow,
                c_r_rasch_1000_10_wide,
                c_r_rasch_1000_20_narrow,
                c_r_rasch_1000_20_wide,
                c_r_rasch_1000_5_narrow,
                c_r_rasch_1000_5_wide,
                c_r_rasch_250_10_narrow,
                c_r_rasch_250_10_wide,
                c_r_rasch_250_20_narrow,
                c_r_rasch_250_20_wide,
                c_r_rasch_250_5_narrow,
                c_r_rasch_250_5_wide,
                c_r_rasch_50_10_narrow,
                c_r_rasch_50_10_wide,
                c_r_rasch_50_20_narrow,
                c_r_rasch_50_20_wide,
                c_r_rasch_50_5_narrow,
                c_r_rasch_50_5_wide,
                c_r_rasch_500_10_narrow,
                c_r_rasch_500_10_wide,
                c_r_rasch_500_20_narrow,
                c_r_rasch_500_20_wide,
                c_r_rasch_500_5_narrow,
                c_r_rasch_500_5_wide,
                c_r_raschhigherror_100_10_narrow,
                c_r_raschhigherror_100_10_wide,
                c_r_raschhigherror_100_20_narrow,
                c_r_raschhigherror_100_20_wide,
                c_r_raschhigherror_100_5_narrow,
                c_r_raschhigherror_100_5_wide,
                c_r_raschhigherror_1000_10_narrow,
                c_r_raschhigherror_1000_10_wide,
                c_r_raschhigherror_1000_20_narrow,
                c_r_raschhigherror_1000_20_wide,
                c_r_raschhigherror_1000_5_narrow,
                c_r_raschhigherror_1000_5_wide,
                c_r_raschhigherror_250_10_narrow,
                c_r_raschhigherror_250_10_wide,
                c_r_raschhigherror_250_20_narrow,
                c_r_raschhigherror_250_20_wide,
                c_r_raschhigherror_250_5_narrow,
                c_r_raschhigherror_250_5_wide,
                c_r_raschhigherror_50_10_narrow,
                c_r_raschhigherror_50_10_wide,
                c_r_raschhigherror_50_20_narrow,
                c_r_raschhigherror_50_20_wide,
                c_r_raschhigherror_50_5_narrow,
                c_r_raschhigherror_50_5_wide,
                c_r_raschhigherror_500_10_narrow,
                c_r_raschhigherror_500_10_wide,
                c_r_raschhigherror_500_20_narrow,
                c_r_raschhigherror_500_20_wide,
                c_r_raschhigherror_500_5_narrow,
                c_r_raschhigherror_500_5_wide,
                c_r_raschlowerror_100_10_narrow,
                c_r_raschlowerror_100_10_wide,
                c_r_raschlowerror_100_20_narrow,
                c_r_raschlowerror_100_20_wide,
                c_r_raschlowerror_100_5_narrow,
                c_r_raschlowerror_100_5_wide,
                c_r_raschlowerror_1000_10_narrow,
                c_r_raschlowerror_1000_10_wide,
                c_r_raschlowerror_1000_20_narrow,
                c_r_raschlowerror_1000_20_wide,
                c_r_raschlowerror_1000_5_narrow,
                c_r_raschlowerror_1000_5_wide,
                c_r_raschlowerror_250_10_narrow,
                c_r_raschlowerror_250_10_wide,
                c_r_raschlowerror_250_20_narrow,
                c_r_raschlowerror_250_20_wide,
                c_r_raschlowerror_250_5_narrow,
                c_r_raschlowerror_250_5_wide,
                c_r_raschlowerror_50_10_narrow,
                c_r_raschlowerror_50_10_wide,
                c_r_raschlowerror_50_20_narrow,
                c_r_raschlowerror_50_20_wide,
                c_r_raschlowerror_50_5_narrow,
                c_r_raschlowerror_50_5_wide,
                c_r_raschlowerror_500_10_narrow,
                c_r_raschlowerror_500_10_wide,
                c_r_raschlowerror_500_20_narrow,
                c_r_raschlowerror_500_20_wide,
                c_r_raschlowerror_500_5_narrow,
                c_r_raschlowerror_500_5_wide)

c_r <- data.frame(dataset = c_r$`ls()`,
                  c_r = values_c_r) %>%
  separate(col=dataset, into = c("x", "y", "response_type", "sample_size", "item_number", "range"), sep = "_", remove = FALSE) %>%
  mutate(sample_size = factor(sample_size, levels = c("50","100","250","500","1000")),
         item_number = factor(item_number, levels = c("5","10","20")),
         response_type = factor(response_type, levels = c("guttman", "raschlowerror", "rasch", "raschhigherror"))) %>%
  mutate(response_type = dplyr::recode(response_type, guttman = "Guttman", raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         range = dplyr::recode(range, narrow = "Narrow Item Range (60 to 140 kg)", wide = "Wide Item Range (10 to 200 kg)"),
         item_number_lab = "Number of Items",
         dataset = str_replace(dataset, "c_r_", "resp_")
  ) 

save(c_r, file = "models/c_r")

load("models/c_r")

cr_plot <- ggplot(c_r, aes(x=sample_size, y=c_r)) +
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", color = "red") +
  geom_point() +
  labs(x = "Sample Size",
       y = bquote(italic(C[R])),
       title = bquote("Coefficient of Reproducibility ("~italic(C[R])~") Values")) +
  ggh4x::facet_nested(item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6),
        panel.grid = element_blank())

save(cr_plot, file = "plots/cr_plot")

cr_plot

ggsave("plots/cr_plot.png", width = 12.5, height = 7.5, device = "png", dpi = 300)

##### Prepare and check all datasets that aren't Guttman for cancellation violations

# Rasch
prep_rasch_50_5_narrow <- PrepareChecks(ss.lower=5, resp_rasch_50_5_narrow)

n.3mats <- if_else(dim(prep_rasch_50_5_narrow$n)[1]^3*dim(prep_rasch_50_5_narrow$n)[1]^3 < 5000, dim(prep_rasch_50_5_narrow$n)[1]^3*dim(prep_rasch_50_5_narrow$n)[1]^3, 5000)

c_check_rasch_50_5_narrow <- ConjointChecks(N=prep_rasch_50_5_narrow$N, n=prep_rasch_50_5_narrow$n, n.3mat = n.3mats,
                                                        par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_5_narrow <- as.data.frame(c_check_rasch_50_5_narrow@tab)

c_check_dat_rasch_50_5_narrow <- reshape2::melt(c_check_dat_rasch_50_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_5_narrow$N)[1], times = length(unique(oneRM_dat_50_5_narrow$item))),
         sample_size = 50,
         item_number = 5,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_5_narrow <- summary(c_check_rasch_50_5_narrow)

prep_rasch_50_10_narrow <- PrepareChecks(ss.lower=5, resp_rasch_50_10_narrow)

n.3mats <- if_else(dim(prep_rasch_50_10_narrow$n)[1]^3*dim(prep_rasch_50_10_narrow$n)[1]^3 < 5000, dim(prep_rasch_50_10_narrow$n)[1]^3*dim(prep_rasch_50_10_narrow$n)[1]^3, 5000)

c_check_rasch_50_10_narrow <- ConjointChecks(N=prep_rasch_50_10_narrow$N, n=prep_rasch_50_10_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_10_narrow <- as.data.frame(c_check_rasch_50_10_narrow@tab)

c_check_dat_rasch_50_10_narrow <- reshape2::melt(c_check_dat_rasch_50_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_10_narrow$N)[1], times = length(unique(oneRM_dat_50_10_narrow$item))),
         sample_size = 50,
         item_number = 10,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_10_narrow <- summary(c_check_rasch_50_10_narrow)

prep_rasch_50_20_narrow <- PrepareChecks(ss.lower=5, resp_rasch_50_20_narrow)

n.3mats <- if_else(dim(prep_rasch_50_20_narrow$n)[1]^3*dim(prep_rasch_50_20_narrow$n)[1]^3 < 5000, dim(prep_rasch_50_20_narrow$n)[1]^3*dim(prep_rasch_50_20_narrow$n)[1]^3, 5000)

c_check_rasch_50_20_narrow <- ConjointChecks(N=prep_rasch_50_20_narrow$N, n=prep_rasch_50_20_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_20_narrow <- as.data.frame(c_check_rasch_50_20_narrow@tab)

c_check_dat_rasch_50_20_narrow <- reshape2::melt(c_check_dat_rasch_50_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_20_narrow$N)[1], times = length(unique(oneRM_dat_50_20_narrow$item))),
         sample_size = 50,
         item_number = 20,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_20_narrow <- summary(c_check_rasch_50_20_narrow)

prep_rasch_100_5_narrow <- PrepareChecks(ss.lower=5, resp_rasch_100_5_narrow)

n.3mats <- if_else(dim(prep_rasch_100_5_narrow$n)[1]^3*dim(prep_rasch_100_5_narrow$n)[1]^3 < 5000, dim(prep_rasch_100_5_narrow$n)[1]^3*dim(prep_rasch_100_5_narrow$n)[1]^3, 5000)

c_check_rasch_100_5_narrow <- ConjointChecks(N=prep_rasch_100_5_narrow$N, n=prep_rasch_100_5_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_5_narrow <- as.data.frame(c_check_rasch_100_5_narrow@tab)

c_check_dat_rasch_100_5_narrow <- reshape2::melt(c_check_dat_rasch_100_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_5_narrow$N)[1], times = length(unique(oneRM_dat_100_5_narrow$item))),
         sample_size = 100,
         item_number = 5,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_5_narrow <- summary(c_check_rasch_100_5_narrow)

prep_rasch_100_10_narrow <- PrepareChecks(ss.lower=5, resp_rasch_100_10_narrow)

n.3mats <- if_else(dim(prep_rasch_100_10_narrow$n)[1]^3*dim(prep_rasch_100_10_narrow$n)[1]^3 < 5000, dim(prep_rasch_100_10_narrow$n)[1]^3*dim(prep_rasch_100_10_narrow$n)[1]^3, 5000)

c_check_rasch_100_10_narrow <- ConjointChecks(N=prep_rasch_100_10_narrow$N, n=prep_rasch_100_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_10_narrow <- as.data.frame(c_check_rasch_100_10_narrow@tab)

c_check_dat_rasch_100_10_narrow <- reshape2::melt(c_check_dat_rasch_100_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_10_narrow$N)[1], times = length(unique(oneRM_dat_100_10_narrow$item))),
         sample_size = 100,
         item_number = 10,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_10_narrow <- summary(c_check_rasch_100_10_narrow)

prep_rasch_100_20_narrow <- PrepareChecks(ss.lower=5, resp_rasch_100_20_narrow)

n.3mats <- if_else(dim(prep_rasch_100_20_narrow$n)[1]^3*dim(prep_rasch_100_20_narrow$n)[1]^3 < 5000, dim(prep_rasch_100_20_narrow$n)[1]^3*dim(prep_rasch_100_20_narrow$n)[1]^3, 5000)

c_check_rasch_100_20_narrow <- ConjointChecks(N=prep_rasch_100_20_narrow$N, n=prep_rasch_100_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_20_narrow <- as.data.frame(c_check_rasch_100_20_narrow@tab)

c_check_dat_rasch_100_20_narrow <- reshape2::melt(c_check_dat_rasch_100_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_20_narrow$N)[1], times = length(unique(oneRM_dat_100_20_narrow$item))),
         sample_size = 100,
         item_number = 20,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_20_narrow <- summary(c_check_rasch_100_20_narrow)

prep_rasch_250_5_narrow <- PrepareChecks(ss.lower=5, resp_rasch_250_5_narrow)

n.3mats <- if_else(dim(prep_rasch_250_5_narrow$n)[1]^3*dim(prep_rasch_250_5_narrow$n)[1]^3 < 5000, dim(prep_rasch_250_5_narrow$n)[1]^3*dim(prep_rasch_250_5_narrow$n)[1]^3, 5000)

c_check_rasch_250_5_narrow <- ConjointChecks(N=prep_rasch_250_5_narrow$N, n=prep_rasch_250_5_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_5_narrow <- as.data.frame(c_check_rasch_250_5_narrow@tab)

c_check_dat_rasch_250_5_narrow <- reshape2::melt(c_check_dat_rasch_250_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_5_narrow$N)[1], times = length(unique(oneRM_dat_250_5_narrow$item))),
         sample_size = 250,
         item_number = 5,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_5_narrow <- summary(c_check_rasch_250_5_narrow)

prep_rasch_250_10_narrow <- PrepareChecks(ss.lower=5, resp_rasch_250_10_narrow)

n.3mats <- if_else(dim(prep_rasch_250_10_narrow$n)[1]^3*dim(prep_rasch_250_10_narrow$n)[1]^3 < 5000, dim(prep_rasch_250_10_narrow$n)[1]^3*dim(prep_rasch_250_10_narrow$n)[1]^3, 5000)

c_check_rasch_250_10_narrow <- ConjointChecks(N=prep_rasch_250_10_narrow$N, n=prep_rasch_250_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_10_narrow <- as.data.frame(c_check_rasch_250_10_narrow@tab)

c_check_dat_rasch_250_10_narrow <- reshape2::melt(c_check_dat_rasch_250_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_10_narrow$N)[1], times = length(unique(oneRM_dat_250_10_narrow$item))),
         sample_size = 250,
         item_number = 10,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_10_narrow <- summary(c_check_rasch_250_10_narrow)

prep_rasch_250_20_narrow <- PrepareChecks(ss.lower=5, resp_rasch_250_20_narrow)

n.3mats <- if_else(dim(prep_rasch_250_20_narrow$n)[1]^3*dim(prep_rasch_250_20_narrow$n)[1]^3 < 5000, dim(prep_rasch_250_20_narrow$n)[1]^3*dim(prep_rasch_250_20_narrow$n)[1]^3, 5000)

c_check_rasch_250_20_narrow <- ConjointChecks(N=prep_rasch_250_20_narrow$N, n=prep_rasch_250_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_20_narrow <- as.data.frame(c_check_rasch_250_20_narrow@tab)

c_check_dat_rasch_250_20_narrow <- reshape2::melt(c_check_dat_rasch_250_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_20_narrow$N)[1], times = length(unique(oneRM_dat_250_20_narrow$item))),
         sample_size = 250,
         item_number = 20,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_20_narrow <- summary(c_check_rasch_250_20_narrow)

prep_rasch_500_5_narrow <- PrepareChecks(ss.lower=5, resp_rasch_500_5_narrow)

n.3mats <- if_else(dim(prep_rasch_500_5_narrow$n)[1]^3*dim(prep_rasch_500_5_narrow$n)[1]^3 < 5000, dim(prep_rasch_500_5_narrow$n)[1]^3*dim(prep_rasch_500_5_narrow$n)[1]^3, 5000)

c_check_rasch_500_5_narrow <- ConjointChecks(N=prep_rasch_500_5_narrow$N, n=prep_rasch_500_5_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_5_narrow <- as.data.frame(c_check_rasch_500_5_narrow@tab)

c_check_dat_rasch_500_5_narrow <- reshape2::melt(c_check_dat_rasch_500_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_5_narrow$N)[1], times = length(unique(oneRM_dat_500_5_narrow$item))),
         sample_size = 500,
         item_number = 5,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_5_narrow <- summary(c_check_rasch_500_5_narrow)

prep_rasch_500_10_narrow <- PrepareChecks(ss.lower=5, resp_rasch_500_10_narrow)

n.3mats <- if_else(dim(prep_rasch_500_10_narrow$n)[1]^3*dim(prep_rasch_500_10_narrow$n)[1]^3 < 5000, dim(prep_rasch_500_10_narrow$n)[1]^3*dim(prep_rasch_500_10_narrow$n)[1]^3, 5000)

c_check_rasch_500_10_narrow <- ConjointChecks(N=prep_rasch_500_10_narrow$N, n=prep_rasch_500_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_10_narrow <- as.data.frame(c_check_rasch_500_10_narrow@tab)

c_check_dat_rasch_500_10_narrow <- reshape2::melt(c_check_dat_rasch_500_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_10_narrow$N)[1], times = length(unique(oneRM_dat_500_10_narrow$item))),
         sample_size = 500,
         item_number = 10,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_10_narrow <- summary(c_check_rasch_500_10_narrow)

prep_rasch_500_20_narrow <- PrepareChecks(ss.lower=5, resp_rasch_500_20_narrow)

n.3mats <- if_else(dim(prep_rasch_500_20_narrow$n)[1]^3*dim(prep_rasch_500_20_narrow$n)[1]^3 < 5000, dim(prep_rasch_500_20_narrow$n)[1]^3*dim(prep_rasch_500_20_narrow$n)[1]^3, 5000)

c_check_rasch_500_20_narrow <- ConjointChecks(N=prep_rasch_500_20_narrow$N, n=prep_rasch_500_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_20_narrow <- as.data.frame(c_check_rasch_500_20_narrow@tab)

c_check_dat_rasch_500_20_narrow <- reshape2::melt(c_check_dat_rasch_500_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_20_narrow$N)[1], times = length(unique(oneRM_dat_500_20_narrow$item))),
         sample_size = 500,
         item_number = 20,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_20_narrow <- summary(c_check_rasch_500_20_narrow)

prep_rasch_1000_5_narrow <- PrepareChecks(ss.lower=5, resp_rasch_1000_5_narrow)

n.3mats <- if_else(dim(prep_rasch_1000_5_narrow$n)[1]^3*dim(prep_rasch_1000_5_narrow$n)[1]^3 < 5000, dim(prep_rasch_1000_5_narrow$n)[1]^3*dim(prep_rasch_1000_5_narrow$n)[1]^3, 5000)

c_check_rasch_1000_5_narrow <- ConjointChecks(N=prep_rasch_1000_5_narrow$N, n=prep_rasch_1000_5_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_5_narrow <- as.data.frame(c_check_rasch_1000_5_narrow@tab)

c_check_dat_rasch_1000_5_narrow <- reshape2::melt(c_check_dat_rasch_1000_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_5_narrow$N)[1], times = length(unique(oneRM_dat_1000_5_narrow$item))),
         sample_size = 1000,
         item_number = 5,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_5_narrow <- summary(c_check_rasch_1000_5_narrow)

prep_rasch_1000_10_narrow <- PrepareChecks(ss.lower=5, resp_rasch_1000_10_narrow)

n.3mats <- if_else(dim(prep_rasch_1000_10_narrow$n)[1]^3*dim(prep_rasch_1000_10_narrow$n)[1]^3 < 5000, dim(prep_rasch_1000_10_narrow$n)[1]^3*dim(prep_rasch_1000_10_narrow$n)[1]^3, 5000)

c_check_rasch_1000_10_narrow <- ConjointChecks(N=prep_rasch_1000_10_narrow$N, n=prep_rasch_1000_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_10_narrow <- as.data.frame(c_check_rasch_1000_10_narrow@tab)

c_check_dat_rasch_1000_10_narrow <- reshape2::melt(c_check_dat_rasch_1000_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_10_narrow$N)[1], times = length(unique(oneRM_dat_1000_10_narrow$item))),
         sample_size = 1000,
         item_number = 10,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_10_narrow <- summary(c_check_rasch_1000_10_narrow)

prep_rasch_1000_20_narrow <- PrepareChecks(ss.lower=5, resp_rasch_1000_20_narrow)

n.3mats <- if_else(dim(prep_rasch_1000_20_narrow$n)[1]^3*dim(prep_rasch_1000_20_narrow$n)[1]^3 < 5000, dim(prep_rasch_1000_20_narrow$n)[1]^3*dim(prep_rasch_1000_20_narrow$n)[1]^3, 5000)

c_check_rasch_1000_20_narrow <- ConjointChecks(N=prep_rasch_1000_20_narrow$N, n=prep_rasch_1000_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_20_narrow <- as.data.frame(c_check_rasch_1000_20_narrow@tab)

c_check_dat_rasch_1000_20_narrow <- reshape2::melt(c_check_dat_rasch_1000_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_20_narrow$N)[1], times = length(unique(oneRM_dat_1000_20_narrow$item))),
         sample_size = 1000,
         item_number = 20,
         range = "narrow", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_20_narrow <- summary(c_check_rasch_1000_20_narrow)

prep_rasch_50_5_wide <- PrepareChecks(ss.lower=5, resp_rasch_50_5_wide)

n.3mats <- if_else(dim(prep_rasch_50_5_wide$n)[1]^3*dim(prep_rasch_50_5_wide$n)[1]^3 < 5000, dim(prep_rasch_50_5_wide$n)[1]^3*dim(prep_rasch_50_5_wide$n)[1]^3, 5000)

c_check_rasch_50_5_wide <- ConjointChecks(N=prep_rasch_50_5_wide$N, n=prep_rasch_50_5_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_5_wide <- as.data.frame(c_check_rasch_50_5_wide@tab)

c_check_dat_rasch_50_5_wide <- reshape2::melt(c_check_dat_rasch_50_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_5_wide$N)[1], times = length(unique(oneRM_dat_50_5_wide$item))),
         sample_size = 50,
         item_number = 5,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_5_wide <- summary(c_check_rasch_50_5_wide)

prep_rasch_50_10_wide <- PrepareChecks(ss.lower=5, resp_rasch_50_10_wide)

n.3mats <- if_else(dim(prep_rasch_50_10_wide$n)[1]^3*dim(prep_rasch_50_10_wide$n)[1]^3 < 5000, dim(prep_rasch_50_10_wide$n)[1]^3*dim(prep_rasch_50_10_wide$n)[1]^3, 5000)

c_check_rasch_50_10_wide <- ConjointChecks(N=prep_rasch_50_10_wide$N, n=prep_rasch_50_10_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_10_wide <- as.data.frame(c_check_rasch_50_10_wide@tab)

c_check_dat_rasch_50_10_wide <- reshape2::melt(c_check_dat_rasch_50_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_10_wide$N)[1], times = length(unique(oneRM_dat_50_10_wide$item))),
         sample_size = 50,
         item_number = 10,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_10_wide <- summary(c_check_rasch_50_10_wide)

prep_rasch_50_20_wide <- PrepareChecks(ss.lower=5, resp_rasch_50_20_wide)

n.3mats <- if_else(dim(prep_rasch_50_20_wide$n)[1]^3*dim(prep_rasch_50_20_wide$n)[1]^3 < 5000, dim(prep_rasch_50_20_wide$n)[1]^3*dim(prep_rasch_50_20_wide$n)[1]^3, 5000)

c_check_rasch_50_20_wide <- ConjointChecks(N=prep_rasch_50_20_wide$N, n=prep_rasch_50_20_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_50_20_wide <- as.data.frame(c_check_rasch_50_20_wide@tab)

c_check_dat_rasch_50_20_wide <- reshape2::melt(c_check_dat_rasch_50_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_50_20_wide$N)[1], times = length(unique(oneRM_dat_50_20_wide$item))),
         sample_size = 50,
         item_number = 20,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_50_20_wide <- summary(c_check_rasch_50_20_wide)

prep_rasch_100_5_wide <- PrepareChecks(ss.lower=5, resp_rasch_100_5_wide)

n.3mats <- if_else(dim(prep_rasch_100_5_wide$n)[1]^3*dim(prep_rasch_100_5_wide$n)[1]^3 < 5000, dim(prep_rasch_100_5_wide$n)[1]^3*dim(prep_rasch_100_5_wide$n)[1]^3, 5000)

c_check_rasch_100_5_wide <- ConjointChecks(N=prep_rasch_100_5_wide$N, n=prep_rasch_100_5_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_5_wide <- as.data.frame(c_check_rasch_100_5_wide@tab)

c_check_dat_rasch_100_5_wide <- reshape2::melt(c_check_dat_rasch_100_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_5_wide$N)[1], times = length(unique(oneRM_dat_100_5_wide$item))),
         sample_size = 100,
         item_number = 5,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_5_wide <- summary(c_check_rasch_100_5_wide)

prep_rasch_100_10_wide <- PrepareChecks(ss.lower=5, resp_rasch_100_10_wide)

n.3mats <- if_else(dim(prep_rasch_100_10_wide$n)[1]^3*dim(prep_rasch_100_10_wide$n)[1]^3 < 5000, dim(prep_rasch_100_10_wide$n)[1]^3*dim(prep_rasch_100_10_wide$n)[1]^3, 5000)

c_check_rasch_100_10_wide <- ConjointChecks(N=prep_rasch_100_10_wide$N, n=prep_rasch_100_10_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_10_wide <- as.data.frame(c_check_rasch_100_10_wide@tab)

c_check_dat_rasch_100_10_wide <- reshape2::melt(c_check_dat_rasch_100_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_10_wide$N)[1], times = length(unique(oneRM_dat_100_10_wide$item))),
         sample_size = 100,
         item_number = 10,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_10_wide <- summary(c_check_rasch_100_10_wide)

prep_rasch_100_20_wide <- PrepareChecks(ss.lower=5, resp_rasch_100_20_wide)

n.3mats <- if_else(dim(prep_rasch_100_20_wide$n)[1]^3*dim(prep_rasch_100_20_wide$n)[1]^3 < 5000, dim(prep_rasch_100_20_wide$n)[1]^3*dim(prep_rasch_100_20_wide$n)[1]^3, 5000)

c_check_rasch_100_20_wide <- ConjointChecks(N=prep_rasch_100_20_wide$N, n=prep_rasch_100_20_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_100_20_wide <- as.data.frame(c_check_rasch_100_20_wide@tab)

c_check_dat_rasch_100_20_wide <- reshape2::melt(c_check_dat_rasch_100_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_100_20_wide$N)[1], times = length(unique(oneRM_dat_100_20_wide$item))),
         sample_size = 100,
         item_number = 20,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_100_20_wide <- summary(c_check_rasch_100_20_wide)

prep_rasch_250_5_wide <- PrepareChecks(ss.lower=5, resp_rasch_250_5_wide)

n.3mats <- if_else(dim(prep_rasch_250_5_wide$n)[1]^3*dim(prep_rasch_250_5_wide$n)[1]^3 < 5000, dim(prep_rasch_250_5_wide$n)[1]^3*dim(prep_rasch_250_5_wide$n)[1]^3, 5000)

c_check_rasch_250_5_wide <- ConjointChecks(N=prep_rasch_250_5_wide$N, n=prep_rasch_250_5_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_5_wide <- as.data.frame(c_check_rasch_250_5_wide@tab)

c_check_dat_rasch_250_5_wide <- reshape2::melt(c_check_dat_rasch_250_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_5_wide$N)[1], times = length(unique(oneRM_dat_250_5_wide$item))),
         sample_size = 250,
         item_number = 5,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_5_wide <- summary(c_check_rasch_250_5_wide)

prep_rasch_250_10_wide <- PrepareChecks(ss.lower=5, resp_rasch_250_10_wide)

n.3mats <- if_else(dim(prep_rasch_250_10_wide$n)[1]^3*dim(prep_rasch_250_10_wide$n)[1]^3 < 5000, dim(prep_rasch_250_10_wide$n)[1]^3*dim(prep_rasch_250_10_wide$n)[1]^3, 5000)

c_check_rasch_250_10_wide <- ConjointChecks(N=prep_rasch_250_10_wide$N, n=prep_rasch_250_10_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_10_wide <- as.data.frame(c_check_rasch_250_10_wide@tab)

c_check_dat_rasch_250_10_wide <- reshape2::melt(c_check_dat_rasch_250_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_10_wide$N)[1], times = length(unique(oneRM_dat_250_10_wide$item))),
         sample_size = 250,
         item_number = 10,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_10_wide <- summary(c_check_rasch_250_10_wide)

prep_rasch_250_20_wide <- PrepareChecks(ss.lower=5, resp_rasch_250_20_wide)

n.3mats <- if_else(dim(prep_rasch_250_20_wide$n)[1]^3*dim(prep_rasch_250_20_wide$n)[1]^3 < 5000, dim(prep_rasch_250_20_wide$n)[1]^3*dim(prep_rasch_250_20_wide$n)[1]^3, 5000)

c_check_rasch_250_20_wide <- ConjointChecks(N=prep_rasch_250_20_wide$N, n=prep_rasch_250_20_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_250_20_wide <- as.data.frame(c_check_rasch_250_20_wide@tab)

c_check_dat_rasch_250_20_wide <- reshape2::melt(c_check_dat_rasch_250_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_250_20_wide$N)[1], times = length(unique(oneRM_dat_250_20_wide$item))),
         sample_size = 250,
         item_number = 20,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_250_20_wide <- summary(c_check_rasch_250_20_wide)

prep_rasch_500_5_wide <- PrepareChecks(ss.lower=5, resp_rasch_500_5_wide)

n.3mats <- if_else(dim(prep_rasch_500_5_wide$n)[1]^3*dim(prep_rasch_500_5_wide$n)[1]^3 < 5000, dim(prep_rasch_500_5_wide$n)[1]^3*dim(prep_rasch_500_5_wide$n)[1]^3, 5000)

c_check_rasch_500_5_wide <- ConjointChecks(N=prep_rasch_500_5_wide$N, n=prep_rasch_500_5_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_5_wide <- as.data.frame(c_check_rasch_500_5_wide@tab)

c_check_dat_rasch_500_5_wide <- reshape2::melt(c_check_dat_rasch_500_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_5_wide$N)[1], times = length(unique(oneRM_dat_500_5_wide$item))),
         sample_size = 500,
         item_number = 5,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_5_wide <- summary(c_check_rasch_500_5_wide)

prep_rasch_500_10_wide <- PrepareChecks(ss.lower=5, resp_rasch_500_10_wide)

n.3mats <- if_else(dim(prep_rasch_500_10_wide$n)[1]^3*dim(prep_rasch_500_10_wide$n)[1]^3 < 5000, dim(prep_rasch_500_10_wide$n)[1]^3*dim(prep_rasch_500_10_wide$n)[1]^3, 5000)

c_check_rasch_500_10_wide <- ConjointChecks(N=prep_rasch_500_10_wide$N, n=prep_rasch_500_10_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_10_wide <- as.data.frame(c_check_rasch_500_10_wide@tab)

c_check_dat_rasch_500_10_wide <- reshape2::melt(c_check_dat_rasch_500_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_10_wide$N)[1], times = length(unique(oneRM_dat_500_10_wide$item))),
         sample_size = 500,
         item_number = 10,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_10_wide <- summary(c_check_rasch_500_10_wide)

prep_rasch_500_20_wide <- PrepareChecks(ss.lower=5, resp_rasch_500_20_wide)

n.3mats <- if_else(dim(prep_rasch_500_20_wide$n)[1]^3*dim(prep_rasch_500_20_wide$n)[1]^3 < 5000, dim(prep_rasch_500_20_wide$n)[1]^3*dim(prep_rasch_500_20_wide$n)[1]^3, 5000)

c_check_rasch_500_20_wide <- ConjointChecks(N=prep_rasch_500_20_wide$N, n=prep_rasch_500_20_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_500_20_wide <- as.data.frame(c_check_rasch_500_20_wide@tab)

c_check_dat_rasch_500_20_wide <- reshape2::melt(c_check_dat_rasch_500_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_500_20_wide$N)[1], times = length(unique(oneRM_dat_500_20_wide$item))),
         sample_size = 500,
         item_number = 20,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_500_20_wide <- summary(c_check_rasch_500_20_wide)

prep_rasch_1000_5_wide <- PrepareChecks(ss.lower=5, resp_rasch_1000_5_wide)

n.3mats <- if_else(dim(prep_rasch_1000_5_wide$n)[1]^3*dim(prep_rasch_1000_5_wide$n)[1]^3 < 5000, dim(prep_rasch_1000_5_wide$n)[1]^3*dim(prep_rasch_1000_5_wide$n)[1]^3, 5000)

c_check_rasch_1000_5_wide <- ConjointChecks(N=prep_rasch_1000_5_wide$N, n=prep_rasch_1000_5_wide$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_5_wide <- as.data.frame(c_check_rasch_1000_5_wide@tab)

c_check_dat_rasch_1000_5_wide <- reshape2::melt(c_check_dat_rasch_1000_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_5_wide$N)[1], times = length(unique(oneRM_dat_1000_5_wide$item))),
         sample_size = 1000,
         item_number = 5,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_5_wide <- summary(c_check_rasch_1000_5_wide)

prep_rasch_1000_10_wide <- PrepareChecks(ss.lower=5, resp_rasch_1000_10_wide)

n.3mats <- if_else(dim(prep_rasch_1000_10_wide$n)[1]^3*dim(prep_rasch_1000_10_wide$n)[1]^3 < 5000, dim(prep_rasch_1000_10_wide$n)[1]^3*dim(prep_rasch_1000_10_wide$n)[1]^3, 5000)

c_check_rasch_1000_10_wide <- ConjointChecks(N=prep_rasch_1000_10_wide$N, n=prep_rasch_1000_10_wide$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_10_wide <- as.data.frame(c_check_rasch_1000_10_wide@tab)

c_check_dat_rasch_1000_10_wide <- reshape2::melt(c_check_dat_rasch_1000_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_10_wide$N)[1], times = length(unique(oneRM_dat_1000_10_wide$item))),
         sample_size = 1000,
         item_number = 10,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_10_wide <- summary(c_check_rasch_1000_10_wide)

prep_rasch_1000_20_wide <- PrepareChecks(ss.lower=5, resp_rasch_1000_20_wide)

n.3mats <- if_else(dim(prep_rasch_1000_20_wide$n)[1]^3*dim(prep_rasch_1000_20_wide$n)[1]^3 < 5000, dim(prep_rasch_1000_20_wide$n)[1]^3*dim(prep_rasch_1000_20_wide$n)[1]^3, 5000)

c_check_rasch_1000_20_wide <- ConjointChecks(N=prep_rasch_1000_20_wide$N, n=prep_rasch_1000_20_wide$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_rasch_1000_20_wide <- as.data.frame(c_check_rasch_1000_20_wide@tab)

c_check_dat_rasch_1000_20_wide <- reshape2::melt(c_check_dat_rasch_1000_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_rasch_1000_20_wide$N)[1], times = length(unique(oneRM_dat_1000_20_wide$item))),
         sample_size = 1000,
         item_number = 20,
         range = "wide", response_type = "Typical Error (?? = 1)")

summary_c_check_rasch_1000_20_wide <- summary(c_check_rasch_1000_20_wide)

# Rasch high error
prep_raschhigherror_50_5_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_5_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_50_5_narrow$n)[1]^3*dim(prep_raschhigherror_50_5_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_50_5_narrow$n)[1]^3*dim(prep_raschhigherror_50_5_narrow$n)[1]^3, 5000)

c_check_raschhigherror_50_5_narrow <- ConjointChecks(N=prep_raschhigherror_50_5_narrow$N, n=prep_raschhigherror_50_5_narrow$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_5_narrow <- as.data.frame(c_check_raschhigherror_50_5_narrow@tab)

c_check_dat_raschhigherror_50_5_narrow <- reshape2::melt(c_check_dat_raschhigherror_50_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_5_narrow$N)[1], times = length(unique(oneRM_dat_50_5_narrow$item))),
         sample_size = 50,
         item_number = 5,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_5_narrow <- summary(c_check_raschhigherror_50_5_narrow)

prep_raschhigherror_50_10_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_10_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_50_10_narrow$n)[1]^3*dim(prep_raschhigherror_50_10_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_50_10_narrow$n)[1]^3*dim(prep_raschhigherror_50_10_narrow$n)[1]^3, 5000)

c_check_raschhigherror_50_10_narrow <- ConjointChecks(N=prep_raschhigherror_50_10_narrow$N, n=prep_raschhigherror_50_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_10_narrow <- as.data.frame(c_check_raschhigherror_50_10_narrow@tab)

c_check_dat_raschhigherror_50_10_narrow <- reshape2::melt(c_check_dat_raschhigherror_50_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_10_narrow$N)[1], times = length(unique(oneRM_dat_50_10_narrow$item))),
         sample_size = 50,
         item_number = 10,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_10_narrow <- summary(c_check_raschhigherror_50_10_narrow)

prep_raschhigherror_50_20_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_20_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_50_20_narrow$n)[1]^3*dim(prep_raschhigherror_50_20_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_50_20_narrow$n)[1]^3*dim(prep_raschhigherror_50_20_narrow$n)[1]^3, 5000)

c_check_raschhigherror_50_20_narrow <- ConjointChecks(N=prep_raschhigherror_50_20_narrow$N, n=prep_raschhigherror_50_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_20_narrow <- as.data.frame(c_check_raschhigherror_50_20_narrow@tab)

c_check_dat_raschhigherror_50_20_narrow <- reshape2::melt(c_check_dat_raschhigherror_50_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_20_narrow$N)[1], times = length(unique(oneRM_dat_50_20_narrow$item))),
         sample_size = 50,
         item_number = 20,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_20_narrow <- summary(c_check_raschhigherror_50_20_narrow)

prep_raschhigherror_100_5_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_5_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_100_5_narrow$n)[1]^3*dim(prep_raschhigherror_100_5_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_100_5_narrow$n)[1]^3*dim(prep_raschhigherror_100_5_narrow$n)[1]^3, 5000)

c_check_raschhigherror_100_5_narrow <- ConjointChecks(N=prep_raschhigherror_100_5_narrow$N, n=prep_raschhigherror_100_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_5_narrow <- as.data.frame(c_check_raschhigherror_100_5_narrow@tab)

c_check_dat_raschhigherror_100_5_narrow <- reshape2::melt(c_check_dat_raschhigherror_100_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_5_narrow$N)[1], times = length(unique(oneRM_dat_100_5_narrow$item))),
         sample_size = 100,
         item_number = 5,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_5_narrow <- summary(c_check_raschhigherror_100_5_narrow)

prep_raschhigherror_100_10_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_10_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_100_10_narrow$n)[1]^3*dim(prep_raschhigherror_100_10_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_100_10_narrow$n)[1]^3*dim(prep_raschhigherror_100_10_narrow$n)[1]^3, 5000)

c_check_raschhigherror_100_10_narrow <- ConjointChecks(N=prep_raschhigherror_100_10_narrow$N, n=prep_raschhigherror_100_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_10_narrow <- as.data.frame(c_check_raschhigherror_100_10_narrow@tab)

c_check_dat_raschhigherror_100_10_narrow <- reshape2::melt(c_check_dat_raschhigherror_100_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_10_narrow$N)[1], times = length(unique(oneRM_dat_100_10_narrow$item))),
         sample_size = 100,
         item_number = 10,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_10_narrow <- summary(c_check_raschhigherror_100_10_narrow)

prep_raschhigherror_100_20_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_20_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_100_20_narrow$n)[1]^3*dim(prep_raschhigherror_100_20_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_100_20_narrow$n)[1]^3*dim(prep_raschhigherror_100_20_narrow$n)[1]^3, 5000)

c_check_raschhigherror_100_20_narrow <- ConjointChecks(N=prep_raschhigherror_100_20_narrow$N, n=prep_raschhigherror_100_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_20_narrow <- as.data.frame(c_check_raschhigherror_100_20_narrow@tab)

c_check_dat_raschhigherror_100_20_narrow <- reshape2::melt(c_check_dat_raschhigherror_100_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_20_narrow$N)[1], times = length(unique(oneRM_dat_100_20_narrow$item))),
         sample_size = 100,
         item_number = 20,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_20_narrow <- summary(c_check_raschhigherror_100_20_narrow)

prep_raschhigherror_250_5_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_5_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_250_5_narrow$n)[1]^3*dim(prep_raschhigherror_250_5_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_250_5_narrow$n)[1]^3*dim(prep_raschhigherror_250_5_narrow$n)[1]^3, 5000)

c_check_raschhigherror_250_5_narrow <- ConjointChecks(N=prep_raschhigherror_250_5_narrow$N, n=prep_raschhigherror_250_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_5_narrow <- as.data.frame(c_check_raschhigherror_250_5_narrow@tab)

c_check_dat_raschhigherror_250_5_narrow <- reshape2::melt(c_check_dat_raschhigherror_250_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_5_narrow$N)[1], times = length(unique(oneRM_dat_250_5_narrow$item))),
         sample_size = 250,
         item_number = 5,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_5_narrow <- summary(c_check_raschhigherror_250_5_narrow)

prep_raschhigherror_250_10_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_10_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_250_10_narrow$n)[1]^3*dim(prep_raschhigherror_250_10_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_250_10_narrow$n)[1]^3*dim(prep_raschhigherror_250_10_narrow$n)[1]^3, 5000)

c_check_raschhigherror_250_10_narrow <- ConjointChecks(N=prep_raschhigherror_250_10_narrow$N, n=prep_raschhigherror_250_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_10_narrow <- as.data.frame(c_check_raschhigherror_250_10_narrow@tab)

c_check_dat_raschhigherror_250_10_narrow <- reshape2::melt(c_check_dat_raschhigherror_250_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_10_narrow$N)[1], times = length(unique(oneRM_dat_250_10_narrow$item))),
         sample_size = 250,
         item_number = 10,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_10_narrow <- summary(c_check_raschhigherror_250_10_narrow)

prep_raschhigherror_250_20_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_20_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_250_20_narrow$n)[1]^3*dim(prep_raschhigherror_250_20_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_250_20_narrow$n)[1]^3*dim(prep_raschhigherror_250_20_narrow$n)[1]^3, 5000)

c_check_raschhigherror_250_20_narrow <- ConjointChecks(N=prep_raschhigherror_250_20_narrow$N, n=prep_raschhigherror_250_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_20_narrow <- as.data.frame(c_check_raschhigherror_250_20_narrow@tab)

c_check_dat_raschhigherror_250_20_narrow <- reshape2::melt(c_check_dat_raschhigherror_250_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_20_narrow$N)[1], times = length(unique(oneRM_dat_250_20_narrow$item))),
         sample_size = 250,
         item_number = 20,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_20_narrow <- summary(c_check_raschhigherror_250_20_narrow)

prep_raschhigherror_500_5_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_5_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_500_5_narrow$n)[1]^3*dim(prep_raschhigherror_500_5_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_500_5_narrow$n)[1]^3*dim(prep_raschhigherror_500_5_narrow$n)[1]^3, 5000)

c_check_raschhigherror_500_5_narrow <- ConjointChecks(N=prep_raschhigherror_500_5_narrow$N, n=prep_raschhigherror_500_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_5_narrow <- as.data.frame(c_check_raschhigherror_500_5_narrow@tab)

c_check_dat_raschhigherror_500_5_narrow <- reshape2::melt(c_check_dat_raschhigherror_500_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_5_narrow$N)[1], times = length(unique(oneRM_dat_500_5_narrow$item))),
         sample_size = 500,
         item_number = 5,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_5_narrow <- summary(c_check_raschhigherror_500_5_narrow)

prep_raschhigherror_500_10_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_10_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_500_10_narrow$n)[1]^3*dim(prep_raschhigherror_500_10_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_500_10_narrow$n)[1]^3*dim(prep_raschhigherror_500_10_narrow$n)[1]^3, 5000)

c_check_raschhigherror_500_10_narrow <- ConjointChecks(N=prep_raschhigherror_500_10_narrow$N, n=prep_raschhigherror_500_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_10_narrow <- as.data.frame(c_check_raschhigherror_500_10_narrow@tab)

c_check_dat_raschhigherror_500_10_narrow <- reshape2::melt(c_check_dat_raschhigherror_500_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_10_narrow$N)[1], times = length(unique(oneRM_dat_500_10_narrow$item))),
         sample_size = 500,
         item_number = 10,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_10_narrow <- summary(c_check_raschhigherror_500_10_narrow)

prep_raschhigherror_500_20_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_20_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_500_20_narrow$n)[1]^3*dim(prep_raschhigherror_500_20_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_500_20_narrow$n)[1]^3*dim(prep_raschhigherror_500_20_narrow$n)[1]^3, 5000)

c_check_raschhigherror_500_20_narrow <- ConjointChecks(N=prep_raschhigherror_500_20_narrow$N, n=prep_raschhigherror_500_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_20_narrow <- as.data.frame(c_check_raschhigherror_500_20_narrow@tab)

c_check_dat_raschhigherror_500_20_narrow <- reshape2::melt(c_check_dat_raschhigherror_500_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_20_narrow$N)[1], times = length(unique(oneRM_dat_500_20_narrow$item))),
         sample_size = 500,
         item_number = 20,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_20_narrow <- summary(c_check_raschhigherror_500_20_narrow)

prep_raschhigherror_1000_5_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_5_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_1000_5_narrow$n)[1]^3*dim(prep_raschhigherror_1000_5_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_5_narrow$n)[1]^3*dim(prep_raschhigherror_1000_5_narrow$n)[1]^3, 5000)

c_check_raschhigherror_1000_5_narrow <- ConjointChecks(N=prep_raschhigherror_1000_5_narrow$N, n=prep_raschhigherror_1000_5_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_5_narrow <- as.data.frame(c_check_raschhigherror_1000_5_narrow@tab)

c_check_dat_raschhigherror_1000_5_narrow <- reshape2::melt(c_check_dat_raschhigherror_1000_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_5_narrow$N)[1], times = length(unique(oneRM_dat_1000_5_narrow$item))),
         sample_size = 1000,
         item_number = 5,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_5_narrow <- summary(c_check_raschhigherror_1000_5_narrow)

prep_raschhigherror_1000_10_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_10_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_1000_10_narrow$n)[1]^3*dim(prep_raschhigherror_1000_10_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_10_narrow$n)[1]^3*dim(prep_raschhigherror_1000_10_narrow$n)[1]^3, 5000)

c_check_raschhigherror_1000_10_narrow <- ConjointChecks(N=prep_raschhigherror_1000_10_narrow$N, n=prep_raschhigherror_1000_10_narrow$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_10_narrow <- as.data.frame(c_check_raschhigherror_1000_10_narrow@tab)

c_check_dat_raschhigherror_1000_10_narrow <- reshape2::melt(c_check_dat_raschhigherror_1000_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_10_narrow$N)[1], times = length(unique(oneRM_dat_1000_10_narrow$item))),
         sample_size = 1000,
         item_number = 10,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_10_narrow <- summary(c_check_raschhigherror_1000_10_narrow)

prep_raschhigherror_1000_20_narrow <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_20_narrow)

n.3mats <- if_else(dim(prep_raschhigherror_1000_20_narrow$n)[1]^3*dim(prep_raschhigherror_1000_20_narrow$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_20_narrow$n)[1]^3*dim(prep_raschhigherror_1000_20_narrow$n)[1]^3, 5000)

c_check_raschhigherror_1000_20_narrow <- ConjointChecks(N=prep_raschhigherror_1000_20_narrow$N, n=prep_raschhigherror_1000_20_narrow$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_20_narrow <- as.data.frame(c_check_raschhigherror_1000_20_narrow@tab)

c_check_dat_raschhigherror_1000_20_narrow <- reshape2::melt(c_check_dat_raschhigherror_1000_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_20_narrow$N)[1], times = length(unique(oneRM_dat_1000_20_narrow$item))),
         sample_size = 1000,
         item_number = 20,
         range = "narrow", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_20_narrow <- summary(c_check_raschhigherror_1000_20_narrow)

prep_raschhigherror_50_5_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_5_wide)

n.3mats <- if_else(dim(prep_raschhigherror_50_5_wide$n)[1]^3*dim(prep_raschhigherror_50_5_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_50_5_wide$n)[1]^3*dim(prep_raschhigherror_50_5_wide$n)[1]^3, 5000)

c_check_raschhigherror_50_5_wide <- ConjointChecks(N=prep_raschhigherror_50_5_wide$N, n=prep_raschhigherror_50_5_wide$n, n.3mat = n.3mats,
                                          par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_5_wide <- as.data.frame(c_check_raschhigherror_50_5_wide@tab)

c_check_dat_raschhigherror_50_5_wide <- reshape2::melt(c_check_dat_raschhigherror_50_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_5_wide$N)[1], times = length(unique(oneRM_dat_50_5_wide$item))),
         sample_size = 50,
         item_number = 5,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_5_wide <- summary(c_check_raschhigherror_50_5_wide)

prep_raschhigherror_50_10_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_10_wide)

n.3mats <- if_else(dim(prep_raschhigherror_50_10_wide$n)[1]^3*dim(prep_raschhigherror_50_10_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_50_10_wide$n)[1]^3*dim(prep_raschhigherror_50_10_wide$n)[1]^3, 5000)

c_check_raschhigherror_50_10_wide <- ConjointChecks(N=prep_raschhigherror_50_10_wide$N, n=prep_raschhigherror_50_10_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_10_wide <- as.data.frame(c_check_raschhigherror_50_10_wide@tab)

c_check_dat_raschhigherror_50_10_wide <- reshape2::melt(c_check_dat_raschhigherror_50_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_10_wide$N)[1], times = length(unique(oneRM_dat_50_10_wide$item))),
         sample_size = 50,
         item_number = 10,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_10_wide <- summary(c_check_raschhigherror_50_10_wide)

prep_raschhigherror_50_20_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_50_20_wide)

n.3mats <- if_else(dim(prep_raschhigherror_50_20_wide$n)[1]^3*dim(prep_raschhigherror_50_20_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_50_20_wide$n)[1]^3*dim(prep_raschhigherror_50_20_wide$n)[1]^3, 5000)

c_check_raschhigherror_50_20_wide <- ConjointChecks(N=prep_raschhigherror_50_20_wide$N, n=prep_raschhigherror_50_20_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_50_20_wide <- as.data.frame(c_check_raschhigherror_50_20_wide@tab)

c_check_dat_raschhigherror_50_20_wide <- reshape2::melt(c_check_dat_raschhigherror_50_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_50_20_wide$N)[1], times = length(unique(oneRM_dat_50_20_wide$item))),
         sample_size = 50,
         item_number = 20,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_50_20_wide <- summary(c_check_raschhigherror_50_20_wide)

prep_raschhigherror_100_5_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_5_wide)

n.3mats <- if_else(dim(prep_raschhigherror_100_5_wide$n)[1]^3*dim(prep_raschhigherror_100_5_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_100_5_wide$n)[1]^3*dim(prep_raschhigherror_100_5_wide$n)[1]^3, 5000)

c_check_raschhigherror_100_5_wide <- ConjointChecks(N=prep_raschhigherror_100_5_wide$N, n=prep_raschhigherror_100_5_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_5_wide <- as.data.frame(c_check_raschhigherror_100_5_wide@tab)

c_check_dat_raschhigherror_100_5_wide <- reshape2::melt(c_check_dat_raschhigherror_100_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_5_wide$N)[1], times = length(unique(oneRM_dat_100_5_wide$item))),
         sample_size = 100,
         item_number = 5,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_5_wide <- summary(c_check_raschhigherror_100_5_wide)

prep_raschhigherror_100_10_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_10_wide)

n.3mats <- if_else(dim(prep_raschhigherror_100_10_wide$n)[1]^3*dim(prep_raschhigherror_100_10_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_100_10_wide$n)[1]^3*dim(prep_raschhigherror_100_10_wide$n)[1]^3, 5000)

c_check_raschhigherror_100_10_wide <- ConjointChecks(N=prep_raschhigherror_100_10_wide$N, n=prep_raschhigherror_100_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_10_wide <- as.data.frame(c_check_raschhigherror_100_10_wide@tab)

c_check_dat_raschhigherror_100_10_wide <- reshape2::melt(c_check_dat_raschhigherror_100_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_10_wide$N)[1], times = length(unique(oneRM_dat_100_10_wide$item))),
         sample_size = 100,
         item_number = 10,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_10_wide <- summary(c_check_raschhigherror_100_10_wide)

prep_raschhigherror_100_20_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_100_20_wide)

n.3mats <- if_else(dim(prep_raschhigherror_100_20_wide$n)[1]^3*dim(prep_raschhigherror_100_20_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_100_20_wide$n)[1]^3*dim(prep_raschhigherror_100_20_wide$n)[1]^3, 5000)

c_check_raschhigherror_100_20_wide <- ConjointChecks(N=prep_raschhigherror_100_20_wide$N, n=prep_raschhigherror_100_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_100_20_wide <- as.data.frame(c_check_raschhigherror_100_20_wide@tab)

c_check_dat_raschhigherror_100_20_wide <- reshape2::melt(c_check_dat_raschhigherror_100_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_100_20_wide$N)[1], times = length(unique(oneRM_dat_100_20_wide$item))),
         sample_size = 100,
         item_number = 20,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_100_20_wide <- summary(c_check_raschhigherror_100_20_wide)

prep_raschhigherror_250_5_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_5_wide)

n.3mats <- if_else(dim(prep_raschhigherror_250_5_wide$n)[1]^3*dim(prep_raschhigherror_250_5_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_250_5_wide$n)[1]^3*dim(prep_raschhigherror_250_5_wide$n)[1]^3, 5000)

c_check_raschhigherror_250_5_wide <- ConjointChecks(N=prep_raschhigherror_250_5_wide$N, n=prep_raschhigherror_250_5_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_5_wide <- as.data.frame(c_check_raschhigherror_250_5_wide@tab)

c_check_dat_raschhigherror_250_5_wide <- reshape2::melt(c_check_dat_raschhigherror_250_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_5_wide$N)[1], times = length(unique(oneRM_dat_250_5_wide$item))),
         sample_size = 250,
         item_number = 5,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_5_wide <- summary(c_check_raschhigherror_250_5_wide)

prep_raschhigherror_250_10_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_10_wide)

n.3mats <- if_else(dim(prep_raschhigherror_250_10_wide$n)[1]^3*dim(prep_raschhigherror_250_10_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_250_10_wide$n)[1]^3*dim(prep_raschhigherror_250_10_wide$n)[1]^3, 5000)

c_check_raschhigherror_250_10_wide <- ConjointChecks(N=prep_raschhigherror_250_10_wide$N, n=prep_raschhigherror_250_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_10_wide <- as.data.frame(c_check_raschhigherror_250_10_wide@tab)

c_check_dat_raschhigherror_250_10_wide <- reshape2::melt(c_check_dat_raschhigherror_250_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_10_wide$N)[1], times = length(unique(oneRM_dat_250_10_wide$item))),
         sample_size = 250,
         item_number = 10,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_10_wide <- summary(c_check_raschhigherror_250_10_wide)

prep_raschhigherror_250_20_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_250_20_wide)

n.3mats <- if_else(dim(prep_raschhigherror_250_20_wide$n)[1]^3*dim(prep_raschhigherror_250_20_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_250_20_wide$n)[1]^3*dim(prep_raschhigherror_250_20_wide$n)[1]^3, 5000)

c_check_raschhigherror_250_20_wide <- ConjointChecks(N=prep_raschhigherror_250_20_wide$N, n=prep_raschhigherror_250_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_250_20_wide <- as.data.frame(c_check_raschhigherror_250_20_wide@tab)

c_check_dat_raschhigherror_250_20_wide <- reshape2::melt(c_check_dat_raschhigherror_250_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_250_20_wide$N)[1], times = length(unique(oneRM_dat_250_20_wide$item))),
         sample_size = 250,
         item_number = 20,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_250_20_wide <- summary(c_check_raschhigherror_250_20_wide)

prep_raschhigherror_500_5_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_5_wide)

n.3mats <- if_else(dim(prep_raschhigherror_500_5_wide$n)[1]^3*dim(prep_raschhigherror_500_5_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_500_5_wide$n)[1]^3*dim(prep_raschhigherror_500_5_wide$n)[1]^3, 5000)

c_check_raschhigherror_500_5_wide <- ConjointChecks(N=prep_raschhigherror_500_5_wide$N, n=prep_raschhigherror_500_5_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_5_wide <- as.data.frame(c_check_raschhigherror_500_5_wide@tab)

c_check_dat_raschhigherror_500_5_wide <- reshape2::melt(c_check_dat_raschhigherror_500_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_5_wide$N)[1], times = length(unique(oneRM_dat_500_5_wide$item))),
         sample_size = 500,
         item_number = 5,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_5_wide <- summary(c_check_raschhigherror_500_5_wide)

prep_raschhigherror_500_10_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_10_wide)

n.3mats <- if_else(dim(prep_raschhigherror_500_10_wide$n)[1]^3*dim(prep_raschhigherror_500_10_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_500_10_wide$n)[1]^3*dim(prep_raschhigherror_500_10_wide$n)[1]^3, 5000)

c_check_raschhigherror_500_10_wide <- ConjointChecks(N=prep_raschhigherror_500_10_wide$N, n=prep_raschhigherror_500_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_10_wide <- as.data.frame(c_check_raschhigherror_500_10_wide@tab)

c_check_dat_raschhigherror_500_10_wide <- reshape2::melt(c_check_dat_raschhigherror_500_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_10_wide$N)[1], times = length(unique(oneRM_dat_500_10_wide$item))),
         sample_size = 500,
         item_number = 10,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_10_wide <- summary(c_check_raschhigherror_500_10_wide)

prep_raschhigherror_500_20_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_500_20_wide)

n.3mats <- if_else(dim(prep_raschhigherror_500_20_wide$n)[1]^3*dim(prep_raschhigherror_500_20_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_500_20_wide$n)[1]^3*dim(prep_raschhigherror_500_20_wide$n)[1]^3, 5000)

c_check_raschhigherror_500_20_wide <- ConjointChecks(N=prep_raschhigherror_500_20_wide$N, n=prep_raschhigherror_500_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_500_20_wide <- as.data.frame(c_check_raschhigherror_500_20_wide@tab)

c_check_dat_raschhigherror_500_20_wide <- reshape2::melt(c_check_dat_raschhigherror_500_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_500_20_wide$N)[1], times = length(unique(oneRM_dat_500_20_wide$item))),
         sample_size = 500,
         item_number = 20,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_500_20_wide <- summary(c_check_raschhigherror_500_20_wide)

prep_raschhigherror_1000_5_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_5_wide)

n.3mats <- if_else(dim(prep_raschhigherror_1000_5_wide$n)[1]^3*dim(prep_raschhigherror_1000_5_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_5_wide$n)[1]^3*dim(prep_raschhigherror_1000_5_wide$n)[1]^3, 5000)

c_check_raschhigherror_1000_5_wide <- ConjointChecks(N=prep_raschhigherror_1000_5_wide$N, n=prep_raschhigherror_1000_5_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_5_wide <- as.data.frame(c_check_raschhigherror_1000_5_wide@tab)

c_check_dat_raschhigherror_1000_5_wide <- reshape2::melt(c_check_dat_raschhigherror_1000_5_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_5_wide$N)[1], times = length(unique(oneRM_dat_1000_5_wide$item))),
         sample_size = 1000,
         item_number = 5,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_5_wide <- summary(c_check_raschhigherror_1000_5_wide)

prep_raschhigherror_1000_10_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_10_wide)

n.3mats <- if_else(dim(prep_raschhigherror_1000_10_wide$n)[1]^3*dim(prep_raschhigherror_1000_10_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_10_wide$n)[1]^3*dim(prep_raschhigherror_1000_10_wide$n)[1]^3, 5000)

c_check_raschhigherror_1000_10_wide <- ConjointChecks(N=prep_raschhigherror_1000_10_wide$N, n=prep_raschhigherror_1000_10_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_10_wide <- as.data.frame(c_check_raschhigherror_1000_10_wide@tab)

c_check_dat_raschhigherror_1000_10_wide <- reshape2::melt(c_check_dat_raschhigherror_1000_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_10_wide$N)[1], times = length(unique(oneRM_dat_1000_10_wide$item))),
         sample_size = 1000,
         item_number = 10,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_10_wide <- summary(c_check_raschhigherror_1000_10_wide)

prep_raschhigherror_1000_20_wide <- PrepareChecks(ss.lower=5, resp_raschhigherror_1000_20_wide)

n.3mats <- if_else(dim(prep_raschhigherror_1000_20_wide$n)[1]^3*dim(prep_raschhigherror_1000_20_wide$n)[1]^3 < 5000, dim(prep_raschhigherror_1000_20_wide$n)[1]^3*dim(prep_raschhigherror_1000_20_wide$n)[1]^3, 5000)

c_check_raschhigherror_1000_20_wide <- ConjointChecks(N=prep_raschhigherror_1000_20_wide$N, n=prep_raschhigherror_1000_20_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschhigherror_1000_20_wide <- as.data.frame(c_check_raschhigherror_1000_20_wide@tab)

c_check_dat_raschhigherror_1000_20_wide <- reshape2::melt(c_check_dat_raschhigherror_1000_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschhigherror_1000_20_wide$N)[1], times = length(unique(oneRM_dat_1000_20_wide$item))),
         sample_size = 1000,
         item_number = 20,
         range = "wide", response_type = "High Error (?? = 0.1)")

summary_c_check_raschhigherror_1000_20_wide <- summary(c_check_raschhigherror_1000_20_wide)

# Rasch low error
              # prep_raschlowerror_50_5_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_5_narrow)
              # 
              # n.3mats <- if_else(dim(prep_raschlowerror_50_5_narrow$n)[1]^3*dim(prep_raschlowerror_50_5_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_50_5_narrow$n)[1]^3*dim(prep_raschlowerror_50_5_narrow$n)[1]^3, 5000)
              # 
              # c_check_raschlowerror_50_5_narrow <- ConjointChecks(N=prep_raschlowerror_50_5_narrow$N, n=prep_raschlowerror_50_5_narrow$n, n.3mat = n.3mats,
              #                                             par.options=list(n.workers=cores,type="PSOCK"))
              # 
              # c_check_dat_raschlowerror_50_5_narrow <- as.data.frame(c_check_raschlowerror_50_5_narrow@tab)
              # 
              # c_check_dat_raschlowerror_50_5_narrow <- reshape2::melt(c_check_dat_raschlowerror_50_5_narrow) %>%
              #   mutate(item = variable,
              #          one_RM_group = rep(1:dim(prep_raschlowerror_50_5_narrow$N)[1], times = length(unique(oneRM_dat_50_5_narrow$item))),
              #          sample_size = 50,
              #          item_number = 5,
              #          range = "narrow", response_type = "Low Error (?? = 5)")
              # 
              # summary_c_check_raschlowerror_50_5_narrow <- summary(c_check_raschlowerror_50_5_narrow)

prep_raschlowerror_50_10_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_10_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_50_10_narrow$n)[1]^3*dim(prep_raschlowerror_50_10_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_50_10_narrow$n)[1]^3*dim(prep_raschlowerror_50_10_narrow$n)[1]^3, 5000)

c_check_raschlowerror_50_10_narrow <- ConjointChecks(N=prep_raschlowerror_50_10_narrow$N, n=prep_raschlowerror_50_10_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_50_10_narrow <- as.data.frame(c_check_raschlowerror_50_10_narrow@tab)

c_check_dat_raschlowerror_50_10_narrow <- reshape2::melt(c_check_dat_raschlowerror_50_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_50_10_narrow$N)[1], times = length(unique(oneRM_dat_50_10_narrow$item))),
         sample_size = 50,
         item_number = 10,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_50_10_narrow <- summary(c_check_raschlowerror_50_10_narrow)

prep_raschlowerror_50_20_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_20_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_50_20_narrow$n)[1]^3*dim(prep_raschlowerror_50_20_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_50_20_narrow$n)[1]^3*dim(prep_raschlowerror_50_20_narrow$n)[1]^3, 5000)

c_check_raschlowerror_50_20_narrow <- ConjointChecks(N=prep_raschlowerror_50_20_narrow$N, n=prep_raschlowerror_50_20_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_50_20_narrow <- as.data.frame(c_check_raschlowerror_50_20_narrow@tab)

c_check_dat_raschlowerror_50_20_narrow <- reshape2::melt(c_check_dat_raschlowerror_50_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_50_20_narrow$N)[1], times = length(unique(oneRM_dat_50_20_narrow$item))),
         sample_size = 50,
         item_number = 20,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_50_20_narrow <- summary(c_check_raschlowerror_50_20_narrow)

prep_raschlowerror_100_5_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_5_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_100_5_narrow$n)[1]^3*dim(prep_raschlowerror_100_5_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_100_5_narrow$n)[1]^3*dim(prep_raschlowerror_100_5_narrow$n)[1]^3, 5000)

c_check_raschlowerror_100_5_narrow <- ConjointChecks(N=prep_raschlowerror_100_5_narrow$N, n=prep_raschlowerror_100_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_100_5_narrow <- as.data.frame(c_check_raschlowerror_100_5_narrow@tab)

c_check_dat_raschlowerror_100_5_narrow <- reshape2::melt(c_check_dat_raschlowerror_100_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_100_5_narrow$N)[1], times = length(unique(oneRM_dat_100_5_narrow$item))),
         sample_size = 100,
         item_number = 5,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_100_5_narrow <- summary(c_check_raschlowerror_100_5_narrow)

prep_raschlowerror_100_10_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_10_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_100_10_narrow$n)[1]^3*dim(prep_raschlowerror_100_10_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_100_10_narrow$n)[1]^3*dim(prep_raschlowerror_100_10_narrow$n)[1]^3, 5000)

c_check_raschlowerror_100_10_narrow <- ConjointChecks(N=prep_raschlowerror_100_10_narrow$N, n=prep_raschlowerror_100_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_100_10_narrow <- as.data.frame(c_check_raschlowerror_100_10_narrow@tab)

c_check_dat_raschlowerror_100_10_narrow <- reshape2::melt(c_check_dat_raschlowerror_100_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_100_10_narrow$N)[1], times = length(unique(oneRM_dat_100_10_narrow$item))),
         sample_size = 100,
         item_number = 10,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_100_10_narrow <- summary(c_check_raschlowerror_100_10_narrow)

prep_raschlowerror_100_20_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_20_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_100_20_narrow$n)[1]^3*dim(prep_raschlowerror_100_20_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_100_20_narrow$n)[1]^3*dim(prep_raschlowerror_100_20_narrow$n)[1]^3, 5000)

c_check_raschlowerror_100_20_narrow <- ConjointChecks(N=prep_raschlowerror_100_20_narrow$N, n=prep_raschlowerror_100_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_100_20_narrow <- as.data.frame(c_check_raschlowerror_100_20_narrow@tab)

c_check_dat_raschlowerror_100_20_narrow <- reshape2::melt(c_check_dat_raschlowerror_100_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_100_20_narrow$N)[1], times = length(unique(oneRM_dat_100_20_narrow$item))),
         sample_size = 100,
         item_number = 20,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_100_20_narrow <- summary(c_check_raschlowerror_100_20_narrow)

prep_raschlowerror_250_5_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_5_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_250_5_narrow$n)[1]^3*dim(prep_raschlowerror_250_5_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_250_5_narrow$n)[1]^3*dim(prep_raschlowerror_250_5_narrow$n)[1]^3, 5000)

c_check_raschlowerror_250_5_narrow <- ConjointChecks(N=prep_raschlowerror_250_5_narrow$N, n=prep_raschlowerror_250_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_250_5_narrow <- as.data.frame(c_check_raschlowerror_250_5_narrow@tab)

c_check_dat_raschlowerror_250_5_narrow <- reshape2::melt(c_check_dat_raschlowerror_250_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_250_5_narrow$N)[1], times = length(unique(oneRM_dat_250_5_narrow$item))),
         sample_size = 250,
         item_number = 5,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_250_5_narrow <- summary(c_check_raschlowerror_250_5_narrow)

prep_raschlowerror_250_10_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_10_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_250_10_narrow$n)[1]^3*dim(prep_raschlowerror_250_10_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_250_10_narrow$n)[1]^3*dim(prep_raschlowerror_250_10_narrow$n)[1]^3, 5000)

c_check_raschlowerror_250_10_narrow <- ConjointChecks(N=prep_raschlowerror_250_10_narrow$N, n=prep_raschlowerror_250_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_250_10_narrow <- as.data.frame(c_check_raschlowerror_250_10_narrow@tab)

c_check_dat_raschlowerror_250_10_narrow <- reshape2::melt(c_check_dat_raschlowerror_250_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_250_10_narrow$N)[1], times = length(unique(oneRM_dat_250_10_narrow$item))),
         sample_size = 250,
         item_number = 10,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_250_10_narrow <- summary(c_check_raschlowerror_250_10_narrow)

prep_raschlowerror_250_20_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_20_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_250_20_narrow$n)[1]^3*dim(prep_raschlowerror_250_20_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_250_20_narrow$n)[1]^3*dim(prep_raschlowerror_250_20_narrow$n)[1]^3, 5000)

c_check_raschlowerror_250_20_narrow <- ConjointChecks(N=prep_raschlowerror_250_20_narrow$N, n=prep_raschlowerror_250_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_250_20_narrow <- as.data.frame(c_check_raschlowerror_250_20_narrow@tab)

c_check_dat_raschlowerror_250_20_narrow <- reshape2::melt(c_check_dat_raschlowerror_250_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_250_20_narrow$N)[1], times = length(unique(oneRM_dat_250_20_narrow$item))),
         sample_size = 250,
         item_number = 20,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_250_20_narrow <- summary(c_check_raschlowerror_250_20_narrow)

prep_raschlowerror_500_5_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_5_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_500_5_narrow$n)[1]^3*dim(prep_raschlowerror_500_5_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_500_5_narrow$n)[1]^3*dim(prep_raschlowerror_500_5_narrow$n)[1]^3, 5000)

c_check_raschlowerror_500_5_narrow <- ConjointChecks(N=prep_raschlowerror_500_5_narrow$N, n=prep_raschlowerror_500_5_narrow$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_500_5_narrow <- as.data.frame(c_check_raschlowerror_500_5_narrow@tab)

c_check_dat_raschlowerror_500_5_narrow <- reshape2::melt(c_check_dat_raschlowerror_500_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_500_5_narrow$N)[1], times = length(unique(oneRM_dat_500_5_narrow$item))),
         sample_size = 500,
         item_number = 5,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_500_5_narrow <- summary(c_check_raschlowerror_500_5_narrow)

prep_raschlowerror_500_10_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_10_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_500_10_narrow$n)[1]^3*dim(prep_raschlowerror_500_10_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_500_10_narrow$n)[1]^3*dim(prep_raschlowerror_500_10_narrow$n)[1]^3, 5000)

c_check_raschlowerror_500_10_narrow <- ConjointChecks(N=prep_raschlowerror_500_10_narrow$N, n=prep_raschlowerror_500_10_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_500_10_narrow <- as.data.frame(c_check_raschlowerror_500_10_narrow@tab)

c_check_dat_raschlowerror_500_10_narrow <- reshape2::melt(c_check_dat_raschlowerror_500_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_500_10_narrow$N)[1], times = length(unique(oneRM_dat_500_10_narrow$item))),
         sample_size = 500,
         item_number = 10,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_500_10_narrow <- summary(c_check_raschlowerror_500_10_narrow)

prep_raschlowerror_500_20_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_20_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_500_20_narrow$n)[1]^3*dim(prep_raschlowerror_500_20_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_500_20_narrow$n)[1]^3*dim(prep_raschlowerror_500_20_narrow$n)[1]^3, 5000)

c_check_raschlowerror_500_20_narrow <- ConjointChecks(N=prep_raschlowerror_500_20_narrow$N, n=prep_raschlowerror_500_20_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_500_20_narrow <- as.data.frame(c_check_raschlowerror_500_20_narrow@tab)

c_check_dat_raschlowerror_500_20_narrow <- reshape2::melt(c_check_dat_raschlowerror_500_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_500_20_narrow$N)[1], times = length(unique(oneRM_dat_500_20_narrow$item))),
         sample_size = 500,
         item_number = 20,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_500_20_narrow <- summary(c_check_raschlowerror_500_20_narrow)

prep_raschlowerror_1000_5_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_5_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_1000_5_narrow$n)[1]^3*dim(prep_raschlowerror_1000_5_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_5_narrow$n)[1]^3*dim(prep_raschlowerror_1000_5_narrow$n)[1]^3, 5000)

c_check_raschlowerror_1000_5_narrow <- ConjointChecks(N=prep_raschlowerror_1000_5_narrow$N, n=prep_raschlowerror_1000_5_narrow$n, n.3mat = n.3mats,
                                              par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_1000_5_narrow <- as.data.frame(c_check_raschlowerror_1000_5_narrow@tab)

c_check_dat_raschlowerror_1000_5_narrow <- reshape2::melt(c_check_dat_raschlowerror_1000_5_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_1000_5_narrow$N)[1], times = length(unique(oneRM_dat_1000_5_narrow$item))),
         sample_size = 1000,
         item_number = 5,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_1000_5_narrow <- summary(c_check_raschlowerror_1000_5_narrow)

prep_raschlowerror_1000_10_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_10_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_1000_10_narrow$n)[1]^3*dim(prep_raschlowerror_1000_10_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_10_narrow$n)[1]^3*dim(prep_raschlowerror_1000_10_narrow$n)[1]^3, 5000)

c_check_raschlowerror_1000_10_narrow <- ConjointChecks(N=prep_raschlowerror_1000_10_narrow$N, n=prep_raschlowerror_1000_10_narrow$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_1000_10_narrow <- as.data.frame(c_check_raschlowerror_1000_10_narrow@tab)

c_check_dat_raschlowerror_1000_10_narrow <- reshape2::melt(c_check_dat_raschlowerror_1000_10_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_1000_10_narrow$N)[1], times = length(unique(oneRM_dat_1000_10_narrow$item))),
         sample_size = 1000,
         item_number = 10,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_1000_10_narrow <- summary(c_check_raschlowerror_1000_10_narrow)

prep_raschlowerror_1000_20_narrow <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_20_narrow)

n.3mats <- if_else(dim(prep_raschlowerror_1000_20_narrow$n)[1]^3*dim(prep_raschlowerror_1000_20_narrow$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_20_narrow$n)[1]^3*dim(prep_raschlowerror_1000_20_narrow$n)[1]^3, 5000)

c_check_raschlowerror_1000_20_narrow <- ConjointChecks(N=prep_raschlowerror_1000_20_narrow$N, n=prep_raschlowerror_1000_20_narrow$n, n.3mat = n.3mats,
                                               par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_1000_20_narrow <- as.data.frame(c_check_raschlowerror_1000_20_narrow@tab)

c_check_dat_raschlowerror_1000_20_narrow <- reshape2::melt(c_check_dat_raschlowerror_1000_20_narrow) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_1000_20_narrow$N)[1], times = length(unique(oneRM_dat_1000_20_narrow$item))),
         sample_size = 1000,
         item_number = 20,
         range = "narrow", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_1000_20_narrow <- summary(c_check_raschlowerror_1000_20_narrow)

          # prep_raschlowerror_50_5_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_5_wide)
          # 
          # n.3mats <- if_else(dim(prep_raschlowerror_50_5_wide$n)[1]^3*dim(prep_raschlowerror_50_5_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_50_5_wide$n)[1]^3*dim(prep_raschlowerror_50_5_wide$n)[1]^3, 5000)
          # 
          # c_check_raschlowerror_50_5_wide <- ConjointChecks(N=prep_raschlowerror_50_5_wide$N, n=prep_raschlowerror_50_5_wide$n, n.3mat = n.3mats,
          #                                           par.options=list(n.workers=cores,type="PSOCK"))
          # 
          # c_check_dat_raschlowerror_50_5_wide <- as.data.frame(c_check_raschlowerror_50_5_wide@tab)
          # 
          # c_check_dat_raschlowerror_50_5_wide <- reshape2::melt(c_check_dat_raschlowerror_50_5_wide) %>%
          #   mutate(item = variable,
          #          one_RM_group = rep(1:dim(prep_raschlowerror_50_5_wide$N)[1], times = length(unique(oneRM_dat_50_5_wide$item))),
          #          sample_size = 50,
          #          item_number = 5,
          #          range = "wide", response_type = "Low Error (?? = 5)")
          # 
          # summary_c_check_raschlowerror_50_5_wide <- summary(c_check_raschlowerror_50_5_wide)

prep_raschlowerror_50_10_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_10_wide)

n.3mats <- if_else(dim(prep_raschlowerror_50_10_wide$n)[1]^3*dim(prep_raschlowerror_50_10_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_50_10_wide$n)[1]^3*dim(prep_raschlowerror_50_10_wide$n)[1]^3, 5000)

c_check_raschlowerror_50_10_wide <- ConjointChecks(N=prep_raschlowerror_50_10_wide$N, n=prep_raschlowerror_50_10_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_50_10_wide <- as.data.frame(c_check_raschlowerror_50_10_wide@tab)

c_check_dat_raschlowerror_50_10_wide <- reshape2::melt(c_check_dat_raschlowerror_50_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_50_10_wide$N)[1], times = length(unique(oneRM_dat_50_10_wide$item))),
         sample_size = 50,
         item_number = 10,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_50_10_wide <- summary(c_check_raschlowerror_50_10_wide)

prep_raschlowerror_50_20_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_50_20_wide)

n.3mats <- if_else(dim(prep_raschlowerror_50_20_wide$n)[1]^3*dim(prep_raschlowerror_50_20_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_50_20_wide$n)[1]^3*dim(prep_raschlowerror_50_20_wide$n)[1]^3, 5000)

c_check_raschlowerror_50_20_wide <- ConjointChecks(N=prep_raschlowerror_50_20_wide$N, n=prep_raschlowerror_50_20_wide$n, n.3mat = n.3mats,
                                           par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_50_20_wide <- as.data.frame(c_check_raschlowerror_50_20_wide@tab)

c_check_dat_raschlowerror_50_20_wide <- reshape2::melt(c_check_dat_raschlowerror_50_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_50_20_wide$N)[1], times = length(unique(oneRM_dat_50_20_wide$item))),
         sample_size = 50,
         item_number = 20,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_50_20_wide <- summary(c_check_raschlowerror_50_20_wide)

          # prep_raschlowerror_100_5_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_5_wide)
          # 
          # n.3mats <- if_else(dim(prep_raschlowerror_100_5_wide$n)[1]^3*dim(prep_raschlowerror_100_5_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_100_5_wide$n)[1]^3*dim(prep_raschlowerror_100_5_wide$n)[1]^3, 5000)
          # 
          # c_check_raschlowerror_100_5_wide <- ConjointChecks(N=prep_raschlowerror_100_5_wide$N, n=prep_raschlowerror_100_5_wide$n, n.3mat = n.3mats,
          #                                            par.options=list(n.workers=cores,type="PSOCK"))
          # 
          # c_check_dat_raschlowerror_100_5_wide <- as.data.frame(c_check_raschlowerror_100_5_wide@tab)
          # 
          # c_check_dat_raschlowerror_100_5_wide <- reshape2::melt(c_check_dat_raschlowerror_100_5_wide) %>%
          #   mutate(item = variable,
          #          one_RM_group = rep(1:dim(prep_raschlowerror_100_5_wide$N)[1], times = length(unique(oneRM_dat_100_5_wide$item))),
          #          sample_size = 100,
          #          item_number = 5,
          #          range = "wide", response_type = "Low Error (?? = 5)")
          # 
          # summary_c_check_raschlowerror_100_5_wide <- summary(c_check_raschlowerror_100_5_wide)

prep_raschlowerror_100_10_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_10_wide)

n.3mats <- if_else(dim(prep_raschlowerror_100_10_wide$n)[1]^3*dim(prep_raschlowerror_100_10_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_100_10_wide$n)[1]^3*dim(prep_raschlowerror_100_10_wide$n)[1]^3, 5000)

c_check_raschlowerror_100_10_wide <- ConjointChecks(N=prep_raschlowerror_100_10_wide$N, n=prep_raschlowerror_100_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_100_10_wide <- as.data.frame(c_check_raschlowerror_100_10_wide@tab)

c_check_dat_raschlowerror_100_10_wide <- reshape2::melt(c_check_dat_raschlowerror_100_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_100_10_wide$N)[1], times = length(unique(oneRM_dat_100_10_wide$item))),
         sample_size = 100,
         item_number = 10,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_100_10_wide <- summary(c_check_raschlowerror_100_10_wide)

prep_raschlowerror_100_20_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_100_20_wide)

n.3mats <- if_else(dim(prep_raschlowerror_100_20_wide$n)[1]^3*dim(prep_raschlowerror_100_20_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_100_20_wide$n)[1]^3*dim(prep_raschlowerror_100_20_wide$n)[1]^3, 5000)

c_check_raschlowerror_100_20_wide <- ConjointChecks(N=prep_raschlowerror_100_20_wide$N, n=prep_raschlowerror_100_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_100_20_wide <- as.data.frame(c_check_raschlowerror_100_20_wide@tab)

c_check_dat_raschlowerror_100_20_wide <- reshape2::melt(c_check_dat_raschlowerror_100_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_100_20_wide$N)[1], times = length(unique(oneRM_dat_100_20_wide$item))),
         sample_size = 100,
         item_number = 20,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_100_20_wide <- summary(c_check_raschlowerror_100_20_wide)

          # prep_raschlowerror_250_5_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_5_wide)
          # 
          # n.3mats <- if_else(dim(prep_raschlowerror_250_5_wide$n)[1]^3*dim(prep_raschlowerror_250_5_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_250_5_wide$n)[1]^3*dim(prep_raschlowerror_250_5_wide$n)[1]^3, 5000)
          # 
          # c_check_raschlowerror_250_5_wide <- ConjointChecks(N=prep_raschlowerror_250_5_wide$N, n=prep_raschlowerror_250_5_wide$n, n.3mat = n.3mats,
          #                                            par.options=list(n.workers=cores,type="PSOCK"))
          # 
          # c_check_dat_raschlowerror_250_5_wide <- as.data.frame(c_check_raschlowerror_250_5_wide@tab)
          # 
          # c_check_dat_raschlowerror_250_5_wide <- reshape2::melt(c_check_dat_raschlowerror_250_5_wide) %>%
          #   mutate(item = variable,
          #          one_RM_group = rep(1:dim(prep_raschlowerror_250_5_wide$N)[1], times = length(unique(oneRM_dat_250_5_wide$item))),
          #          sample_size = 250,
          #          item_number = 5,
          #          range = "wide", response_type = "Low Error (?? = 5)")
          # 
          # summary_c_check_raschlowerror_250_5_wide <- summary(c_check_raschlowerror_250_5_wide)

prep_raschlowerror_250_10_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_10_wide)

n.3mats <- if_else(dim(prep_raschlowerror_250_10_wide$n)[1]^3*dim(prep_raschlowerror_250_10_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_250_10_wide$n)[1]^3*dim(prep_raschlowerror_250_10_wide$n)[1]^3, 5000)

c_check_raschlowerror_250_10_wide <- ConjointChecks(N=prep_raschlowerror_250_10_wide$N, n=prep_raschlowerror_250_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_250_10_wide <- as.data.frame(c_check_raschlowerror_250_10_wide@tab)

c_check_dat_raschlowerror_250_10_wide <- reshape2::melt(c_check_dat_raschlowerror_250_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_250_10_wide$N)[1], times = length(unique(oneRM_dat_250_10_wide$item))),
         sample_size = 250,
         item_number = 10,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_250_10_wide <- summary(c_check_raschlowerror_250_10_wide)

prep_raschlowerror_250_20_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_250_20_wide)

n.3mats <- if_else(dim(prep_raschlowerror_250_20_wide$n)[1]^3*dim(prep_raschlowerror_250_20_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_250_20_wide$n)[1]^3*dim(prep_raschlowerror_250_20_wide$n)[1]^3, 5000)

c_check_raschlowerror_250_20_wide <- ConjointChecks(N=prep_raschlowerror_250_20_wide$N, n=prep_raschlowerror_250_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_250_20_wide <- as.data.frame(c_check_raschlowerror_250_20_wide@tab)

c_check_dat_raschlowerror_250_20_wide <- reshape2::melt(c_check_dat_raschlowerror_250_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_250_20_wide$N)[1], times = length(unique(oneRM_dat_250_20_wide$item))),
         sample_size = 250,
         item_number = 20,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_250_20_wide <- summary(c_check_raschlowerror_250_20_wide)

          # prep_raschlowerror_500_5_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_5_wide)
          # 
          # n.3mats <- if_else(dim(prep_raschlowerror_500_5_wide$n)[1]^3*dim(prep_raschlowerror_500_5_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_500_5_wide$n)[1]^3*dim(prep_raschlowerror_500_5_wide$n)[1]^3, 5000)
          # 
          # c_check_raschlowerror_500_5_wide <- ConjointChecks(N=prep_raschlowerror_500_5_wide$N, n=prep_raschlowerror_500_5_wide$n, n.3mat = n.3mats,
          #                                            par.options=list(n.workers=cores,type="PSOCK"))
          # 
          # c_check_dat_raschlowerror_500_5_wide <- as.data.frame(c_check_raschlowerror_500_5_wide@tab)
          # 
          # c_check_dat_raschlowerror_500_5_wide <- reshape2::melt(c_check_dat_raschlowerror_500_5_wide) %>%
          #   mutate(item = variable,
          #          one_RM_group = rep(1:dim(prep_raschlowerror_500_5_wide$N)[1], times = length(unique(oneRM_dat_500_5_wide$item))),
          #          sample_size = 500,
          #          item_number = 5,
          #          range = "wide", response_type = "Low Error (?? = 5)")
          # 
          # summary_c_check_raschlowerror_500_5_wide <- summary(c_check_raschlowerror_500_5_wide)

prep_raschlowerror_500_10_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_10_wide)

n.3mats <- if_else(dim(prep_raschlowerror_500_10_wide$n)[1]^3*dim(prep_raschlowerror_500_10_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_500_10_wide$n)[1]^3*dim(prep_raschlowerror_500_10_wide$n)[1]^3, 5000)

c_check_raschlowerror_500_10_wide <- ConjointChecks(N=prep_raschlowerror_500_10_wide$N, n=prep_raschlowerror_500_10_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_500_10_wide <- as.data.frame(c_check_raschlowerror_500_10_wide@tab)

c_check_dat_raschlowerror_500_10_wide <- reshape2::melt(c_check_dat_raschlowerror_500_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_500_10_wide$N)[1], times = length(unique(oneRM_dat_500_10_wide$item))),
         sample_size = 500,
         item_number = 10,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_500_10_wide <- summary(c_check_raschlowerror_500_10_wide)

prep_raschlowerror_500_20_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_500_20_wide)

n.3mats <- if_else(dim(prep_raschlowerror_500_20_wide$n)[1]^3*dim(prep_raschlowerror_500_20_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_500_20_wide$n)[1]^3*dim(prep_raschlowerror_500_20_wide$n)[1]^3, 5000)

c_check_raschlowerror_500_20_wide <- ConjointChecks(N=prep_raschlowerror_500_20_wide$N, n=prep_raschlowerror_500_20_wide$n, n.3mat = n.3mats,
                                            par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_500_20_wide <- as.data.frame(c_check_raschlowerror_500_20_wide@tab)

c_check_dat_raschlowerror_500_20_wide <- reshape2::melt(c_check_dat_raschlowerror_500_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_500_20_wide$N)[1], times = length(unique(oneRM_dat_500_20_wide$item))),
         sample_size = 500,
         item_number = 20,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_500_20_wide <- summary(c_check_raschlowerror_500_20_wide)

          # prep_raschlowerror_1000_5_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_5_wide)
          # 
          # n.3mats <- if_else(dim(prep_raschlowerror_1000_5_wide$n)[1]^3*dim(prep_raschlowerror_1000_5_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_5_wide$n)[1]^3*dim(prep_raschlowerror_1000_5_wide$n)[1]^3, 5000)
          # 
          # c_check_raschlowerror_1000_5_wide <- ConjointChecks(N=prep_raschlowerror_1000_5_wide$N, n=prep_raschlowerror_1000_5_wide$n, n.3mat = n.3mats,
          #                                             par.options=list(n.workers=cores,type="PSOCK"))
          # 
          # c_check_dat_raschlowerror_1000_5_wide <- as.data.frame(c_check_raschlowerror_1000_5_wide@tab)
          # 
          # c_check_dat_raschlowerror_1000_5_wide <- reshape2::melt(c_check_dat_raschlowerror_1000_5_wide) %>%
          #   mutate(item = variable,
          #          one_RM_group = rep(1:dim(prep_raschlowerror_1000_5_wide$N)[1], times = length(unique(oneRM_dat_1000_5_wide$item))),
          #          sample_size = 1000,
          #          item_number = 5,
          #          range = "wide", response_type = "Low Error (?? = 5)")
          # 
          # summary_c_check_raschlowerror_1000_5_wide <- summary(c_check_raschlowerror_1000_5_wide)

prep_raschlowerror_1000_10_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_10_wide)

n.3mats <- if_else(dim(prep_raschlowerror_1000_10_wide$n)[1]^3*dim(prep_raschlowerror_1000_10_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_10_wide$n)[1]^3*dim(prep_raschlowerror_1000_10_wide$n)[1]^3, 5000)

c_check_raschlowerror_1000_10_wide <- ConjointChecks(N=prep_raschlowerror_1000_10_wide$N, n=prep_raschlowerror_1000_10_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_1000_10_wide <- as.data.frame(c_check_raschlowerror_1000_10_wide@tab)

c_check_dat_raschlowerror_1000_10_wide <- reshape2::melt(c_check_dat_raschlowerror_1000_10_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_1000_10_wide$N)[1], times = length(unique(oneRM_dat_1000_10_wide$item))),
         sample_size = 1000,
         item_number = 10,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_1000_10_wide <- summary(c_check_raschlowerror_1000_10_wide)

prep_raschlowerror_1000_20_wide <- PrepareChecks(ss.lower=5, resp_raschlowerror_1000_20_wide)

n.3mats <- if_else(dim(prep_raschlowerror_1000_20_wide$n)[1]^3*dim(prep_raschlowerror_1000_20_wide$n)[1]^3 < 5000, dim(prep_raschlowerror_1000_20_wide$n)[1]^3*dim(prep_raschlowerror_1000_20_wide$n)[1]^3, 5000)

c_check_raschlowerror_1000_20_wide <- ConjointChecks(N=prep_raschlowerror_1000_20_wide$N, n=prep_raschlowerror_1000_20_wide$n, n.3mat = n.3mats,
                                             par.options=list(n.workers=cores,type="PSOCK"))

c_check_dat_raschlowerror_1000_20_wide <- as.data.frame(c_check_raschlowerror_1000_20_wide@tab)

c_check_dat_raschlowerror_1000_20_wide <- reshape2::melt(c_check_dat_raschlowerror_1000_20_wide) %>%
  mutate(item = variable,
         one_RM_group = rep(1:dim(prep_raschlowerror_1000_20_wide$N)[1], times = length(unique(oneRM_dat_1000_20_wide$item))),
         sample_size = 1000,
         item_number = 20,
         range = "wide", response_type = "Low Error (?? = 5)")

summary_c_check_raschlowerror_1000_20_wide <- summary(c_check_raschlowerror_1000_20_wide)



### Combine and plot the conjoint checks

c_check_dat <- rbind(
  c_check_dat_rasch_50_5_narrow,
  c_check_dat_rasch_50_10_narrow,
  c_check_dat_rasch_50_20_narrow,
  c_check_dat_rasch_100_5_narrow,
  c_check_dat_rasch_100_10_narrow,
  c_check_dat_rasch_100_20_narrow,
  c_check_dat_rasch_250_5_narrow,
  c_check_dat_rasch_250_10_narrow,
  c_check_dat_rasch_250_20_narrow,
  c_check_dat_rasch_500_5_narrow,
  c_check_dat_rasch_500_10_narrow,
  c_check_dat_rasch_500_20_narrow,
  c_check_dat_rasch_1000_5_narrow,
  c_check_dat_rasch_1000_10_narrow,
  c_check_dat_rasch_1000_20_narrow,
  c_check_dat_rasch_50_5_wide,
  c_check_dat_rasch_50_10_wide,
  c_check_dat_rasch_50_20_wide,
  c_check_dat_rasch_100_5_wide,
  c_check_dat_rasch_100_10_wide,
  c_check_dat_rasch_100_20_wide,
  c_check_dat_rasch_250_5_wide,
  c_check_dat_rasch_250_10_wide,
  c_check_dat_rasch_250_20_wide,
  c_check_dat_rasch_500_5_wide,
  c_check_dat_rasch_500_10_wide,
  c_check_dat_rasch_500_20_wide,
  c_check_dat_rasch_1000_5_wide,
  c_check_dat_rasch_1000_10_wide,
  c_check_dat_rasch_1000_20_wide,
  c_check_dat_raschhigherror_50_5_narrow,
  c_check_dat_raschhigherror_50_10_narrow,
  c_check_dat_raschhigherror_50_20_narrow,
  c_check_dat_raschhigherror_100_5_narrow,
  c_check_dat_raschhigherror_100_10_narrow,
  c_check_dat_raschhigherror_100_20_narrow,
  c_check_dat_raschhigherror_250_5_narrow,
  c_check_dat_raschhigherror_250_10_narrow,
  c_check_dat_raschhigherror_250_20_narrow,
  c_check_dat_raschhigherror_500_5_narrow,
  c_check_dat_raschhigherror_500_10_narrow,
  c_check_dat_raschhigherror_500_20_narrow,
  c_check_dat_raschhigherror_1000_5_narrow,
  c_check_dat_raschhigherror_1000_10_narrow,
  c_check_dat_raschhigherror_1000_20_narrow,
  c_check_dat_raschhigherror_50_5_wide,
  c_check_dat_raschhigherror_50_10_wide,
  c_check_dat_raschhigherror_50_20_wide,
  c_check_dat_raschhigherror_100_5_wide,
  c_check_dat_raschhigherror_100_10_wide,
  c_check_dat_raschhigherror_100_20_wide,
  c_check_dat_raschhigherror_250_5_wide,
  c_check_dat_raschhigherror_250_10_wide,
  c_check_dat_raschhigherror_250_20_wide,
  c_check_dat_raschhigherror_500_5_wide,
  c_check_dat_raschhigherror_500_10_wide,
  c_check_dat_raschhigherror_500_20_wide,
  c_check_dat_raschhigherror_1000_5_wide,
  c_check_dat_raschhigherror_1000_10_wide,
  c_check_dat_raschhigherror_1000_20_wide,
  # c_check_dat_raschlowerror_50_5_narrow,
  c_check_dat_raschlowerror_50_10_narrow,
  c_check_dat_raschlowerror_50_20_narrow,
  c_check_dat_raschlowerror_100_5_narrow,
  c_check_dat_raschlowerror_100_10_narrow,
  c_check_dat_raschlowerror_100_20_narrow,
  c_check_dat_raschlowerror_250_5_narrow,
  c_check_dat_raschlowerror_250_10_narrow,
  c_check_dat_raschlowerror_250_20_narrow,
  c_check_dat_raschlowerror_500_5_narrow,
  c_check_dat_raschlowerror_500_10_narrow,
  c_check_dat_raschlowerror_500_20_narrow,
  c_check_dat_raschlowerror_1000_5_narrow,
  c_check_dat_raschlowerror_1000_10_narrow,
  c_check_dat_raschlowerror_1000_20_narrow,
  # c_check_dat_raschlowerror_50_5_wide,
  c_check_dat_raschlowerror_50_10_wide,
  c_check_dat_raschlowerror_50_20_wide,
  # c_check_dat_raschlowerror_100_5_wide,
  c_check_dat_raschlowerror_100_10_wide,
  c_check_dat_raschlowerror_100_20_wide,
  # c_check_dat_raschlowerror_250_5_wide,
  c_check_dat_raschlowerror_250_10_wide,
  c_check_dat_raschlowerror_250_20_wide,
  # c_check_dat_raschlowerror_500_5_wide,
  c_check_dat_raschlowerror_500_10_wide,
  c_check_dat_raschlowerror_500_20_wide,
  # c_check_dat_raschlowerror_1000_5_wide,
  c_check_dat_raschlowerror_1000_10_wide,
  c_check_dat_raschlowerror_1000_20_wide
) %>%
  mutate(sample_size = factor(sample_size, levels = c("50","100","250","500","1000")),
         item_number = factor(item_number, levels = c("5","10","20")),
         response_type = factor(response_type, levels = c("Low Error (?? = 5)", "Typical Error (?? = 1)", "High Error (?? = 0.1)"))) %>%
  mutate(range = dplyr::recode(range, narrow = "Narrow Item Range (60 to 140 kg)", wide = "Wide Item Range (10 to 200 kg)"),
         item_number_lab = "Number of Items",
         sample_size_lab = "Sample Size")

save(c_check_dat, file = "models/c_check_dat")

summary_c_check <- data.frame(
  sample_size = c(50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  # 50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  # 50,50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  50,50,100,100,100,250,250,250,500,500,500,1000,1000,1000,
                  50,50,100,100,250,250,500,500,1000,1000),
  item_number = c(5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  # 5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  # 5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  10,20,5,10,20,5,10,20,5,10,20,5,10,20,
                  10,20,10,20,10,20,10,20,10,20
                  ),
  range = c("narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow",
            "wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide",
            "narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow",
            "wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide",
            # "narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow",
            # "wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide","wide",
            "narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow","narrow",
            "wide","wide","wide","wide","wide","wide","wide","wide","wide","wide"),
  response_type = c("Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)", 
                    "Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)","Typical Error (?? = 1)",
                    "High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)",
                    "High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)","High Error (?? = 0.1)",
                    # "Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)", 
                    # "Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)",
                    "Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)", 
                    "Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)","Low Error (?? = 5)"
                    ),
  unweighted = c(summary_c_check_rasch_50_5_narrow$Means$unweighted,
                 summary_c_check_rasch_50_10_narrow$Means$unweighted,
                 summary_c_check_rasch_50_20_narrow$Means$unweighted,
                 summary_c_check_rasch_100_5_narrow$Means$unweighted,
                 summary_c_check_rasch_100_10_narrow$Means$unweighted,
                 summary_c_check_rasch_100_20_narrow$Means$unweighted,
                 summary_c_check_rasch_250_5_narrow$Means$unweighted,
                 summary_c_check_rasch_250_10_narrow$Means$unweighted,
                 summary_c_check_rasch_250_20_narrow$Means$unweighted,
                 summary_c_check_rasch_500_5_narrow$Means$unweighted,
                 summary_c_check_rasch_500_10_narrow$Means$unweighted,
                 summary_c_check_rasch_500_20_narrow$Means$unweighted,
                 summary_c_check_rasch_1000_5_narrow$Means$unweighted,
                 summary_c_check_rasch_1000_10_narrow$Means$unweighted,
                 summary_c_check_rasch_1000_20_narrow$Means$unweighted,
                 summary_c_check_rasch_50_5_wide$Means$unweighted,
                 summary_c_check_rasch_50_10_wide$Means$unweighted,
                 summary_c_check_rasch_50_20_wide$Means$unweighted,
                 summary_c_check_rasch_100_5_wide$Means$unweighted,
                 summary_c_check_rasch_100_10_wide$Means$unweighted,
                 summary_c_check_rasch_100_20_wide$Means$unweighted,
                 summary_c_check_rasch_250_5_wide$Means$unweighted,
                 summary_c_check_rasch_250_10_wide$Means$unweighted,
                 summary_c_check_rasch_250_20_wide$Means$unweighted,
                 summary_c_check_rasch_500_5_wide$Means$unweighted,
                 summary_c_check_rasch_500_10_wide$Means$unweighted,
                 summary_c_check_rasch_500_20_wide$Means$unweighted,
                 summary_c_check_rasch_1000_5_wide$Means$unweighted,
                 summary_c_check_rasch_1000_10_wide$Means$unweighted,
                 summary_c_check_rasch_1000_20_wide$Means$unweighted,
                 summary_c_check_raschhigherror_50_5_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_50_10_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_50_20_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_100_5_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_100_10_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_100_20_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_250_5_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_250_10_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_250_20_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_500_5_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_500_10_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_500_20_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_1000_5_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_1000_10_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_1000_20_narrow$Means$unweighted,
                 summary_c_check_raschhigherror_50_5_wide$Means$unweighted,
                 summary_c_check_raschhigherror_50_10_wide$Means$unweighted,
                 summary_c_check_raschhigherror_50_20_wide$Means$unweighted,
                 summary_c_check_raschhigherror_100_5_wide$Means$unweighted,
                 summary_c_check_raschhigherror_100_10_wide$Means$unweighted,
                 summary_c_check_raschhigherror_100_20_wide$Means$unweighted,
                 summary_c_check_raschhigherror_250_5_wide$Means$unweighted,
                 summary_c_check_raschhigherror_250_10_wide$Means$unweighted,
                 summary_c_check_raschhigherror_250_20_wide$Means$unweighted,
                 summary_c_check_raschhigherror_500_5_wide$Means$unweighted,
                 summary_c_check_raschhigherror_500_10_wide$Means$unweighted,
                 summary_c_check_raschhigherror_500_20_wide$Means$unweighted,
                 summary_c_check_raschhigherror_1000_5_wide$Means$unweighted,
                 summary_c_check_raschhigherror_1000_10_wide$Means$unweighted,
                 summary_c_check_raschhigherror_1000_20_wide$Means$unweighted,
                 # summary_c_check_raschlowerror_50_5_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_50_10_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_50_20_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_100_5_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_100_10_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_100_20_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_250_5_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_250_10_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_250_20_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_500_5_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_500_10_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_500_20_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_1000_5_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_1000_10_narrow$Means$unweighted,
                 summary_c_check_raschlowerror_1000_20_narrow$Means$unweighted,
                 # summary_c_check_raschlowerror_50_5_wide$Means$unweighted,
                 summary_c_check_raschlowerror_50_10_wide$Means$unweighted,
                 summary_c_check_raschlowerror_50_20_wide$Means$unweighted,
                 # summary_c_check_raschlowerror_100_5_wide$Means$unweighted,
                 summary_c_check_raschlowerror_100_10_wide$Means$unweighted,
                 summary_c_check_raschlowerror_100_20_wide$Means$unweighted,
                 # summary_c_check_raschlowerror_250_5_wide$Means$unweighted,
                 summary_c_check_raschlowerror_250_10_wide$Means$unweighted,
                 summary_c_check_raschlowerror_250_20_wide$Means$unweighted,
                 # summary_c_check_raschlowerror_500_5_wide$Means$unweighted,
                 summary_c_check_raschlowerror_500_10_wide$Means$unweighted,
                 summary_c_check_raschlowerror_500_20_wide$Means$unweighted,
                 # summary_c_check_raschlowerror_1000_5_wide$Means$unweighted,
                 summary_c_check_raschlowerror_1000_10_wide$Means$unweighted,
                 summary_c_check_raschlowerror_1000_20_wide$Means$unweighted),
  weighted = c(summary_c_check_rasch_50_5_narrow$Means$weighted,
               summary_c_check_rasch_50_10_narrow$Means$weighted,
               summary_c_check_rasch_50_20_narrow$Means$weighted,
               summary_c_check_rasch_100_5_narrow$Means$weighted,
               summary_c_check_rasch_100_10_narrow$Means$weighted,
               summary_c_check_rasch_100_20_narrow$Means$weighted,
               summary_c_check_rasch_250_5_narrow$Means$weighted,
               summary_c_check_rasch_250_10_narrow$Means$weighted,
               summary_c_check_rasch_250_20_narrow$Means$weighted,
               summary_c_check_rasch_500_5_narrow$Means$weighted,
               summary_c_check_rasch_500_10_narrow$Means$weighted,
               summary_c_check_rasch_500_20_narrow$Means$weighted,
               summary_c_check_rasch_1000_5_narrow$Means$weighted,
               summary_c_check_rasch_1000_10_narrow$Means$weighted,
               summary_c_check_rasch_1000_20_narrow$Means$weighted,
               summary_c_check_rasch_50_5_wide$Means$weighted,
               summary_c_check_rasch_50_10_wide$Means$weighted,
               summary_c_check_rasch_50_20_wide$Means$weighted,
               summary_c_check_rasch_100_5_wide$Means$weighted,
               summary_c_check_rasch_100_10_wide$Means$weighted,
               summary_c_check_rasch_100_20_wide$Means$weighted,
               summary_c_check_rasch_250_5_wide$Means$weighted,
               summary_c_check_rasch_250_10_wide$Means$weighted,
               summary_c_check_rasch_250_20_wide$Means$weighted,
               summary_c_check_rasch_500_5_wide$Means$weighted,
               summary_c_check_rasch_500_10_wide$Means$weighted,
               summary_c_check_rasch_500_20_wide$Means$weighted,
               summary_c_check_rasch_1000_5_wide$Means$weighted,
               summary_c_check_rasch_1000_10_wide$Means$weighted,
               summary_c_check_rasch_1000_20_wide$Means$weighted,
               summary_c_check_raschhigherror_50_5_narrow$Means$weighted,
               summary_c_check_raschhigherror_50_10_narrow$Means$weighted,
               summary_c_check_raschhigherror_50_20_narrow$Means$weighted,
               summary_c_check_raschhigherror_100_5_narrow$Means$weighted,
               summary_c_check_raschhigherror_100_10_narrow$Means$weighted,
               summary_c_check_raschhigherror_100_20_narrow$Means$weighted,
               summary_c_check_raschhigherror_250_5_narrow$Means$weighted,
               summary_c_check_raschhigherror_250_10_narrow$Means$weighted,
               summary_c_check_raschhigherror_250_20_narrow$Means$weighted,
               summary_c_check_raschhigherror_500_5_narrow$Means$weighted,
               summary_c_check_raschhigherror_500_10_narrow$Means$weighted,
               summary_c_check_raschhigherror_500_20_narrow$Means$weighted,
               summary_c_check_raschhigherror_1000_5_narrow$Means$weighted,
               summary_c_check_raschhigherror_1000_10_narrow$Means$weighted,
               summary_c_check_raschhigherror_1000_20_narrow$Means$weighted,
               summary_c_check_raschhigherror_50_5_wide$Means$weighted,
               summary_c_check_raschhigherror_50_10_wide$Means$weighted,
               summary_c_check_raschhigherror_50_20_wide$Means$weighted,
               summary_c_check_raschhigherror_100_5_wide$Means$weighted,
               summary_c_check_raschhigherror_100_10_wide$Means$weighted,
               summary_c_check_raschhigherror_100_20_wide$Means$weighted,
               summary_c_check_raschhigherror_250_5_wide$Means$weighted,
               summary_c_check_raschhigherror_250_10_wide$Means$weighted,
               summary_c_check_raschhigherror_250_20_wide$Means$weighted,
               summary_c_check_raschhigherror_500_5_wide$Means$weighted,
               summary_c_check_raschhigherror_500_10_wide$Means$weighted,
               summary_c_check_raschhigherror_500_20_wide$Means$weighted,
               summary_c_check_raschhigherror_1000_5_wide$Means$weighted,
               summary_c_check_raschhigherror_1000_10_wide$Means$weighted,
               summary_c_check_raschhigherror_1000_20_wide$Means$weighted,
               # summary_c_check_raschlowerror_50_5_narrow$Means$weighted,
               summary_c_check_raschlowerror_50_10_narrow$Means$weighted,
               summary_c_check_raschlowerror_50_20_narrow$Means$weighted,
               summary_c_check_raschlowerror_100_5_narrow$Means$weighted,
               summary_c_check_raschlowerror_100_10_narrow$Means$weighted,
               summary_c_check_raschlowerror_100_20_narrow$Means$weighted,
               summary_c_check_raschlowerror_250_5_narrow$Means$weighted,
               summary_c_check_raschlowerror_250_10_narrow$Means$weighted,
               summary_c_check_raschlowerror_250_20_narrow$Means$weighted,
               summary_c_check_raschlowerror_500_5_narrow$Means$weighted,
               summary_c_check_raschlowerror_500_10_narrow$Means$weighted,
               summary_c_check_raschlowerror_500_20_narrow$Means$weighted,
               summary_c_check_raschlowerror_1000_5_narrow$Means$weighted,
               summary_c_check_raschlowerror_1000_10_narrow$Means$weighted,
               summary_c_check_raschlowerror_1000_20_narrow$Means$weighted,
               # summary_c_check_raschlowerror_50_5_wide$Means$weighted,
               summary_c_check_raschlowerror_50_10_wide$Means$weighted,
               summary_c_check_raschlowerror_50_20_wide$Means$weighted,
               # summary_c_check_raschlowerror_100_5_wide$Means$weighted,
               summary_c_check_raschlowerror_100_10_wide$Means$weighted,
               summary_c_check_raschlowerror_100_20_wide$Means$weighted,
               # summary_c_check_raschlowerror_250_5_wide$Means$weighted,
               summary_c_check_raschlowerror_250_10_wide$Means$weighted,
               summary_c_check_raschlowerror_250_20_wide$Means$weighted,
               # summary_c_check_raschlowerror_500_5_wide$Means$weighted,
               summary_c_check_raschlowerror_500_10_wide$Means$weighted,
               summary_c_check_raschlowerror_500_20_wide$Means$weighted,
               # summary_c_check_raschlowerror_1000_5_wide$Means$weighted,
               summary_c_check_raschlowerror_1000_10_wide$Means$weighted,
               summary_c_check_raschlowerror_1000_20_wide$Means$weighted)
) %>%
  mutate(sample_size = factor(sample_size, levels = c("50","100","250","500","1000")),
         item_number = factor(item_number, levels = c("5","10","20")),
         response_type = factor(response_type, levels = c("Low Error (?? = 5)", "Typical Error (?? = 1)", "High Error (?? = 0.1)"))) %>%
  mutate(range = dplyr::recode(range, narrow = "Narrow Item Range (60 to 140 kg)", wide = "Wide Item Range (10 to 200 kg)"),
         item_number_lab = "Number of Items",
         sample_size_lab = "Sample Size")

save(summary_c_check, file = "models/summary_c_check")

c_check_plot <- c_check_dat %>%
  ggplot(aes(x=one_RM_group,y=value,group=item)) +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed", color = "red") +
  geom_line(alpha = 0.5) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  labs(x = "Increasing Ability (i.e., 1RM)",
       y = "Proportion of Violations") +
  geom_text(data = summary_c_check, 
            aes(x=15, y=0.9, group=1, label = paste("Unweighted Mean = ", round(unweighted, digits=3))),
            size = 2) +
  geom_text(data = summary_c_check, 
            aes(x=15, y=0.7, group=1, label = paste("Weighted Mean = ", round(weighted, digits=3))),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(panel.grid = element_blank())

save(c_check_plot, file = "plots/c_check_plot")

c_check_plot

ggsave("plots/c_check_plot.png", width = 15, height = 10, device = "png", dpi = 300)

violations_means_plot <- summary_c_check %>%
  pivot_longer(5:6, values_to = "violations", names_to = "weighted_unweighted") %>%
  ggplot(aes(x = sample_size, y = violations, group = item_number)) +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed", color = "red") +
  geom_line(aes(linetype = item_number)) +
  labs(x = "Sample Size",
      y = "Proportion of Violations",
      linetype = "Item Number") +
  scale_y_continuous(limits = c(0,1)) +
  facet_nested(weighted_unweighted ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=7),
        panel.grid = element_blank())

save(violations_means_plot, file = "plots/violations_means_plot")

violations_means_plot 

ggsave("plots/violations_means_plot.png", width = 12.5, height = 5, device = "png", dpi = 300)

  

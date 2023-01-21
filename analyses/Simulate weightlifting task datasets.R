##### Analysis Script for "Can we measure effort in cognitive tasks?: Examining the application of Additive Conjoint Measurement and the Rasch model"

# Author: James Steele

##### In this script we just simulate the weightlifting task datasets
# For each we simulate the following responses
  # No errors reflecting a guttman type scale i.e., if the load is <1RM then the participant definitely can lift it
  # Rasch response with normally distributed error
  # Rasch response with normally distributed, but substantial, error i.e., a very low discrimination (0.1)
  # Rasch response with normally distributed, but low, error i.e., a very high discrimination (5)
# We simulate a range of sample sizes for both persons and items  
  # i.e., persons 50, 100, 250, 500, 1000; and items 5, 10, 20
# We also vary the items as either being (see error and precision paper - reflects largest RMSE differences)
  # Evenly spaced over a narrow range [60,140] kg
  # Evenly spaced over a wide range [10,200] kg
# Note, when using the wide range we get some very large exponents for the difference between oneRM and item so we use an if_else to impute 1 if NaN occurs
# For each we simulate 500 responses to each person-item pair (for the purpose of the rasch models)

##### Note, I've been somewhat lazy with this and was running out of time for the submission deadline 
  # as such I've just copied and pasted the code rather than nesting varying conditions and simulating within loops

### Open required packages
library(tidyverse)
library(faux)

### Functions
round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

##### Simulate the datasets

### For 50 participants; 5 items; narrow range [60,140]
persons = 50
items = 5
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_50_5_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 50 participants; 10 items; narrow range [60,140]
persons = 50
items = 10
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_50_10_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 50 participants; 20 items; narrow range [60,140]
persons = 50
items = 20
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_50_20_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 5 items; narrow range [60,140]
persons = 100
items = 5
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_100_5_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 10 items; narrow range [60,140]
persons = 100
items = 10
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_100_10_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 20 items; narrow range [60,140]
persons = 100
items = 20
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_100_20_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 5 items; narrow range [60,140]
persons = 250
items = 5
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_250_5_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 10 items; narrow range [60,140]
persons = 250
items = 10
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_250_10_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 20 items; narrow range [60,140]
persons = 250
items = 20
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_250_20_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 5 items; narrow range [60,140]
persons = 500
items = 5
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_500_5_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 10 items; narrow range [60,140]
persons = 500
items = 10
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_500_10_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 20 items; narrow range [60,140]
persons = 500
items = 20
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_500_20_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 5 items; narrow range [60,140]
persons = 1000
items = 5
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_1000_5_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 10 items; narrow range [60,140]
persons = 1000
items = 10
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_1000_10_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 20 items; narrow range [60,140]
persons = 1000
items = 20
min_item = 60
max_item = 140

set.seed(1988)
oneRM_dat_1000_20_narrow <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 50, length=50)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 50 participants; 5 items; wide range [10,200]
persons = 50
items = 5
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_50_5_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 50 participants; 10 items; wide range [10,200]
persons = 50
items = 10
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_50_10_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 50 participants; 20 items; wide range [10,200]
persons = 50
items = 20
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_50_20_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 5 items; wide range [10,200]
persons = 100
items = 5
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_100_5_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 10 items; wide range [10,200]
persons = 100
items = 10
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_100_10_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 100 participants; 20 items; wide range [10,200]
persons = 100
items = 20
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_100_20_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 5 items; wide range [10,200]
persons = 250
items = 5
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_250_5_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 10 items; wide range [10,200]
persons = 250
items = 10
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_250_10_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 250 participants; 20 items; wide range [10,200]
persons = 250
items = 20
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_250_20_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 5 items; wide range [10,200]
persons = 500
items = 5
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_500_5_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 10 items; wide range [10,200]
persons = 500
items = 10
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_500_10_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 500 participants; 20 items; wide range [10,200]
persons = 500
items = 20
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_500_20_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 5 items; wide range [10,200]
persons = 1000
items = 5
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_1000_5_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 10 items; wide range [10,200]
persons = 1000
items = 10
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_1000_10_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

### For 1000 participants; 20 items; wide range [10,200]
persons = 1000
items = 20
min_item = 10
max_item = 200

set.seed(1988)
oneRM_dat_1000_20_wide <- add_random(person = persons) %>%
  add_ranef("person", one_RM = 25) %>%
  add_within("person", item = seq(from = min_item, to = max_item, length=items)) %>%
  add_within(c("person", "item"), trial = seq(from = 1, to = 500, length=500)) %>%
  mutate(person = as.factor(person),
         item = as.numeric(as.character(item))) %>%
  mutate(one_RM = one_RM+100,
         actual_effort = if_else(one_RM >= item, (item/one_RM)*100, 100)) %>%
  mutate(response_guttman = if_else(one_RM >= item, 1, 0), # Guttman type scale response
  ) %>%
  mutate(response_prob_rasch = one_RM/(one_RM + item),
         test = runif(item*one_RM),
         response_rasch = if_else(response_prob_rasch == "NaN", 1, if_else(response_prob_rasch > test, 1, 0)) # Rasch response
  ) %>%
  mutate(response_prob_raschhigherror = one_RM^0.1/(one_RM^0.1 + item^0.1),
         test = runif(item*one_RM),
         response_raschhigherror = if_else(response_prob_raschhigherror  == "NaN", 1, if_else(response_prob_raschhigherror > test, 1, 0)) # Rasch response substantial error
  ) %>%
  mutate(response_prob_raschlowerror = one_RM^5/(one_RM^5 + item^5),
         test = runif(item*one_RM),
         response_raschlowerror = if_else(response_prob_raschlowerror  == "NaN", 1, if_else(response_prob_raschlowerror > test, 1, 0)) # Rasch response minimal error
  ) %>%
  mutate(item = as.factor(item),
         one_RM_group = round_any(one_RM, 2.5)) %>%
  filter(one_RM >= 0)

##### Save all datasets
save(oneRM_dat_50_5_narrow, file = "datasets/oneRM_dat_50_5_narrow")
save(oneRM_dat_50_10_narrow, file = "datasets/oneRM_dat_50_10_narrow")
save(oneRM_dat_50_20_narrow, file = "datasets/oneRM_dat_50_20_narrow")
save(oneRM_dat_100_5_narrow, file = "datasets/oneRM_dat_100_5_narrow")
save(oneRM_dat_100_10_narrow, file = "datasets/oneRM_dat_100_10_narrow")
save(oneRM_dat_100_20_narrow, file = "datasets/oneRM_dat_100_20_narrow")
save(oneRM_dat_250_5_narrow, file = "datasets/oneRM_dat_250_5_narrow")
save(oneRM_dat_250_10_narrow, file = "datasets/oneRM_dat_250_10_narrow")
save(oneRM_dat_250_20_narrow, file = "datasets/oneRM_dat_250_20_narrow")
save(oneRM_dat_500_5_narrow, file = "datasets/oneRM_dat_500_5_narrow")
save(oneRM_dat_500_10_narrow, file = "datasets/oneRM_dat_500_10_narrow")
save(oneRM_dat_500_20_narrow, file = "datasets/oneRM_dat_500_20_narrow")
save(oneRM_dat_1000_5_narrow, file = "datasets/oneRM_dat_1000_5_narrow")
save(oneRM_dat_1000_10_narrow, file = "datasets/oneRM_dat_1000_10_narrow")
save(oneRM_dat_1000_20_narrow, file = "datasets/oneRM_dat_1000_20_narrow")
save(oneRM_dat_50_5_wide, file = "datasets/oneRM_dat_50_5_wide")
save(oneRM_dat_50_10_wide, file = "datasets/oneRM_dat_50_10_wide")
save(oneRM_dat_50_20_wide, file = "datasets/oneRM_dat_50_20_wide")
save(oneRM_dat_100_5_wide, file = "datasets/oneRM_dat_100_5_wide")
save(oneRM_dat_100_10_wide, file = "datasets/oneRM_dat_100_10_wide")
save(oneRM_dat_100_20_wide, file = "datasets/oneRM_dat_100_20_wide")
save(oneRM_dat_250_5_wide, file = "datasets/oneRM_dat_250_5_wide")
save(oneRM_dat_250_10_wide, file = "datasets/oneRM_dat_250_10_wide")
save(oneRM_dat_250_20_wide, file = "datasets/oneRM_dat_250_20_wide")
save(oneRM_dat_500_5_wide, file = "datasets/oneRM_dat_500_5_wide")
save(oneRM_dat_500_10_wide, file = "datasets/oneRM_dat_500_10_wide")
save(oneRM_dat_500_20_wide, file = "datasets/oneRM_dat_500_20_wide")
save(oneRM_dat_1000_5_wide, file = "datasets/oneRM_dat_1000_5_wide")
save(oneRM_dat_1000_10_wide, file = "datasets/oneRM_dat_1000_10_wide")
save(oneRM_dat_1000_20_wide, file = "datasets/oneRM_dat_1000_20_wide")
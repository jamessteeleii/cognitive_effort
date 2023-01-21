##### Analysis Script for "Can we measure effort in cognitive tasks?: Examining the application of Additive Conjoint Measurement and the Rasch model"

# Author: James Steele

##### In this script we fit Rasch models to all of the simulated datasets 
### We explore whether we can recover the original kg scale for loads and 1RMs
### And then we examine the different methods for deriving effort and compare to the actual effort calculated from loads and 1RMs

### Open required packages
library(tidyverse)
library(lme4)
library(patchwork)
library(ggh4x)

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

##### Using lme4 to fit and then using the mean-sigma approach to recover the original kg scale for loads and 1RMs 

### Rasch datasets

glmer_rasch_50_5_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                               data = oneRM_dat_50_5_narrow, 
                                               family = binomial(link = "logit"))

person_abilities_rasch_50_5_narrow <- as.data.frame(coef(glmer_rasch_50_5_narrow)$person)

person_abilities_rasch_50_5_narrow <- person_abilities_rasch_50_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_5_narrow <- as.data.frame(coef(glmer_rasch_50_5_narrow)$item)

item_difficulties_rasch_50_5_narrow <- item_difficulties_rasch_50_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_narrow <- left_join(oneRM_dat_50_5_narrow, person_abilities_rasch_50_5_narrow, by="person") %>%
  left_join(item_difficulties_rasch_50_5_narrow, by="item")

oneRM_abilities_rasch_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_5_narrow <- rbind(oneRM_abilities_rasch_50_5_narrow, load_difficulties_rasch_50_5_narrow) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_50_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                data = oneRM_dat_50_10_narrow, 
                                                family = binomial(link = "logit"))

person_abilities_rasch_50_10_narrow <- as.data.frame(coef(glmer_rasch_50_10_narrow)$person)

person_abilities_rasch_50_10_narrow <- person_abilities_rasch_50_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_10_narrow <- as.data.frame(coef(glmer_rasch_50_10_narrow)$item)

item_difficulties_rasch_50_10_narrow <- item_difficulties_rasch_50_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_narrow <- left_join(oneRM_dat_50_10_narrow, person_abilities_rasch_50_10_narrow, by="person") %>%
  left_join(item_difficulties_rasch_50_10_narrow, by="item")

oneRM_abilities_rasch_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_10_narrow <- rbind(oneRM_abilities_rasch_50_10_narrow, load_difficulties_rasch_50_10_narrow) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_50_20_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                data = oneRM_dat_50_20_narrow, 
                                                family = binomial(link = "logit"))

person_abilities_rasch_50_20_narrow <- as.data.frame(coef(glmer_rasch_50_20_narrow)$person)

person_abilities_rasch_50_20_narrow <- person_abilities_rasch_50_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_20_narrow <- as.data.frame(coef(glmer_rasch_50_20_narrow)$item)

item_difficulties_rasch_50_20_narrow <- item_difficulties_rasch_50_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_narrow <- left_join(oneRM_dat_50_20_narrow, person_abilities_rasch_50_20_narrow, by="person") %>%
  left_join(item_difficulties_rasch_50_20_narrow, by="item")

oneRM_abilities_rasch_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_20_narrow <- rbind(oneRM_abilities_rasch_50_20_narrow, load_difficulties_rasch_50_20_narrow) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_100_5_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                data = oneRM_dat_100_5_narrow, 
                                                family = binomial(link = "logit"))

person_abilities_rasch_100_5_narrow <- as.data.frame(coef(glmer_rasch_100_5_narrow)$person)

person_abilities_rasch_100_5_narrow <- person_abilities_rasch_100_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_5_narrow <- as.data.frame(coef(glmer_rasch_100_5_narrow)$item)

item_difficulties_rasch_100_5_narrow <- item_difficulties_rasch_100_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_narrow <- left_join(oneRM_dat_100_5_narrow, person_abilities_rasch_100_5_narrow, by="person") %>%
  left_join(item_difficulties_rasch_100_5_narrow, by="item")

oneRM_abilities_rasch_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_5_narrow <- rbind(oneRM_abilities_rasch_100_5_narrow, load_difficulties_rasch_100_5_narrow) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_100_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_100_10_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_100_10_narrow <- as.data.frame(coef(glmer_rasch_100_10_narrow)$person)

person_abilities_rasch_100_10_narrow <- person_abilities_rasch_100_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_10_narrow <- as.data.frame(coef(glmer_rasch_100_10_narrow)$item)

item_difficulties_rasch_100_10_narrow <- item_difficulties_rasch_100_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_narrow <- left_join(oneRM_dat_100_10_narrow, person_abilities_rasch_100_10_narrow, by="person") %>%
  left_join(item_difficulties_rasch_100_10_narrow, by="item")

oneRM_abilities_rasch_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_10_narrow <- rbind(oneRM_abilities_rasch_100_10_narrow, load_difficulties_rasch_100_10_narrow) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_100_20_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_100_20_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_100_20_narrow <- as.data.frame(coef(glmer_rasch_100_20_narrow)$person)

person_abilities_rasch_100_20_narrow <- person_abilities_rasch_100_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_20_narrow <- as.data.frame(coef(glmer_rasch_100_20_narrow)$item)

item_difficulties_rasch_100_20_narrow <- item_difficulties_rasch_100_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_narrow <- left_join(oneRM_dat_100_20_narrow, person_abilities_rasch_100_20_narrow, by="person") %>%
  left_join(item_difficulties_rasch_100_20_narrow, by="item")

oneRM_abilities_rasch_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_20_narrow <- rbind(oneRM_abilities_rasch_100_20_narrow, load_difficulties_rasch_100_20_narrow) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_250_5_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                data = oneRM_dat_250_5_narrow, 
                                                family = binomial(link = "logit"))

person_abilities_rasch_250_5_narrow <- as.data.frame(coef(glmer_rasch_250_5_narrow)$person)

person_abilities_rasch_250_5_narrow <- person_abilities_rasch_250_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_5_narrow <- as.data.frame(coef(glmer_rasch_250_5_narrow)$item)

item_difficulties_rasch_250_5_narrow <- item_difficulties_rasch_250_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_narrow <- left_join(oneRM_dat_250_5_narrow, person_abilities_rasch_250_5_narrow, by="person") %>%
  left_join(item_difficulties_rasch_250_5_narrow, by="item")

oneRM_abilities_rasch_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_5_narrow <- rbind(oneRM_abilities_rasch_250_5_narrow, load_difficulties_rasch_250_5_narrow) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_250_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_250_10_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_250_10_narrow <- as.data.frame(coef(glmer_rasch_250_10_narrow)$person)

person_abilities_rasch_250_10_narrow <- person_abilities_rasch_250_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_10_narrow <- as.data.frame(coef(glmer_rasch_250_10_narrow)$item)

item_difficulties_rasch_250_10_narrow <- item_difficulties_rasch_250_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_narrow <- left_join(oneRM_dat_250_10_narrow, person_abilities_rasch_250_10_narrow, by="person") %>%
  left_join(item_difficulties_rasch_250_10_narrow, by="item")

oneRM_abilities_rasch_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_10_narrow <- rbind(oneRM_abilities_rasch_250_10_narrow, load_difficulties_rasch_250_10_narrow) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_250_20_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_250_20_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_250_20_narrow <- as.data.frame(coef(glmer_rasch_250_20_narrow)$person)

person_abilities_rasch_250_20_narrow <- person_abilities_rasch_250_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_20_narrow <- as.data.frame(coef(glmer_rasch_250_20_narrow)$item)

item_difficulties_rasch_250_20_narrow <- item_difficulties_rasch_250_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_narrow <- left_join(oneRM_dat_250_20_narrow, person_abilities_rasch_250_20_narrow, by="person") %>%
  left_join(item_difficulties_rasch_250_20_narrow, by="item")

oneRM_abilities_rasch_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_20_narrow <- rbind(oneRM_abilities_rasch_250_20_narrow, load_difficulties_rasch_250_20_narrow) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_500_5_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                data = oneRM_dat_500_5_narrow, 
                                                family = binomial(link = "logit"))

person_abilities_rasch_500_5_narrow <- as.data.frame(coef(glmer_rasch_500_5_narrow)$person)

person_abilities_rasch_500_5_narrow <- person_abilities_rasch_500_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_5_narrow <- as.data.frame(coef(glmer_rasch_500_5_narrow)$item)

item_difficulties_rasch_500_5_narrow <- item_difficulties_rasch_500_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_narrow <- left_join(oneRM_dat_500_5_narrow, person_abilities_rasch_500_5_narrow, by="person") %>%
  left_join(item_difficulties_rasch_500_5_narrow, by="item")

oneRM_abilities_rasch_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_5_narrow <- rbind(oneRM_abilities_rasch_500_5_narrow, load_difficulties_rasch_500_5_narrow) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_500_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_500_10_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_500_10_narrow <- as.data.frame(coef(glmer_rasch_500_10_narrow)$person)

person_abilities_rasch_500_10_narrow <- person_abilities_rasch_500_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_10_narrow <- as.data.frame(coef(glmer_rasch_500_10_narrow)$item)

item_difficulties_rasch_500_10_narrow <- item_difficulties_rasch_500_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_narrow <- left_join(oneRM_dat_500_10_narrow, person_abilities_rasch_500_10_narrow, by="person") %>%
  left_join(item_difficulties_rasch_500_10_narrow, by="item")

oneRM_abilities_rasch_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_10_narrow <- rbind(oneRM_abilities_rasch_500_10_narrow, load_difficulties_rasch_500_10_narrow) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_500_20_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_500_20_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_500_20_narrow <- as.data.frame(coef(glmer_rasch_500_20_narrow)$person)

person_abilities_rasch_500_20_narrow <- person_abilities_rasch_500_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_20_narrow <- as.data.frame(coef(glmer_rasch_500_20_narrow)$item)

item_difficulties_rasch_500_20_narrow <- item_difficulties_rasch_500_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_narrow <- left_join(oneRM_dat_500_20_narrow, person_abilities_rasch_500_20_narrow, by="person") %>%
  left_join(item_difficulties_rasch_500_20_narrow, by="item")

oneRM_abilities_rasch_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_20_narrow <- rbind(oneRM_abilities_rasch_500_20_narrow, load_difficulties_rasch_500_20_narrow) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_1000_5_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                 data = oneRM_dat_1000_5_narrow, 
                                                 family = binomial(link = "logit"))

person_abilities_rasch_1000_5_narrow <- as.data.frame(coef(glmer_rasch_1000_5_narrow)$person)

person_abilities_rasch_1000_5_narrow <- person_abilities_rasch_1000_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_5_narrow <- as.data.frame(coef(glmer_rasch_1000_5_narrow)$item)

item_difficulties_rasch_1000_5_narrow <- item_difficulties_rasch_1000_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_narrow <- left_join(oneRM_dat_1000_5_narrow, person_abilities_rasch_1000_5_narrow, by="person") %>%
  left_join(item_difficulties_rasch_1000_5_narrow, by="item")

oneRM_abilities_rasch_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_5_narrow <- rbind(oneRM_abilities_rasch_1000_5_narrow, load_difficulties_rasch_1000_5_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_1000_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                  data = oneRM_dat_1000_10_narrow, 
                                                  family = binomial(link = "logit"))

person_abilities_rasch_1000_10_narrow <- as.data.frame(coef(glmer_rasch_1000_10_narrow)$person)

person_abilities_rasch_1000_10_narrow <- person_abilities_rasch_1000_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_10_narrow <- as.data.frame(coef(glmer_rasch_1000_10_narrow)$item)

item_difficulties_rasch_1000_10_narrow <- item_difficulties_rasch_1000_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_narrow <- left_join(oneRM_dat_1000_10_narrow, person_abilities_rasch_1000_10_narrow, by="person") %>%
  left_join(item_difficulties_rasch_1000_10_narrow, by="item")

oneRM_abilities_rasch_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_10_narrow <- rbind(oneRM_abilities_rasch_1000_10_narrow, load_difficulties_rasch_1000_10_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_1000_20_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                                  data = oneRM_dat_1000_20_narrow, 
                                                  family = binomial(link = "logit"))

person_abilities_rasch_1000_20_narrow <- as.data.frame(coef(glmer_rasch_1000_20_narrow)$person)

person_abilities_rasch_1000_20_narrow <- person_abilities_rasch_1000_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_20_narrow <- as.data.frame(coef(glmer_rasch_1000_20_narrow)$item)

item_difficulties_rasch_1000_20_narrow <- item_difficulties_rasch_1000_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_narrow <- left_join(oneRM_dat_1000_20_narrow, person_abilities_rasch_1000_20_narrow, by="person") %>%
  left_join(item_difficulties_rasch_1000_20_narrow, by="item")

oneRM_abilities_rasch_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_20_narrow <- rbind(oneRM_abilities_rasch_1000_20_narrow, load_difficulties_rasch_1000_20_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "narrow") 

glmer_rasch_50_5_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_50_5_wide, 
                                       family = binomial(link = "logit"))



person_abilities_rasch_50_5_wide <- as.data.frame(coef(glmer_rasch_50_5_wide)$person)

person_abilities_rasch_50_5_wide <- person_abilities_rasch_50_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_5_wide <- as.data.frame(coef(glmer_rasch_50_5_wide)$item)

item_difficulties_rasch_50_5_wide <- item_difficulties_rasch_50_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_wide <- left_join(oneRM_dat_50_5_wide, person_abilities_rasch_50_5_wide, by="person") %>%
  left_join(item_difficulties_rasch_50_5_wide, by="item")

oneRM_abilities_rasch_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_5_wide <- rbind(oneRM_abilities_rasch_50_5_wide, load_difficulties_rasch_50_5_wide) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_50_10_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_10_wide, 
                                        family = binomial(link = "logit"))

person_abilities_rasch_50_10_wide <- as.data.frame(coef(glmer_rasch_50_10_wide)$person)

person_abilities_rasch_50_10_wide <- person_abilities_rasch_50_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_10_wide <- as.data.frame(coef(glmer_rasch_50_10_wide)$item)

item_difficulties_rasch_50_10_wide <- item_difficulties_rasch_50_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_wide <- left_join(oneRM_dat_50_10_wide, person_abilities_rasch_50_10_wide, by="person") %>%
  left_join(item_difficulties_rasch_50_10_wide, by="item")

oneRM_abilities_rasch_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_10_wide <- rbind(oneRM_abilities_rasch_50_10_wide, load_difficulties_rasch_50_10_wide) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_50_20_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_20_wide, 
                                        family = binomial(link = "logit"))

person_abilities_rasch_50_20_wide <- as.data.frame(coef(glmer_rasch_50_20_wide)$person)

person_abilities_rasch_50_20_wide <- person_abilities_rasch_50_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_50_20_wide <- as.data.frame(coef(glmer_rasch_50_20_wide)$item)

item_difficulties_rasch_50_20_wide <- item_difficulties_rasch_50_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_wide <- left_join(oneRM_dat_50_20_wide, person_abilities_rasch_50_20_wide, by="person") %>%
  left_join(item_difficulties_rasch_50_20_wide, by="item")

oneRM_abilities_rasch_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_50_20_wide <- rbind(oneRM_abilities_rasch_50_20_wide, load_difficulties_rasch_50_20_wide) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_100_5_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_100_5_wide, 
                                        family = binomial(link = "logit"))

person_abilities_rasch_100_5_wide <- as.data.frame(coef(glmer_rasch_100_5_wide)$person)

person_abilities_rasch_100_5_wide <- person_abilities_rasch_100_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_5_wide <- as.data.frame(coef(glmer_rasch_100_5_wide)$item)

item_difficulties_rasch_100_5_wide <- item_difficulties_rasch_100_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_wide <- left_join(oneRM_dat_100_5_wide, person_abilities_rasch_100_5_wide, by="person") %>%
  left_join(item_difficulties_rasch_100_5_wide, by="item")

oneRM_abilities_rasch_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_5_wide <- rbind(oneRM_abilities_rasch_100_5_wide, load_difficulties_rasch_100_5_wide) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_100_10_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_10_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_100_10_wide <- as.data.frame(coef(glmer_rasch_100_10_wide)$person)

person_abilities_rasch_100_10_wide <- person_abilities_rasch_100_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_10_wide <- as.data.frame(coef(glmer_rasch_100_10_wide)$item)

item_difficulties_rasch_100_10_wide <- item_difficulties_rasch_100_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_wide <- left_join(oneRM_dat_100_10_wide, person_abilities_rasch_100_10_wide, by="person") %>%
  left_join(item_difficulties_rasch_100_10_wide, by="item")

oneRM_abilities_rasch_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_10_wide <- rbind(oneRM_abilities_rasch_100_10_wide, load_difficulties_rasch_100_10_wide) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_100_20_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_20_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_100_20_wide <- as.data.frame(coef(glmer_rasch_100_20_wide)$person)

person_abilities_rasch_100_20_wide <- person_abilities_rasch_100_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_100_20_wide <- as.data.frame(coef(glmer_rasch_100_20_wide)$item)

item_difficulties_rasch_100_20_wide <- item_difficulties_rasch_100_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_wide <- left_join(oneRM_dat_100_20_wide, person_abilities_rasch_100_20_wide, by="person") %>%
  left_join(item_difficulties_rasch_100_20_wide, by="item")

oneRM_abilities_rasch_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_100_20_wide <- rbind(oneRM_abilities_rasch_100_20_wide, load_difficulties_rasch_100_20_wide) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_250_5_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_250_5_wide, 
                                        family = binomial(link = "logit"))

person_abilities_rasch_250_5_wide <- as.data.frame(coef(glmer_rasch_250_5_wide)$person)

person_abilities_rasch_250_5_wide <- person_abilities_rasch_250_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_5_wide <- as.data.frame(coef(glmer_rasch_250_5_wide)$item)

item_difficulties_rasch_250_5_wide <- item_difficulties_rasch_250_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_wide <- left_join(oneRM_dat_250_5_wide, person_abilities_rasch_250_5_wide, by="person") %>%
  left_join(item_difficulties_rasch_250_5_wide, by="item")

oneRM_abilities_rasch_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_5_wide <- rbind(oneRM_abilities_rasch_250_5_wide, load_difficulties_rasch_250_5_wide) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_250_10_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_10_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_250_10_wide <- as.data.frame(coef(glmer_rasch_250_10_wide)$person)

person_abilities_rasch_250_10_wide <- person_abilities_rasch_250_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_10_wide <- as.data.frame(coef(glmer_rasch_250_10_wide)$item)

item_difficulties_rasch_250_10_wide <- item_difficulties_rasch_250_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_wide <- left_join(oneRM_dat_250_10_wide, person_abilities_rasch_250_10_wide, by="person") %>%
  left_join(item_difficulties_rasch_250_10_wide, by="item")

oneRM_abilities_rasch_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_10_wide <- rbind(oneRM_abilities_rasch_250_10_wide, load_difficulties_rasch_250_10_wide) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_250_20_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_20_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_250_20_wide <- as.data.frame(coef(glmer_rasch_250_20_wide)$person)

person_abilities_rasch_250_20_wide <- person_abilities_rasch_250_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_250_20_wide <- as.data.frame(coef(glmer_rasch_250_20_wide)$item)

item_difficulties_rasch_250_20_wide <- item_difficulties_rasch_250_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_wide <- left_join(oneRM_dat_250_20_wide, person_abilities_rasch_250_20_wide, by="person") %>%
  left_join(item_difficulties_rasch_250_20_wide, by="item")

oneRM_abilities_rasch_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_250_20_wide <- rbind(oneRM_abilities_rasch_250_20_wide, load_difficulties_rasch_250_20_wide) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_500_5_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_500_5_wide, 
                                        family = binomial(link = "logit"))

person_abilities_rasch_500_5_wide <- as.data.frame(coef(glmer_rasch_500_5_wide)$person)

person_abilities_rasch_500_5_wide <- person_abilities_rasch_500_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_5_wide <- as.data.frame(coef(glmer_rasch_500_5_wide)$item)

item_difficulties_rasch_500_5_wide <- item_difficulties_rasch_500_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_wide <- left_join(oneRM_dat_500_5_wide, person_abilities_rasch_500_5_wide, by="person") %>%
  left_join(item_difficulties_rasch_500_5_wide, by="item")

oneRM_abilities_rasch_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_5_wide <- rbind(oneRM_abilities_rasch_500_5_wide, load_difficulties_rasch_500_5_wide) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_500_10_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_10_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_500_10_wide <- as.data.frame(coef(glmer_rasch_500_10_wide)$person)

person_abilities_rasch_500_10_wide <- person_abilities_rasch_500_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_10_wide <- as.data.frame(coef(glmer_rasch_500_10_wide)$item)

item_difficulties_rasch_500_10_wide <- item_difficulties_rasch_500_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_wide <- left_join(oneRM_dat_500_10_wide, person_abilities_rasch_500_10_wide, by="person") %>%
  left_join(item_difficulties_rasch_500_10_wide, by="item")

oneRM_abilities_rasch_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_10_wide <- rbind(oneRM_abilities_rasch_500_10_wide, load_difficulties_rasch_500_10_wide) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_500_20_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_20_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_500_20_wide <- as.data.frame(coef(glmer_rasch_500_20_wide)$person)

person_abilities_rasch_500_20_wide <- person_abilities_rasch_500_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_500_20_wide <- as.data.frame(coef(glmer_rasch_500_20_wide)$item)

item_difficulties_rasch_500_20_wide <- item_difficulties_rasch_500_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_wide <- left_join(oneRM_dat_500_20_wide, person_abilities_rasch_500_20_wide, by="person") %>%
  left_join(item_difficulties_rasch_500_20_wide, by="item")

oneRM_abilities_rasch_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_500_20_wide <- rbind(oneRM_abilities_rasch_500_20_wide, load_difficulties_rasch_500_20_wide) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_1000_5_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_1000_5_wide, 
                                         family = binomial(link = "logit"))

person_abilities_rasch_1000_5_wide <- as.data.frame(coef(glmer_rasch_1000_5_wide)$person)

person_abilities_rasch_1000_5_wide <- person_abilities_rasch_1000_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_5_wide <- as.data.frame(coef(glmer_rasch_1000_5_wide)$item)

item_difficulties_rasch_1000_5_wide <- item_difficulties_rasch_1000_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_wide <- left_join(oneRM_dat_1000_5_wide, person_abilities_rasch_1000_5_wide, by="person") %>%
  left_join(item_difficulties_rasch_1000_5_wide, by="item")

oneRM_abilities_rasch_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_5_wide <- rbind(oneRM_abilities_rasch_1000_5_wide, load_difficulties_rasch_1000_5_wide) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_1000_10_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_10_wide, 
                                          family = binomial(link = "logit"))

person_abilities_rasch_1000_10_wide <- as.data.frame(coef(glmer_rasch_1000_10_wide)$person)

person_abilities_rasch_1000_10_wide <- person_abilities_rasch_1000_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_10_wide <- as.data.frame(coef(glmer_rasch_1000_10_wide)$item)

item_difficulties_rasch_1000_10_wide <- item_difficulties_rasch_1000_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_wide <- left_join(oneRM_dat_1000_10_wide, person_abilities_rasch_1000_10_wide, by="person") %>%
  left_join(item_difficulties_rasch_1000_10_wide, by="item")

oneRM_abilities_rasch_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_10_wide <- rbind(oneRM_abilities_rasch_1000_10_wide, load_difficulties_rasch_1000_10_wide) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

glmer_rasch_1000_20_wide <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_20_wide, 
                                          family = binomial(link = "logit"))

person_abilities_rasch_1000_20_wide <- as.data.frame(coef(glmer_rasch_1000_20_wide)$person)

person_abilities_rasch_1000_20_wide <- person_abilities_rasch_1000_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_1000_20_wide <- as.data.frame(coef(glmer_rasch_1000_20_wide)$item)

item_difficulties_rasch_1000_20_wide <- item_difficulties_rasch_1000_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_wide <- left_join(oneRM_dat_1000_20_wide, person_abilities_rasch_1000_20_wide, by="person") %>%
  left_join(item_difficulties_rasch_1000_20_wide, by="item")

oneRM_abilities_rasch_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_rasch_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_rasch_1000_20_wide <- rbind(oneRM_abilities_rasch_1000_20_wide, load_difficulties_rasch_1000_20_wide) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "Typical Error (?? = 1)",
         range = "wide") 

### Rasch high error datasets

#reload datasets

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

glmer_raschhigherror_50_5_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_50_5_narrow, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_50_5_narrow <- as.data.frame(coef(glmer_raschhigherror_50_5_narrow)$person)

person_abilities_raschhigherror_50_5_narrow <- person_abilities_raschhigherror_50_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_5_narrow <- as.data.frame(coef(glmer_raschhigherror_50_5_narrow)$item)

item_difficulties_raschhigherror_50_5_narrow <- item_difficulties_raschhigherror_50_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_narrow <- left_join(oneRM_dat_50_5_narrow, person_abilities_raschhigherror_50_5_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_5_narrow, by="item")

oneRM_abilities_raschhigherror_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_5_narrow <- rbind(oneRM_abilities_raschhigherror_50_5_narrow, load_difficulties_raschhigherror_50_5_narrow) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_50_10_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_10_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_50_10_narrow <- as.data.frame(coef(glmer_raschhigherror_50_10_narrow)$person)

person_abilities_raschhigherror_50_10_narrow <- person_abilities_raschhigherror_50_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_10_narrow <- as.data.frame(coef(glmer_raschhigherror_50_10_narrow)$item)

item_difficulties_raschhigherror_50_10_narrow <- item_difficulties_raschhigherror_50_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_narrow <- left_join(oneRM_dat_50_10_narrow, person_abilities_raschhigherror_50_10_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_10_narrow, by="item")

oneRM_abilities_raschhigherror_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_10_narrow <- rbind(oneRM_abilities_raschhigherror_50_10_narrow, load_difficulties_raschhigherror_50_10_narrow) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_50_20_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_20_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_50_20_narrow <- as.data.frame(coef(glmer_raschhigherror_50_20_narrow)$person)

person_abilities_raschhigherror_50_20_narrow <- person_abilities_raschhigherror_50_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_20_narrow <- as.data.frame(coef(glmer_raschhigherror_50_20_narrow)$item)

item_difficulties_raschhigherror_50_20_narrow <- item_difficulties_raschhigherror_50_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_narrow <- left_join(oneRM_dat_50_20_narrow, person_abilities_raschhigherror_50_20_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_20_narrow, by="item")

oneRM_abilities_raschhigherror_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_20_narrow <- rbind(oneRM_abilities_raschhigherror_50_20_narrow, load_difficulties_raschhigherror_50_20_narrow) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_100_5_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_100_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_100_5_narrow <- as.data.frame(coef(glmer_raschhigherror_100_5_narrow)$person)

person_abilities_raschhigherror_100_5_narrow <- person_abilities_raschhigherror_100_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_5_narrow <- as.data.frame(coef(glmer_raschhigherror_100_5_narrow)$item)

item_difficulties_raschhigherror_100_5_narrow <- item_difficulties_raschhigherror_100_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_narrow <- left_join(oneRM_dat_100_5_narrow, person_abilities_raschhigherror_100_5_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_5_narrow, by="item")

oneRM_abilities_raschhigherror_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_5_narrow <- rbind(oneRM_abilities_raschhigherror_100_5_narrow, load_difficulties_raschhigherror_100_5_narrow) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_100_10_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_100_10_narrow <- as.data.frame(coef(glmer_raschhigherror_100_10_narrow)$person)

person_abilities_raschhigherror_100_10_narrow <- person_abilities_raschhigherror_100_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_10_narrow <- as.data.frame(coef(glmer_raschhigherror_100_10_narrow)$item)

item_difficulties_raschhigherror_100_10_narrow <- item_difficulties_raschhigherror_100_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_narrow <- left_join(oneRM_dat_100_10_narrow, person_abilities_raschhigherror_100_10_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_10_narrow, by="item")

oneRM_abilities_raschhigherror_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_10_narrow <- rbind(oneRM_abilities_raschhigherror_100_10_narrow, load_difficulties_raschhigherror_100_10_narrow) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_100_20_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_100_20_narrow <- as.data.frame(coef(glmer_raschhigherror_100_20_narrow)$person)

person_abilities_raschhigherror_100_20_narrow <- person_abilities_raschhigherror_100_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_20_narrow <- as.data.frame(coef(glmer_raschhigherror_100_20_narrow)$item)

item_difficulties_raschhigherror_100_20_narrow <- item_difficulties_raschhigherror_100_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_narrow <- left_join(oneRM_dat_100_20_narrow, person_abilities_raschhigherror_100_20_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_20_narrow, by="item")

oneRM_abilities_raschhigherror_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_20_narrow <- rbind(oneRM_abilities_raschhigherror_100_20_narrow, load_difficulties_raschhigherror_100_20_narrow) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_250_5_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_250_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_250_5_narrow <- as.data.frame(coef(glmer_raschhigherror_250_5_narrow)$person)

person_abilities_raschhigherror_250_5_narrow <- person_abilities_raschhigherror_250_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_5_narrow <- as.data.frame(coef(glmer_raschhigherror_250_5_narrow)$item)

item_difficulties_raschhigherror_250_5_narrow <- item_difficulties_raschhigherror_250_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_narrow <- left_join(oneRM_dat_250_5_narrow, person_abilities_raschhigherror_250_5_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_5_narrow, by="item")

oneRM_abilities_raschhigherror_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_5_narrow <- rbind(oneRM_abilities_raschhigherror_250_5_narrow, load_difficulties_raschhigherror_250_5_narrow) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_250_10_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_250_10_narrow <- as.data.frame(coef(glmer_raschhigherror_250_10_narrow)$person)

person_abilities_raschhigherror_250_10_narrow <- person_abilities_raschhigherror_250_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_10_narrow <- as.data.frame(coef(glmer_raschhigherror_250_10_narrow)$item)

item_difficulties_raschhigherror_250_10_narrow <- item_difficulties_raschhigherror_250_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_narrow <- left_join(oneRM_dat_250_10_narrow, person_abilities_raschhigherror_250_10_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_10_narrow, by="item")

oneRM_abilities_raschhigherror_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_10_narrow <- rbind(oneRM_abilities_raschhigherror_250_10_narrow, load_difficulties_raschhigherror_250_10_narrow) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_250_20_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_250_20_narrow <- as.data.frame(coef(glmer_raschhigherror_250_20_narrow)$person)

person_abilities_raschhigherror_250_20_narrow <- person_abilities_raschhigherror_250_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_20_narrow <- as.data.frame(coef(glmer_raschhigherror_250_20_narrow)$item)

item_difficulties_raschhigherror_250_20_narrow <- item_difficulties_raschhigherror_250_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_narrow <- left_join(oneRM_dat_250_20_narrow, person_abilities_raschhigherror_250_20_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_20_narrow, by="item")

oneRM_abilities_raschhigherror_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_20_narrow <- rbind(oneRM_abilities_raschhigherror_250_20_narrow, load_difficulties_raschhigherror_250_20_narrow) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_500_5_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_500_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_500_5_narrow <- as.data.frame(coef(glmer_raschhigherror_500_5_narrow)$person)

person_abilities_raschhigherror_500_5_narrow <- person_abilities_raschhigherror_500_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_5_narrow <- as.data.frame(coef(glmer_raschhigherror_500_5_narrow)$item)

item_difficulties_raschhigherror_500_5_narrow <- item_difficulties_raschhigherror_500_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_narrow <- left_join(oneRM_dat_500_5_narrow, person_abilities_raschhigherror_500_5_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_5_narrow, by="item")

oneRM_abilities_raschhigherror_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_5_narrow <- rbind(oneRM_abilities_raschhigherror_500_5_narrow, load_difficulties_raschhigherror_500_5_narrow) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_500_10_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_500_10_narrow <- as.data.frame(coef(glmer_raschhigherror_500_10_narrow)$person)

person_abilities_raschhigherror_500_10_narrow <- person_abilities_raschhigherror_500_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_10_narrow <- as.data.frame(coef(glmer_raschhigherror_500_10_narrow)$item)

item_difficulties_raschhigherror_500_10_narrow <- item_difficulties_raschhigherror_500_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_narrow <- left_join(oneRM_dat_500_10_narrow, person_abilities_raschhigherror_500_10_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_10_narrow, by="item")

oneRM_abilities_raschhigherror_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_10_narrow <- rbind(oneRM_abilities_raschhigherror_500_10_narrow, load_difficulties_raschhigherror_500_10_narrow) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_500_20_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_500_20_narrow <- as.data.frame(coef(glmer_raschhigherror_500_20_narrow)$person)

person_abilities_raschhigherror_500_20_narrow <- person_abilities_raschhigherror_500_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_20_narrow <- as.data.frame(coef(glmer_raschhigherror_500_20_narrow)$item)

item_difficulties_raschhigherror_500_20_narrow <- item_difficulties_raschhigherror_500_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_narrow <- left_join(oneRM_dat_500_20_narrow, person_abilities_raschhigherror_500_20_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_20_narrow, by="item")

oneRM_abilities_raschhigherror_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_20_narrow <- rbind(oneRM_abilities_raschhigherror_500_20_narrow, load_difficulties_raschhigherror_500_20_narrow) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_1000_5_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_1000_5_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_5_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_5_narrow)$person)

person_abilities_raschhigherror_1000_5_narrow <- person_abilities_raschhigherror_1000_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_5_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_5_narrow)$item)

item_difficulties_raschhigherror_1000_5_narrow <- item_difficulties_raschhigherror_1000_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_narrow <- left_join(oneRM_dat_1000_5_narrow, person_abilities_raschhigherror_1000_5_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_5_narrow, by="item")

oneRM_abilities_raschhigherror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_5_narrow <- rbind(oneRM_abilities_raschhigherror_1000_5_narrow, load_difficulties_raschhigherror_1000_5_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_1000_10_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_10_narrow, 
                                          family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_10_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_10_narrow)$person)

person_abilities_raschhigherror_1000_10_narrow <- person_abilities_raschhigherror_1000_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_10_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_10_narrow)$item)

item_difficulties_raschhigherror_1000_10_narrow <- item_difficulties_raschhigherror_1000_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_narrow <- left_join(oneRM_dat_1000_10_narrow, person_abilities_raschhigherror_1000_10_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_10_narrow, by="item")

oneRM_abilities_raschhigherror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_10_narrow <- rbind(oneRM_abilities_raschhigherror_1000_10_narrow, load_difficulties_raschhigherror_1000_10_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_1000_20_narrow <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_20_narrow, 
                                          family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_20_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_20_narrow)$person)

person_abilities_raschhigherror_1000_20_narrow <- person_abilities_raschhigherror_1000_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_20_narrow <- as.data.frame(coef(glmer_raschhigherror_1000_20_narrow)$item)

item_difficulties_raschhigherror_1000_20_narrow <- item_difficulties_raschhigherror_1000_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_narrow <- left_join(oneRM_dat_1000_20_narrow, person_abilities_raschhigherror_1000_20_narrow, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_20_narrow, by="item")

oneRM_abilities_raschhigherror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_20_narrow <- rbind(oneRM_abilities_raschhigherror_1000_20_narrow, load_difficulties_raschhigherror_1000_20_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "narrow") 

glmer_raschhigherror_50_5_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                     data = oneRM_dat_50_5_wide, 
                                     family = binomial(link = "logit"))



person_abilities_raschhigherror_50_5_wide <- as.data.frame(coef(glmer_raschhigherror_50_5_wide)$person)

person_abilities_raschhigherror_50_5_wide <- person_abilities_raschhigherror_50_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_5_wide <- as.data.frame(coef(glmer_raschhigherror_50_5_wide)$item)

item_difficulties_raschhigherror_50_5_wide <- item_difficulties_raschhigherror_50_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_wide <- left_join(oneRM_dat_50_5_wide, person_abilities_raschhigherror_50_5_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_5_wide, by="item")

oneRM_abilities_raschhigherror_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_5_wide <- rbind(oneRM_abilities_raschhigherror_50_5_wide, load_difficulties_raschhigherror_50_5_wide) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_50_10_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_50_10_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschhigherror_50_10_wide <- as.data.frame(coef(glmer_raschhigherror_50_10_wide)$person)

person_abilities_raschhigherror_50_10_wide <- person_abilities_raschhigherror_50_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_10_wide <- as.data.frame(coef(glmer_raschhigherror_50_10_wide)$item)

item_difficulties_raschhigherror_50_10_wide <- item_difficulties_raschhigherror_50_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_wide <- left_join(oneRM_dat_50_10_wide, person_abilities_raschhigherror_50_10_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_10_wide, by="item")

oneRM_abilities_raschhigherror_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_10_wide <- rbind(oneRM_abilities_raschhigherror_50_10_wide, load_difficulties_raschhigherror_50_10_wide) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_50_20_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_50_20_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschhigherror_50_20_wide <- as.data.frame(coef(glmer_raschhigherror_50_20_wide)$person)

person_abilities_raschhigherror_50_20_wide <- person_abilities_raschhigherror_50_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_50_20_wide <- as.data.frame(coef(glmer_raschhigherror_50_20_wide)$item)

item_difficulties_raschhigherror_50_20_wide <- item_difficulties_raschhigherror_50_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_wide <- left_join(oneRM_dat_50_20_wide, person_abilities_raschhigherror_50_20_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_50_20_wide, by="item")

oneRM_abilities_raschhigherror_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_50_20_wide <- rbind(oneRM_abilities_raschhigherror_50_20_wide, load_difficulties_raschhigherror_50_20_wide) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_100_5_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_100_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschhigherror_100_5_wide <- as.data.frame(coef(glmer_raschhigherror_100_5_wide)$person)

person_abilities_raschhigherror_100_5_wide <- person_abilities_raschhigherror_100_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_5_wide <- as.data.frame(coef(glmer_raschhigherror_100_5_wide)$item)

item_difficulties_raschhigherror_100_5_wide <- item_difficulties_raschhigherror_100_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_wide <- left_join(oneRM_dat_100_5_wide, person_abilities_raschhigherror_100_5_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_5_wide, by="item")

oneRM_abilities_raschhigherror_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_5_wide <- rbind(oneRM_abilities_raschhigherror_100_5_wide, load_difficulties_raschhigherror_100_5_wide) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_100_10_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_100_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_100_10_wide <- as.data.frame(coef(glmer_raschhigherror_100_10_wide)$person)

person_abilities_raschhigherror_100_10_wide <- person_abilities_raschhigherror_100_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_10_wide <- as.data.frame(coef(glmer_raschhigherror_100_10_wide)$item)

item_difficulties_raschhigherror_100_10_wide <- item_difficulties_raschhigherror_100_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_wide <- left_join(oneRM_dat_100_10_wide, person_abilities_raschhigherror_100_10_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_10_wide, by="item")

oneRM_abilities_raschhigherror_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_10_wide <- rbind(oneRM_abilities_raschhigherror_100_10_wide, load_difficulties_raschhigherror_100_10_wide) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_100_20_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_100_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_100_20_wide <- as.data.frame(coef(glmer_raschhigherror_100_20_wide)$person)

person_abilities_raschhigherror_100_20_wide <- person_abilities_raschhigherror_100_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_100_20_wide <- as.data.frame(coef(glmer_raschhigherror_100_20_wide)$item)

item_difficulties_raschhigherror_100_20_wide <- item_difficulties_raschhigherror_100_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_wide <- left_join(oneRM_dat_100_20_wide, person_abilities_raschhigherror_100_20_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_100_20_wide, by="item")

oneRM_abilities_raschhigherror_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_100_20_wide <- rbind(oneRM_abilities_raschhigherror_100_20_wide, load_difficulties_raschhigherror_100_20_wide) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_250_5_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_250_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschhigherror_250_5_wide <- as.data.frame(coef(glmer_raschhigherror_250_5_wide)$person)

person_abilities_raschhigherror_250_5_wide <- person_abilities_raschhigherror_250_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_5_wide <- as.data.frame(coef(glmer_raschhigherror_250_5_wide)$item)

item_difficulties_raschhigherror_250_5_wide <- item_difficulties_raschhigherror_250_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_wide <- left_join(oneRM_dat_250_5_wide, person_abilities_raschhigherror_250_5_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_5_wide, by="item")

oneRM_abilities_raschhigherror_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_5_wide <- rbind(oneRM_abilities_raschhigherror_250_5_wide, load_difficulties_raschhigherror_250_5_wide) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_250_10_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_250_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_250_10_wide <- as.data.frame(coef(glmer_raschhigherror_250_10_wide)$person)

person_abilities_raschhigherror_250_10_wide <- person_abilities_raschhigherror_250_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_10_wide <- as.data.frame(coef(glmer_raschhigherror_250_10_wide)$item)

item_difficulties_raschhigherror_250_10_wide <- item_difficulties_raschhigherror_250_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_wide <- left_join(oneRM_dat_250_10_wide, person_abilities_raschhigherror_250_10_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_10_wide, by="item")

oneRM_abilities_raschhigherror_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_10_wide <- rbind(oneRM_abilities_raschhigherror_250_10_wide, load_difficulties_raschhigherror_250_10_wide) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_250_20_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_250_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_250_20_wide <- as.data.frame(coef(glmer_raschhigherror_250_20_wide)$person)

person_abilities_raschhigherror_250_20_wide <- person_abilities_raschhigherror_250_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_250_20_wide <- as.data.frame(coef(glmer_raschhigherror_250_20_wide)$item)

item_difficulties_raschhigherror_250_20_wide <- item_difficulties_raschhigherror_250_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_wide <- left_join(oneRM_dat_250_20_wide, person_abilities_raschhigherror_250_20_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_250_20_wide, by="item")

oneRM_abilities_raschhigherror_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_250_20_wide <- rbind(oneRM_abilities_raschhigherror_250_20_wide, load_difficulties_raschhigherror_250_20_wide) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_500_5_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_500_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschhigherror_500_5_wide <- as.data.frame(coef(glmer_raschhigherror_500_5_wide)$person)

person_abilities_raschhigherror_500_5_wide <- person_abilities_raschhigherror_500_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_5_wide <- as.data.frame(coef(glmer_raschhigherror_500_5_wide)$item)

item_difficulties_raschhigherror_500_5_wide <- item_difficulties_raschhigherror_500_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_wide <- left_join(oneRM_dat_500_5_wide, person_abilities_raschhigherror_500_5_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_5_wide, by="item")

oneRM_abilities_raschhigherror_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_5_wide <- rbind(oneRM_abilities_raschhigherror_500_5_wide, load_difficulties_raschhigherror_500_5_wide) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_500_10_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_500_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_500_10_wide <- as.data.frame(coef(glmer_raschhigherror_500_10_wide)$person)

person_abilities_raschhigherror_500_10_wide <- person_abilities_raschhigherror_500_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_10_wide <- as.data.frame(coef(glmer_raschhigherror_500_10_wide)$item)

item_difficulties_raschhigherror_500_10_wide <- item_difficulties_raschhigherror_500_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_wide <- left_join(oneRM_dat_500_10_wide, person_abilities_raschhigherror_500_10_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_10_wide, by="item")

oneRM_abilities_raschhigherror_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_10_wide <- rbind(oneRM_abilities_raschhigherror_500_10_wide, load_difficulties_raschhigherror_500_10_wide) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_500_20_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_500_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_500_20_wide <- as.data.frame(coef(glmer_raschhigherror_500_20_wide)$person)

person_abilities_raschhigherror_500_20_wide <- person_abilities_raschhigherror_500_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_500_20_wide <- as.data.frame(coef(glmer_raschhigherror_500_20_wide)$item)

item_difficulties_raschhigherror_500_20_wide <- item_difficulties_raschhigherror_500_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_wide <- left_join(oneRM_dat_500_20_wide, person_abilities_raschhigherror_500_20_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_500_20_wide, by="item")

oneRM_abilities_raschhigherror_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_500_20_wide <- rbind(oneRM_abilities_raschhigherror_500_20_wide, load_difficulties_raschhigherror_500_20_wide) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_1000_5_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_1000_5_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_5_wide <- as.data.frame(coef(glmer_raschhigherror_1000_5_wide)$person)

person_abilities_raschhigherror_1000_5_wide <- person_abilities_raschhigherror_1000_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_5_wide <- as.data.frame(coef(glmer_raschhigherror_1000_5_wide)$item)

item_difficulties_raschhigherror_1000_5_wide <- item_difficulties_raschhigherror_1000_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_wide <- left_join(oneRM_dat_1000_5_wide, person_abilities_raschhigherror_1000_5_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_5_wide, by="item")

oneRM_abilities_raschhigherror_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_5_wide <- rbind(oneRM_abilities_raschhigherror_1000_5_wide, load_difficulties_raschhigherror_1000_5_wide) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_1000_10_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_1000_10_wide, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_10_wide <- as.data.frame(coef(glmer_raschhigherror_1000_10_wide)$person)

person_abilities_raschhigherror_1000_10_wide <- person_abilities_raschhigherror_1000_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_10_wide <- as.data.frame(coef(glmer_raschhigherror_1000_10_wide)$item)

item_difficulties_raschhigherror_1000_10_wide <- item_difficulties_raschhigherror_1000_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_wide <- left_join(oneRM_dat_1000_10_wide, person_abilities_raschhigherror_1000_10_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_10_wide, by="item")

oneRM_abilities_raschhigherror_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_10_wide <- rbind(oneRM_abilities_raschhigherror_1000_10_wide, load_difficulties_raschhigherror_1000_10_wide) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

glmer_raschhigherror_1000_20_wide <- lme4::glmer(response_raschhigherror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_1000_20_wide, 
                                        family = binomial(link = "logit"))

person_abilities_raschhigherror_1000_20_wide <- as.data.frame(coef(glmer_raschhigherror_1000_20_wide)$person)

person_abilities_raschhigherror_1000_20_wide <- person_abilities_raschhigherror_1000_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschhigherror_1000_20_wide <- as.data.frame(coef(glmer_raschhigherror_1000_20_wide)$item)

item_difficulties_raschhigherror_1000_20_wide <- item_difficulties_raschhigherror_1000_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_wide <- left_join(oneRM_dat_1000_20_wide, person_abilities_raschhigherror_1000_20_wide, by="person") %>%
  left_join(item_difficulties_raschhigherror_1000_20_wide, by="item")

oneRM_abilities_raschhigherror_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschhigherror_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschhigherror_1000_20_wide <- rbind(oneRM_abilities_raschhigherror_1000_20_wide, load_difficulties_raschhigherror_1000_20_wide) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "High Error (?? = 0.1)",
         range = "wide") 

### Rasch low error datasets

# reload datasets

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

glmer_raschlowerror_50_5_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_50_5_narrow, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_50_5_narrow <- as.data.frame(coef(glmer_raschlowerror_50_5_narrow)$person)

person_abilities_raschlowerror_50_5_narrow <- person_abilities_raschlowerror_50_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_5_narrow <- as.data.frame(coef(glmer_raschlowerror_50_5_narrow)$item)

item_difficulties_raschlowerror_50_5_narrow <- item_difficulties_raschlowerror_50_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_narrow$one_RM))+mean(log(oneRM_dat_50_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_narrow <- left_join(oneRM_dat_50_5_narrow, person_abilities_raschlowerror_50_5_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_5_narrow, by="item")

oneRM_abilities_raschlowerror_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_5_narrow <- oneRM_dat_50_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_5_narrow <- rbind(oneRM_abilities_raschlowerror_50_5_narrow, load_difficulties_raschlowerror_50_5_narrow) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_50_10_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_10_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_50_10_narrow <- as.data.frame(coef(glmer_raschlowerror_50_10_narrow)$person)

person_abilities_raschlowerror_50_10_narrow <- person_abilities_raschlowerror_50_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_10_narrow <- as.data.frame(coef(glmer_raschlowerror_50_10_narrow)$item)

item_difficulties_raschlowerror_50_10_narrow <- item_difficulties_raschlowerror_50_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_narrow$one_RM))+mean(log(oneRM_dat_50_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_narrow <- left_join(oneRM_dat_50_10_narrow, person_abilities_raschlowerror_50_10_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_10_narrow, by="item")

oneRM_abilities_raschlowerror_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_10_narrow <- oneRM_dat_50_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_10_narrow <- rbind(oneRM_abilities_raschlowerror_50_10_narrow, load_difficulties_raschlowerror_50_10_narrow) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_50_20_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_50_20_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_50_20_narrow <- as.data.frame(coef(glmer_raschlowerror_50_20_narrow)$person)

person_abilities_raschlowerror_50_20_narrow <- person_abilities_raschlowerror_50_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_20_narrow <- as.data.frame(coef(glmer_raschlowerror_50_20_narrow)$item)

item_difficulties_raschlowerror_50_20_narrow <- item_difficulties_raschlowerror_50_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_narrow$one_RM))+mean(log(oneRM_dat_50_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_narrow <- left_join(oneRM_dat_50_20_narrow, person_abilities_raschlowerror_50_20_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_20_narrow, by="item")

oneRM_abilities_raschlowerror_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_20_narrow <- oneRM_dat_50_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_20_narrow <- rbind(oneRM_abilities_raschlowerror_50_20_narrow, load_difficulties_raschlowerror_50_20_narrow) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_100_5_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_100_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_100_5_narrow <- as.data.frame(coef(glmer_raschlowerror_100_5_narrow)$person)

person_abilities_raschlowerror_100_5_narrow <- person_abilities_raschlowerror_100_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_5_narrow <- as.data.frame(coef(glmer_raschlowerror_100_5_narrow)$item)

item_difficulties_raschlowerror_100_5_narrow <- item_difficulties_raschlowerror_100_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_narrow$one_RM))+mean(log(oneRM_dat_100_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_narrow <- left_join(oneRM_dat_100_5_narrow, person_abilities_raschlowerror_100_5_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_5_narrow, by="item")

oneRM_abilities_raschlowerror_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_5_narrow <- oneRM_dat_100_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_5_narrow <- rbind(oneRM_abilities_raschlowerror_100_5_narrow, load_difficulties_raschlowerror_100_5_narrow) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_100_10_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_100_10_narrow <- as.data.frame(coef(glmer_raschlowerror_100_10_narrow)$person)

person_abilities_raschlowerror_100_10_narrow <- person_abilities_raschlowerror_100_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_10_narrow <- as.data.frame(coef(glmer_raschlowerror_100_10_narrow)$item)

item_difficulties_raschlowerror_100_10_narrow <- item_difficulties_raschlowerror_100_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_narrow$one_RM))+mean(log(oneRM_dat_100_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_narrow <- left_join(oneRM_dat_100_10_narrow, person_abilities_raschlowerror_100_10_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_10_narrow, by="item")

oneRM_abilities_raschlowerror_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_10_narrow <- oneRM_dat_100_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_10_narrow <- rbind(oneRM_abilities_raschlowerror_100_10_narrow, load_difficulties_raschlowerror_100_10_narrow) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_100_20_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_100_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_100_20_narrow <- as.data.frame(coef(glmer_raschlowerror_100_20_narrow)$person)

person_abilities_raschlowerror_100_20_narrow <- person_abilities_raschlowerror_100_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_20_narrow <- as.data.frame(coef(glmer_raschlowerror_100_20_narrow)$item)

item_difficulties_raschlowerror_100_20_narrow <- item_difficulties_raschlowerror_100_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_narrow$one_RM))+mean(log(oneRM_dat_100_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_narrow <- left_join(oneRM_dat_100_20_narrow, person_abilities_raschlowerror_100_20_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_20_narrow, by="item")

oneRM_abilities_raschlowerror_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_20_narrow <- oneRM_dat_100_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_20_narrow <- rbind(oneRM_abilities_raschlowerror_100_20_narrow, load_difficulties_raschlowerror_100_20_narrow) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_250_5_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_250_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_250_5_narrow <- as.data.frame(coef(glmer_raschlowerror_250_5_narrow)$person)

person_abilities_raschlowerror_250_5_narrow <- person_abilities_raschlowerror_250_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_5_narrow <- as.data.frame(coef(glmer_raschlowerror_250_5_narrow)$item)

item_difficulties_raschlowerror_250_5_narrow <- item_difficulties_raschlowerror_250_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_narrow$one_RM))+mean(log(oneRM_dat_250_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_narrow <- left_join(oneRM_dat_250_5_narrow, person_abilities_raschlowerror_250_5_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_5_narrow, by="item")

oneRM_abilities_raschlowerror_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_5_narrow <- oneRM_dat_250_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_5_narrow <- rbind(oneRM_abilities_raschlowerror_250_5_narrow, load_difficulties_raschlowerror_250_5_narrow) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_250_10_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_250_10_narrow <- as.data.frame(coef(glmer_raschlowerror_250_10_narrow)$person)

person_abilities_raschlowerror_250_10_narrow <- person_abilities_raschlowerror_250_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_10_narrow <- as.data.frame(coef(glmer_raschlowerror_250_10_narrow)$item)

item_difficulties_raschlowerror_250_10_narrow <- item_difficulties_raschlowerror_250_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_narrow$one_RM))+mean(log(oneRM_dat_250_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_narrow <- left_join(oneRM_dat_250_10_narrow, person_abilities_raschlowerror_250_10_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_10_narrow, by="item")

oneRM_abilities_raschlowerror_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_10_narrow <- oneRM_dat_250_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_10_narrow <- rbind(oneRM_abilities_raschlowerror_250_10_narrow, load_difficulties_raschlowerror_250_10_narrow) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_250_20_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_250_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_250_20_narrow <- as.data.frame(coef(glmer_raschlowerror_250_20_narrow)$person)

person_abilities_raschlowerror_250_20_narrow <- person_abilities_raschlowerror_250_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_20_narrow <- as.data.frame(coef(glmer_raschlowerror_250_20_narrow)$item)

item_difficulties_raschlowerror_250_20_narrow <- item_difficulties_raschlowerror_250_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_narrow$one_RM))+mean(log(oneRM_dat_250_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_narrow <- left_join(oneRM_dat_250_20_narrow, person_abilities_raschlowerror_250_20_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_20_narrow, by="item")

oneRM_abilities_raschlowerror_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_20_narrow <- oneRM_dat_250_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_20_narrow <- rbind(oneRM_abilities_raschlowerror_250_20_narrow, load_difficulties_raschlowerror_250_20_narrow) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_500_5_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_500_5_narrow, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_500_5_narrow <- as.data.frame(coef(glmer_raschlowerror_500_5_narrow)$person)

person_abilities_raschlowerror_500_5_narrow <- person_abilities_raschlowerror_500_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_5_narrow <- as.data.frame(coef(glmer_raschlowerror_500_5_narrow)$item)

item_difficulties_raschlowerror_500_5_narrow <- item_difficulties_raschlowerror_500_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_narrow$one_RM))+mean(log(oneRM_dat_500_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_narrow <- left_join(oneRM_dat_500_5_narrow, person_abilities_raschlowerror_500_5_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_5_narrow, by="item")

oneRM_abilities_raschlowerror_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_5_narrow <- oneRM_dat_500_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_5_narrow <- rbind(oneRM_abilities_raschlowerror_500_5_narrow, load_difficulties_raschlowerror_500_5_narrow) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_500_10_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_10_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_500_10_narrow <- as.data.frame(coef(glmer_raschlowerror_500_10_narrow)$person)

person_abilities_raschlowerror_500_10_narrow <- person_abilities_raschlowerror_500_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_10_narrow <- as.data.frame(coef(glmer_raschlowerror_500_10_narrow)$item)

item_difficulties_raschlowerror_500_10_narrow <- item_difficulties_raschlowerror_500_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_narrow$one_RM))+mean(log(oneRM_dat_500_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_narrow <- left_join(oneRM_dat_500_10_narrow, person_abilities_raschlowerror_500_10_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_10_narrow, by="item")

oneRM_abilities_raschlowerror_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_10_narrow <- oneRM_dat_500_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_10_narrow <- rbind(oneRM_abilities_raschlowerror_500_10_narrow, load_difficulties_raschlowerror_500_10_narrow) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_500_20_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_500_20_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_500_20_narrow <- as.data.frame(coef(glmer_raschlowerror_500_20_narrow)$person)

person_abilities_raschlowerror_500_20_narrow <- person_abilities_raschlowerror_500_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_20_narrow <- as.data.frame(coef(glmer_raschlowerror_500_20_narrow)$item)

item_difficulties_raschlowerror_500_20_narrow <- item_difficulties_raschlowerror_500_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_narrow$one_RM))+mean(log(oneRM_dat_500_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_narrow <- left_join(oneRM_dat_500_20_narrow, person_abilities_raschlowerror_500_20_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_20_narrow, by="item")

oneRM_abilities_raschlowerror_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_20_narrow <- oneRM_dat_500_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_20_narrow <- rbind(oneRM_abilities_raschlowerror_500_20_narrow, load_difficulties_raschlowerror_500_20_narrow) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_1000_5_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                         data = oneRM_dat_1000_5_narrow, 
                                         family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_5_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_5_narrow)$person)

person_abilities_raschlowerror_1000_5_narrow <- person_abilities_raschlowerror_1000_5_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_5_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_5_narrow)$item)

item_difficulties_raschlowerror_1000_5_narrow <- item_difficulties_raschlowerror_1000_5_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_narrow$one_RM))+mean(log(oneRM_dat_1000_5_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_narrow <- left_join(oneRM_dat_1000_5_narrow, person_abilities_raschlowerror_1000_5_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_5_narrow, by="item")

oneRM_abilities_raschlowerror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_5_narrow <- oneRM_dat_1000_5_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_5_narrow <- rbind(oneRM_abilities_raschlowerror_1000_5_narrow, load_difficulties_raschlowerror_1000_5_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_1000_10_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_10_narrow, 
                                          family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_10_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_10_narrow)$person)

person_abilities_raschlowerror_1000_10_narrow <- person_abilities_raschlowerror_1000_10_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_10_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_10_narrow)$item)

item_difficulties_raschlowerror_1000_10_narrow <- item_difficulties_raschlowerror_1000_10_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_narrow$one_RM))+mean(log(oneRM_dat_1000_10_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_narrow <- left_join(oneRM_dat_1000_10_narrow, person_abilities_raschlowerror_1000_10_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_10_narrow, by="item")

oneRM_abilities_raschlowerror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_10_narrow <- rbind(oneRM_abilities_raschlowerror_1000_10_narrow, load_difficulties_raschlowerror_1000_10_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_1000_20_narrow <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                          data = oneRM_dat_1000_20_narrow, 
                                          family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_20_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_20_narrow)$person)

person_abilities_raschlowerror_1000_20_narrow <- person_abilities_raschlowerror_1000_20_narrow %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_20_narrow <- as.data.frame(coef(glmer_raschlowerror_1000_20_narrow)$item)

item_difficulties_raschlowerror_1000_20_narrow <- item_difficulties_raschlowerror_1000_20_narrow %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_narrow$one_RM))+mean(log(oneRM_dat_1000_20_narrow$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_narrow$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_narrow <- left_join(oneRM_dat_1000_20_narrow, person_abilities_raschlowerror_1000_20_narrow, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_20_narrow, by="item")

oneRM_abilities_raschlowerror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_20_narrow <- oneRM_dat_1000_20_narrow %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_20_narrow <- rbind(oneRM_abilities_raschlowerror_1000_20_narrow, load_difficulties_raschlowerror_1000_20_narrow) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "narrow") 

glmer_raschlowerror_50_5_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                     data = oneRM_dat_50_5_wide, 
                                     family = binomial(link = "logit"))



person_abilities_raschlowerror_50_5_wide <- as.data.frame(coef(glmer_raschlowerror_50_5_wide)$person)

person_abilities_raschlowerror_50_5_wide <- person_abilities_raschlowerror_50_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_5_wide <- as.data.frame(coef(glmer_raschlowerror_50_5_wide)$item)

item_difficulties_raschlowerror_50_5_wide <- item_difficulties_raschlowerror_50_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_5_wide$one_RM))+mean(log(oneRM_dat_50_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_5_wide <- left_join(oneRM_dat_50_5_wide, person_abilities_raschlowerror_50_5_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_5_wide, by="item")

oneRM_abilities_raschlowerror_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_5_wide <- oneRM_dat_50_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_5_wide <- rbind(oneRM_abilities_raschlowerror_50_5_wide, load_difficulties_raschlowerror_50_5_wide) %>%
  mutate(sample_size = "50",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_50_10_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_50_10_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschlowerror_50_10_wide <- as.data.frame(coef(glmer_raschlowerror_50_10_wide)$person)

person_abilities_raschlowerror_50_10_wide <- person_abilities_raschlowerror_50_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_10_wide <- as.data.frame(coef(glmer_raschlowerror_50_10_wide)$item)

item_difficulties_raschlowerror_50_10_wide <- item_difficulties_raschlowerror_50_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_10_wide$one_RM))+mean(log(oneRM_dat_50_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_10_wide <- left_join(oneRM_dat_50_10_wide, person_abilities_raschlowerror_50_10_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_10_wide, by="item")

oneRM_abilities_raschlowerror_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_10_wide <- oneRM_dat_50_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_10_wide <- rbind(oneRM_abilities_raschlowerror_50_10_wide, load_difficulties_raschlowerror_50_10_wide) %>%
  mutate(sample_size = "50",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_50_20_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_50_20_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschlowerror_50_20_wide <- as.data.frame(coef(glmer_raschlowerror_50_20_wide)$person)

person_abilities_raschlowerror_50_20_wide <- person_abilities_raschlowerror_50_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_50_20_wide <- as.data.frame(coef(glmer_raschlowerror_50_20_wide)$item)

item_difficulties_raschlowerror_50_20_wide <- item_difficulties_raschlowerror_50_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_50_20_wide$one_RM))+mean(log(oneRM_dat_50_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_50_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_50_20_wide <- left_join(oneRM_dat_50_20_wide, person_abilities_raschlowerror_50_20_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_50_20_wide, by="item")

oneRM_abilities_raschlowerror_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_50_20_wide <- oneRM_dat_50_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_50_20_wide <- rbind(oneRM_abilities_raschlowerror_50_20_wide, load_difficulties_raschlowerror_50_20_wide) %>%
  mutate(sample_size = "50",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_100_5_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_100_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschlowerror_100_5_wide <- as.data.frame(coef(glmer_raschlowerror_100_5_wide)$person)

person_abilities_raschlowerror_100_5_wide <- person_abilities_raschlowerror_100_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_5_wide <- as.data.frame(coef(glmer_raschlowerror_100_5_wide)$item)

item_difficulties_raschlowerror_100_5_wide <- item_difficulties_raschlowerror_100_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_5_wide$one_RM))+mean(log(oneRM_dat_100_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_5_wide <- left_join(oneRM_dat_100_5_wide, person_abilities_raschlowerror_100_5_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_5_wide, by="item")

oneRM_abilities_raschlowerror_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_5_wide <- oneRM_dat_100_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_5_wide <- rbind(oneRM_abilities_raschlowerror_100_5_wide, load_difficulties_raschlowerror_100_5_wide) %>%
  mutate(sample_size = "100",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_100_10_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_100_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_100_10_wide <- as.data.frame(coef(glmer_raschlowerror_100_10_wide)$person)

person_abilities_raschlowerror_100_10_wide <- person_abilities_raschlowerror_100_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_10_wide <- as.data.frame(coef(glmer_raschlowerror_100_10_wide)$item)

item_difficulties_raschlowerror_100_10_wide <- item_difficulties_raschlowerror_100_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_10_wide$one_RM))+mean(log(oneRM_dat_100_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_10_wide <- left_join(oneRM_dat_100_10_wide, person_abilities_raschlowerror_100_10_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_10_wide, by="item")

oneRM_abilities_raschlowerror_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_10_wide <- oneRM_dat_100_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_10_wide <- rbind(oneRM_abilities_raschlowerror_100_10_wide, load_difficulties_raschlowerror_100_10_wide) %>%
  mutate(sample_size = "100",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_100_20_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_100_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_100_20_wide <- as.data.frame(coef(glmer_raschlowerror_100_20_wide)$person)

person_abilities_raschlowerror_100_20_wide <- person_abilities_raschlowerror_100_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_100_20_wide <- as.data.frame(coef(glmer_raschlowerror_100_20_wide)$item)

item_difficulties_raschlowerror_100_20_wide <- item_difficulties_raschlowerror_100_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_100_20_wide$one_RM))+mean(log(oneRM_dat_100_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_100_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_100_20_wide <- left_join(oneRM_dat_100_20_wide, person_abilities_raschlowerror_100_20_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_100_20_wide, by="item")

oneRM_abilities_raschlowerror_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_100_20_wide <- oneRM_dat_100_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_100_20_wide <- rbind(oneRM_abilities_raschlowerror_100_20_wide, load_difficulties_raschlowerror_100_20_wide) %>%
  mutate(sample_size = "100",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_250_5_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_250_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschlowerror_250_5_wide <- as.data.frame(coef(glmer_raschlowerror_250_5_wide)$person)

person_abilities_raschlowerror_250_5_wide <- person_abilities_raschlowerror_250_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_5_wide <- as.data.frame(coef(glmer_raschlowerror_250_5_wide)$item)

item_difficulties_raschlowerror_250_5_wide <- item_difficulties_raschlowerror_250_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_5_wide$one_RM))+mean(log(oneRM_dat_250_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_5_wide <- left_join(oneRM_dat_250_5_wide, person_abilities_raschlowerror_250_5_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_5_wide, by="item")

oneRM_abilities_raschlowerror_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_5_wide <- oneRM_dat_250_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_5_wide <- rbind(oneRM_abilities_raschlowerror_250_5_wide, load_difficulties_raschlowerror_250_5_wide) %>%
  mutate(sample_size = "250",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_250_10_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_250_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_250_10_wide <- as.data.frame(coef(glmer_raschlowerror_250_10_wide)$person)

person_abilities_raschlowerror_250_10_wide <- person_abilities_raschlowerror_250_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_10_wide <- as.data.frame(coef(glmer_raschlowerror_250_10_wide)$item)

item_difficulties_raschlowerror_250_10_wide <- item_difficulties_raschlowerror_250_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_10_wide$one_RM))+mean(log(oneRM_dat_250_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_10_wide <- left_join(oneRM_dat_250_10_wide, person_abilities_raschlowerror_250_10_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_10_wide, by="item")

oneRM_abilities_raschlowerror_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_10_wide <- oneRM_dat_250_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_10_wide <- rbind(oneRM_abilities_raschlowerror_250_10_wide, load_difficulties_raschlowerror_250_10_wide) %>%
  mutate(sample_size = "250",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_250_20_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_250_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_250_20_wide <- as.data.frame(coef(glmer_raschlowerror_250_20_wide)$person)

person_abilities_raschlowerror_250_20_wide <- person_abilities_raschlowerror_250_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_250_20_wide <- as.data.frame(coef(glmer_raschlowerror_250_20_wide)$item)

item_difficulties_raschlowerror_250_20_wide <- item_difficulties_raschlowerror_250_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_250_20_wide$one_RM))+mean(log(oneRM_dat_250_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_250_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_250_20_wide <- left_join(oneRM_dat_250_20_wide, person_abilities_raschlowerror_250_20_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_250_20_wide, by="item")

oneRM_abilities_raschlowerror_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_250_20_wide <- oneRM_dat_250_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_250_20_wide <- rbind(oneRM_abilities_raschlowerror_250_20_wide, load_difficulties_raschlowerror_250_20_wide) %>%
  mutate(sample_size = "250",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_500_5_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                      data = oneRM_dat_500_5_wide, 
                                      family = binomial(link = "logit"))

person_abilities_raschlowerror_500_5_wide <- as.data.frame(coef(glmer_raschlowerror_500_5_wide)$person)

person_abilities_raschlowerror_500_5_wide <- person_abilities_raschlowerror_500_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_5_wide <- as.data.frame(coef(glmer_raschlowerror_500_5_wide)$item)

item_difficulties_raschlowerror_500_5_wide <- item_difficulties_raschlowerror_500_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_5_wide$one_RM))+mean(log(oneRM_dat_500_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_5_wide <- left_join(oneRM_dat_500_5_wide, person_abilities_raschlowerror_500_5_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_5_wide, by="item")

oneRM_abilities_raschlowerror_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_5_wide <- oneRM_dat_500_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_5_wide <- rbind(oneRM_abilities_raschlowerror_500_5_wide, load_difficulties_raschlowerror_500_5_wide) %>%
  mutate(sample_size = "500",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_500_10_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_500_10_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_500_10_wide <- as.data.frame(coef(glmer_raschlowerror_500_10_wide)$person)

person_abilities_raschlowerror_500_10_wide <- person_abilities_raschlowerror_500_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_10_wide <- as.data.frame(coef(glmer_raschlowerror_500_10_wide)$item)

item_difficulties_raschlowerror_500_10_wide <- item_difficulties_raschlowerror_500_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_10_wide$one_RM))+mean(log(oneRM_dat_500_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_10_wide <- left_join(oneRM_dat_500_10_wide, person_abilities_raschlowerror_500_10_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_10_wide, by="item")

oneRM_abilities_raschlowerror_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_10_wide <- oneRM_dat_500_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_10_wide <- rbind(oneRM_abilities_raschlowerror_500_10_wide, load_difficulties_raschlowerror_500_10_wide) %>%
  mutate(sample_size = "500",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_500_20_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_500_20_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_500_20_wide <- as.data.frame(coef(glmer_raschlowerror_500_20_wide)$person)

person_abilities_raschlowerror_500_20_wide <- person_abilities_raschlowerror_500_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_500_20_wide <- as.data.frame(coef(glmer_raschlowerror_500_20_wide)$item)

item_difficulties_raschlowerror_500_20_wide <- item_difficulties_raschlowerror_500_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_500_20_wide$one_RM))+mean(log(oneRM_dat_500_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_500_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_500_20_wide <- left_join(oneRM_dat_500_20_wide, person_abilities_raschlowerror_500_20_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_500_20_wide, by="item")

oneRM_abilities_raschlowerror_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_500_20_wide <- oneRM_dat_500_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_500_20_wide <- rbind(oneRM_abilities_raschlowerror_500_20_wide, load_difficulties_raschlowerror_500_20_wide) %>%
  mutate(sample_size = "500",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_1000_5_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                       data = oneRM_dat_1000_5_wide, 
                                       family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_5_wide <- as.data.frame(coef(glmer_raschlowerror_1000_5_wide)$person)

person_abilities_raschlowerror_1000_5_wide <- person_abilities_raschlowerror_1000_5_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_5_wide <- as.data.frame(coef(glmer_raschlowerror_1000_5_wide)$item)

item_difficulties_raschlowerror_1000_5_wide <- item_difficulties_raschlowerror_1000_5_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_5_wide$one_RM))+mean(log(oneRM_dat_1000_5_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_5_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_5_wide <- left_join(oneRM_dat_1000_5_wide, person_abilities_raschlowerror_1000_5_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_5_wide, by="item")

oneRM_abilities_raschlowerror_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_5_wide <- oneRM_dat_1000_5_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_5_wide <- rbind(oneRM_abilities_raschlowerror_1000_5_wide, load_difficulties_raschlowerror_1000_5_wide) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_1000_10_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_1000_10_wide, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_10_wide <- as.data.frame(coef(glmer_raschlowerror_1000_10_wide)$person)

person_abilities_raschlowerror_1000_10_wide <- person_abilities_raschlowerror_1000_10_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_10_wide <- as.data.frame(coef(glmer_raschlowerror_1000_10_wide)$item)

item_difficulties_raschlowerror_1000_10_wide <- item_difficulties_raschlowerror_1000_10_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_10_wide$one_RM))+mean(log(oneRM_dat_1000_10_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_10_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_10_wide <- left_join(oneRM_dat_1000_10_wide, person_abilities_raschlowerror_1000_10_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_10_wide, by="item")

oneRM_abilities_raschlowerror_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_10_wide <- oneRM_dat_1000_10_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_10_wide <- rbind(oneRM_abilities_raschlowerror_1000_10_wide, load_difficulties_raschlowerror_1000_10_wide) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

glmer_raschlowerror_1000_20_wide <- lme4::glmer(response_raschlowerror ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_1000_20_wide, 
                                        family = binomial(link = "logit"))

person_abilities_raschlowerror_1000_20_wide <- as.data.frame(coef(glmer_raschlowerror_1000_20_wide)$person)

person_abilities_raschlowerror_1000_20_wide <- person_abilities_raschlowerror_1000_20_wide %>% 
  mutate(person_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         person_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("person")

item_difficulties_raschlowerror_1000_20_wide <- as.data.frame(coef(glmer_raschlowerror_1000_20_wide)$item)

item_difficulties_raschlowerror_1000_20_wide <- item_difficulties_raschlowerror_1000_20_wide %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate_oneRM = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(oneRM_dat_1000_20_wide$one_RM))+mean(log(oneRM_dat_1000_20_wide$one_RM))),2),
         item_true_scale_estimate_load = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))+mean(log(as.numeric(as.character(oneRM_dat_1000_20_wide$item))))),2)) %>%
  rownames_to_column("item")

oneRM_dat_1000_20_wide <- left_join(oneRM_dat_1000_20_wide, person_abilities_raschlowerror_1000_20_wide, by="person") %>%
  left_join(item_difficulties_raschlowerror_1000_20_wide, by="item")

oneRM_abilities_raschlowerror_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(person) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(one_RM)),
         true_load_estimate_oneRM = person_true_scale_estimate_oneRM,
         true_load_estimate_item = person_true_scale_estimate_load,
         parameter = "person") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

load_difficulties_raschlowerror_1000_20_wide <- oneRM_dat_1000_20_wide %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_load = as.numeric(as.character(item)),
         true_load_estimate_oneRM = item_true_scale_estimate_oneRM,
         true_load_estimate_item = item_true_scale_estimate_load,         
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_load, true_load_estimate_oneRM, true_load_estimate_item, parameter)

true2estimate_scales_raschlowerror_1000_20_wide <- rbind(oneRM_abilities_raschlowerror_1000_20_wide, load_difficulties_raschlowerror_1000_20_wide) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         response_type = "Low Error (?? = 5)",
         range = "wide") 

### Save all the models
save(glmer_rasch_50_5_narrow, file = "models/glmer_rasch_50_5_narrow")
save(glmer_rasch_50_10_narrow, file = "models/glmer_rasch_50_10_narrow")
save(glmer_rasch_50_20_narrow, file = "models/glmer_rasch_50_20_narrow")
save(glmer_rasch_100_5_narrow, file = "models/glmer_rasch_100_5_narrow")
save(glmer_rasch_100_10_narrow, file = "models/glmer_rasch_100_10_narrow")
save(glmer_rasch_100_20_narrow, file = "models/glmer_rasch_100_20_narrow")
save(glmer_rasch_250_5_narrow, file = "models/glmer_rasch_250_5_narrow")
save(glmer_rasch_250_10_narrow, file = "models/glmer_rasch_250_10_narrow")
save(glmer_rasch_250_20_narrow, file = "models/glmer_rasch_250_20_narrow")
save(glmer_rasch_500_5_narrow, file = "models/glmer_rasch_500_5_narrow")
save(glmer_rasch_500_10_narrow, file = "models/glmer_rasch_500_10_narrow")
save(glmer_rasch_500_20_narrow, file = "models/glmer_rasch_500_20_narrow")
save(glmer_rasch_1000_5_narrow, file = "models/glmer_rasch_1000_5_narrow")
save(glmer_rasch_1000_10_narrow, file = "models/glmer_rasch_1000_10_narrow")
save(glmer_rasch_1000_20_narrow, file = "models/glmer_rasch_1000_20_narrow")
save(glmer_rasch_50_5_wide, file = "models/glmer_rasch_50_5_wide")
save(glmer_rasch_50_10_wide, file = "models/glmer_rasch_50_10_wide")
save(glmer_rasch_50_20_wide, file = "models/glmer_rasch_50_20_wide")
save(glmer_rasch_100_5_wide, file = "models/glmer_rasch_100_5_wide")
save(glmer_rasch_100_10_wide, file = "models/glmer_rasch_100_10_wide")
save(glmer_rasch_100_20_wide, file = "models/glmer_rasch_100_20_wide")
save(glmer_rasch_250_5_wide, file = "models/glmer_rasch_250_5_wide")
save(glmer_rasch_250_10_wide, file = "models/glmer_rasch_250_10_wide")
save(glmer_rasch_250_20_wide, file = "models/glmer_rasch_250_20_wide")
save(glmer_rasch_500_5_wide, file = "models/glmer_rasch_500_5_wide")
save(glmer_rasch_500_10_wide, file = "models/glmer_rasch_500_10_wide")
save(glmer_rasch_500_20_wide, file = "models/glmer_rasch_500_20_wide")
save(glmer_rasch_1000_5_wide, file = "models/glmer_rasch_1000_5_wide")
save(glmer_rasch_1000_10_wide, file = "models/glmer_rasch_1000_10_wide")
save(glmer_rasch_1000_20_wide, file = "models/glmer_rasch_1000_20_wide")
save(glmer_raschhigherror_50_5_narrow, file = "models/glmer_raschhigherror_50_5_narrow")
save(glmer_raschhigherror_50_10_narrow, file = "models/glmer_raschhigherror_50_10_narrow")
save(glmer_raschhigherror_50_20_narrow, file = "models/glmer_raschhigherror_50_20_narrow")
save(glmer_raschhigherror_100_5_narrow, file = "models/glmer_raschhigherror_100_5_narrow")
save(glmer_raschhigherror_100_10_narrow, file = "models/glmer_raschhigherror_100_10_narrow")
save(glmer_raschhigherror_100_20_narrow, file = "models/glmer_raschhigherror_100_20_narrow")
save(glmer_raschhigherror_250_5_narrow, file = "models/glmer_raschhigherror_250_5_narrow")
save(glmer_raschhigherror_250_10_narrow, file = "models/glmer_raschhigherror_250_10_narrow")
save(glmer_raschhigherror_250_20_narrow, file = "models/glmer_raschhigherror_250_20_narrow")
save(glmer_raschhigherror_500_5_narrow, file = "models/glmer_raschhigherror_500_5_narrow")
save(glmer_raschhigherror_500_10_narrow, file = "models/glmer_raschhigherror_500_10_narrow")
save(glmer_raschhigherror_500_20_narrow, file = "models/glmer_raschhigherror_500_20_narrow")
save(glmer_raschhigherror_1000_5_narrow, file = "models/glmer_raschhigherror_1000_5_narrow")
save(glmer_raschhigherror_1000_10_narrow, file = "models/glmer_raschhigherror_1000_10_narrow")
save(glmer_raschhigherror_1000_20_narrow, file = "models/glmer_raschhigherror_1000_20_narrow")
save(glmer_raschhigherror_50_5_wide, file = "models/glmer_raschhigherror_50_5_wide")
save(glmer_raschhigherror_50_10_wide, file = "models/glmer_raschhigherror_50_10_wide")
save(glmer_raschhigherror_50_20_wide, file = "models/glmer_raschhigherror_50_20_wide")
save(glmer_raschhigherror_100_5_wide, file = "models/glmer_raschhigherror_100_5_wide")
save(glmer_raschhigherror_100_10_wide, file = "models/glmer_raschhigherror_100_10_wide")
save(glmer_raschhigherror_100_20_wide, file = "models/glmer_raschhigherror_100_20_wide")
save(glmer_raschhigherror_250_5_wide, file = "models/glmer_raschhigherror_250_5_wide")
save(glmer_raschhigherror_250_10_wide, file = "models/glmer_raschhigherror_250_10_wide")
save(glmer_raschhigherror_250_20_wide, file = "models/glmer_raschhigherror_250_20_wide")
save(glmer_raschhigherror_500_5_wide, file = "models/glmer_raschhigherror_500_5_wide")
save(glmer_raschhigherror_500_10_wide, file = "models/glmer_raschhigherror_500_10_wide")
save(glmer_raschhigherror_500_20_wide, file = "models/glmer_raschhigherror_500_20_wide")
save(glmer_raschhigherror_1000_5_wide, file = "models/glmer_raschhigherror_1000_5_wide")
save(glmer_raschhigherror_1000_10_wide, file = "models/glmer_raschhigherror_1000_10_wide")
save(glmer_raschhigherror_1000_20_wide, file = "models/glmer_raschhigherror_1000_20_wide")
save(glmer_raschlowerror_50_5_narrow, file = "models/glmer_raschlowerror_50_5_narrow")
save(glmer_raschlowerror_50_10_narrow, file = "models/glmer_raschlowerror_50_10_narrow")
save(glmer_raschlowerror_50_20_narrow, file = "models/glmer_raschlowerror_50_20_narrow")
save(glmer_raschlowerror_100_5_narrow, file = "models/glmer_raschlowerror_100_5_narrow")
save(glmer_raschlowerror_100_10_narrow, file = "models/glmer_raschlowerror_100_10_narrow")
save(glmer_raschlowerror_100_20_narrow, file = "models/glmer_raschlowerror_100_20_narrow")
save(glmer_raschlowerror_250_5_narrow, file = "models/glmer_raschlowerror_250_5_narrow")
save(glmer_raschlowerror_250_10_narrow, file = "models/glmer_raschlowerror_250_10_narrow")
save(glmer_raschlowerror_250_20_narrow, file = "models/glmer_raschlowerror_250_20_narrow")
save(glmer_raschlowerror_500_5_narrow, file = "models/glmer_raschlowerror_500_5_narrow")
save(glmer_raschlowerror_500_10_narrow, file = "models/glmer_raschlowerror_500_10_narrow")
save(glmer_raschlowerror_500_20_narrow, file = "models/glmer_raschlowerror_500_20_narrow")
save(glmer_raschlowerror_1000_5_narrow, file = "models/glmer_raschlowerror_1000_5_narrow")
save(glmer_raschlowerror_1000_10_narrow, file = "models/glmer_raschlowerror_1000_10_narrow")
save(glmer_raschlowerror_1000_20_narrow, file = "models/glmer_raschlowerror_1000_20_narrow")
save(glmer_raschlowerror_50_5_wide, file = "models/glmer_raschlowerror_50_5_wide")
save(glmer_raschlowerror_50_10_wide, file = "models/glmer_raschlowerror_50_10_wide")
save(glmer_raschlowerror_50_20_wide, file = "models/glmer_raschlowerror_50_20_wide")
save(glmer_raschlowerror_100_5_wide, file = "models/glmer_raschlowerror_100_5_wide")
save(glmer_raschlowerror_100_10_wide, file = "models/glmer_raschlowerror_100_10_wide")
save(glmer_raschlowerror_100_20_wide, file = "models/glmer_raschlowerror_100_20_wide")
save(glmer_raschlowerror_250_5_wide, file = "models/glmer_raschlowerror_250_5_wide")
save(glmer_raschlowerror_250_10_wide, file = "models/glmer_raschlowerror_250_10_wide")
save(glmer_raschlowerror_250_20_wide, file = "models/glmer_raschlowerror_250_20_wide")
save(glmer_raschlowerror_500_5_wide, file = "models/glmer_raschlowerror_500_5_wide")
save(glmer_raschlowerror_500_10_wide, file = "models/glmer_raschlowerror_500_10_wide")
save(glmer_raschlowerror_500_20_wide, file = "models/glmer_raschlowerror_500_20_wide")
save(glmer_raschlowerror_1000_5_wide, file = "models/glmer_raschlowerror_1000_5_wide")
save(glmer_raschlowerror_1000_10_wide, file = "models/glmer_raschlowerror_1000_10_wide")
save(glmer_raschlowerror_1000_20_wide, file = "models/glmer_raschlowerror_1000_20_wide")

### Let's combine the true scale estimates and plot the scale recovery

true2estimate_scales <- rbind(true2estimate_scales_rasch_50_5_narrow,
                              true2estimate_scales_rasch_50_10_narrow,
                              true2estimate_scales_rasch_50_20_narrow,
                              true2estimate_scales_rasch_100_5_narrow,
                              true2estimate_scales_rasch_100_10_narrow,
                              true2estimate_scales_rasch_100_20_narrow,
                              true2estimate_scales_rasch_250_5_narrow,
                              true2estimate_scales_rasch_250_10_narrow,
                              true2estimate_scales_rasch_250_20_narrow,
                              true2estimate_scales_rasch_500_5_narrow,
                              true2estimate_scales_rasch_500_10_narrow,
                              true2estimate_scales_rasch_500_20_narrow,
                              true2estimate_scales_rasch_1000_5_narrow,
                              true2estimate_scales_rasch_1000_10_narrow,
                              true2estimate_scales_rasch_1000_20_narrow,
                              true2estimate_scales_rasch_50_5_wide,
                              true2estimate_scales_rasch_50_10_wide,
                              true2estimate_scales_rasch_50_20_wide,
                              true2estimate_scales_rasch_100_5_wide,
                              true2estimate_scales_rasch_100_10_wide,
                              true2estimate_scales_rasch_100_20_wide,
                              true2estimate_scales_rasch_250_5_wide,
                              true2estimate_scales_rasch_250_10_wide,
                              true2estimate_scales_rasch_250_20_wide,
                              true2estimate_scales_rasch_500_5_wide,
                              true2estimate_scales_rasch_500_10_wide,
                              true2estimate_scales_rasch_500_20_wide,
                              true2estimate_scales_rasch_1000_5_wide,
                              true2estimate_scales_rasch_1000_10_wide,
                              true2estimate_scales_rasch_1000_20_wide,
                              true2estimate_scales_raschhigherror_50_5_narrow,
                              true2estimate_scales_raschhigherror_50_10_narrow,
                              true2estimate_scales_raschhigherror_50_20_narrow,
                              true2estimate_scales_raschhigherror_100_5_narrow,
                              true2estimate_scales_raschhigherror_100_10_narrow,
                              true2estimate_scales_raschhigherror_100_20_narrow,
                              true2estimate_scales_raschhigherror_250_5_narrow,
                              true2estimate_scales_raschhigherror_250_10_narrow,
                              true2estimate_scales_raschhigherror_250_20_narrow,
                              true2estimate_scales_raschhigherror_500_5_narrow,
                              true2estimate_scales_raschhigherror_500_10_narrow,
                              true2estimate_scales_raschhigherror_500_20_narrow,
                              true2estimate_scales_raschhigherror_1000_5_narrow,
                              true2estimate_scales_raschhigherror_1000_10_narrow,
                              true2estimate_scales_raschhigherror_1000_20_narrow,
                              true2estimate_scales_raschhigherror_50_5_wide,
                              true2estimate_scales_raschhigherror_50_10_wide,
                              true2estimate_scales_raschhigherror_50_20_wide,
                              true2estimate_scales_raschhigherror_100_5_wide,
                              true2estimate_scales_raschhigherror_100_10_wide,
                              true2estimate_scales_raschhigherror_100_20_wide,
                              true2estimate_scales_raschhigherror_250_5_wide,
                              true2estimate_scales_raschhigherror_250_10_wide,
                              true2estimate_scales_raschhigherror_250_20_wide,
                              true2estimate_scales_raschhigherror_500_5_wide,
                              true2estimate_scales_raschhigherror_500_10_wide,
                              true2estimate_scales_raschhigherror_500_20_wide,
                              true2estimate_scales_raschhigherror_1000_5_wide,
                              true2estimate_scales_raschhigherror_1000_10_wide,
                              true2estimate_scales_raschhigherror_1000_20_wide,
                              true2estimate_scales_raschlowerror_50_5_narrow,
                              true2estimate_scales_raschlowerror_50_10_narrow,
                              true2estimate_scales_raschlowerror_50_20_narrow,
                              true2estimate_scales_raschlowerror_100_5_narrow,
                              true2estimate_scales_raschlowerror_100_10_narrow,
                              true2estimate_scales_raschlowerror_100_20_narrow,
                              true2estimate_scales_raschlowerror_250_5_narrow,
                              true2estimate_scales_raschlowerror_250_10_narrow,
                              true2estimate_scales_raschlowerror_250_20_narrow,
                              true2estimate_scales_raschlowerror_500_5_narrow,
                              true2estimate_scales_raschlowerror_500_10_narrow,
                              true2estimate_scales_raschlowerror_500_20_narrow,
                              true2estimate_scales_raschlowerror_1000_5_narrow,
                              true2estimate_scales_raschlowerror_1000_10_narrow,
                              true2estimate_scales_raschlowerror_1000_20_narrow,
                              true2estimate_scales_raschlowerror_50_5_wide,
                              true2estimate_scales_raschlowerror_50_10_wide,
                              true2estimate_scales_raschlowerror_50_20_wide,
                              true2estimate_scales_raschlowerror_100_5_wide,
                              true2estimate_scales_raschlowerror_100_10_wide,
                              true2estimate_scales_raschlowerror_100_20_wide,
                              true2estimate_scales_raschlowerror_250_5_wide,
                              true2estimate_scales_raschlowerror_250_10_wide,
                              true2estimate_scales_raschlowerror_250_20_wide,
                              true2estimate_scales_raschlowerror_500_5_wide,
                              true2estimate_scales_raschlowerror_500_10_wide,
                              true2estimate_scales_raschlowerror_500_20_wide,
                              true2estimate_scales_raschlowerror_1000_5_wide,
                              true2estimate_scales_raschlowerror_1000_10_wide,
                              true2estimate_scales_raschlowerror_1000_20_wide
                              ) %>%
  mutate(deviation_oneRM = abs((true_load - true_load_estimate_oneRM) / true_load ),
         deviation_item = abs((true_load - true_load_estimate_item) / true_load )) %>%
  mutate(sample_size = factor(sample_size, levels = c("50","100","250","500","1000")),
         item_number = factor(item_number, levels = c("5","10","20")),
         response_type = factor(response_type, levels = c("Low Error (?? = 5)", "Typical Error (?? = 1)", "High Error (?? = 0.1)"))) %>%
  mutate(range = dplyr::recode(range, narrow = "Narrow Item Range (60 to 140 kg)", wide = "Wide Item Range (10 to 200 kg)"),
         item_number_lab = "Number of Items",
         sample_size_lab = "Sample Size")

true2estimate_scales$parameter <- recode(true2estimate_scales$parameter, item = "Item (i.e., Load)", person = "Person (i.e., 1RM)")


save(true2estimate_scales, file = "models/true2estimate_scales")

true2estimate_oneRM_scales_plot <- true2estimate_scales %>%
  ggplot(aes(x=true_load, y=true_load_estimate_oneRM, color=parameter)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_line(alpha=0.75) +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Estimated Scale (kg)") +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(true2estimate_oneRM_scales_plot, file = "plots/true2estimate_oneRM_scales_plot")

true2estimate_oneRM_scales_plot

ggsave("plots/true2estimate_oneRM_scales_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

true2estimate_item_scales_plot <- true2estimate_scales %>%
  ggplot(aes(x=true_load, y=true_load_estimate_item, color=parameter)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_line(alpha=0.75) +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Estimated Scale (kg)") +
  scale_y_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(true2estimate_item_scales_plot, file = "plots/true2estimate_item_scales_plot")

true2estimate_item_scales_plot

ggsave("plots/true2estimate_item_scales_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

mean_deviations <- true2estimate_scales %>%
  group_by(sample_size_lab, sample_size, item_number_lab, item_number, range, response_type, parameter) %>%
  add_count() %>%
  summarise(mean_deviation_oneRM = sum(deviation_oneRM)/n,
            mean_deviation_item = sum(deviation_item)/n) %>%
  filter(row_number() == 1)

save(mean_deviations, file = "models/mean_deviations")

true2estimate_deviation_oneRM_plot <- true2estimate_scales %>%
  ggplot(aes(x=true_load, y=deviation_oneRM*100)) +
  geom_point(aes(color=parameter), size=0.75, alpha=0.5)  +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Deviation of Estimated Scale (%)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = subset(mean_deviations, parameter == "Item (i.e., Load)"), 
            aes(x=125, y=500, label = paste("Item MADP = ", round(mean_deviation_oneRM*100, digits=2),"%")),
            size = 2) +
  geom_text(data = subset(mean_deviations, parameter == "Person (i.e., 1RM)"), 
            aes(x=125, y=400, label = paste("Person MADP = ", round(mean_deviation_oneRM*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(true2estimate_deviation_oneRM_plot, file = "plots/true2estimate_deviation_oneRM_plot")

true2estimate_deviation_oneRM_plot

ggsave("plots/true2estimate_deviation_oneRM_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

true2estimate_deviation_item_plot <- true2estimate_scales %>%
  ggplot(aes(x=true_load, y=deviation_item*100)) +
  geom_point(aes(color=parameter), size=0.75, alpha=0.5)  +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Deviation of Estimated Scale (%)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = subset(mean_deviations, parameter == "Item (i.e., Load)"), 
            aes(x=125, y=500, label = paste("Item MADP = ", round(mean_deviation_item*100, digits=2),"%")),
            size = 2) +
  geom_text(data = subset(mean_deviations, parameter == "Person (i.e., 1RM)"), 
            aes(x=125, y=400, label = paste("Person MADP = ", round(mean_deviation_item*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(true2estimate_deviation_item_plot, file = "plots/true2estimate_deviation_item_plot")

true2estimate_deviation_item_plot

ggsave("plots/true2estimate_deviation_item_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)


### Following this we explore various alternative approaches to deriving effort based on the Rasch person and item parameters
# Including
  # The mean-sigma transformation to the raw scale (with both person and item anchoring)
  # The odds scale i.e., exp(logit)
  # The logit difference scale
  # The logit shift transformation

### Example of the logit shift
logit_shift_plot <- (data_frame(
  x = seq(-4,4,length=200),
  y = 1/sqrt(2*pi)*exp(-x^2/2)
) %>% 
  ggplot(aes(x,y)) +
  geom_area(fill="grey") +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(aes(x = 0, y = 0.25, xend = -4, yend=0.25), color = "black",
               arrow = arrow(length=unit(0.15,"cm"), angle = 90, ends="both", type = "open")) +
  geom_segment(aes(x = -4, y = 0, xend = 4, yend=0), color = "black") +
  annotate("text", x = -4, y = 0.275, label = "paste(italic(R))", parse = TRUE) + 
  labs(y = "",
       x = "Empirical distribution of scale from model (logits)") +
  scale_x_continuous(limits = c(-8, 8), breaks = c(0)) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.line.y = element_blank())) /
  
  (data_frame(
    x = seq(-4,4,length=200),
    y = 1/sqrt(2*pi)*exp(-x^2/2)
  ) %>% 
    ggplot(aes(x+4,y)) +
    geom_area(fill="grey") +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_segment(aes(x = 0, y = 0, xend = 8, yend=0), color = "black") +
    labs(y = "",
         x = expression("Transformed distribution of scale (logits + (0 -"~italic("R")~"))")) +
    scale_x_continuous(limits = c(-8, 8), breaks = c(0)) +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, size = 20),
          axis.line.y = element_blank())) +
  plot_annotation(tag_levels = 'A')

save(logit_shift_plot, file = "plots/logit_shift_plot")

logit_shift_plot

ggsave("plots/logit_shift_plot.png", width = 7.5, height = 5, device = "png", dpi = 300)

# Reload datasets
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

# Combine datasets with estimated person abilities and item difficulties, true scale estimates, and calculate all effort operationalisations

person_abilities_50_5_narrow <- data.frame(person = unique(oneRM_dat_50_5_narrow$person),
                   rasch_ability = person_abilities_rasch_50_5_narrow$`(Intercept)`, 
                   raschhigherror_ability = person_abilities_raschhigherror_50_5_narrow$`(Intercept)`, 
                   raschlowerror_ability = person_abilities_raschlowerror_50_5_narrow$`(Intercept)`,
                   rasch_true_oneRM_oneRM = person_abilities_rasch_50_5_narrow$person_true_scale_estimate_oneRM,
                   raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_5_narrow$person_true_scale_estimate_oneRM,
                   raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_5_narrow$person_true_scale_estimate_oneRM,
                   rasch_true_oneRM_item = person_abilities_rasch_50_5_narrow$person_true_scale_estimate_load,
                   raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_5_narrow$person_true_scale_estimate_load,
                   raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_5_narrow$person_true_scale_estimate_load)

item_difficulties_50_5_narrow <- data.frame(item = unique(oneRM_dat_50_5_narrow$item),
                                           rasch_difficulty = item_difficulties_rasch_50_5_narrow$`(Intercept)`, 
                                           raschhigherror_difficulty = item_difficulties_raschhigherror_50_5_narrow$`(Intercept)`, 
                                           raschlowerror_difficulty = item_difficulties_raschlowerror_50_5_narrow$`(Intercept)`,
                                           rasch_true_load_oneRM = item_difficulties_rasch_50_5_narrow$item_true_scale_estimate_oneRM,
                                           raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_5_narrow$item_true_scale_estimate_oneRM,
                                           raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_5_narrow$item_true_scale_estimate_oneRM,
                                           rasch_true_load_item = item_difficulties_rasch_50_5_narrow$item_true_scale_estimate_load,
                                           raschhigherror_true_load_item = item_difficulties_raschhigherror_50_5_narrow$item_true_scale_estimate_load,
                                           raschlowerror_true_load_item = item_difficulties_raschlowerror_50_5_narrow$item_true_scale_estimate_load)

oneRM_dat_50_5_narrow <- left_join(oneRM_dat_50_5_narrow, person_abilities_50_5_narrow, by="person") %>%
  left_join(item_difficulties_50_5_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "5",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_50_10_narrow <- data.frame(person = unique(oneRM_dat_50_10_narrow$person),
                                           rasch_ability = person_abilities_rasch_50_10_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_50_10_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_50_10_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_50_10_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_10_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_10_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_50_10_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_10_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_10_narrow$person_true_scale_estimate_load)

item_difficulties_50_10_narrow <- data.frame(item = unique(oneRM_dat_50_10_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_50_10_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_50_10_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_50_10_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_50_10_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_10_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_10_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_50_10_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_50_10_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_50_10_narrow$item_true_scale_estimate_load)

oneRM_dat_50_10_narrow <- left_join(oneRM_dat_50_10_narrow, person_abilities_50_10_narrow, by="person") %>%
  left_join(item_difficulties_50_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "10",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_50_20_narrow <- data.frame(person = unique(oneRM_dat_50_20_narrow$person),
                                           rasch_ability = person_abilities_rasch_50_20_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_50_20_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_50_20_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_50_20_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_20_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_20_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_50_20_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_20_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_20_narrow$person_true_scale_estimate_load)

item_difficulties_50_20_narrow <- data.frame(item = unique(oneRM_dat_50_20_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_50_20_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_50_20_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_50_20_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_50_20_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_20_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_20_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_50_20_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_50_20_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_50_20_narrow$item_true_scale_estimate_load)

oneRM_dat_50_20_narrow <- left_join(oneRM_dat_50_20_narrow, person_abilities_50_20_narrow, by="person") %>%
  left_join(item_difficulties_50_20_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "20",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_5_narrow <- data.frame(person = unique(oneRM_dat_100_5_narrow$person),
                                           rasch_ability = person_abilities_rasch_100_5_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_100_5_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_100_5_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_100_5_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_5_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_5_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_100_5_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_5_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_5_narrow$person_true_scale_estimate_load)

item_difficulties_100_5_narrow <- data.frame(item = unique(oneRM_dat_100_5_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_100_5_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_100_5_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_100_5_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_100_5_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_5_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_5_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_100_5_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_100_5_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_100_5_narrow$item_true_scale_estimate_load)

oneRM_dat_100_5_narrow <- left_join(oneRM_dat_100_5_narrow, person_abilities_100_5_narrow, by="person") %>%
  left_join(item_difficulties_100_5_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "5",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_10_narrow <- data.frame(person = unique(oneRM_dat_100_10_narrow$person),
                                            rasch_ability = person_abilities_rasch_100_10_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_100_10_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_100_10_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_100_10_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_10_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_10_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_100_10_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_10_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_10_narrow$person_true_scale_estimate_load)

item_difficulties_100_10_narrow <- data.frame(item = unique(oneRM_dat_100_10_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_100_10_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_100_10_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_100_10_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_100_10_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_10_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_10_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_100_10_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_100_10_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_100_10_narrow$item_true_scale_estimate_load)

oneRM_dat_100_10_narrow <- left_join(oneRM_dat_100_10_narrow, person_abilities_100_10_narrow, by="person") %>%
  left_join(item_difficulties_100_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "10",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_20_narrow <- data.frame(person = unique(oneRM_dat_100_20_narrow$person),
                                            rasch_ability = person_abilities_rasch_100_20_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_100_20_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_100_20_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_100_20_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_20_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_20_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_100_20_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_20_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_20_narrow$person_true_scale_estimate_load)

item_difficulties_100_20_narrow <- data.frame(item = unique(oneRM_dat_100_20_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_100_20_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_100_20_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_100_20_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_100_20_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_20_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_20_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_100_20_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_100_20_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_100_20_narrow$item_true_scale_estimate_load)

oneRM_dat_100_20_narrow <- left_join(oneRM_dat_100_20_narrow, person_abilities_100_20_narrow, by="person") %>%
  left_join(item_difficulties_100_20_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "20",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_250_5_narrow <- data.frame(person = unique(oneRM_dat_250_5_narrow$person),
                                           rasch_ability = person_abilities_rasch_250_5_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_250_5_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_250_5_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_250_5_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_5_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_5_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_250_5_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_5_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_5_narrow$person_true_scale_estimate_load)

item_difficulties_250_5_narrow <- data.frame(item = unique(oneRM_dat_250_5_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_250_5_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_250_5_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_250_5_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_250_5_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_5_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_5_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_250_5_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_250_5_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_250_5_narrow$item_true_scale_estimate_load)

oneRM_dat_250_5_narrow <- left_join(oneRM_dat_250_5_narrow, person_abilities_250_5_narrow, by="person") %>%
  left_join(item_difficulties_250_5_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "5",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_250_10_narrow <- data.frame(person = unique(oneRM_dat_250_10_narrow$person),
                                            rasch_ability = person_abilities_rasch_250_10_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_250_10_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_250_10_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_250_10_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_10_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_10_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_250_10_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_10_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_10_narrow$person_true_scale_estimate_load)

item_difficulties_250_10_narrow <- data.frame(item = unique(oneRM_dat_250_10_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_250_10_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_250_10_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_250_10_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_250_10_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_10_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_10_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_250_10_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_250_10_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_250_10_narrow$item_true_scale_estimate_load)

oneRM_dat_250_10_narrow <- left_join(oneRM_dat_250_10_narrow, person_abilities_250_10_narrow, by="person") %>%
  left_join(item_difficulties_250_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "10",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_250_20_narrow <- data.frame(person = unique(oneRM_dat_250_20_narrow$person),
                                            rasch_ability = person_abilities_rasch_250_20_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_250_20_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_250_20_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_250_20_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_20_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_20_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_250_20_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_20_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_20_narrow$person_true_scale_estimate_load)

item_difficulties_250_20_narrow <- data.frame(item = unique(oneRM_dat_250_20_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_250_20_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_250_20_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_250_20_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_250_20_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_20_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_20_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_250_20_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_250_20_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_250_20_narrow$item_true_scale_estimate_load)

oneRM_dat_250_20_narrow <- left_join(oneRM_dat_250_20_narrow, person_abilities_250_20_narrow, by="person") %>%
  left_join(item_difficulties_250_20_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "20",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_500_5_narrow <- data.frame(person = unique(oneRM_dat_500_5_narrow$person),
                                           rasch_ability = person_abilities_rasch_500_5_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_500_5_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_500_5_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_500_5_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_5_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_5_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_500_5_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_5_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_5_narrow$person_true_scale_estimate_load)

item_difficulties_500_5_narrow <- data.frame(item = unique(oneRM_dat_500_5_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_500_5_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_500_5_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_500_5_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_500_5_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_5_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_5_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_500_5_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_500_5_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_500_5_narrow$item_true_scale_estimate_load)

oneRM_dat_500_5_narrow <- left_join(oneRM_dat_500_5_narrow, person_abilities_500_5_narrow, by="person") %>%
  left_join(item_difficulties_500_5_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "5",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_500_10_narrow <- data.frame(person = unique(oneRM_dat_500_10_narrow$person),
                                            rasch_ability = person_abilities_rasch_500_10_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_500_10_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_500_10_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_500_10_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_10_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_10_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_500_10_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_10_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_10_narrow$person_true_scale_estimate_load)

item_difficulties_500_10_narrow <- data.frame(item = unique(oneRM_dat_500_10_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_500_10_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_500_10_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_500_10_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_500_10_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_10_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_10_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_500_10_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_500_10_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_500_10_narrow$item_true_scale_estimate_load)

oneRM_dat_500_10_narrow <- left_join(oneRM_dat_500_10_narrow, person_abilities_500_10_narrow, by="person") %>%
  left_join(item_difficulties_500_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "10",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_500_20_narrow <- data.frame(person = unique(oneRM_dat_500_20_narrow$person),
                                            rasch_ability = person_abilities_rasch_500_20_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_500_20_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_500_20_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_500_20_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_20_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_20_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_500_20_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_20_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_20_narrow$person_true_scale_estimate_load)

item_difficulties_500_20_narrow <- data.frame(item = unique(oneRM_dat_500_20_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_500_20_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_500_20_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_500_20_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_500_20_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_20_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_20_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_500_20_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_500_20_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_500_20_narrow$item_true_scale_estimate_load)

oneRM_dat_500_20_narrow <- left_join(oneRM_dat_500_20_narrow, person_abilities_500_20_narrow, by="person") %>%
  left_join(item_difficulties_500_20_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "20",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_1000_5_narrow <- data.frame(person = unique(oneRM_dat_1000_5_narrow$person),
                                           rasch_ability = person_abilities_rasch_1000_5_narrow$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_1000_5_narrow$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_1000_5_narrow$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_1000_5_narrow$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_5_narrow$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_5_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_1000_5_narrow$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_5_narrow$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_5_narrow$person_true_scale_estimate_load)

item_difficulties_1000_5_narrow <- data.frame(item = unique(oneRM_dat_1000_5_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_1000_5_narrow$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_1000_5_narrow$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_1000_5_narrow$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_1000_5_narrow$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_5_narrow$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_5_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_1000_5_narrow$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_5_narrow$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_5_narrow$item_true_scale_estimate_load)

oneRM_dat_1000_5_narrow <- left_join(oneRM_dat_1000_5_narrow, person_abilities_1000_5_narrow, by="person") %>%
  left_join(item_difficulties_1000_5_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_1000_10_narrow <- data.frame(person = unique(oneRM_dat_1000_10_narrow$person),
                                            rasch_ability = person_abilities_rasch_1000_10_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_1000_10_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_1000_10_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_1000_10_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_10_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_10_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_1000_10_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_10_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_10_narrow$person_true_scale_estimate_load)

item_difficulties_1000_10_narrow <- data.frame(item = unique(oneRM_dat_1000_10_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_1000_10_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_1000_10_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_1000_10_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_1000_10_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_10_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_10_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_1000_10_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_10_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_10_narrow$item_true_scale_estimate_load)

oneRM_dat_1000_10_narrow <- left_join(oneRM_dat_1000_10_narrow, person_abilities_1000_10_narrow, by="person") %>%
  left_join(item_difficulties_1000_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_1000_20_narrow <- data.frame(person = unique(oneRM_dat_1000_20_narrow$person),
                                            rasch_ability = person_abilities_rasch_1000_20_narrow$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_1000_20_narrow$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_1000_20_narrow$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_1000_20_narrow$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_20_narrow$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_20_narrow$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_1000_20_narrow$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_20_narrow$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_20_narrow$person_true_scale_estimate_load)

item_difficulties_1000_20_narrow <- data.frame(item = unique(oneRM_dat_1000_20_narrow$item),
                                             rasch_difficulty = item_difficulties_rasch_1000_20_narrow$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_1000_20_narrow$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_1000_20_narrow$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_1000_20_narrow$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_20_narrow$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_20_narrow$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_1000_20_narrow$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_20_narrow$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_20_narrow$item_true_scale_estimate_load)

oneRM_dat_1000_20_narrow <- left_join(oneRM_dat_1000_20_narrow, person_abilities_1000_20_narrow, by="person") %>%
  left_join(item_difficulties_1000_20_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         range = "narrow") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


# Combine datasets with estimated person abilities and item difficulties, true scale estimates, and calculate all effort operationalisations

person_abilities_50_5_wide <- data.frame(person = unique(oneRM_dat_50_5_wide$person),
                                           rasch_ability = person_abilities_rasch_50_5_wide$`(Intercept)`, 
                                           raschhigherror_ability = person_abilities_raschhigherror_50_5_wide$`(Intercept)`, 
                                           raschlowerror_ability = person_abilities_raschlowerror_50_5_wide$`(Intercept)`,
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_50_5_wide$person_true_scale_estimate_oneRM,
                                           raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_5_wide$person_true_scale_estimate_oneRM,
                                           raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_5_wide$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_50_5_wide$person_true_scale_estimate_load,
                                           raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_5_wide$person_true_scale_estimate_load,
                                           raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_5_wide$person_true_scale_estimate_load)

item_difficulties_50_5_wide <- data.frame(item = unique(oneRM_dat_50_5_wide$item),
                                            rasch_difficulty = item_difficulties_rasch_50_5_wide$`(Intercept)`, 
                                            raschhigherror_difficulty = item_difficulties_raschhigherror_50_5_wide$`(Intercept)`, 
                                            raschlowerror_difficulty = item_difficulties_raschlowerror_50_5_wide$`(Intercept)`,
                                            rasch_true_load_oneRM = item_difficulties_rasch_50_5_wide$item_true_scale_estimate_oneRM,
                                            raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_5_wide$item_true_scale_estimate_oneRM,
                                            raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_5_wide$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_50_5_wide$item_true_scale_estimate_load,
                                            raschhigherror_true_load_item = item_difficulties_raschhigherror_50_5_wide$item_true_scale_estimate_load,
                                            raschlowerror_true_load_item = item_difficulties_raschlowerror_50_5_wide$item_true_scale_estimate_load)

oneRM_dat_50_5_wide <- left_join(oneRM_dat_50_5_wide, person_abilities_50_5_wide, by="person") %>%
  left_join(item_difficulties_50_5_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "5",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_50_10_wide <- data.frame(person = unique(oneRM_dat_50_10_wide$person),
                                            rasch_ability = person_abilities_rasch_50_10_wide$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_50_10_wide$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_50_10_wide$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_50_10_wide$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_10_wide$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_10_wide$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_50_10_wide$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_10_wide$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_10_wide$person_true_scale_estimate_load)

item_difficulties_50_10_wide <- data.frame(item = unique(oneRM_dat_50_10_wide$item),
                                             rasch_difficulty = item_difficulties_rasch_50_10_wide$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_50_10_wide$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_50_10_wide$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_50_10_wide$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_10_wide$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_10_wide$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_50_10_wide$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_50_10_wide$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_50_10_wide$item_true_scale_estimate_load)

oneRM_dat_50_10_wide <- left_join(oneRM_dat_50_10_wide, person_abilities_50_10_wide, by="person") %>%
  left_join(item_difficulties_50_10_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "10",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_50_20_wide <- data.frame(person = unique(oneRM_dat_50_20_wide$person),
                                            rasch_ability = person_abilities_rasch_50_20_wide$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_50_20_wide$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_50_20_wide$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_50_20_wide$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_50_20_wide$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_50_20_wide$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_50_20_wide$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_50_20_wide$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_50_20_wide$person_true_scale_estimate_load)

item_difficulties_50_20_wide <- data.frame(item = unique(oneRM_dat_50_20_wide$item),
                                             rasch_difficulty = item_difficulties_rasch_50_20_wide$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_50_20_wide$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_50_20_wide$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_50_20_wide$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_50_20_wide$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_50_20_wide$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_50_20_wide$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_50_20_wide$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_50_20_wide$item_true_scale_estimate_load)

oneRM_dat_50_20_wide <- left_join(oneRM_dat_50_20_wide, person_abilities_50_20_wide, by="person") %>%
  left_join(item_difficulties_50_20_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "50",
         item_number = "20",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_5_wide <- data.frame(person = unique(oneRM_dat_100_5_wide$person),
                                            rasch_ability = person_abilities_rasch_100_5_wide$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_100_5_wide$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_100_5_wide$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_100_5_wide$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_5_wide$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_5_wide$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_100_5_wide$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_5_wide$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_5_wide$person_true_scale_estimate_load)

item_difficulties_100_5_wide <- data.frame(item = unique(oneRM_dat_100_5_wide$item),
                                             rasch_difficulty = item_difficulties_rasch_100_5_wide$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_100_5_wide$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_100_5_wide$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_100_5_wide$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_5_wide$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_5_wide$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_100_5_wide$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_100_5_wide$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_100_5_wide$item_true_scale_estimate_load)

oneRM_dat_100_5_wide <- left_join(oneRM_dat_100_5_wide, person_abilities_100_5_wide, by="person") %>%
  left_join(item_difficulties_100_5_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "5",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_10_wide <- data.frame(person = unique(oneRM_dat_100_10_wide$person),
                                             rasch_ability = person_abilities_rasch_100_10_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_100_10_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_100_10_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_100_10_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_10_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_10_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_100_10_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_10_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_10_wide$person_true_scale_estimate_load)

item_difficulties_100_10_wide <- data.frame(item = unique(oneRM_dat_100_10_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_100_10_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_100_10_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_100_10_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_100_10_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_10_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_10_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_100_10_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_100_10_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_100_10_wide$item_true_scale_estimate_load)

oneRM_dat_100_10_wide <- left_join(oneRM_dat_100_10_wide, person_abilities_100_10_wide, by="person") %>%
  left_join(item_difficulties_100_10_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "10",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_100_20_wide <- data.frame(person = unique(oneRM_dat_100_20_wide$person),
                                             rasch_ability = person_abilities_rasch_100_20_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_100_20_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_100_20_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_100_20_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_100_20_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_100_20_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_100_20_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_100_20_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_100_20_wide$person_true_scale_estimate_load)

item_difficulties_100_20_wide <- data.frame(item = unique(oneRM_dat_100_20_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_100_20_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_100_20_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_100_20_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_100_20_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_100_20_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_100_20_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_100_20_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_100_20_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_100_20_wide$item_true_scale_estimate_load)

oneRM_dat_100_20_wide <- left_join(oneRM_dat_100_20_wide, person_abilities_100_20_wide, by="person") %>%
  left_join(item_difficulties_100_20_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "100",
         item_number = "20",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_250_5_wide <- data.frame(person = unique(oneRM_dat_250_5_wide$person),
                                            rasch_ability = person_abilities_rasch_250_5_wide$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_250_5_wide$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_250_5_wide$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_250_5_wide$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_5_wide$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_5_wide$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_250_5_wide$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_5_wide$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_5_wide$person_true_scale_estimate_load)

item_difficulties_250_5_wide <- data.frame(item = unique(oneRM_dat_250_5_wide$item),
                                             rasch_difficulty = item_difficulties_rasch_250_5_wide$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_250_5_wide$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_250_5_wide$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_250_5_wide$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_5_wide$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_5_wide$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_250_5_wide$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_250_5_wide$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_250_5_wide$item_true_scale_estimate_load)

oneRM_dat_250_5_wide <- left_join(oneRM_dat_250_5_wide, person_abilities_250_5_wide, by="person") %>%
  left_join(item_difficulties_250_5_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "5",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_250_10_wide <- data.frame(person = unique(oneRM_dat_250_10_wide$person),
                                             rasch_ability = person_abilities_rasch_250_10_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_250_10_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_250_10_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_250_10_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_10_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_10_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_250_10_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_10_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_10_wide$person_true_scale_estimate_load)

item_difficulties_250_10_wide <- data.frame(item = unique(oneRM_dat_250_10_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_250_10_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_250_10_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_250_10_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_250_10_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_10_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_10_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_250_10_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_250_10_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_250_10_wide$item_true_scale_estimate_load)

oneRM_dat_250_10_wide <- left_join(oneRM_dat_250_10_wide, person_abilities_250_10_wide, by="person") %>%
  left_join(item_difficulties_250_10_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "10",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_250_20_wide <- data.frame(person = unique(oneRM_dat_250_20_wide$person),
                                             rasch_ability = person_abilities_rasch_250_20_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_250_20_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_250_20_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_250_20_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_250_20_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_250_20_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_250_20_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_250_20_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_250_20_wide$person_true_scale_estimate_load)

item_difficulties_250_20_wide <- data.frame(item = unique(oneRM_dat_250_20_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_250_20_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_250_20_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_250_20_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_250_20_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_250_20_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_250_20_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_250_20_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_250_20_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_250_20_wide$item_true_scale_estimate_load)

oneRM_dat_250_20_wide <- left_join(oneRM_dat_250_20_wide, person_abilities_250_20_wide, by="person") %>%
  left_join(item_difficulties_250_20_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "250",
         item_number = "20",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_500_5_wide <- data.frame(person = unique(oneRM_dat_500_5_wide$person),
                                            rasch_ability = person_abilities_rasch_500_5_wide$`(Intercept)`, 
                                            raschhigherror_ability = person_abilities_raschhigherror_500_5_wide$`(Intercept)`, 
                                            raschlowerror_ability = person_abilities_raschlowerror_500_5_wide$`(Intercept)`,
                                            rasch_true_oneRM_oneRM = person_abilities_rasch_500_5_wide$person_true_scale_estimate_oneRM,
                                            raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_5_wide$person_true_scale_estimate_oneRM,
                                            raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_5_wide$person_true_scale_estimate_oneRM,
                                            rasch_true_oneRM_item = person_abilities_rasch_500_5_wide$person_true_scale_estimate_load,
                                            raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_5_wide$person_true_scale_estimate_load,
                                            raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_5_wide$person_true_scale_estimate_load)

item_difficulties_500_5_wide <- data.frame(item = unique(oneRM_dat_500_5_wide$item),
                                             rasch_difficulty = item_difficulties_rasch_500_5_wide$`(Intercept)`, 
                                             raschhigherror_difficulty = item_difficulties_raschhigherror_500_5_wide$`(Intercept)`, 
                                             raschlowerror_difficulty = item_difficulties_raschlowerror_500_5_wide$`(Intercept)`,
                                             rasch_true_load_oneRM = item_difficulties_rasch_500_5_wide$item_true_scale_estimate_oneRM,
                                             raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_5_wide$item_true_scale_estimate_oneRM,
                                             raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_5_wide$item_true_scale_estimate_oneRM,
                                             rasch_true_load_item = item_difficulties_rasch_500_5_wide$item_true_scale_estimate_load,
                                             raschhigherror_true_load_item = item_difficulties_raschhigherror_500_5_wide$item_true_scale_estimate_load,
                                             raschlowerror_true_load_item = item_difficulties_raschlowerror_500_5_wide$item_true_scale_estimate_load)

oneRM_dat_500_5_wide <- left_join(oneRM_dat_500_5_wide, person_abilities_500_5_wide, by="person") %>%
  left_join(item_difficulties_500_5_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "5",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_500_10_wide <- data.frame(person = unique(oneRM_dat_500_10_wide$person),
                                             rasch_ability = person_abilities_rasch_500_10_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_500_10_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_500_10_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_500_10_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_10_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_10_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_500_10_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_10_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_10_wide$person_true_scale_estimate_load)

item_difficulties_500_10_wide <- data.frame(item = unique(oneRM_dat_500_10_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_500_10_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_500_10_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_500_10_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_500_10_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_10_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_10_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_500_10_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_500_10_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_500_10_wide$item_true_scale_estimate_load)

oneRM_dat_500_10_wide <- left_join(oneRM_dat_500_10_wide, person_abilities_500_10_wide, by="person") %>%
  left_join(item_difficulties_500_10_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "10",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_500_20_wide <- data.frame(person = unique(oneRM_dat_500_20_wide$person),
                                             rasch_ability = person_abilities_rasch_500_20_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_500_20_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_500_20_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_500_20_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_500_20_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_500_20_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_500_20_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_500_20_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_500_20_wide$person_true_scale_estimate_load)

item_difficulties_500_20_wide <- data.frame(item = unique(oneRM_dat_500_20_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_500_20_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_500_20_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_500_20_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_500_20_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_500_20_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_500_20_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_500_20_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_500_20_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_500_20_wide$item_true_scale_estimate_load)

oneRM_dat_500_20_wide <- left_join(oneRM_dat_500_20_wide, person_abilities_500_20_wide, by="person") %>%
  left_join(item_difficulties_500_20_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "500",
         item_number = "20",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


person_abilities_1000_5_wide <- data.frame(person = unique(oneRM_dat_1000_5_wide$person),
                                             rasch_ability = person_abilities_rasch_1000_5_wide$`(Intercept)`, 
                                             raschhigherror_ability = person_abilities_raschhigherror_1000_5_wide$`(Intercept)`, 
                                             raschlowerror_ability = person_abilities_raschlowerror_1000_5_wide$`(Intercept)`,
                                             rasch_true_oneRM_oneRM = person_abilities_rasch_1000_5_wide$person_true_scale_estimate_oneRM,
                                             raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_5_wide$person_true_scale_estimate_oneRM,
                                             raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_5_wide$person_true_scale_estimate_oneRM,
                                             rasch_true_oneRM_item = person_abilities_rasch_1000_5_wide$person_true_scale_estimate_load,
                                             raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_5_wide$person_true_scale_estimate_load,
                                             raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_5_wide$person_true_scale_estimate_load)

item_difficulties_1000_5_wide <- data.frame(item = unique(oneRM_dat_1000_5_wide$item),
                                              rasch_difficulty = item_difficulties_rasch_1000_5_wide$`(Intercept)`, 
                                              raschhigherror_difficulty = item_difficulties_raschhigherror_1000_5_wide$`(Intercept)`, 
                                              raschlowerror_difficulty = item_difficulties_raschlowerror_1000_5_wide$`(Intercept)`,
                                              rasch_true_load_oneRM = item_difficulties_rasch_1000_5_wide$item_true_scale_estimate_oneRM,
                                              raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_5_wide$item_true_scale_estimate_oneRM,
                                              raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_5_wide$item_true_scale_estimate_oneRM,
                                              rasch_true_load_item = item_difficulties_rasch_1000_5_wide$item_true_scale_estimate_load,
                                              raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_5_wide$item_true_scale_estimate_load,
                                              raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_5_wide$item_true_scale_estimate_load)

oneRM_dat_1000_5_wide <- left_join(oneRM_dat_1000_5_wide, person_abilities_1000_5_wide, by="person") %>%
  left_join(item_difficulties_1000_5_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "5",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_1000_10_wide <- data.frame(person = unique(oneRM_dat_1000_10_wide$person),
                                              rasch_ability = person_abilities_rasch_1000_10_wide$`(Intercept)`, 
                                              raschhigherror_ability = person_abilities_raschhigherror_1000_10_wide$`(Intercept)`, 
                                              raschlowerror_ability = person_abilities_raschlowerror_1000_10_wide$`(Intercept)`,
                                              rasch_true_oneRM_oneRM = person_abilities_rasch_1000_10_wide$person_true_scale_estimate_oneRM,
                                              raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_10_wide$person_true_scale_estimate_oneRM,
                                              raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_10_wide$person_true_scale_estimate_oneRM,
                                              rasch_true_oneRM_item = person_abilities_rasch_1000_10_wide$person_true_scale_estimate_load,
                                              raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_10_wide$person_true_scale_estimate_load,
                                              raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_10_wide$person_true_scale_estimate_load)

item_difficulties_1000_10_wide <- data.frame(item = unique(oneRM_dat_1000_10_wide$item),
                                               rasch_difficulty = item_difficulties_rasch_1000_10_wide$`(Intercept)`, 
                                               raschhigherror_difficulty = item_difficulties_raschhigherror_1000_10_wide$`(Intercept)`, 
                                               raschlowerror_difficulty = item_difficulties_raschlowerror_1000_10_wide$`(Intercept)`,
                                               rasch_true_load_oneRM = item_difficulties_rasch_1000_10_wide$item_true_scale_estimate_oneRM,
                                               raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_10_wide$item_true_scale_estimate_oneRM,
                                               raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_10_wide$item_true_scale_estimate_oneRM,
                                               rasch_true_load_item = item_difficulties_rasch_1000_10_wide$item_true_scale_estimate_load,
                                               raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_10_wide$item_true_scale_estimate_load,
                                               raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_10_wide$item_true_scale_estimate_load)

oneRM_dat_1000_10_wide <- left_join(oneRM_dat_1000_10_wide, person_abilities_1000_10_wide, by="person") %>%
  left_join(item_difficulties_1000_10_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "10",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)

person_abilities_1000_20_wide <- data.frame(person = unique(oneRM_dat_1000_20_wide$person),
                                              rasch_ability = person_abilities_rasch_1000_20_wide$`(Intercept)`, 
                                              raschhigherror_ability = person_abilities_raschhigherror_1000_20_wide$`(Intercept)`, 
                                              raschlowerror_ability = person_abilities_raschlowerror_1000_20_wide$`(Intercept)`,
                                              rasch_true_oneRM_oneRM = person_abilities_rasch_1000_20_wide$person_true_scale_estimate_oneRM,
                                              raschhigherror_true_oneRM_oneRM = person_abilities_raschhigherror_1000_20_wide$person_true_scale_estimate_oneRM,
                                              raschlowerror_true_oneRM_oneRM = person_abilities_raschlowerror_1000_20_wide$person_true_scale_estimate_oneRM,
                                              rasch_true_oneRM_item = person_abilities_rasch_1000_20_wide$person_true_scale_estimate_load,
                                              raschhigherror_true_oneRM_item = person_abilities_raschhigherror_1000_20_wide$person_true_scale_estimate_load,
                                              raschlowerror_true_oneRM_item = person_abilities_raschlowerror_1000_20_wide$person_true_scale_estimate_load)

item_difficulties_1000_20_wide <- data.frame(item = unique(oneRM_dat_1000_20_wide$item),
                                               rasch_difficulty = item_difficulties_rasch_1000_20_wide$`(Intercept)`, 
                                               raschhigherror_difficulty = item_difficulties_raschhigherror_1000_20_wide$`(Intercept)`, 
                                               raschlowerror_difficulty = item_difficulties_raschlowerror_1000_20_wide$`(Intercept)`,
                                               rasch_true_load_oneRM = item_difficulties_rasch_1000_20_wide$item_true_scale_estimate_oneRM,
                                               raschhigherror_true_load_oneRM = item_difficulties_raschhigherror_1000_20_wide$item_true_scale_estimate_oneRM,
                                               raschlowerror_true_load_oneRM = item_difficulties_raschlowerror_1000_20_wide$item_true_scale_estimate_oneRM,
                                               rasch_true_load_item = item_difficulties_rasch_1000_20_wide$item_true_scale_estimate_load,
                                               raschhigherror_true_load_item = item_difficulties_raschhigherror_1000_20_wide$item_true_scale_estimate_load,
                                               raschlowerror_true_load_item = item_difficulties_raschlowerror_1000_20_wide$item_true_scale_estimate_load)

oneRM_dat_1000_20_wide <- left_join(oneRM_dat_1000_20_wide, person_abilities_1000_20_wide, by="person") %>%
  left_join(item_difficulties_1000_20_wide, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschhigherror = if_else(raschhigherror_true_oneRM_oneRM >= raschhigherror_true_load_oneRM, raschhigherror_true_load_oneRM/raschhigherror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschhigherror = if_else(raschhigherror_true_oneRM_item >= raschhigherror_true_load_item, raschhigherror_true_load_item/raschhigherror_true_oneRM_item*100, 100),
         irt_effort_odds_raschhigherror = if_else(exp(raschhigherror_ability) >= exp(raschhigherror_difficulty), (exp(raschhigherror_difficulty)/exp(raschhigherror_ability))*100, 100),
         irt_effort_logit_diff_raschhigherror = if_else(raschhigherror_ability >= raschhigherror_difficulty, raschhigherror_ability - raschhigherror_difficulty, 0)*-1,
         raschhigherror_difficulty_logit_shift = raschhigherror_difficulty + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         raschhigherror_ability_logit_shift = raschhigherror_ability + (0 - if_else(min(raschhigherror_ability) < min(raschhigherror_difficulty), min(raschhigherror_ability), min(raschhigherror_difficulty))),
         irt_effort_logit_shift_raschhigherror = if_else(raschhigherror_ability_logit_shift >= raschhigherror_difficulty_logit_shift, raschhigherror_difficulty_logit_shift/raschhigherror_ability_logit_shift*100, 100),
         irt_effort_meansigma_oneRM_raschlowerror = if_else(raschlowerror_true_oneRM_oneRM >= raschlowerror_true_load_oneRM, raschlowerror_true_load_oneRM/raschlowerror_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_raschlowerror = if_else(raschlowerror_true_oneRM_item >= raschlowerror_true_load_item, raschlowerror_true_load_item/raschlowerror_true_oneRM_item*100, 100),
         irt_effort_odds_raschlowerror = if_else(exp(raschlowerror_ability) >= exp(raschlowerror_difficulty), (exp(raschlowerror_difficulty)/exp(raschlowerror_ability))*100, 100),
         irt_effort_logit_diff_raschlowerror = if_else(raschlowerror_ability >= raschlowerror_difficulty, raschlowerror_ability - raschlowerror_difficulty, 0)*-1,
         raschlowerror_difficulty_logit_shift = raschlowerror_difficulty + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         raschlowerror_ability_logit_shift = raschlowerror_ability + (0 - if_else(min(raschlowerror_ability) < min(raschlowerror_difficulty), min(raschlowerror_ability), min(raschlowerror_difficulty))),
         irt_effort_logit_shift_raschlowerror = if_else(raschlowerror_ability_logit_shift >= raschlowerror_difficulty_logit_shift, raschlowerror_difficulty_logit_shift/raschlowerror_ability_logit_shift*100, 100)) %>%
  mutate(sample_size = "1000",
         item_number = "20",
         range = "wide") %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


# Combine all effort derivations and plot

oneRM_dat_all_efforts <- rbind(oneRM_dat_50_5_narrow,
                               oneRM_dat_50_10_narrow,
                               oneRM_dat_50_20_narrow,
                               oneRM_dat_100_5_narrow,
                               oneRM_dat_100_10_narrow,
                               oneRM_dat_100_20_narrow,
                               oneRM_dat_250_5_narrow,
                               oneRM_dat_250_10_narrow,
                               oneRM_dat_250_20_narrow,
                               oneRM_dat_500_5_narrow,
                               oneRM_dat_500_10_narrow,
                               oneRM_dat_500_20_narrow,
                               oneRM_dat_1000_5_narrow,
                               oneRM_dat_1000_10_narrow,
                               oneRM_dat_1000_20_narrow,
                               oneRM_dat_50_5_wide,
                               oneRM_dat_50_10_wide,
                               oneRM_dat_50_20_wide,
                               oneRM_dat_100_5_wide,
                               oneRM_dat_100_10_wide,
                               oneRM_dat_100_20_wide,
                               oneRM_dat_250_5_wide,
                               oneRM_dat_250_10_wide,
                               oneRM_dat_250_20_wide,
                               oneRM_dat_500_5_wide,
                               oneRM_dat_500_10_wide,
                               oneRM_dat_500_20_wide,
                               oneRM_dat_1000_5_wide,
                               oneRM_dat_1000_10_wide,
                               oneRM_dat_1000_20_wide) %>%
  mutate(sample_size = factor(sample_size, levels = c("50","100","250","500","1000")),
         item_number = factor(item_number, levels = c("5","10","20")),
         range = dplyr::recode(range, narrow = "Narrow Item Range (60 to 140 kg)", wide = "Wide Item Range (10 to 200 kg)"),
         sample_size_lab = "Sample Size",
         item_number_lab = "Number of Items")

meansigma_oneRM_effort_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_oneRM_rasch, irt_effort_meansigma_oneRM_raschhigherror, irt_effort_meansigma_oneRM_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)")) %>%
  ggplot(aes(x=actual_effort, y=meansigma_effort)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_point(alpha=0.25, size = 0.25) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (Mean-Sigma Strength Anchor Derivation; %)") +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(meansigma_oneRM_effort_plot, file = "plots/meansigma_oneRM_effort_plot")

meansigma_oneRM_effort_plot

ggsave("plots/meansigma_oneRM_effort_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

mean_deviations_meansigma_oneRM <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_oneRM_rasch, irt_effort_meansigma_oneRM_raschhigherror, irt_effort_meansigma_oneRM_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_meansigma_oneRM = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  group_by(sample_size_lab, sample_size, item_number_lab, item_number, range, response_type) %>%
  add_count() %>%
  summarise(mean_deviation_meansigma_oneRM = sum(deviation_meansigma_oneRM)/n) %>%
  filter(row_number() == 1)

save(mean_deviations_meansigma_oneRM, file = "models/mean_deviations_meansigma_oneRM")

effort_deviation_meansigma_oneRM_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_oneRM_rasch, irt_effort_meansigma_oneRM_raschhigherror, irt_effort_meansigma_oneRM_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_meansigma_oneRM = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  ggplot(aes(x=actual_effort, y=deviation_meansigma_oneRM*100)) +
  geom_point(size=0.25, alpha=0.25)  +
  labs(x = "True Effort (%)",
       y = "Deviation of Estimated Effort (Mean-Sigma Strength Anchor Derivation; %)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = mean_deviations_meansigma_oneRM, 
            aes(x=50, y=400, label = paste("MADP = ", round(mean_deviation_meansigma_oneRM*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(effort_deviation_meansigma_oneRM_plot, file = "plots/effort_deviation_meansigma_oneRM_plot")

effort_deviation_meansigma_oneRM_plot

ggsave("plots/effort_deviation_meansigma_oneRM_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

meansigma_item_effort_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_item_rasch, irt_effort_meansigma_item_raschhigherror, irt_effort_meansigma_item_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)")) %>%
  ggplot(aes(x=actual_effort, y=meansigma_effort)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_point(alpha=0.25, size = 0.25) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (Mean-Sigma Load Anchor Derivation; %)") +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(meansigma_item_effort_plot, file = "plots/meansigma_item_effort_plot")

meansigma_item_effort_plot

ggsave("plots/meansigma_item_effort_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

mean_deviations_meansigma_item <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_item_rasch, irt_effort_meansigma_item_raschhigherror, irt_effort_meansigma_item_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_meansigma_item = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  group_by(sample_size_lab, sample_size, item_number_lab, item_number, range, response_type) %>%
  add_count() %>%
  summarise(mean_deviation_meansigma_item = sum(deviation_meansigma_item)/n) %>%
  filter(row_number() == 1)

save(mean_deviations_meansigma_item, file = "models/mean_deviations_meansigma_item")

effort_deviation_meansigma_item_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_meansigma_item_rasch, irt_effort_meansigma_item_raschhigherror, irt_effort_meansigma_item_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_meansigma_item = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  ggplot(aes(x=actual_effort, y=deviation_meansigma_item*100)) +
  geom_point(size=0.25, alpha=0.25)  +
  labs(x = "True Effort (%)",
       y = "Deviation of Estimated Effort (Mean-Sigma Load Anchor Derivation; %)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = mean_deviations_meansigma_item, 
            aes(x=50, y=400, label = paste("MADP = ", round(mean_deviation_meansigma_item*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(effort_deviation_meansigma_item_plot, file = "plots/effort_deviation_meansigma_item_plot")

effort_deviation_meansigma_item_plot

ggsave("plots/effort_deviation_meansigma_item_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

odds_effort_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_odds_rasch, irt_effort_odds_raschhigherror, irt_effort_odds_raschlowerror), names_to = "response_type", values_to = "odds_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)")) %>%
  ggplot(aes(x=actual_effort, y=odds_effort)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_point(alpha=0.25, size = 0.25) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (Odds Derivation; %)") +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(odds_effort_plot, file = "plots/odds_effort_plot")

odds_effort_plot

ggsave("plots/odds_effort_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

mean_deviations_odds <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_odds_rasch, irt_effort_odds_raschhigherror, irt_effort_odds_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_odds = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  group_by(sample_size_lab, sample_size, item_number_lab, item_number, range, response_type) %>%
  add_count() %>%
  summarise(mean_deviation_odds = sum(deviation_odds)/n) %>%
  filter(row_number() == 1)

save(mean_deviations_odds, file = "models/mean_deviations_odds")

effort_deviation_odds_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_odds_rasch, irt_effort_odds_raschhigherror, irt_effort_odds_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_odds = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  ggplot(aes(x=actual_effort, y=deviation_odds*100)) +
  geom_point(size=0.25, alpha=0.25)  +
  labs(x = "True Effort (%)",
       y = "Deviation of Estimated Effort (Odds Derivation; %)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = mean_deviations_odds, 
            aes(x=50, y=500, label = paste("MADP = ", round(mean_deviation_odds*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(effort_deviation_odds_plot, file = "plots/effort_deviation_odds_plot")

effort_deviation_odds_plot

ggsave("plots/effort_deviation_odds_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

logit_diff_effort_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_logit_diff_rasch, irt_effort_logit_diff_raschhigherror, irt_effort_logit_diff_raschlowerror), names_to = "response_type", values_to = "logit_diff_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)")) %>%
  ggplot(aes(x=actual_effort, y=logit_diff_effort)) +
  geom_point(alpha=0.25, size = 0.25) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (Logit Difference Derivation; logits)") +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(logit_diff_effort_plot, file = "plots/logit_diff_effort_plot")

logit_diff_effort_plot

ggsave("plots/logit_diff_effort_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

logit_shift_effort_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_logit_shift_rasch, irt_effort_logit_shift_raschhigherror, irt_effort_logit_shift_raschlowerror), names_to = "response_type", values_to = "logit_shift_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)")) %>%
  ggplot(aes(x=actual_effort, y=logit_shift_effort)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_point(alpha=0.25, size = 0.25) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (Logit Shift Derivation; %)") +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(logit_shift_effort_plot, file = "plots/logit_shift_effort_plot")

logit_shift_effort_plot

ggsave("plots/logit_shift_effort_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)

mean_deviations_logit_shift <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_logit_shift_rasch, irt_effort_logit_shift_raschhigherror, irt_effort_logit_shift_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_logit_shift = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  group_by(sample_size_lab, sample_size, item_number_lab, item_number, range, response_type) %>%
  add_count() %>%
  summarise(mean_deviation_logit_shift = sum(deviation_logit_shift)/n) %>%
  filter(row_number() == 1)

save(mean_deviations_logit_shift, file = "models/mean_deviations_logit_shift")

effort_deviation_logit_shift_plot <- oneRM_dat_all_efforts %>%
  pivot_longer(c(irt_effort_logit_shift_rasch, irt_effort_logit_shift_raschhigherror, irt_effort_logit_shift_raschlowerror), names_to = "response_type", values_to = "meansigma_effort") %>%
  separate(col = response_type, into = c("x", "y", "z", "a", "response_type"), sep = "_", remove = TRUE) %>%
  mutate(response_type = factor(response_type, levels = c("raschlowerror", "rasch", "raschhigherror")),
         response_type = dplyr::recode(response_type, raschlowerror = "Low Error (?? = 5)", rasch = "Typical Error (?? = 1)", raschhigherror = "High Error (?? = 0.1)"),
         deviation_logit_shift = abs((actual_effort - meansigma_effort) / actual_effort )) %>%
  ggplot(aes(x=actual_effort, y=deviation_logit_shift*100)) +
  geom_point(size=0.25, alpha=0.25)  +
  labs(x = "True Effort (%)",
       y = "Deviation of Estimated Effort (Logit Shift Derivation; %)") +
  scale_y_continuous(limits = c(0,550)) +
  geom_text(data = mean_deviations_logit_shift, 
            aes(x=50, y=500, label = paste("MADP = ", round(mean_deviation_logit_shift*100, digits=2),"%")),
            size = 2) +
  facet_nested(sample_size_lab + sample_size + item_number_lab + item_number ~ response_type + range) +
  theme_bw() +
  theme(strip.text.x = element_text(size=6.5),
        panel.grid = element_blank(),
        legend.position="bottom")

save(effort_deviation_logit_shift_plot, file = "plots/effort_deviation_logit_shift_plot")

effort_deviation_logit_shift_plot

ggsave("plots/effort_deviation_logit_shift_plot.png", width = 12.5, height = 15, device = "png", dpi = 300)








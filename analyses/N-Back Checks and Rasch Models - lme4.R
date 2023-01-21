##### Analysis Script for "Can we measure effort in cognitive tasks?: Examining the application of Additive Conjoint Measurement and the Rasch model"

# Author: James Steele

##### In this script we fit look at a real dataset compiled from several studies that employed N-back tasks
### Checks for Guttman patterns and cancellation are first performed
### Then Rasch models are fit and the ability to recover the integer scale of the N-back items is examined
### Lastly the various effort operationalisations are examined, and in some of the data compared with perception of effort

### Open packages
### Open required packages
library(tidyverse)
library(ConjointChecks)
library(R.utils)
library(lme4)
library(rstan)
library(ordbetareg)
library(patchwork)
library(tidybayes)

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

# run rstan quicker - for bayesian beta regression later on
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)

##### We'll explore this idea in a real dataset now from the open data of several studies

### Gamer's study Blacker and Curby - https://link.springer.com/article/10.3758/s13414-013-0487-0 
nback_gamers <- read.csv("datasets/Blacker and Curby/nback_gamers.csv") %>%
  select(person, item, response, trial) %>%
  mutate(response = if_else(response == 1, 1, 0))
  

### Arithmetical Strategy Selection data from Tiberhien et al. https://jnc.psychopen.eu/index.php/jnc/article/view/5767
nback_AX_CPT <- read.csv("datasets/Tiberghien et al/nback_AX_CPT.csv") 

nback_AX_CPT <- nback_AX_CPT %>%
  rename(trial = "X",
         person = "Sub",
         response = "Stimulus.ACC") %>%
  filter(Condition == "Uncued") %>%
  mutate(item = 3,
         data = "AX_CPT") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial)

### MAUS data - https://arxiv.org/abs/2111.02561
# load and prepare data
nback_MAUS <- read.csv("datasets/MAUS/Response data csv/nback_dat_MAUS.csv") 

nback_MAUS_1 <- nback_MAUS %>% 
  mutate(response = if_else(b1.Rsp == b1.CRsp, 0, 1), # Response numbering reversed in their raw data it seems as otherwise everyone was getting the 0-back wrong a lot
         item = "0",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS_2 <- nback_MAUS %>% 
  mutate(response = if_else(b2.Rsp == b2.CRsp, 0, 1),
         item = "2",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS_3 <- nback_MAUS %>% 
  mutate(response = if_else(b3.Rsp == b3.CRsp, 0, 1),
         item = "3",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS_4 <- nback_MAUS %>% 
  mutate(response = if_else(b4.Rsp == b4.CRsp, 0, 1),
         item = "2",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS_5 <- nback_MAUS %>% 
  mutate(response = if_else(b5.Rsp == b5.CRsp, 0, 1),
         item = "3",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS_6 <- nback_MAUS %>% 
  mutate(response = if_else(b6.Rsp == b6.CRsp, 0, 1),
         item = "0",
         person = id) %>%
  group_by(person) %>%
  mutate(trial = seq(1:101)) %>%
  select(person, item, response, trial) %>%
  ungroup()

nback_MAUS <- rbind(nback_MAUS_1, nback_MAUS_2, nback_MAUS_3, nback_MAUS_4, nback_MAUS_5, nback_MAUS_6) %>%
  filter(!is.na(response)) %>%
  mutate(data = "MAUS") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial)

NASA_MAUS <- read.csv("datasets/MAUS/NASA-TLX data/NASA_dat_MAUS.csv") %>%
  filter(Scale.Title == "Effort") %>%
  select(2,4:9) %>%
  mutate(`Trial.1..0_back` = as.numeric(`Trial.1..0_back`)) %>%
  pivot_longer(as.numeric(2:7), names_to = "trial", values_to = "perception_effort") %>%
  separate(trial, c("x", "y", "item", "z")) %>%
  mutate(perception_effort = perception_effort/100,
         person = id,
         data = "MAUS") %>%
  unite("person", c(data,person)) %>%
  select(person, item, perception_effort)

### Westbrook et al. 2013 data - https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068210
# load and prepare data
nback_a_dat <- read.csv("datasets/Westbrook et al./plos_one/2013_Nback_a.csv") 
nback_b_dat <- read.csv("datasets/Westbrook et al./plos_one/2013_Nback_b.csv") 

nback_Westbrook <- rbind(nback_a_dat, nback_b_dat) %>%
  rename("person" = Subject,
         "item" = stimcol,
         "response" = TrialAcc) %>%
  mutate(person = factor(person),
         item = factor(item)) %>%
  filter(person != "135" &
           person != "145") %>%  # both did different numbers of trials for each n-back item
  select(person, item, response, trial)  %>%
  mutate(data = "Westbrook") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial)

nback_Westbrook$item <- recode(nback_Westbrook$item, "black" = "1", "red" = "2", "blue" = "3", "purple" = "4", "yellow" = "5", "brown" = "6") 

NASA_Westbrook <- read.csv("datasets/Westbrook et al./plos_one/NASA_TLX.csv")

NASA_Westbrook <- NASA_Westbrook %>%
  rename("person" = Participant,
         "item" = Task) %>%
  mutate(person = factor(person),
         item = factor(item),
         Effort = if_else(Effort == 105, 10.5, Effort),
         perception_effort = (Effort/20.5),
         data = "Westbrook") %>%
  unite("person", c(data,person)) %>%
  select(person, item, perception_effort)

NASA_Westbrook$item <- recode(NASA_Westbrook$item, "BLK" = "1", "RED" = "2", "BLU" = "3", "PRP" = "4")

NASA_Westbrook <- NASA_Westbrook %>%
  filter(!is.na(perception_effort)) %>%
  mutate(item = as.numeric(item)-1)

### Westbrook et al. 2020 data - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7430502/
# load data
nback_drug_trial <- read.csv("datasets/Westbrook et al./drug_trial/nback_drug_trial.csv") %>%
  mutate(item = as.character(item),
         id = as.character(id)) %>%
  rename("response" = resp) %>%
  select(-person) %>%
  rename("person" = id) %>%
  mutate(data = "drug_trial") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial)


### Zerna et al. data pilot data from  stage 1 registered report - https://psyarxiv.com/ysh3q
nback_Zerna <- read.csv("datasets/Zerna et al/nback_Zerna.csv")  %>% 
  rename("person" = Participant,
         "item" = currentlevel,
         "response" = trial_resp.corr,
         "trial" = nback_run.thisIndex) %>%
  mutate(person = factor(person),
         item = factor(item),
         data = "Zerna") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial)

##### Combine all datatsets

nback_dat <- rbind(nback_AX_CPT, nback_gamers, nback_Westbrook, nback_MAUS, nback_Zerna, nback_drug_trial) %>%
  mutate(item_plus = as.numeric(as.character(item)) + 1) # for including the 0-back in example analysis

NASA_dat <- rbind(NASA_Westbrook, NASA_MAUS) %>%
  mutate(item = as.numeric(as.character(item)))

##### Check for Guttman pattern (i.e., Coefficient of Reproducibility) and whether axioms of additive conjoint measurement are met
### For this we just use the Westbrook et al. PloS One and Drug Trial data, and Zerna et al pilot data as they used 1-4 back tasks giving a wider range of total score possibilities

#### Examine the first 30 "Target" trials
# 128 total trials (i.e., 2 blocks of 64) for Westbrook et al. PloS One and Zerna who performed 1-4 back
# 16 trials of each block were "Target" trials
# The Westbrook et al. drug trial used >=30 "Target" trials so we look at the first 30 across all to maximise data for cancellation checks

# Reload the data for checks including the target kind
nback_Westbrook_checks <- rbind(nback_a_dat, nback_b_dat) %>%
  rename("person" = Subject,
         "item" = stimcol,
         "response" = TrialAcc) %>%
  mutate(person = factor(person),
         item = factor(item),
         data = "Westbrook") %>%
  unite("person", c(data,person)) %>%
  filter(person != "135" &
           person != "145") %>% # both did different numbers of trials for each n-back item
  select(person, item, response, trial, kind) %>%
  group_by(person, item) %>%
  filter(row_number() <= 128)

nback_Westbrook_checks$item <- recode(nback_Westbrook_checks$item, "black" = "1", "red" = "2", "blue" = "3", "purple" = "4", "yellow" = "5", "brown" = "6") 

nback_Zerna_checks <- read.csv("datasets/Zerna et al/nback_Zerna.csv")  %>% 
  rename("person" = Participant,
         "item" = currentlevel,
         "response" = trial_resp.corr,
         "trial" = nback_run.thisIndex,
         "kind" = Stimulus) %>%
  mutate(person = factor(person),
         item = factor(item),
         data = "Zerna") %>%
  unite("person", c(data,person)) %>%
  select(person, item, response, trial, kind) %>%
  group_by(person, item) %>%
  filter(row_number() <= 128)

nback_drug_trial_checks <- read.csv("datasets/Westbrook et al./drug_trial/nback_drug_trial.csv") %>%
  mutate(item = as.character(item),
         id = as.character(id),
         data = "drug_trial") %>%
  unite("person", c(data,person)) %>%
  rename("response" = resp) %>%
  select(-person) %>%
  rename("person" = id) %>%
  select(person, item, response, trial, kind)

nback_checks <- rbind(nback_Westbrook_checks, nback_Zerna_checks, nback_drug_trial_checks) %>%
  filter(item != "0"  & item != "5" & item != "6") 

### empty df
checks_out <- data.frame(
  i = as.numeric(),
  CR = as.numeric(),
  unweighted =  as.numeric(),
  weighted =  as.numeric()
)

### Run ConjointChecks quicker
cores <- detectCores()/2

##### Each check for only target trials
# Note, attempted both for loop (and foreach loop) and vectorised function, but kept running into issues with tryCatch
# Some trials do not result in sufficient rows (i.e., total scores) to produce sufficient matrix size for cancellation checks
# Thus the tryCatch wrapper catches errors and timeouts which occur for trials where this is the case when running Conjoint Checks

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 1) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 1,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 2) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 2,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 3) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 3,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 4) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 4,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 5) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 5,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 6) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 6,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 7) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 7,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 8) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 8,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 9) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 9,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 10) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 10,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 11) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 11,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 12) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 12,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 13) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 13,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 14) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 14,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 15) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 15,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 16) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 


checks_out <- rbind(checks_out, data.frame(
  i = 16,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 17) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 17,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 18) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 18,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 19) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 19,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 20) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 20,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 21) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 21,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 22) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 22,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 23) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 23,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 24) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 24,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 25) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 25,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 26) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 26,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 27) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 27,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 28) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 28,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 29) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 29,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

resp_rasch_nback <- nback_checks %>%
  filter(kind == "Target") %>%
  select(person, item, response) %>%
  group_by(person, item) %>%
  filter(row_number() == 30) %>% # just the ith row i.e., trial
  pivot_wider(values_from = "response", names_from = "item") %>%
  filter(!is.na(`1`) & !is.na(`2`) & !is.na(`3`) & !is.na(`4`)) %>%
  select(`1`,`2`,`3`,`4`)

resp_mat_nback <- as.matrix(resp_rasch_nback[2:5]) 

c_r_resp_rasch_nback  <- CR(resp_mat_nback)

prep_rasch_nback <- PrepareChecks(resp_mat_nback)

prep_rasch_nback_mat <- prep_rasch_nback$n/prep_rasch_nback$N

n.3mats <- if_else(dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3 < 5000, dim(prep_rasch_nback$n)[1]^3*dim(prep_rasch_nback$n)[2]^3, 5000)

c_check_rasch_nback <- tryCatch(
  expr = {
    withTimeout(ConjointChecks(N=prep_rasch_nback$N, n=prep_rasch_nback$n, n.3mat = n.3mats,
                               par.options=list(n.workers=cores,type="PSOCK")), timeout = 120)
  }, 
  TimeoutException = function(x){NULL},
  error = function(e) {NULL}
)

if(is.null(c_check_rasch_nback)) {
  unweighted <- NA_real_
  weighted <- NA_real_ 
} else {
  unweighted <- summary(c_check_rasch_nback)$Means$unweighted
  weighted <- summary(c_check_rasch_nback)$Means$weighted
} 

checks_out <- rbind(checks_out, data.frame(
  i = 30,
  CR = c_r_resp_rasch_nback,
  unweighted =  unweighted,
  weighted =  weighted
)
)

# Save CR and ACM checks
save(checks_out, file = "datasets/nback_acm_checks_target")

### Plot both the CR and cancellation checks
nback_cr_plot <- checks_out %>%
  ggplot(aes(x = 1, y = CR)) +
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.8, position = position_jitter(w=0.01)) +
  labs(x = "",
       y = bquote(italic(C[R])),
       color = "Mean Weighting") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

save(nback_cr_plot, file = "plots/nback_cr_plot")

nback_violations_means_plot <- checks_out %>%
  pivot_longer(3:4, values_to = "violations", names_to = "weighted_unweighted") %>%
  ggplot(aes(x = 1, y = violations)) +
  geom_abline(intercept = 0.05, slope = 0, linetype = "dashed", color = "red") +
  geom_point(aes(color = weighted_unweighted), alpha = 0.8, position = position_jitter(w=0.01)) +
  scale_fill_manual("Mean Weighting", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Mean Weighting", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "",
       y = "Proportion of Violations") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

save(nback_violations_means_plot, file = "plots/nback_violations_means_plot")

(nback_cr_plot + nback_violations_means_plot) + plot_annotation(tag_levels = "A")

ggsave("plots/nback_cr_checks_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)

##### We now fit the Rasch models to the full dataset

### One study includes the 0-back item, but this seems to impact the scalability as the 0-back is so easy
          # Fit the Rasch model (including the 0-back item)
          glmer_rasch_nback_zero <- lme4::glmer(response ~ 1 + (1 | item) + (1 | person), 
                                           data = nback_dat, 
                                           family = binomial(link = "logit"))
          
          save(glmer_rasch_nback_zero, file = "models/glmer_rasch_nback_zero")
          
          # Extract person and item parameters and calculate mean-sigma transformation based on items (using item plus as log(zero) is Inf)
          person_abilities_rasch_nback_zero <- as.data.frame(coef(glmer_rasch_nback_zero)$person)
          
          person_abilities_rasch_nback_zero <- person_abilities_rasch_nback_zero %>%
            mutate(person_true_scale_estimate = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(unique(nback_dat$item_plus))), na.rm = TRUE)+mean(log(as.numeric(unique(nback_dat$item_plus))), na.rm = TRUE)),2)) %>%
            rownames_to_column("person")
          
          item_difficulties_rasch_nback_zero <- as.data.frame(coef(glmer_rasch_nback_zero)$item)
          
          item_difficulties_rasch_nback_zero <- item_difficulties_rasch_nback_zero %>% 
            mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
                   item_true_scale_estimate = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(unique(nback_dat$item_plus))), na.rm = TRUE)+mean(log(as.numeric(unique(nback_dat$item_plus))), na.rm = TRUE)),2)) %>%
            rownames_to_column("item") %>%
            mutate(item = as.numeric(as.character(item)))
          
          ### Item fit
          # First, we need fitted and residual values
          nback_dat$fitted <- fitted(glmer_rasch_nback_zero)
          nback_dat$resid <- resid(glmer_rasch_nback_zero, type = "response")
          
          # To calculate outfit MSQ:
          nback_dat$Outfit <- (nback_dat$resid ^ 2) /
            (nback_dat$fitted * (1 - nback_dat$fitted))
          
          # Summarize it by item using mean
          item_difficulties_rasch_nback_zero$Outfit <- summarize(group_by(nback_dat, item),
                                                            Outfit = mean(Outfit))$Outfit
          
          # To calculate infit MSQ:
          item_difficulties_rasch_nback_zero$Infit <- summarize(group_by(nback_dat, item), Infit = sum(resid ^ 2) /
                                                             sum(fitted * (1 - fitted)))$Infit
          
          nback_item_fit_plot_zero <- item_difficulties_rasch_nback_zero %>%
            pivot_longer(4:5, names_to = "Fit Statistic", values_to = "Mean-Square") %>%
            ggplot(aes(x=item, y=`Mean-Square`)) +
            geom_hline(yintercept = c(0.5,1.5), linetype = "dashed", color="red") +
            geom_point() +
            scale_x_continuous(limits = c(0,6), breaks = c(0,1,2,3,4,5,6)) +
            labs(x = "Item (N-Back Level)",
                 title = "Item Fit Statistics") +
            facet_grid(.~`Fit Statistic`) +
            theme_bw() + 
            theme(panel.grid = element_blank())
          
          ### Person fit
          # Person outfit MSQ:
          person_abilities_rasch_nback_zero$Outfit <- summarize(group_by(nback_dat, person), Outfit = mean(Outfit))$Outfit
          
          # Person infit MSQ:
          person_abilities_rasch_nback_zero$Infit <- summarize(group_by(nback_dat, person), Infit = sum(resid ^ 2) /
                                                            sum(fitted * (1 - fitted)))$Infit
          
          
          nback_person_fit_plot_zero <- person_abilities_rasch_nback_zero %>%
            pivot_longer(4:5, names_to = "Fit Statistic", values_to = "Mean-Square") %>%
            ggplot(aes(x=person, y=`Mean-Square`)) +
            geom_hline(yintercept = c(0.5,1.5), linetype = "dashed", color="red") +
            geom_point() +
            labs(x = "Person",
                 title = "Person Fit Statistics") +
            facet_grid(.~`Fit Statistic`) +
            theme_bw() + 
            theme(panel.grid = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
          
          nback_fit_plot_zero <- (nback_item_fit_plot_zero + nback_person_fit_plot_zero)
          
          save(nback_fit_plot_zero, file = "plots/nback_fit_plot_zero")
          
          nback_fit_plot_zero
          
          ggsave("plots/nback_fit_plot_zero.png", width = 10, height = 3.75, device = "png", dpi = 300)
          
          # Recombine with data and plot the item difficulty estimates with the true difficulty (i.e., N)
          nback_dat_zero <- nback_dat %>%
            mutate(item = as.numeric(item)) %>%
            left_join(person_abilities_rasch_nback_zero, by="person") %>%
            left_join(item_difficulties_rasch_nback_zero, by="item") %>%
            rename(ability = `(Intercept).x`,
                   difficulty = `(Intercept).y`)
          
          true2logit_plot_zero <- nback_dat_zero %>%
            group_by(item) %>%
            filter(row_number() == 1) %>%
            ggplot(aes(x=item, y=difficulty)) +
            geom_point() +
            labs(x = "True Difficulty (N-Back)",
                 y = "Difficulty (logits)") +
            scale_x_continuous(breaks = c(0,1,2,3,4,5,6)) +
            theme_bw() + 
            theme(panel.grid = element_blank())
          
          save(true2logit_plot_zero, file = "plots/true2logit_plot_zero")
          
          # Calculate the deviation between the mean-sigma transformation with the true difficulty (i.e., N) and plot
          nback_difficulties_rasch_nback_zero <- nback_dat_zero %>%
            group_by(item) %>%
            filter(row_number() == 1) %>%
            mutate(true_difficulty = as.numeric(as.character(item))+1, # add one to match the true scale estimate with item_plus
                   true_difficulty_estimate = item_true_scale_estimate,
                   parameter = "item") %>%
            ungroup() %>%
            dplyr::select(true_difficulty, true_difficulty_estimate, parameter)
          
          mean_deviation_zero <- nback_difficulties_rasch_nback_zero %>%
            mutate(deviation = abs((true_difficulty - true_difficulty_estimate) / true_difficulty)) %>%
            summarise(mean_deviation = mean(deviation, na.rm=TRUE))
          
          true2estimate_scales_plot_zero <- nback_difficulties_rasch_nback_zero %>%
            ggplot(aes(x=true_difficulty-1, y=true_difficulty_estimate-1)) +
            geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
            geom_point() +
            geom_text(data = mean_deviation_zero,
                      aes(x=3, y=4, label = paste("Item MADP = ", round(mean_deviation*100, digits=2),"%")),
                      size = 2) +
            labs(x = "True Difficulty (N-Back)",
                 y = "Estimated Difficulty (N-Back)") +
            scale_x_continuous(breaks = c(0,1,2,3,4,5,6)) +
            scale_y_continuous(limits = c(-0.5,6), breaks = c(0,1,2,3,4,5,6)) +
            theme_bw() + 
            theme(panel.grid = element_blank())
          
          save(true2estimate_scales_plot_zero, file = "plots/true2estimate_scales_plot_zero")
          
          (true2logit_plot_zero + true2estimate_scales_plot_zero) +
            plot_annotation(title = "Scale recovery for N-back Task when including the 0-back item")
          
          ggsave("plots/nback_true2logit_zero_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)

### So we exclude the 0-back item and fit our Rasch model          

# Fit the Rasch model (excluding the 0-back item)
nback_dat <- nback_dat %>%
  filter(item != "0")

glmer_rasch_nback <- lme4::glmer(response ~ 1 + (1 | item) + (1 | person), 
                                 data = nback_dat, 
                                 family = binomial(link = "logit"))

save(glmer_rasch_nback, file = "models/glmer_rasch_nback")

# Extract person and item parameters and calculate mean-sigma transformation based on items
person_abilities_rasch_nback <- as.data.frame(coef(glmer_rasch_nback)$person)

person_abilities_rasch_nback <- person_abilities_rasch_nback %>%
  mutate(person_true_scale_estimate = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(unique(nback_dat$item))), na.rm = TRUE)+mean(log(as.numeric(unique(nback_dat$item))), na.rm = TRUE)),2)) %>%
  rownames_to_column("person")

item_difficulties_rasch_nback <- as.data.frame(coef(glmer_rasch_nback)$item)

item_difficulties_rasch_nback <- item_difficulties_rasch_nback %>% 
  mutate(`(Intercept)` = -1*`(Intercept)`, # convert 'easiness' to 'difficulty' 
         item_true_scale_estimate = round(exp(((`(Intercept)`-mean(`(Intercept)`))/sd(`(Intercept)`))*sd(log(as.numeric(unique(nback_dat$item))), na.rm = TRUE)+mean(log(as.numeric(unique(nback_dat$item))), na.rm = TRUE)),2)) %>%
  rownames_to_column("item") %>%
  mutate(item = as.numeric(as.character(item)))

### Item fit
# First, we need fitted and residual values
nback_dat$fitted <- fitted(glmer_rasch_nback)
nback_dat$resid <- resid(glmer_rasch_nback, type = "response")

# To calculate outfit MSQ:
nback_dat$Outfit <- (nback_dat$resid ^ 2) /
  (nback_dat$fitted * (1 - nback_dat$fitted))

# Summarize it by item using mean
item_difficulties_rasch_nback$Outfit <- summarize(group_by(nback_dat, item),
                                                   Outfit = mean(Outfit))$Outfit

# To calculate infit MSQ:
item_difficulties_rasch_nback$Infit <- summarize(group_by(nback_dat, item), Infit = sum(resid ^ 2) /
                                                    sum(fitted * (1 - fitted)))$Infit

nback_item_fit_plot <- item_difficulties_rasch_nback %>%
  pivot_longer(4:5, names_to = "Fit Statistic", values_to = "Mean-Square") %>%
  ggplot(aes(x=item, y=`Mean-Square`)) +
  geom_hline(yintercept = c(0.5,1.5), linetype = "dashed", color="red") +
  geom_point() +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6)) +
  labs(x = "Item (N-Back Level)",
       title = "Item Fit Statistics") +
  facet_grid(.~`Fit Statistic`) +
  theme_bw() + 
  theme(panel.grid = element_blank())

### Person fit
# Person outfit MSQ:
person_abilities_rasch_nback$Outfit <- summarize(group_by(nback_dat, person), Outfit = mean(Outfit))$Outfit

# Person infit MSQ:
person_abilities_rasch_nback$Infit <- summarize(group_by(nback_dat, person), Infit = sum(resid ^ 2) /
                                                  sum(fitted * (1 - fitted)))$Infit


nback_person_fit_plot <- person_abilities_rasch_nback %>%
  pivot_longer(4:5, names_to = "Fit Statistic", values_to = "Mean-Square") %>%
  ggplot(aes(x=person, y=`Mean-Square`)) +
  geom_hline(yintercept = c(0.5,1.5), linetype = "dashed", color="red") +
  geom_point() +
  labs(x = "Person",
       title = "Person Fit Statistics") +
  facet_grid(.~`Fit Statistic`) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

nback_fit_plot <- (nback_item_fit_plot + nback_person_fit_plot)

save(nback_fit_plot, file = "plots/nback_fit_plot")

nback_fit_plot

ggsave("plots/nback_fit_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)

# Recombine with data and plot the item difficulty estimates with the true difficulty (i.e., N)
nback_dat <- nback_dat %>%
  mutate(item = as.numeric(item)) %>%
  left_join(person_abilities_rasch_nback, by="person") %>%
  left_join(item_difficulties_rasch_nback, by="item") %>%
  rename(ability = `(Intercept).x`,
         difficulty = `(Intercept).y`)

true2logit_plot <- nback_dat %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x=item, y=difficulty)) +
  geom_point() +
  labs(x = "True Difficulty (N-Back)",
       y = "Difficulty (logits)") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
  theme_bw() + 
  theme(panel.grid = element_blank())

save(true2logit_plot, file = "plots/true2logit_plot")

# Calculate the deviation between the mean-sigma transformation with the true difficulty (i.e., N) and plot
nback_difficulties_rasch_nback <- nback_dat %>%
  group_by(item) %>%
  filter(row_number() == 1) %>%
  mutate(true_difficulty = as.numeric(as.character(item)),
         true_difficulty_estimate = item_true_scale_estimate,
         parameter = "item") %>%
  ungroup() %>%
  dplyr::select(true_difficulty, true_difficulty_estimate, parameter)

mean_deviation <- nback_difficulties_rasch_nback %>%
  mutate(deviation = abs((true_difficulty - true_difficulty_estimate) / true_difficulty)) %>%
  summarise(mean_deviation = mean(deviation, na.rm=TRUE))

true2estimate_scales_plot <- nback_difficulties_rasch_nback %>%
  ggplot(aes(x=true_difficulty, y=true_difficulty_estimate)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_point() +
  geom_text(data = mean_deviation,
            aes(x=3, y=4, label = paste("Item MADP = ", round(mean_deviation*100, digits=2),"%")),
            size = 2) +
  labs(x = "True Difficulty (N-Back)",
       y = "Estimated Difficulty (N-Back)") +
  scale_x_continuous(limits = c(0,6), breaks = c(1,2,3,4,5,6)) +
  scale_y_continuous(limits = c(0,6.5), breaks = c(1,2,3,4,5,6)) +
  theme_bw() + 
  theme(panel.grid = element_blank())

save(true2estimate_scales_plot, file = "plots/true2estimate_scales_plot")

(true2logit_plot + true2estimate_scales_plot) +
  plot_annotation(tag_levels = "A")

ggsave("plots/nback_true2logit_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)

##### Now we calculating effort using the different operationalisations from the Rasch model

nback_dat <- nback_dat %>%
  ungroup() %>%
  mutate(irt_effort_meansigma = if_else(person_true_scale_estimate >= item_true_scale_estimate, item_true_scale_estimate/person_true_scale_estimate*100, 100),
         irt_effort_odds = if_else(exp(ability) >= exp(difficulty), (exp(difficulty)/exp(ability))*100, 100),
         irt_effort_logit_diff = if_else(ability >= difficulty, ability - difficulty, 0)*-1, 
         difficulty_logit_shift = difficulty + (0 - if_else(min(ability, na.rm=TRUE) < min(difficulty, na.rm=TRUE), min(ability, na.rm=TRUE), min(difficulty, na.rm=TRUE))),
         ability_logit_shift = ability + (0 - if_else(min(ability, na.rm=TRUE) < min(difficulty, na.rm=TRUE), min(ability, na.rm=TRUE), min(difficulty, na.rm=TRUE))),
         irt_effort_logit_shift = if_else(ability_logit_shift >= difficulty_logit_shift, difficulty_logit_shift/ability_logit_shift*100, 100))

# Plot the actual effort estimated for each item and operationalisation
effort_dat <- nback_dat %>%
  group_by(person,item) %>%
  filter(row_number() == 1) %>%
  pivot_longer(cols = c("irt_effort_meansigma",
                        "irt_effort_odds",
                        "irt_effort_logit_shift"),
               names_to = "effort_operation",
               values_to = "effort") 

effort_dat$effort_operation <- recode(effort_dat$effort_operation, 
                                      irt_effort_meansigma = "Mean-sigma Method", 
                                      irt_effort_odds = "Odds Method",
                                      irt_effort_logit_shift = "Logit Shift Method")

nback_effort_plot <- effort_dat %>%
  mutate(effort_operation = factor(effort_operation, levels = c("Mean-sigma Method","Odds Method","Logit Shift Method"))) %>%
  ggplot(aes(x=item, y=effort, color=effort_operation)) +
  geom_point(alpha = 0.5, size = 0.75, position = position_jitter()) +
  # geom_line(aes(group=interaction(person,effort_operation))) +
  # stat_summary(fun.data = "mean_cl_boot", position = position_dodge(w=0.1)) +
  scale_fill_manual("Effort Operation", values = alpha(c("#009E73", "#D55E00","black"),0.5)) +
  scale_color_manual("Effort Operation", values = alpha(c("#009E73", "#D55E00","black"),0.5)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
  labs(x = "True Difficulty (N-Back)",
       y = "Estimated Effort (%)") +
  facet_grid(. ~ effort_operation) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

save(nback_effort_plot, file = "plots/nback_effort_plot")

nback_effort_plot

ggsave("plots/nback_effort_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)

##### We see a considerable difference in the efforts calculated by each method with some low and some high
### Likely because the N-back items are actually quite easy relative to the abilities of this sample

nback_rasch_estimates_distribution <- rbind(
  data.frame(Parameter = "Item",
             Estimate = item_difficulties_rasch_nback$`(Intercept)`),
  data.frame(Parameter = "Person",
             Estimate = person_abilities_rasch_nback$`(Intercept)`)
) %>%
  ggplot(aes(x=Estimate, color = Parameter, fill = Parameter)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "Estimate (logits)",
       y = "Density") +
  theme_bw() + 
  theme(panel.grid = element_blank())

save(nback_rasch_estimates_distribution, file = "plots/nback_rasch_estimates_distribution")

nback_rasch_estimates_distribution

ggsave("plots/nback_rasch_estimates_distribution.png", width = 7.5, height = 3.75, device = "png", dpi = 300)

  
### Let's look back at one of the simulated datasets, the normal error with narrow items
### We'll omit the items above mean oneRM, and also the persons below it, to simulate a situation similar to this one

load("datasets/oneRM_dat_1000_10_narrow")

oneRM_dat_1000_10_narrow <- oneRM_dat_1000_10_narrow %>%
  filter(as.numeric(as.character(item)) < 100 & one_RM > 100)

# Fit the Rasch model to the altered simulated dataset
glmer_rasch_1000_10_narrow <- lme4::glmer(response_rasch ~ 1 + (1 | item) + (1 | person), 
                                        data = oneRM_dat_1000_10_narrow, 
                                        family = binomial(link = "logit"))

# Extract the parameters and calculate the mean-sigma scale transformations
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
         range = "narrow") %>%
  mutate(deviation_oneRM = abs((true_load - true_load_estimate_oneRM) / true_load ),
         deviation_item = abs((true_load - true_load_estimate_item) / true_load ))

# Plot the scale estimates and deviations
true2estimate_scales_rasch_1000_10_narrow$parameter <- recode(true2estimate_scales_rasch_1000_10_narrow$parameter, item = "Item (i.e., Load)", person = "Person (i.e., 1RM)")

true2estimate_oneRM_scales_plot <- true2estimate_scales_rasch_1000_10_narrow %>%
  ggplot(aes(x=true_load, y=true_load_estimate_oneRM, color=parameter)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_line(alpha=0.75) +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Estimated Scale (kg)",
       title = "Mean-sigma Scale Recovery (Person Strength Anchoring)") +
  theme_bw() +
  theme(title = element_text(size = 8),
        panel.grid = element_blank())

true2estimate_item_scales_plot <- true2estimate_scales_rasch_1000_10_narrow %>%
  ggplot(aes(x=true_load, y=true_load_estimate_item, color=parameter)) +
  geom_abline(intercept = 0, slope = 1, size=0.15, linetype = "dashed") +
  geom_line(alpha=0.75) +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Estimated Scale (kg)",
       title = "Mean-sigma Scale Recovery (Item Load Anchoring)") +
  scale_y_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) +
  theme_bw() +
  theme(title = element_text(size = 8),
        panel.grid = element_blank())

mean_deviations <- true2estimate_scales_rasch_1000_10_narrow %>%
  group_by(parameter) %>%
  add_count() %>%
  summarise(mean_deviation_oneRM = sum(deviation_oneRM)/n,
            mean_deviation_item = sum(deviation_item)/n) %>%
  filter(row_number() == 1)

true2estimate_deviation_oneRM_plot <- true2estimate_scales_rasch_1000_10_narrow %>%
  ggplot(aes(x=true_load, y=deviation_oneRM*100)) +
  geom_point(aes(color=parameter), size=0.75, alpha=0.5)  +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Deviation of Estimated Scale (%)",
       title = "Absolute Deviation Proportion (Person Strength Anchoring)") +
  scale_y_continuous(limits = c(0,150)) +
  geom_text(data = subset(mean_deviations, parameter == "Item (i.e., Load)"), 
            aes(x=125, y=140, label = paste("Item MADP = ", round(mean_deviation_oneRM*100, digits=2),"%")),
            size = 2) +
  geom_text(data = subset(mean_deviations, parameter == "Person (i.e., 1RM)"), 
            aes(x=125, y=120, label = paste("Person MADP = ", round(mean_deviation_oneRM*100, digits=2),"%")),
            size = 2) +
  theme_bw() +
  theme(title = element_text(size = 8),
        panel.grid = element_blank())

true2estimate_deviation_item_plot <- true2estimate_scales_rasch_1000_10_narrow %>%
  ggplot(aes(x=true_load, y=deviation_item*100)) +
  geom_point(aes(color=parameter), size=0.75, alpha=0.5)  +
  scale_fill_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  scale_color_manual("Parameter", values = alpha(c("#009E73", "#D55E00"),0.5)) +
  labs(x = "True Scale (kg)",
       y = "Deviation of Estimated Scale (%)",
       title = "Absolute Deviation Proportion (Item Load Anchoring)") +
  scale_y_continuous(limits = c(0,150)) +
  geom_text(data = subset(mean_deviations, parameter == "Item (i.e., Load)"), 
            aes(x=125, y=140, label = paste("Item MADP = ", round(mean_deviation_item*100, digits=2),"%")),
            size = 2) +
  geom_text(data = subset(mean_deviations, parameter == "Person (i.e., 1RM)"), 
            aes(x=125, y=120, label = paste("Person MADP = ", round(mean_deviation_item*100, digits=2),"%")),
            size = 2) +
  theme_bw() +
  theme(title = element_text(size = 8),
        panel.grid = element_blank())

true2estimate_deviation_item_plot_easy_item_high_ability <- ((true2estimate_oneRM_scales_plot | true2estimate_item_scales_plot) /
  (true2estimate_deviation_oneRM_plot | true2estimate_deviation_item_plot)) +
  plot_layout(guides = "collect")

save(true2estimate_deviation_item_plot_easy_item_high_ability, file = "plots/true2estimate_deviation_item_plot_easy_item_high_ability")

true2estimate_deviation_item_plot_easy_item_high_ability

ggsave("plots/true2estimate_deviation_item_plot_easy_item_high_ability.png", width = 10, height = 5, device = "png", dpi = 300)

# Calculate the different effort operationalisations and examine whether any bias is present in this case
person_abilities_1000_10_narrow <- data.frame(person = unique(oneRM_dat_1000_10_narrow$person),
                                           rasch_ability = person_abilities_rasch_1000_10_narrow$`(Intercept)`, 
                                           rasch_true_oneRM_oneRM = person_abilities_rasch_1000_10_narrow$person_true_scale_estimate_oneRM,
                                           rasch_true_oneRM_item = person_abilities_rasch_1000_10_narrow$person_true_scale_estimate_load)

item_difficulties_1000_10_narrow <- data.frame(item = unique(oneRM_dat_1000_10_narrow$item),
                                            rasch_difficulty = item_difficulties_rasch_1000_10_narrow$`(Intercept)`, 
                                            rasch_true_load_oneRM = item_difficulties_rasch_1000_10_narrow$item_true_scale_estimate_oneRM,
                                            rasch_true_load_item = item_difficulties_rasch_1000_10_narrow$item_true_scale_estimate_load)

oneRM_dat_1000_10_narrow <- left_join(oneRM_dat_1000_10_narrow, person_abilities_1000_10_narrow, by="person") %>%
  left_join(item_difficulties_1000_10_narrow, by="item") %>%
  mutate(irt_effort_meansigma_oneRM_rasch = if_else(rasch_true_oneRM_oneRM >= rasch_true_load_oneRM, rasch_true_load_oneRM/rasch_true_oneRM_oneRM*100, 100),
         irt_effort_meansigma_item_rasch = if_else(rasch_true_oneRM_item >= rasch_true_load_item, rasch_true_load_item/rasch_true_oneRM_item*100, 100),
         irt_effort_odds_rasch = if_else(exp(rasch_ability) >= exp(rasch_difficulty), (exp(rasch_difficulty)/exp(rasch_ability))*100, 100),
         irt_effort_logit_diff_rasch = if_else(rasch_ability >= rasch_difficulty, rasch_ability - rasch_difficulty, 0)*-1, 
         rasch_difficulty_logit_shift = rasch_difficulty + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         rasch_ability_logit_shift = rasch_ability + (0 - if_else(min(rasch_ability) < min(rasch_difficulty), min(rasch_ability), min(rasch_difficulty))),
         irt_effort_logit_shift_rasch = if_else(rasch_ability_logit_shift >= rasch_difficulty_logit_shift, rasch_difficulty_logit_shift/rasch_ability_logit_shift*100, 100)) %>%
  group_by(person, item) %>%
  filter(row_number() == 1)


effort_meansigma_oneRM_easy_items_high_abilities_plot <- oneRM_dat_1000_10_narrow %>%
  group_by(person,item) %>%
  ggplot(aes(x=actual_effort, y=irt_effort_meansigma_oneRM_rasch)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (%)",
       title = "Estimation of Effort (Mean-sigma Person Strength Anchor)") +
  theme_bw() +
  theme(title = element_text(size = 7),
        panel.grid = element_blank())

effort_meansigma_item_easy_items_high_abilities_plot <- oneRM_dat_1000_10_narrow %>%
  group_by(person,item) %>%
  ggplot(aes(x=actual_effort, y=irt_effort_meansigma_item_rasch)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (%)",
       title = "Estimation of Effort (Mean-sigma Item Load Anchor)") +
  theme_bw() +
  theme(title = element_text(size = 7),
        panel.grid = element_blank())

effort_odds_easy_items_high_abilities_plot <- oneRM_dat_1000_10_narrow %>%
  group_by(person,item) %>%
  ggplot(aes(x=actual_effort, y=irt_effort_odds_rasch)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (%)",
       title = "Estimation of Effort (Odds Formulation)") +
  theme_bw() +
  theme(title = element_text(size = 7),
        panel.grid = element_blank())

effort_logit_shift_easy_items_high_abilities_plot <- oneRM_dat_1000_10_narrow %>%
  group_by(person,item) %>%
  ggplot(aes(x=actual_effort, y=irt_effort_logit_shift_rasch)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Actual Effort (%)",
       y = "Estimated Effort (%)",
       title = "Estimation of Effort (Logit Shift Method)") +
  theme_bw() +
  theme(title = element_text(size = 7),
        panel.grid = element_blank())

effort_easy_items_high_abilities_plot <- (effort_meansigma_oneRM_easy_items_high_abilities_plot | effort_meansigma_item_easy_items_high_abilities_plot) /
  (effort_odds_easy_items_high_abilities_plot | effort_logit_shift_easy_items_high_abilities_plot)

save(effort_easy_items_high_abilities_plot, file = "plots/effort_easy_items_high_abilities_plot")

effort_easy_items_high_abilities_plot

ggsave("plots/effort_easy_items_high_abilities_plot.png", width = 7.5, height = 5, device = "png", dpi = 300)


##### We also have some self-report data for the perception of effort (NASA-TLX effort subscale) 
### So we can explore how our actual effort estimates relate to this i.e,. the psychophysical relationships

# Recombine with NASA-TLX data for each item 
# Note, NASA-TLX data only available for 2 studies and items 1- to 4-back (MAUS includes 2- & 3-back and Westbrook et al. PloS One 1- to 4-back)
nback_dat <- left_join(nback_dat, NASA_dat, by = c("person", "item"))

effort_NASA_nback_level_plot <- nback_dat %>%
  group_by(person,item) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x=item, y=perception_effort*100)) +
  # geom_point(alpha = 0.25, size = 0.8, position = position_jitter(0.1)) +
  geom_line(aes(group=person), position = position_jitter(0.1)) +
  scale_x_continuous(limits = c(0.8,4.2), breaks = c(1,2,3,4)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "True Difficulty (N-Back)",
       y = "Perception of Effort (NASA-TLX; %)") +
  theme_bw() +
  theme(panel.grid = element_blank())

effort_NASA_meansigma_plot <- nback_dat %>%
  group_by(person,item) %>%
  filter(row_number() == 1 & irt_effort_meansigma != 100) %>%
  ggplot(aes(x=irt_effort_meansigma, y=perception_effort*100)) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=person), method = "lm", se=FALSE, color = "black", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Estimated Effort (Mean-sigma Method; %)",
       y = "Perception of Effort (NASA-TLX; %)") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=8),
        panel.grid = element_blank())

effort_NASA_odds_plot <- nback_dat %>%
  group_by(person,item) %>%
  filter(row_number() == 1 & irt_effort_odds != 100) %>%
  ggplot(aes(x=irt_effort_odds, y=perception_effort*100)) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=person), method = "lm", se=FALSE, color = "black", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Estimated Effort (Odds Method; %)",
       y = "Perception of Effort (NASA-TLX; %)") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=8),
        panel.grid = element_blank())

effort_NASA_logit_shift_plot <- nback_dat %>%
  group_by(person,item) %>%
  filter(row_number() == 1 & irt_effort_logit_shift != 100) %>%
  ggplot(aes(x=irt_effort_logit_shift, y=perception_effort*100)) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=person), method = "lm", se=FALSE, color = "black", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  labs(x = "Estimated Effort (Logit Shift Method; %)",
       y = "Perception of Effort (NASA-TLX; %)") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=8),
        panel.grid = element_blank())


nback_effort_NASA_plot <- 
  (effort_NASA_meansigma_plot | effort_NASA_odds_plot | effort_NASA_logit_shift_plot)

save(nback_effort_NASA_plot, file = "plots/nback_effort_NASA_plot")

nback_effort_NASA_plot

ggsave("plots/nback_effort_NASA_plot.png", width = 10, height = 2.5, device = "png", dpi = 300)


### Fit an ordered beta regression model to the actual effort operationalisations and perception of effort

nback_NASA_dat <- nback_dat %>%
  filter(!is.na(perception_effort)) %>%
  group_by(person,item) %>%
  filter(row_number() == 1)

ordbeta_meansigma_NASA_effort <- ordbetareg(formula = perception_effort ~ irt_effort_meansigma + (irt_effort_meansigma | person),
                                  data = nback_NASA_dat,
                                  chains = 4,
                                  iter = 3000, warmup = 1000,
                                  cores = 4,
                                  control = list(adapt_delta = 0.95), inits = 0)

ordbeta_meansigma_NASA_effort_plot <- 
  crossing(irt_effort_meansigma = seq(from = 0, to = 100, length = 101),
           person = unique(nback_NASA_dat$person)) %>%
  add_epred_draws(ordbeta_meansigma_NASA_effort) %>%
  ggplot(aes(x = irt_effort_meansigma, y = .epred*100)) +
  stat_lineribbon(aes(y = .epred*100, fill_ramp = stat(.width)), size = 0.5, alpha = 0.8, .width = ppoints(100), fill = "black") +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide = ggdist::guide_rampbar(to = "black")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  labs(x = "Estimated Effort (Mean-sigma Method; %)",
       y = "Perception of Effort (NASA-TLX; %)",
       fill_ramp = "Credible Interval Width") +
  theme_bw() + 
  theme(axis.title = element_text(size=8),
        panel.grid = element_blank(),
        legend.position = "bottom")

ordbeta_odds_NASA_effort <- ordbetareg(formula = perception_effort ~ irt_effort_odds + (irt_effort_odds | person),
                                            data = nback_NASA_dat,
                                            chains = 4,
                                            iter = 3000, warmup = 1000,
                                            cores = 4,
                                            control = list(adapt_delta = 0.95), inits = 0)

ordbeta_odds_NASA_effort_plot <- 
  crossing(irt_effort_odds = seq(from = 0, to = 100, length = 101),
           person = unique(nback_NASA_dat$person)) %>%
  add_epred_draws(ordbeta_odds_NASA_effort) %>%
  ggplot(aes(x = irt_effort_odds, y = .epred*100)) +
  stat_lineribbon(aes(y = .epred*100, fill_ramp = stat(.width)), size = 0.5, alpha = 0.8, .width = ppoints(100), fill = "black") +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide = ggdist::guide_rampbar(to = "black")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  labs(x = "Estimated Effort (Odds Method; %)",
       y = "Perception of Effort (NASA-TLX; %)",
       fill_ramp = "Credible Interval Width") +
  theme_bw() + 
  theme(axis.title = element_text(size=8),
        panel.grid = element_blank(),
        legend.position = "bottom")

ordbeta_logit_shift_NASA_effort <- ordbetareg(formula = perception_effort ~ irt_effort_logit_shift + (irt_effort_logit_shift | person),
                                            data = nback_NASA_dat,
                                            chains = 4,
                                            iter = 3000, warmup = 1000,
                                            cores = 4,
                                            control = list(adapt_delta = 0.95), inits = 0)

ordbeta_logit_shift_NASA_effort_plot <- 
  crossing(irt_effort_logit_shift = seq(from = 0, to = 100, length = 101),
           person = unique(nback_NASA_dat$person)) %>%
  add_epred_draws(ordbeta_logit_shift_NASA_effort) %>%
  ggplot(aes(x = irt_effort_logit_shift, y = .epred*100)) +
  stat_lineribbon(aes(y = .epred*100, fill_ramp = stat(.width)), size = 0.5, alpha = 0.8, .width = ppoints(100), fill = "black") +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide = ggdist::guide_rampbar(to = "black")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  labs(x = "Estimated Effort (Logit Shift Method; %)",
       y = "Perception of Effort (NASA-TLX; %)",
       fill_ramp = "Credible Interval Width") +
  theme_bw() + 
  theme(axis.title = element_text(size=8),
        panel.grid = element_blank(),
        legend.position = "bottom")

nback_effort_NASA_ordbeta_plot <- 
  (ordbeta_meansigma_NASA_effort_plot | ordbeta_odds_NASA_effort_plot | ordbeta_logit_shift_NASA_effort_plot) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

save(nback_effort_NASA_ordbeta_plot, file = "plots/nback_effort_NASA_ordbeta_plot")

nback_effort_NASA_ordbeta_plot

ggsave("plots/nback_effort_NASA_ordbeta_plot.png", width = 10, height = 3.75, device = "png", dpi = 300)



mat2df <- function(path){
  mat <- R.matlab::readMat(path)
  
  data <- lapply(mat$data, unlist, use.names=FALSE)
  
  item <- as.data.frame(data[[1]])
  trial <- as.data.frame(data[[2]])
  kind <- as.data.frame(data[[5]])
  resp <- as.data.frame(data[[9]])
  
  df <- data.frame(person=path,
                   item=item$V1,
                   resp=resp$V1,
                   trial=trial$V1, 
                   kind=kind$V1) 
}


# Read in individual player files and recode
raw <- list.files(path="datasets/Westbrook et al./drug_trial", pattern="*.mat")

raw <- data.frame(path = "datasets/Westbrook et al./drug_trial/",
                  raw) %>%
  unite("path", 1:2, sep = "")

df_all <- sapply(raw$path, mat2df, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>%
  separate(id, sep =  "[^[:alnum:]]+", into = LETTERS) %>%
  select(person, H, J, item, trial, kind, resp)

df_all <- df_all %>%
  rename("id" = H,
         "drug" = J) %>%
  mutate(id = as.numeric(id))

deblind_drugs <- read.csv("datasets/Westbrook et al./drug_trial/ses-drug_desc-deblindingByDrug.tsv") %>%
  pivot_longer(2:4,names_to = "drug", values_to = "what") %>%
  rename("id" = subject)

deblind_drugs$drug <- recode(deblind_drugs$drug, session1 = "drug1", session2 = "drug2", session3 = "drug3")

df_all <- left_join(df_all, deblind_drugs, by = c("id","drug")) %>%
  mutate(kind = if_else(kind == 1, "Target", 
                        if_else(kind == 0, "Nontarget", "Lure")))

write.csv(df_all, file = "datasets/Westbrook et al./drug_trial/nback_drug_trial.csv")


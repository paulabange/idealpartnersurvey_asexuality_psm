## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Library---------------------------------------------------------------------------------------------------------------------
library(formr)
library(dplyr)
library(tidyr)


## ----Load data-------------------------------------------------------------------------------------------------------------------
data_wrangled = read.csv(file = "data/data_wrangled.csv")[,-1]


## --------------------------------------------------------------------------------------------------------------------------------
nrow(data_wrangled) # total number of clicks on the survey
nrow(data_wrangled %>% filter(age >= 18)) # total number of responses for participants 18 or older


## --------------------------------------------------------------------------------------------------------------------------------
data_wrangled$id = 1:94738


## ----Exlcusion criterion 1-------------------------------------------------------------------------------------------------------
data_wrangled_main = data_wrangled %>%
  filter(sex == "Woman")

nrow(data_wrangled_main) # 64879 women 
nrow(data_wrangled) - nrow(data_wrangled_main) # number of excluded participants

table(data_wrangled_main$sex) # check gender
table(data_wrangled_main$sexual_orientation)# check sexual orientations


## ----Excluison criterion 2-------------------------------------------------------------------------------------------------------
data_wrangled_main = data_wrangled_main %>% 
  filter(sexual_orientation == "Straight/Heterosexual" | sexual_orientation == "Asexual")

nrow(data_wrangled_main) # 53390 women 
64879 - nrow(data_wrangled_main) # number of excluded participants
table(data_wrangled_main$sexual_orientation) # 457 asexuals and 52933 heterosexuals


## ----Exclusion criterion 3-------------------------------------------------------------------------------------------------------
data_wrangled_main = data_wrangled_main %>% 
 mutate(answer_accuracy = replace_na(answer_accuracy, "No value"))
data_wrangled_main = data_wrangled_main %>%
  filter(answer_accuracy == "I took the survey seriously; please use my information in the study." | answer_accuracy == "No value")

nrow(data_wrangled_main) # 51775 women 
53390 - nrow(data_wrangled_main) # number of excluded participants
table(data_wrangled_main$sexual_orientation) # 447 asexuals and 51328 heterosexuals

# check NAs
table(is.na(data_wrangled_main$answer_accuracy)) 
table(data_wrangled_main$answer_accuracy)


## ----Criterion 1 robustness------------------------------------------------------------------------------------------------------
data_wrangled_robustness = data_wrangled %>%
  filter(sex == "Woman" | sex == "Man" | sex == "Genderqueer/Nonbinary")

nrow(data_wrangled_robustness) # 67774 participants 
nrow(data_wrangled) - nrow(data_wrangled_robustness) # number of excluded participants

table(data_wrangled_robustness$sex) # check gender
table(data_wrangled_robustness$sexual_orientation)# check sexual orientations


## ----Criterion 2 robustness------------------------------------------------------------------------------------------------------
data_wrangled_robustness = data_wrangled_robustness %>% 
  filter(sexual_orientation == "Straight/Heterosexual" | sexual_orientation == "Asexual")

nrow(data_wrangled_robustness) # 55076 participants
67774 - nrow(data_wrangled_robustness) # number of excluded participants

table(data_wrangled_robustness$sex) # check gender
table(data_wrangled_robustness$sexual_orientation) # check sexual orientation


## ----Criterion 3 robustness------------------------------------------------------------------------------------------------------
data_wrangled_robustness = data_wrangled_robustness %>% 
 mutate(answer_accuracy = replace_na(answer_accuracy, "No value"))
data_wrangled_robustness = data_wrangled_robustness %>%
  filter(answer_accuracy == "I took the survey seriously; please use my information in the study." | answer_accuracy == "No value")

nrow(data_wrangled_robustness) # 53412 partiicpants
55076 - nrow(data_wrangled_robustness) # total number of excluded participants

table(data_wrangled_robustness$sex) # check gender
table(data_wrangled_robustness$sexual_orientation) # check sexual orientation


## --------------------------------------------------------------------------------------------------------------------------------
# for main analyses
nrow(data_wrangled) - nrow(data_wrangled_main)

# for robustness check
nrow(data_wrangled) - nrow(data_wrangled_robustness)


## --------------------------------------------------------------------------------------------------------------------------------
# for main analyses
# by sexual orientation
table(data_wrangled_main$sexual_orientation)

# for robustness check
# by sexual orientation
table(data_wrangled_robustness$sexual_orientation)

# by gender
table(data_wrangled_robustness$sex)

# by gender and sexual orientation
xtabs(~sex+sexual_orientation, data_wrangled_robustness) 


## ----Save data main analyses-----------------------------------------------------------------------------------------------------
# write.csv(data_wrangled_main,
#           file = "data/data_main.csv")


## ----Save data robustness check--------------------------------------------------------------------------------------------------
# write.csv(data_wrangled_robustness,
#           file = "data/data_robustness.csv")


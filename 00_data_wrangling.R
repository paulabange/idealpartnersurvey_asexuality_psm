## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
library(formr)
library(dplyr)


## --------------------------------------------------------------------------------------------------------------------------------
# load data
fulldata_selected = read.csv(file = "data/fulldata_selected.csv")[,-1]
names(fulldata_selected)


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled = fulldata_selected


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$sexual_orientation)

# Set missing values as NA
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(sexual_orientation = as.character(sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation == "", NA, sexual_orientation),
         sexual_orientation = factor(sexual_orientation))

table(fulldata_selected_wrangled$sexual_orientation)

table(is.na(fulldata_selected_wrangled$sexual_orientation))


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(country = ifelse(country == "Korea, South",
                          "South Korea",
                          country),
         country = ifelse(country == "Congo, Democratic Republic of the",
                          "Congo, Republic of the",
                          country))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$country)
table(is.na(fulldata_selected_wrangled$country))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$language)
table(is.na(fulldata_selected_wrangled$country))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$age)


## ----Age 2-----------------------------------------------------------------------------------------------------------------------
sum(fulldata_selected_wrangled$age > 100)
# This means that age is set as missing for 15 people

fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(age = ifelse(age > 100, NA, age))

table(is.na(fulldata_selected_wrangled$age))

table(fulldata_selected_wrangled$age < 18)
# 26638 people were not allowed to participate in the survey because they were younger than 18


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$sex)

# Set missing values as NA
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(sex = as.character(sex),
         sex = ifelse(sex == "", NA, sex),
         sex = factor(sex))

table(fulldata_selected_wrangled$sex)

table(is.na(fulldata_selected_wrangled$sex))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$transgender)

# Set missing values as NA
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(transgender = as.character(transgender),
         transgender = ifelse(transgender == "", NA, transgender),
         transgender = factor(transgender))

table(fulldata_selected_wrangled$transgender)
table(is.na(fulldata_selected_wrangled$transgender))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$relationship) #No romantic or sexual relationships during the past 3 months
table(fulldata_selected_wrangled$relationship1) #Other
table(fulldata_selected_wrangled$relationship2) #Short-term (casual) sexual relationship (e.g. hookups or one-night-stands)
table(fulldata_selected_wrangled$relationship3) #New (less than 1 month old) romantic and/or sexual relationship 
table(fulldata_selected_wrangled$relationship4) #Ongoing (longer than 1 month) uncommitted/non-exclusive romantic and/or sexual relationship
table(fulldata_selected_wrangled$relationship5)#Long-term committed/exclusive sexual relationship with one or more partners

fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(relationship = ifelse(relationship == "True",
                               "No romantic or sexual relationships during the past 3 months, ",
                               ""),
         relationship = ifelse(relationship1 == "True",
                               paste(relationship,
                                     "Other, "),
                               relationship),
         relationship = ifelse(relationship2 == "True",
                               paste(relationship,
                                     "Short-term (casual) sexual relationship (e.g. hookups or one-night-stands), "),
                               relationship),
         relationship = ifelse(relationship3 == "True",
                               paste(relationship,
                                     "New (less than 1 month old) romantic and/or sexual relationship, "),
                               relationship),
         relationship = ifelse(relationship4 == "True",
                               paste(relationship,
                                     "Ongoing (longer than 1 month) uncommitted/non-exclusive romantic and/or sexual relationship, "),
                               relationship),
         relationship = ifelse(relationship5 == "True", 
                               paste(relationship,
                                     "Long-term committed/exclusive sexual relationship with one or more partners, "), 
                               relationship),
         relationship = ifelse(relationship == "", NA, relationship))

table(fulldata_selected_wrangled$relationship)

# Check whether command worked correctly
table(fulldata_selected_wrangled$relationship %contains% "No romantic or sexual relationships during the past 3 months")
table(is.na(fulldata_selected_wrangled$relationship))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$relationship_length)

fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(relationship_length = ifelse(is.na(relationship_length) &
                                        !is.na(relationship),
                                      0,
                                      relationship_length))
table(fulldata_selected_wrangled$relationship_length)


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$marital_status)

# Set missing values as NA
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(marital_status = as.character(marital_status),
         marital_status = ifelse(marital_status == "", NA, marital_status),
         marital_status = factor(marital_status))

table(fulldata_selected_wrangled$marital_status)
table(is.na(fulldata_selected_wrangled$marital_status))


## --------------------------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$political_spectrum)

fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(political_spectrum = as.character(political_spectrum),
         political_spectrum = ifelse(political_spectrum == "", NA, political_spectrum),
         political_spectrum = factor(political_spectrum))

table(fulldata_selected_wrangled$political_spectrum)
table(is.na(fulldata_selected_wrangled$political_spectrum))


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled %>%
  select(starts_with("interest_")) %>%
  lapply(table)


## --------------------------------------------------------------------------------------------------------------------------------
weird_sevens = fulldata_selected_wrangled %>%
  filter(interest_single == 7)
table(weird_sevens$language)


## --------------------------------------------------------------------------------------------------------------------------------
danish = fulldata_selected_wrangled %>%
  filter(language == "danish")

danish %>%
  select(starts_with("interest_")) %>%
  lapply(table)


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(interest_single = ifelse(language == "danish",
                                  interest_single - 1,
                                  interest_single),
         interest_nonsexrel = ifelse(language == "danish",
                                  interest_nonsexrel - 1,
                                  interest_nonsexrel),
         interest_nonmonrel = ifelse(language == "danish",
                                  interest_nonmonrel - 1,
                                  interest_nonmonrel),
         interest_monrel = ifelse(language == "danish",
                                  interest_monrel - 1,
                                  interest_monrel),
         interest_hookups = ifelse(language == "danish",
                                  interest_hookups - 1,
                                  interest_hookups),
         interst_parent = ifelse(language == "danish",
                                  interst_parent - 1,
                                  interst_parent),
         interest_altrel = ifelse(language == "danish",
                                  interest_altrel - 1,
                                  interest_altrel))


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled %>%
  select(starts_with("interest_")) %>%
  lapply(table)


## --------------------------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled = fulldata_selected_wrangled %>% rename(interest_parent = interst_parent)


## ----Partner preferences---------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled %>%
  select(starts_with("pref_")) %>%
  lapply(table)

# percentage of missings for each variable 
fulldata_selected_wrangled %>%
  select(starts_with("pref_")) %>%
  miss_frac()


## ----Self ratings----------------------------------------------------------------------------------------------------------------
fulldata_selected_wrangled %>%
  select(starts_with("self_")) %>%
  lapply(table)

# percentage of missings for each variable 
fulldata_selected_wrangled %>%
  select(starts_with("self_")) %>%
  miss_frac()


## ----Answer Accuracy-------------------------------------------------------------------------------------------------------------
table(fulldata_selected_wrangled$answer_accuracy)

# Set missing values as NA
fulldata_selected_wrangled = fulldata_selected_wrangled %>%
  mutate(answer_accuracy = as.character(answer_accuracy),
         answer_accuracy = ifelse(answer_accuracy == "", NA, answer_accuracy),
         answer_accuracy = factor(answer_accuracy))

table(fulldata_selected_wrangled$answer_accuracy)

table(is.na(fulldata_selected_wrangled$answer_accuracy))

fulldata_selected_wrangled %>%
  select(sex, answer_accuracy) %>%
  miss_frac()


## ----Categories for relationship status------------------------------------------------------------------------------------------
fulldata_selected_wrangled$relationship_status= ifelse(grepl("Long-term", fulldata_selected_wrangled$relationship), "Long-term",
                                                       ifelse(grepl("Ongoing", fulldata_selected_wrangled$relationship), "Long-term", 
                                                              ifelse(grepl("Short-term", fulldata_selected_wrangled$relationship), "Short-term",
                                                                     ifelse(grepl("New", fulldata_selected_wrangled$relationship), "Short-term",
                                                                            ifelse(grepl("No romantic", fulldata_selected_wrangled$relationship), "No relationship",
                                                                                   NA)))))

# fulldata_selected_wrangled$other = ifelse(grepl("Other", fulldata_selected_wrangled$relationship), "Other", "No")
# other = fulldata_selected_wrangled %>% filter(other == "Other")
# table(other$relationship_status)
# sum(is.na(other$relationship_status))
# looks like there is no case in which only "Other" was selected


## ----Partner preference dimensions-----------------------------------------------------------------------------------------------

# Kind-supportive
fulldata_selected_wrangled$pref_imp_ks = fulldata_selected_wrangled %>% 
  select(pref_imp_kind, pref_imp_supportive) %>% 
  rowMeans(na.rm = T)

# Attractiveness
fulldata_selected_wrangled$pref_imp_att = fulldata_selected_wrangled %>%
  select(pref_imp_attractive_body, pref_imp_attractive_face) %>%
  rowMeans(na.rm = T)
  
# Financially secure-successful
fulldata_selected_wrangled$pref_imp_fs = fulldata_selected_wrangled %>%
  select(pref_imp_financially_secure, pref_imp_successful_ambitous) %>% 
  rowMeans(na.rm = T)

# Confident-assertive
fulldata_selected_wrangled$pref_imp_ca = fulldata_selected_wrangled %>%
  select(pref_imp_confident, pref_imp_assertive) %>%
  rowMeans(na.rm = T)

# Educated-intelligent
fulldata_selected_wrangled$pref_imp_ei = fulldata_selected_wrangled %>%
  select(pref_imp_educated, pref_imp_intelligence) %>%
  rowMeans(na.rm = T)


## ----Self rating dimensions------------------------------------------------------------------------------------------------------

# Kind-supportive
fulldata_selected_wrangled$self_ks = fulldata_selected_wrangled %>% 
  select(self_kind, self_supportive) %>%
  rowMeans(na.rm = T)

# Attractiveness
fulldata_selected_wrangled$self_att = fulldata_selected_wrangled %>%
  select(self_attractive_body, self_attractive_face) %>%
  rowMeans(na.rm = T)
  
# Financially secure-successful
fulldata_selected_wrangled$self_fs = fulldata_selected_wrangled %>%
  select(self_financially_secure, self_successful_ambitous) %>%
  rowMeans(na.rm = T)

# Confident-assertive
fulldata_selected_wrangled$self_ca = fulldata_selected_wrangled %>%
  select(self_confident, self_assertive) %>%
  rowMeans(na.rm = T)

# Educated-intelligent
fulldata_selected_wrangled$self_ei = fulldata_selected_wrangled %>% 
  select(self_educated, self_intelligence) %>%
  rowMeans(na.rm = T)


## ----Save data-------------------------------------------------------------------------------------------------------------------
write.csv(fulldata_selected_wrangled,
           file = "data/data_wrangled.csv")


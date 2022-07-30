## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(formr)
library(ggplot2)
library(MatchIt)
library(optmatch)
library(cobalt)
library(psych)
library(effsize)

## set options for bal.tab function in cobalt package
set.cobalt.options(binary = "std") # for binary covariates used in the matching procedure the bal.tab function will now display standardized mean differences instead of the raw difference


## --------------------------------------------------------------------------------------------------------------------------------
data_robustness = read.csv(file = "data/data_robustness.csv")[,-1]


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_robust_nomiss_rel <- data_robustness %>% select(id, sex, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                         interest_hookups,
                                             interest_nonsexrel,
                                             interest_monrel,
                                             interest_monrel,
                                             interest_nonmonrel,
                                             interest_altrel,
                                             interest_single,
                                             interest_parent) %>%
                                  na.omit()

table(is.na(data_robust_nomiss_rel)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_robust_nomiss_rel$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_robust_nomiss_rel$sexual_orientation = as.factor(data_robust_nomiss_rel$sexual_orientation) # convert to factor
levels(data_robust_nomiss_rel$sexual_orientation) # check levels of factor
data_robust_nomiss_rel$sexual_orientation = factor(data_robust_nomiss_rel$sexual_orientation,
                                             levels=rev(levels(data_robust_nomiss_rel$sexual_orientation))) # reverse order of factor levels 
levels(data_robust_nomiss_rel$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm_robust_rel <- matchit(sexual_orientation ~ sex + language + age + country + relationship_status + relationship_length, method = "nearest", data = data_robust_nomiss_rel)

nnm_robust_data_rel <- match.data(nnm_robust_rel)

nrow(nnm_robust_data_rel) # 758 observations
table(nnm_robust_data_rel$subclass) # data includes only pairs of two observations
dim(nnm_robust_data_rel) # 758 observations and 18 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_robust_rel = summary(nnm_robust_rel, standardize = T)
print(summary_nnm_robust_rel)

bal.tab(nnm_robust_rel, m.threshold = 0.1, un = T, binary = "std")#smd

bal.tab(nnm_robust_rel, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_robust_rel, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_robust_rel, type = "jitter", interactive = FALSE)

# distributional density/histogram
bal.plot(nnm_robust_rel, var.name = "age", which = "both", grid = T)
bal.plot(nnm_robust_rel, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_robust_rel, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_robust_rel, var.name = "language", which = "both", grid = T)
bal.plot(nnm_robust_rel, var.name = "country", which = "both", grid = T) # not readable
bal.plot(nnm_robust_rel, var.name = "sex", which = "both", grid = T)

# remove country variable for love plot
nnm_robust_without_country_rel = nnm_robust_rel
nnm_robust_without_country_rel$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_robust_without_country_rel, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          binary = "std") 


## love plot using alternate variable names
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length", "sex_Woman", "sex_Man", "sex_Genderqueer/Nonbinary"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length", "Woman", "Man", "Genderqueer/Nonbinary"))

love.plot(bal.tab(nnm_robust_without_country_rel, m.threshold = 0.1, binary = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before Matching", "After Matching"),
          var.names = v,
          binary = "std") 


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_robust_nomiss_prefs <- data_robustness %>% select(id, sex, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                         pref_imp_ks, pref_imp_att, pref_imp_fs, pref_imp_ca,
                                         pref_imp_ei, pref_imp_sexually_experienced) %>%
                                  na.omit()

table(is.na(data_robust_nomiss_prefs)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_robust_nomiss_prefs$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_robust_nomiss_prefs$sexual_orientation = as.factor(data_robust_nomiss_prefs$sexual_orientation) # convert to factor
levels(data_robust_nomiss_prefs$sexual_orientation) # check levels of factor
data_robust_nomiss_prefs$sexual_orientation = factor(data_robust_nomiss_prefs$sexual_orientation,
                                             levels=rev(levels(data_robust_nomiss_prefs$sexual_orientation))) # reverse order of factor levels 
levels(data_robust_nomiss_prefs$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm_robust_prefs <- matchit(sexual_orientation ~ sex + language + age + country + relationship_status + relationship_length, method = "nearest", data = data_robust_nomiss_prefs)

nnm_robust_data_prefs <- match.data(nnm_robust_prefs)

nrow(nnm_robust_data_prefs) # 920 observations
table(nnm_robust_data_prefs$subclass) # data includes only pairs of two observations
dim(nnm_robust_data_prefs) # 920 observations and 17 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_robust_prefs = summary(nnm_robust_prefs, standardize = T)
print(summary_nnm_robust_prefs)

bal.tab(nnm_robust_prefs, m.threshold = 0.1, un = T, binary = "std") #smd
bal.tab(nnm_robust_prefs, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_robust_prefs, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_robust_prefs, type = "jitter", interactive = FALSE)

# distributional density/histogram
bal.plot(nnm_robust_prefs, var.name = "age", which = "both", grid = T)
bal.plot(nnm_robust_prefs, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_robust_prefs, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_robust_prefs, var.name = "language", which = "both", grid = T)
bal.plot(nnm_robust_prefs, var.name = "country", which = "both", grid = T) # not readable
bal.plot(nnm_robust_prefs, var.name = "sex", which = "both", grid = T)

# remove country variable for love plot
nnm_robust_without_country_prefs = nnm_robust_prefs
nnm_robust_without_country_prefs$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_robust_without_country_prefs, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          binary = "std") 


## love plot using alternate variable names
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length", "sex_Woman", "sex_Man", "sex_Genderqueer/Nonbinary"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length", "Woman", "Man", "Genderqueer/Nonbinary"))

love.plot(bal.tab(nnm_robust_without_country_prefs, m.threshold = 0.1, binary = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before Matching", "After Matching"),
          var.names = v,
          binary = "std") 


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_robust_nomiss_self <- data_robustness %>% select(id, sex, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                        self_ks, self_att, self_fs,self_ca, self_ei, self_sexually_experienced) %>%
                                  na.omit()

table(is.na(data_robust_nomiss_self)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_robust_nomiss_self$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_robust_nomiss_self$sexual_orientation = as.factor(data_robust_nomiss_self$sexual_orientation) # convert to factor
levels(data_robust_nomiss_self$sexual_orientation) # check levels of factor
data_robust_nomiss_self$sexual_orientation = factor(data_robust_nomiss_self$sexual_orientation,
                                             levels=rev(levels(data_robust_nomiss_self$sexual_orientation))) # reverse order of factor levels 
levels(data_robust_nomiss_self$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm_robust_self <- matchit(sexual_orientation ~ sex + language + age + country + relationship_status + relationship_length, method = "nearest", data = data_robust_nomiss_self)

nnm_robust_data_self <- match.data(nnm_robust_self)

nrow(nnm_robust_data_self) # 902 observations
table(nnm_robust_data_self$subclass) # data includes only pairs of two observations
dim(nnm_robust_data_self) # 902 observations and 17 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_robust_self = summary(nnm_robust_self, standardize = T)
print(summary_nnm_robust_self)

bal.tab(nnm_robust_self, m.threshold = 0.1, un = T, binary = "std") #smd
bal.tab(nnm_robust_self, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_robust_self, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_robust_self, type = "jitter", interactive = FALSE)

# distributional density/histogram
bal.plot(nnm_robust_self, var.name = "age", which = "both", grid = T)
bal.plot(nnm_robust_self, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_robust_self, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_robust_self, var.name = "language", which = "both", grid = T)
bal.plot(nnm_robust_self, var.name = "country", which = "both", grid = T) # not readable
bal.plot(nnm_robust_self, var.name = "sex", which = "both", grid = T)

# remove country variable for love plot
nnm_robust_without_country_self = nnm_robust_self
nnm_robust_without_country_self$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_robust_without_country_self, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          binary = "std") 


## love plot using alternate variable names
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length", "sex_Woman", "sex_Man", "sex_Genderqueer/Nonbinary"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length", "Woman", "Man", "Genderqueer/Nonbinary"))

love.plot(bal.tab(nnm_robust_without_country_self, m.threshold = 0.1, binary = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before Matching", "After Matching"),
          var.names = v,
          binary = "std") 


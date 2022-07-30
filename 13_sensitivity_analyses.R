## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(formr)
library(ggplot2)
library(psych)
library(MatchIt)
library(rbounds)
library(EValue)


## --------------------------------------------------------------------------------------------------------------------------------
# Matching must be run again
# Load data
data_main = read.csv(file = "data/data_main.csv")[,-1]


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss <- data_main %>% dplyr::select(id, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                         pref_imp_ks, pref_imp_att, pref_imp_fs, pref_imp_ca,
                                         pref_imp_ei, pref_imp_sexually_experienced) %>%
                                  na.omit()

table(is.na(data_main_nomiss)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_main_nomiss$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_main_nomiss$sexual_orientation = as.factor(data_main_nomiss$sexual_orientation) # convert to factor
levels(data_main_nomiss$sexual_orientation) # check levels of factor
data_main_nomiss$sexual_orientation = factor(data_main_nomiss$sexual_orientation,
                                             levels=rev(levels(data_main_nomiss$sexual_orientation))) # reverse order of factor levels 
levels(data_main_nomiss$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", data = data_main_nomiss)

nnm_data <- match.data(nnm)


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss_self <- data_main %>% dplyr::select(id, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                         self_ks, self_att, self_fs,self_ca, self_ei, self_sexually_experienced) %>%
                                  na.omit()

table(is.na(data_main_nomiss_self)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_main_nomiss_self$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_main_nomiss_self$sexual_orientation = as.factor(data_main_nomiss_self$sexual_orientation) # convert to factor
levels(data_main_nomiss_self$sexual_orientation) # check levels of factor
data_main_nomiss_self$sexual_orientation = factor(data_main_nomiss_self$sexual_orientation,
                                             levels=rev(levels(data_main_nomiss_self$sexual_orientation))) # reverse order of factor levels
levels(data_main_nomiss_self$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm_self <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", data = data_main_nomiss_self)

nnm_self_data <- match.data(nnm_self)


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss_rel <- data_main %>% dplyr::select(id, language, age, sex, country, relationship_status, relationship_length,
                                             sexual_orientation,
                                             interest_hookups,
                                             interest_nonsexrel,
                                             interest_monrel,
                                             interest_monrel,
                                             interest_nonmonrel,
                                             interest_altrel,
                                             interest_single,
                                             interest_parent) %>%
                                      na.omit()

table(is.na(data_main_nomiss_rel)) # no more NAs in data set


## --------------------------------------------------------------------------------------------------------------------------------
typeof(data_main_nomiss_rel$sexual_orientation) # is variable of type character, but for PSM it needs to be a factor
data_main_nomiss_rel$sexual_orientation = as.factor(data_main_nomiss_rel$sexual_orientation) # convert to factor
levels(data_main_nomiss_rel$sexual_orientation) # check levels of factor
data_main_nomiss_rel$sexual_orientation = factor(data_main_nomiss_rel$sexual_orientation,
                                             levels=rev(levels(data_main_nomiss_rel$sexual_orientation))) # reverse order of factor levels
levels(data_main_nomiss_rel$sexual_orientation) # check factor level order


## --------------------------------------------------------------------------------------------------------------------------------
nnm_rel <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", data = data_main_nomiss_rel)

nnm_rel_data <- match.data(nnm_rel)


## --------------------------------------------------------------------------------------------------------------------------------
# rbounds package evaluates match Object from Matching package, since I have used the MatchIt package for matching, I must extract the values for the outcomes for each matching pair 
m.pairs.pref_imp_ca <- cbind(nnm_data[row.names(nnm$match.matrix), 'pref_imp_ca'],
nnm_data[nnm$match.matrix, 'pref_imp_ca'])

x = m.pairs.pref_imp_ca[,1] # asexuals
y = m.pairs.pref_imp_ca[,2] # heterosexuals


psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)

####
nnm
nnm$match.matrix
nnm$treat
row.names(nnm$match.matrix)


## --------------------------------------------------------------------------------------------------------------------------------
data = nnm_data %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)


x = data %>% dplyr::filter(sexual_orientation == "Asexual") %>% dplyr::select(pref_imp_ca)
x = x[,2]

y = data %>% dplyr::filter(sexual_orientation == "Straight/Heterosexual") %>% dplyr::select(pref_imp_ca)
y = y[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)
psens(x = y, y = x, Gamma = 6, GammaInc = 0.1)




## --------------------------------------------------------------------------------------------------------------------------------
# extracting outcome for both group in same order
m.pairs.pref_imp_att <- cbind(nnm_data[row.names(nnm$match.matrix), 'pref_imp_att'],
nnm_data[nnm$match.matrix, 'pref_imp_att'])

x = m.pairs.pref_imp_att[,1]
y = m.pairs.pref_imp_att[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
# extracting outcome for both group in same order
m.pairs.pref_imp_sexually_experienced <- cbind(nnm_data[row.names(nnm$match.matrix), 'pref_imp_sexually_experienced'],
nnm_data[nnm$match.matrix, 'pref_imp_sexually_experienced'])

x = m.pairs.pref_imp_sexually_experienced[,1]
y = m.pairs.pref_imp_sexually_experienced[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
# extracting outcome for both group in same order
m.pairs.pref_imp_ks<- cbind(nnm_data[row.names(nnm$match.matrix), 'pref_imp_ks'],
nnm_data[nnm$match.matrix, 'pref_imp_ks'])

x = m.pairs.pref_imp_ks[,1]
y = m.pairs.pref_imp_ks[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
# extracting outcome for both group in same order
m.pairs.pref_imp_fs <- cbind(nnm_data[row.names(nnm$match.matrix), 'pref_imp_fs'],
nnm_data[nnm$match.matrix, 'pref_imp_fs'])

x = m.pairs.pref_imp_fs[,1]
y = m.pairs.pref_imp_fs[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_ca <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_ca'],
nnm_self_data[nnm_self$match.matrix, 'self_ca'])

x = m.pairs.self_ca[,1]
y = m.pairs.self_ca[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_att <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_att'],
nnm_self_data[nnm_self$match.matrix, 'self_att'])

x = m.pairs.self_att[,1]
y = m.pairs.self_att[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_sexually_experienced <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_sexually_experienced'],
nnm_self_data[nnm_self$match.matrix, 'self_sexually_experienced'])

x = m.pairs.self_sexually_experienced[,1]
y = m.pairs.self_sexually_experienced[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1) 


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_ks <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_ks'],
nnm_self_data[nnm_self$match.matrix, 'self_ks'])

x = m.pairs.self_ks[,1]
y = m.pairs.self_ks[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_fs <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_fs'],
nnm_self_data[nnm_self$match.matrix, 'self_fs'])

x = m.pairs.self_fs[,1]
y = m.pairs.self_fs[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.self_ei <- cbind(nnm_self_data[row.names(nnm_self$match.matrix), 'self_ei'],
nnm_self_data[nnm_self$match.matrix, 'self_ei'])

x = m.pairs.self_ei[,1]
y = m.pairs.self_ei[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_nonsexrel <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_nonsexrel'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_nonsexrel'])

x = m.pairs.interest_nonsexrel[,1]
y = m.pairs.interest_nonsexrel[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_hookups <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_hookups'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_hookups'])

x = m.pairs.interest_hookups[,1]
y = m.pairs.interest_hookups[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_monrel <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_monrel'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_monrel'])

x = m.pairs.interest_monrel[,1]
y = m.pairs.interest_monrel[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_nonmonrel <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_nonmonrel'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_nonmonrel'])

x = m.pairs.interest_nonmonrel[,1]
y = m.pairs.interest_nonmonrel[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_altrel <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_altrel'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_altrel'])

x = m.pairs.interest_altrel[,1]
y = m.pairs.interest_altrel[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_single <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_single'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_single'])

x = m.pairs.interest_single[,1]
y = m.pairs.interest_single[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
#  extracting outcome for both group in same order
m.pairs.interest_parent <- cbind(nnm_rel_data[row.names(nnm_rel$match.matrix), 'interest_parent'],
nnm_rel_data[nnm_rel$match.matrix, 'interest_parent'])

x = m.pairs.interest_parent[,1]
y = m.pairs.interest_parent[,2]

psens(x = x, y = y, Gamma = 6, GammaInc = 0.1)


## --------------------------------------------------------------------------------------------------------------------------------
library(effsize)
# SE = (upper CI limit - d)/1.96

# confident-assertive
d_prefs_ca = effsize::cohen.d(pref_imp_ca ~ sexual_orientation, data = data_prefs, paired = T)
se_prefs_ca = (d_prefs_ca$conf.int[[2]] - d_prefs_ca$estimate)/1.96
se_prefs_ca


# attractive
d_prefs_att = effsize::cohen.d(pref_imp_att ~ sexual_orientation, data = data_prefs, paired = T)
se_prefs_att = (d_prefs_att$conf.int[[2]] - d_prefs_att$estimate)/1.96
se_prefs_att

# sexually expierienced
d_prefs_sexually_experienced = effsize::cohen.d(pref_imp_sexually_experienced ~ sexual_orientation, data = data_prefs, paired = T)
se_prefs_sexually_experienced = (d_prefs_sexually_experienced$conf.int[[2]] - d_prefs_sexually_experienced$estimate)/1.96
se_prefs_sexually_experienced

# kind-supportive
d_prefs_ks = effsize::cohen.d(pref_imp_ks ~ sexual_orientation, data = data_prefs, paired = T)
se_prefs_ks = (d_prefs_ks$conf.int[[2]] - d_prefs_ks$estimate)/1.96
se_prefs_ks

# financially secure-successful
d_prefs_fs = effsize::cohen.d(pref_imp_fs ~ sexual_orientation, data = data_prefs, paired = T)
se_prefs_fs = (d_prefs_fs$conf.int[[2]] - d_prefs_fs$estimate)/1.96
se_prefs_fs


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(est = -0.37, se = se_prefs_ca)

# "non-null E-value" 
evalues.MD(-0.37, se = se_prefs_ca, true = -0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(est = -0.52, se = se_prefs_att)

# "non-null E-value" 
evalues.MD(-0.52, se = se_prefs_att, true = -0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.66, se = se_prefs_sexually_experienced)

# "non-null E-value" 
evalues.MD(-0.66, se = se_prefs_sexually_experienced, true = -0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d
evalues.MD(0.16, se = se_prefs_ks) 

# "non-null E-value" --> substantial effect
evalues.MD(-0.16, se = se_prefs_ks, true = -0.26) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.21, se = se_prefs_fs) 

# "non-null E-value" --> substantial effect
evalues.MD(-0.21, se = se_prefs_fs, true = -0.26) %>%  round(2)


## --------------------------------------------------------------------------------------------------------------------------------
# SE = (upper CI limit - d)/1.96

# confident-assertive
d_self_ca = effsize::cohen.d(self_ca ~ sexual_orientation, data = data_self, paired = T)
se_self_ca = (d_self_ca$conf.int[[2]] - d_self_ca$estimate)/1.96
se_self_ca


# attractive
d_self_att = effsize::cohen.d(self_att ~ sexual_orientation, data = data_self, paired = T)
se_self_att = (d_self_att$conf.int[[2]] - d_self_att$estimate)/1.96
se_self_att

# sexually expierienced
d_self_sexually_experienced = effsize::cohen.d(self_sexually_experienced ~ sexual_orientation, data = data_self, paired = T)
se_self_sexually_experienced = (d_self_sexually_experienced$conf.int[[2]] - d_self_sexually_experienced$estimate)/1.96
se_self_sexually_experienced

# kind-supportive
d_self_ks = effsize::cohen.d(self_ks ~ sexual_orientation, data = data_self, paired = T)
se_self_ks = (d_self_ks$conf.int[[2]] - d_self_ks$estimate)/1.96
se_self_ks

# financially secure-successful
d_self_fs = effsize::cohen.d(self_fs ~ sexual_orientation, data = data_self, paired = T)
se_self_fs = (d_self_fs$conf.int[[2]] - d_self_fs$estimate)/1.96
se_self_fs

# educated-intelligent
d_self_ei = effsize::cohen.d(self_ei ~ sexual_orientation, data = data_self, paired = T)
se_self_ei = (d_self_ei$conf.int[[2]] - d_self_ei$estimate)/1.96
se_self_ei


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.43, se = se_self_ca) 

# "non-null E-value"
evalues.MD(-0.43, se = se_self_ca, true = -0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.49, se = se_self_att) 

# "non-null E-value"
evalues.MD(-0.49, se = se_self_att, true = -0.25)  



## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.65,se = se_self_sexually_experienced) 

# "non-null E-value"
evalues.MD(-0.65, se = se_self_sexually_experienced, true = -0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.40, se = se_self_ks)  

# "non-null E-value"
evalues.MD(-0.40, se = se_self_ks, true = -0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.45, se = se_self_fs) 

# "non-null E-value"
evalues.MD(-0.45, se = se_self_fs, true = -0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.17, se = se_self_ei)

# "non-null E-value"
evalues.MD(-0.17, se = se_self_ei, true = -0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# SE = (upper CI limit - d)/1.96

# interest non-sexual relationships
d_rel_nonsexrel = effsize::cohen.d(interest_nonsexrel ~ sexual_orientation, data = data_rel, paired = T)
se_rel_nonsexrel = (d_rel_nonsexrel$conf.int[[2]] - d_rel_nonsexrel$estimate)/1.96
se_rel_nonsexrel

# interest sexual relationships
d_rel_sexrel = effsize::cohen.d(interest_hookups ~ sexual_orientation, data = data_rel, paired = T)
se_rel_sexrel = (d_rel_sexrel$conf.int[[2]] - d_rel_sexrel$estimate)/1.96
se_rel_sexrel

# interest monogamous relationships
d_rel_monrel = effsize::cohen.d(interest_monrel ~ sexual_orientation, data = data_rel, paired = T)
se_rel_monrel = (d_rel_monrel$conf.int[[2]] - d_rel_monrel$estimate)/1.96
se_rel_monrel

# interest non-monogamous relationships
d_rel_nonmonrel = effsize::cohen.d(interest_nonmonrel ~ sexual_orientation, data = data_rel, paired = T)
se_rel_nonmonrel = (d_rel_nonmonrel$conf.int[[2]] - d_rel_nonmonrel$estimate)/1.96
se_rel_nonmonrel

# interest alternative committed relationships
d_rel_altrel = effsize::cohen.d(interest_altrel ~ sexual_orientation, data = data_rel, paired = T)
se_rel_altrel = (d_rel_altrel$conf.int[[2]] - d_rel_altrel$estimate)/1.96
se_rel_altrel

# interest single
d_rel_single = effsize::cohen.d(interest_single ~ sexual_orientation, data = data_rel, paired = T)
se_rel_single = (d_rel_single$conf.int[[2]] - d_rel_single$estimate)/1.96
se_rel_single

# interest parent
d_rel_parent = effsize::cohen.d(interest_parent ~ sexual_orientation, data = data_rel, paired = T)
se_rel_parent = (d_rel_parent$conf.int[[2]] - d_rel_parent$estimate)/1.96
se_rel_parent


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(1.12, se = se_rel_nonsexrel)

# "non-null E-value"
evalues.MD(1.12, se = se_rel_nonsexrel, true = 0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.67, se = se_rel_sexrel) 

# "non-null E-value"
evalues.MD(-0.67, se = se_rel_sexrel, true = -0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.46, se = se_rel_monrel) 

# "non-null E-value"
evalues.MD(-0.46, se = se_rel_monrel, true = -0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(0.20, se = se_rel_nonmonrel) 

# "non-null E-value"
evalues.MD(0.20, se = se_rel_nonmonrel, true = 0.25) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(0.57, se = se_rel_altrel) 

# "non-null E-value"
evalues.MD(0.58, se = se_rel_altrel, true = 0.25)


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(0.61, se = se_rel_single) 

# "non-null E-value"
evalues.MD(0.61, se = se_rel_single, true = 0.26) 


## --------------------------------------------------------------------------------------------------------------------------------
# for d and confidence interval closest to null
evalues.MD(-0.77, se = se_rel_parent) 


# "non-null E-value"
evalues.MD(-0.77, se = se_rel_parent, true = -0.26)


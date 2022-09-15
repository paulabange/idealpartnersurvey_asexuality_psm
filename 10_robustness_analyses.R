## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(formr)
library(ggplot2)
library(psych)
library(effsize)


## --------------------------------------------------------------------------------------------------------------------------------
data_robust_rel = nnm_robust_data_rel %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

data_robust_prefs = nnm_robust_data_prefs%>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

data_robust_self = nnm_robust_data_self %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

# check variable type of sexual_orientation
typeof(data_robust_rel$sexual_orientation)
typeof(data_robust_prefs$sexual_orientation)
typeof(data_robust_self$sexual_orientation)

# check factor levels
levels(data_robust_rel$sexual_orientation)
levels(data_robust_prefs$sexual_orientation)
levels(data_robust_self$sexual_orientation)

# for analyses, I reverse the factor levels so that 0 = Asexuals and 1 = Straight/Heterosexual 
data_robust_rel$sexual_orientation =  factor(data_robust_rel$sexual_orientation,
                                             levels=rev(levels(data_robust_rel$sexual_orientation)))
data_robust_prefs$sexual_orientation =  factor(data_robust_prefs$sexual_orientation,
                                             levels=rev(levels(data_robust_prefs$sexual_orientation)))
data_robust_self$sexual_orientation =  factor(data_robust_self$sexual_orientation,
                                             levels=rev(levels(data_robust_self$sexual_orientation)))


## --------------------------------------------------------------------------------------------------------------------------------
describeBy(data_robust_rel, data_robust_rel$sexual_orientation)

# mean
data_robust_rel %>% group_by(sexual_orientation) %>% 
  summarise_at(c("interest_nonsexrel", "interest_hookups", "interest_nonmonrel", "interest_altrel", "interest_single",
               "interest_monrel", "interest_parent"), mean, na.rm = T) 

# standard deviation
data_robust_rel %>% group_by(sexual_orientation) %>% 
  summarise_at(c("interest_nonsexrel", "interest_hookups", "interest_nonmonrel", "interest_altrel", "interest_single",
               "interest_monrel", "interest_parent"), sd, na.rm = T)
# min
data_robust_rel %>% group_by(sexual_orientation) %>% 
  summarise_at(c("interest_nonsexrel", "interest_hookups", "interest_nonmonrel", "interest_altrel", "interest_single",
               "interest_monrel", "interest_parent"), min, na.rm = T)
# max
data_robust_rel %>% group_by(sexual_orientation) %>% 
  summarise_at(c("interest_nonsexrel", "interest_hookups", "interest_nonmonrel", "interest_altrel", "interest_single",
               "interest_monrel", "interest_parent"), max, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonsexrel ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "greater",
       paired = TRUE) 

# effect size
effsize::cohen.d(interest_nonsexrel ~ sexual_orientation, data = data_robust_rel, paired = T) 



## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_hookups ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_hookups ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonmonrel ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "greater",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_nonmonrel ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_altrel ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "greater",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_altrel ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_single ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "greater",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_single ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_monrel ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_monrel ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_parent ~ sexual_orientation, 
       data = data_robust_rel,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(interest_parent ~ sexual_orientation, data = data_robust_rel, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
describeBy(data_robust_prefs, data_robust_prefs$sexual_orientation)

# mean
data_robust_prefs %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), mean, na.rm = T) 

# standard deviation
data_robust_prefs %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), sd, na.rm = T)
# min
data_robust_prefs %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), min, na.rm = T)
# max
data_robust_prefs %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), max, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ca ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ca ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_att ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_att ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_sexually_experienced ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_sexually_experienced ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ks ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ks ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_fs ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_fs ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ei ~ sexual_orientation, 
       data = data_robust_prefs,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ei ~ sexual_orientation, data = data_robust_prefs, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
describeBy(data_robust_self, data_robust_self$sexual_orientation)

# mean
data_robust_self %>% group_by(sexual_orientation) %>% 
  summarise_at(c("self_ks", "self_att", "self_fs", "self_ca",
                                         "self_ei", "self_sexually_experienced"), mean, na.rm = T) 

# standard deviation
data_robust_self %>% group_by(sexual_orientation) %>% 
  summarise_at(c("self_ks", "self_att", "self_fs", "self_ca",
                                         "self_ei", "self_sexually_experienced"), sd, na.rm = T) 
# min
data_robust_self %>% group_by(sexual_orientation) %>% 
  summarise_at(c("self_ks", "self_att", "self_fs", "self_ca",
                                         "self_ei", "self_sexually_experienced"), min, na.rm = T)
# max
data_robust_self %>% group_by(sexual_orientation) %>% 
  summarise_at(c("self_ks", "self_att", "self_fs", "self_ca",
                                         "self_ei", "self_sexually_experienced"), max, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ca ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(self_ca ~ sexual_orientation, data = data_robust_self, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_att ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(self_att ~ sexual_orientation, data = data_robust_self, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_sexually_experienced ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(self_sexually_experienced ~ sexual_orientation, data = data_robust_self, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ks ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(self_ks ~ sexual_orientation, data = data_robust_self, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_fs ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(self_fs ~ sexual_orientation, data = data_robust_self, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ei ~ sexual_orientation, 
       data = data_robust_self,
       alternative = "two.sided",
       paired = TRUE)


# effect size
d=effsize::cohen.d(self_ei ~ sexual_orientation, data = data_robust_self, paired = T)
d$estimate %>%round(2)
d$conf.int %>% round(2)


## --------------------------------------------------------------------------------------------------------------------------------
# relationship sample
nrow(nnm_robust_data_rel) 

# preference sample
nrow(nnm_robust_data_prefs) 

# self-rating sample
nrow(nnm_robust_data_self) 


## --------------------------------------------------------------------------------------------------------------------------------
# relationship sample
table(nnm_robust_data_rel$sex) # absolute numbers
round(table(nnm_robust_data_rel$sex)/nrow(nnm_robust_data_rel),3) # percentages 
# preference sample
table(nnm_robust_data_prefs$sex) # absolute numbers
round(table(nnm_robust_data_prefs$sex)/nrow(nnm_robust_data_prefs),3) # percentages 

# self-rating sample
table(nnm_robust_data_self$sex) # absolute numbers
round(table(nnm_robust_data_self$sex)/nrow(nnm_robust_data_self),3) # percentages


## --------------------------------------------------------------------------------------------------------------------------------
# for partner preference sample (because it is the largest out of the three)
gender_asexual = nnm_robust_data_prefs %>% filter(sexual_orientation == "Asexual")
gender_heterosexual = nnm_robust_data_prefs %>% filter(sexual_orientation == "Straight/Heterosexual")

# sort by subclass
gender_asexual = gender_asexual %>% 
        arrange(subclass)

gender_heterosexual = gender_heterosexual %>% 
        arrange(subclass)

# create column in dataframe "gender_asexual" that indicates whether matched pairs have same or different gender
gender_asexual$samegender = ifelse(gender_asexual$sex == gender_heterosexual$sex, "same", "different" )
table(gender_asexual$samegender) # 430 pairs have the same gender, 30 have a different gender

# which genders do the different gender pairs have?
different_gender_asexual = gender_asexual %>% select(sex, samegender, subclass) %>%  filter(samegender == "different") %>% 
  rename("gender_asexual" = sex)

different_gender_heterosexual = gender_heterosexual %>% select(sex, subclass) %>%
  rename("gender_heterosexual" = sex)

different_gender_both = left_join(different_gender_asexual, different_gender_heterosexual, by = "subclass")
nrow(different_gender_both) # total number of pairs with non-corresponding genders
round(nrow(different_gender_both)/(nrow(nnm_robust_data_prefs)/2),2) # percentage of pairs with non-corresponding gender

table(different_gender_both$gender_asexual, different_gender_both$gender_heterosexual)

woman_man = different_gender_both %>% filter(gender_asexual == "Woman" | gender_asexual == "Man") %>% filter(gender_heterosexual == "Woman" | gender_heterosexual == "Man")
nrow(woman_man)
round(nrow(woman_man)/nrow(different_gender_both),2)

woman_genderqueer = different_gender_both %>% filter(gender_asexual == "Woman" | gender_asexual == "Genderqueer/Nonbinary") %>% filter(gender_heterosexual == "Woman" | gender_heterosexual == "Genderqueer/Nonbinary")
nrow(woman_genderqueer)
round(nrow(woman_genderqueer)/nrow(different_gender_both),2)


genderqueer_man = different_gender_both %>% filter(gender_asexual == "Man" | gender_asexual == "Genderqueer/Nonbinary") %>% filter(gender_heterosexual == "Man" | gender_heterosexual == "Genderqueer/Nonbinary")
nrow(woman_man)
round(nrow(genderqueer_man)/nrow(different_gender_both),2)



## --------------------------------------------------------------------------------------------------------------------------------
nnm_robust_prefs_gender <- matchit(sexual_orientation ~  language + age + country + relationship_status + relationship_length, method = "nearest", exact = "sex", data = data_robust_nomiss_prefs)

nnm_robust_data_prefs_gender <- match.data(nnm_robust_prefs_gender)

nrow(nnm_robust_data_prefs_gender) # 862 observations
table(nnm_robust_data_prefs_gender$subclass) # data includes only pairs of two observations
dim(nnm_robust_data_prefs_gender) # 862 observations and 17 variables

table(nnm_robust_data_prefs_gender$sex) # gender 


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_robust_prefs_gender = summary(nnm_robust_prefs_gender, standardize = T)
print(summary_nnm_robust_prefs_gender)

bal.tab(nnm_robust_prefs_gender, m.threshold = 0.1, un = T, binary = "std") #smd
bal.tab(nnm_robust_prefs_gender, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_robust_prefs_gender, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_robust_prefs_gender, type = "jitter", interactive = FALSE)

# distributional density/histogram
bal.plot(nnm_robust_prefs_gender, var.name = "age", which = "both", grid = T)
bal.plot(nnm_robust_prefs_gender, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_robust_prefs_gender, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_robust_prefs_gender, var.name = "language", which = "both", grid = T)
bal.plot(nnm_robust_prefs_gender, var.name = "country", which = "both", grid = T) # not readable
bal.plot(nnm_robust_prefs_gender, var.name = "sex", which = "both", grid = T)

# remove country variable for love plot
nnm_robust_without_country_prefs_gender = nnm_robust_prefs_gender
nnm_robust_without_country_prefs_gender$X[c("country")] <- NULL 

# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length", "sex_Woman", "sex_Man", "sex_Genderqueer/Nonbinary"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length", "Woman", "Man", "Genderqueer/Nonbinary"))

love.plot(bal.tab(nnm_robust_without_country_prefs_gender, m.threshold = 0.1, binary = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before Matching", "After Matching"),
          var.names = v,
          binary = "std") 


## --------------------------------------------------------------------------------------------------------------------------------
data_robust_prefs_gender = nnm_robust_data_prefs_gender%>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

# check variable type of sexual_orientation
typeof(data_robust_prefs_gender$sexual_orientation)

# check factor levels
levels(data_robust_prefs_gender$sexual_orientation)

# for analyses, I reverse the factor levels so that 0 = Asexuals and 1 = Straight/Heterosexual 
data_robust_prefs_gender$sexual_orientation =  factor(data_robust_prefs_gender$sexual_orientation,
                                             levels=rev(levels(data_robust_prefs_gender$sexual_orientation)))


## --------------------------------------------------------------------------------------------------------------------------------
describeBy(data_robust_prefs_gender, data_robust_prefs_gender$sexual_orientation)

# mean
data_robust_prefs_gender %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), mean, na.rm = T) 

# standard deviation
data_robust_prefs_gender %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), sd, na.rm = T)
# min
data_robust_prefs_gender %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), min, na.rm = T)
# max
data_robust_prefs_gender %>% group_by(sexual_orientation) %>% 
  summarise_at(c("pref_imp_ks", "pref_imp_att", "pref_imp_fs", "pref_imp_ca",
                                         "pref_imp_ei", "pref_imp_sexually_experienced"), max, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ca ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ca ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_att ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_att ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_sexually_experienced ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "less",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_sexually_experienced ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ks ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ks ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_fs ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_fs ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ei ~ sexual_orientation, 
       data = data_robust_prefs_gender,
       alternative = "two.sided",
       paired = TRUE)


# effect size
effsize::cohen.d(pref_imp_ei ~ sexual_orientation, data = data_robust_prefs_gender, paired = T)


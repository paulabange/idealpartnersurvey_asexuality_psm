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

## set options for bal.tab function in cobalt package
set.cobalt.options(binary = "std") # for binary covariates used in the matching procedure the bal.tab function will now display standardized mean differences instead of the raw difference


## --------------------------------------------------------------------------------------------------------------------------------
data_main = read.csv(file = "data/data_main.csv")[,-1]


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss <- data_main %>% select(id, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
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
om = matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "optimal", data = data_main_nomiss) # default setting: propensity score is estimated using logistic regression

om_data = match.data(om) # construct a matched data set from match it object om

nrow(om_data) # 780 observations
table(om_data$subclass) # data includes only pairs of two observations
dim(om_data) # 780 observations and 16 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_om = summary(om, standardize = T)
print(summary_om)

bal.tab(om, m.threshold = 0.1, un = T, binary = "std") #sm
bal.tab(om, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(om, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks worse than before matching

# Jitter plot
plot(om, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(om, var.name = "age", which = "both", grid = T)
bal.plot(om, var.name = "relationship_length", which = "both", grid = T)

bal.plot(om, var.name = "relationship_status", which = "both", grid = T)
bal.plot(om, var.name = "language", which = "both", grid = T) # looks worse than before matching, some countries are not represented for heterosexual individuals anymore 
bal.plot(om, var.name = "country", which = "both", grid = T) # recht unübersichtlich

bal.plot(om, var.name = "distance", which = "both",
         type = "histogram", mirror = TRUE) # for propensity score

love.plot(bal.tab(om, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# not readable due to country variable

# remove country variable for love plot
om_without_country = om
om_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(om_without_country, m.threshold = 0.1, binardy = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 

# In cobalt, you can use the formula method as follows:
# 
# cobalt::love.plot(y ~ x1 + x2, data = D, weights = get.w(m.out))
# Because you control which covariates are used, you don't have to display the covariates you don't want.
# 
# In MatchIt, one hacky way to do this would be to manually remove those variables from the matchit object. That is, you could run
# 
# m.out$X[c("location")] <- NULL
# before running summary() and the location variable will be removed from the output.


## love plot using alternate variable names
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length"),
                new = c("Propensity scores", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length"))

love.plot(bal.tab(om_without_country, m.threshold = 0.1, binardy = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Unmatched", "Optimal Matching"),
          var.names = v) 




## --------------------------------------------------------------------------------------------------------------------------------
nnm <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", data = data_main_nomiss)

nnm_data <- match.data(nnm)

nrow(nnm_data) # 780 observations
table(nnm_data$subclass) # data includes only pairs of two observations
dim(nnm_data) # 780 observations and 17 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm = summary(nnm, standardize = T)
print(summary_nnm)

bal.tab(nnm, m.threshold = 0.1, un = T, binary = "std") #smd
bal.tab(nnm, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(nnm, var.name = "age", which = "both", grid = T)
bal.plot(nnm, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm, var.name = "language", which = "both", grid = T)
bal.plot(nnm, var.name = "country", which = "both", grid = T) # not readable


love.plot(bal.tab(nnm, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# not readable due to country variable

# remove country variable for love plot
nnm_without_country = nnm
nnm_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 


## love plot using alternate variable names
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length"))

love.plot(bal.tab(nnm_without_country, m.threshold = 0.1, binary = "std"),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before Matching", "After Matching"),
          var.names = v)
?love.plot


## --------------------------------------------------------------------------------------------------------------------------------
nnm_25 <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", caliper = 0.25, data = data_main_nomiss)

nnm_25_data <- match.data(nnm_25)

nrow(nnm_25_data) # 778 observations
table(nnm_25_data$subclass) # data includes only pairs of two observations
dim(nnm_25_data) # 778 observations and 16 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_25 = summary(nnm_25, standardize = T)
print(summary_nnm_25)

bal.tab(nnm_25, m.threshold = 0.1, un = T, binary = "std") #smd
bal.tab(nnm_25, v.threshold = 2, un = T)#vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_25, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# sieht besser aus als vorher

# Jitter plot
plot(nnm_25, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(nnm_25, var.name = "age", which = "both", grid = T)
bal.plot(nnm_25, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_25, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_25, var.name = "language", which = "both", grid = T)
bal.plot(nnm_25, var.name = "country", which = "both", grid = T)  # not readable


love.plot(bal.tab(nnm_25, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# nt readable due to country variable

# remove country variable for love plot
nnm_25_without_country = nnm_25
nnm_25_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_25_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 


## --------------------------------------------------------------------------------------------------------------------------------
nnm_10 <- matchit(sexual_orientation ~ language + age + country + relationship_status + relationship_length, method = "nearest", caliper = 0.1, data = data_main_nomiss)

nnm_10_data <- match.data(nnm_10)

nrow(nnm_10_data) # 774 observations
table(nnm_10_data$subclass) # data includes only pairs of two observations
dim(nnm_10_data) # 774 observations and 16 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_10 = summary(nnm_10, standardize = T)
print(summary_nnm_10)
round(tail(summary_nnm_10$sum.matched), 2)

bal.tab(nnm_10, m.threshold = 0.1, un = T) #smd
bal.tab(nnm_10, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_25, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# sieht besser aus als vorher

# Jitter plot
plot(nnm_10, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(nnm_10, var.name = "age", which = "both", grid = T)
bal.plot(nnm_10, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_10, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_10, var.name = "language", which = "both", grid = T)
bal.plot(nnm_10, var.name = "country", which = "both", grid = T) # not readable


love.plot(bal.tab(nnm_10, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# not readable due to country variable

# remove country variable for love plot
nnm_10_without_country = nnm
nnm_10_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_10_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 


## --------------------------------------------------------------------------------------------------------------------------------
## looking at the percent balance improvement
# nnm vs nnm_25
summary_nnm$reduction[,1]
summary_nnm_25$reduction[,1]
comp = ifelse(summary_nnm$reduction[,1]==summary_nnm_25$reduction[,1], "Yes",
             summary_nnm$reduction[,1]-summary_nnm_25$reduction[,1])
table(comp)


# nnm vs nnm_10
summary_nnm$reduction[,1]
summary_nnm_10$reduction[,1]
comp2 = ifelse(summary_nnm$reduction[,1]==summary_nnm_10$reduction[,1], "Yes",
             summary_nnm$reduction[,1]-summary_nnm_10$reduction[,1])
table(comp2)



### smd
# nnm vs nnm_25
comp_smd = ifelse(summary_nnm$sum.matched[,1]==summary_nnm_25$sum.matched[,1], "Yes",
             summary_nnm$sum.matched[,1]-summary_nnm_25$sum.matched[,1])
table(comp_smd)

# nnm vs nnm_10
comp2_smd = ifelse(summary_nnm$sum.matched[,1]==summary_nnm_10$sum.matched[,1], "Yes",
             summary_nnm$sum.matched[,1]-summary_nnm_10$sum.matched[,1])
table(comp2_smd)


## --------------------------------------------------------------------------------------------------------------------------------
# smds
love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          weights = list(w1 = get.w(om),
                         w2 = get.w(nnm),
                         w3 = get.w(nnm_25),
                         w4 = get.w(nnm_10)),
          thresholds = c(m = .1),
          sample.names = c("Before Matching", "Optimal Matching", "Nearest-Neighbor-Matching", "Nearest-Neighbor-Matching with caliper = 0.25", "Nearest-Neighbor-Matching with caliper = 0.10"),
          var.names = v,
          s.d.denom = "treated")


# variance ratios
# alternatve variable names 
v.vr = data.frame(old = c("distance", "age",
                        "relationship_length"),
                new = c("Propensity scores", "Age", "Relationship length"))

love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "variance.ratios",
          grid = T,
          stars = "raw",
          abs = F,
          thresholds = c(v = 2),
          weights = list(w1 = get.w(om),
                         w2 = get.w(nnm),
                         w3 = get.w(nnm_25),
                         w4 = get.w(nnm_10)),
          sample.names = c("Before Matching", "Optimal Matching", "Nearest-Neighbor-Matching", "Nearest-Neighbor-Matching with caliper = 0.25", "Nearest-Neighbor-Matching with caliper = 0.10"),
          var.names = v.vr,
          s.d.denom = "treated") 


## --------------------------------------------------------------------------------------------------------------------------------
# smds
love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          weights = list(w1 = get.w(nnm),
                         w2 = get.w(nnm_25),
                         w3 = get.w(nnm_10)),
          thresholds = c(m = .1),
          sample.names = c("Unadjusted", "Nearest-Neighbor-Matching", "NNM 0.25", "NNM 0.10")) 

# variance ratios
love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "variance.ratios",
          grid = T,
          stars = "raw",
          abs = F,
          weights = list(w1 = get.w(om),
                         w2 =  get.w(nnm_25),
                         w3 = get.w(nnm_10)),
          sample.names = c("Unadjusted","Nearest-Neighbor-Matching", "NNM 0.25", "NNM 0.10")) 


## --------------------------------------------------------------------------------------------------------------------------------
# stat = "variance.ratios"
# add confidence interval: +
#  geom_errorbar(aes(ymin=lower,ymax=upper))



## --------------------------------------------------------------------------------------------------------------------------------
# create subsamples based on sexual orientation
asexual_nnm = nnm_data %>% filter(sexual_orientation == "Asexual")
heterosex_nnm = nnm_data %>%  filter(sexual_orientation =="Straight/Heterosexual")

# match subsamples based on assigned subclass (propensity-score matched pairs will be matched)
matched = inner_join(asexual_nnm, heterosex_nnm, by = "subclass")


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair comes from same country, if that is the case country is printed
matched$same_country = ifelse(matched$country.x == matched$country.y, matched$country.x, "no")

# how many pairs per country
table(matched$same_country) 

# countries of pairs from differing countries
matched$country_1 = ifelse(matched$country.x != matched$country.y, matched$country.x, "no")
matched$country_2 = ifelse(matched$country.x != matched$country.y, matched$country.y, "no")

# table 
xtabs(~country_1 + country_2, matched) 

# how many pairs share same country?
sum(matched$same_country != "no") # 358
sum(matched$same_country != "no")/nrow(matched) # 92%


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair answered survey in same language, if that is the case that language is printed
matched$same_language = ifelse(matched$language.x == matched$language.y, matched$language.x, "no")

# how many pairs per language
table(matched$same_language) 

# countries of pairs with different languages
matched$language_1 = ifelse(matched$language.x != matched$language.y, matched$language.x, "no")
matched$language_2 = ifelse(matched$language.x != matched$language.y, matched$language.y, "no")

# table 
xtabs(~language_1 + language_2, matched) 

# how many pairs share same language?
sum(matched$same_language != "no") # 376
sum(matched$same_language != "no")/nrow(matched) # 96%


## --------------------------------------------------------------------------------------------------------------------------------
describe.by(nnm_data,nnm_data$sexual_orientation)


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss_self <- data_main %>% select(id, language, age, sex, country, relationship_status, relationship_length, sexual_orientation,
                                         self_ks, self_att, self_fs,self_ca, self_ei, self_sexually_experienced ) %>%
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

nrow(nnm_self_data) # 772 observations
table(nnm_self_data$subclass) # data includes only pairs of two observations
dim(nnm_self_data) # 772 observations and 17 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_self = summary(nnm_self, standardize = T)
print(summary_nnm_self)

bal.tab(nnm_self, m.threshold = 0.1, un = T) #smd
bal.tab(nnm_self, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_self, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_self, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(nnm_self, var.name = "age", which = "both", grid = T)
bal.plot(nnm_self, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_self, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_self, var.name = "language", which = "both", grid = T)
bal.plot(nnm_self, var.name = "country", which = "both", grid = T) # not readable


love.plot(bal.tab(nnm_self, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# not readable due to country variable

# remove country variable for love plot
nnm_self_without_country = nnm_self
nnm_self_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_self_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 


## --------------------------------------------------------------------------------------------------------------------------------
# create subsamples based on sexual orientation
asexual_nnm_self = nnm_self_data %>% filter(sexual_orientation == "Asexual")
heterosex_nnm_self = nnm_self_data %>%  filter(sexual_orientation =="Straight/Heterosexual")

# match subsamples based on assigned subclass (propensity-score matched pairs will be matched)
matched_self = inner_join(asexual_nnm_self, heterosex_nnm_self, by = "subclass")


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair comes from same country, if so country is printed
matched_self$same_country = ifelse(matched_self$country.x == matched_self$country.y, matched_self$country.x, "no")

# how many pairs per country
table(matched_self$same_country) 

# countries of pairs from differing countries
matched_self$country_1 = ifelse(matched_self$country.x != matched_self$country.y, matched_self$country.x, "no")
matched_self$country_2 = ifelse(matched_self$country.x != matched_self$country.y, matched_self$country.y, "no")

# table 
xtabs(~country_1 + country_2, matched_self) 

# how many pairs share same country?
sum(matched_self$same_country != "no") # 351
sum(matched_self$same_country != "no")/nrow(matched_self) # 91%


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair asnwered survey in same language, if so that language is printed
matched_self$same_language = ifelse(matched_self$language.x == matched_self$language.y, matched_self$language.x, "no")


# how many pairs per language
table(matched_self$same_language) 

# countries of pairs with different languages
matched_self$language_1 = ifelse(matched_self$language.x != matched_self$language.y, matched_self$language.x, "no")
matched_self$language_2 = ifelse(matched_self$language.x != matched_self$language.y, matched_self$language.y, "no")

# table 
xtabs(~language_1 + language_2, matched_self) 

# how many pairs share same language?
sum(matched_self$same_language != "no") # 368
sum(matched_self$same_language != "no")/nrow(matched_self) # 95%


## --------------------------------------------------------------------------------------------------------------------------------
describe.by(nnm_self_data, nnm_self_data$sexual_orientation) 


## --------------------------------------------------------------------------------------------------------------------------------
# MatchIt does not allow missing values
data_main_nomiss_rel <- data_main %>% select(id, language, age, sex, country, relationship_status, relationship_length,
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

nrow(nnm_rel_data) # 646 observations
table(nnm_rel_data$subclass) # data includes only pairs of two observations
dim(nnm_rel_data) # 646 observations and 18 variables


## --------------------------------------------------------------------------------------------------------------------------------
summary_nnm_rel = summary(nnm_rel, standardize = T)
print(summary_nnm_rel)

bal.tab(nnm_rel, m.threshold = 0.1, un = T) #smd
bal.tab(nnm_rel, v.threshold = 2, un = T) #vr (can only be calculated for continuous variables)


## --------------------------------------------------------------------------------------------------------------------------------
# Q-Q plot
plot(nnm_rel, type = "qq", interactive = FALSE, which.xs = c("age", "relationship_length"))
# looks better after matching

# Jitter plot
plot(nnm_rel, type = "jitter", interactive = FALSE)

# with cobalt package:
# distributional density/histogram
bal.plot(nnm_rel, var.name = "age", which = "both", grid = T)
bal.plot(nnm_rel, var.name = "relationship_length", which = "both", grid = T)

bal.plot(nnm_rel, var.name = "relationship_status", which = "both", grid = T)
bal.plot(nnm_rel, var.name = "language", which = "both", grid = T)
bal.plot(nnm_rel, var.name = "country", which = "both", grid = T) # not readable


love.plot(bal.tab(nnm_rel, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 
# not readable due to coutnry variable

# remove country variable for love plot
nnm_rel_without_country = nnm_rel
nnm_rel_without_country$X[c("country")] <- NULL 

love.plot(bal.tab(nnm_rel_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F) 


## --------------------------------------------------------------------------------------------------------------------------------
# create subsamples based on sexual orientation
asexual_nnm_rel = nnm_rel_data %>% filter(sexual_orientation == "Asexual")
heterosex_nnm_rel = nnm_rel_data %>%  filter(sexual_orientation =="Straight/Heterosexual")

# match subsamples based on assigned subclass (propensity-score matched pairs will be matched)
matched_rel = inner_join(asexual_nnm_rel, heterosex_nnm_rel, by = "subclass")


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair comes from same country, if so country is printed
matched_rel$same_country = ifelse(matched_rel$country.x == matched_rel$country.y, matched_rel$country.x, "no")

# how many pairs per country
table(matched_rel$same_country) 

# countries of pairs from differing countries
matched_rel$country_1 = ifelse(matched_rel$country.x != matched_rel$country.y, matched_rel$country.x, "no")
matched_rel$country_2 = ifelse(matched_rel$country.x != matched_rel$country.y, matched_rel$country.y, "no")

# table 
xtabs(~country_1 + country_2, matched_rel) 

# how many pairs share same country?
sum(matched_rel$same_country != "no") # 293
sum(matched_rel$same_country != "no")/nrow(matched_rel) # 90%


## --------------------------------------------------------------------------------------------------------------------------------
# create variable indicating whether pair answered survey in same language, if so that language is printed
matched_rel$same_language = ifelse(matched_rel$language.x == matched_rel$language.y, matched_rel$language.x, "no")


# how many pairs per language
table(matched_rel$same_language) 

# countries of pairs with different languages
matched_rel$language_1 = ifelse(matched_rel$language.x != matched_rel$language.y, matched_rel$language.x, "no")
matched_rel$language_2 = ifelse(matched_rel$language.x != matched_rel$language.y, matched_rel$language.y, "no")

# table 
xtabs(~language_1 + language_2, matched_rel) 

# how many pairs share same language?
sum(matched_rel$same_language != "no") # 308
sum(matched_rel$same_language != "no")/nrow(matched_rel) # 94%


## --------------------------------------------------------------------------------------------------------------------------------
describe.by(nnm_rel_data, nnm_rel_data$sexual_orientation) 


## --------------------------------------------------------------------------------------------------------------------------------
# for partner preference psm sample
write.csv(nnm_data,
          file = "data/data_prefs.csv")

# for self-ratings psm sample
write.csv(nnm_self_data,
          file = "data/data_self.csv")

# for preferred relationship options psm sample
write.csv(nnm_rel_data,
          file = "data/data_rel.csv")


## --------------------------------------------------------------------------------------------------------------------------------
# for partner preference sample
data_prefs_unmatched = anti_join(data_main_nomiss, nnm_data) # saves only unmatched cases
write.csv(data_prefs_unmatched,
          file = "data/data_prefs_unmatched.csv")

# for self-rating sample
data_self_unmatched = anti_join(data_main_nomiss_self, nnm_self_data) 
write.csv(data_self_unmatched,
          file = "data/data_self_unmatched.csv")

# for preferred relationship optiopns sample
data_rel_unmatched = anti_join(data_main_nomiss_rel, nnm_rel_data) 
write.csv(data_rel_unmatched,
          file = "data/data_rel_unmatched.csv")


## --------------------------------------------------------------------------------------------------------------------------------
# Using alternate variable names
v <- data.frame(old = c("distance", "language_chinese", "language_japanese", "language_german","language_english", "language_italian", "language_danish", "language_spanish", "language_russian", "language_portuguese", "language_french", "age", "relationship_status_Long-term", "relationship_status_No relationship", "relationship_status_Short-term",
                        "relationship_length"),
                new = c("Propensity score", "Chinese", "Japanese", "German", "English", "Italian", "Danish", "Spanish", "Russian", "Portuguese", "French", "Long-term relationship", "No relationship", "Dating/Sexual Relationship","Age",  "Relationship length"))

love.plot(bal.tab(nnm_without_country, m.threshold = 0.1),
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          sample.names = c("Before matching", "After matching"),
          var.names = v) 



## --------------------------------------------------------------------------------------------------------------------------------
# smds
love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "mean.diffs",
          grid = T,
          stars = "raw",
          abs = F,
          weights = list(w1 = get.w(om),
                         w2 = get.w(nnm),
                         w3 = get.w(nnm_25),
                         w4 = get.w(nnm_10)),
          thresholds = c(m = .1),
          s.d.denom = "pooled",
          sample.names = c("Unmatched", "Optimal matching", "Nearest-neighbor-matching", "Nearest-neighbor-matching with caliper = 0.25", "Nearest-neighbor-matching with caliper = 0.10"),
          var.names = v) 


# variance ratios
love.plot(sexual_orientation ~ language + age + 
            relationship_status + relationship_length,
          data = data_main_nomiss,
          stat = "variance.ratios",
          grid = T,
          stars = "raw",
          abs = F,
          weights = list(w1 = get.w(om),
                         w2 = get.w(nnm)),
          sample.names = c("Unadjusted", "Optimal Matching", "Nearest-Neighbor-Matching")) 


## --------------------------------------------------------------------------------------------------------------------------------
# check matched pairs
# matchedobject$match.matrix

# nnm --> order in which treated individuals is picked "largest", "smallest" or "random" (m.order = "largest")
# nnm --> caliper (caliper = number) 8caliper = number of standard deviations of difference which can be tolerated between PS of the matched pair (default = 0 -- no caliper)


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(formr)
library(ggplot2)
library(psych)
library(rcompanion) # for Cramer's V
library(confintr)


## --------------------------------------------------------------------------------------------------------------------------------
# matched samples
data_rel = read.csv(file = "data/data_rel.csv")[,-1]
data_prefs = read.csv(file = "data/data_prefs.csv")[,-1]
data_self = read.csv(file = "data/data_self.csv")[,-1]

# unmatched heterosexual sample (includes only heterosexuals because during psm no asexual was discarded)
data_rel_unmatched = read.csv(file = "data/data_rel_unmatched.csv")[,-1]
data_prefs_unmatched = read.csv(file = "data/data_prefs_unmatched.csv")[,-1]
data_self_unmatched = read.csv(file = "data/data_self_unmatched.csv")[,-1]

nrow(data_rel_unmatched)
describe.by(asex_v_umhetero_rel, asex_v_umhetero_rel$sexual_orientation)
nrow(data_prefs_unmatched)
describe.by(asex_v_umhetero_prefs, asex_v_umhetero_prefs$sexual_orientation)
nrow(data_self_unmatched)
describe.by(asex_v_umhetero_self, asex_v_umhetero_prefs$sexual_orientation)

# asexual sample 
asexuals_rel = data_rel %>% filter(sexual_orientation == "Asexual")
asexuals_prefs = data_prefs %>% filter(sexual_orientation == "Asexual")
asexuals_self = data_self %>% filter(sexual_orientation == "Asexual")

# matched heterosexual sample (also from partner preference psm sample)
heterosexuals_rel = data_rel %>%  filter(sexual_orientation == "Straight/Heterosexual")
heterosexuals_prefs = data_prefs %>%  filter(sexual_orientation == "Straight/Heterosexual")
heterosexuals_self = data_self %>%  filter(sexual_orientation == "Straight/Heterosexual")


## --------------------------------------------------------------------------------------------------------------------------------
# Asexuals v. matched heterosexuals: 
asex_v_hetero_rel = data_rel
asex_v_hetero_prefs = data_prefs
asex_v_hetero_self = data_self

# Asexuals v. unmatched heterosexuals:
asex_v_umhetero_rel = full_join(asexuals_rel, data_rel_unmatched)
asex_v_umhetero_prefs = full_join(asexuals_prefs, data_prefs_unmatched)
asex_v_umhetero_self = full_join(asexuals_self, data_self_unmatched)

# Matched heterosexuals v. unmatched heterosexuals:
# add variable matched (matched or unmatched) as grouping variable to data sets
data_rel_unmatched$matched = "unmatched"
heterosexuals_rel$matched = "matched"

data_prefs_unmatched$matched = "unmatched"
heterosexuals_prefs$matched = "matched"

data_self_unmatched$matched = "unmatched"
heterosexuals_self$matched = "matched"

# merge data sets
hetero_v_umhetero_rel = full_join(heterosexuals_rel, data_rel_unmatched)
hetero_v_umhetero_prefs = full_join(heterosexuals_prefs, data_prefs_unmatched)
hetero_v_umhetero_self = full_join(heterosexuals_self, data_self_unmatched)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview 
describe.by(data_prefs_unmatched)


## --------------------------------------------------------------------------------------------------------------------------------
sort(table(data_prefs_unmatched$country), decreasing = T) # absolute numbers
sort(round(table(data_prefs_unmatched$country)/sum(table(data_prefs_unmatched$country)),4)*100, decreasing = T) # in percent


## --------------------------------------------------------------------------------------------------------------------------------
sort(table(data_prefs_unmatched$language), decreasing = T) # absolute numbers
sort(round(table(data_prefs_unmatched$language)/sum(table(data_prefs_unmatched$language)),4)*100, decreasing = T) # in percent


## --------------------------------------------------------------------------------------------------------------------------------
# mean, sd, min, max
data_prefs_unmatched %>%
  summarise(mean(age, na.rm = T), 
            min(age, na.rm = T), 
            max(age, na.rm = T),
            sd(age, na.rm = T))


# Histogram 
ggplot(data_prefs_unmatched, aes(x = age, fill = sexual_orientation)) + 
  geom_histogram(position = "dodge")


## --------------------------------------------------------------------------------------------------------------------------------
sort(table(data_prefs_unmatched$relationship_status), decreasing = T) # absolute numbers
sort(round(table(data_prefs_unmatched$relationship_status)/sum(table(data_prefs_unmatched$relationship_status)),4)*100, decreasing = T) # in percent


## --------------------------------------------------------------------------------------------------------------------------------
# mean, sd, min, max
data_prefs_unmatched %>%
  summarise(mean(relationship_length, na.rm = T), 
            min(relationship_length, na.rm = T), 
            max(relationship_length, na.rm = T),
            sd(relationship_length, na.rm = T))


# Histogram by group 
ggplot(data_prefs, aes(x = relationship_length, fill = sexual_orientation)) + 
  geom_histogram(position = "dodge")


## --------------------------------------------------------------------------------------------------------------------------------
data_rel = data_rel %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

data_prefs = data_prefs %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)

data_self = data_self %>% 
        group_by(sexual_orientation) %>%
        arrange(subclass, .by_group = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
# assumptions not met for chi square test: some cells contain less than 5 observations; therefore a Fisher's exact test ist calculated
matrix_country = as.matrix(table(data_prefs$country, data_prefs$sexual_orientation), header=TRUE, row.names=1)
table(d < 5) # absolute number of cells containing less than 5 counts
nrow(matrix_country) * 2 # number of cells

round(table(matrix_country<5)/(nrow(matrix_country)*2),2) # 53% of cells have counts less than 5 

# running Fisher's exact test
fisher_test_country = fisher.test(matrix_country, simulate.p.value = TRUE)
fisher_test_country$p.value

# chi square test
chisq_country = chisq.test(data_prefs$sexual_orientation, data_prefs$country) 
chisq_country 
chisq_country$observed # observed counts
round(chisq_country$expected,2) # expected counts


# calculate Cramer's V
ci_cramersv(chisq_country, type = "chi-squared")


## --------------------------------------------------------------------------------------------------------------------------------
chisq_language = chisq.test(data_prefs$sexual_orientation, data_prefs$language) 
chisq_language
chisq_language$observed
round(chisq_language$expected,2)

# calculate Cramer's V
matrix_language = as.matrix(table(data_prefs$language, data_prefs$sexual_orientation), header=TRUE,
                     row.names=1)
round(cramerV(matrix_language, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(age ~ sexual_orientation, data = data_prefs,
       alternative = "two.sided",
       paired = TRUE)

# effect size
effsize::cohen.d(age ~ sexual_orientation, data = data_prefs, paired = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------
chisq_relstat = chisq.test(data_prefs$sexual_orientation, data_prefs$relationship_status) 
chisq_relstat
chisq_relstat$observed
round(chisq_relstat$expected,2)

# calculate Cramer's V
matrix_relstat = as.matrix(table(data_prefs$relationship_status, data_prefs$sexual_orientation), header=TRUE,
                     row.names=1)
round(cramerV(matrix_relstat, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(relationship_length ~ sexual_orientation, data = data_prefs,
       alternative = "two.sided",
       paired = TRUE)

# effect size
effsize::cohen.d(relationship_length ~ sexual_orientation, data = data_prefs, paired = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------



## --------------------------------------------------------------------------------------------------------------------------------
# assumptions not met for chi square test: some cells contain less than 5 observations; therefore a Fisher's exact test ist calculated
matrix_asex_v_umhetero_country = as.matrix(table(asex_v_umhetero_prefs$country, asex_v_umhetero_prefs$sexual_orientation), header=TRUE, row.names=1)
table(matrix_asex_v_umhetero_country < 5) # absolute number of cells containing less than 5 counts
nrow(matrix_asex_v_umhetero_country) * 2 # number of cells

round(table(matrix_asex_v_umhetero_country<5)/(nrow(matrix_asex_v_umhetero_country)*2),2) # 64% of cells have counts less than 5 

# running Fisher's exact test
fisher_test_asex_v_umhetero_country = fisher.test(matrix_asex_v_umhetero_country, simulate.p.value = TRUE)
fisher_test_asex_v_umhetero_country$p.value

# chi square test
chisq_asex_v_umhetero_country = chisq.test(asex_v_umhetero_prefs$sexual_orientation, asex_v_umhetero_prefs$country) 
chisq_asex_v_umhetero_country
chisq_asex_v_umhetero_country$observed # observed counts
round(chisq_asex_v_umhetero_country$expected,2) # expected counts


# calculate Cramer's V
ci_cramersv(chisq_asex_v_umhetero_country, type = "chi-squared")


## --------------------------------------------------------------------------------------------------------------------------------
language_asex_v_umhetero = chisq.test(asex_v_umhetero_prefs$language, asex_v_umhetero_prefs$sexual_orientation)
language_asex_v_umhetero
language_asex_v_umhetero$observed
round(language_asex_v_umhetero$expected,2)

# calculate Cramer's V
matrix_asex_v_umhetero_language = as.matrix(table(asex_v_umhetero_prefs$language, asex_v_umhetero_prefs$sexual_orientation), header=TRUE,
                     row.names=1)
round(cramerV(matrix_asex_v_umhetero_language, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(age ~ sexual_orientation, data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(age ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
relstat_asex_v_umhetero = chisq.test(asex_v_umhetero_prefs$relationship_status, asex_v_umhetero_prefs$sexual_orientation)
relstat_asex_v_umhetero
relstat_asex_v_umhetero$observed
round(relstat_asex_v_umhetero$expected,2)

# calculate Cramer's V
matrix_asex_v_umhetero_relstat = as.matrix(table(asex_v_umhetero_prefs$relationship_status, asex_v_umhetero_prefs$sexual_orientation), header=TRUE,
                     row.names=1)
round(cramerV(matrix_asex_v_umhetero_relstat, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(relationship_length ~ sexual_orientation, data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(relationship_length ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe.by(asex_v_umhetero_prefs, asex_v_umhetero_prefs$sexual_orientation)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ca ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ca ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_att ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_att ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_sexually_experienced ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_sexually_experienced ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ks ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ks ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_fs ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_fs ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ei ~ sexual_orientation, 
       data = asex_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ei ~ sexual_orientation, data = asex_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe.by(asex_v_umhetero_self, asex_v_umhetero_self$sexual_orientation)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ca ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ca ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_att ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_att ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_sexually_experienced ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_sexually_experienced ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ks ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ks ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_fs ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_fs ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ei ~ sexual_orientation, 
       data = asex_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ei ~ sexual_orientation, data = asex_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe.by(asex_v_umhetero_rel, asex_v_umhetero_rel$sexual_orientation)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonsexrel ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(interest_nonsexrel ~ sexual_orientation, data = asex_v_umhetero_rel , paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_hookups ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_hookups ~ sexual_orientation, data = data_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonmonrel ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_nonmonrel ~ sexual_orientation, data = asex_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_altrel ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_altrel ~ sexual_orientation, data = asex_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_single ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_single ~ sexual_orientation, data = asex_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_monrel ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_monrel ~ sexual_orientation, data = asex_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_parent ~ sexual_orientation, 
       data = asex_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_parent ~ sexual_orientation, data = asex_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
# assumptions not met for chi square test: some cells contain less than 5 observations; therefore a Fisher's exact test ist calculated
matrix_hetero_v_umhetero_country = as.matrix(table(hetero_v_umhetero_prefs$country, hetero_v_umhetero_prefs$matched), header=TRUE, row.names=1)
table(matrix_hetero_v_umhetero_country < 5) # absolute number of cells containing less than 5 counts
nrow(matrix_hetero_v_umhetero_country) * 2 # number of cells

round(table(matrix_hetero_v_umhetero_country<5)/(nrow(matrix_hetero_v_umhetero_country)*2),2) # 64 % of cells have counts less than 5 

# running Fisher's exact test
fisher.test(matrix_hetero_v_umhetero_country, simulate.p.value = TRUE)

# chi square test
chisq_hetero_v_umhetero_country = chisq.test(matrix_hetero_v_umhetero_country)
chisq_hetero_v_umhetero_country
chisq_hetero_v_umhetero_country$observed # observed counts
round(chisq_hetero_v_umhetero_country$expected,2) # expected counts


# calculate Cramer's V
ci_cramersv(chisq_hetero_v_umhetero_country, type = "chi-squared")


## --------------------------------------------------------------------------------------------------------------------------------
language_hetero_v_umhetero = chisq.test(hetero_v_umhetero_prefs$language, hetero_v_umhetero_prefs$matched)
language_hetero_v_umhetero
language_hetero_v_umhetero$observed
round(language_hetero_v_umhetero$expected,2)

# calculate Cramer's V
matrix_hetero_v_umhetero_language = as.matrix(table(hetero_v_umhetero_prefs$language, hetero_v_umhetero_prefs$matched), header=TRUE,
                     row.names=1)
round(cramerV(matrix_hetero_v_umhetero_language, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(age ~ matched, data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(age ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
relstat_hetero_v_umhetero = chisq.test(hetero_v_umhetero_prefs$relationship_status, hetero_v_umhetero_prefs$matched)
relstat_hetero_v_umhetero
relstat_hetero_v_umhetero$observed
round(relstat_hetero_v_umhetero$expected,2)

# calculate Cramer's V
matrix_hetero_v_umhetero_relstat = as.matrix(table(hetero_v_umhetero_prefs$relationship_status, hetero_v_umhetero_prefs$matched), header=TRUE,
                     row.names=1)
round(cramerV(matrix_hetero_v_umhetero_relstat, ci = T),2)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(relationship_length ~ matched, data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(relationship_length ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe.by(hetero_v_umhetero_prefs, hetero_v_umhetero_prefs$matched) 


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ca ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ca ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_att ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_att ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_sexually_experienced ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_sexually_experienced ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ks ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ks ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_fs ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_fs ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(pref_imp_ei ~ matched, 
       data = hetero_v_umhetero_prefs,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(pref_imp_ei ~ matched, data = hetero_v_umhetero_prefs, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe.by(hetero_v_umhetero_self, hetero_v_umhetero_self$matched) 


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ca ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ca ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_att ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_att ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_sexually_experienced ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_sexually_experienced ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ks ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ks ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_fs ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_fs ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(self_ei ~ matched, 
       data = hetero_v_umhetero_self,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(self_ei ~ matched, data = hetero_v_umhetero_self, paired = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------
# quick overview
describe(hetero_v_umhetero_rel)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonsexrel ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)

# effect size
effsize::cohen.d(interest_nonsexrel ~ matched, data = hetero_v_umhetero_rel , paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_hookups ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_hookups ~ sexual_orientation, data = data_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_nonmonrel ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_nonmonrel ~ matched, data = hetero_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_altrel ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_altrel ~ matched, data = hetero_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_single ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_single ~ matched, data = hetero_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_monrel ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_monrel ~ matched, data = hetero_v_umhetero_rel, paired = F)


## --------------------------------------------------------------------------------------------------------------------------------
t.test(interest_parent ~ matched, 
       data = hetero_v_umhetero_rel,
       alternative = "two.sided",
       paired = FALSE)


# effect size
effsize::cohen.d(interest_parent ~ matched, data = hetero_v_umhetero_rel, paired = F)


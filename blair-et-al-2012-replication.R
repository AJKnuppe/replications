## Replication:
## Blair, Graeme, et al. "Poverty and support for militant politics: Evidence from Pakistan." 
## American Journal of Political Science 57.1 (2013): 30-48.

library(data.table)
library(readstata13)
library(survey)

# library(endorse)
# data("pakistan")
# View(pakistan)

real_dat <- read.dta13("~/Dropbox/Data/endorsement/blair-et-al-2012/data_for_analysis.dta")
setDT(real_dat)

blair_dat <- real_dat
real_dat <- real_dat[1:10, ]
  
cols <- c("id", "province", "complete", "wtpru", "q800a", "newq800a","newq800b", 
          "newq800c", "newq800d", "newq800e", "newq810a", "newq810b", "newq810c", 
          "newq810d", "newq810e","newq830a", "newq830b", "newq830c", "newq830d", 
          "newq830e", "newq840a", "newq840b", "newq840c", "newq840d", "newq840e", 
          "controlweight", "group1weight", "group2weight", "group3weight", 
          "group4weight")

## subset out incomplete interviews
# table(blair_dat$complete) # 5358 complete, 642 incomplete
blair_dat <- blair_dat[complete == 1, cols, with = FALSE] 

blair_dat[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

## sum control and treatment outcomes individually
blair_dat$control_combx <- rowSums(blair_dat[, c("newq800a", "newq810a", 
                                                 "newq830a", "newq840a")])

blair_dat$control_comb_avgx <- ifelse(blair_dat$control_combx > 0, 
                         blair_dat$control_combx / 20, NA)

blair_dat$pakmil_combx <- rowSums(blair_dat[, c("newq800b", "newq810b", 
                                         "newq830b", "newq840b")], na.rm = TRUE)

blair_dat$pakmil_comb_avgx <- ifelse(blair_dat$pakmil_combx > 0, 
                                     blair_dat$pakmil_combx / 5, NA)

blair_dat$afgmil_combx <- rowSums(blair_dat[, c("newq800c", "newq810c", 
                                         "newq830c", "newq840c")], na.rm = TRUE)

blair_dat$afgmil_comb_avgx <- ifelse(blair_dat$afgmil_combx > 0,
                                     blair_dat$afgmil_combx / 5, NA)

blair_dat$alq_combx <- rowSums(blair_dat[, c("newq800d", "newq810d", 
                                         "newq830d", "newq840d")], na.rm = TRUE)

blair_dat$alq_comb_avgx <- ifelse(blair_dat$alq_combx > 0,
                                  blair_dat$alq_combx / 5, NA)
  
blair_dat$tanzeem_combx <- rowSums(blair_dat[, c("newq800e", "newq810e", 
                                         "newq830e", "newq840e")], na.rm = TRUE)

blair_dat$tanzeem_comb_avgx <- ifelse(blair_dat$tanzeem_combx > 0, 
                                      blair_dat$tanzeem_combx / 5, NA)

## combines control condition with each group individually
blair_dat$comb_avgx_pakmilx <- rowSums(blair_dat[, c("control_comb_avgx", 
                                             "pakmil_comb_avgx")], na.rm = TRUE)

blair_dat$comb_avgx_afgmilx <- rowSums(blair_dat[, c("control_comb_avgx", 
                                             "afgmil_comb_avgx")], na.rm = TRUE)

blair_dat$comb_avgx_alqx <- rowSums(blair_dat[, c("control_comb_avgx", 
                                                "alq_comb_avgx")], na.rm = TRUE)

blair_dat$comb_avgx_tanzeemx <- rowSums(blair_dat[, c("control_comb_avgx", 
                                            "tanzeem_comb_avgx")], na.rm = TRUE)

setnames(blair_dat, "comb_avgx_pakmilx", "comb_avgx_group1")
setnames(blair_dat, "comb_avgx_afgmilx", "comb_avgx_group2")
setnames(blair_dat, "comb_avgx_alqx", "comb_avgx_group3")
setnames(blair_dat, "comb_avgx_tanzeemx", "comb_avgx_group4") 

blair_dat$pakmil <- ifelse(!is.na(blair_dat$pakmil_comb_avgx), 
                           blair_dat$pakmil_comb_avgx, NA)

blair_dat$afgmil <- ifelse(!is.na(blair_dat$afgmil_comb_avgx), 
                           blair_dat$afgmil_comb_avgx, NA)

blair_dat$alq <- ifelse(!is.na(blair_dat$alq_comb_avgx), 
                           blair_dat$alq_comb_avgx, NA)

blair_dat$tanzeem <- ifelse(!is.na(blair_dat$tanzeem_comb_avgx), 
                        blair_dat$tanzeem_comb_avgx, NA)

blair_dat$group1 <- ifelse(is.na(blair_dat$pakmil), 0, 1)
blair_dat$group2 <- ifelse(is.na(blair_dat$afgmil), 0, 1)
blair_dat$group3 <- ifelse(is.na(blair_dat$alq), 0, 1)
blair_dat$group4 <- ifelse(is.na(blair_dat$tanzeem), 0, 1)

blair_dat$militancy_support <- rowSums(blair_dat[, c("comb_avgx_group1",
  "comb_avgx_group2", "comb_avgx_group3", "comb_avgx_group4")], na.rm = TRUE) / 4

blair_dat$militancy_weight <- 1.148945 * blair_dat$wtpru

blair_dat$control <- ifelse(is.na(blair_dat$q800a), 0, 1)
blair_dat$treatment <- ifelse(blair_dat$control == 1, 0, 1)

## ensure that our strata, province, is a factor
str(blair_dat$province)
blair_dat$province <- as.factor(blair_dat$province)
str(blair_dat$province)
is.ordered(blair_dat$province)

### run OLS to get unconditional mean for support
blair_design <- svydesign(ids = ~1, weights = ~militancy_weight, 
                          strata = ~province, data = blair_dat)

summary(blair_design)

summary(svyglm(militancy_support ~ treatment, family = "gaussian", design = blair_design))

group1_design <- svydesign(ids = ~1, weights = ~group1weight, 
                           strata = ~province, data = blair_dat)

summary(svyglm(comb_avgx_group1 ~ treatment, family = "gaussian", design = group1_design))

group2_design <- svydesign(ids = ~1, weights = ~group2weight, 
                           strata = ~province, data = blair_dat)

summary(svyglm(comb_avgx_group2 ~ treatment, family = "gaussian", design = group2_design))

group3_design <- svydesign(ids = ~1, weights = ~group3weight, 
                           strata = ~province, data = blair_dat)

summary(svyglm(comb_avgx_group3 ~ treatment, family = "gaussian", design = group3_design))

group4_design <- svydesign(ids = ~1, weights = ~group4weight, 
                           strata = ~province, data = blair_dat)

summary(svyglm(comb_avgx_group4 ~ treatment, family = "gaussian", design = group4_design))

# fit1 <- lm(militancy_support ~ treatment, blair_dat, singular.ok = FALSE)
# summary(fit1)
# 
# fit2 <- lm(comb_avgx_group1 ~ treatment, blair_dat, singular.ok = FALSE)
# summary(fit2)
#  
# fit3 <- lm(comb_avgx_group2 ~ treatment, blair_dat, singular.ok = FALSE)
# summary(fit3)
#  
# fit4 <- lm(comb_avgx_group3 ~ treatment, blair_dat, singular.ok = FALSE)
# summary(fit4)
# 
# fit5 <- lm(comb_avgx_group4 ~ treatment, blair_dat, singular.ok = FALSE)
# summary(fit5)


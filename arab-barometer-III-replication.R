#==============================================================================
## Arab Barometer Wave III (June 2013) Descriptive Stats 
## (use as baseline comparison to my sample)
#==============================================================================

library(readstata13)
library(data.table)

ab3 <- read.dta13("/Users/AustinKnuppe/Dropbox/Data/Arab-Barometer/Arab_Barometer_Third_Wave_English_Data_Set_v1.dta")
ab3 <- as.data.table(ab3)
setDT(ab3)

## AB III Iraq -- June 6 – 29, 2013 by IIACSS
ab3_iraq <- ab3[country == "Iraq", .(q1, q1001, q1002, q1003, q1016, q2001ir, 
                                     q2004ir)]

ab3_baghdad <- ab3_iraq[q1 == "Baghdad", ]

# N = 255

## rename columns 
setnames(ab3_baghdad, c("q1", "q1001", "q1002", "q1003", "q1016", "q2001ir", "q2004ir"), 
         c("city", "age","gender", "education", "income", "ethnicity", "religion"))

## gender
table(ab3_baghdad$gender)  / nrow(ab3_baghdad) # 49.8% male

## age
summary(ab3_baghdad$age)
table(ab3_baghdad$age)
mean(ab3_baghdad$age, na.rm = TRUE) # 34.96 yrs old

# bin age in 18-29, 30-39, 50+
ab3_baghdad$age_grp <- findInterval(ab3_baghdad$age, c(18, 30, 49)) # bin age into 4 groups
table(ab3_baghdad$age_grp) / nrow(ab3_baghdad) # 18-30, 31-49, 50+

## ethnicity
table(ab3_baghdad$ethnicity) / nrow(ab3_baghdad) # 97.64% Arab

## religion
table(ab3_baghdad$religion) / nrow(ab3_baghdad) # 91.76% Shia

## educaion
hist(as.numeric(ab3_baghdad$education))
levels(ab3_baghdad$education)
table(ab3_baghdad$education) / nrow(ab3_baghdad)
summary(ab3_baghdad$education) 

## income
levels(ab3_baghdad$income)
hist(as.numeric(ab3_baghdad$income))
table(ab3_baghdad$income) / nrow(ab3_baghdad)
summary(ab3_baghdad$income) 

##----------------------------------------
# arab barometer christian public opinion

library(readstata13)
library(data.table)
library(psych)

dat <- read.dta13("/Users/AustinKnuppe/Dropbox/Data/Arab-Barometer/Arab_Barometer_Fourth_Wave_English_Data_Set_v1.dta")

dat <- as.data.table(dat)
setDT(dat)


dat <- dat[q1012 == 2]

cov_profile <- dat[, 233:290]

table(dat$country)
# 10 = Lebanon
# 5 = Egypt
# 15 = Palestine
# 8 = Jordan

table(dat$q609) # are you religious
table(dat$q610) # do you...pray, read scripture, etc.

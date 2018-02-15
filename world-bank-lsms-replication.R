# URL:  http://go.worldbank.org/0IX1HGHAG0

library(readstata13)
library(data.table)

roster <- read.dta13("/Users/AustinKnuppe/Dropbox/Data/world-bank-lsms/2012ihses01_household_roster.dta")

roster <- as.data.table(roster)

roster_baghdad <- roster

roster_baghdad <- roster[governorate == "BAGHDAD" & q0104 > 17, 
                         .(`_questid`, q0102, q0104, q0112)]

setnames(roster_baghdad, c("_questid", "q0102", "q0104", "q0112"),
         c("ID", "gender", "age", "education"))

table(complete.cases(roster_baghdad))
roster_baghdad <- na.omit(roster_baghdad)

rm(roster)

## gender
table(roster_baghdad$gender) # 42% male

## age
mean(roster_baghdad$age) # 41.78
roster_baghdad$age_grp <- findInterval(roster_baghdad$age, c(18, 31, 49))
hist(roster_baghdad$age_grp)
table(roster_baghdad$age_grp)
# 1 = 18-30, 2 = 31-49, 3 = 50+

## education
summary(roster_baghdad$education)
roster_baghdad$edu_grp <- findInterval(roster_baghdad$education, 
                                       c(4, 5, 6, 7))
hist(roster_baghdad$edu_grp)
table(roster_baghdad$edu_grp)
# 0 = none = 4156
# 1 = elementary = 579
# 2 = middle = 301
# 3 = HS = 197
# 4 = college +  = 249

## proportions of total
props <- roster_baghdad[, .(gender, age_grp, edu_grp)]
lapply(props, function(x) prop.table(table(x)))

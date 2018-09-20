setwd("C:/Users/kevin/OneDrive/School/MIDS/W203 - Statistics for Data Science/Lab 1/W203_lab1_corruption")
#library(car)
#library(grid)
#library(ggplot2)


load("Corrupt.Rdata")
summary(FMcorrupt)
dim(FMcorrupt)

# Full data set, those with non-NA values for any of: violations, mission, staff
cor = subset(FMcorrupt, !is.na(violations) & !is.na(mission) & !is.na(staff) )

# Variables investigating
colnames(FMcorrupt[9:19])

##pctmuslim
###########
summary(cor$pctmuslim)
cor$pctmuslim

subset(cor, is.na(pctmuslim))

##majoritymuslim
################

summary(factor(FMcorrupt$majoritymuslim))
#  -1    0    1 NA's 
#   8  210   80   66 

#converting majoritymuslim to factor
FMcorrupt$majoritymuslim = as.factor(FMcorrupt$majoritymuslim)

# find all rows where majoritymuslim is -1 OR pctmuslim == 0.0
summary(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == -1 | FMcorrupt$pctmuslim == 0.0))

length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == -1 | FMcorrupt$pctmuslim == 0.0)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == -1 & FMcorrupt$pctmuslim != 0.0)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim != -1 & FMcorrupt$pctmuslim == 0.0)$majoritymuslim)
# Results show majoritymulsim == -1 happens only when pctmuslim is 0.  

# find all rows where majoritymuslim is 0 OR pctmuslim < 0.5
summary(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 0 | FMcorrupt$pctmuslim < 0.5))

length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 0 & FMcorrupt$pctmuslim < 0.5)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 0 & FMcorrupt$pctmuslim >= 0.5)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim != 0 & FMcorrupt$pctmuslim < 0.5 & FMcorrupt$pctmuslim != 0.0)$majoritymuslim)
# Results show majoritymulsim == 0 happens only when pctmuslim is < 0.5 (not equal to).  

# find all rows where majoritymuslim is 1 OR pctmuslim >= 0.5
summary(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 1 | FMcorrupt$pctmuslim >= 0.5))

length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 1 & FMcorrupt$pctmuslim >= 0.5)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim == 1 & FMcorrupt$pctmuslim < 0.5)$majoritymuslim)
length(subset(FMcorrupt[9:10], FMcorrupt$majoritymuslim != 1 & FMcorrupt$pctmuslim >= 0.5 & FMcorrupt$pctmuslim != 0.0)$majoritymuslim)

##trade
###########
summary(cor$trade)
cor$trade


subset(cor, is.na(trade) | trade == 0)

##cars_total
###########
summary(cor$cars_total)
cor$cars_total
sum(cor$cars_total, na.rm = TRUE)/2

subset(cor, is.na(cars_total) | cars_total == 0)
subset(cor, is.na(cars_total))[c("wbcode", "country", "cars_total")]
subset(cor, cars_total > 20)[c("wbcode", "country", "cars_total", "cars_personal", "cars_mission")]

sum(cor$cars_total, na.rm = TRUE)

##cars_personal
###########
summary(cor$cars_personal)
cor$cars_personal
sum(cor$cars_personal, na.rm = TRUE)/2


subset(cor, is.na(cars_personal) | cars_personal == 0)
subset(cor, is.na(cars_personal))[c("wbcode", "country", "cars_total")]
subset(cor, cars_personal > 20)[c("wbcode", "country", "cars_total", "cars_personal", "cars_mission")]

##cars_mission
###########
summary(cor$cars_mission)
cor$cars_mission
sum(cor$cars_mission, na.rm = TRUE)/2

subset(cor, is.na(cars_mission) | cars_mission == 0)
subset(cor, is.na(cars_mission))[c("wbcode", "country", "cars_total")]
subset(cor, cars_mission > 20)[c("wbcode", "country", "cars_total", "cars_personal", "cars_mission")]


#bivariate analysis of cars
cor_cars = cor[c("wbcode", "country", "cars_total", "cars_personal", "cars_mission")]
cor_cars$pct_personal_cars = cor_cars$cars_personal/cor_cars$cars_total

summary(cor_cars$pct_personal_cars)

remove(cor_cars)

##pop1998
###########
summary(FMcorrupt$pop1998)
summary(cor$pop1998)

FMcorrupt$pop1998
cor$pop1998


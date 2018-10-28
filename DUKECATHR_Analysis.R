
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)


## Read in Data

dat <- read_csv("dukecathr.csv")
attach(dat)

cathdat = data.frame(age = AGE_G, gender = GENDER, race = RACE_G, year = YRCATH_G, 
                     chfsev = CHFSEV, histcereb = HXCEREB, histchf = HXCHF, 
                     histcopd = HXCOPD, histdiab = HXDIAB, histhyp = HXHTN, histmyo = HXMI, 
                     histhyplip = HXHYL, histsmoke = HXSMOKE, height = HEIGHT_R, 
                     weight = WEIGHT_R, s3g = S3, maxsten = LMST, leftvenEF = LVEF_R, numdisves = NUMDZV, death = DEATH, time = DAYS2LKA)

detach(dat)



## Variable Manipulation

cathdat$age = factor(cathdat$age)
cathdat$gender = factor(cathdat$gender)
cathdat$race = factor(cathdat$race)
cathdat$year = factor(cathdat$year)
cathdat$chfsev = factor(cathdat$chfsev)
cathdat$histcereb = factor(cathdat$histcereb)
cathdat$histchf = factor(cathdat$histchf)
cathdat$histdiab = factor(cathdat$histdiab)
cathdat$histcopd = factor(cathdat$histcopd)
cathdat$histhyp = factor(cathdat$histhyp)
cathdat$histmyo = factor(cathdat$histmyo)
cathdat$histhyplip = factor(cathdat$histhyplip)
cathdat$histsmoke = factor(cathdat$histsmoke)
cathdat$s3g = factor(cathdat$s3g)
cathdat$numdisves= factor(cathdat$numdisves)
#cathdat$death= factor(cathdat$death)
#cathdat$time = as.numeric(cathdat$time)

attach(cathdat)


## Non-parametric Kaplan Meier Survival Estimate

surv_obj = Surv(time = time, event = death)
# surv_obj


## KP unstratified

kp.uns = survfit(surv_obj ~ 1, data = cathdat)
kp.uns.sum = summary(kp.uns)
ggsurvplot(kp.uns, data = cathdat)


## KP Stratified




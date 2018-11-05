
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(DataExplorer)


## Read in Data

dat = read.csv("dukecathr.csv")
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


## Create Survival Object

surv_obj = Surv(time = time, event = death)
surv_obj


## Unstratified Kaplan-Meier Estimate

kp.uns = survfit(surv_obj ~ 1, data = cathdat)
kp.uns.sum = summary(kp.uns)
ggsurvplot(kp.uns, data = cathdat)


## Stratified Kaplan-Meier Estimates and Mantel-Haenszel log rank test

### Demographics

#### Gender
kp.gender = survfit(surv_obj ~ gender, data = cathdat)
ggsurvplot(kp.gender, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ gender, data = cathdat)

#### Race
kp.race = survfit(surv_obj ~ race, data = cathdat,na.action = na.omit)
ggsurvplot(kp.race, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ race, data = cathdat)


### History of diseases

#### Cerebrovascular
kp.cereb = survfit(surv_obj ~ histcereb, data = cathdat,na.action = na.omit)
ggsurvplot(kp.cereb, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histcereb, data = cathdat)

#### Congestive Heart Failure
kp.chf = survfit(surv_obj ~ histchf, data = cathdat,na.action = na.omit)
ggsurvplot(kp.chf, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histchf, data = cathdat)

#### COPD
kp.copd = survfit(surv_obj ~ histcopd, data = cathdat,na.action = na.omit)
ggsurvplot(kp.copd, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histcopd, data = cathdat)

#### Diabetes
kp.diab = survfit(surv_obj ~ histdiab, data = cathdat,na.action = na.omit)
ggsurvplot(kp.diab, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histdiab, data = cathdat)

#### Hypertension
kp.hypt = survfit(surv_obj ~ histhyp, data = cathdat,na.action = na.omit)
ggsurvplot(kp.hypt, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histhyp, data = cathdat)

#### Hyperlipidemia
kp.hyplip = survfit(surv_obj ~ histhyplip, data = cathdat,na.action = na.omit)
ggsurvplot(kp.hyplip, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histhyplip, data = cathdat)

#### Myocardial Infarction
kp.myo = survfit(surv_obj ~ histmyo, data = cathdat,na.action = na.omit)
ggsurvplot(kp.myo, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histmyo, data = cathdat)

#### Smoking
kp.smoke = survfit(surv_obj ~ histsmoke, data = cathdat,na.action = na.omit)
ggsurvplot(kp.smoke, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ histsmoke, data = cathdat)



### Other

#### S3 Gallop
kp.s3g = survfit(surv_obj ~ s3g, data = cathdat,na.action = na.omit)
ggsurvplot(kp.s3g, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ s3g, data = cathdat)


#### No. of diseased vessels
kp.disves = survfit(surv_obj ~ numdisves, data = cathdat,na.action = na.omit)
ggsurvplot(kp.disves, data = cathdat,pval = TRUE)
survdiff(surv_obj ~ numdisves, data = cathdat)


# Note --> Not estimating KM curves for leftvenEF,chfsev,maxsten(continuous)  
# To stratify KM curves by year and age group


## MISSING COVARIATES

plot_missing(cathdat)

# Visualize missingness pattern 

aggr_plot = aggr(cathdat, numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"),plot=TRUE)


# Pairwise Visualizations


# Multiple imputation using MICE; m=5 imputations

imputed = mice(cathdat,m=5,seed=500)


# View Imputations

densityplot(imputed)


# Pooling and fitting cox regression

surv_obj = Surv(time = time, event = death)
coxmod = with(imputed,coxph(surv_obj ~ age + gender + race + year + 
                              chfsev + histcereb + histchf + 
                              histcopd + histdiab + histhyp + histmyo + 
                              histhyplip + histsmoke + height + 
                              weight + s3g + maxsten + leftvenEF + numdisves))
summary(pool(coxmod))


# Diagnostics for Cox model - each imputed dataset

for ( i in 1:5 )
{
  cox.mod = coxph(surv_obj ~ age + gender + race + year + 
                    chfsev + histcereb + histchf + 
                    histcopd + histdiab + histhyp + histmyo + 
                    histhyplip + histsmoke + height + 
                    weight + s3g + maxsten + leftvenEF + numdisves,data=complete(imputed,i))
  cox.zph(cox.mod)
  
}


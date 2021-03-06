
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

cols = names(cathdat)[-c(20,21)]
cathdat %<>%
  mutate_each_(funs(factor(.)),cols)

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

PH.diag = PHplot.diag = IOplot.diag = nonlinlvef.diag = nonlinchf.diag = nonlinmax.diag = vector(mode="list",length=5)

for ( i in 1:5 )
{
  
  # Check for proportional hazards - Schoenfield Residuals
  
  cox.mod = coxph(surv_obj ~ age + gender + race + year + 
                    chfsev + histcereb + histchf + 
                    histcopd + histdiab + histhyp + histmyo + 
                    histhyplip + histsmoke + height + 
                    weight + s3g + maxsten + leftvenEF + numdisves,data=complete(imputed,i))
  PH.diag[[i]] = cox.zph(cox.mod)
  PHplot.diag[[i]] = ggcoxzph(cox.mod)
  
  
  # Check for influential observations - deltabeta residuals
  
  IOplot.diag[[i]] = ggcoxdiagnostics(cox.mod, type = "dfbeta",
                                      linear.predictions = FALSE, ggtheme = theme_bw())
  
  
  # Check for non-linearity - leftvenEF,chfsev,maxsten(continuous)
  
  nonlinlvef.diag[[i]] = ggcoxfunctional(surv_obj ~ leftvenEF + log(leftvenEF) + sqrt(leftvenEF), data = complete(imputed,i))
  nonlinchf.diag[[i]] =  ggcoxfunctional(surv_obj ~ chfsev + log(chfsev) + sqrt(chfsev), data = complete(imputed,i))
  nonlinmax.diag[[i]] =  ggcoxfunctional(surv_obj ~ maxsten + log(maxsten) + sqrt(maxsten), data = complete(imputed,i))
  
}



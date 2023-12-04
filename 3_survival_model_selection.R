gc()
options(scipen=100, digits = 5)

library(arrow)
import <- "insert custom filepath"
df <- read_parquet(paste0(import, "df_surv.parquet"))

#Required sample size calculations 
#Based on prediction models from 10.1136/bmj.m1501
get_D_from_c <- function(c) {
  5.5 * (c - 0.5) + 10.26 * (c - 0.5)^3
}

get_r2_from_D <- function(D,
                          sigma2 = (pi^2) / 3,
                          k = sqrt(8 / pi)) {
  (D^2 / k^2) / (sigma2 + (D^2) / (k^2))
}

get_r2_from_c <- function(c, ...) {
  get_r2_from_D(D = get_D_from_c(c), ...)
  }

pmsampsize::pmsampsize(
  type = "s",
  rsquared = get_r2_from_c(c = 0.84), #Assuming range of AUCs from literature of ~0.70 to ~0.97
  parameters = 30, #Expected upper limit of the number of degrees of freedom
  rate = 0.004, #From 10.1016/j.jclinepi.2023.05.020
  timepoint = 24, #Interested in ~24 hours as first pass
  meanfup = 66 #Assuming ~66hrs of mean follow up time based on data
)

#Survival package coxph - note slow convergence but good for expected events estimation
#Non-linearity observed for all continuous variables (from anova(coxfit))
#Pain (from AVPU) assumed to be reference category
library(survival)
library(rms)
coxsurv <- coxph(Surv(index, index2, outcome) ~
                   rcs(Resp_Rate, c(14, 16, 18, 22)) + rcs(SpO2, c(92, 95, 97, 98, 100)) + 
                   rcs(SBP, c(97, 117, 133, 167)) + rcs(DBP, c(55, 69, 77, 94)) + 
                   rcs(Pulse, c(55, 71, 83, 106)) + rcs(Temp, c(36, 36.5, 36.8, 37.4)) + 
                   Alert + Verbal + Unresponsive + O2_Therapy + rcs(Age, c(32, 64, 85)),
                data = df)


#Repeat analysis using cph from rms, can't generate "expected" probabilities but useful for Wald tests (anova(coxfit))
coxfit <- cph(Surv(index, index2, outcome) ~
                rcs(Resp_Rate, c(14, 16, 18, 22)) + rcs(SpO2, c(92, 95, 97, 98, 100)) + 
                rcs(SBP, c(97, 117, 133, 167)) + rcs(DBP, c(55, 69, 77, 94)) + 
                rcs(Pulse, c(55, 71, 83, 106)) + rcs(Temp, c(36, 36.5, 36.8, 37.4)) + 
                Alert + Verbal + Unresponsive + O2_Therapy + rcs(Age, c(32, 64, 85)),
              x = TRUE, y = TRUE, iter.max = 1000,
              data = df)
coxfit
AIC(coxfit)
anova(coxfit)
specs(coxfit)
concordance(coxfit)
Function(coxfit)


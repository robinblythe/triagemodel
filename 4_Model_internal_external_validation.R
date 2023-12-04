gc()
options(scipen=100, digits = 5)

library(arrow)
library(rms)
library(survival)
library(dplyr)
library(broom)

import <- "insert custom filepath"

df <- read_parquet(paste0(import, "df_surv.parquet"))

#Create facility folds
df <- df |> 
  mutate(Facility = case_when("insert facility name" == 1 ~ 1,
                              "insert facility name" == 1 ~ 2,
                              "insert facility name" == 1 ~ 3,
                              "insert facility name" == 1 ~ 4,
                              "insert facility name" == 1 ~ 5))

#Run internal-external validation across 5 facilities
#Process: 
#Fit model to 4 facilities, holding 1 out
#Predict(type = "risk") on held out observations
#Repeat until all 5 hospitals have predictions
folds <- max(df$Facility)
fitcoef <- list()
pred_lp <- list()
pred_risk <- list()
pred_exp <- list()
harrells_c <- list()

for (i in 1:folds){
  set.seed = 888
  train <- df[df$Facility != i,]
  test <- df[df$Facility == i,]
  fit <- coxph(Surv(index, index2, outcome) ~
                 rcs(Resp_Rate) + rcs(SpO2) + rcs(SBP) + rcs(DBP) + rcs(Pulse) + rcs(Temp) + 
                 Alert + Verbal + Unresponsive + O2_Therapy + 
                 rcs(Age),
               x = T, y = T,
               data = train)
  
  fitcoef[[i]] <- tidy(fit)
  pred_lp[[i]] <- predict(fit, newdata = test, type = "lp")
  pred_risk[[i]] <- predict(fit, newdata = test, type = "risk")
  pred_exp[[i]] <- predict(fit, newdata = test, type = "expected")
  harrells_c[[i]] <- concordance(fit, newdata = test)
  saveRDS(fit, file = paste0("./coxfit_fold",i,".rds"))
  print(i)
}

saveRDS(fitcoef, file = "./fitcoefs.rds")
saveRDS(harrells_c, file = "./concordance.rds")

#Now add predicted values to full dataset
for (i in 1:folds){
  df$lp[df$Facility == i] <- pred_lp[[i]]
  df$risk[df$Facility == i] <- pred_risk[[i]]
  df$exp[df$Facility == i] <- pred_exp[[i]]
}

write_parquet(df, paste0(import, "df_risk.parquet"))

folds <- max(df$Facility)
timepoints <- c(24,168,336,504) #days 1, 7, 14, 21

#Discrimination
library(timeROC)
library(survival)

inner_aucs <- list()
outer_aucs <- list()

for(i in 1:folds){
  for(j in 1:length(timepoints)){
    inner_aucs[[j]] <- c(
      "Hospital" = i,
      "Timepoint" = timepoints[j],
      "AUC" = with(
        subset(df, Facility == i),
        timeROC(T = index2,
                delta = outcome,
                marker = risk,
                cause = 1,
                times = timepoints[j]
                ))$AUC[2])
    outer_aucs[[i]] <- inner_aucs
    print(paste0("Timepoint ", j, " done"))
  }
  print(paste0("Hospital ", i, " done"))
}

df_auc <- rbind.data.frame(do.call(rbind, outer_aucs[[1]]),
                           do.call(rbind, outer_aucs[[2]]),
                           do.call(rbind, outer_aucs[[3]]),
                           do.call(rbind, outer_aucs[[4]]),
                           do.call(rbind, outer_aucs[[5]]))
df_auc <- rename(df_auc, "AUC" = "AUC.t=24")
df_auc$Hospital <- as.factor(df_auc$Hospital)
remove(inner_aucs, outer_aucs)

saveRDS(df_auc, file = "./auc_by_fold_time.rds")



#Calibration
library(rms)

#Moderate calibration (Poisson model on y axis ('observed') vs Cox model on x axis ('expected))
df$p <- log(df$exp) #Obtain log cumulative hazard
df$logbase <- df$p - df$lp #Subtract prognostic index from log cumulative hazard

#Calibration slope and intercept for each fold
for(i in 1:folds){
  calfit <- glm(outcome ~ rcs(lp, 3) + offset(p), family = poisson, data = subset(df, exp > 0 & Facility == i))
  
  df$pois[df$Facility == i] <- predict(calfit, newdata = df[df$Facility == i], type = "response")
  print(i)
}

df_calib <- df |> select("Facility", "exp", "pois")
df_calib$Hospital <- as.character(df_calib$Facility)
df_calib$Facility <- NULL

saveRDS(df_calib, file = "./calibration_by_fold.rds")


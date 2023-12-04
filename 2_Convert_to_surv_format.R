gc()
options(scipen = 100, digits = 5)

library(arrow)
library(data.table)

import <- "insert custom filepath"
df <- as.data.table(read_parquet(paste0(import,"df_mr.parquet")))

#Re-define index variable as time since first observation and sort
df <- df[, index := difftime(Performed_DT_TM, min(Performed_DT_TM), units = "hours"), by = "ID"]
df <- df[order(ID, index)]

#Prepare for survival modelling: create outcome flag for death
#First create last observation per ID flag
df <- df[, is.end := .I == last(.I), by = "ID"]

#index1 column: start time of observation (when time-varying covariates are entered)
#index2 column: either the next observation's index time (if not last obs) or discharge time (if last obs)
df$index2 <- shift(df$index, 1, type = "lead")
df$index2 <- fifelse(df$is.end == TRUE, as.numeric(difftime(df$DISCH_DT_TM, df$REG_DT_TM, units = "hours")), df$index2)

#If observation is last for that ID and died in hospital within 30 days, outcome is 1
df$outcome <- fifelse(df$is.end == TRUE & df$Died == 1 & df$index2 <= 720, 1, 0)

#Censor at 30 days, cutting index2 back to 720
df <- subset(df, index < 720)
df$index2 <- fifelse(df$index2 > 720, 720, df$index2)

#Drop duplicates
df <- subset(df, index != index2)

#Recast AVPU as multiple binary variables
df$Alert <- fifelse(df$AVPU == 1, 1, 0)
df$Verbal <- fifelse(df$AVPU == 2, 1, 0)
df$Pain <- fifelse(df$AVPU == 3, 1, 0)
df$Unresponsive <- fifelse(df$AVPU == 4, 1, 0)

#Prepare data for model
df <- dplyr::select(df, 
                    "ID", "index", "index2", "outcome", "Died", "Performed_DT_TM", "REG_DT_TM":"DECEASED_DT_TM", 
                    "Resp_Rate", "SpO2", "SBP":"Mean_AP_Cuff_Calc", "Pulse", "Temp", "Alert":"Verbal", "Pain", "Unresponsive", 
                    "mean_Resp_Rate":"slope_Temp", "O2_Therapy", "Age", "WEIGHT", "female", "ATSI", "obs_no", "hourofday", 
                    "Facility_BEAH":"Facility_RLH", "Dept_Cardiology":"Dept_Vascular")

export <- "insert custom filepath"
write_parquet(df, paste0(export, "df_surv.parquet"))

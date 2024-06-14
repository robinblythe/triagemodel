# Prepare data for conversion to survival format
options(scipen = 100, digits = 5)

library(arrow)
library(data.table)
library(dplyr)

import <- "insert your filepath"
df <- as.data.table(read_parquet(paste0(import, "df_prepared.parquet")))

# Remove linkage errors where observation is outside of admission time
df <- subset(df, (Performed_DT_TM < DISCH_DT_TM) & (Performed_DT_TM > REG_DT_TM))

# Convert consciousness stat to categorical prior to imputation for easier imputation
df <- df |>
  mutate(AVPU = case_when(
    Alert == 1 ~ 1,
    Verbal == 1 ~ 2,
    Rousable == 1 ~ 3,
    Unresponsive == 1 ~ 4
  ))


# Random forest impute using predictive mean matching to reduce implausible/erroneous imputations
# Usually converges by iteration 3 on repetition, but set max iterations to 10 just in case
# Can only single impute due to computational limitations (5.5m observations * 30 imputations = infeasible)
# Currently random forest imputation appears similar to MICE in terms of performance
library(missRanger)
df_mr <- missRanger(df,
  formula = Resp_Rate + SpO2 + SBP + DBP + Pulse + Temp + AVPU ~ .,
  pmm.k = 3,
  num.trees = 50,
  max.iter = 10,
  verbose = 1
)

# Redefine the MAP variable now that SBP/DBP are fully imputed
df$Mean_AP_Cuff_Calc <- df$DBP + (df$SBP - df$DBP) * (1 / 3)

write_parquet(df_mr, paste0(import, "df_mr.parquet"))

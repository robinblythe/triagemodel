# Convert data to survival format
# For more details see the survival package vignettes
options(scipen = 100, digits = 5)

library(arrow)
library(data.table)

import <- "custom filepath"
df <- as.data.table(read_parquet(paste0(import, "df_mr.parquet")))

# Define index as entry time (interval censoring)
df <- df[, index := difftime(Performed_DT_TM, min(Performed_DT_TM), units = "hours"), by = "ID"]
df <- df[order(ID, index)]

# Prepare for survival modelling: create outcome flag for death
# First create last observation per ID flag
df <- df[, is.end := .I == last(.I), by = "ID"]

# index1 column: entry time, i.e. start time of observation (when time-varying covariates are entered)
# index2 column: exit time, i.e. either the next observation's index time (if not last obs) or discharge/death time (if last obs)
# This method ensures timing of event is captured
df$index2 <- shift(df$index, 1, type = "lead")
df$index2 <- fifelse(df$is.end == TRUE, as.numeric(difftime(df$DISCH_DT_TM, df$REG_DT_TM, units = "hours")), df$index2)

# If observation is last for that ID and died in hospital within 30 days, outcome is 1
df$outcome <- fifelse(df$is.end == TRUE & df$Died == 1 & df$index2 <= 720, 1, 0)

# Censor at 30 days
df <- subset(df, index < 720)
df$index2 <- fifelse(df$index2 > 720, NA_real_, df$index2)

# Drop duplicate observations
df <- subset(df, index != index2)

# Recast AVPU back to multiple binary variables for prediction model - use dummies rather than ordinal
df$Alert <- fifelse(df$AVPU == 1, 1, 0)
df$Verbal <- fifelse(df$AVPU == 2, 1, 0)
df$Pain <- fifelse(df$AVPU == 3, 1, 0)
df$Unresponsive <- fifelse(df$AVPU == 4, 1, 0)

# Prepare data for model
library(dplyr)
df <- select(
  df,
  "ID", "index", "index2", "outcome", "Died", "Performed_DT_TM", "Admit_to_Perform", "REG_DT_TM":"DECEASED_DT_TM",
  "Resp_Rate", "SpO2", "SBP":"Mean_AP_Cuff_Calc", "Pulse", "Temp", "Alert":"Verbal", "Pain", "Unresponsive",
  "mean_Resp_Rate":"slope_Temp", "O2_Therapy", "Age", "WEIGHT", "female", "ATSI", "obs_no", "hourofday", "LOS_In_Days",
  "LOS_hrs", "FACILITY", "MED_SERVICE", "is.end"
)

# Create outcome variables for predicting different window widths
df <- df |>
  group_by(ID) |>
  mutate(Died = max(outcome)) |>
  mutate(index_max = max(index2)) |>
  mutate(
    died_12h = ifelse(index > (index_max - 12) & Died == 1, 1, 0),
    died_24h = ifelse(index > (index_max - 24) & Died == 1, 1, 0),
    died_48h = ifelse(index > (index_max - 48) & Died == 1, 1, 0),
    died_72h = ifelse(index > (index_max - 72) & Died == 1, 1, 0),
    died_168h = ifelse(index > (index_max - 168) & Died == 1, 1, 0)
  ) |>
  ungroup()

write_parquet(df, paste0(import, "df_surv.parquet"))

# Plot cumulative incidence (Figure 2)
library(survminer)
library(cmprsk)

survset <- df |>
  group_by(ID) |>
  arrange(ID, index) |>
  slice_tail(n = 1) |>
  mutate(
    Outcome = case_when(
      outcome == 0 & is.end == T ~ factor(0, labels = "Discharged alive"),
      outcome == 1 & is.end == T ~ factor(1, labels = "Died in hospital"),
      is.end == F ~ factor(2, labels = "Censored")
    ),
    Days = ceiling(index2 / 24)
  )

cif.fit <- with(survset, cuminc(
  ftime = Days,
  fstatus = Outcome,
  group = Facility,
  cencode = "Censored"
))

g <- ggcompetingrisks(
  fit = cif.fit,
  conf.int = T,
  xlab = "Time in days",
  title = "",
  legend.title = ""
)
g$data$group <- paste0("Hospital ", g$data$group)
g

ggsave(plot = g, filename = "./cmpriskplot.jpg")

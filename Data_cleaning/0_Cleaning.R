# Data were fairly rough and required significant cleaning prior to use in modelling process
# Load requisite libraries
library(tidyverse)
library(arrow)
library(data.table)
options(scipen = 100, digits = 5)

# Import data - Patient information (demographics), admission status, and vitals
demographics <- as.data.table(read_parquet("file location"))
vital <- as.data.table(read_parquet("file location"))
inpatients <- as.data.table(read_parquet("file location"))

# Select columns to keep
vital <- vital |>
  select(
    "Enc_Encntr_ID", "Performed_DT_TM", "Performed_Prsnl_Role", "Admit_to_Perform",
    "Resp_Rate", "SpO2", "O2_Therapy", "SBP", "DBP", "Mean_AP_Cuff_Calc",
    "Peripheral_Pulse_Rate", "Temp_Oral", "AVPU", "Sedation_Score", "SBP_Supine",
    "DBP_Supine", "SBP_Sitting", "DBP_Sitting", "SBP_Standing", "DBP_Standing", "GCS",
    "Resp_Distress", "O2_Flow_Rate", "FIO2", "Temp_Tympanic", "Capillary_Refill", "Blood_Ketone_Lvl_Bedside"
  )

demographics <- demographics |>
  select(
    "Enc_Encntr_ID", "FACILITY", "MED_SERVICE", "ADMIT_TYPE", "SEX", "Age",
    "WEIGHT", "INDIGEN_STATUS_CD", "LOS_In_Days", "REG_DT_TM", "DISCH_DT_TM", "DECEASED_DT_TM"
  )

inpatients <- inpatients |> select("Enc_Encntr_ID", "IsInpatient")

# Convert vital signs to numeric
tonum <- c(
  "Resp_Rate", "SpO2", "SBP", "DBP", "Peripheral_Pulse_Rate", "Temp_Oral", "Temp_Tympanic", "SBP_Supine",
  "DBP_Supine", "SBP_Sitting", "DBP_Sitting", "SBP_Standing", "DBP_Standing", "GCS"
)
vital <- suppressWarnings(vital[, (tonum) := lapply(.SD, as.numeric), .SDcols = tonum])
remove(tonum)

# Collate SBP/DBP values into one column and add binary flags for posture
vital <- vital |>
  mutate(
    SBP = coalesce(SBP, SBP_Supine, SBP_Sitting, SBP_Standing),
    DBP = coalesce(DBP, DBP_Supine, DBP_Sitting, DBP_Standing),
    supine = ifelse(is.na(SBP_Supine), 0, 1),
    sitting = ifelse(is.na(SBP_Sitting), 0, 1),
    standing = ifelse(is.na(SBP_Standing), 0, 1)
  ) |>
  select(-c("SBP_Supine", "DBP_Supine", "SBP_Sitting", "DBP_Sitting", "SBP_Standing", "DBP_Standing")) |>
  arrange(Enc_encntr_ID, Performed_DT_TM)

# Create a time between observations variable
vital$timediff <- ave(as.numeric(vital$Performed_DT_TM), vital$Enc_Encntr_ID, FUN = function(x) c(0, diff(x))) / 3600
# Cumulative sum of time difference - time since first observation (hours) - for interval censoring this is entry time
vital$index <- ave(vital$timediff, vital$Enc_Encntr_ID, FUN = cumsum)

# Merge data IFF patients present in both datasets - excludes broken linkages or patients without vital signs measurements
df <- merge(vital, demographics, by = "Enc_Encntr_ID", all.x = FALSE, all.y = FALSE)
remove(demographics, vital)

# Drop non-admitted patients
inpatients <- subset(inpatients, IsInpatient == 1)$Enc_Encntr_ID
df <- subset(df, Enc_Encntr_ID %in% inpatients)
remove(inpatients)

# Include 2019-2020 observations only - data quality significantly worse in 2018 & 2021
df <- df |>
  filter(Performed_DT_TM >= "2019-01-02 00:00:00" & Performed_DT_TM < "2021-01-01 00:00:00") |>
  as.data.table()

# Drop blank observations
df <- df[!with(
  df,
  is.na(Resp_Rate) &
    is.na(SpO2) &
    is.na(SBP) &
    is.na(DBP) &
    is.na(Peripheral_Pulse_Rate) &
    is.na(Temp_Oral) &
    is.na(Temp_Tympanic)
)]

# Add observation number
df$counter <- 1
df$obs_no <- ave(ifelse(df$timediff < 0.25, 0, df$counter), df$Enc_Encntr_ID, FUN = cumsum)
df$counter <- NULL

# Times of observation
df$hourofday <- as.numeric(format(df$Performed_DT_TM, "%H")) + as.numeric(format(df$Performed_DT_TM, "%M")) / 60
df$day <- weekdays(as.Date(df$Performed_DT_TM))
df$weekend <- ifelse(df$day == "Saturday" | df$day == "Sunday", 1, 0)
df$month <- month(as.Date(df$Performed_DT_TM))
df$day <- NULL
df$LOS_hrs <- as.numeric(difftime(df$DISCH_DT_TM, df$REG_DT_TM, units = "hours"))

# Ensure dataframe is chronological by ID, create nicer ID variable
df <- setorder(df, Enc_Encntr_ID, index)
df$ID <- as.numeric(as.factor(df$Enc_Encntr_ID))
df$Enc_Encntr_ID <- NULL

# Merge AVPU and Sedation Score, convert to binary - but maintain missingness
df$Alert <- NA
df$Alert <- fifelse(df$AVPU == "Alert" | df$Sedation_Score == "0=Awake and alert", 1, 0)
df$Alert <- fifelse(df$AVPU == "" & df$Sedation_Score == "", NA_real_, df$Alert)
df$Verbal <- NA
df$Verbal <- fifelse(df$AVPU == "Verbal" | df$Sedation_Score == "1=Easy to rouse", 1, 0)
df$Verbal <- fifelse(df$AVPU == "" & df$Sedation_Score == "", NA_real_, df$Verbal)
df$Rousable <- NA
df$Rousable <- fifelse(df$AVPU == "Pain" | df$Sedation_Score == "2=Rousable but can't stay awake", 1, 0)
df$Rousable <- fifelse(df$AVPU == "" & df$Sedation_Score == "", NA_real_, df$Rousable)
df$Unresponsive <- NA
df$Unresponsive <- fifelse(df$AVPU == "Unresponsive" | df$Sedation_Score == "3=Hard to rouse or unrousable", 1, 0)
df$Unresponsive <- fifelse(df$AVPU == "" & df$Sedation_Score == "", NA_real_, df$Unresponsive)

# Convert supplemental oxygen, sex, indigenous status, to dummy vars
df$O2_Therapy <- ifelse(df$O2_Therapy == "Room air", 0, 1)
df$female <- ifelse(df$SEX == "FEMALE", 1, 0)
df$ATSI <- ifelse(df$INDIGEN_STATUS_CD == "Not Aboriginal or Torres Strait Islander" | df$INDIGEN_STATUS_CD == "Not Stated/Unknown", 0, 1)
df$SEX <- NULL
df$INDIGEN_STATUS_CD <- NULL
df$Performed_Prsnl_Role <- NULL

# Temperature can be oral or tympanic
df <- df |> mutate(Temp = coalesce(Temp_Oral, Temp_Tympanic))

# Death variable - Died in hospital if time of death less than or equal to discharge time
df$Died <- fifelse(df$DECEASED_DT_TM <= df$DISCH_DT_TM, 1, 0)
df$Died <- fifelse(is.na(df$Died), 0, df$Died)

# Omissions: "Day Surgery", "Dental medicine and surgery", "Emergency Medicine", "Gynaecology", "Intensive Care",
# "Neonatology", "Obstetrics", "Paediatric-Ear Nose and Throat", "Paediatric-General", "Palliative medicine"
df <- df |>
  filter(
    !MED_SERVICE %in% c(
      "Day Surgery", "Dental medicine and surgery", "Emergency Medicine", "Gynaecology", "Intensive Care",
      "Neonatology", "Obstetrics", "Paediatric-Ear Nose and Throat", "Paediatric-General", "Palliative medicine"
    ),
    ADMIT_TYPE == c("Acute Care")
  )

# Rename pulse
df$Pulse <- df$Peripheral_Pulse_Rate
df$Peripheral_Pulse_Rate <- NULL

# Weight and age as numeric variables
df$WEIGHT <- as.numeric(df$WEIGHT)

# Drop vestigial variables
df <- df[, `:=`(Temp_Oral = NULL, Temp_Tympanic = NULL, Sedation_Score = NULL, GCS = NULL, ADMIT_TYPE = NULL)]

# Ensure patients are adults only
df <- subset(df, Age >= 18)

# Drop impossible values based on expert advice
df$SpO2 <- fifelse(df$SpO2 > 100, NA_real_, df$SpO2)
df$SBP <- fifelse(df$SBP > 300 | df$SBP < 1, NA_real_, df$SBP)
df$DBP <- fifelse(df$DBP > 200 | df$DBP < 1, NA_real_, df$DBP)
df$Pulse <- fifelse(df$Pulse > 300, NA_real_, df$Pulse)
df$Temp <- fifelse(df$Temp < 20 | df$Temp > 42, NA_real_, df$Temp)

# Rerun blank obs drop now that impossible values omitted
df <- df[!with(
  df,
  is.na(Resp_Rate) &
    is.na(SpO2) &
    is.na(SBP) &
    is.na(DBP) &
    is.na(Pulse) &
    is.na(Temp)
)]

# Done - save to file
write_parquet(df, "df_raw.parquet")

#Triage model summary statistic preparation
options(scipen=100, digits = 5)
library(arrow)

# Load cleaned raw data
df <- read_parquet("df_raw.parquet")

# Prepare data for analysis
library(data.table)
library(dplyr)
window <- 24 #Summarise vitals within 24 hours

#Create new data.table
df_id <- df |>
  mutate(start=index - window)
df_id <- setDT(df_id, key=c("ID", "index"))
df_id <- df_id %>% select(c(ID,Admission_to_Perform,Resp_Rate,SpO2,SBP,DBP,Pulse,Temp,index,start))

#Create matching table
df_match <- df_id |>
  select(ID, ob_time=index)


#Create long form table
new <- df_id[df_match, on = .(ID, start <= ob_time, index <= ob_time), nomatch = 0L]
new$start <- NULL
remove(df_match, window, df_id)

#Roll up into summary stats
means <- new[,lapply(.SD, mean, na.rm=TRUE), by=.(ID, index)]
means <- means |> rename_with(~paste0("mean_", .x), c(4:9))
sds <- new[,lapply(.SD, sd, na.rm=TRUE), by=.(ID, index)]
sds <- sds |> rename_with(~paste0("SD_", .x), c(4:9))
mins <- new[,lapply(.SD, min, na.rm=TRUE), by=.(ID, index)]
mins <- mins |> rename_with(~paste0("min_", .x), c(4:9))
maxes <- new[,lapply(.SD, max, na.rm=TRUE), by=.(ID, index)]
maxes <- maxes |> rename_with(~paste0("max_", .x), c(4:9))

#Merge means, sds, mins, maxes:
df_summary <- merge.data.table(means, sds, by = c("ID", "index"))
df_summary <- merge.data.table(df_summary, maxes, by = c("ID", "index"))
df_summary <- merge.data.table(df_summary, mins, by = c("ID", "index"))
df_summary$Admission_to_Perform.x <- NULL
df_summary$Admission_to_Perform.x <- NULL
df_summary$Admission_to_Perform.y <- NULL
df_summary$Admission_to_Perform.y <- NULL

remove(means, sds, maxes, mins)

#Slopes
resp_slope <- subset(new, !is.na(Resp_Rate), select = c(1,2,3,9))
resp_slope <- resp_slope[,.(slope_Resp_Rate = coef(lm(Resp_Rate~Admission_to_Perform))[2]), by=.(ID, index)]

spo2_slope <- subset(new, !is.na(SpO2), select = c(1,2,4,9))
spo2_slope <- spo2_slope[,.(slope_SpO2 = coef(lm(SpO2~Admission_to_Perform, na.action = na.omit))[2]), by=.(ID, index)]

sbp_slope <- subset(new, !is.na(SBP), select = c(1,2,5,9))
sbp_slope <- sbp_slope[,.(slope_SBP = coef(lm(SBP~Admission_to_Perform, na.action = na.omit))[2]), by=.(ID, index)]

dbp_slope <- subset(new, !is.na(DBP), select = c(1,2,6,9))
dbp_slope <- dbp_slope[,.(slope_DBP = coef(lm(DBP~Admission_to_Perform, na.action = na.omit))[2]), by=.(ID, index)]

pulse_slope <- subset(new, !is.na(Pulse), select = c(1,2,7,9))
pulse_slope <- pulse_slope[,.(slope_Pulse = coef(lm(Pulse~Admission_to_Perform, na.action = na.omit))[2]), by=.(ID, index)]

temp_slope <- subset(new, !is.na(Temp), select = c(1,2,8,9))
temp_slope <- temp_slope[,.(slope_Temp = coef(lm(Temp~Admission_to_Perform, na.action = na.omit))[2]), by=.(ID, index)]

df_summary <- merge.data.table(df_summary, resp_slope, by = c("ID", "index"), all.x = TRUE)
df_summary <- merge.data.table(df_summary, spo2_slope, by = c("ID", "index"), all.x = TRUE)
df_summary <- merge.data.table(df_summary, sbp_slope, by = c("ID", "index"), all.x = TRUE)
df_summary <- merge.data.table(df_summary, dbp_slope, by = c("ID", "index"), all.x = TRUE)
df_summary <- merge.data.table(df_summary, pulse_slope, by = c("ID", "index"), all.x = TRUE)
df_summary <- merge.data.table(df_summary, temp_slope, by = c("ID", "index"), all.x = TRUE)
remove(resp_slope, spo2_slope, sbp_slope, dbp_slope, pulse_slope, temp_slope, new)

df <- merge.data.table(df, df_summary, by = c("ID", "index"), all = TRUE)

#Fixing infinite values:
invisible(lapply(names(df), function(.name) set(df, which(is.infinite(df[[.name]])), j = .name, value = NA)))
invisible(lapply(names(df), function(.name) set(df, which(is.nan(df[[.name]])), j = .name, value = NA)))

write_parquet(df, "df_prepared.parquet")
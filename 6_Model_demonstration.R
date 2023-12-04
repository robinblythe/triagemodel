gc()
options(scipen=100, digits = 5)
library(arrow)
library(survival)
library(rms)
library(dplyr)
library(lubridate)
import <- "insert custom filepath"
df <- read_parquet(paste0(import, "df_risk.parquet"))

#Full model (survival)
fit <- readRDS("./full_model_survival.rds")

#Full model (rms)
cphfit <- readRDS("./full_model_rms.rds")

#Pick time interval to demonstrate model using 6pm as the endpoint/tstop

df_demo <- df |>
  filter(Performed_DT_TM >= ymd_hms("insert custom time") & Performed_DT_TM < ymd_hms("insert custom time") & Facility == "") |>
  group_by(ID) |>
  arrange(desc(index)) |>
  slice(1) |>
  ungroup() |>
  mutate(index2 = index + as.numeric(difftime(ymd_hms("insert custom time"), Performed_DT_TM, units = "hours"))) |>
  select("ID", "index", "index2", "outcome", "Resp_Rate", "SpO2", "SBP", "DBP", 
         "Pulse", "Temp", "Alert", "Verbal", "Unresponsive", "O2_Therapy", "Age")

#Obtain predicted risks, lp
df_demo$pred_risk <- predict(fit, newdata = df_demo, type = "risk")
df_demo$lp <- predict(fit, newdata = df_demo, type = "lp")

#Create rounding list based on top 10 predicted risks
roundlist <- df_demo |>
  select(-outcome) |>
  arrange(desc(pred_risk)) |>
  slice_head(n = 5)

roundlist <- head(read.csv("./roundlist.csv"), 5)

#Plot risks for top 5 patients
demoset <- subset(df, ID %in% roundlist$ID & 
                    Performed_DT_TM < ymd_hms("insert custom time") & 
                    Performed_DT_TM > ymd_hms("insert custom time"))
demoset <- demoset |>
  mutate(ID = as.factor(case_when(ID == roundlist$ID[1] ~ 1,
                                  ID == roundlist$ID[2] ~ 2,
                                  ID == roundlist$ID[3] ~ 3,
                                  ID == roundlist$ID[4] ~ 4,
                                  ID == roundlist$ID[5] ~ 5))) |>
  arrange(ID, index) |>
  group_by(ID) |>
  mutate(indexmax = max(index)) |>
  mutate(index_rev = indexmax - index)
  

library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

p <- demoset |> ggplot()
p +
  geom_line(aes(x = index_rev, y = lp, group = ID, colour = ID), linewidth = 1.2, alpha = 0.6) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_reverse(limits = c(24, 0), breaks = seq(24, 0, -6)) +
  scale_colour_manual(values = cbPalette) +
  labs(x = "Hours",
       y = "Predicted risk",
       colour = "Patient")

ggsave("demoplot.jpg")

#Obtain terms for roundlist to determine share of overall risk
share <- as.data.frame(predict(fit, newdata = roundlist, type = "terms"))
write.csv(share, file = "./roundlist_share.csv")


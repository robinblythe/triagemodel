# Internal-external validation of the survival model approach
options(scipen = 100, digits = 5)
library(arrow)
library(rms)
library(survival)
library(dplyr)
library(riskRegression)

import <- "custom filepath"
df <- read_parquet(paste0(import, "df_surv.parquet"))

# Run internal-external validation across 5 facilities
# Process:
# Fit model to 4 facilities, holding 1 out
# Predict risk on held out observations
# Repeat until all 5 hospitals have predictions
folds <- max(df$Facility)
pred_lp <- list() # Linear predictors (sufficient for ranking)
pred_risk <- list() # Exponentiation of linear predictors ("risk")
pred_exp <- list() # Predicted number of events at time t

for (i in 1:folds) {
  set.seed(888)
  train <- df[df$Facility != i, ]
  test <- df[df$Facility == i, ]
  fit <- coxph( # Note number of knots remains unspecified during CV process
    Surv(index, index2, outcome) ~
      rcs(Resp_Rate) + rcs(SpO2) +
      rcs(SBP) + rcs(DBP) +
      rcs(Pulse) + rcs(Temp) +
      +rcs(Age) +
      Alert + Verbal + Unresponsive + O2_Therapy,
    x = T, y = T,
    data = train
  )

  pred_lp[[i]] <- predict(fit, newdata = test, type = "lp")
  pred_risk[[i]] <- predict(fit, newdata = test, type = "risk")
  pred_exp[[i]] <- predict(fit, newdata = test, type = "expected")
  saveRDS(fit, file = paste0("./coxfit_fold", i, ".rds"))
  remove(fit, test)
  print(i)
}

# Now add predicted values to full dataset
for (i in 1:folds) {
  df$lp[df$Facility == i] <- pred_lp[[i]]
  df$risk[df$Facility == i] <- pred_risk[[i]]
  df$exp[df$Facility == i] <- pred_exp[[i]]
}

write_parquet(df, paste0(import, "df_risk"))

# Create AUC summary dataset based on model predictions
folds <- max(df$Facility)
timepoints <- c(12, 24, 48, 72, 168)

# Discrimination
library(timeROC)
library(survival)

inner_aucs <- list()
outer_aucs <- list()

for (i in 1:folds) {
  for (j in 1:length(timepoints)) {
    inner_aucs[[j]] <- c(
      "Hospital" = i,
      "Timepoint" = timepoints[j],
      "AUC" = with(
        subset(df, Facility == i),
        timeROC(
          T = index2,
          delta = outcome,
          marker = risk,
          cause = 1,
          times = timepoints[j]
        )
      )$AUC[2]
    )
    outer_aucs[[i]] <- inner_aucs
    print(paste0("Timepoint ", j, " done"))
  }
  print(paste0("Hospital ", i, " done"))
}

df_auc <- rbind.data.frame(
  do.call(rbind, outer_aucs[[1]]),
  do.call(rbind, outer_aucs[[2]]),
  do.call(rbind, outer_aucs[[3]]),
  do.call(rbind, outer_aucs[[4]]),
  do.call(rbind, outer_aucs[[5]])
)
df_auc <- rename(df_auc, "AUC" = "AUC.t=12")
df_auc$Hospital <- as.factor(df_auc$Hospital)
remove(inner_aucs, outer_aucs)

df_auc <- df_auc |>
  group_by(Timepoint) |>
  summarise(AUC = mean(AUC)) |>
  mutate(Hospital = "Mean") |>
  bind_rows(df_auc)

p_auc <- df_auc |> ggplot()

# Calibration plot
# Restrict observations to 1 per patient per day to make dataset more manageable (still crashes occasionally anyway)
set.seed(888)
df_calib <- df |>
  mutate(
    day = ceiling(index / 24),
    Hospital = as.factor(Facility)
  ) |>
  arrange(ID, index) |>
  group_by(ID, day) |>
  slice_sample(n = 1) |>
  ungroup()

for (i in 1:5) {
  model <- readRDS(paste0("filepath", "coxfit_fold", i, ".rds"))
  df_calib$pred[df_calib$Facility == i] <- predictRisk(model, newdata = subset(df_calib, Facility == i), times = 24)
  remove(model)
  print(i)
}
p_calib <- df_calib |> ggplot()

# Combined plot
library(patchwork)
colours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "black")
timepoints <- c(12, 24, 48, 72, 168)
linetypes <- c(rep("solid", 5), "dotdash")
xlabs <- c("12", "24", "48", "72", "168")

p_auc +
  geom_line(aes(x = Timepoint, y = AUC, group = Hospital, colour = Hospital, linetype = Hospital), linewidth = 1.1) +
  geom_vline(xintercept = timepoints, alpha = 0.3) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.tag = element_text()
  ) +
  scale_colour_manual(values = colours) +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(breaks = timepoints, labels = xlabs) +
  scale_y_continuous(limits = c(0.70, 1), breaks = seq(0.70, 1, 0.05), expand = c(0, 0)) +
  labs(
    x = "Hours since first observation",
    y = "Time-dependent AUC",
    tag = "A"
  ) +
  p_calib +
  stat_plsmo(aes(x = pred, y = died_24h, colour = Hospital), span = 1, linewidth = 1.1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.tag = element_text()
  ) +
  annotate("text", x = 0.35, y = 0.85, label = "Model underestimates risks") +
  annotate("text", x = 0.50, y = 0, label = "Model overestimates risks") +
  scale_colour_manual(values = colours) +
  scale_linetype_manual(values = linetypes) +
  coord_equal() +
  labs(
    x = "Predicted risks",
    y = "Observed risks",
    tag = "B"
  ) +
  plot_layout(ncol = 1)

ggsave("coxplots.jpg", height = 8, width = 6)

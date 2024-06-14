# Compare discrete time logistic model to Cox model
options(scipen = 100, digits = 5)
library(tidyverse)
library(arrow)
library(rms)
library(pROC)
import <- "custom filepath"

# Create dataset with one obs/patient/day to predict death within 24 hours (consistent with Cox model performance metrics)
df <- read_parquet(paste0(import, "df_surv.parquet"))
df_discrete <- df |>
  mutate(Day = floor((index + 1) / 24)) |> # Create day of admission variable
  arrange(ID, index) |>
  group_by(ID, Day) |>
  slice(1) |>
  ungroup()

preds <- list()
folds <- max(df$Facility)

for (i in 1:5) {
  set.seed(888)
  train <- subset(df_discrete, Facility != i)
  test <- subset(df_discrete, Facility == i)
  fit <- glm(
    formula = died_24h ~
      Day +
      rcs(Resp_Rate) + rcs(SpO2) +
      rcs(SBP) + rcs(DBP) +
      rcs(Pulse) + rcs(Temp) +
      +rcs(Age) +
      Alert + Verbal + Unresponsive + O2_Therapy,
    family = binomial(link = "cloglog"),
    data = train
  )

  preds[[i]] <- predict(fit, newdata = test, type = "response")
  print(i)
}

for (i in 1:folds) df_discrete$pred[df_discrete$Facility == i] <- preds[[i]]

# AUC
folds <- max(df$Facility)
times <- c(12, 24, 48, 72, 168)
timepoints <- c("died_12h", "died_24h", "died_48h", "died_72h", "died_168h")
inner_aucs <- list()
outer_aucs <- list()

for (i in 1:folds) {
  for (j in 1:length(timepoints)) {
    inner_aucs[[j]] <- c(
      "Hospital" = i,
      "Timepoint" = times[j],
      "AUC" = auc(
        predictor = df_discrete$pred[df_discrete$Facility == i],
        response = df_discrete[[timepoints[j]]][df_discrete$Facility == i]
      )
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

df_auc$Hospital <- as.factor(df_auc$Hospital)
df_auc$AUC <- as.numeric(df_auc$AUC)
remove(inner_aucs, outer_aucs, i, j)

df_auc <- df_auc |>
  group_by(Timepoint) |>
  summarise(AUC = mean(AUC)) |>
  mutate(Hospital = "Mean") |>
  bind_rows(df_auc)

library(patchwork)

colours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "black")
timepoints <- c(12, 24, 48, 72, 168)
linetypes <- c(rep("solid", 5), "dotdash")
xlabs <- c("12", "24", "48", "72", "168")

p_auc <- df_auc |> ggplot()
p_calib <- df_discrete |>
  mutate(Hospital = as.factor(Facility)) |>
  ggplot()

(p_auc +
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
  scale_y_continuous(limits = c(0.7, 1), breaks = c(seq(0.7, 1, 0.05)), expand = c(0, 0)) +
  labs(
    x = "Hours since first observation",
    y = "Time-dependent AUC",
    tag = "A"
  )) /
  p_calib +
  geom_abline() +
  stat_plsmo(aes(x = pred, y = died_24h, colour = Hospital), span = 1, linewidth = 1.1) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_colour_manual(values = colours) +
  annotate("text", x = 0.3, y = 0.9, label = "Model underestimates risks") +
  annotate("text", x = 0.8, y = 0.3, label = "Model overestimates risks") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Predicted risks",
    y = "Observed risks",
    tag = "B"
  )

ggsave("./discretetime_plots.jpg", height = 8, width = 6)

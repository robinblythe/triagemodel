#Figures
gc()
options(scipen=100, digits = 5)

library(tidyverse)


#AUC plot

df_auc <- readRDS("./auc_by_fold_time.rds")

df_auc <- df_auc %>%
  group_by(Timepoint) %>%
  summarise(AUC = mean(AUC)) %>%
  mutate(Hospital = "Mean") %>%
  bind_rows(df_auc)

colours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "black")
linetypes <- c(rep("solid", 5), "dotdash")
xlabs <- c("Day 1", "Day 7", "Day 14", "Day 21")

p <- df_auc |> ggplot()
p +
  geom_line(aes(x = Timepoint, y = AUC, group = Hospital, colour = Hospital, linetype = Hospital), linewidth = 1.1) +
  geom_vline(xintercept = c(24,168,336,504), alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_colour_manual(values = colours) +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(breaks = c(24, 168, 336, 504), labels = xlabs) +
  scale_y_continuous(limits = c(0.75, 1), expand = c(0,0)) +
  labs(x = "Prediction times (days since first observation)",
       y = "Area under the time-dependent receiver operating characteristic curve")

ggsave("aucplot.jpg")

#Calibration plot
colours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "black")
df_calib <- readRDS("./calibration_by_fold.rds")
linetypes <- c(rep("solid", 5), "dotdash")

p <- subset(df_calib) |> ggplot()
p + 
  geom_smooth(aes(exp, pois, colour = Hospital), linewidth = 1.1, se = F) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  annotate("text", x = 1.5, y = 4.5, label = "Model underestimates risks") +
  annotate("text", x = 3.5, y = 0.5, label = "Model overestimates risks") +
  scale_colour_manual(values = colours) +
  scale_linetype_manual(values = linetypes) +
  coord_equal() +
  labs(x = "Cumulative hazard: Cox regression (predicted)",
       y = "Cumulative hazard: Poisson regression (observed)")

ggsave("calplot.jpg")



#Density plots for each day
library(arrow)
import <- "insert custom filepath"
df <- read_parquet(paste0(import, "df_risk.parquet"))

p <- df |>
  mutate(Survived = case_when(outcome == 0 ~ "Survived",
                              outcome == 1 ~ "Died"),
         Days = ceiling(index/24)+1,
         Hospital = Facility) |>
  ggplot()

hosps <- c(
  "1" = "Hospital 1",
  "2" = "Hospital 2",
  "3" = "Hospital 3",
  "4" = "Hospital 4",
  "5" = "Hospital 5"
)

p +
  stat_ecdf(aes(x = Days, linetype = Survived)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~Hospital, ncol = 1, labeller = labeller(Hospital = hosps)) +
  scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = "Days since ward admission",
       y = "Percent of observations (cumulative)")

ggsave("cdfplot.jpg")


#Relationships between variables and outcome
import <- "insert custom filepath"
df <- read_parquet(paste0(import, "df_surv.parquet"))

library(cowplot)

p1 <- ggplot(df, aes(x = Resp_Rate, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Respiratory rate", y = "Mortality rate")

p2 <- ggplot(df, aes(x = SpO2, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +  
  theme(axis.title.y = element_blank()) +
  labs(x = "Oxygen saturation (%)", y = "Mortality rate")

p3 <- ggplot(df, aes(x = SBP, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +  
  theme(axis.title.y = element_blank()) +
  labs(x = "Systolic blood pressure", y = "Mortality rate")

p4 <- ggplot(df, aes(x = DBP, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) + 
  labs(x = "Diastolic blood pressure", y = "Mortality rate")

p5 <- ggplot(df, aes(x = Pulse, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +  
  theme(axis.title.y = element_blank()) +
  labs(x = "Pulse rate", y = "Mortality rate")

p6 <- ggplot(df, aes(x = Temp, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Temperature", y = "Mortality rate")

p7 <- ggplot(df, aes(x = Age, y = outcome)) + 
  geom_smooth(colour = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Age at admission (years)", y = "Mortality rate")

title <- ggdraw() +
  draw_label(
    "Smoothed relationships between predictor and outcome",
    fontface = "bold",
    x = 0,
    hjust = 0
  )

plot_row <- plot_grid(p1, p2, p3, p4, p5, p6, NULL, p7, NULL)
plot_grid(title, plot_row, nrow = 2, rel_heights = c(0.1, 1))
ggsave("xplots.jpg")


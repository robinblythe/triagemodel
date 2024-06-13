#Full model specification
options(scipen=100, digits = 5)
library(rms)
library(arrow)
library(tidyverse)
import <- "custom filepath"
df <- read_parquet(paste0(import, "df_surv.parquet"))

#Required sample size calculations 
#Based on prediction models from 10.1136/bmj.m1501
get_D_from_c <- function(c) {
  5.5 * (c - 0.5) + 10.26 * (c - 0.5)^3
}

get_r2_from_D <- function(D,
                          sigma2 = (pi^2) / 3,
                          k = sqrt(8 / pi)) {
  (D^2 / k^2) / (sigma2 + (D^2) / (k^2))
}

get_r2_from_c <- function(c, ...) {
  get_r2_from_D(D = get_D_from_c(c), ...)
}

pmsampsize::pmsampsize(
  type = "s",
  csrsquared = get_r2_from_c(c = 0.80),
  parameters = 35,
  rate = 0.003, #From 10.1016/j.jclinepi.2023.05.020
  timepoint = 24, #Interested in ~24 hours as first pass
  meanfup = 74 #Assuming roughly 3.1 days of mean follow up time based on length of stay across hospitals
)

#Model fitting
df <- df |>
  rename(`Respiratory rate` = Resp_Rate,
         `Oxygen saturation %` = SpO2,
         `Systolic blood pressure` = SBP,
         `Diastolic blood pressure` = DBP,
         `Temperature` = Temp,
         `Supplemental oxygen` = O2_Therapy)

dd <- datadist(df)
options(datadist = dd)

# Select number of knots for each spline by checking AIC
coxfit <- cph(Surv(index, index2, outcome) ~
                rcs(`Respiratory rate`, 4) + rcs(`Oxygen saturation %`, 5) + 
                rcs(`Systolic blood pressure`, 5) + rcs(`Diastolic blood pressure`, 5) + 
                rcs(Pulse, 5) + rcs(`Temperature`, 5) + 
                Alert + Verbal + Unresponsive + `Supplemental oxygen` + rcs(Age, 3),
              x = TRUE, y = TRUE, iter.max = 1000,
              data = df)

AIC(coxfit) #Use to determine # of knots
anova(coxfit) #Wald test for non-linearity
specs(coxfit) #To obtain knot placements
Function(coxfit) # To obtain model equation

#Plot coefficients (figure 3)
library(ggplot2)
library(patchwork)
p <- Predict(coxfit) |> 
  ggplot(sepdiscrete = "list")

(p$continuous +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    ggtitle("Continuous predictors") +
    theme(plot.title = element_text(size = 12))) /
  (p$discrete +
     theme_bw() +
     theme(panel.grid.minor = element_blank()) +
     scale_x_continuous(breaks = seq(-5, 5, 2.5), limits = c(-5, 5)) +
     coord_flip() +
     ggtitle("Binary predictors") +
     theme(plot.title = element_text(size = 12))) +
  plot_layout(heights = c(2, 1))


ggsave(filename = "Figure 3.jpg", height = 8, width = 6)
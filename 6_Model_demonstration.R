# Create rank map (figure 6)

options(scipen = 100, digits = 5)
library(arrow)
library(rms)
library(tidyverse)
import <- "custom filepath"
df <- read_parquet(paste0(import, "df_risk.parquet"))

# Reduce dataset to only first 48 hours and drop patients with shorter LOS
df1 <- df |>
  group_by(ID) |>
  filter(index_max > 48,
         index2 <= 48) |>
  arrange(ID, index) |>
  ungroup()

set.seed(888)
df_died <- df1 |>
  filter(died_168h == 1) |>
  filter(ID %in% sample(ID, 2))

df_survived <- df1 |>
  filter(Died == 0) |>
  filter(ID %in% sample(ID, 8))

df1 <- full_join(df_died, df_survived) |>
  arrange(ID, Died)

remove(df_died, df_survived, df_lrm)

#Reduce down to single observation every 8 hours - to simulate checking every shift change
df8 <- df1 |>
  filter(index2 < 8) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 8)

df16 <- df1 |>
  filter(index2 < 16) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 16)

df24 <- df1 |>
  filter(index2 < 24) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 24)

df32 <- df1 |>
  filter(index2 < 32) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 32)

df40 <- df1 |>
  filter(index2 < 40) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 40)

df48 <- df1 |>
  filter(index2 < 48) |>
  group_by(ID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(risk_cox)) |>
  mutate(rank_cox = row_number(),
         Timepoint = 48)

df_rank <- do.call(rbind, list(df8, df16, df24, df32, df40, df48))
df_rank <- df_rank |>
  select(ID, Timepoint, rank_cox, died_12h, died_24h, died_48h, died_72h, died_168h, Died) |>
  mutate(ID = as.factor(as.numeric(as.factor(df_rank$ID))),
         `Died in hospital` = as.factor(Died))

remove(df1, df8, df16, df24, df32, df40, df48, fit_lrm, coxfit)

p <- df_rank |>
  ggplot(aes(x = Timepoint, y = rank_cox, group = ID))

colours <- viridis::viridis(10)

p + 
  geom_line(aes(colour = ID, linetype = `Died in hospital`), linewidth = 1.5, alpha = 0.8) +
  geom_point(aes(colour = ID), size = 3) +
  scale_y_reverse(breaks = 1:30) +
  scale_x_continuous(breaks = seq(8, 48, 8), limits = c(8, 48)) +
  scale_colour_manual(values = colours) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Hours since first observation",
       y = "Rank order from Cox model")

ggsave("./rank_map.jpg", height = 6, width = 6)


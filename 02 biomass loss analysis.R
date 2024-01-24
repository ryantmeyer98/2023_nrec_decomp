# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
biomass.df <- read_csv("output/clean_drywts.csv")

# MODIFICATIONS ----

# calculate the % mass remaining for forage bags
biomass.df <- biomass.df %>%
  mutate(forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100)

# calculate the % mass remaining for tea bags
biomass.df <- biomass.df %>%
  mutate(tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100)

# wanna see how stuff looks
biomass.df %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge2(width = 0.5)) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5,
  #              position = position_dodge2(width = 0.5)) +
  stat_summary(fun = mean, geom = "line")








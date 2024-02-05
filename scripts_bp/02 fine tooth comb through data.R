# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
biomass.df <- read_csv("output/clean_drywts.csv")

# want to break this down into the forage and tea bag datasets for right now 
forage.df <- biomass.df %>%
  select(-c(tea_id, tea_final_drywt_g, tea_initial_drywt_g))

tea.df <- biomass.df %>%
  select(-c(tea_id, forage_initial_drywt_g, forage_final_drywt_g))

# MODIFICATIONS ----

# calculate the % mass remaining for forage bags
forage.df <- forage.df %>%
  mutate(forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100)

# calculate the % mass remaining for tea bags
tea.df <- tea.df %>%
  mutate(tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100)

# ALL OF THE ABOVE SHOULD GO IN 01 and then save to a final tea and forage Df and 
# then do the stuff to below in separeate code sheets!!!

# STARTING WITH FORAGE DATASET ----

# wanna see how stuff looks

# ISU
forage.df %>%
  filter(location == "isu") %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop)) +
  # stat_summary(fun = mean, geom = "point", position = position_dodge2(width = 0.5)) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5,
  #              position = position_dodge2(width = 0.5)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge2(width = 0.5))

# WIU
forage.df %>%
  filter(location == "wiu") %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop)) +
  # stat_summary(fun = mean, geom = "point", position = position_dodge2(width = 0.5)) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5,
  #              position = position_dodge2(width = 0.5)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge2(width = 0.5))

# so before we start thinking about fitting any curves we need to 
# 1: get the starting pct_remaining to be 100%, 2 figures out why the pct remaining increases
# at some points 

# setting all initials to be 100%
forage.df <- forage.df %>%
  mutate(forage_pct_remain = case_when(
    sample_time == "t0" ~ 100,
    TRUE ~ forage_pct_remain
  ))

# lets start isolating some data to see where increases happen
test.df <- forage.df %>%
  filter(forage_final_drywt_g > forage_initial_drywt_g)

# 217, 57, 347, 17, 125, 195, 44, 56, 167, 184, 117, 202, 75, 124, 21, 107, 165, 156, 323, 

# those are the IDs for the problem locations, going to get rid of them
forage.df <- forage.df %>%
  filter(bag_no != 217 | bag_no != 57 | bag_no != 347 | bag_no != 17 | bag_no != 125 | bag_no != 195 | 
           bag_no != 44 |bag_no != 56 | bag_no != 167 | bag_no != 184 | bag_no != 117 | bag_no != 202 | 
           bag_no != 75 | bag_no != 124 | bag_no != 21 | bag_no != 107 | bag_no != 165 | bag_no != 156 | 
           bag_no != 323)

# so things are looking good i wanna see what the standard error for some of these odd looking data
forage.df %>%
  filter(location == "isu") %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop, shape = as.factor(block))) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(size = 2, position = position_dodge2(width = 0.2)) +
  ggtitle("ISU") +
  facet_wrap(~crop)

# what about wiu? 
forage.df %>%
  filter(location == "wiu") %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop, shape = as.factor(block))) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(size = 2, position = position_dodge2(width = 0.2)) +
  ggtitle("wiu") +
  facet_wrap(~crop)

# TEA BAGS



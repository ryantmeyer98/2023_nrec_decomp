# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# READ IN THE DATA ----

# forage bag data
# decomp data
raw.df <- read_excel("data/Meyer_nrec2023decompbagdata.xlsx") %>% 
  clean_names()

# tea bag data
# initial tea weights
tea.df <- read_excel("data/tea wt.xlsx") %>% clean_names()

# CLEAN THE DATAFRAMES PRIOR TO MERGING ----

# convert all text to lowercase
raw.df <- raw.df %>%
  mutate(crop = tolower(crop),
         location = tolower(location))

# convert the dry bad with no staple weight to a number
raw.df <- raw.df %>%
  mutate(dry_no_staple = as.numeric(dry_no_staple))

# calculate initial dry and initial final weights
raw.df <- raw.df %>%
  mutate(forage_initial_drywt_g = bag_no_staple - bag_wt_g_white) %>%
  mutate(forage_final_drywt_g = dry_no_staple - bag_wt_g_white)

# select the columns we care about
raw.df <- raw.df %>%
  select(location, crop, block, sample_time, forage_initial_drywt_g, forage_final_drywt_g)

# plot this up to make sure something didn't go horribly wrong
raw.df %>%
  ggplot(aes(sample_time, forage_final_drywt_g)) +
  geom_point()

# remove na-values where bags were lost 
raw.df <- raw.df %>%
  na.omit()

















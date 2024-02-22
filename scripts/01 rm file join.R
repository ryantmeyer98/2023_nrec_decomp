# LIBRARIES ----
library(tidyverse)
library(readxl)

# READ IN THE DATA ----

# forage bags
forage.df <- read_excel("data/23 nrec biomass outliers fixed.xlsx")

# get the numnber of samples per location, crop, and block
forage.df %>%
  group_by(crop, block) %>%
  summarize(n = n())


# tea bags
tea.df <- read_excel("data/tea wt.xlsx") %>%
  rename(tea_initial_drywt_g = initial_dry_wt_g)

# carbon and nitrogen data 
cn.df <- read_excel("data/usda carbon nitrogen data.xlsx")

# CHECKING FOR DUPLICATES ----

# forage bags ----
forage.df %>%
  group_by(forage_id) %>%
  filter(n() > 1) %>% summarize(n = n())

# tea bags ----
tea.df %>%
  group_by(tea_id) %>%
  filter(n() > 1) %>% summarize(n = n())

# cn data ---- # would be good to keep -BP
cn.df %>%
  group_by(id) %>%
  filter(n() > 1) %>% summarize(n = n())

# dupes are clean!!!!!

# MODIFICATIONS PRIOR TO JOINING ----

# there are a few extra columns in the tea datasheet to remove, doing that here, also clean names up
tea.df <- tea.df %>%
  select(tea_id, tea_initial_drywt_g)

# cleaning forage dataset ----

# need to make forage_id and tea_id numeric as well
forage.df <- forage.df %>%
  # mutate(forage_id = as.numeric(forage_id)) %>% # should NEVER NEED THIS!!!!
  mutate(tea_id = as.numeric(tea_id))  # WHY ARE THERE 255A abnd the other A and you lose them

# THERE ARE NO MISSING VALUES HERE NOW!!!
# # remove places where samples were lost
# forage.df <- forage.df %>%
#   filter(!is.na(sample_time))

# cleaning carbon and nitrogen dataset ----
# we then need to split by tea and forage data and remove the F and T
tea_cn.df <- cn.df %>%
  filter(str_detect(id, "^T")) %>%
  rename(tea_id = id,
         tea_pct_n = pct_n,
         tea_pct_c = pct_c) %>%
  mutate(tea_id = substr(tea_id, 2, nchar(tea_id))) %>% # AGAIN I CORRECTED THIS - WOULD NEVER DO BASED ON POSITION!!!!
  mutate(tea_id = as.numeric(tea_id))

# for forage bags
forage_cn.df <- cn.df %>%
  filter(str_detect(id, "^F")) %>%
  rename(forage_id = id,
         forage_pct_n = pct_n,
         forage_pct_c = pct_c) %>%
  mutate(forage_id = substr(forage_id, 2, nchar(forage_id))) %>% # AGAIN I CORRECTED THIS - WOULD NEVER DO BASED ON POSITION!!!!
  mutate(forage_id = as.numeric(forage_id))

# JOIN THE DATA ----

# join the tea initials to the tea finals
full.df <- full_join(forage.df, tea.df, by = "tea_id")

# get the numnber of samples per location, crop, and block
full.df %>%
  group_by(crop, block) %>%
  summarize(n = n())


# join the pct data to the forage bags
full.df <- full_join(full.df, forage_cn.df, by = "forage_id")

# join the pct data to the tea bags
full.df <- full_join(full.df, tea_cn.df, by = "tea_id")

write_csv(full.df, "output/joined data.csv")

# # remove data with no sample time as this indicates that the samples were lost
# full.df <- full.df %>%
#   filter(!is.na(sample_time))

# REMOVE THE WEIGHTS OF THE BAGS AND THE STAPLES ----

# only for the forage bags, we could not do bag weights for the tea bags bc they came pre-bagged
full.df <- full.df %>%
  mutate(forage_initial_drywt_g = forage_initial_drywt_g - (forage_bag_wt_g + staple_weight)) %>%
  mutate(forage_final_drywt_g = forage_final_drywt_g - (forage_bag_wt_g + staple_weight))

# convert t value into the days after placement so that following calculations work
full.df <- full.df %>%
  mutate(days = case_when(
    sample_time == "t0" ~ 0,
    sample_time == "t1" ~ 4,
    sample_time == "t2" ~ 6,
    sample_time == "t3" ~ 8,
    sample_time == "t4" ~ 12,
    sample_time == "t5" ~ 16,
    sample_time == "t6" ~ 20,
    sample_time == "t7" ~ 25,
    sample_time == "t8" ~ 30,
    sample_time == "t9" ~ 35))

# remove the columns we do not want 
full.df <- full.df %>%
  select(-c(staple_weight, forage_id, tea_id, forage_bag_wt_g, sample_time))

# SAVE OUTPUT ----
write_csv(full.df, file = "output/cleaned raw data.csv")




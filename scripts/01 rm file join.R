# LIBRARIES ----
library(tidyverse)
library(readxl)

# READ IN THE DATA ----

# forage bags
forage.df <- read_excel("data/23 nrec decomp biomass.xlsx")

# tea bags
tea.df <- read_excel("data/tea wt.xlsx")

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

# cleaning tea dataset ----

# there are a few extra columns in the tea datasheet to remove, doing that here, also clean names up
tea.df <- tea.df %>%
  select(tea_id, tea_initial_drywt_g =initial_dry_wt_g) %>%
  # rename(tea_initial_drywt_g = initial_dry_wt_g) %>% # you can remove this and rename above -BP
  mutate(tea_id = as.numeric(tea_id))

# cleaning forage dataset ----

# need to make forage_id and tea_id numeric as well
forage.df <- forage.df %>%
  mutate(forage_id = as.numeric(forage_id)) %>%
  mutate(tea_id = as.numeric(tea_id))

# cleaning carbon and nitrogen dataset ----

# next we have to remove the data checks (d) from USDA lab
cn.df <- cn.df %>%
  filter(id != "d") # I would keep these !!! -BP as suggested in prior email

# we then need to split by tea and forage data and remove the F and T
tea_cn.df <- cn.df %>%
  filter(str_detect(id, "^T")) %>%
  rename(tea_id = id,
         tea_pct_n = pct_n,
         tea_pct_c = pct_c) %>%
  mutate(tea_id = substr(tea_id, 2, nchar(tea_id))) %>%
  mutate(tea_id = as.numeric(tea_id))

# for forage bags
forage_cn.df <- cn.df %>%
  filter(str_detect(id, "^F")) %>%
  rename(forage_id = id,
         forage_pct_n = pct_n,
         forage_pct_c = pct_c) %>%
  mutate(forage_id = substr(forage_id, 2, nchar(forage_id))) %>%
  mutate(forage_id = as.numeric(forage_id))

# JOIN THE DATA ----

# join the tea initials to the tea finals
full.df <- left_join(forage.df, tea.df, by = "tea_id")

# join the pct data to the forage bags
full.df <- left_join(full.df, forage_cn.df, by = "forage_id")

# foin the pct data to the tea bags
full.df <- left_join(full.df, tea_cn.df, by = "tea_id")

# SAVE OUTPUT ----
write_csv(full.df, file = "output/cleaned raw data.csv")




























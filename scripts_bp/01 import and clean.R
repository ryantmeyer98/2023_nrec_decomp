# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# READ IN THE DATA ----

# forage bag data
raw.df <- read_excel("data_bp/2023_nrec_decomp_2.xlsx") %>% 
  clean_names()

# tea bag data
tea_initial.df <- read_excel("data_bp/2023_tea_wt.xlsx") %>% clean_names()

# carbon nitrogen data
pct_cn.df <- read_excel("data_bp/2024_cn_analyses_mn.xlsx")


# check for duplicated data 
# check for duplicates!!!
raw.df %>%
  group_by(bag_no) %>%
  filter(n() > 1) %>% summarize(n = n())

# check for duplicates!!!
tea_initial.df %>%
  group_by(tea_id) %>%
  filter(n() > 1) %>% summarize(n = n())

# check for duplicates!!!
pct_cn.df %>%
  group_by(id) %>%
  filter(n() > 1) %>% summarize(n = n())
# so there are dups in the pct data


# Initial cleaning of data ----


# clean up the pct_cn data # # # # # # # # #  # # # # # # # # #  # # # # # # # # #  # # # # # # # # # 

# there are duplicates and we need to remove d and fill down and then take average of the duplicates
pct_cn.df <- pct_cn.df %>%
  mutate(
    id = dplyr::na_if(id, "d"))  %>% 
  fill(id)

# now take the mean of duplicate IDs by grouping them 
pct_cn.df <- pct_cn.df %>%
  group_by(id) %>%
  summarise(
    pct_n = mean(pct_n, na.rm = TRUE),
    pct_c = mean(pct_c, na.rm = TRUE))

# check for duplicates
pct_cn.df %>%
  group_by(id) %>%
  filter(n() > 1) %>% summarize(n = n())

# now to get groups of data by forage or tea from pct_cn.df dataframe
#tea
tea_cn.df <- pct_cn.df %>%
  filter(str_detect(id, "^T")) %>% # what is this doing???
  rename(tea_id = id) %>% 
  mutate(tea_id = as.numeric(str_remove_all(tea_id, "T")))

# forage
forage_cn.df <- pct_cn.df %>%
  filter(str_detect(id, "^F")) %>%
  rename(forage_id = id) %>% 
  mutate(forage_id = as.numeric(str_remove_all(forage_id, "F")))
  

# FORAGE DF  # # # # # # # # #  # # # # # # # # #  # # # # # # # # # # # # # # # # # # 
# forage bag cleaning ----
# convert all text to lowercase
raw.df <- raw.df %>%
  mutate(crop = tolower(crop),
         location = tolower(location))

# convert the dry bag with no staple weight to a number
raw.df <- raw.df %>%
  # mutate(dry_no_staple = as.numeric(dry_no_staple)) %>%
  mutate(dry_no_staple = dry_wt_g - staple_weight)

# calculate initial dry and initial final weights
raw.df <- raw.df %>%
  mutate(forage_initial_drywt_g = bag_no_staple - bag_wt_g_white) %>%
  mutate(forage_final_drywt_g = dry_no_staple - bag_wt_g_white)

# select the columns we care about
raw.df <- raw.df %>%
  select(location, crop, bag_no, block, sample_time, forage_initial_drywt_g, forage_final_drywt_g, 
         tea_id, tea_final)

# separate out the data
# forage datafame
forage.df <- raw.df %>%
  select(forage_id = bag_no, location, crop,  block, sample_time, forage_initial_drywt_g, forage_final_drywt_g)

# now join the forage_cn.df with the forage.df 
forage.df <- full_join(forage.df, forage_cn.df, by = "forage_id")



# TEA DF # # # # # # # # #  # # # # # # # # #  # # # # # # # # #  # # # # # # # # # 
# tea dataframe
tea.df <- raw.df %>%
  mutate(tea_id = as.numeric(tea_id)) %>%
  select(location, crop, bag_no, block, sample_time, tea_id, tea_final_dry_wt_g =  tea_final)

# fix tea_initial dataframe
tea_initial.df <- tea_initial.df %>%
  mutate(tea_id = as.numeric(tea_id)) %>%
  select(tea_id, initial_dry_wt_g)

# merge the tea_initial with the tea dataframe to get all data in one place
tea.df <- full_join(tea.df, tea_initial.df,  by = "tea_id")

# reorder the tea dataframe
tea.df <- tea.df %>% 
  select(tea_id, location, crop, bag_no, block, sample_time, 
         tea_initial_dry_wt_g = initial_dry_wt_g, tea_final_dry_wt_g )

tea.df <- tea.df %>% 
  mutate(tea_initial_dry_wt_g = as.numeric(tea_initial_dry_wt_g))

# now join pct cn 
tea.df <- full_join(tea.df, tea_cn.df, by = "tea_id")



# remove the old dataframes
rm(tea_initial.df, raw.df, forage_cn.df, tea_cn.df, pct_cn.df) 






# PERCENT MASS REMAINING ----

# calculate percent remaining and proportions
full.df <- full.df %>%
  mutate(forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100, na.rm = TRUE) %>%
  mutate(tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100, na.rm = TRUE) 

# calculate percent c and n remaining

# first get everything to a proportion
full.df <- full.df %>%
  mutate(forage_prop_c = forage_pct_c / 100, 
         forage_prop_n = forage_pct_n / 100,
         tea_prop_c = tea_pct_c / 100,
         tea_prop_n = tea_pct_n / 100) 

# then, summarize from the initial proportion
forage_initial_prop_c <- full.df %>%
  filter(sample_time == "t0") %>%
  group_by(crop) %>%
  summarize(forage_initial_prop_c = mean(forage_prop_c, na.rm = TRUE))

# ar 0.4190000; cr 0.4252500; gpc 0.4400000; pcro 0.4513333; wpc 0.4315000

# multiply each sample by the initial proportion of carbon to get the initial for every sample
full.df <- full.df %>%
  mutate(forage_initial_carbon = case_when(
    crop == "ar" ~ forage_initial_drywt_g * 0.4190000,
    crop == "cr" ~ forage_initial_drywt_g * 0.4252500, 
    crop == "gpc" ~ forage_initial_drywt_g * 0.4400000, 
    crop == "pcro" ~ forage_initial_drywt_g * 0.4513333, 
    crop == "wpc" ~ forage_initial_drywt_g * 0.4315000
  ))

# now we take the final weight and multiply by the collected proportion to get collected c
full.df <- full.df %>%
  mutate(forage_final_carbon = forage_final_drywt_g * forage_prop_c)

# finally, divide initial vby collected to get prop c remaining and multiply by 100 for pct
full.df <- full.df %>%
  mutate(forage_prop_c_remain = forage_final_carbon / forage_initial_carbon) %>%
  mutate(forage_pct_c_remain = forage_prop_c_remain * 100)
  

# save the file to output diredtory
write_csv(full.df, "output/clean_drywts.csv")

# forage_initial_prop_n <- full.df %>%
# 
# tea_initial_prop_c <- full.df %>%
# 
# tea_initial_prop_n <- full.df 
# 
#
# finally divide initial by collected to get prop n remaining then * 100 for pct
full.df <- full.df %>%
  mutate(prop_c_remain = collected_prop_c / initial_prop_c) %>%
  mutate(pct_c_remaing = prop_c_remain * 100)



# some preliminary plotting to see how things look, are we fucked?
full.df %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  geom_point() +
  geom_smooth() +
  facet_grid(crop~location)

full.df %>%
  ggplot(aes(sample_time, forage_pct_n, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  facet_grid(crop~location)

full.df %>%
  ggplot(aes(sample_time, forage_pct_c_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  facet_grid(crop~location)

full.df %>%
  ggplot(aes(sample_time, tea_pct_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  facet_grid(crop~location)

full.df %>%
  ggplot(aes(sample_time, tea_pct_c, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  facet_grid(crop~location)

full.df %>%
  ggplot(aes(sample_time, tea_pct_n, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
  facet_grid(crop~location)

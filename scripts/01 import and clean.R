# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# READ IN THE DATA ----

# forage bag data
raw.df <- read_excel("data/Meyer_nrec2023decompbagdata.xlsx") %>% 
  clean_names()

# tea bag data
tea.df <- read_excel("data/tea wt.xlsx") %>% clean_names()

# carbon nitrogen data
pct_cn <- read_excel("data/RG 2023_ISU Plant CN.xlsx")

# CLEAN THE DATAFRAMES PRIOR TO MERGING ----

# forage bag cleaning ----

# convert all text to lowercase
raw.df <- raw.df %>%
  mutate(crop = tolower(crop),
         location = tolower(location))

# convert the dry bag with no staple weight to a number
raw.df <- raw.df %>%
  mutate(dry_no_staple = as.numeric(dry_no_staple))

# calculate initial dry and initial final weights
raw.df <- raw.df %>%
  mutate(forage_initial_drywt_g = bag_no_staple - bag_wt_g_white) %>%
  mutate(forage_final_drywt_g = dry_no_staple - bag_wt_g_white)

# select the columns we care about
raw.df <- raw.df %>%
  select(location, crop, bag_no, block, sample_time, forage_initial_drywt_g, forage_final_drywt_g, 
         tea_id, tea_final)

# remove na-values where bags were lost 
raw.df <- raw.df %>%
  na.omit()

# tea bag cleaning ----

# select the columns we care about
tea.df <- tea.df %>%
  select(tea_id, initial_dry_wt_g)

# rename 
tea.df <- tea.df %>%
  rename(tea_initial_drywt_g = initial_dry_wt_g) %>%
  mutate(tea_id = as.character(tea_id))

# join the tea and forage bag data ----
full.df <- left_join(raw.df, tea.df, by = "tea_id")

# cleaning full dataframe ----

# rename columns and make correct columns numeric
full.df <- full.df %>%
  mutate(tea_initial_drywt_g = as.numeric(tea_initial_drywt_g)) %>%
  rename(tea_final_drywt_g = tea_final) %>%
  rename(forage_id = bag_no)

# remove the data we are no longer using
rm(raw.df, tea.df)

# get the carbon and nitrogen data in there ----

# first thing i am going to do is make columns for forage and tea bag id
pct_cn <- pct_cn %>%
  filter(id != "d")

# seems like the best way to do this is going to be to get dataframes for forage and tea data

#tea
tea_cn <- pct_cn %>%
  filter(str_detect(id, "^T")) %>%
  rename(tea_id = id)

# forage
forage_cn <- pct_cn %>%
  filter(str_detect(id, "^F")) %>%
  rename(forage_id = id)

# getting everything joined together now ----

# forage carbon and nitrogen

# get character set up correctly 
full.df <- full.df %>%
  mutate(forage_id = as.numeric(forage_id)) %>%
  mutate(tea_id = as.numeric(tea_id))

# remove the f
forage_cn <- forage_cn %>%
  mutate(forage_id = substr(forage_id, 2, nchar(forage_id))) %>%
  mutate(forage_id = as.numeric(forage_id))

# rename columns
forage_cn <- forage_cn %>%
  rename(forage_pct_c = pct_c,
         forage_pct_n = pct_n)

full.df <- left_join(full.df, forage_cn, by = "forage_id")

# tea bag carbon and nitrogen

# remove the t
tea_cn <- tea_cn %>%
  mutate(tea_id = substr(tea_id, 2, nchar(tea_id))) %>%
  mutate(tea_id = as.numeric(tea_id))

# rename columns
tea_cn <- tea_cn %>%
  rename(tea_pct_c = pct_c,
         tea_pct_n = pct_n)

# join everything
full.df <- left_join(full.df, tea_cn, by = "tea_id", relationship = "many-to-many")

# remove the stuff we are no longer using
rm(forage_cn, pct_cn, tea_cn, test.df)

# PERCENT MASS REMAINING ----

# calculate percent remaining and proportions
full.df <- full.df %>%
  mutate(forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100) %>%
  mutate(tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100) 

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
  mutate(forage_initial_prop_c = case_when(
    crop == "ar" ~ forage_initial_drywt_g * 0.4190000,
    crop == "cr" ~ forage_initial_drywt_g * 0.4252500, 
    crop == "gpc" ~ forage_initial_drywt_g * 0.4400000, 
    crop == "pcro" ~ forage_initial_drywt_g * 0.4513333, 
    crop == "wpc" ~ forage_initial_drywt_g * 0.4315000
  ))

# now we take the final weight and multiply by the collected proportion to get collected c
full.df <- full.df %>%
  mutate(forage_collected_prop_c = forage_initial_drywt_g * forage_prop_c)

# finally, divide initial vby collected to get prop c remaining and multiply by 100 for pct
full.df <- full.df %>%
  mutate(forage_prop_c_remain = forage_collected_prop_c / forage_initial_prop_c) %>%
  mutate(forage_pct_c_remain = forage_prop_c_remain * 100)
  

forage_initial_prop_n <- full.df %>%

tea_initial_prop_c <- full.df %>%

tea_initial_prop_n <- full.df 

# 
# # finally divide initial by collected to get prop n remaining then * 100 for pct
# full.df <- full.df %>%
#   mutate(prop_c_remain = collected_prop_c / initial_prop_c) %>%
#   mutate(pct_c_remaing = prop_c_remain * 100)



# some preliminary plotting to see how things look, are we fucked?
full.df %>%
  ggplot(aes(sample_time, forage_pct_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar") +
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

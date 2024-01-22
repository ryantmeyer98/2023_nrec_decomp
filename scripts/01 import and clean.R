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

# remove the unnecessary columns and save final dataframe to csv file
final.df <- raw.df %>% 
  select(location, plot, crop, bag_no, series_no, block, sample_time, initial_drywt_g, final_drywt_g)

write_csv(final.df, "output/nrec_decomp_2_final_df.csv")

# Now to clean the Tea dataframe and merge with the initial weights
tea_final_drywt.df <- raw.df %>% 
  select(location, plot, crop, bag_no, series_no, block, tea_id, tea_final ) %>% 
  mutate(crop = tolower(crop),
         location = tolower(location)) 

tea_final.df <- full_join(tea.df, tea_final_drywt.df, by = c("tea_id", "crop", "block"))

# taking only the tea columns that we want and reorder
tea_final.df <- tea_final.df %>%
  select(location, crop, plot, block, number, bag_no, series_no, tea_id,
         tea_initial_drywt_g = initial_dry_wt_g,
         tea_final_drywt_g = tea_final, notes
  )

write_csv(tea_final.df, "output/tea_final_drywts.csv")















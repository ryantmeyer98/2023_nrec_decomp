# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

# READ IN THE DATA----

# decomp data
raw.df <- read_excel("data/Meyer_nrec2023decompbagdata.xlsx") %>% 
  clean_names()

# initial tea weights
tea.df <- read_excel("data/tea wt.xlsx") %>% clean_names()

# CLEAN DATA ----

# cleaning the raw data ----
# for the raw data need to get initial biomass wet and final biomass wt

raw.df <- raw.df %>%
  mutate(crop = tolower(crop),
         location = tolower(location)) %>% 
  mutate(initial_drywt_g = bag_no_staple - bag_no) %>%
  mutate(final_drywt_g = dry_no_staple) 

# remove the unnecessary columns and save final dataframe to csv file
final.df <- raw.df %>% 
  select(location, plot, crop, bag_no, series_no, block, initial_drywt_g, final_drywt_g)

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



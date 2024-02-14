# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
decomp.df <- read_csv("output/cleaned raw data.csv")

# GET COLUMNS ALL SET UP TO BE THE RIGHT KIND OF THING ----
decomp.df <- decomp.df %>%
  mutate(location = as.factor(location),
         crop = as.factor(crop),
         tea_initial_drywt_g = as.numeric(tea_initial_drywt_g))

# CALCULATE PERCENT MASS REMAINING ----

# arrange - IMPORTANT BECAUSE PCT REMAIN CALC IS POSITIONAL BASED ON T0, THIS MUST BE CORRECT!!!!
decomp.df <- decomp.df %>%
  arrange(location, crop, block, days)

# forage bags ----
decomp.df <- decomp.df %>%
  mutate(
    forage_mass_loss = forage_initial_drywt_g - forage_final_drywt_g,
    forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100) %>%
# calculate proportion of nutrient remaining
  mutate(
    forage_prop_c = forage_pct_c / 100, 
    forage_prop_n = forage_pct_n / 100) %>%
# calculate final weight of nutrient
  mutate(
    forage_final_c_g = forage_prop_c * forage_final_drywt_g,
    forage_final_n_g = forage_prop_n * forage_final_drywt_g)

# calculate the initial c for earch group of location, crop, block, using sample time 0 for that group
# for carbon
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(forage_initial_c_g = ifelse(days == 0, forage_final_c_g, NA)) %>%
  fill(forage_initial_c_g, .direction = "down") %>%
  mutate(forage_pct_c_remain = forage_final_c_g / forage_initial_c_g * 100) %>%
  ungroup()

# for nitrogen
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(forage_initial_n_g = ifelse(days == 0, forage_final_n_g, NA)) %>%
  fill(forage_initial_n_g, .direction = "down") %>%
  mutate(forage_pct_n_remain = forage_final_n_g / forage_initial_n_g * 100) %>%
  ungroup()

# tea bags ----
decomp.df <- decomp.df %>%
  mutate(
    tea_mass_loss = tea_initial_drywt_g - tea_final_drywt_g,
    tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100) %>%
  # calculate proportion of nutrient remaining
  mutate(
    tea_prop_c = tea_pct_c / 100, 
    tea_prop_n = tea_pct_n / 100) %>%
  # calculate final weight of nutrient
  mutate(
    tea_final_c_g = tea_prop_c * tea_final_drywt_g,
    tea_final_n_g = tea_prop_n * tea_final_drywt_g)

# calculate the initial c for earch group of location, crop, block, using sample time 0 for that group
# for carbon
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(tea_initial_c_g = ifelse(days == 0, tea_final_c_g, NA)) %>%
  fill(tea_initial_c_g, .direction = "down") %>%
  mutate(tea_pct_c_remain = tea_final_c_g / tea_initial_c_g * 100) %>%
  ungroup()

# for nitrogen
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(tea_initial_n_g = ifelse(days == 0, tea_final_n_g, NA)) %>%
  fill(tea_initial_n_g, .direction = "down") %>%
  mutate(tea_pct_n_remain = tea_final_n_g / tea_initial_n_g * 100) %>%
  ungroup()

# REMOVE COLUMNS WE WILL NOT USE IN THE FUTURE ----
decomp.df <- decomp.df %>%
  select(forage_id, tea_id, location, crop, block, days, forage_pct_remain, forage_pct_c_remain,
         forage_pct_n_remain,
         tea_pct_remain, tea_pct_c_remain, tea_pct_n_remain)

# SAVE THE OUTPUT TO A CSV ----
write_csv(decomp.df, file = "output/pct remaining data.csv")

# SOME EARLY PLOTTING ----

# bill to look at 
decomp.df %>%
  ggplot(aes(days, forage_pct_c_remain, color = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  geom_point() +
  facet_grid(location~crop)

# LOOKING FOR OUTLIERS ----
outlier.df <- decomp.df %>%
  filter(location == "WIU") %>%
  filter(crop == "PCRO")

test.df <- decomp.df %>%
  filter(location == "WIU") %>%
  filter(crop == "PCRO") %>%
  filter(sample_time == "t0")










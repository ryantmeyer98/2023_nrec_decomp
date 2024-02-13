# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
decomp.df <- read_csv("output/cleaned raw data.csv")

# GET COLUMNS ALL SET UP TO BE THE RIGHT KIND OF THING ----
decomp.df <- decomp.df %>%
  mutate(location = as.factor(location),
         crop = as.factor(crop),
         sample_time = as.factor(sample_time),
         tea_initial_drywt_g = as.numeric(tea_initial_drywt_g))

# CALCULATE PERCENT MASS REMAINING ----

# arrange 
decomp.df <- decomp.df %>%
  arrange(location, crop, block, sample_time)

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
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(forage_initial_c_g = first(forage_final_c_g),
         forage_initial_n_g = first(forage_final_n_g)) %>%
  mutate(forage_pct_c_remain = forage_final_c_g / forage_initial_c_g * 100,
         forage_pct_n_remain = forage_final_n_g / forage_initial_n_g * 100) %>%
  ungroup()


test.df <- decomp.df %>%
  filter(sample_time == "t0") %>%
  filter(crop == "AR")


# arrange by site crop and time and find the other values so i can fill in the averages

# tea bags ----
decomp.df <- decomp.df %>%
  # tea percent remaining
  mutate(
    tea_mass_loss = tea_initial_drywt_g - tea_final_drywt_g, 
    tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100) %>%
  # turning the percent into a proportion 
  mutate(
    tea_prop_c = tea_pct_c / 100, 
    tea_prop_n = tea_pct_n / 100) %>%
  # calculate final weight of nutrient per sample
  mutate(
    tea_final_c_g = tea_prop_c * tea_final_drywt_g,
    tea_final_n_g = tea_prop_n * tea_final_drywt_g) %>%
  # calculate the initial weight of nutrient per sample
  mutate(
    tea_initial_c_g = tea_prop_c * tea_initial_drywt_g,
    tea_initial_n_g = tea_prop_n * tea_initial_drywt_g) %>%
  # percent remaining
  mutate(
    tea_pct_c_remain = tea_final_c_g / tea_initial_c_g * 100,
    tea_pct_n_remain = tea_final_n_g / tea_initial_n_g * 100)

# SOME EARLY PLOTTING ----

# bill to look at 
decomp.df %>%
  ggplot(aes(sample_time, tea_pct_remain, color = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  #geom_point(na.rm = TRUE) +
  facet_grid(location~crop)

# nonlinear regression line, bc why not check and see how that looks
decomp.df %>%
  ggplot(aes(sample_time, tea_pct_n_remain, color = crop, group = crop)) +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE) +
  geom_point(na.rm = TRUE) +
  facet_grid(location ~ crop) 


# LOCATIONS TO CHECK DATA ----
# my plan is to now create dataframe the contain just each location and crop i want to check
check.df <- decomp_reduced.df %>%
  filter(location == "ISU") %>%
  filter(crop == "CR")



# PIVOT LONGER ----

# now lets select out the columns we care about
decomp_reduced.df <- decomp.df %>%
  select(location, crop, forage_id, tea_id, block, sample_time, forage_pct_remain, tea_pct_remain,
         forage_pct_c_remain, forage_pct_n_remain, tea_pct_c_remain, tea_pct_n_remain)

# pivot longer
reduced_long.df <- decomp_reduced.df %>%
  pivot_longer(cols = c(forage_pct_remain, tea_pct_remain,forage_pct_c_remain, forage_pct_n_remain,
                        tea_pct_c_remain, tea_pct_n_remain),
               names_to = "variable",
               values_to = "value")

# SAVE OUTPUTS ----
# percent remaining wide
write_csv(decomp_reduced.df, file = "output/percent remaining wide.csv")


# pct remaining long
write_csv(reduced_long.df, file = "output/percent remaining long.csv")

# QUICK PLOTS FOR AG MEETING ----
# full
reduced_long.df %>%
  ggplot(aes(sample_time, value, color = location, shape = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  coord_cartesian(ylim = c(50, 100)) +
  facet_grid(variable ~ crop)


# for closer viewing
decomp.df %>%
  ggplot(aes(sample_time, forage_pct_n_remain, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  coord_cartesian(ylim = (c(50,100))) +
  facet_grid(location~crop)









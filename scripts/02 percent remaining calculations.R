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

# REMOVE THE WEIGHTS OF THE BAGS AND THE STAPLES ----

# only for the forage bags, we could not do bag weights for the tea bags bc they came pre-bagged
decomp.df <- decomp.df %>%
  mutate(forage_initial_drywt_g = forage_initial_drywt_g - (forage_bag_wt_g + staple_weight)) %>%
  mutate(forage_final_drywt_g = forage_final_drywt_g - (forage_bag_wt_g + staple_weight))


# CALCULATE PERCENT MASS REMAINING ----

# first we have to remove places where the sample was lost sample
decomp.df <- decomp.df %>%
  filter(!is.na(sample_time))

bill.df <- decomp.df

decomp.df <- decomp.df %>%
  mutate(forage_pct_remain = (forage_final_drywt_g / forage_initial_drywt_g) * 100) %>%
  mutate(tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100)

# now we need to get the percent remaining for the carbon and the nitrogen ----

# first get everything to a proportion
decomp.df <- decomp.df %>%
  mutate(forage_prop_c = forage_pct_c / 100,
         forage_prop_n = forage_pct_n / 100,
         tea_prop_c = tea_pct_c / 100, 
         tea_prop_n = tea_pct_n / 100)

# forage carbon
forage_initial_prop_c <- decomp.df %>%
  filter(sample_time == "t0") %>%
  group_by(crop) %>%
  summarize(forage_initial_prop_c = mean(forage_prop_c, na.rm = TRUE))
# AR: 0.41900 CR: 0.42525 GPC: 0.44000 PCRO: 0.44900 WPC: 0.43150

# forage nitrogen
forage_initial_prop_n <- decomp.df %>%
  filter(sample_time == "t0") %>%
  group_by(crop) %>%
  summarize(forage_initial_prop_n = mean(forage_prop_n, na.rm = TRUE))
# AR: 0.025275; CR 0.019650; GPC 0.022400; PCRO 0.042500; WPC 0.026650

# tea carbon
tea_initial_prop_c <- decomp.df %>%
  filter(sample_time == "t0") %>%
  group_by(crop) %>%
  summarize(tea_initial_prop_c = mean(tea_prop_c, na.rm = TRUE))
# AR: 0.4902500; CR: 0.4682500; GPC: 0.4793333; PCRO: 0.4218333; WPC: 0.4680000

# tea nitrogen
tea_initial_prop_n <- decomp.df %>%
  filter(sample_time == "t0") %>%
  group_by(crop) %>%
  summarize(tea_initial_prop_n = mean(tea_prop_n, na.rm = TRUE))
# AR: 0.04180000; CR: 0.03485000; GPC:0.04123333; PCRO: 0.03406667 WPC: 0.03970000

# multiply each sample by the initial proportion of carbon to get the initial for every sample

# forage carbon
decomp.df <- decomp.df %>%
  mutate(forage_initial_carbon_g = case_when(
    crop == "AR" ~ forage_initial_drywt_g * 0.41900,
    crop == "CR" ~ forage_initial_drywt_g * 0.42525, 
    crop == "GPC" ~ forage_initial_drywt_g * 0.44000, 
    crop == "PCRO" ~ forage_initial_drywt_g * 0.44900, 
    crop == "WPC" ~ forage_initial_drywt_g * 0.43150
  ))

# forage nitrogen
decomp.df <- decomp.df %>%
  mutate(forage_initial_nitrogen_g = case_when(
    crop == "AR" ~ forage_initial_drywt_g * 0.025275,
    crop == "CR" ~ forage_initial_drywt_g * 0.019650, 
    crop == "GPC" ~ forage_initial_drywt_g * 0.022400, 
    crop == "PCRO" ~ forage_initial_drywt_g * 0.042500, 
    crop == "WPC" ~ forage_initial_drywt_g * 0.026650
  ))
# tea carbon
decomp.df <- decomp.df %>%
  mutate(tea_initial_carbon_g = case_when(
    crop == "AR" ~ tea_initial_drywt_g * 0.4902500,
    crop == "CR" ~ tea_initial_drywt_g * 0.4682500, 
    crop == "GPC" ~ tea_initial_drywt_g * 0.4793333, 
    crop == "PCRO" ~ tea_initial_drywt_g * 0.4218333, 
    crop == "WPC" ~ tea_initial_drywt_g * 0.4680000
  ))

# tea nitrogen
decomp.df <- decomp.df %>%
  mutate(tea_initial_nitrogen_g = case_when(
    crop == "AR" ~ tea_initial_drywt_g * 0.04180000,
    crop == "CR" ~ tea_initial_drywt_g * 0.03485000, 
    crop == "GPC" ~ tea_initial_drywt_g * 0.04123333, 
    crop == "PCRO" ~ tea_initial_drywt_g * 0.03406667, 
    crop == "WPC" ~ tea_initial_drywt_g * 0.03970000
  ))

# now we take the final weight and multiply by the collected proportion to get collected c
decomp.df <- decomp.df %>%
  mutate(forage_final_carbon_g = forage_final_drywt_g * forage_prop_c,
         forage_final_nitrogen_g = forage_final_drywt_g * forage_prop_n,
         tea_final_carbon_g = tea_final_drywt_g * tea_prop_c,
         tea_final_nitrogen_g = tea_final_drywt_g * tea_prop_n)

# calculate the percent remaining
decomp.df <- decomp.df %>%
  mutate(forage_pct_c_remain = (forage_final_carbon_g / forage_initial_carbon_g) * 100) %>%
  mutate(forage_pct_n_remain = (forage_final_nitrogen_g / forage_initial_nitrogen_g) * 100) %>%
  mutate(tea_pct_n_remain = (tea_final_carbon_g / tea_initial_carbon_g) * 100) %>%
  mutate(tea_pct_c_remain = (tea_final_nitrogen_g / tea_initial_nitrogen_g) * 100)

# BILL'S METHOD ----
# forage bags ----
bill.df <- decomp.df %>%
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
    forage_final_n_g = forage_prop_n * forage_final_drywt_g) %>%
# calculate initial weight of nutrient
  mutate(
    forage_initial_c_g = forage_prop_c * forage_initial_drywt_g,
    forage_initial_n_g = forage_prop_n * forage_initial_drywt_g) %>%
# calculate percent remainig 
  mutate(forage_pct_c_remain = (forage_final_c_g / forage_initial_c_g) * 100,
         forage_pct_n_remain = (forage_final_n_g / forage_initial_n_g) * 100)

# tea bags ----
bill.df <- bill.df %>%
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
  mutate(tea_pct_c_remain = tea_final_c_g / tea_initial_c_g * 100,
         tea_pct_n_remain = tea_final_n_g / tea_initial_n_g * 100)

# COMPARISONS ----
# locations to look at 
decomp_reduced.df %>%
  ggplot(aes(sample_time, forage_pct_n_remain, color = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_grid(location~crop)

# bill to look at 
bill.df %>%
  ggplot(aes(sample_time, tea_pct_n_remain, color = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_grid(location~crop)

# now lets select out the columns we care about
decomp_reduced.df <- decomp.df %>%
  select(location, crop, forage_id, tea_id, block, sample_time, forage_pct_remain, tea_pct_remain,
         forage_pct_c_remain, forage_pct_n_remain, tea_pct_c_remain, tea_pct_n_remain)

# pivot longer
reduced_long.df <- decomp_reduced.df %>%
  pivot_longer(cols = c(forage_pct_remain, tea_pct_remain,forage_pct_c_remaining, forage_pct_n_remaining,
                        tea_pct_c_remaining, tea_pct_n_remaining),
               names_to = "variable",
               values_to = "value")

# WANT TO DO SOME PRELIMINARY PLOTTING TO FIND ANY WEIRD DATA ----

# plotting it to see how things look
reduced_long.df %>%
  ggplot(aes(sample_time, value, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "line", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", na.rm = TRUE) +
  facet_grid(location ~ variable)

# locations to look at 
decomp_reduced.df %>%
  ggplot(aes(sample_time, tea_pct_n_remaining, color = crop)) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_grid(location~crop)

# PLACES I NEED TO CHECK THE DATA ----

# forage percent remaining
# ISU CR T6,7; ISU GPC T7; ISU PCRO T7; ISU WPC T5
# WIU AR T5,6,9; WIU PCRO T7,9

# forage carbon remaining 
# ISU AR T1; ISU CR T2; ISU GPC T3; ISU WPC T2,5
# WIU AR T9; WIU GPC T5; WIU WPC T6

# SAVE OUTPUTS ----
# percent remaining wide
write_csv(decomp_reduced.df, file = "output/percent remaining wide.csv")


# pct remaining long
write_csv(reduced_long.df, file = "output/percent remaining long.csv")





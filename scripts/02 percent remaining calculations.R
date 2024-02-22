# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
decomp.df <- read_csv("output/cleaned raw data.csv") 

# remove na values from sample time
decomp.df <- decomp.df %>%
  filter(!is.na(days))

# CALCULATE PERCENT MASS REMAINING ----

# forage bags ----

# forage bag percent remaining and round the forage_pct_remain to 2 decimals
decomp.df <- decomp.df %>%
  mutate(
    forage_mass_loss = forage_initial_drywt_g - forage_final_drywt_g,
    forage_pct_remain = round(((forage_final_drywt_g / forage_initial_drywt_g) * 100),3))


# tea bag percent remaining
decomp.df <- decomp.df %>%
  mutate(
    tea_mass_loss = tea_initial_drywt_g - tea_final_drywt_g,
    tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100, na.rm = TRUE)


# forage proportion of nutrient remaining 
decomp.df <- decomp.df %>%
  mutate(
    forage_collected_prop_c = round(forage_pct_c / 100, 3),
    forage_collected_prop_n = round(forage_pct_n / 100, 3))

# tea proportion of nutrient remaining
decomp.df <- decomp.df %>%
  mutate(
    tea_collected_prop_c = round(tea_pct_c / 100, 3),
    tea_collected_prop_n = round(tea_pct_n / 100, 3))

# DATA CHECK TO MAKE SURE THE VALUES FILL DOWN CORRECTLY ----
forage_t0_carbon.df <- decomp.df %>%
  group_by(location, crop) %>%
  filter(days == "0") %>%
  summarize(forage_t0_prop_c = mean(forage_collected_prop_c, na.rm = TRUE)) %>%
  ungroup()

forage_t0_nitrogen.df <- decomp.df %>%
  group_by(location, crop) %>%
  filter(days == "0") %>%
  summarize(forage_t0_prop_n = mean(forage_collected_prop_n, na.rm = TRUE)) %>%
  ungroup()

tea_t0_carbon.df <- decomp.df %>%
  group_by(location, crop) %>%
  filter(days == "0") %>%
  summarize(tea_t0_prop_c = mean(tea_collected_prop_c, na.rm = TRUE)) %>%
  ungroup()

tea_t0_nitrogen.df <- decomp.df %>%
  group_by(location, crop) %>%
  filter(days == "0") %>%
  summarize(tea_t0_prop_n = mean(tea_collected_prop_n, na.rm = TRUE)) %>%
  ungroup()

# trying to get everything saved in a dataframe, kinda working but don't have time to play with it right now,
# just need to get this done, can play with it after defense

# # save the max value in the dataframe

# decomp.df <- decomp.df %>%
#   group_by(location, crop) %>%
#   mutate(forage_t0_prop_c = max(forage_collected_prop_c, na.rm = TRUE)) %>%
#   fill(forage_t0_prop_c, .direction = "down") %>%
#   mutate(forage_t0_prop_n = max(forage_collected_prop_n, na.rm = TRUE)) %>%
#   fill(forage_t0_prop_n, .direction = "down") %>%
#   mutate(tea_t0_prop_c = max(tea_collected_prop_c, na.rm = TRUE)) %>%
#   fill(tea_t0_prop_c, .direction = "down") %>%
#   mutate(tea_t0_prop_n = max(tea_collected_prop_n, na.rm = TRUE)) %>%
#   fill(tea_t0_prop_n, .direction = "down") %>%
#   ungroup()

# calculate grams of c in t0 samples
decomp.df <- decomp.df %>%
  mutate(forage_t0_c_g = case_when(
    crop == "AR" & location == "ISU" ~ forage_initial_drywt_g * 0.42125,
    crop == "CR" & location == "ISU" ~ forage_initial_drywt_g * 0.43025,
    crop == "GPC" & location == "ISU" ~ forage_initial_drywt_g * 0.44275,
    crop == "PCRO" & location == "ISU" ~ forage_initial_drywt_g * 0.44900,
    crop == "WPC" & location == "ISU" ~ forage_initial_drywt_g * 0.43175,
    crop == "AR" & location == "WIU" ~ forage_initial_drywt_g * 0.41750,
    crop == "CR" & location == "WIU" ~ forage_initial_drywt_g * 0.42000,
    crop == "GPC" & location == "WIU" ~ forage_initial_drywt_g * 0.43725,
    crop == "PCRO" & location == "WIU" ~ forage_initial_drywt_g * 0.44900,
    crop == "WPC" & location == "WIU" ~ forage_initial_drywt_g * 0.43200
  ))

# calculate grams of n in t0 samples
decomp.df <- decomp.df %>%
  mutate(forage_t0_n_g = case_when(
    crop == "AR" & location == "ISU" ~ forage_initial_drywt_g * 0.02750,
    crop == "CR" & location == "ISU" ~ forage_initial_drywt_g * 0.02000,
    crop == "GPC" & location == "ISU" ~ forage_initial_drywt_g * 0.02150,
    crop == "PCRO" & location == "ISU" ~ forage_initial_drywt_g *0.04225,
    crop == "WPC" & location == "ISU" ~ forage_initial_drywt_g * 0.02675,
    crop == "AR" & location == "WIU" ~ forage_initial_drywt_g * 0.02525,
    crop == "CR" & location == "WIU" ~ forage_initial_drywt_g * 0.01950,
    crop == "GPC" & location == "WIU" ~ forage_initial_drywt_g * 0.02250,
    crop == "PCRO" & location == "WIU" ~ forage_initial_drywt_g *0.04300,
    crop == "WPC" & location == "WIU" ~ forage_initial_drywt_g * 0.02700
  ))

# calculate grams of c and n when collected
decomp.df <- decomp.df %>%
  mutate(forage_collected_c_g = forage_final_drywt_g * forage_collected_prop_c,
         forage_collected_n_g = forage_final_drywt_g * forage_collected_prop_n)

# determine percent remaining
decomp.df <- decomp.df %>%
  mutate(forage_pct_c_remain = forage_collected_c_g / forage_t0_c_g * 100,
         forage_pct_n_remain = forage_collected_n_g / forage_t0_n_g * 100)

# tea bags ----

# calculate grams of c in t0 samples
decomp.df <- decomp.df %>%
  mutate(tea_t0_c_g = case_when(
    crop == "AR" & location == "ISU" ~ tea_initial_drywt_g * 0.48900,
    crop == "CR" & location == "ISU" ~ tea_initial_drywt_g * 0.48150,
    crop == "GPC" & location == "ISU" ~ tea_initial_drywt_g * 0.48150,
    crop == "PCRO" & location == "ISU" ~ tea_initial_drywt_g *0.47300,
    crop == "WPC" & location == "ISU" ~ tea_initial_drywt_g * 0.48450,
    crop == "AR" & location == "WIU" ~ tea_initial_drywt_g * 0.49125,
    crop == "CR" & location == "WIU" ~ tea_initial_drywt_g * 0.45475,
    crop == "GPC" & location == "WIU" ~ tea_initial_drywt_g * 0.47675,
    crop == "PCRO" & location == "WIU" ~ tea_initial_drywt_g *0.44600,
    crop == "WPC" & location == "WIU" ~ tea_initial_drywt_g * 0.45150
  ))

# calculate grams of n in t0 samples
decomp.df <- decomp.df %>%
  mutate(tea_t0_n_g = case_when(
    crop == "AR" & location == "ISU" ~ tea_initial_drywt_g * 0.04275,
    crop == "CR" & location == "ISU" ~ tea_initial_drywt_g * 0.03725,
    crop == "GPC" & location == "ISU" ~ tea_initial_drywt_g * 0.04000,
    crop == "PCRO" & location == "ISU" ~ tea_initial_drywt_g *0.03600,
    crop == "WPC" & location == "ISU" ~ tea_initial_drywt_g * 0.04175,
    crop == "AR" & location == "WIU" ~ tea_initial_drywt_g *0.04100,
    crop == "CR" & location == "WIU" ~ tea_initial_drywt_g * 0.03275,
    crop == "GPC" & location == "WIU" ~ tea_initial_drywt_g * 0.04225,
    crop == "PCRO" & location == "WIU" ~ tea_initial_drywt_g *0.03300,
    crop == "WPC" & location == "WIU" ~ tea_initial_drywt_g * 0.03800
  ))

# calculate grams of c and n in when collected
decomp.df <- decomp.df %>%
  mutate(tea_collected_c_g = tea_final_drywt_g * tea_collected_prop_c,
         tea_collected_n_g = tea_final_drywt_g * tea_collected_prop_n)

# determine percent remaining
decomp.df <- decomp.df %>%
  mutate(tea_pct_c_remain = tea_collected_c_g / tea_t0_c_g * 100,
         tea_pct_n_remain = tea_collected_n_g / tea_t0_n_g * 100)

# REMOVE COLUMNS WE WILL NOT USE IN THE FUTURE ----

decomp.df <- decomp.df %>%
  select(location, crop, block, days,
         forage_initial_drywt_g, forage_final_drywt_g, forage_pct_n, forage_pct_c, forage_t0_c_g,
         forage_t0_n_g, forage_collected_c_g, forage_collected_n_g,
         forage_pct_remain, forage_pct_c_remain, forage_pct_n_remain,
         tea_initial_drywt_g, tea_final_drywt_g, tea_pct_n, tea_pct_c, tea_t0_c_g, tea_t0_n_g,
         tea_collected_c_g, tea_collected_n_g,
         tea_pct_remain, tea_pct_c_remain, tea_pct_n_remain)


# SAVE THE OUTPUT TO A CSV ----
write_csv(decomp.df, file = "output/pct remaining data.csv")

# decomp.df <- decomp.df %>%
#   select(location, crop, block, days, forage_pct_remain, forage_pct_c_remain, forage_pct_n_remain,
#          tea_pct_remain, tea_pct_c_remain, tea_pct_n_remain)

# # PIVOT LONGER ----
# decomp_long.df <- decomp.df %>%
#   pivot_longer(cols = c(forage_pct_remain, forage_pct_c_remain, forage_pct_n_remain, 
#                         tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain),
#                names_to = "variable",
#                values_to = "value")
# 
# decomp_long.df %>%
#   ggplot(aes(days, value, color = crop)) +
#   # stat_smooth(method = "glm", formula = y ~ exp(-x), se = FALSE) +
#   #stat_smooth(se = FALSE) +
#   stat_summary(fun = mean, geom = "point", size = 3, alpha = 0.5) +
#   # stat_summary(fun = mean, geom = "line") +
#   facet_grid(variable~location) +
#   coord_cartesian(ylim = c(50,100))


# SOME EARLY PLOTTING ----

# # bill to look at 
# decomp.df %>%
#   ggplot(aes(days, tea_pct_n_remain, color = crop, group = days)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
#   geom_point() +
#   facet_grid(location~crop)
# 
# # LOOKING FOR OUTLIERS ----
# outlier.df <- decomp.df %>%
#   filter(location == "WIU") %>%
#   filter(crop == "PCRO") 
# 
# test.df <- decomp.df %>%
#   filter(location == "ISU") %>%
#   filter(crop == "GPC") %>%
#   filter(block == "1") %>%
#   select(location, crop, block, days,
#          forage_initial_drywt_g, forage_final_drywt_g, forage_pct_n, forage_prop_n, forage_initial_n_g,
#          forage_final_n_g, forage_pct_n_remain)
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ### ADDED CODE # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # this code will help to look for outliers
# library(plotly)
# 
# # note you cant see anything with this data as all the values are gone
# # going to move to a new code script to look at the data
# 
# # make block a factor
# decomp.df <- decomp.df %>%
#   mutate(block = as.factor(block))
# 
# 
# outliers.plot <- decomp.df %>% 
#   filter(location == "ISU", crop == "AR") %>% 
#   ggplot(aes(days, tea_pct_n_remain, color = block)) +
#   geom_point()
# 
# 
#   
# ggplotly(outliers.plot)  
# 
# 
# 



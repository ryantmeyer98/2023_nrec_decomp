# LIBRARIES ----
library(tidyverse)

# READ IN THE DATA ----
decomp.df <- read_csv("output/cleaned raw data.csv") 

# GET COLUMNS ALL SET UP TO BE THE RIGHT KIND OF THING ----
decomp.df <- decomp.df %>%
  mutate(location = as.factor(location),
         crop = as.factor(crop)
        # FIX THIS IN THE EXCEL FILE!!!!  tea_initial_drywt_g = as.numeric(tea_initial_drywt_g)
         ) %>% 
  select(
         tea_initial_drywt_g, tea_final_drywt_g, tea_pct_c, tea_pct_n, location, crop, block, days, 
         forage_initial_drywt_g, forage_final_drywt_g,  forage_pct_c, forage_pct_n) %>%
  arrange(location, crop,days, block)

# CALCULATE PERCENT MASS REMAINING ----

# forage bags ----

# forage bag percent remaining and round the forage_pct_remain to 2 decimals
decomp.df <- decomp.df %>%
  mutate(
    forage_mass_loss = forage_initial_drywt_g - forage_final_drywt_g,
    forage_pct_remain = round(((forage_final_drywt_g / forage_initial_drywt_g) * 100),3))


# proportion of nutrient remaining 
decomp.df <- decomp.df %>%
  mutate(
    forage_collected_prop_c = round(forage_pct_c / 100, 3),
    forage_collected_prop_n = round(forage_pct_n / 100, 3))

# pull out t0 carbon and nitrogen
decomp.df %>%
  group_by(location, crop) %>%
  summarize(forage_t0_prop_n = mean(forage_collected_prop_n, na.rm = TRUE)) %>%
  ungroup()

# NOTE YOU SHOULD REALLY SAVE THE ABOVE TO A DF AND DO A JOIN!!!!! DOING THIS BY HAND LEADS TO ISSUES


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# WHERE DID THE NUMBERS IN BLUE COME FROM !!!11 ????
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# calculate grams of c in t0 samples
decomp.df <- decomp.df %>%
  mutate(forage_t0_c_g = case_when(
    crop == "AR" & location == "ISU" ~ forage_initial_drywt_g * 0.398,
    crop == "CR" & location == "ISU" ~ forage_initial_drywt_g * 0.418,
    crop == "GPC" & location == "ISU" ~ forage_initial_drywt_g * 0.421,
    crop == "PCRO" & location == "ISU" ~ forage_initial_drywt_g * 0.426,
    crop == "WPC" & location == "ISU" ~ forage_initial_drywt_g * 0.401,
    crop == "AR" & location == "WIU" ~ forage_initial_drywt_g * 0.381,
    crop == "CR" & location == "WIU" ~ forage_initial_drywt_g * 0.404,
    crop == "GPC" & location == "WIU" ~ forage_initial_drywt_g * 0.382,
    crop == "PCRO" & location == "WIU" ~ forage_initial_drywt_g * 0.395,
    crop == "WPC" & location == "WIU" ~ forage_initial_drywt_g * 0.401
  ))

# calculate grams of n in t0 samples
decomp.df <- decomp.df %>%
  mutate(forage_t0_n_g = case_when(
    crop == "AR" & location == "ISU" ~ forage_initial_drywt_g * 0.0270,
    crop == "CR" & location == "ISU" ~ forage_initial_drywt_g * 0.0218,
    crop == "GPC" & location == "ISU" ~ forage_initial_drywt_g * 0.0240,
    crop == "PCRO" & location == "ISU" ~ forage_initial_drywt_g *0.0419,
    crop == "WPC" & location == "ISU" ~ forage_initial_drywt_g * 0.0284,
    crop == "AR" & location == "WIU" ~ forage_initial_drywt_g * 0.0270,
    crop == "CR" & location == "WIU" ~ forage_initial_drywt_g * 0.0206,
    crop == "GPC" & location == "WIU" ~ forage_initial_drywt_g * 0.0250,
    crop == "PCRO" & location == "WIU" ~ forage_initial_drywt_g *0.0344,
    crop == "WPC" & location == "WIU" ~ forage_initial_drywt_g * 0.0267
  ))

# calculate grams of c and n in when collected
decomp.df <- decomp.df %>%
  mutate(forage_collected_c_g = forage_final_drywt_g * forage_collected_prop_c,
         forage_collected_n_g = forage_final_drywt_g * forage_collected_prop_n)

# determine percent remaining
decomp.df <- decomp.df %>%
  mutate(forage_pct_c_remain = forage_collected_c_g / forage_t0_c_g * 100,
         forage_pct_n_remain = forage_collected_n_g / forage_t0_n_g * 100)

# tea bags ----

# tea bag percent remaining
decomp.df <- decomp.df %>%
  mutate(
    tea_mass_loss = tea_initial_drywt_g - tea_final_drywt_g,
    tea_pct_remain = (tea_final_drywt_g / tea_initial_drywt_g) * 100, na.rm = TRUE)

# proportion of nutrient remaining 
decomp.df <- decomp.df %>%
  mutate(
    tea_collected_prop_c = tea_pct_c / 100,
    tea_collected_prop_n = tea_pct_n / 100)

# pull out t0 carbon and nitrogen
decomp.df %>%
  group_by(location, crop) %>%
  summarize(tea_t0_prop_c = mean(tea_collected_prop_c, na.rm = TRUE)) %>%
  ungroup()

# calculate grams of c in t0 samples
decomp.df <- decomp.df %>%
  mutate(tea_t0_c_g = case_when(
    crop == "AR" & location == "ISU" ~ tea_initial_drywt_g * 0.437,
    crop == "CR" & location == "ISU" ~ tea_initial_drywt_g * 0.454,
    crop == "GPC" & location == "ISU" ~ tea_initial_drywt_g * 0.447,
    crop == "PCRO" & location == "ISU" ~ tea_initial_drywt_g *0.422,
    crop == "WPC" & location == "ISU" ~ tea_initial_drywt_g * 0.392,
    crop == "AR" & location == "WIU" ~ tea_initial_drywt_g * 0.382,
    crop == "CR" & location == "WIU" ~ tea_initial_drywt_g * 0.437,
    crop == "GPC" & location == "WIU" ~ tea_initial_drywt_g * 0.394,
    crop == "PCRO" & location == "WIU" ~ tea_initial_drywt_g *0.407,
    crop == "WPC" & location == "WIU" ~ tea_initial_drywt_g * 0.376
  ))

# calculate grams of n in t0 samples
decomp.df <- decomp.df %>%
  mutate(tea_t0_n_g = case_when(
    crop == "AR" & location == "ISU" ~ tea_initial_drywt_g * 0.0413,
    crop == "CR" & location == "ISU" ~ tea_initial_drywt_g * 0.0416,
    crop == "GPC" & location == "ISU" ~ tea_initial_drywt_g * 0.0404,
    crop == "PCRO" & location == "ISU" ~ tea_initial_drywt_g *0.0395,
    crop == "WPC" & location == "ISU" ~ tea_initial_drywt_g * 0.0366,
    crop == "AR" & location == "WIU" ~ tea_initial_drywt_g *0.0342,
    crop == "CR" & location == "WIU" ~ tea_initial_drywt_g * 0.0395,
    crop == "GPC" & location == "WIU" ~ tea_initial_drywt_g * 0.0360,
    crop == "PCRO" & location == "WIU" ~ tea_initial_drywt_g *0.0366,
    crop == "WPC" & location == "WIU" ~ tea_initial_drywt_g * 0.0339
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
  select(location, crop, block, days, forage_pct_remain, forage_pct_c_remain,
         forage_pct_n_remain,
         tea_pct_remain, tea_pct_c_remain, tea_pct_n_remain)

# PIVOT LONGER ----
decomp_long.df <- decomp.df %>%
  pivot_longer(cols = c(forage_pct_remain, forage_pct_c_remain, forage_pct_n_remain, 
                        tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain),
               names_to = "variable",
               values_to = "value")

decomp_long.df %>%
  ggplot(aes(days, value, color = crop)) +
  # stat_smooth(method = "glm", formula = y ~ exp(-x), se = FALSE) +
  #stat_smooth(se = FALSE) +
  stat_summary(fun = mean, geom = "point", size = 3, alpha = 0.5) +
  # stat_summary(fun = mean, geom = "line") +
  facet_grid(variable~location) +
  coord_cartesian(ylim = c(50,100))

# SAVE THE OUTPUT TO A CSV ----
write_csv(decomp.df, file = "output/pct remaining data.csv")

# SOME EARLY PLOTTING ----

# bill to look at 
decomp.df %>%
  ggplot(aes(days, tea_pct_n_remain, color = crop, group = days)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  geom_point() +
  facet_grid(location~crop)

# LOOKING FOR OUTLIERS ----
outlier.df <- decomp.df %>%
  filter(location == "WIU") %>%
  filter(crop == "PCRO") 

test.df <- decomp.df %>%
  filter(location == "ISU") %>%
  filter(crop == "GPC") %>%
  filter(block == "1") %>%
  select(location, crop, block, days,
         forage_initial_drywt_g, forage_final_drywt_g, forage_pct_n, forage_prop_n, forage_initial_n_g,
         forage_final_n_g, forage_pct_n_remain)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
### ADDED CODE # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# this code will help to look for outliers
library(plotly)

# note you cant see anything with this data as all the values are gone
# going to move to a new code script to look at the data

# make block a factor
decomp.df <- decomp.df %>%
  mutate(block = as.factor(block))


outliers.plot <- decomp.df %>% 
  filter(location == "ISU", crop == "AR") %>% 
  ggplot(aes(days, tea_pct_n_remain, color = block)) +
  geom_point()


  
ggplotly(outliers.plot)  






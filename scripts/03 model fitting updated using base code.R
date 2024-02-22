# LIBRARIES ----
library(tidyverse)
library(broom)
library(patchwork)
library(minpack.lm)

# READ IN THE DATA ----
decomp.df <- read_csv("output/pct remaining data.csv") %>% 
  arrange(location, crop, block, days)

# interpolate data from the prior value to the next value to fill missing na values using 
# the na.approx function from the zoo package
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(across(c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
                  tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain), 
                ~ zoo::na.approx(., na.rm = FALSE))) %>%
  ungroup()


# FIT THE MODELS ----

# forage
# do a negative exponential regression using SSasymp funciton to get all parameters of the mode for the 
# f.df dataframe
# Asym is the asymptote, R0 is the y-intercept, and lrc is the rate of change
# the SSasymp function is a self-starting model that uses the asymptote, y-intercept, 
# and rate of change to fit the model


# FORAGE BIOMASS ----

# need to nest the data by location, crop, and block to fit the model to each group
forage_biomass_stats.df <- decomp.df %>%
   filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(forage_pct_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.006), 
                         data = .x)))

# pull out fitted values for analysis
forage_biomass_summary.df <- forage_biomass_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)
  
# arrange to be able to read easier by crop, block site
forage_biomass_summary.df <- forage_biomass_summary.df %>% 
  arrange(crop, block, location)

# only the k values
forage_biomass_k_summary.df <- forage_biomass_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(forage_biomass_k_summary.df, "output/forage biomass k values.csv")

# FORAGE CARBON ----

# need to nest the data by location, crop, and block to fit the model to each group
forage_carbon_stats.df <- decomp.df %>%
  filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  filter(!(crop == "CR" & block == 3)) %>% # WE LOST CR BLOCK 3 BAG EXCEPT 1 ONE DATE WHICH FAILED THE MODEL
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(forage_pct_c_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k= 0.006), 
                         na.action = na.exclude,
                         data = .x)))

# pull out fitted values for analysis
forage_carbon_summary.df <- forage_carbon_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# arrange to be able to read easier by crop, block site
forage_carbon_summary.df <- forage_carbon_summary.df %>% 
  arrange(crop, block, location)

# only the k values
forage_carbon_k_summary.df <- forage_carbon_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(forage_carbon_k_summary.df, "output/forage carbon k values.csv")

# FORAGE NITROGEN ----
forage_nitrogen_stats.df <- decomp.df %>%
  filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  filter(!(crop == "CR" & block == 3)) %>% # WE LOST CR BLOCK 3 BAG EXCEPT 1 ONE DATE WHICH FAILED THE MODEL
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(forage_pct_n_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k= 0.006), 
                         na.action = na.exclude,
                         data = .x)))

# pull out fitted values for analysis
forage_nitrogen_summary.df <- forage_nitrogen_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# arrange to be able to read easier by crop, block site
forage_nitrogen_summary.df <- forage_nitrogen_summary.df %>% 
  arrange(crop, block, location)

# only the k values
forage_nitrogen_k_summary.df <- forage_nitrogen_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(forage_nitrogen_k_summary.df, "output/forage nitrogen k values.csv")

# tea biomass ----
tea_biomass_stats.df <- decomp.df %>%
  filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  filter(!(crop == "CR" & block == 3)) %>% # WE LOST CR BLOCK 3 BAG EXCEPT 1 ONE DATE WHICH FAILED THE MODEL
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(tea_pct_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k= 0.006), 
                         na.action = na.exclude,
                         data = .x)))

# pull out fitted values for analysis
tea_biomass_summary.df <- tea_biomass_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# arrange to be able to read easier by crop, block site
tea_biomass_summary.df <- tea_biomass_summary.df %>% 
  arrange(crop, block, location)

# only the k values
tea_biomass_k_summary.df <- tea_biomass_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(tea_biomass_k_summary.df, "output/tea biomass k values.csv")


# tea carbon -----
tea_carbon_stats.df <- decomp.df %>%
  filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  filter(!(crop == "CR" & block == 3)) %>% # WE LOST CR BLOCK 3 BAG EXCEPT 1 ONE DATE WHICH FAILED THE MODEL
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(tea_pct_c_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k= 0.006), 
                         na.action = na.exclude,
                         data = .x)))

# pull out fitted values for analysis
tea_carbon_summary.df <- tea_carbon_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# arrange to be able to read easier by crop, block site
tea_carbon_summary.df <- tea_carbon_summary.df %>% 
  arrange(crop, block, location)

# only the k values
tea_carbon_k_summary.df <- tea_carbon_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(tea_carbon_k_summary.df, "output/tea carbon k values.csv")


# tea nitrogen ----
tea_nitrogen_stats.df <- decomp.df %>%
  filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  filter(!(crop == "CR" & block == 3)) %>% # WE LOST CR BLOCK 3 BAG EXCEPT 1 ONE DATE WHICH FAILED THE MODEL
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(tea_pct_n_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k= 0.006), 
                         na.action = na.exclude,
                         data = .x)))

# pull out fitted values for analysis
tea_nitrogen_summary.df <- tea_nitrogen_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)

# arrange to be able to read easier by crop, block site
tea_nitrogen_summary.df <- tea_nitrogen_summary.df %>% 
  arrange(crop, block, location)

# only the k values
tea_nitrogen_k_summary.df <- tea_nitrogen_summary.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 

write_csv(tea_nitrogen_k_summary.df, "output/tea nitrogen k values.csv")


# PLOTTING ----
# plot the values ----
biomass.plot <- k_summary.df %>% 
  ggplot(aes(crop, estimate, color=crop))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic() +
  facet_grid(.~location)

biomass.plot

# Now to see the regressions plots
# plot the negative exponential ----
decomp.df %>%
  filter(location == "ISU") %>%
  filter(crop %in% c("CR")) %>%
  # filter(forage_pct_remain > 0) %>% 
  # filter(block == 3) %>%
  ggplot(mapping = aes(x=days, y=forage_pct_remain, color=as.factor(block))) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 45, 5)) +
  geom_smooth( aes(x = days, y = forage_pct_remain, color =as.factor(block)),
               method = "nls", formula = y ~ 100 * exp(-k*x),
               method.args = list(start = c(k=0.001)), se = FALSE)




# plot the negative exponential ----
decomp.df %>%
  filter(location == "ISU") %>%
  ggplot(mapping = aes(x=days, y=forage_pct_c_remain, color=as.factor(crop))) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 45, 5)) +
  geom_smooth(aes(x = days, y = forage_pct_c_remain, color =as.factor(crop)),
               method = "nls", formula = y ~ 100 * exp(-k*x),
               method.args = list(start = c(k=0.01)), se = FALSE)







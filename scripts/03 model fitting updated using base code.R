# LIBRARIES ----
library(tidyverse)
library(broom)
library(patchwork)

# READ IN THE DATA ----
decomp.df <- read_csv("output/pct remaining data.csv") %>% 
  arrange(location, crop, block, days)

# # REMOVE NA VALUES ----
# decomp.df <- decomp.df %>%
#   group_by(location, crop, block) %>%
#   mutate(across(c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
#                   tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain), 
#                 ~ replace_na(., mean(., na.rm = TRUE)))) %>%
#   ungroup()

# interpolate data from the prior value to the next value to fill missing na values using the na.approx function from the zoo package
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(across(c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
                  tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain), 
                ~ zoo::na.approx(., na.rm = FALSE))) %>%
  ungroup()


# FIT THE MODELS ----

# forage
# do a negative exponential regression using SSasymp funciton to get all parameters of the mode for the f.df dataframe
# Asym is the asymptote, R0 is the y-intercept, and lrc is the rate of change
# the SSasymp function is a self-starting model that uses the asymptote, y-intercept, and rate of change to fit the model


# need to nest the data by location, crop, and block to fit the model to each group
nested_stats.df <- decomp.df %>%
   filter(crop %in% c("GPC", "WPC", "AR", "CR", "PCRO")) %>%
  nest(data = -c(location, crop, block)) %>% 
  mutate(
    fit = map(data, ~nls(forage_pct_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.006), 
                         data = .x)))

# pull out fitted values for analysis
summary_stats.df <- nested_stats.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)
  
# arrange to be able to read easier by crop, block site
summary_stats.df <- summary_stats.df %>% 
  arrange(crop, block, location)

# only the k values
k_summary.df <- summary_stats.df %>% 
  select (crop, block, location, term, estimate, std.error, statistic, p.value) 


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
  filter(crop %in% c("AR")) %>%
  ggplot(mapping = aes(x=days, y=forage_pct_remain, color=as.factor(block))) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 45, 5)) +
  geom_smooth( aes(x = days, y = forage_pct_remain, as.factor(block)),
               method = "nls", formula = y ~ 100 * exp(-k*x),
               method.args = list(start = c(k=0.001)), se = FALSE)


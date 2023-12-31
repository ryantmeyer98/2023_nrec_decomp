# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)


# read in final finals
tea.df <- read_csv("output/tea_final_drywts.csv") %>% clean_names()



# DATA MODIFICATIONS ----

# getting percent loss for the forage bags
clean.df <- clean.df %>%
  mutate(difference_g = initial_wt_g - final_wt_g) %>%
  mutate(pct_loss = (difference_g / initial_wt_g) * 100)

# getting percent loss for the tea bags

clean.df <- clean.df %>%
  mutate(tea_wt_initial_g = as.numeric(tea_wt_initial_g))

clean.df <- clean.df %>%
  mutate(tea_difference_g = tea_wt_initial_g - tea_wt_final_g) %>%
  mutate(tea_pct_loss = (tea_difference_g / tea_wt_initial_g) * 100)

# now to get the percent mass remaining
clean.df <- clean.df %>%
  mutate(forage_pct_mass_remaining = ((final_wt_g / initial_wt_g) * 100)) %>%
  mutate(tea_pct_mass_remaining = ((tea_wt_final_g / tea_wt_initial_g) * 100))

# for initial plotting purposes i need to get rid of the na values ----
clean.df <- clean.df %>%
  na.omit()

# PRELIMINARY PLOTTING ----

# forage bags
a <- clean.df %>%
  filter(location == "ISU") %>%
  ggplot(aes(time, forage_pct_mass_remaining, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge2(width = 0.05)) +
  stat_summary(fun.data = mean_se, geom = "line") +
  labs(x = "Time", y = "Average Percent Biomass Remaining in Forage Bags at ISU") +
  theme_classic()

a

# tea bags
b <- clean.df %>%
  filter(location == "ISU") %>%
  ggplot(aes(time,tea_pct_mass_remaining, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge2(width = 0.05)) +
  stat_summary(fun.data = mean_se, geom = "line") +
  labs(x = "Time", y = "Average Percent Biomass Remaining in Tea Bags at ISU") +
  theme_classic()

b

# wiu forage bags
c <- clean.df %>%
  filter(location == "WIU") %>%
  ggplot(aes(time, forage_pct_mass_remaining, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge2(width = 0.05)) +
  stat_summary(fun.data = mean_se, geom = "line") +
  #stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(x = "Time", y = "Average Percent Biomass Remaining in Forage Bags at WIU") +
  theme_classic()

c

# wiu tea bags
d <- clean.df %>%
  filter(location == "WIU") %>%
  ggplot(aes(time,tea_pct_mass_remaining, color = crop, group = crop)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge2(width = 0.05)) +
  stat_summary(fun.data = mean_se, geom = "line") +
  labs(x = "Time", y = "Average Percent Biomass Remaining in Tea Bags at WIU") +
  theme_classic()

d

e <- a + b + c + d + plot_layout(guide = "collect")

e


# some mean+/- se plots
clean.df %>%
  filter(location == "ISU") %>%
  filter(time == "t9") %>%
  ggplot(aes(crop, forage_pct_mass_remaining, color = crop)) +
  stat_summary(fun = mean, geom = "point", size = 5)

clean.df %>%
  filter(location == "WIU") %>%
  filter(time == "t9") %>%
  ggplot(aes(crop, forage_pct_mass_remaining, color = crop)) +
  stat_summary(fun = mean, geom = "point", size = 5)

clean.df %>%
  filter(location == "ISU") %>%
  filter(time == "t9") %>%
  ggplot(aes(crop, tea_pct_mass_remaining, color = crop)) +
  stat_summary(fun = mean, geom = "point", size = 5)

clean.df %>%
  filter(location == "WIU") %>%
  filter(time == "t9") %>%
  ggplot(aes(crop, tea_pct_mass_remaining, color = crop)) +
  stat_summary(fun = mean, geom = "point", size = 5)

test.df <- clean.df %>%
  filter(time == "t9" | time == "t8") %>% filter(crop == "PCRO") %>% filter(location == "ISU")

write_csv(clean.df, file = "output/cleaned biomass data.csv")








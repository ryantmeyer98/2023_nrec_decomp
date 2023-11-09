# LIBRARIES ----
library(tidyverse)
library(readxl)
library(patchwork)

# READ IN THE DATA----

# decomp data
raw.df <- read_excel("data/Meyer_nrec2023decompbagdata.xlsx")

# initial tea weights
tea.df <- read_excel("data/tea wt.xlsx")

# CLEAN DATA ----

# cleaning the tea data ----

# taking only the tea columns that i want
tea.df <- tea.df %>%
  select(c(tea_id, initial_dry_wt_g))

# rename the tea final wt column
tea.df <- tea.df %>%
  rename(tea_wt_initial_g = initial_dry_wt_g)

# cleaning up the decomp data ----

# change column names
clean.df <- raw.df %>%
  rename(initial_wt_g = bag_no_staple) %>%
  rename(final_wt_g = dry_wt_g) %>%
  rename(time = sample_time) %>%
  rename(tea_wt_final_g = tea_final)

# select the columns that I want to use
clean.df <- clean.df %>%
  select(c(location, plot, crop, bag_no, block, initial_wt_g, time, final_wt_g,
           tea_id, tea_wt_final_g))

# join ----

# join the tea weights with the rest of the data
clean.df <- left_join(clean.df, tea.df, by = "tea_id")

# there are a few out of place samples that i cant quite figure out what the problem is by
# looking through the physical samples, will need to drop those

clean.df <- clean.df %>%
  filter(bag_no != 217) %>% filter(bag_no != 342) %>% filter(bag_no != 222)

# i want to try something new and keep the environment a little cleaner, not sure if this is a good 
# idea but i want to try dropping the no longer used data frames here so the environment is easier 
# to look through 
rm(raw.df)
rm(tea.df)

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







# LIBRARIES ----
library(tidyverse)
library(broom)
library(patchwork)

# READ IN THE DATA ----
full.df <- read_csv("output/pct remaining data.csv")

# CREATE DATA FRAMES FOR EACH VARIABLE ----

# forage biomass
f.df <- full.df %>%
  select(location, crop, block, days, forage_pct_remain) %>%
  na.omit()

# forage n
f_n.df <- full.df %>%
  select(location, crop, block, days, forage_pct_n_remain) %>%
  na.omit()

# forage c
f_c.df <- full.df %>%
  select(location, crop, block, days, forage_pct_c_remain) %>%
  na.omit()

# tea biomass
t.df <- full.df %>%
  select(location, crop, block, days, tea_pct_remain) %>%
  na.omit()

# tea n
t_n.df <- full.df %>%
  select(location, crop, block, days, tea_pct_n_remain) %>%
  na.omit()

# tea c 
t_c.df <-full.df %>%
  select(location, crop, block, days, tea_pct_c_remain) %>%
  na.omit()

# FIT THE MODELS ----

# forage
f_fit <- nls(forage_pct_remain ~ SSasymp(days, Asym, R0, lrc), data = f.df)

# forage n
fn_fit <- nls(forage_pct_n_remain ~ SSasymp(days, Asym, R0, lrc), data = f_n.df)

# forage c
fc_fit <- nls(forage_pct_c_remain ~ SSasymp(days, Asym, R0, lrc), data = f_c.df)

# tea
t_fit <- nls(tea_pct_remain ~ SSasymp(days, Asym, R0, lrc), data = t.df)

# tea n
tn_fit <- nls(tea_pct_n_remain ~ SSasymp(days, Asym, R0, lrc), data = t_n.df)

# tea c 
tc_fit <- nls(tea_pct_c_remain ~ SSasymp(days, Asym, R0, lrc), data = t_c.df)

# MATCH PREDICTED AND FITTED VALUES ----

# forage
f.df <- f.df %>%
  mutate(f_fit = predict(f_fit))

# forage nitrogen
f_n.df <- f_n.df %>%
  mutate(fn_fit = predict(fn_fit))

# forage carbon
f_c.df <- f_c.df %>%
  mutate(fc_fit = predict(fc_fit))

# tea
t.df <- t.df %>%
  mutate(t_fit = predict(t_fit))

# tea nitrogen
t_n.df <- t_n.df %>%
  mutate(tn_fit = predict(tn_fit))

# tea carbon
t_c.df <- t_c.df %>%
  mutate(tc_fit = predict(tc_fit))

# PLOTTING ----

# forage
a <- f.df %>%
  ggplot(aes(days, forage_pct_remain, group = crop)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point") +
  geom_line(aes(y = f_fit, group = crop)) 

a

# forage nitrogen 
b <- f_n.df %>%
  ggplot(aes(days, forage_pct_n_remain)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", color = "orange") +
  geom_line(aes(y = fn_fit), color = "red") 

b

# forage carbon
c <- f_c.df %>%
  ggplot(aes(days, forage_pct_c_remain)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", color = "orange") +
  geom_line(aes(y = fc_fit), color = "red") 

c

# tea
d <- t.df %>%
  ggplot(aes(days, tea_pct_remain)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", color = "orange") +
  geom_line(aes(y = t_fit), color = "red")

d

# tea nitrogen
e <- t_n.df %>%
  ggplot(aes(days, tea_pct_n_remain)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", color = "orange") +
  geom_line(aes(y = tn_fit), color = "red")

e

# tea carbon
f <- t_c.df %>%
  ggplot(aes(days, tea_pct_c_remain)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", color = "orange") +
  geom_line(aes(y = tc_fit), color = "red") 

f

# put plots together
a + b + c + d + e + f

# EXTRACT K VALUES ----

# are the k - values just the estimate?
f.df %>%
  ggplot() +
  geom_point(aes(days, forage_pct_remain, color = crop, group = crop)) +
  geom_point(aes(days, f_fit, color = crop, group = crop))


# K-values for crop, block, location

# TESTING THINGS WITH BILL ----
forage.df <- full.df %>%
  select(location, crop, block, days, forage_pct_remain) %>%
  na.omit() %>%
  nest(data = -c(crop, block, location)) %>%
  mutate(
    fit = map(data, ~nls(forage_pct_remain ~ SSasymp(days, Asym, R0, lrc), data = .)),
    tidied = map(fit, tidy),
    augmented = map(fit, augment))


nls(forage_pct_remain ~ SSasymp(days, Asym, R0, lrc), data = f.df)








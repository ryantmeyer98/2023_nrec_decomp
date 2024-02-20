# LIBRARIES ----
library(tidyverse)
library(broom)
library(patchwork)

# READ IN THE DATA ----
full.df <- read_csv("output/pct remaining data.csv")

# REMOVE NA VALUES ----
full.df <- full.df %>%
  group_by(location, crop, block) %>%
  mutate(across(c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
                  tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain), 
                ~ replace_na(., mean(., na.rm = TRUE)))) %>%
  ungroup()

# row number
full.df <- full.df %>%
  mutate(row = row_number())

# CREATE DATA FRAMES FOR EACH VARIABLE ----

# forage biomass
f.df <- full.df %>%
  select(location, crop, block, days, forage_pct_remain)

# forage n
f_n.df <- full.df %>%
  select(location, crop, block, days, forage_pct_n_remain)

# forage c
f_c.df <- full.df %>%
  select(location, crop, block, days, forage_pct_c_remain)

# tea biomass
t.df <- full.df %>%
  select(location, crop, block, days, tea_pct_remain)

# tea n
t_n.df <- full.df %>%
  select(location, crop, block, days, tea_pct_n_remain)

# tea c 
t_c.df <-full.df %>%
  select(location, crop, block, days, tea_pct_c_remain)

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

# TESTING THINGS WITH BILL ----

# trying the automation
forage <- full.df %>%
  filter(crop == "AR") %>%
  select(location, crop, block, days, forage_pct_remain) %>%
  nest(data = -c(crop, block, location)) %>%
  mutate(fit = map(data, ~ nls(forage_pct_remain ~ yf + (y0 - yf) * exp(-k * days),
                               start = list(y0 = 100, yf = 80, k = 0.1),
                               data = .)))


# extracting parameters
forage_sum <- forage %>%
  mutate(tidied = map(fit, tidy)) %>%
  unnest(tidied)

# arrange
forage_sum <- forage_sum %>%
  arrange(block, crop)


# pull out fitted values for analysis
k_nonlin_nitrogen_summary.df <- k_nonlin_nitrogen.df %>% 
  mutate(tidied = map(fit, tidy)) %>% 
  unnest(tidied)



full.df %>%
  ggplot(aes(days, forage_pct_remain, group = crop, color = crop)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "nls", 
              formula = y ~ yf + (y0 - yf) * exp(-k * x),
              method.args = list(start = c(y0 = 100, yf = 80, k = 0.1)),
              se = FALSE)

full.df %>%
  ggplot(aes(days, forage_pct_remain, group = crop, color = crop)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "nls", formula = y ~ 100 * exp(-k*x), 
              method.args = list(start = c(k=0.001)), se = FALSE)


full.df %>%
  ggplot(aes(days, forage_pct_n_remain, group = crop, color = crop)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "nls", 
              formula = y ~ yf + (y0 - yf) * exp(-k * x),
              method.args = list(start = c(y0 = 100, yf = 80, k = 0.1)),
              se = FALSE)


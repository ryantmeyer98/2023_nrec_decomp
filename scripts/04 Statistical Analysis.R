# LIBRARIES
library(tidyverse)
library(car)
library(emmeans)
library(multcompView)
library(patchwork)


# READ IN THE DATA FOR ANALYSIS ----

# forage biomass
f_b.df <- read_csv("output/forage biomass k values.csv")
# forage carbon
f_c.df <- read_csv("output/forage carbon k values.csv")
# forage nitrogen
f_n.df <- read_csv("output/forage nitrogen k values.csv")
# tea biomass
t_b.df <- read_csv("output/tea biomass k values.csv")
# tea carbon
t_c.df <- read_csv("output/tea carbon k values.csv")
# tea nitrogen
t_n.df <- read_csv("output/tea nitrogen k values.csv")

# QUESTION: does biomass/nutrient loss differ by location and crop?

# first thing i need to do is make my statistical model to test assumptions
# Fixed effects: location; crop (for our purposes we treat block as a fixed effect)

# MODELS AND ANALYSIS OF RESIDUALS ----

# forage biomass model - RESIDUALS PASS VISUAL INSEPCTION
f_b.lm = lm(estimate ~ block + crop * location, data = f_b.df)

# residuals
residuals <- resid(f_b.lm)
plot(fitted(f_b.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# forage carbon model - RESIDUALS PASS VISUAL INSPECTION
f_c.lm = lm(estimate ~ block + crop * location, data = f_c.df)

# residuals
residuals <- resid(f_c.lm)
plot(fitted(f_c.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# forage nitrogen model 
f_n.lm = lm(estimate ~ block + crop * location, data = f_n.df)

# residuals - RESIDUALS PASS VISUAL INSPECTION
residuals <- resid(f_n.lm)
plot(fitted(f_n.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# tea biomass model 
t_b.lm = lm(estimate ~ block + crop * location, data = t_b.df)

# residuals - RESIDUALS PASS VISUAL INSPECTION
residuals <- resid(t_b.lm)
plot(fitted(t_b.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# tea carbon model 
t_c.lm = lm(estimate ~ block + crop * location, data = t_c.df)

# residuals - RESIDUALS PASS VISUAL INSPECTION
residuals <- resid(t_c.lm)
plot(fitted(t_c.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# tea nitrogen model 
t_n.lm = lm(estimate ~ block + crop * location, data = t_n.df)

# residuals - RESIDUALS PASS VISUAL INSPECTION
residuals <- resid(t_n.lm)
plot(fitted(t_n.lm), residuals) 
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# RUN THE MODEL ----
# residuals look good, we may continue with the analysis

# forage biomass 
Anova(f_b.lm, type = "3", test.statistic = "F")
# fail to reject null hypothesis for the interaction, drop it from the model
f_b.lm = lm(estimate ~ block + crop + location, data = f_b.df)
# run the model without the interaction
Anova(f_b.lm, type = "3", test.statistic = "F")

# forage carbon
Anova(f_c.lm, type = "3", test.statistic = "F")
# fail to reject null hypothesis for the interaction, drop it from the model 
f_c.lm = lm(estimate ~ block + crop + location, data = f_c.df)
# rerun the model
Anova(f_c.lm, type = "3", test.statistic = "F")

# forage nitrogen
Anova(f_n.lm, type = "3", test.statistic = "F")
# fail to reject null hypothesis for the interaction, drop it from the model
f_n.lm = lm(estimate ~ block + crop + location, data = f_n.df)
# rerun the model
Anova(f_n.lm, type = "3", test.statistic = "F")

# tea biomass
Anova(t_b.lm, type = "3", test.statistic = "F")

# tea carbon
Anova(t_c.lm, type = "3", test.statistic = "F")
# fail to reject null hypothesis for the interaction, drop it from the model
t_c.lm = lm(estimate ~ block + crop + location, data = t_c.df)
# rerun the model
Anova(t_c.lm, type = "3", test.statistic = "F")

# tea nitrogen
Anova(t_n.lm, type = "3", test.statistic = "F")


# RESULTS SUMMARY, WHAT POST HOC TESTS DO WE NEED TO DO 
# forage biomass; reject null for crop, continue with post hoc tests
# forage carbon; reject null for crop and location, continue with post hoc tests
# forage nitrogen; reject null for crop, continue with post hoc tests
# tea biomass; reject null for interaction, continue with post hoc tests 
# tea carbon; reject null for crop, continue with post hoc tests
# tea nitrogen; reject null for interaction, continue with post hoc tests. 


# POST F TESTS ----
# WILL USE ALL PAIRWISE COMPARISONS WITH sidak P-VALUE ADJUSTMENT
# not sure how i want to handle the interactions right now

# forage biomass
# extract estimated marginal means and put them into a dataframe
f_b.emm <- emmeans(f_b.lm, ~ crop)
f_b_results.df <- as.data.frame(f_b.emm)
# run all pairwise comparisons with a tukey p-value adjustment
f_b.pw <- pairs(emmeans(f_b.lm, ~ crop))
f_b.pw
# pull out compact letter display and save as a dataframe
f_b.cld <- as.data.frame(multcomp::cld(f_b.emm, Letters = letters, adjust = "sidak"))

# forage carbon
# for crop
# extract estimated marginal means
f_c.emm <- emmeans(f_c.lm, ~ crop)
# get estimates marginal means and letters into a dataframe
f_c_crop.cld <- as.data.frame(multcomp::cld(f_c.emm, Letters = letters, adjust = "sidak"))
#for location
f_c.emm <- emmeans(f_c.lm, ~ location)
# get estimated marginal means and cld for location
f_c_location.cld <- as.data.frame(multcomp::cld(f_c.emm, Letters = letters, adjust = "sidak"))

# forage nitrogen
# extract estimated marginal means
f_n.emm <- emmeans(f_n.lm, ~ crop)
# cld
f_n.cld <- as.data.frame(multcomp::cld(f_n.emm, Letters = letters, adjust = "sidak"))

# tea biomass
# extract estimated marginal means for the interaction
t_b.emm <- emmeans(t_b.lm, ~ crop * location)
# cld
t_b.cld <- as.data.frame(multcomp::cld(t_b.emm, Letters = letters, adjust = "sidak"))

# tea carbon
# extract estimated marginal means for crop
t_c.emm <- emmeans(t_c.lm, ~ crop)
# cld
t_c.cld <- as.data.frame(multcomp::cld(t_c.emm, Letters = letters, adjust = "sidak"))
# these results look funky i want to pull out values for the pairwise comparisons
t_c.pw <- pairs(emmeans(t_c.lm, ~ crop))
t_c.pw

# tea nitrogen
# extract estimated marginal means for the interaction
t_n.emm <- emmeans(t_n.lm, ~ crop * location) 
# cld
t_n.cld <- as.data.frame(multcomp::cld(t_n.emm, Letters = letters, adjust = "Sidak"))

# SAVE OUTPUT ----

# forage biomass
write_csv(f_b.cld, file = "output/stats output/forage biomass.csv")
# forage carbon crop
write_csv(f_c_crop.cld, file = "output/stats output/forage carbon crop.csv")
# forage carbon location
write_csv(f_c_location.cld, file = "output/stats output/forage carbon location.csv")
# forage nitrogen
write_csv(f_n.cld, file = "output/stats output/forage nitrogen.csv")
# tea biomass
write_csv(t_b.cld, file = "output/stats output/tea biomass.csv")
# tea carbon
write_csv(t_c.cld, file = "output/stats output/tea carbon.csv")
# tea nitrogen
write_csv(t_n.cld, file = "output/stats output/tea nitrogen.csv")

# SUMMARY STATS ----

# read in the data
data.df <- read_csv("output/pct remaining data.csv")

data.df %>%
  group_by(block) %>%
  filter(days == 35) %>%
  summarize(mean = 100 - mean(tea_pct_n_remain, na.rm = TRUE),
            sd = sd(forage_pct_remain, na.rm = TRUE)) 


# initial C:N ratios?
data.df %>%
  group_by(days) %>%
  # filter(days == 0) %>%
  summarize(cn = mean(tea_pct_c, na.rm = TRUE) / mean(tea_pct_n, na.rm = TRUE))








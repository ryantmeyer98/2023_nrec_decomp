# LIBRARIES
library(tidyverse)
library(car)


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
# tea nitrogenl; reject null for interaction, continue with post hoc tests. 












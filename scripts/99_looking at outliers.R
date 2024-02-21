# this file is to look for outliers in the entered data

# LIBRARIES ----
library(tidyverse)
library(plotly)

# READ IN THE DATA and arrange columns and data then covert to factors----
decomp.df <- read_csv("output/pct remaining data.csv") %>% 
  # select(location, crop, block, days, 
  #        forage_initial_drywt_g, forage_final_drywt_g,  forage_pct_c, forage_pct_n, 
  #        tea_initial_drywt_g, tea_final_drywt_g, tea_pct_c, tea_pct_n) %>%
  arrange(location, crop, block, days) %>%
  mutate(
    location = as.factor(location),
    crop = as.factor(crop),
    block = as.factor(block))

# now to plot location = ISU, crop = AR of pct_c with color by block
outliers.plot <- decomp.df %>% 
  filter(location == "ISU", crop == "CR") %>% 
  ggplot(aes(days, forage_pct_c_remain, color = block, 
             text = paste0(location, "\n", crop, "\n", block, "\n", 
                           "initial= ", forage_initial_drywt_g , "\n", "final= ", forage_final_drywt_g,  "\n",
                           "mass lost = ", forage_initial_drywt_g - forage_final_drywt_g))) +
  #scale_y_continuous(limits = c(30, 45)) +
  geom_point()


# now to make a ggplotly graph with a text box showing the initial and final dryweights and the forage_pct_c values
ggplotly(outliers.plot) 


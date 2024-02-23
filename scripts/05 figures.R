# LIBRARIES ----
library(tidyverse)
library(patchwork)

# READ IN THE DATA ----

# STATS OUTPUT ----

# forage biomass
f_b.cld <- read_csv("output/stats output/forage biomass.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# forage carbon
f_c_crop.cld <- read_csv("output/stats output/forage carbon crop.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
f_c_location.cld <- read_csv("output/stats output/forage carbon location.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# forage nitrogen
f_n.cld <- read_csv("output/stats output/forage nitrogen.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea biomass
t_b.cld <- read_csv("output/stats output/tea biomass.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea carbon
t_c.cld <- read_csv("output/stats output/tea carbon.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea nitrogen
t_n.cld <- read_csv("output/stats output/tea nitrogen.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))

# K-VALUES ----

# forage biomass
f_b.df <- read_csv("output/forage biomass k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# forage carbon
f_c.df <- read_csv("output/forage carbon k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# forage nitrogen
f_n.df <- read_csv("output/forage nitrogen k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea biomass
t_b.df <- read_csv("output/tea biomass k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea carbon
t_c.df <- read_csv("output/tea carbon k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))
# tea nitrogen
t_n.df <- read_csv("output/tea nitrogen k values.csv") %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC"))

# PLOTTING OPTIONS ----

# theme
theme <- theme_light() +
  theme(text = element_text(size = 10),
        panel.grid = element_blank()) 

# FORAGE PLOTS ----

# forage biomass----
f_b.plot <- f_b.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.2)) + 
  geom_boxplot(data = f_b.df, aes(crop, estimate), width = 0.2, position = position_nudge(x = -0.3)) +
  # geom_point(data = f_b.df, aes(crop, estimate), position = position_nudge(x = -0.25), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  geom_text(aes(x = 1.3, y = 0.011859190, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.010625232, label = "a"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.009586184, label = "b"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.005874195, label = "b"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.006013715, label = "b"), size = 2.5) +
  labs(x = "Crop", y = "Forage Biomass Loss (g/d)") +
  theme

f_b.plot

# forage carbon (crop) ----
f_c_crop.plot <- f_c_crop.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.2)) + 
  geom_boxplot(data = f_c.df, aes(crop, estimate), width = 0.2, position = position_nudge(x = -0.3)) +
  # geom_point(data = f_c.df, aes(crop, estimate), position = position_nudge(x = -0.25), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  geom_text(aes(x = 1.3, y = 0.01814421, label = "c"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.01578856, label = "bc"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.01090678, label = "a"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01310649, label = "ab"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01171767, label = "ab"), size = 2.5) +
  labs(x = "Crop", y = "Forage Carbon Loss (g/d)") +
  theme

f_c_crop.plot

# forage carbon (location) ----
f_c_location.plot <- f_c_location.cld %>%
  ggplot(aes(location, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.2)) +
  geom_boxplot(data = f_c.df, aes(location, estimate), width = 0.2, position = position_nudge(x = -0.3)) +
  # geom_point(data = f_c.df, aes(location, estimate), position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 1.3, y = 0.01292263, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.01494285, label = "b"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "Crop", y = "Forage Carbon Loss by Location (g/d)") +
  theme

f_c_location.plot

# forage nitrogen ----
f_n.plot <- f_n.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.2)) +
  geom_boxplot(data = f_n.df, aes(crop, estimate), width = 0.2, position = position_nudge(x = -0.3)) +
  # geom_point(data = f_n.df, aes(crop, estimate), position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 1.3, y = 0.020976416, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.008756596, label = "b"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.003867444, label = "b"), size = 2.5) +
  geom_text(aes(x = 4.3, y = -0.000330541, label = "b"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.004603586, label = "b"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "Crop", y = "Forage Nitrogen Loss (g/d)") +
  theme

f_n.plot

forage.plot <- f_b.plot + f_c_crop.plot + f_n.plot
forage.plot

ggsave(forage.plot, file = "figures/forage final.pdf", 
       width = 7, height = 7)


# tea biomass ----
t_b_isu.plot <- t_b.cld %>%
  filter(location == "ISU") %>%
  ggplot(aes(crop, emmean, shape = location)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.5), 
                position = position_dodge2(width = 0.5)) +
  # geom_point(data = t_b.df, aes(crop, estimate), size = 3, 
  #            position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 3.3, y = 0.02428049, label = "c"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01569910, label = "abc"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01271000, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01252059, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01161667, label = "a"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.03)) +
  labs(x = "Crop", y = "Tea Biomass Loss (g/d)") +
  theme

t_b_isu.plot


test.plot <- t_b.cld %>%
  filter(location == "WIU") %>%
  ggplot(aes(crop, emmean, shape = location)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.5), 
                position = position_dodge2(width = 0.5)) +
  # geom_point(data = t_b.df, aes(crop, estimate), size = 3, 
  #            position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 1.3, y = 0.01577899, label = "abc"), size = 2.5) +
  geom_text(aes(x = 1.3, y = 0.01304710, label = "ab"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.02137316, label = "bc"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.01157267, label = "a"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.02281235, label = "c"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.02428049, label = "c"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01569910, label = "abc"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01271000, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01252059, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01161667, label = "a"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.03)) +
  labs(x = "Crop", y = "Tea Biomass Loss (g/d)") +
  theme

test.plot

t_b.plot + test.plot

# tea carbon
t_c.plot <- t_c.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) +
  # geom_boxplot(data = t_c.df, aes(crop, estimate), width = 0.1, position = position_nudge(x = -0.15)) +
  # geom_point(data = t_c.df, aes(crop, estimate), position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 1.3, y = 0.02379570, label = "a"), size = 4) +
  geom_text(aes(x = 2.3, y = 0.02914319, label = "b"), size = 4) +
  geom_text(aes(x = 3.3, y = 0.02586301, label = "b"), size = 4) +
  geom_text(aes(x = 4.3, y = 0.02417445, label = "b"), size = 4) +
  geom_text(aes(x = 5.3, y = 0.02444851, label = "b"), size = 4) +
  coord_cartesian(ylim = c(0, 0.03)) +
  labs(x = "Crop", y = "Tea Carbon Loss (g/d)") +
  theme

t_c.plot

# tea nitrogen ----
t_n.plot <- t_n.cld %>%
  ggplot(aes(crop, emmean, shape = location)) +
  geom_point(position = position_dodge2(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1), 
                position = position_dodge2(width = 0.1)) +
  # geom_point(data = t_n.df, aes(crop, estimate), size = 3, 
  #            position = position_nudge(x = -0.25), alpha = 0.5) +
  geom_text(aes(x = 0.7, y = 0.008673201, label = "a"), size = 4) +
  geom_text(aes(x = 1.3, y = 0.011442534, label = "a"), size = 4) +
  geom_text(aes(x = 1.7, y = 0.022304709, label = "c"), size = 4) +
  geom_text(aes(x = 2.3, y = 0.022814062, label = "c"), size = 4) +
  geom_text(aes(x = 2.7, y = 0.011499208, label = "ab"), size = 4) +
  geom_text(aes(x = 3.3, y = 0.013900923, label = "abc"), size = 4) +
  geom_text(aes(x = 3.7, y = 0.015657190, label = "abc"), size = 4) +
  geom_text(aes(x = 4.3, y = 0.022712713, label = "c"), size = 4) +
  geom_text(aes(x = 4.7, y = 0.016414369, label = "abc"), size = 4) +
  geom_text(aes(x = 5.3, y = 0.021067147, label = "bc"), size = 4) +
  coord_cartesian(ylim = c(0, 0.03)) +
  labs(x = "Crop", y = "Tea Nitrogen Loss (g/d)") +
  theme

t_n.plot

tea.plot <- t_b.plot + t_c.plot + t_n.plot + plot_layout(guides = "collect")

ggsave(tea.plot, file = "figures/tea final.pdf", 
       width = 7, height = 7)


  




















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

f_c_location.cld <- read_csv("output/stats output/forage carbon location.csv") 

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
  theme(
    text = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"), 
    panel.border = element_rect(color = "black"),
    panel.grid = element_blank()
  )


# FORAGE PLOTS ----

# forage biomass----
f_b.plot <- f_b.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) + 
  geom_boxplot(data = f_b.df, aes(crop, estimate,), width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  coord_cartesian(ylim = c(0, 0.04)) +
  geom_text(aes(x = 1.3, y = 0.011859190, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.010625232, label = "a"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.009586184, label = "b"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.005874195, label = "b"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.006013715, label = "b"), size = 2.5) +
  labs(x = "", y = "Cover Crop Biomass Loss (%/d)") +
  ggtitle(label = "A)") +
  theme

f_b.plot

# forage carbon (crop) ----
f_c_crop.plot <- f_c_crop.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) + 
  geom_boxplot(data = f_c.df, aes(crop, estimate), width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  coord_cartesian(ylim = c(0, 0.04)) +
  geom_text(aes(x = 1.3, y = 0.01814421, label = "c"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.01578856, label = "bc"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.01090678, label = "a"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01310649, label = "ab"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01171767, label = "ab"), size = 2.5) +
  labs(x = "Crop", y = "Cover Crop Carbon Loss (%/d)") +
  ggtitle(label = "B)") +
  
  theme

f_c_crop.plot

# forage carbon (location) ----
f_c_location.plot <- f_c_location.cld %>%
  ggplot(aes(location, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) +
  geom_boxplot(data = f_c.df, aes(location, estimate), width = 0.05, position = position_nudge(x = -0.15),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.1, y = 0.01292263, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.1, y = 0.01494285, label = "b"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.025)) +
  scale_x_discrete(labels = c("Lexington", "Macomb")) +
  labs(x = "Crop", y = "Cover Crop Carbon Loss by Location (%/d)") +
  
  theme

f_c_location.plot

# forage nitrogen ----
f_n.plot <- f_n.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) +
  geom_boxplot(data = f_n.df, aes(crop, estimate), width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.020976416, label = "a"), size = 2) +
  geom_text(aes(x = 2.3, y = 0.008756596, label = "b"), size = 2) +
  geom_text(aes(x = 3.3, y = 0.003867444, label = "b"), size = 2) +
  geom_text(aes(x = 4.3, y = -0.000330541, label = "b"), size = 2) +
  geom_text(aes(x = 5.3, y = 0.004603586, label = "b"), size = 2) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "", y = "Cover Crop Nitrogen Loss (%/d)") +
  ggtitle(label = "C)") +
  theme


f_n.plot

# forage plot 
f.plot <- f_b.plot + f_c_crop.plot + f_n.plot
f.plot

# forage carbon for location plot
f_c_location.plot

# tea biomass ----

# isu
t_b_isu.plot <- t_b.cld %>%
  filter(location == "ISU") %>%
  ggplot(aes(crop, emmean)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1), 
                position = position_dodge2(width = 0.5)) +
  geom_boxplot(data = subset(t_b.df, location == "ISU"), aes(crop, estimate), 
               width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.01577899, label = "abc"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.02137316, label = "bc"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.02281235, label = "c"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01569910, label = "abc"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01252059, label = "a"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "", y = "Tea Biomass Loss (%/d)") +
  ggtitle(label = "A)") +
  theme

t_b_isu.plot

# wiu
t_b_wiu.plot <- t_b.cld %>%
  filter(location == "WIU") %>%
  ggplot(aes(crop, emmean)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1), 
                position = position_dodge2(width = 0.5)) +
  geom_boxplot(data = subset(t_b.df, location == "WIU"), aes(crop, estimate), 
               width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.01304710, label = "ab"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.01157267, label = "a"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.02428049, label = "c"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.01271000, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.01161667, label = "a"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "", y = "") +
  ggtitle(label = "B)") +
  theme

t_b_wiu.plot

# blank
blank.plot <- ggplot() + 
  labs(x = "Crop") +
  geom_blank() +
  theme_bw() +
  theme(panel.border = element_blank())

blank.plot

# tea biomass plot full
t_b.plot <- t_b_isu.plot + blank.plot + t_b_wiu.plot +
  plot_layout(ncol = 3, widths = c(1, 0.1, 1))

t_b.plot

# tea carbon
t_c.plot <- t_c.cld %>%
  ggplot(aes(crop, emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1)) +
  geom_boxplot(data = t_c.df, aes(crop, estimate), width = 0.1, position = position_nudge(x = -0.15),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.02379570, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.02914319, label = "a"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.02586301, label = "a"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.02417445, label = "a"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.02444851, label = "a"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "Crop", y = "Tea Carbon Loss (%/d)") +
  theme

t_c.plot

# tea nitrogen ----

# isu
t_n_isu.plot <- t_n.cld %>%
  filter(location == "ISU") %>%
  ggplot(aes(crop, emmean)) +
  geom_point(position = position_dodge2(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1), 
                position = position_dodge2(width = 0.1)) +
  geom_boxplot(data = subset(t_n.df, location == "ISU"), aes(crop, estimate), 
               width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.011442534, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.022814062, label = "c"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.013900923, label = "abc"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.015657190, label = "abc"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.021067147, label = "bc"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "", y = "Tea Nitrogen Loss (%/d)") +
  ggtitle(label = "A)") +
  theme

t_n_isu.plot

# wiu
t_n_wiu.plot <- t_n.cld %>%
  filter(location == "WIU") %>%
  ggplot(aes(crop, emmean)) +
  geom_point(position = position_dodge2(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, width = 0.1), 
                position = position_dodge2(width = 0.1)) +
  geom_boxplot(data = subset(t_n.df, location == "WIU"), aes(crop, estimate), 
               width = 0.1, position = position_nudge(x = -0.3),
               fill = "grey90", alpha = 0.5, size = 0.1) +
  geom_text(aes(x = 1.3, y = 0.008673201, label = "a"), size = 2.5) +
  geom_text(aes(x = 2.3, y = 0.022304709, label = "c"), size = 2.5) +
  geom_text(aes(x = 3.3, y = 0.011499208, label = "ab"), size = 2.5) +
  geom_text(aes(x = 4.3, y = 0.022712713, label = "c"), size = 2.5) +
  geom_text(aes(x = 5.3, y = 0.016414369, label = "abc"), size = 2.5) +
  coord_cartesian(ylim = c(0, 0.04)) +
  labs(x = "", y = "") +
  ggtitle(label = "B)") +
  theme

t_n_wiu.plot

# blank
blank.plot <- ggplot() + 
  labs(x = "Crop") +
  geom_blank() +
  theme_bw() +
  theme(panel.border = element_blank())

blank.plot

# tea nitrogen plot 
t_n.plot <- t_n_isu.plot + blank.plot + t_n_wiu.plot + 
  plot_layout(ncol = 3, widths = c(1, 0.1, 1))

t_n.plot

# CALLING FINAL NUTRIENT PLOTS ----
f.plot

f_c_location.plot

t_b.plot

t_c.plot

t_n.plot

# CREATING DECAY PLOTS -----

# read in the data ----
decomp.df <- read_csv("output/pct remaining data.csv") %>% 
  arrange(location, crop, block, days)

# interpolate data from the prior value to the next value to fill missing na values using 
# the na.approx function from the zoo package
decomp.df <- decomp.df %>%
  group_by(location, crop, block) %>%
  mutate(across(c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
                  tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain), 
                ~ zoo::na.approx(., na.rm = FALSE))) %>%
  mutate(crop = as.factor(crop)) %>%
  mutate(crop = fct_relevel(crop, "PCRO", "AR", "CR", "GPC", "WPC")) %>%

  ungroup()

# select only the data we want 
decomp.df <- decomp.df %>%
  select(location, crop, block, days, forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
         tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain)

# pivot longer 
decomp_long.df <- decomp.df %>%
  pivot_longer(cols = c(forage_pct_remain, forage_pct_n_remain, forage_pct_c_remain,
                        tea_pct_remain, tea_pct_n_remain, tea_pct_c_remain),
               names_to = "variable",
               values_to = "value")

# plotting 
isu.plot <- decomp_long.df %>%
  filter(location == "ISU") %>%
  ggplot(aes(days, value, color = as.factor(crop))) +
  geom_smooth(aes(days, value, color = as.factor(crop)),
              method = "nls", formula = y ~ 100 * exp(-k*x),
              method.args = list(start = c(k=0.01)), se = FALSE, linewidth = 0.5) +
  # stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  labs(x = "Days after Placement", y = "Percent Remaining", color = "Crop") +
  ggtitle(label = "Illinois State University") +
  coord_cartesian(ylim = c(0, 100)) +
  theme +
  facet_wrap(~variable, labeller = labeller(variable = c(forage_pct_c_remain = "Forage Carbon",
                                                         forage_pct_n_remain = "Forage Nitrogen",
                                                         forage_pct_remain = "Forage Biomass",
                                                         tea_pct_c_remain = "Tea Carbon",
                                                         tea_pct_n_remain = "Tea Nitrogen",
                                                         tea_pct_remain = "Tea Biomass")))

wiu.plot <- decomp_long.df %>%
  filter(location == "WIU") %>%
  ggplot(aes(days, value, color = as.factor(crop))) +
  geom_smooth(aes(days, value, color = as.factor(crop)),
              method = "nls", formula = y ~ 100 * exp(-k*x),
              method.args = list(start = c(k=0.01)), se = FALSE, linewidth = 0.5) +
  # stat_summary(fun = mean, geom = "point", na.rm = TRUE) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = TRUE) +
  labs(x = "Days after Placement", y = "Percent Remaining", color = "Crop") +
  ggtitle(label = "Western Illinois University") +
  coord_cartesian(ylim = c(0, 100)) +
  theme +
  facet_wrap(~variable, labeller = labeller(variable = c(forage_pct_c_remain = "Forage Carbon",
                                                         forage_pct_n_remain = "Forage Nitrogen",
                                                         forage_pct_remain = "Forage Biomass",
                                                         tea_pct_c_remain = "Tea Carbon",
                                                         tea_pct_n_remain = "Tea Nitrogen",
                                                         tea_pct_remain = "Tea Biomass")))

pct_rem.plot <- isu.plot + wiu.plot + plot_layout(guides = "collect")

pct_rem.plot 
  
# SAVE THINGS ----

f.plot

f_c_location.plot

t_b.plot

t_c.plot

t_n.plot


ggsave(f.plot, file = "figures/forage final.pdf",
       width = 7, height = 7)

ggsave(f_c_location.plot, file = "figures/forage location.pdf",
       width = 7, height = 7)

ggsave(t_b.plot, file = "figures/tea biomass.pdf", 
       width = 7, height = 7)

ggsave(t_c.plot, file = "figures/tea carbon.pdf", 
       width = 7, height = 7)

ggsave(t_n.plot, file = "figures/tea nitrogen.pdf", 
       width = 7, height = 7)

ggsave(pct_rem.plot, file = "figures/pct remaining.pdf",
       width = 7, height = 7)




###
###
#' Script for:
#' 
#' 
#' Last update 2024/10/24
#' 
###
###

# Clear memory to make sure there are not files loaded that could cause problems
rm(list=ls())


##
##
##### libraries #####
##
##
pacman::p_load(tidyverse, 
               brms,
               tidybayes,
               broom,
               broom.mixed,
               emmeans,
               ggdist,
               ggokabeito,   
               gghalves,     
               ggbeeswarm, 
               gtsummary)

#####

##
##
##### read end data #####
##
##
data <- readRDS(file = "./data/02_end_data/prep_data_analysis_FILTERED_frequentist.RDS")
data$year <- as.numeric(data$year)
data$ydate <- yday(data$date)
head(data)

## filtering species with sex info
data <- data %>% 
  filter(sex != 'unknown') %>% 
  filter(species == 'Blackbird' | species == 'Chaffinch' | species == 'Great tit')

table(data$species)

#####

##
## 
##### Raw data summaries and exploration #####
##
##

##
## summarised data set to plot means
df_summary <- data %>% 
  group_by(season_clean, habitat, species, sex) %>% 
  summarise(mean_value = mean(relative_end_bcpa, na.rm=T),
            sd_value = sd(relative_end_bcpa,na.rm=T))

##
## raw data plot
sex_end <- ggplot(data = data %>% 
                      mutate(species = factor(x = species, 
                                              levels =  c("Blackbird", "Great tit", "Chaffinch")),
                             season_clean = factor(x = season_clean, 
                                                   levels =  c("Pre-breeding", 
                                                               "Post-breeding"))), 
                    aes(x = sex, 
                        y = relative_end_bcpa, 
                        fill = habitat,
                        color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15, face = "bold"),
        axis.title = element_text("Arial", size = 15),
        axis.text.y = element_text("Arial", size = 10),
        axis.text.x = element_text("Arial", size = 10, angle = 45, vjust = 0.70)) +
  scale_y_continuous(breaks = -4:4, labels = -4:4) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Relative end of activity ± SD") +
  facet_grid(species~season_clean) +
  geom_errorbar(data = df_summary, 
                aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value, y = mean_value),
                position = position_dodge(0.75), 
                color = "black",
                width = 0) +
  geom_point(data = df_summary, 
             aes(x = sex, y = mean_value),
             position = position_dodge(0.75),
             shape = 21,
             color = "black",
             size = 2.5)


#####

##
##
##### Analysis of end of activity using heterogeneous within and between individual variance across habitats #####
##
##

# model formula
model_formula_end_activity <- bf(relative_end_bcpa ~ 
                                   species : sex +
                                   season_clean : sex +
                                   habitat : sex +
                                   habitat : species +     # Q1: whether the effect of urbanisation on diel activity depended on the species?
                                   habitat : season_clean +# Q2: whether the effect of urbanisation on diel activity depended on the time of the year?
                                   season_clean +
                                   habitat + 
                                   species +
                                   year + 
                                   min_temperature +
                                   rainfall +
                                   (1|date) +
                                   (1|location) +
                                   (1|gr(ring_nuber, by = habitat)),
                                 sigma ~ 0 + habitat)

# fit model
job(
  {
model_end_activity <- brm(model_formula_end_activity, 
                          data = data, 
                          chains = 3, 
                          cores = 3, 
                          iter = 75000, 
                          warmup = 50000, 
                          thin = 10,
                          seed = 7, 
                          backend = 'cmdstanr',
                          file_refit = 'always',
                          file = './models/02_end_activity/brms_model_end_activity_freq_sex.RDS')
  },
title = 'End of activity frequentist model including sex info only'
)

# read model back if needed (no need to fit the model every time the script is run)
model_end_activity <- readRDS('./models/02_end_activity/brms_model_end_activity_freq_sex.RDS')
print(summary(model_end_activity), digits = 3)
plot(model_end_activity) # looks fairly okay
max(rhat(model_end_activity)) # looks fairly okay
pp_check(model_end_activity) # looks fairly okay

#####

##
##
##### Plot model results and raw data #####
##
##

# data with model predictions
grand_mean_end <- model_end_activity %>% 
  epred_draws(newdata = expand_grid(year = mean(data$year),
                                    min_temperature = mean(data$min_temperature),
                                    rainfall = mean(data$rainfall),
                                    habitat = c("Urban", "Forest"),
                                    sex = c('male', 'female'),
                                    species = unique(data$species),
                                    season_clean = unique(data$season_clean)), 
              re_formula = NA) %>% 
  rename(relative_end_bcpa = '.epred')

# plot
grand_mean_end$filter_out <- ifelse(grand_mean_end$species == 'Blackbird' & 
                                      grand_mean_end$season_clean == 'Post-breeding' &
                                      grand_mean_end$habitat == 'Forest', 1, 0)
grand_mean_end_plot <- grand_mean_end %>% 
  filter(filter_out == 0)

data$filter_out <- ifelse(data$species == 'Blackbird' & 
                            data$season_clean == 'Post-breeding' &
                            data$habitat == 'Forest', 1, 0)
# plot
fig_s7b <- ggplot(data = data %>% 
                    filter(filter_out == 0) %>% 
                    mutate(species = factor(x = species, 
                                            levels =  c("Robin", "Blackbird",
                                                        "Great tit", "Blue tit",
                                                        "Dunnock", "Chaffinch")),
                           season_clean = factor(x = season_clean, 
                                                 levels =  c("Pre-breeding", 
                                                             "Post-breeding"))), 
                  aes(x = species, 
                      y = relative_end_bcpa, 
                      fill = habitat,
                      color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(legend.position = 'top', 
        legend.direction = 'horizontal',
        legend.text = element_text("Arial", size = 16),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 20, face = "bold"),
        axis.title = element_text("Arial", size = 16),
        axis.text.y = element_text("Arial", size = 14),
        axis.text.x = element_text("Arial", size = 14, angle = 45, vjust = 0.70)) +
  scale_y_continuous(breaks = -4:2, labels = -4:2) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Relative end of activity") +
  facet_grid(sex~season_clean) +
  stat_pointinterval(data = grand_mean_end_plot, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') +
  geom_hline(yintercept = 0, linetype = 2)

saveRDS(object = fig_s7b, file = './plots/Figure_S7b.RDS')

#####

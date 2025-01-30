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
               gt,
               gtsummary)

#####

##
##
##### read onset data #####
##
##
data <- readRDS(file = "./data/01_onset_data/prep_data_analysis_FILTERED_frequentist.RDS")
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
## Initial visualisation of data

##
## summarised data set to plot means
df_summary <- data %>% 
  group_by(season_clean, habitat, species, sex) %>% 
  summarise(mean_value = mean(relative_onset_bcpa, na.rm=T),
            sd_value = sd(relative_onset_bcpa,na.rm=T))

##
## raw data plot
sex_onset <- ggplot(data = data %>% 
         mutate(species = factor(x = species, 
                                 levels =  c("Blackbird", "Great tit", "Chaffinch")),
                season_clean = factor(x = season_clean, 
                                      levels =  c("Pre-breeding", 
                                                  "Post-breeding"))), 
       aes(x = sex, 
           y = relative_onset_bcpa, 
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
  scale_y_continuous(breaks = -4:2, labels = -4:2) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Relative onset of activity ± SD") +
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

ggsave(filename = './plots/relative_onset_sex.png', 
       plot = sex_onset, 
       units = 'mm',
       height = 150, 
       width = 150)

#####

##
##
##### Analysis of onset of activity using heterogeneous within and between individual variance across habitats #####
##
##

# read model output below - running the model takes a while

# model formula
model_formula_onset_activity <- bf(relative_onset_bcpa ~ 
                                     species : sex +
                                     season_clean : sex +
                                     habitat : sex +
                                     habitat : species +     # Q1: whether the effect of urbanisation on diel activity depended on the species?
                                     habitat : season_clean +# Q2: whether the effect of urbanisation on diel activity depended on the time of the year?
                                     season_clean +
                                     habitat + 
                                     species +
                                     sex +
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
    model_onset_activity <- brm(model_formula_onset_activity, 
                                data = data, 
                                chains = 3, 
                                cores = 3, 
                                iter = 75000, 
                                warmup = 50000, 
                                thin = 10,
                                seed = 15,
                                backend = 'cmdstanr',
                                file_refit = 'always',
                                file = './models/01_onset_activity/brms_model_onset_activity_freq_sex.RDS')
  },
  title = 'Onset of activity frequentist model including sex info only'
)

# read model back if needed (no need to fit the model every time the script is run)
model_onset_activity <- readRDS('./models/01_onset_activity/brms_model_onset_activity_freq_sex.RDS')
print(summary(model_onset_activity), digits = 3)
plot(model_onset_activity) # looks fairly okay
max(rhat(model_onset_activity)) # looks fairly okay
pp_check(model_onset_activity) # looks fairly okay

#####

##
##
##### Plot model results and raw data #####
##
##

# data with model predictions
grand_mean_onset <- model_onset_activity %>% 
  epred_draws(newdata = expand_grid(year = mean(data$year),
                                    min_temperature = mean(data$min_temperature),
                                    rainfall = mean(data$rainfall),
                                    habitat = c("Urban", "Forest"),
                                    sex = c('male', 'female'),
                                    species = unique(data$species),
                                    season_clean = unique(data$season_clean)), 
              re_formula = NA) %>% 
  rename(relative_onset_bcpa = '.epred')

# plot
fig_s7a <- ggplot(data = data %>% 
                   mutate(species = factor(x = species, 
                                           levels =  c("Robin", "Blackbird",
                                                       "Great tit", "Blue tit",
                                                       "Dunnock", "Chaffinch")),
                          season_clean = factor(x = season_clean, 
                                                levels =  c("Pre-breeding", 
                                                            "Post-breeding"))), 
                 aes(x = species, 
                     y = relative_onset_bcpa, 
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
  labs(x = "Species", y = "Relative onset of activity") +
  facet_grid(sex~season_clean) +
  stat_pointinterval(data = grand_mean_onset, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') +
  geom_hline(yintercept = 0, linetype = 2)

saveRDS(object = fig_s7a, file = './plots/Figure_S7a.RDS')

#####

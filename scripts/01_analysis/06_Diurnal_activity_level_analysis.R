###
###
#' Script for:
#' 
#' 
#' Last update 2024/10/22
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
               marginaleffects,
               ggdist,
               ggokabeito,   
               gghalves,     
               ggbeeswarm,
               gt,
               gtsummary)

#####

##
##
##### read end data #####
##
##
data <- readRDS(file = "./data/03_activity_data/prep_activity_active_windows.RDS")
data$obs_ID <- 1:nrow(data)
data$year <- as.numeric(data$year)
head(data)

#####

##
##
##### Calculate times active during day and night and filter based on non-missing data #####
##
##

# for day window
data$time_active_day <- (data$active_day*3)/60
data$time_non_active_day <- (data$active_day*3)/60
data$proportion_active_day <- data$active_day/data$non_missing_obs_day

## keep day and night data with 50% of non-missing values
table(data$non_missing_obs_day/data$total_obs_day > 0.50)

## data filtered
data <- data %>% 
  filter(non_missing_obs_day / total_obs_day > 0.50)

summary(data$proportion_active_day)
summary(data$time_active_day)
data[data$time_active_day == 0,]

#####

##
##
##### samples sizes and raw data summaries #####
##
##

## (it should have already been cleaned in filtering script)
head(data)

# n observations per habitat
data %>% 
  group_by(habitat) %>% 
  summarise(n())

# n observations per season
data %>% 
  group_by(season_clean) %>% 
  summarise(n()) 

# n observations per sex
data %>% 
  group_by(sex) %>% 
  summarise(n())

# n observations per species
data %>% 
  group_by(species) %>% 
  summarise(n())

# n observations per year
data %>% 
  group_by(year) %>% 
  summarise(n())

####

# n ind per habitat
data %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(habitat) %>% 
  summarise(n())

# n ind per season
data %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(season_clean) %>% 
  summarise(n())


# n ind per sex
data %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(sex) %>% 
  summarise(n())

# n ind per species
data %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(species) %>% 
  summarise(n())

# n ind per year
data %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(year) %>% 
  summarise(n())

####

# n of individuals samples across multiple years and seasons

# season
data %>% 
  group_by(ring_nuber, season_clean) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()

# years
data %>% 
  group_by(ring_nuber, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()




length(unique(data$ring_nuber))
length(unique(data$ring_nuber[data$habitat == 'Urban']))
length(unique(data$ring_nuber[data$habitat == 'Forest']))


table(data$species, data$habitat)
table({data %>% 
    group_by(ring_nuber) %>% 
    filter(row_number() == 1)}$species, 
    {data %>% 
        group_by(ring_nuber) %>% 
        filter(row_number() == 1)}$habitat)

## mean
data %>% 
  group_by(habitat) %>% 
  filter(!is.na(proportion_active_day)) %>% 
  summarise(mean_act = mean(proportion_active_day), 
            sd_act = sd(proportion_active_day),
            n_obs = n())

#####

##
##
##### Initial visualisation of data #####
##
##

##
## summarised data set to plot means
df_summary <- data %>% 
  group_by(season_clean, habitat, species) %>% 
  summarise(mean_value = mean(active_day/non_missing_obs_day, na.rm=T),
            sd_value = sd(active_day/non_missing_obs_day,na.rm=T),
            se_value = sd(active_day/non_missing_obs_day,na.rm=T)/sqrt(n()),
            n_birds = n()) %>% 
  mutate(species = factor(x = species, 
                          levels =  c("Robin", "Blackbird",
                                      "Great tit", "Blue tit",
                                      "Dunnock", "Chaffinch")),
         season_clean = factor(x = season_clean,
                               levels =  c("Pre-breeding", 
                                           "Post-breeding")))

##
## raw data plot
ggplot(data = data %>% 
         mutate(species = factor(x = species, 
                                 levels =  c("Robin", "Blackbird",
                                             "Great tit", "Blue tit",
                                             "Dunnock", "Chaffinch")),
                season_clean = factor(x = season_clean,
                                      levels =  c("Pre-breeding", 
                                                  "Post-breeding"))), 
       aes(x = species, 
           y = active_day/non_missing_obs_day, 
           fill = habitat,
           color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(legend.position = "top", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15, face = "bold"),
        axis.title = element_text("Arial", size = 15),
        axis.text.y = element_text("Arial", size = 10),
        axis.text.x = element_text("Arial", size = 10, angle = 45, vjust = 0.70)) +
  scale_y_continuous(breaks = seq(0,1,0.1), labels =  seq(0,1,0.1)) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Level of activity day time ± SE", title = "Day time") +
  facet_wrap(~season_clean) +
  geom_errorbar(data = df_summary, 
                aes(ymin = mean_value-se_value, ymax = mean_value+se_value, y = mean_value),
                position = position_dodge(0.75), 
                color = "black",
                width = 0) +
  geom_point(data = df_summary, 
             aes(x = species, y = mean_value),
             position = position_dodge(0.75),
             shape = 21,
             color = "black",
             size = 3)

#####

##
##
##### Analysis of activity levels during day #####
##
##

# read model output below - running the model takes a while

# model for day
#bf_day_activity <- bf(active_day | trials(non_missing_obs_day) ~
#                        habitat : species +      # Q1: whether the effect of urbanisation on diel activity depended on the species?
#                        habitat : season_clean + # Q2: whether the effect of urbanisation on diel activity depended on the time of the year?
#                        season_clean +
#                        habitat + 
#                        species +
#                        year + 
#                        min_temperature +
#                        rainfall +
#                        (1|a|gr(obs_ID, by = habitat))+
#                        (1|b|gr(ring_nuber, by = habitat)) +
#                        (1|c|date) +
#                        (1|d|location))
#
#model_activity_day <- brm(bf_day_activity,
#                          data = data,
#                          family = binomial(link = "logit"),
#                          chains = 3, 
#                          cores = 3, 
#                          iter = 100000, 
#                          warmup = 25000, 
#                          thin = 50,
#                          seed = 1234,
#                          backend = 'cmdstanr',
#                          file_refit = 'always',
#                          file = './models/brms_model_activity_level_day.RDS')

# read model back if needed (no need to fit the model every time the script is run)
model_activity_day <- readRDS('./models/04_level_activity/brms_model_activity_level_day.RDS')
plot(model_activity_day) # looks fairly okay
max(rhat(model_activity_day)) # looks fairly okay
pp_check(model_activity_day) # looks fairly okay

print(summary(model_activity_day), digits = 3)

# posterior predictive checks
pp_check(model_activity_day) # looks fairly okay
pp_check(model_activity_day, type = "error_hist", ndraws = 11) # looks fairly okay

## extract variance components
get_variables(model_activity_day)

##
## table with model results - CAREFUL NAME OF VARIABLES INCORRECTLY ASSIGNED, NEEDS TO BE ADJUSTED MANUALLY
table_model_activity_day <- model_activity_day %>%
  tbl_regression(intercept = T,tidy_fun = broom.mixed::tidy,
                 estimate_fun = ~ style_number(.x, digits = 3)) %>% 
  modify_header(label ~ "**Model term**") %>% 
  modify_header(estimate ~ "**Estimate**") %>%
  modify_header(ci~ "**95%CrI**") %>% 
  as_gt() 

##
## save table
gtsave(table_model_activity_day, "./tables/Table S5 raw.html")

#####

##
##
##### Plot model results and raw data #####
##
##

# data with model predictions
grand_mean <- model_activity_day %>% 
  epred_draws(newdata = expand_grid(year = mean(data$year),
                                    min_temperature = mean(data$min_temperature),
                                    rainfall = mean(data$rainfall),
                                    habitat = c("Urban", "Forest"),
                                    species = unique(data$species),
                                    season_clean = unique(data$season_clean),
                                    non_missing_obs_day = floor(mean(data$non_missing_obs_day))), 
              re_formula = NA) %>% 
  rename(active_day = '.epred')


# plot
fig_s10a <- ggplot(data = data %>% 
                   mutate(species = factor(x = species, 
                                           levels =  c("Robin", "Blackbird",
                                                       "Great tit", "Blue tit",
                                                       "Dunnock", "Chaffinch")),
                          season_clean = factor(x = season_clean, 
                                                levels =  c("Pre-breeding", 
                                                            "Post-breeding"), ordered = T)), 
                 aes(x = species, 
                     y = active_day / non_missing_obs_day, 
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
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2), labels = seq(0, 0.8, 0.2), limits = c(0,0.8)) +
  labs(x = "Species", y = "Proportion of active time during diurnal phase") +
  facet_wrap(~season_clean) +
  stat_pointinterval(data = grand_mean %>% 
                       mutate(season_clean = factor(x = season_clean, 
                                                    levels =  c("Pre-breeding", 
                                                                "Post-breeding"), 
                                                    ordered = T)), .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') 

saveRDS(object = fig_s10a, file = './plots/Figure_s10a.RDS')

#####

##
##
##### Comparisons of posterior dist to test specific hypothesis #####
##
##

##
##  Q1: does the effect of urbanisation on diel activity depended on the species?
model_activity_day_pred <- model_activity_day %>% 
  epred_draws(newdata = expand.grid(year = mean(data$year),
                                    min_temperature = mean(data$min_temperature),
                                    rainfall = mean(data$rainfall),
                                    non_missing_obs_day = floor(mean(data$non_missing_obs_day)),
                                    species = c("Robin", "Blackbird", "Great tit", "Blue tit","Dunnock", "Chaffinch"),
                                    season_clean = c('Pre-breeding', 'Post-breeding'),
                                    habitat = c('Urban', 'Forest')),
              re_formula = NA) %>% 
  mutate(response_prediction = .epred/non_missing_obs_day) 
              
model_activity_day_pred_diff <- model_activity_day_pred %>% 
  compare_levels(variable = .epred, by = habitat, comparison = list(c('Urban', 'Forest'))) %>% 
  group_by(species) %>% 
  median_hdi(.epred)
  

# plot
fig_s10b <- model_activity_day_pred %>% 
  compare_levels(variable = response_prediction, by = habitat, comparison = list(c('Urban', 'Forest'))) %>% 
  mutate(species = factor(x = species, 
                          levels =  c("Robin", "Blackbird",
                                      "Great tit", "Blue tit",
                                      "Dunnock", "Chaffinch"))) %>% 
  ggplot(., 
         aes(x = response_prediction, 
             y = species)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(shape = 21, .width = 0.95, 
               point_size = 4, linewidth = 1.5,
               fill = 'grey90') + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_fill_okabe_ito() +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15), 
                     labels = c('-0.3', '-0.15', '0', '0.15', '0.3'),
                     limits = c(-0.3, 0.3)) +
  labs(y = 'Species', x = expression(atop('Urban - Forest difference ', 
                                          'in diurnal level of activity')))

saveRDS(object = fig_s10b, file = './plots/Figure_s10b.RDS')

##
##  Q2: does the effect of urbanisation on diel activity depended on the time of the year?
model_activity_day_pred_diff_season <- model_activity_day_pred %>% 
  compare_levels(variable = response_prediction, by = habitat, comparison = list(c('Urban', 'Forest'))) %>% 
  group_by(season_clean) %>% 
  median_hdi(response_prediction)

# formal comparison
model_activity_day_pred %>% 
  compare_levels(variable = response_prediction, by = habitat, comparison = list(c('Urban', 'Forest'))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = 'season_clean', values_from = 'response_prediction') %>% 
  mutate(diff_time_year = `Pre-breeding` - `Post-breeding`) %>% 
  median_hdi(diff_time_year)


# plot
fig_s10c <- model_activity_day_pred %>% 
  compare_levels(variable = response_prediction, by = habitat, comparison = list(c('Urban', 'Forest'))) %>% 
  mutate(season_clean = factor(x = season_clean, 
                               levels =  c("Pre-breeding", 
                                           "Post-breeding"))) %>% 
  ggplot(., 
         aes(x = response_prediction, 
             y = season_clean)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(shape = 21, .width = 0.95, 
               point_size = 4, linewidth = 1.5,
               fill = 'grey90') + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15), 
                     labels = c('-0.3', '-0.15', '0', '0.15', '0.3'),
                     limits = c(-0.3, 0.3)) +
  labs(y = 'Time of the year', x = expression(atop('Urban - Forest difference ', 
                                                   'in diurnal level of activity')))


saveRDS(object = fig_s10c, file = './plots/Figure_s10c.RDS')

#####



##
##
##### Calculate among individual variances in prop of activity #####
##
##
get_variables(model_activity_day)

model_activity_day %>% 
  gather_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  median_hdi()

model_activity_day %>% 
  spread_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(difference_among = `sd_ring_nuber__Intercept:habitatUrban` - `sd_ring_nuber__Intercept:habitatForest`) %>% 
  median_hdi(difference_among)

#####

##
##
##### Calculate within individual variances in onset of activity #####
##
##
model_activity_day %>% 
  gather_draws(`sd_obs_ID__Intercept:habitatForest`, 
               `sd_obs_ID__Intercept:habitatUrban`) %>% 
  median_hdi()

model_activity_day %>% 
  spread_draws(`sd_obs_ID__Intercept:habitatForest`, 
               `sd_obs_ID__Intercept:habitatUrban`) %>% 
  mutate(difference_within = `sd_obs_ID__Intercept:habitatUrban` - `sd_obs_ID__Intercept:habitatForest`) %>% 
  median_hdi(difference_within)

#####

##
##
##### Plot between and within ind variance estimates #####
##
##
# plot
fig_2d <- model_activity_day %>% 
  gather_draws(`sd_obs_ID__Intercept:habitatForest`,
               `sd_obs_ID__Intercept:habitatUrban`,
               `sd_ring_nuber__Intercept:habitatForest`,
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(habitat = ifelse(.variable == 'sd_obs_ID__Intercept:habitatForest' | .variable == 'sd_ring_nuber__Intercept:habitatForest', 
                          'Forest', 'Urban'),
         variance_type = ifelse(.variable == 'sd_obs_ID__Intercept:habitatUrban' | .variable == 'sd_obs_ID__Intercept:habitatForest', 
                                'Within', 'Between')) %>% 
  ggplot(., aes(x = (.value), 
                fill = habitat, 
                y = variance_type)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(aes(group = habitat, fill = habitat), 
               #position = position_dodge(width = 0.10),
               shape = 21, .width = 0.95, slab_alpha = 0.5,
               point_size = 4, linewidth = 1.5, position = position_dodge(0.05)) + 
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2.5, 0.5)) +
  scale_fill_okabe_ito() +
  labs(title = 'Diurnal level of activity', 
       y = 'Variance component', 
       x = bquote('Standard deviation [95%CrI]'~(hours)))

saveRDS(object = fig_2d, file = './plots/Figure_2d.RDS')

#####


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
onset <- readRDS(file = "./data/01_onset_data/prep_data_analysis_FILTERED_frequentist.RDS")
head(onset)

end <- readRDS(file = "./data/02_end_data/prep_data_analysis_FILTERED_frequentist.RDS")
head(end)

##
## combine end of duration data
data <- left_join(x= onset, 
                  y = end %>%  
                    dplyr::select(date, ring_nuber, bcpa_end_time), 
                  by = c("date", "ring_nuber")) %>% 
  filter(!is.na(bcpa_onset_time)) %>% 
  filter(!is.na(bcpa_end_time)) %>% 
  mutate(duration_active_day = bcpa_end_time - bcpa_onset_time)
data$year <- as.numeric(data$year)

head(data)

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
  group_by(season_clean, habitat, species) %>% 
  summarise(mean_value = mean(duration_active_day, na.rm=T),
            sd_value = sd(duration_active_day,na.rm=T))

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
           y = duration_active_day, 
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
  facet_wrap(~season_clean) +
  geom_errorbar(data = df_summary, 
                aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value, y = mean_value),
                position = position_dodge(0.75), 
                color = "black",
                width = 0) +
  geom_point(data = df_summary, 
             aes(x = species, y = mean_value),
             position = position_dodge(0.75),
             shape = 21,
             color = "black",
             size = 2.5)

##
## Data Summaries 
data_model <- data %>% 
  filter(species != "Blackbird") 

##
##
##### Data Summaries #####
##
##

## (it should have already been cleaned in filtering script)
head(data_model)

# n observations per habitat
data_model %>% 
  group_by(habitat) %>% 
  summarise(n())

# n observations per season
data_model %>% 
  group_by(season_clean) %>% 
  summarise(n()) 

# n observations per sex
data_model %>% 
  group_by(sex) %>% 
  summarise(n())

# n observations per species
data_model %>% 
  group_by(species) %>% 
  summarise(n())

# n observations per year
data_model %>% 
  group_by(year) %>% 
  summarise(n())

####

# n ind per habitat
data_model %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(habitat) %>% 
  summarise(n())

# n ind per season
data_model %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(season_clean) %>% 
  summarise(n())

# n ind per sex
data_model %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(sex) %>% 
  summarise(n())

# n ind per species
data_model %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(species) %>% 
  summarise(n())

# n ind per year
data_model %>% 
  group_by(ring_nuber) %>% 
  filter(row_number() == 1) %>% 
  group_by(year) %>% 
  summarise(n())

####

# n of individuals samples across multiple years and seasons

# season
data_model %>% 
  group_by(ring_nuber, season_clean) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()

# years
data_model %>% 
  group_by(ring_nuber, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()





# n individuals 
length(unique(data_model$ring_nuber))
length(unique(data_model$ring_nuber[data_model$habitat == 'Urban']))
length(unique(data_model$ring_nuber[data_model$habitat == 'Forest']))

##
## summary of sample sizes
table(data_model$species, data_model$habitat)
table({data_model %>% 
    group_by(ring_nuber) %>% 
    filter(row_number() == 1)}$species, 
    {data_model %>% 
        group_by(ring_nuber) %>% 
        filter(row_number() == 1)}$habitat)

## mean duration
data_model %>% 
  group_by(habitat) %>% 
  summarise(mean_onset = mean(duration_active_day), 
            sd_onset = sd(duration_active_day))

#####

##
##
##### Analysis of onset of activity using heterogeneous within and between individual variance across habitats #####
##
##

# read model output below - running the model takes a while

# model formula
model_formula_duration_activity <- bf(duration_active_day ~ 
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
    model_duration_activity <- brm(model_formula_duration_activity, 
                                   data = data_model, 
                                   chains = 3, 
                                   cores = 3, 
                                   iter = 50000, 
                                   warmup = 20000, 
                                   thin = 10,
                                   seed = 25,
                                   backend = 'cmdstanr',
                                   file_refit = 'always',
                                   file = './models/03_duration_activity/brms_model_duration_activity_freq.RDS')
  },
  title = 'Duration of activity frequentist model'
)

# read model back if needed (no need to fit the model every time the script is run)
model_duration_activity <- readRDS('./models/03_duration_activity/brms_model_duration_activity_freq.RDS')
print(summary(model_duration_activity), digits = 3)
plot(model_duration_activity) # looks fairly okay
max(rhat(model_duration_activity)) # looks fairly okay
pp_check(model_duration_activity) # looks fairly okay

## extract variance components
get_variables(model_duration_activity)

##
## table with model results - gtsummary, broom.mixed::tidy and tab_model don't seem to work with this model structure. I extract the table of fixed effects witht the code below, and adjust manually
table_model_duration_activity <- model_duration_activity %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   sigma_habitatForest = 'Residual Variance Forest',
                   sigma_habitatUrban = 'Residual Variance Urban',
                   min_temperature = "Minimum daily temperature",
                   rainfall = "Daily rainfall",
                   season_clean = "Time of the year", 
                   habitat = "Habitat",
                   year = "Year",
                   species = "Species",
                   `date` = "Habitat x Species",
                   `location` = "Habitat x Season"),
                 estimate_fun = ~ style_number(.x, digits = 3)) %>% 
  modify_header(label ~ "**Model term**") %>% 
  modify_header(estimate ~ "**Estimate**") %>%
  modify_header(ci~ "**95%CrI**") %>% 
  as_gt() 

##
## save table
gtsave(table_model_duration_activity, "./tables/Table S4 raw.html")

#####

##
##
##### Comparisons of posterior dist to test specific hypothesis #####
##
##

##
##  Q1: does the effect of urbanisation on diel activity depended on the species?
q1_comparisons <- model_duration_activity %>% 
  emmeans(~ habitat + species,
          at = list(year = mean(data$year),
                    min_temperature = mean(data$min_temperature),
                    rainfall = mean(data$rainfall)),
          epred = TRUE, 
          re_formula = NA) %>% 
  contrast(method = 'revpairwise', by = c('species')) 

# plot
fig_S9b <- q1_comparisons %>% 
  gather_emmeans_draws() %>% 
  mutate(species = factor(x = species, 
                          levels =  c("Robin", "Blackbird",
                                      "Great tit", "Blue tit",
                                      "Dunnock", "Chaffinch"))) %>% 
  ggplot(., 
         aes(x = .value, 
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
  scale_x_continuous(breaks = c(-2:2), labels = c(-2:2), limits = c(-2.5, 2.5)) +
  labs(y = 'Species', x = expression(atop('Urban - Forest difference', 
                                          'duration of activity')))

saveRDS(object = fig_S9b, file = './plots/Figure_S9b.RDS')

##
##  Q2: does the effect of urbanisation on diel activity depended on the time of the year?
q2_comparisons <- model_duration_activity %>% 
  emmeans(~ habitat + season_clean,
          at = list(year = mean(data$year),
                    min_temperature = mean(data$min_temperature),
                    rainfall = mean(data$rainfall)),
          epred = TRUE, 
          re_formula = NA) %>% 
  contrast(method = 'revpairwise', by = c('season_clean')) 

# formal comparison
q2_comparisons %>% 
  gather_emmeans_draws() %>% 
  pivot_wider(names_from = 'season_clean', values_from = '.value') %>% 
  mutate(diff_time_year = `Pre-breeding` - `Post-breeding`) %>% 
  summarise(median = median(diff_time_year), 
            lower = quantile(diff_time_year, probs = 0.025),
            upper = quantile(diff_time_year, probs = 0.975))


# plot
fig_S9c <- q2_comparisons %>% 
  gather_emmeans_draws() %>% 
  mutate(season_clean = factor(x = season_clean, 
                               levels =  c("Pre-breeding", 
                                           "Post-breeding"))) %>% 
  ggplot(., 
         aes(x = .value, 
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
  scale_fill_okabe_ito() +
  scale_x_continuous(breaks = c(-2:2), labels = c(-2:2), limits = c(-2.5, 2.5)) +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(y = 'Time of the year', x = expression(atop('Urban - Forest difference', 
                                                   'duration of activity')))


saveRDS(object = fig_S9c, file = './plots/Figure_S9c.RDS')

#####

##
##
##### Plot model results and raw data #####
##
##

# data with model predictions
grand_mean_onset <- model_duration_activity %>% 
  epred_draws(newdata = expand_grid(year = mean(data_model$year),
                                    min_temperature = mean(data_model$min_temperature),
                                    rainfall = mean(data_model$rainfall),
                                    habitat = c("Urban", "Forest"),
                                    species = unique(data_model$species),
                                    season_clean = unique(data_model$season_clean)), 
              re_formula = NA) %>% 
  rename(duration_active_day = '.epred') %>% 
  mutate(season_clean = factor(x = season_clean, 
                               levels =  c("Pre-breeding", 
                                           "Post-breeding"), ordered = T))


# plot
fig_S9a <- ggplot(data = data_model %>% 
                   mutate(species = factor(x = species, 
                                           levels =  c("Robin", "Blackbird",
                                                       "Great tit", "Blue tit",
                                                       "Dunnock", "Chaffinch")),
                          season_clean = factor(x = season_clean, 
                                                levels =  c("Pre-breeding", 
                                                            "Post-breeding"), ordered = T)), 
                 aes(x = species, 
                     y = duration_active_day, 
                     fill = habitat,
                     color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(legend.position = 'top', 
        legend.direction = 'horizontal',
        legend.text = element_text("Arial", size = 15),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 20, face = "bold"),
        axis.title = element_text("Arial", size = 16),
        axis.text.y = element_text("Arial", size = 14),
        axis.text.x = element_text("Arial", size = 14, angle = 45, vjust = 0.70)) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Duration of diurnal activity (hours)") +
  facet_wrap(~season_clean) +
  stat_pointinterval(data = grand_mean_onset, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') 

saveRDS(object = fig_S9a, file = './plots/Figure_S9a.RDS')

#####

##
##
##### Calculate among individual variances in onset of activity #####
##
##
model_duration_activity %>% 
  gather_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(.value = .value) %>% 
  median_hdi()

model_duration_activity %>% 
  spread_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(`sd_ring_nuber__Intercept:habitatUrban` = `sd_ring_nuber__Intercept:habitatUrban`,
         `sd_ring_nuber__Intercept:habitatForest` = `sd_ring_nuber__Intercept:habitatForest`) %>% 
  mutate(difference_among = `sd_ring_nuber__Intercept:habitatUrban` - `sd_ring_nuber__Intercept:habitatForest`) %>% 
  median_hdi(difference_among)

#####

##
##
##### Calculate within individual variances in onset of activity #####
##
##
model_duration_activity %>% 
  gather_draws(b_sigma_habitatForest, 
               b_sigma_habitatUrban) %>% 
  mutate(.value = (exp(.value))) %>% 
  median_hdi()

model_duration_activity %>% 
  spread_draws(b_sigma_habitatForest, 
               b_sigma_habitatUrban) %>% 
  mutate(`b_sigma_habitatForest` = (exp(`b_sigma_habitatForest`)),
         `b_sigma_habitatUrban` = (exp(`b_sigma_habitatUrban`))) %>% 
  mutate(difference_within = b_sigma_habitatUrban - b_sigma_habitatForest) %>% 
  median_hdi(difference_within)

#####

##
## 
##### Calculate repeatability #####
##
##
model_duration_activity %>% 
  spread_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`,
               b_sigma_habitatForest,
               b_sigma_habitatUrban,
               sd_location__Intercept,
               sd_date__Intercept) %>%
  mutate(`b_sigma_habitatForest` = exp(`b_sigma_habitatForest`),
         `b_sigma_habitatUrban` = exp(`b_sigma_habitatUrban`)) %>% 
  mutate(total_pheno_forest = `sd_ring_nuber__Intercept:habitatForest`^2 + b_sigma_habitatForest^2 + sd_location__Intercept^2 + sd_date__Intercept^2,
         total_pheno_urban = `sd_ring_nuber__Intercept:habitatUrban`^2 + b_sigma_habitatUrban^2 + sd_location__Intercept^2 + sd_date__Intercept^2) %>% 
  mutate(rep_forest = `sd_ring_nuber__Intercept:habitatForest`^2 / total_pheno_forest,
         rep_urban = `sd_ring_nuber__Intercept:habitatUrban`^2 / total_pheno_urban) %>% 
  mutate(diff_urban_forest_rep = rep_urban - rep_forest) %>% 
  summarise(median_rep_forest = median(rep_forest),
            lower_rep_forest = quantile(rep_forest, 0.025),
            upper_rep_forest = quantile(rep_forest, 0.975),
            median_rep_urban = median(rep_urban),
            lower_rep_urban = quantile(rep_urban, 0.025),
            upper_rep_urban = quantile(rep_urban, 0.975),
            median_diff_rep = median(diff_urban_forest_rep),
            lower_diff_rep = quantile(diff_urban_forest_rep, 0.025),
            upper_diff_rep = quantile(diff_urban_forest_rep, 0.975))

#####

##
##
##### Plot between and within ind variance estimates #####
##
##
# plot
fig_2c <- model_duration_activity %>% 
  gather_draws(b_sigma_habitatForest, 
               b_sigma_habitatUrban,
               `sd_ring_nuber__Intercept:habitatForest`,
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(habitat = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'sd_ring_nuber__Intercept:habitatForest', 
                          'Forest', 'Urban'),
         variance_type = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'b_sigma_habitatUrban', 
                                'Within', 'Between'), 
         .value = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'b_sigma_habitatUrban', exp(.value), .value)) %>% 
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
               shape = 21, .width = 0.95, slab_alpha = 0.5,
               point_size = 4, linewidth = 1.5, position = position_dodge(0.05)) + 
  scale_fill_okabe_ito() +
  scale_x_continuous(limits = c(0, 1.4), breaks = seq(0, 2, 0.2)) +
  labs(title = 'Duration of diurnal activity', y = 'Variance component', x = bquote('Standard deviation [95%CrI]'~(hours)))

saveRDS(object = fig_2c, file = './plots/Figure_2c.RDS')

#####


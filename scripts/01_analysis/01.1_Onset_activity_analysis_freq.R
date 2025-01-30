###
###
#' Script for: Frequentist analysis
#' 
#' 
#' Last update 2024/10/16
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
               rstan,
               tidybayes,
               broom,
               broom.mixed,
               emmeans,
               ggdist,
               ggokabeito,   
               gghalves,     
               ggbeeswarm,
               gt,
               gtsummary,
               job)

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

summary(data$bcpa_onset_prop_data)
hist(data$bcpa_onset_prop_data)

cor.test(data$bcpa_onset_time, data$bbs_onset_activity_cp)

#####

##
##
##### number of days individuals were tracked #####
##
##
data %>% 
  group_by(ring_nuber, season, species, date) %>% 
  filter(row_number() == 1) %>% 
  summarise(days = n()) %>% 
  group_by(ring_nuber, season, species) %>% 
  summarise(n_days = n()) %>% 
  group_by(species) %>% 
  summarise(mean_n_days = mean(n_days),
            sd_n_days = sd(n_days))

#####




##
## 
##### Raw data visualisation #####
##
##

##
## list of birds to gather ringing information
#df_save <- data %>% 
#  group_by(ring_nuber, species) %>% 
#  filter(row_number() == 1) %>% 
#  dplyr::select(species, ring_nuber) %>% 
#  arrange(species, ring_nuber)

## save list
# write.csv(x = df_save, file = "birds_analysis.csv", row.names = F)

## 
## Initial visualisation of data

##
## summarised data set to plot means
df_summary <- data %>% 
  group_by(season_clean, habitat, species, location) %>% 
  summarise(mean_value = mean(relative_onset_bcpa, na.rm=T),
            sd_value = sd(relative_onset_bcpa,na.rm=T))

##
## raw data plot
figures4 <- ggplot(data = data %>% 
         mutate(species = factor(x = species, 
                                 levels =  c("Robin", "Blackbird",
                                             "Great tit", "Blue tit",
                                             "Dunnock", "Chaffinch")),
                season_clean = factor(x = season_clean, 
                                      levels =  c("Pre-breeding", 
                                                  "Post-breeding"))), 
       aes(x = species, 
           y = relative_onset_bcpa, 
           fill = location,
           color = location)) +
  facet_grid(season_clean~habitat) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.20,
             size = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'top',
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15, face = "bold"),
        axis.title = element_text("Arial", size = 15),
        axis.text.y = element_text("Arial", size = 10),
        axis.text.x = element_text("Arial", size = 10, angle = 45, vjust = 0.70)) +
  scale_y_continuous(breaks = -4:2, labels = -4:2) +
  scale_color_okabe_ito(order = c(5,8,7,6)) +
  scale_fill_okabe_ito(order = c(5,8,7,6)) +
  labs(x = "Species", y = "Relative onset of activity ± SD") +
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

#saveRDS(object = figures4a, file = './plots/Figure_S4a.RDS')

ggsave(filename = "./plots/Figure_S4.png", 
       plot = figures4, 
       units = "mm",
       device = "png", 
       width = 150,
       height = 150)




##
## density plot for relative onset of activity
fig1a <- ggplot(data = data %>% 
                  mutate(species = factor(x = species, 
                                          levels =  c("Robin", "Blackbird",
                                                      "Great tit", "Blue tit",
                                                      "Dunnock", "Chaffinch")),
                         season_clean = factor(x = season_clean, 
                                               levels =  c("Pre-breeding", 
                                                           "Post-breeding"))), 
                aes(x = relative_onset_bcpa, 
                    fill = habitat)) +
  facet_grid(species ~ season_clean) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text("Arial", size = 17, face = "bold"),
        strip.text.y = element_text("Arial", size = 12, face = "bold"),
        axis.title = element_text("Arial", size = 17),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text("Arial", size = 15, vjust = 0.70)) +
  scale_y_continuous(breaks = -4:4, labels = -4:4) +
  scale_x_continuous(limits = c(-4, 2)) +
  scale_fill_okabe_ito() +
  labs(x = "Relative onset of activity", y = ' ')

saveRDS(object = fig1a, file = './plots/Figure_1a.RDS')

#ggsave(filename = './plots/density_onset.png', 
#       plot = dens_onset, 
#       units = 'mm',
#       height = 250, 
#       width = 175)

##
## raw data absolute onset plot continuous date

# sunrise time per day to include in plot
df_sunrise <- data %>% 
  group_by(location, ydate) %>% 
  filter(row_number() == 1) %>% 
  select(ydate, location, sunrise, habitat) %>% 
  group_by(ydate) %>% 
  summarise(sunrise_plot = mean(sunrise))

data <- left_join(x = data, 
                  y = df_sunrise, 
                  by = c('ydate'))

fig_s3a <- ggplot(data = data %>% 
                    mutate(species = factor(x = species, 
                                            levels =  c("Robin", "Blackbird",
                                                        "Great tit", "Blue tit",
                                                        "Dunnock", "Chaffinch")),
                           season_clean = factor(x = season_clean, 
                                                 levels =  c("Pre-breeding", 
                                                             "Post-breeding"), ordered = T)), 
                  aes(x = ydate, 
                      y = bbs_onset_activity_cp, 
                      fill = habitat,
                      color = habitat)) +
  facet_grid(species~season_clean, scales = 'free') +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  geom_line(aes(x = ydate, y = sunrise_plot, fill = NULL, color = NULL),
            color = 'grey15',
            linetype = 'dashed',
            linewidth = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'top',
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text('Arial', size = 15),
        axis.title = element_text("Arial", size = 15),
        axis.text.y = element_text("Arial", size = 13),
        axis.text.x = element_text("Arial", size = 13, vjust = 0.70)) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Days after January 1", y = 'Onset of activity') 

saveRDS(object = fig_s3a, file = './plots/Figure_S3a.RDS')

#ggsave(filename = './plots/Figure S2a.png', 
#       plot = fig_s2a, 
#       units = 'mm',
#       height = 250, 
#       width = 150)

#####

##
## 
##### Data Summaries #####
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

# years
data %>% 
  group_by(ring_nuber, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()

# season
data %>% 
  group_by(ring_nuber, season_clean) %>% 
  filter(row_number() == 1) %>% 
  group_by(ring_nuber) %>% 
  summarise(Obs_per_ring = n()) %>% 
  filter(Obs_per_ring > 1) %>% 
  nrow()



##
## summary of sample sizes
table(data$species, data$habitat)
table({data %>% 
    group_by(ring_nuber) %>% 
    filter(row_number() == 1)}$species, 
    {data %>% 
        group_by(ring_nuber) %>% 
        filter(row_number() == 1)}$habitat)

## mean onset
data %>% 
  group_by(habitat) %>% 
  summarise(mean_onset = mean(relative_onset_bcpa), 
            sd_onset = sd(relative_onset_bcpa))

## dates of tracking
data %>% 
  mutate(yday_date = yday(date)) %>% 
  group_by(season_clean) %>% 
  summarise(min_date = min(yday_date), 
            max_date = max(yday_date),
            median_date = median(yday_date),
            mean_date = mean(yday_date)) %>% 
  mutate(min_date = as.Date(min_date),
         max_date = as.Date(max_date),
         median_date = as.Date(median_date),
         mean_date = as.Date(mean_date))

#####

##
##
##### Analysis of onset of activity using heterogeneous within and between individual variance across habitats #####
##
##

# read model output below - running the model takes a while

# model formula
#model_formula_onset_activity <- bf(relative_onset_bcpa ~ 
#                                     habitat : species +     
#                                     habitat : season_clean +
#                                     season_clean +
#                                     habitat + 
#                                     species +
#                                     year + 
#                                     min_temperature +
#                                     rainfall +
#                                     (1|date) +
#                                     (1|location) +
#                                     (1|gr(ring_nuber, by = habitat)),
#                                   sigma ~ 0 + habitat)
#
### fit model
#job(
#  {
#    model_onset_activity <- brm(model_formula_onset_activity, 
#                              data = data, 
#                              chains = 3, 
#                              cores = 3, 
#                              iter = 50000, 
#                              warmup = 20000, 
#                              thin = 10,
#                              seed = 25,
#                              silent = 2,
#                              backend = 'cmdstanr',
#                              file_refit = 'always',
#                              file = './models/01_onset_activity/brms_model_onset_activity_freq.RDS')
#  },
#  title = 'Onset of activity frequentist model'
#)

# read model back if needed (no need to fit the model every time the script is run)
model_onset_activity <- readRDS('./models/01_onset_activity/brms_model_onset_activity_freq.RDS')
print(summary(model_onset_activity), digits = 3)
plot(model_onset_activity) # looks fairly okay
max(rhat(model_onset_activity)) # looks fairly okay
pp_check(model_onset_activity) # looks fairly okay

## extract variance components
get_variables(model_onset_activity)

##
## table with model results - gtsummary, broom.mixed::tidy and tab_model don't seem to work with this model structure. I extract the table of fixed effects witht the code below, and adjust manually
table_model_onset_activity <- model_onset_activity %>%
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
gtsave(table_model_onset_activity, "./tables/Table S2 raw freq.html")

#####

##
##
##### Comparisons of posterior dist to test specific hypothesis #####
##
##

##
##  Q1: does the effect of urbanisation on diel activity depended on the species?
q1_comparisons <- model_onset_activity %>% 
  emmeans(~ habitat + species,
          at = list(year = mean(data$year),
                    min_temperature = mean(data$min_temperature),
                    rainfall = mean(data$rainfall)),
          epred = TRUE, 
          re_formula = NA) %>% 
  contrast(method = 'revpairwise', by = c('species')) 

# plot
figs6a <- q1_comparisons %>% 
  gather_emmeans_draws() %>% 
  mutate(species = factor(x = species, 
                          levels =  c("Robin", "Blackbird",
                                      "Great tit", "Blue tit",
                                      "Dunnock", "Chaffinch"))) %>% 
  ggplot(., 
         aes(x = .value, 
             y = species)) +
  #facet_grid(~season_clean) +
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
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(-1, 0, 1), limits = c(-1.5,1.5)) +
  labs(y = 'Species', x = expression(atop('Urban - Forest difference', 
                                          'relative onset of activity')))

saveRDS(object = figs6a, file = './plots/Figure_S6a.RDS')


##
##  Q2: does the effect of urbanisation on diel activity depended on the time of the year?
q2_comparisons <- model_onset_activity %>% 
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
figs6b <- q2_comparisons %>% 
  gather_emmeans_draws() %>% 
  mutate(season_clean = factor(x = season_clean, 
                          levels =  c("Pre-breeding", 
                                      "Post-breeding"))) %>% 
  ggplot(., 
         aes(x = .value, 
             y = season_clean)) +
  #facet_grid(~species) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(shape = 21, .width = 0.95, 
               point_size = 4, linewidth = 1.5,
               fill = 'grey90') + 
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(-1, 0, 1), limits = c(-1.5,1.5)) +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(y = 'Time of the year', x = expression(atop('Urban - Forest difference', 
                                          'relative onset of activity')))

saveRDS(object = figs6b, file = './plots/Figure_S6b.RDS')

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
                                    species = unique(data$species),
                                    season_clean = unique(data$season_clean)), 
              re_formula = NA) %>% 
  rename(relative_onset_bbs = '.epred') %>% 
  mutate(season_clean = factor(x = season_clean, 
                        levels =  c("Pre-breeding", 
                                    "Post-breeding"), ordered = T))

grand_mean_onset %>% 
  ungroup() %>% 
  select(-.row) %>% 
  pivot_wider(names_from = 'season_clean', 
              values_from = c(relative_onset_bbs)) %>% 
  mutate(season_effect = `Pre-breeding` - `Post-breeding`) %>% 
  group_by(species, habitat) %>% 
  reframe(median = median(season_effect),
          low = quantile(season_effect, probs = 0.025),
          high = quantile(season_effect, probs = 0.975)) 



# plot
fig_1b <- ggplot(data = data %>% 
         mutate(species = factor(x = species, 
                                 levels =  c("Robin", "Blackbird",
                                             "Great tit", "Blue tit",
                                             "Dunnock", "Chaffinch")),
                season_clean = factor(x = season_clean, 
                                      levels =  c("Pre-breeding", 
                                                  "Post-breeding"), ordered = T)), 
       aes(x = species, 
           y = relative_onset_bbs, 
           fill = habitat,
           color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(legend.position.inside = c(0.25, 0.085), 
        legend.position = 'inside',
        legend.direction = 'horizontal',
        legend.text = element_text("Arial", size = 14),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 20, face = "bold"),
        axis.title = element_text("Arial", size = 16),
        axis.text.y = element_text("Arial", size = 14),
        axis.text.x = element_text("Arial", size = 14, angle = 45, vjust = 0.70)) +
  scale_y_continuous(limits = c(-4,2), breaks = -4:2, labels = -4:2) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Relative onset of activity") +
  facet_wrap(~season_clean) +
  stat_pointinterval(data = grand_mean_onset, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') +
  geom_hline(yintercept = 0, linetype = 2)

saveRDS(object = fig_1b, file = './plots/Figure_1b.RDS')

#####

##
##
##### Calculate among individual variances in onset of activity #####
##
##
model_onset_activity %>% 
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
model_onset_activity %>% 
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
model_onset_activity %>% 
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
  summarise(median_total_forest = median(total_pheno_forest),
            median_total_urban = median(total_pheno_urban),
            median_among_forest = median((`sd_ring_nuber__Intercept:habitatForest`^2)),
            median_among_urban = median((`sd_ring_nuber__Intercept:habitatUrban`^2)))

##
## numbers from previous version (from Table S1 of the first version of the manuscript)
total_forest = 0.317^2 + 0.178^2 + 0.082^ 2 + (0.307^2)
total_urban = 0.474^2 + 0.178^2 + 0.082^ 2 + (0.438^2)

among_forest = 0.317^2
among_urban = 0.474^2

among_forest / total_forest
among_urban / total_urban


t(
  model_onset_activity %>% 
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
  mutate(rep_forest = (`sd_ring_nuber__Intercept:habitatForest`^2) / total_pheno_forest,
         rep_urban = (`sd_ring_nuber__Intercept:habitatUrban`^2) / total_pheno_urban) %>% 
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
) 

#####

##
##
##### Plot between and within ind variance estimates #####
##
##
# plot
fig_2a <- model_onset_activity %>% 
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
        legend.position.inside = c(0.85, 0.25),
        legend.position = 'inside',
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(aes(group = habitat, fill = habitat),
               shape = 21, .width = 0.95, slab_alpha = 0.5,
               point_size = 4, linewidth = 1.5, position = position_dodge(0.05)) + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_okabe_ito() +
  labs(title = 'Relative onset of activity', y = 'Variance component', x = bquote('Standard deviation [95%CrI]'~(hours)))

saveRDS(object = fig_2a, file = './plots/Figure_2a.RDS')


#####

##
##
##### Model including 3-way interaction #####
##
##

model_formula_onset_activity_3w <- bf(relative_onset_bcpa ~ 
                                        habitat : species : season_clean +     
                                        season_clean : species +     
                                        habitat : species +     
                                        habitat : season_clean +
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

## fit model
job(
  {
    model_onset_activity_3w <- brm(model_formula_onset_activity_3w, 
                              data = data, 
                              chains = 3, 
                              cores = 3, 
                              iter = 50000, 
                              warmup = 20000, 
                              thin = 10,
                              seed = 25,
                              silent = 2,
                              backend = 'cmdstanr',
                              file_refit = 'always',
                              file = './models/01_onset_activity/brms_model_onset_activity_freq_3way_interaction.RDS')
  },
  title = 'Onset of activity frequentist model, including 3-way interactions'
)

model_onset_activity_3w <- readRDS('./models/01_onset_activity/brms_model_onset_activity_freq_3way_interaction.RDS')


# add comparison criterium
model_onset_activity <- add_criterion(model_onset_activity, "loo")
loo(model_onset_activity)
model_onset_activity_3w <- add_criterion(model_onset_activity_3w, "loo")
loo(model_onset_activity_3w)


loo_compare(model_onset_activity, model_onset_activity_3w, criterion = c('waic'))
loo_compare(model_onset_activity, model_onset_activity_3w, criterion = c('loo'))

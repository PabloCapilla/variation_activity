###
###
#' 
#' Authors: Pablo Capilla-Lasheras
#' Script aim: NERC  Telemetry project - analysis of relative end of activity heterogeneous variances
#' 
#' Last update 2023/09/26
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
data <- readRDS(file = "./data/7_radio_end/prep_data_analysis_FILTERED_231211.RDS")
data$year <- as.numeric(data$year)
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
  scale_y_continuous(breaks = -4:2, labels = -4:2) +
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

ggsave(filename = './plots/relative_end_sex.png', 
       plot = sex_end, 
       units = 'mm',
       height = 150, 
       width = 150)

#####


#####

##
##
##### Analysis of end of activity using heterogeneous within and between individual variance across habitats #####
##
##

# model formula
model_formula_end_activity <- bf(relative_end_bcpa ~ 
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
model_end_activity <- brm(model_formula_end_activity, 
                          data = data, 
                          chains = 3, 
                          cores = 3, 
                          iter = 50000, 
                          warmup = 20000, 
                          thin = 10,
                          seed = 7, 
                          backend = 'cmdstanr',
                          file_refit = 'always',
                          file = './models/brms_model_end_activity.RDS')

# read model back if needed (no need to fit the model every time the script is run)
model_end_activity <- readRDS('./models/brms_model_end_activity.rds')
print(summary(model_end_activity), digits = 3)
plot(model_end_activity) # looks fairly okay
max(rhat(model_end_activity)) # looks fairly okay
pp_check(model_end_activity) # looks fairly okay

## extract variance components
get_variables(model_end_activity)

##
## table with model results - gtsummary, broom.mixed::tidy and tab_model don't seem to work with this model structure. I extract the table of fixed effects witht the code below, and adjust manually
table_model_end_activity <- model_end_activity %>%
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
gtsave(table_model_end_activity, "./tables/TABLE_end_activity.html")

#####

##
##
##### Comparisons of posterior dist to test specific hypothesis #####
##
##

##
##  Q1: does the effect of urbanisation on diel activity depended on the species?
q1_comparisons <- model_end_activity %>% 
  emmeans(~ habitat + species,
          at = list(year = mean(data$year),
                    min_temperature = mean(data$min_temperature),
                    rainfall = mean(data$rainfall)),
          epred = TRUE, 
          re_formula = NA) %>% 
  contrast(method = 'revpairwise', by = c('species')) 

# plot
fig_1e <- q1_comparisons %>% 
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
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(-1, 0, 1), limits = c(-1.5,1.5)) +
  labs(y = 'Species', x = expression(atop('Urban - Forest difference', 
                                          'relative end of activity')))

saveRDS(object = fig_1e, file = './plots/Figure_1e.RDS')

##
##  Q2: does the effect of urbanisation on diel activity depended on the time of the year?
q2_comparisons <- model_end_activity %>% 
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
fig_1f <- q2_comparisons %>% 
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
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(-1, 0, 1), limits = c(-1.5,1.5)) +
  labs(y = 'Time of the year', x = expression(atop('Urban - Forest difference', 
                                                   'relative end of activity')))

saveRDS(object = fig_1f, file = './plots/Figure_1f.RDS')

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
                                    species = unique(data$species),
                                    season_clean = unique(data$season_clean)), 
              re_formula = NA) %>% 
  rename(relative_end_bbs = '.epred')

# plot
fig_1d <- ggplot(data = data %>% 
                   mutate(species = factor(x = species, 
                                           levels =  c("Robin", "Blackbird",
                                                       "Great tit", "Blue tit",
                                                       "Dunnock", "Chaffinch")),
                          season_clean = factor(x = season_clean, 
                                                levels =  c("Pre-breeding", 
                                                            "Post-breeding"))), 
                 aes(x = species, 
                     y = relative_end_bbs, 
                     fill = habitat,
                     color = habitat)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  theme_bw() +
  theme(legend.position = 'none', 
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
  facet_wrap(~season_clean) +
  stat_pointinterval(data = grand_mean_end, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') +
  geom_hline(yintercept = 0, linetype = 2)

saveRDS(object = fig_1d, file = './plots/Figure_1d.RDS')

#####

##
##
##### Calculate among individual variances in end of activity #####
##
##
model_end_activity %>% 
  spread_draws(`sd_ring_nuber__Intercept:habitatForest`, 
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(`sd_ring_nuber__Intercept:habitatUrban` = `sd_ring_nuber__Intercept:habitatUrban`,
         `sd_ring_nuber__Intercept:habitatForest` = `sd_ring_nuber__Intercept:habitatForest`) %>% 
  mutate(difference_among = `sd_ring_nuber__Intercept:habitatUrban` - `sd_ring_nuber__Intercept:habitatForest`) %>% 
  median_hdi(difference_among)

#####

##
##
##### Calculate within individual variances in end of activity #####
##
##

## residual SD
model_end_activity %>% 
  gather_draws(b_sigma_habitatForest, 
               b_sigma_habitatUrban) %>% 
  mutate(.value = exp(.value)) %>% 
  median_hdi()

model_end_activity %>% 
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
model_end_activity %>% 
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
  summarise(total_pheno_forest = median(total_pheno_forest),
            total_pheno_urban = median(total_pheno_urban),
            among_forest = median(`sd_ring_nuber__Intercept:habitatForest`^2),
            among_urban = median(`sd_ring_nuber__Intercept:habitatUrban`^2),
            median_rep_forest = median(rep_forest),
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
fig_2b <- model_end_activity %>% 
  gather_draws(b_sigma_habitatForest, 
               b_sigma_habitatUrban,
               `sd_ring_nuber__Intercept:habitatForest`,
               `sd_ring_nuber__Intercept:habitatUrban`) %>% 
  mutate(habitat = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'sd_ring_nuber__Intercept:habitatForest', 
                          'Forest', 'Urban'),
         variance_type = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'b_sigma_habitatUrban', 
                                'Within', 'Between'), 
         .value = ifelse(.variable == 'b_sigma_habitatForest' | .variable == 'b_sigma_habitatUrban', exp(.value), .value)) %>% 
  ggplot(., aes(x = (.value)^2, 
                fill = habitat, 
                y = variance_type)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        legend.title = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)) +
  stat_halfeye(aes(group = habitat, fill = habitat),
               shape = 21, .width = 0.95, slab_alpha = 0.5,
               point_size = 4, linewidth = 1.5) + 
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  
  scale_fill_okabe_ito() +
  labs(title = 'Relative end of activity', y = 'Variance component', x = bquote('Standard deviation [95%CrI]'~(hours)))

saveRDS(object = fig_2b, file = './plots/Figure_2b.RDS')


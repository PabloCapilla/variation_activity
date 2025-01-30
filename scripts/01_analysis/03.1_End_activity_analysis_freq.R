###
###
#' Script for:
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

summary(data$bcpa_end_prop_data)

cor.test(data$bcpa_end_time, data$bbs_end_activity_cp)

#####

##
##
##### Initial raw data visualisation #####
##
##

##
## summarised data set to plot means
df_summary <- data %>% 
  group_by(season_clean, habitat, species, location) %>% 
  summarise(mean_value = mean(relative_end_bcpa, na.rm=T),
            sd_value = sd(relative_end_bcpa,na.rm=T))

##
## raw data plot
figures5 <- ggplot(data = data %>% 
                                   mutate(species = factor(x = species, 
                                                           levels =  c("Robin", "Blackbird",
                                                                       "Great tit", "Blue tit",
                                                                       "Dunnock", "Chaffinch")),
                                          season_clean = factor(x = season_clean, 
                                                                levels =  c("Pre-breeding", 
                                                                            "Post-breeding"))), 
                                 aes(x = species, 
                                     y = relative_end_bcpa, 
                                     fill = location,
                                     color = location)) +
  facet_grid(season_clean~habitat) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.20),
             alpha = 0.25,
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
  labs(x = "Species", y = "Relative end of activity ± SD") +
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

#saveRDS(object = figures4b, file = './plots/Figure_S4b.RDS')

ggsave(filename = './plots/Figure_S5.png', 
       plot = figures5, 
       units = 'mm',
       height = 150, 
       width = 150)

##
## density plot for relative onset of activity
fig1c <- ggplot(data = data %>% 
                  mutate(species = factor(x = species, 
                                          levels =  c("Robin", "Blackbird",
                                                      "Great tit", "Blue tit",
                                                      "Dunnock", "Chaffinch")),
                         season_clean = factor(x = season_clean, 
                                               levels =  c("Pre-breeding", 
                                                           "Post-breeding"))), 
                aes(x = relative_end_bcpa, 
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
  scale_x_continuous(limits = c(-3, 3)) +
  scale_fill_okabe_ito() +
  labs(x = "Relative end of activity", y = ' ')

saveRDS(object = fig1c, file = './plots/Figure_1c.RDS')

##
## raw data plot continuous date

# sunrise time per day
df_sunset <- data %>% 
  group_by(location, ydate) %>% 
  filter(row_number() == 1) %>% 
  select(ydate, location, sunset, habitat) %>% 
  group_by(ydate) %>% 
  summarise(sunset_plot = mean(sunset))

data <- left_join(x = data, 
                  y = df_sunset, 
                  by = c('ydate'))

fig_s3b <- ggplot(data = data %>% 
                    mutate(species = factor(x = species, 
                                            levels =  c("Robin", "Blackbird",
                                                        "Great tit", "Blue tit",
                                                        "Dunnock", "Chaffinch")),
                           season_clean = factor(x = season_clean, 
                                                 levels =  c("Pre-breeding", 
                                                             "Post-breeding"), ordered = T)), 
                  aes(x = ydate, 
                      y = bbs_end_activity_cp, 
                      fill = habitat,
                      color = habitat)) +
  facet_grid(species~season_clean, scales = 'free') +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, 
                                             jitter.width = 0.25),
             alpha = 0.25,
             size = 1.5) +
  geom_line(aes(x = ydate, y = sunset_plot, fill = NULL, color = NULL),
            color = 'grey15',
            linetype = 'dashed',
            size = 1.5) +
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
  labs(x = "Days after January 1", y = 'End of activity') 

saveRDS(object = fig_s3b, file = './plots/Figure_S3b.RDS')

#####      

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




# n individuals 
length(unique(data$ring_nuber))
length(unique(data$ring_nuber[data$habitat == 'Urban']))
length(unique(data$ring_nuber[data$habitat == 'Forest']))

##
## summary of sample sizes
table(data$species, data$habitat)
table({data %>% 
    group_by(ring_nuber) %>% 
    filter(row_number() == 1)}$species, 
    {data %>% 
        group_by(ring_nuber) %>% 
        filter(row_number() == 1)}$habitat)

## mean end
data %>% 
  group_by(habitat) %>% 
  summarise(mean_end = mean(relative_end_bcpa), 
            sd_end = sd(relative_end_bcpa))

## dates of tracking
data %>% 
  group_by(season, habitat) %>% 
  summarise(min_date = min(date), 
            max_date = max(date))

#####

##
##
##### Analysis of end of activity using heterogeneous within and between individual variance across habitats #####
##
##

# read model output below - running the model takes a while

# model formula
#model_formula_end_activity <- bf(relative_end_bcpa ~ 
#                                     habitat : species +     # Q1: whether the effect of urbanisation on diel activity depended on the species?
#                                     habitat : season_clean +# Q2: whether the effect of urbanisation on diel activity depended on the time of the year?
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
#    model_end_activity <- brm(model_formula_end_activity, 
#                              data = data, 
#                              chains = 3, 
#                              cores = 3, 
#                              iter = 50000, 
#                              warmup = 20000, 
#                              thin = 10,
#                              seed = 7, 
#                              backend = 'cmdstanr',
#                              file_refit = 'always',
#                              file = './models/02_end_activity/brms_model_end_activity_frequentist.RDS')
#    
#  },
#  title = 'End of activity frequentist model'
#)


# read model back if needed (no need to fit the model every time the script is run)
model_end_activity <- readRDS('./models/02_end_activity/brms_model_end_activity_frequentist.RDS')
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
gtsave(table_model_end_activity, "./tables/Table S3 raw freq.html")

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
fig_s6c <- q1_comparisons %>% 
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

saveRDS(object = fig_s6c, file = './plots/Figure_S6c.RDS')

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
fig_s6d <- q2_comparisons %>% 
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
saveRDS(object = fig_s6d, file = './plots/Figure_S6d.RDS')

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
  rename(relative_end_bbs = '.epred') %>% 
  mutate(season_clean = factor(x = season_clean, 
                               levels =  c("Pre-breeding", 
                                           "Post-breeding"), ordered = T))

# plot
grand_mean_end$filter_out <- ifelse(grand_mean_end$species == 'Blackbird' & 
                                      grand_mean_end$season_clean == 'Post-breeding' &
                                    grand_mean_end$habitat == 'Forest', 1, 0)
grand_mean_end <- grand_mean_end %>% 
  filter(filter_out == 0)

data$filter_out <- ifelse(data$species == 'Blackbird' & 
                            data$season_clean == 'Post-breeding' &
                            data$habitat == 'Forest', 1, 0)


fig_1d <- ggplot(data = data %>% 
                   filter(filter_out == 0) %>% 
                   mutate(species = factor(x = species, 
                                           levels =  c("Robin", "Blackbird",
                                                       "Great tit", "Blue tit",
                                                       "Dunnock", "Chaffinch")),
                          season_clean = factor(x = season_clean, 
                                                levels =  c("Pre-breeding", 
                                                            "Post-breeding"), ordered = T)), 
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
  scale_y_continuous(limits = c(-4,4), breaks = -4:4, labels = -4:4) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(x = "Species", y = "Relative end of activity") +
  facet_wrap(~season_clean) +
  stat_pointinterval(data = grand_mean_end,
                     .width = c(0.95),
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
t(
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
)
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
               point_size = 4, linewidth = 1.5, position = position_dodge(0.05)) + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  
  scale_fill_okabe_ito() +
  labs(title = 'Relative end of activity', y = 'Variance component', x = bquote('Standard deviation [95%CrI]'~(hours)))

saveRDS(object = fig_2b, file = './plots/Figure_2b.RDS')

#####

model_formula_end_activity_3w <- bf(relative_end_bcpa ~ 
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
job::job(
  {
    model_end_activity_3w <- brm(model_formula_end_activity_3w, 
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
                                   file = './models/02_end_activity/brms_model_end_activity_freq_3way_interaction.RDS')
  },
  title = 'End of activity frequentist model, including 3-way interactions'
)

model_end_activity_3w <- readRDS('./models/02_end_activity/brms_model_end_activity_freq_3way_interaction.RDS')
summary(model_end_activity_3w)

# add comparison criterium
job(
  {
    model_end_activity <- add_criterion(model_end_activity, "loo")
  },
  title = 'Loo calculation end of activity frequentist model not including 3-way interactions'
)

job(
  {
    model_end_activity_3w <- add_criterion(model_end_activity_3w, "loo")
  },
  title = 'Loo calculation end of activity frequentist model  including 3-way interactions'
  
)

# check loo calculations
loo(model_end_activity)
loo(model_onset_activity_3w)

# compare models
loo_compare(model_onset_activity, model_onset_activity_3w, criterion = c('waic'))
loo_compare(model_onset_activity, model_onset_activity_3w, criterion = c('loo'))

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
data <- readRDS(file = "./data/01_onset_data/prep_data_analysis_FILTERED_bayes.RDS")
data$year <- as.numeric(data$year)
data$ydate <- yday(data$date)
head(data)

summary(data$bcpa_onset_prop_data)
hist(data$bcpa_onset_prop_data)

#####

##
## 
##### Data Summaries #####
##
##

## (it should have already been cleaned in filtering script)
head(data)

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

## mean onset
data %>% 
  group_by(habitat) %>% 
  summarise(mean_onset = mean(relative_onset_bbs), 
            sd_onset = sd(relative_onset_bbs))

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
model_formula_onset_activity <- bf(relative_onset_bbs ~ 

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
    model_onset_activity <- brm(model_formula_onset_activity, 
                                data = data, 
                                chains = 3, 
                                cores = 3, 
                                iter = 50000, 
                                warmup = 20000, 
                                thin = 10,
                                seed = 25,
                                backend = 'cmdstanr',
                                file_refit = 'always',
                                file = './models/01_onset_activity/brms_model_onset_activity_Bayes.RDS')
  },
  title = 'Onset of activity Bayesian model'
)

# read model back if needed (no need to fit the model every time the script is run)
model_onset_activity <- readRDS('./models/01_onset_activity/brms_model_onset_activity_Bayes.RDS')
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
gtsave(table_model_onset_activity, "./tables/Table S2 raw Bayes.html")

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

# plot
fig_s8a <- ggplot(data = data %>% 
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
  theme(legend.position = c(0.25, 0.065), 
        legend.direction = 'horizontal',
        legend.text = element_text("Arial", size = 8),
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

saveRDS(object = fig_s8a, file = './plots/Figure_S8a.RDS')

#####

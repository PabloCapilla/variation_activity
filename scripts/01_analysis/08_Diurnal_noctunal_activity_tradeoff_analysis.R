###
###
#' Script for:
#' 
#' 
#' Last update 2023/12/21
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

# for night window
data$time_active_night <- (data$active_night*3)/60
data$time_non_active_night <- (data$nonactive_night*3)/60
data$proportion_active_night <- data$active_night/data$non_missing_obs_night

## keep day and night data with 50% of non-missing values
table(data$non_missing_obs_day/data$total_obs_day > 0.50)
table(data$non_missing_obs_night/data$total_obs_night > 0.50)

data$active_day <- ifelse(data$non_missing_obs_day/data$total_obs_day > 0.50, 
                          data$active_day, 
                          NA)
table(is.na(data$active_day))

data$active_night <- ifelse(data$non_missing_obs_night/data$total_obs_night > 0.50, 
                            data$active_night, 
                            NA)
table(is.na(data$active_night))

## only filter out completely rows where both day and night observations don't pass the 0.5 threshold
table(is.na(data$active_day), is.na(data$active_night))
data <- data %>% 
  filter(!(is.na(active_day) & is.na(active_night))) # 5871 observations

# check that NA + NA observations have been removed
table(is.na(data$active_day), is.na(data$active_night))

#####

##
##
##### Analysis of activity levels #####
##
##

# read model output below - running the model takes a while

## model for day
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
## model for the night
#bf_night_activity <- bf(active_night | trials(non_missing_obs_night) ~
#                          habitat : species +     # Q1: whether the effect of urbanisation on diel activity depended on the species?
#                          habitat : season_clean +# Q2: whether the effect of urbanisation on diel activity depended on the time of the year?
#                          season_clean +
#                          habitat + 
#                          species +
#                          year + 
#                          min_temperature +
#                          rainfall +
#                          (1|a|gr(obs_ID, by = habitat)) +
#                          (1|b|gr(ring_nuber, by = habitat)) +
#                          (1|e|date) +
#                          (1|f|location),
#                        zi ~ 1)
#
### bivariate model
#bivar_tradeoff <- brm(bf_day_activity + bf_night_activity + set_rescor(F), 
#                      data = data,
#                      family = zero_inflated_binomial(link = "logit", link_zi = "logit"),
#                      chains = 3, 
#                      cores = 3, 
#                      iter = 100000, 
#                      warmup = 25000, 
#                      thin = 50,
#                      seed = 1234,
#                      backend = 'cmdstanr', 
#                      file_refit = 'always',
#                      file = './models/brms_model_activity_level_tradeoff.RDS')
#summary(bivar_tradeoff)

# read model back if needed (no need to fit the model every time the script is run)
bivar_tradeoff <- readRDS('./models/04_level_activity/brms_model_activity_level_tradeoff.RDS')
print(summary(bivar_tradeoff), digits = 3)
plot(bivar_tradeoff) # looks fairly okay
max(rhat(bivar_tradeoff)) # looks fairly okay
pp_check(bivar_tradeoff) # looks fairly okay

## extract variance components
get_variables(model_activity_day)

##
## table with model results - gtsummary, broom.mixed::tidy and tab_model don't seem to work with this model structure. I extract the table of fixed effects witht the code below, and adjust manually
table_model_activity_tradeoff <- bivar_tradeoff %>%
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
gtsave(table_model_activity_tradeoff, "./tables/Table S7 raw.html")

#####

##
##
##### Calculate among individual correlations in diurnal and nocturnal activity #####
##
##
get_variables(bivar_tradeoff)

bivar_tradeoff %>% 
  gather_draws(`cor_ring_nuber__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_ring_nuber__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban`) %>% 
  median_hdi()

bivar_tradeoff %>% 
  spread_draws(`cor_ring_nuber__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_ring_nuber__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban`) %>% 
  mutate(cor_difference_among = `cor_ring_nuber__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban` - `cor_ring_nuber__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`) %>% 
  median_hdi(cor_difference_among)

#####

##
##
##### Calculate within individual correlations in diurnal and nocturnal activity #####
##
##
bivar_tradeoff %>% 
  gather_draws(`cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_obs_ID__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban`) %>% 
  median_hdi()

bivar_tradeoff %>% 
  spread_draws(`cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_obs_ID__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban`) %>% 
  mutate(cor_difference_within = `cor_obs_ID__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban` - 
           `cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`) %>% 
  median_hdi(cor_difference_within)

#####

##
##
##### Plot between and within ind correlation estimates #####
##
##
# plot
fig_2f <- bivar_tradeoff %>% 
  gather_draws(`cor_ring_nuber__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_ring_nuber__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban`,
               `cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest`, 
               `cor_obs_ID__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban` ) %>%
  mutate(habitat = ifelse(.variable == 'cor_ring_nuber__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest' | 
                            .variable == 'cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest', 
                          'Forest', 'Urban'),
         variance_type = ifelse(.variable == 'cor_obs_ID__activeday_Intercept:habitatForest__activenight_Intercept:habitatForest' | 
                                  .variable == 'cor_obs_ID__activeday_Intercept:habitatUrban__activenight_Intercept:habitatUrban', 
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
               position = position_dodge(width = 0.05),
               shape = 21, .width = 0.95, slab_alpha = 0.5,
               point_size = 4, linewidth = 1.5) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1,1,0.5)) +
  scale_fill_okabe_ito() +
  labs(title = 'Correlation in activity level', 
       y = 'Correlation component', 
       x = expression(atop('Correlation [95%CrI] between', 'diurnal and nocturnal activity level')))

saveRDS(object = fig_2f, file = './plots/Figure_2f.RDS')


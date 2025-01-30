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
data <- readRDS(file = "./data/02_end_data/prep_data_analysis_FILTERED_bayes.RDS")
data$year <- as.numeric(data$year)
data$ydate <- yday(data$date)
head(data)

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

## mean end
data %>% 
  group_by(habitat) %>% 
  summarise(mean_end = mean(relative_end_bbs), 
            sd_end = sd(relative_end_bbs))

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
model_formula_end_activity <- bf(relative_end_bbs ~ 
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

## fit model
job::job(
  {
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
                              file = './models/02_end_activity/brms_model_end_activity_Bayes.RDS')
    
  },
  title = 'End of activity Bayesian model'
)


# read model back if needed (no need to fit the model every time the script is run)
model_end_activity <- readRDS('./models/02_end_activity/brms_model_end_activity_Bayes.RDS')
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
gtsave(table_model_end_activity, "./tables/Table S3 raw Bayes.html")

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
fig_S8b <- ggplot(data = data %>% 
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
  stat_pointinterval(data = grand_mean_end, .width = c(0.95),
                     position = position_dodge(width = 0.75), 
                     linewidth = 1.5,
                     shape = 21,
                     size = 10,
                     color = 'black') +
  geom_hline(yintercept = 0, linetype = 2)

saveRDS(object = fig_S8b, file = './plots/Figure_S8b.RDS')

#####


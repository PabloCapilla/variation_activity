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
               cowplot,
               ggdist,
               ggokabeito,   
               gghalves,     
               ggbeeswarm)

#####

##
##
##### Read individual panels #####
##
##
fig_s8a <- readRDS('./plots/Figure_s8a.RDS')
fig_s8b <- readRDS('./plots/Figure_s8b.RDS')

#####

##
##
##### Panel Figure #####
##
##
Figures8 <- ggdraw() +
  draw_plot(fig_s8a, x = 0.00, y = 0.00, width = 0.48, height = 0.98) +
  draw_plot(fig_s8b, x = 0.50, y = 0.00, width = 0.48, height = 0.98) +
  draw_plot_label(label = c("a", "b"), 
                  size = 25,
                  x = c(0.00, 0.50), 
                  y = c(1.00, 1.00)) 

ggsave(filename = "./plots/Figure_S8.png", 
       plot = Figures8, 
       units = "mm",
       device = "png", 
       width = 250,
       height = 250)



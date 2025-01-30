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
fig_s11a <- readRDS('./plots/Figure_s11a.RDS')
fig_s11b <- readRDS('./plots/Figure_s11b.RDS')
fig_s11c <- readRDS('./plots/Figure_s11c.RDS')

#####

##
##
##### Panel Figure #####
##
##
Figures11 <- ggdraw() +
  draw_plot(fig_s11a, x = 0.00, y = 0.00, width = 0.55, height = 0.92) +
  draw_plot(fig_s11b, x = 0.60, y = 0.50, width = 0.40, height = 0.46) +
  draw_plot(fig_s11c, x = 0.60, y = 0.00, width = 0.40, height = 0.46) +
  draw_plot_label(label = c("a", "b", 'c'), 
                  size = 25,
                  x = c(0, 0.60, 0.60), 
                  y = c(1, 1, 0.55)) 

ggsave(filename = "./plots/Figure_S11.png", 
       plot = Figures11, 
       units = "mm",
       device = "png", 
       width = 250,
       height = 150)


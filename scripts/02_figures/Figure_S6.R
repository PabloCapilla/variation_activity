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
fig_s6a <- readRDS('./plots/Figure_S6a.RDS')
fig_s6b <- readRDS('./plots/Figure_S6b.RDS')
fig_s6c <- readRDS('./plots/Figure_S6c.RDS')

#####

##
##
##### Panel Figure #####
##
##
FigureS6 <- ggdraw() +
  draw_plot(fig_s6a, x = 0.00, y = 0.00, width = 0.55, height = 0.92) +
  draw_plot(fig_s6b, x = 0.60, y = 0.50, width = 0.40, height = 0.46) +
  draw_plot(fig_s6c, x = 0.60, y = 0.00, width = 0.40, height = 0.46) +
  draw_plot_label(label = c("a", "b", 'c'), 
                  size = 25,
                  x = c(0, 0.60, 0.60), 
                  y = c(1, 1, 0.55)) 

ggsave(filename = "./plots/Figure_S6.png", 
       plot = FigureS6, 
       units = "mm",
       device = "png", 
       width = 250,
       height = 150)


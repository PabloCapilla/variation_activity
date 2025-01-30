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
fig_s6a <- readRDS('./plots/Figure_s6a.RDS')
fig_s6b <- readRDS('./plots/Figure_s6b.RDS')
fig_s6c <- readRDS('./plots/Figure_s6c.RDS')
fig_s6d <- readRDS('./plots/Figure_s6d.RDS')

#####

##
##
##### Panel Figure #####
##
##
FigureS6 <- ggdraw() +
  draw_plot(fig_s6a, x = 0.00, y = 0.50, width = 0.48, height = 0.48) +
  draw_plot(fig_s6b, x = 0.50, y = 0.50, width = 0.48, height = 0.48) +
  draw_plot(fig_s6c, x = 0.00, y = 0.00, width = 0.48, height = 0.48) +
  draw_plot(fig_s6d, x = 0.50, y = 0.00, width = 0.48, height = 0.48) +
  draw_plot_label(label = c("a", "b", 'c', 'd'), 
                  size = 25,
                  x = c(0, 0.5, 0, 0.5), 
                  y = c(1, 1, 0.5, 0.5)) 

ggsave(filename = "./plots/Figure_S6.png", 
       plot = FigureS6, 
       units = "mm",
       device = "png", 
       width = 220,
       height = 220)


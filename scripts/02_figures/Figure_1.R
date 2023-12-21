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
fig_1a <- readRDS('./plots/Figure_1a.RDS')
fig_1b <- readRDS('./plots/Figure_1b.RDS')
fig_1c <- readRDS('./plots/Figure_1c.RDS')
fig_1d <- readRDS('./plots/Figure_1d.RDS')

#####

##
##
##### Panel Figure 1 #####
##
##
Figure1 <- ggdraw() +
  draw_plot(fig_1a, x = 0.00, y = 0.50, width = 0.38, height = 0.48) +
  draw_plot(fig_1b, x = 0.40, y = 0.50, width = 0.58, height = 0.48) +
  draw_plot(fig_1c, x = 0.00, y = 0.00, width = 0.38, height = 0.48) +
  draw_plot(fig_1d, x = 0.40, y = 0.00, width = 0.58, height = 0.48) +
  draw_plot_label(label = c("a", "b", 'c', "d"), 
                  size = 25,
                  x = c(0.00, 0.40, 0.00, 0.40), 
                  y = c(1.00, 1.00, 0.50, 0.50)) 

ggsave(filename = "./plots/Figure_1.png", 
       plot = Figure1, 
       units = "mm",
       device = "png", 
       width = 275,
       height = 320)



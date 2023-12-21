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
fig_s2a <- readRDS('./plots/Figure_S2a.RDS')
fig_s2b <- readRDS('./plots/Figure_S2b.RDS')

#####

##
##
##### Panel Figure #####
##
##
FigureS2 <- ggdraw() +
  draw_plot(fig_s2a, x = 0.00, y = 0.00, width = 0.48, height = 0.98) +
  draw_plot(fig_s2b, x = 0.50, y = 0.00, width = 0.48, height = 0.98) +
  draw_plot_label(label = c("a", "b"), 
                  size = 25,
                  x = c(0.00, 0.50), 
                  y = c(1.00, 1.00)) 

ggsave(filename = "./plots/Figure_S2.png", 
       plot = FigureS2, 
       units = "mm",
       device = "png", 
       width = 200,
       height = 250)



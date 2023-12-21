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
fig_2a <- readRDS('./plots/Figure_2a.RDS')
fig_2b <- readRDS('./plots/Figure_2b.RDS')
fig_2c <- readRDS('./plots/Figure_2c.RDS')
fig_2d <- readRDS('./plots/Figure_2d.RDS')
fig_2e <- readRDS('./plots/Figure_2e.RDS')
fig_2f <- readRDS('./plots/Figure_2f.RDS')



#####

##
##
##### Panel Figure 2 #####
##
##
Figure2 <- ggdraw() +
  draw_plot(fig_2a, x = 0, y = 0.66, width = 0.5, height = 0.30) +
  draw_plot(fig_2b, x = 0, y = 0.33, width = 0.5, height = 0.30) +
  draw_plot(fig_2c, x = 0, y = 0.00, width = 0.5, height = 0.30) +
  draw_plot(fig_2d, x = 0.5, y = 0.66, width = 0.5, height = 0.30) +  
  draw_plot(fig_2e, x = 0.5, y = 0.33, width = 0.5, height = 0.30) +
  draw_plot(fig_2f, x = 0.5, y = 0.00, width = 0.5, height = 0.30) +
  draw_plot_label(label = c("a", "b", 'c', 'd', 'e', 'f'), 
                  size = 25,
                  x = c(0, 0, 0, 0.5, 0.5, 0.5), 
                  y = c(1, 0.66, 0.33, 1, 0.66, 0.33)) 



ggsave(filename = "./plots/Figure_2.png", 
       plot = Figure2, 
       units = "mm",
       device = "png", 
       width = 250,
       height = 300)


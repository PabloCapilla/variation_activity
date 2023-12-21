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
fig_s3a <- readRDS('./plots/Figure_S3a.RDS')
fig_s3b <- readRDS('./plots/Figure_S3b.RDS')
fig_s3c <- readRDS('./plots/Figure_S3c.RDS')
fig_s3d <- readRDS('./plots/Figure_S3d.RDS')

#####

##
##
##### Panel Figure #####
##
##
FigureS3 <- ggdraw() +
  draw_plot(fig_s3a, x = 0.00, y = 0.50, width = 0.48, height = 0.48) +
  draw_plot(fig_s3b, x = 0.50, y = 0.50, width = 0.48, height = 0.48) +
  draw_plot(fig_s3c, x = 0.00, y = 0.00, width = 0.48, height = 0.48) +
  draw_plot(fig_s3d, x = 0.50, y = 0.00, width = 0.48, height = 0.48) +
  draw_plot_label(label = c("a", "b", 'c', 'd'), 
                  size = 25,
                  x = c(0, 0.5, 0, 0.5), 
                  y = c(1, 1, 0.5, 0.5)) 

ggsave(filename = "./plots/Figure_S3.png", 
       plot = FigureS3, 
       units = "mm",
       device = "png", 
       width = 220,
       height = 220)


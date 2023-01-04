# Load major packages

library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(mapview)
library(readxl)
library(lubridate)
library(stringr)
library(ggspatial)
library(gridExtra)
library(viridis)

library(ggpubr)
library(jtools)
library(ggfortify)
library(grid)
library(gridExtra)

# Additional packages

library(move)
library(ctmm)
library(ctmmweb)


# ------------------------------------------------------------------------------

# Function: linear_regression_plots
# Description: Make regression analysis plots and scatter plot (speed vs diffusion) per period per sex 

# Parameters:
# - t.obj = treatment site summary object (xlsx file exported from processing periods)
# - c.obj = control site summary object (xlsx file exported from processing periods)
# - export.folder = path to the folder to save the results (can be absolute or relative path)


linear_regression_plots <- function(t.obj = t.obj, c.obj = c.obj, export.folder = NA){
  
  t.obj %<>% replace(is.na(.), 0) 
  c.obj %<>% replace(is.na(.), 0)
  
  t.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean)
  
  t.obj %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean)
  
  c.obj %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  scc_plot_t <- ggscatter(t.obj, x = "speed_mean", y = "diffusion_mean",
                          color = "dodgerblue3", #alpha = "speed_mean",
                          add = "reg.line",
                          add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = t.obj, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.01, nudge_x = 0.2, size = 4, color = "dodgerblue3", fontface = "bold") + 
    scale_x_continuous(limits = c(min(c(t.obj$speed_mean, c.obj$speed_mean)), max(c(t.obj$speed_mean, c.obj$speed_mean))+0.5)) +
    scale_y_continuous(limits = c(min(c(t.obj$diffusion_mean, c.obj$diffusion_mean)), max(c(t.obj$diffusion_mean, c.obj$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = 'none')
  
  
  LRmodel_t <- lm(speed_mean ~ diffusion_mean, t.obj)
  
  # summ(LRmodel)
  # plot_summs(LRmodel, scale = TRUE, plot.distributions = TRUE)
  
  reg_plot_t <- autoplot(LRmodel_t, 
                         which = 1:6, 
                         colour = 'dodgerblue3',
                         smooth.colour = 'black', 
                         smooth.linetype = 'dashed',
                         ad.colour = 'blue',
                         label.size = 3, 
                         label.n = 5, 
                         label.colour = 'blue',
                         ncol = 2) + 
    labs(caption = "Linear regression - Staten Island population")
  
  scc_plot_c <- ggscatter(c.obj, x = "speed_mean", y = "diffusion_mean",
                          color = "red",
                          add = "reg.line",
                          add.params = list(color = "red", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = c.obj, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.01, nudge_x = 0.2, size = 4, color = "red", fontface = "bold") + 
    scale_x_continuous(limits = c(min(c(t.obj$speed_mean, c.obj$speed_mean)), max(c(t.obj$speed_mean, c.obj$speed_mean))+0.5)) +
    scale_y_continuous(limits = c(min(c(t.obj$diffusion_mean, c.obj$diffusion_mean)), max(c(t.obj$diffusion_mean, c.obj$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nRockefeller population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    theme(legend.position = 'none')
  
  
  LRmodel_c <- lm(speed_mean ~ diffusion_mean, c.obj)
  
  reg_plot_c <- autoplot(LRmodel_c, 
                         which = 1:6, 
                         colour = 'red',
                         smooth.colour = 'black', 
                         smooth.linetype = 'dashed',
                         ad.colour = 'red',
                         label.size = 3, 
                         label.n = 5, 
                         label.colour = 'red',
                         ncol = 2) + 
    labs(caption = "Linear regression - Rockefeller population")
  
  
  gg1 <- grid.arrange(scc_plot_t, scc_plot_c, ncol = 2)
  
  gg2 <- reg_plot_t + reg_plot_c
  
  gg2 <- grid.arrange(arrangeGrob(grobs = gg2@plots, ncol = 2))
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "scatter_plots_treatment_vs_control.jpg"),
           width = 35,
           height = 22,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
    ggsave(plot = gg2,
           filename = paste0(export.folder, "/", "regression_plots_treatment_vs_control.jpg"),
           width = 28,
           height = 55,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(list(scater_plot = gg1, reg_plot = gg2))
}

# ------------------------------------------------------------------------------

# Function: linear_regression_plots_per_period
# Description: Make scatter plot (speed vs diffusion) per period - joint plot of both males and females per site 

# Parameters:
# - t.obj.m = treatment site summary object - males (xlsx file exported from processing periods)
# - c.obj.m = control site summary object - males (xlsx file exported from processing periods)
# - t.obj.f = treatment site summary object - females (xlsx file exported from processing periods)
# - c.obj.f = control site summary object - females (xlsx file exported from processing periods)
# - export.folder = path to the folder to save the results (can be absolute or relative path)
# - period = specify period name ("Breeding", "Post Breeding" or "Baseline")

# Notes: 
# - scale_x_continuous and scale_y_continuous are commented - in this case for left and right plot x and y axes ranges - limits will be different

linear_regression_plots_per_period <- function(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, export.folder = NA, period = NA){
  
  t.obj.m %<>% replace(is.na(.), 0) 
  c.obj.m %<>% replace(is.na(.), 0)
  
  t.obj.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  t.obj.m %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.obj.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  c.obj.m %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  t.obj.m %<>% dplyr::mutate(Site = "treatment")
  c.obj.m %<>% dplyr::mutate(Site = "control")
  
  
  t.obj.f %<>% replace(is.na(.), 0) 
  c.obj.f %<>% replace(is.na(.), 0)
  
  t.obj.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  t.obj.f %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.obj.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  c.obj.f %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  t.obj.f %<>% dplyr::mutate(Site = "treatment")
  c.obj.f %<>% dplyr::mutate(Site = "control")
  
  # --------------------------------
  
  
  tc.obj.m <- rbind(t.obj.m, c.obj.m)
  tc.obj.f <- rbind(t.obj.f, c.obj.f)
  
  
  gg.m <- ggscatter(tc.obj.m, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = tc.obj.m, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean)) - 1.5, max(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean))+1.5)) +
    # scale_y_continuous(limits = c(min(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean)) - 0.3, max(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean))+0.3)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [MALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  
  
  gg.f <- ggscatter(tc.obj.f, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = tc.obj.f, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean)) - 1.5, max(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean))+1.5)) +
    # scale_y_continuous(limits = c(min(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean)) - 0.3, max(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean))+0.3)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [FEMALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  gg1 <- grid.arrange(gg.m, gg.f, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "lin_reg_treatment_vs_control_per_sex.jpg"),
           width = 40,
           height = 25,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(gg1)
  
}



# ------------------------------------------------------------------------------

# Function: linear_regression_plots_with_error_bars
# Description: Make regression analysis scatter plot with added error bars (speed vs diffusion) per period per sex 
#              Error bars are added from low and high values (as 95%) available from processing for each variable (in this case speed)  
# Parameters:
# - t.obj = treatment site summary object (xlsx file exported from processing periods)
# - c.obj = control site summary object (xlsx file exported from processing periods)
# - export.folder = path to the folder to save the results (can be absolute or relative path)

# Notes: 
# - scale_x_continuous and scale_y_continuous are commented - in this case for left and right plot x and y axes ranges will be different



linear_regression_plots_with_error_bars <- function(t.obj = t.obj, c.obj = c.obj, export.folder = NA){
  
  t.obj %<>% replace(is.na(.), 0) 
  c.obj %<>% replace(is.na(.), 0)
  
  t.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            # diffusion_low = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_low/100,
                            #                            diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_low),
                            # diffusion_high = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_high/100,
                            #                            diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_high),
                            
                            speed_mean = speed_mean,
                            speed_low = speed_low,
                            speed_high = speed_high)
  
  t.obj %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean,
                            speed_low = speed_low,
                            speed_high = speed_high)
  
  c.obj %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  scc_plot_t <- ggscatter(t.obj, x = "speed_mean", y = "diffusion_mean",
                          color = "dodgerblue3", #alpha = "speed_mean",
                          add = "reg.line",
                          add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.1, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = t.obj, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.05, nudge_x = 0.18, size = 4, color = "dodgerblue3", fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.obj$speed_mean, c.obj$speed_mean)) - 2, max(c(t.obj$speed_mean, c.obj$speed_mean))+2)) +
    #scale_y_continuous(limits = c(min(c(t.obj$diffusion_mean, c.obj$diffusion_mean)) - 0.5, max(c(t.obj$diffusion_mean, c.obj$diffusion_mean))+0.5)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = 'none')
  
  
  scc_plot_c <- ggscatter(c.obj, x = "speed_mean", y = "diffusion_mean",
                          color = "red",
                          add = "reg.line",
                          add.params = list(color = "red", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.1, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = c.obj, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.05, nudge_x = 0.18, size = 4, color = "red", fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(t.obj$speed_mean, c.obj$speed_mean)) - 2, max(c(t.obj$speed_mean, c.obj$speed_mean))+2)) +
    # scale_y_continuous(limits = c(min(c(t.obj$diffusion_mean, c.obj$diffusion_mean)) - 0.5, max(c(t.obj$diffusion_mean, c.obj$diffusion_mean))+0.5)) +
    labs(title = "Linear regression: speed ~ diffusion\nRockefeller population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    theme(legend.position = 'none')
  
  gg1 <- grid.arrange(scc_plot_t, scc_plot_c, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "scatter_plots_with_error_bars_treatment_vs_control_no_scale.jpg"),
           width = 85,
           height = 45,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(list(scater_plot = gg1))
}


# ------------------------------------------------------------------------------

# Function: linear_regression_plots_per_period_with_error_bars
# Description: Make scatter plot with added error bars (speed vs diffusion) per period - joint plot of both males and females per site 
#              Error bars are added from low and high values (as 95%) available from processing for each variable (in this case speed)  

# Parameters:
# - t.obj.m = treatment site summary object - males (xlsx file exported from processing periods)
# - c.obj.m = control site summary object - males (xlsx file exported from processing periods)
# - t.obj.f = treatment site summary object - females (xlsx file exported from processing periods)
# - c.obj.f = control site summary object - females (xlsx file exported from processing periods)
# - export.folder = path to the folder to save the results (can be absolute or relative path)
# - period = specify period name ("Breeding", "Post Breeding" or "Baseline")

# Notes: 
# - scale_x_continuous and scale_y_continuous are commented - in this case for left and right plot x and y axes ranges will be different


linear_regression_plots_per_period_with_error_bars <- function(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, export.folder = NA, period = NA){
  
  t.obj.m %<>% replace(is.na(.), 0) 
  c.obj.m %<>% replace(is.na(.), 0)
  
  t.obj.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  t.obj.m %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.obj.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  c.obj.m %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  t.obj.m %<>% dplyr::mutate(Site = "treatment")
  c.obj.m %<>% dplyr::mutate(Site = "control")
  
  
  t.obj.f %<>% replace(is.na(.), 0) 
  c.obj.f %<>% replace(is.na(.), 0)
  
  t.obj.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  t.obj.f %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.obj.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  c.obj.f %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  t.obj.f %<>% dplyr::mutate(Site = "treatment")
  c.obj.f %<>% dplyr::mutate(Site = "control")
  
  # --------------------------------
  
  
  tc.obj.m <- rbind(t.obj.m, c.obj.m)
  tc.obj.f <- rbind(t.obj.f, c.obj.f)
  
  
  gg.m <- ggscatter(tc.obj.m, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.025, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = tc.obj.m, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean)) - 1.5, max(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean))+1.5)) +
    # scale_y_continuous(limits = c(min(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean)) - 0.3, max(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean))+0.3)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [MALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  
  
  gg.f <- ggscatter(tc.obj.f, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.025, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = tc.obj.f, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean)) - 1.5, max(c(tc.obj.m$speed_mean, tc.obj.f$speed_mean))+1.5)) +
    # scale_y_continuous(limits = c(min(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean)) - 0.3, max(c(tc.obj.m$diffusion_mean, tc.obj.f$diffusion_mean))+0.3)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [FEMALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  gg1 <- grid.arrange(gg.m, gg.f, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "lin_reg_treatment_vs_control_with_error_bars_per_sex.jpg"),
           width = 85,
           height = 45,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(gg1)
  
}


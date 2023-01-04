# Author: Petar Bursac
# Date: 4 January 2023
# This script is used to make linear regression model between speed and diffusion values of individuals per period per site per sex;
# Functions are available in separate script called: Linear_regression_functions.R and final products are plots (a lot of different plots);

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
library(writexl)
library(gridExtra)
library(viridis)

library(ggpubr)
library(jtools)
library(ggfortify)
library(grid)
library(gridExtra)

# Movement packages

library(move)
library(ctmm)
library(ctmmweb)

wdir <- "Analysis/Linear_regression/" # set the path to the working directory (can be relative or absolute path)
setwd(wdir) # set working directory

source("Linear_regression_functions.R") # call the R script with functions (or execute that script)


# Notes:

# t.obj - treatment site object with summary values per treatment site obtained from processing periods
# c.obj - control site object with summary values per control site obtained from processing periods

# ------------------------------------------------------------------------------



# Example
# ------------------------------------------------------------------------------

## The code from this example is embedded in functions, I've left it here for you to look at
## how to run linear regression


# First read summary tables with available values for (low, est and high) speed and diffusion
## This example is for Breeding/Male

t.obj <- readxl::read_xlsx("Breeding/Male/treatment.site_summary.xlsx") %>% # Read xlsx file 
  as.data.frame() # and make dataframe instead of tibble

c.obj <- readxl::read_xlsx("Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame() 

t.obj %<>% replace(is.na(.), 0) # replace all NAs with zeros
c.obj %<>% replace(is.na(.), 0) # replace all NAs with zeros

t.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100, # transform all diffusion values to be in the same units 
                                                    diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                         
                         speed_mean = speed_mean) # speed values are all in the same units

t.obj %<>% dplyr::select(ind.id, speed_mean, diffusion_mean) # select only columns of interest

c.obj %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100, # transform all diffusion values to be in the same units 
                                                    diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                         
                         speed_mean = speed_mean) # speed values are all in the same units

c.obj %<>% dplyr::select(ind.id, speed_mean, diffusion_mean) # select only columns of interest

# Take a look at treatment and control objects
t.obj
c.obj

# Linear regression - treatment site

LRmodel_t <- lm(speed_mean ~ diffusion_mean, t.obj) # run linear regression model

summary(LRmodel_t) # take a look at model
summ(LRmodel_t) # summary
plot_summs(LRmodel_t, scale = TRUE, plot.distributions = TRUE) # distributions

reg_plot_t <- autoplot(LRmodel_t, # take a look at plots and statistics
                       which = 1:6, # some parameters to make nice plots 
                       colour = 'dodgerblue3',
                       smooth.colour = 'black', 
                       smooth.linetype = 'dashed',
                       ad.colour = 'blue',
                       label.size = 3, 
                       label.n = 5, 
                       label.colour = 'blue',
                       ncol = 2) + 
  labs(caption = "Linear regression - Staten Island population") # add caption

reg_plot_t 


# Linear regression - control site

LRmodel_c <- lm(speed_mean ~ diffusion_mean, c.obj) # run linear regression model

summary(LRmodel_c) # take a look at model
summ(LRmodel_c) # summary
plot_summs(LRmodel_c, scale = TRUE, plot.distributions = TRUE) # distributions

reg_plot_c <- autoplot(LRmodel_c,  # take a look at plots and statistics
                       which = 1:6, # some parameters to make nice plots 
                       colour = 'red',
                       smooth.colour = 'black', 
                       smooth.linetype = 'dashed',
                       ad.colour = 'red',
                       label.size = 3, 
                       label.n = 5, 
                       label.colour = 'red',
                       ncol = 2) + 
  labs(caption = "Linear regression - Rockefeller population") # add caption

reg_plot_c

# ------------------------------------------------------------------------------
## end of example



# Linear regression speed vs diffusion
# ------------------------------------------------------------------------------


# Code and functions below are made to produce linear regression plots for all periods per sex
# It is separated per period and sex



# Breeding - Males
# ------------------------------------------------------------------------------
 
# First read summary tables with available values for (low, est and high) speed and diffusion

t.obj <- readxl::read_xlsx("Breeding/Male/treatment.site_summary.xlsx") %>% # Read xlsx file 
  as.data.frame() # and make dataframe instead of tibble

c.obj <- readxl::read_xlsx("Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame() 

# Take a look at dataframes 
t.obj

c.obj

# Linear regression and scatter plot without error bars
# -------------

# Without saving results
lin.breeding.males <- linear_regression_plots(t.obj = t.obj, # define treatment object
                        c.obj = c.obj, # define control object
                        export.folder = NA) 

plot(lin.breeding.males$scater_plot) # take a look at scatter plot
plot(lin.breeding.males$reg_plot) # take a look at regression analysis plots


# With saving results - export folder is defined
linear_regression_plots(t.obj = t.obj, # define treatment object
                        c.obj = c.obj, # define control object
                        export.folder = "Breeding/Male/") # define export folder to store the plot/results (can be relative or absolute path)

# now take a look at exported files in specified folder


# Linear regression and scatter plot with error bars
# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj,
                                        c.obj = c.obj, 
                                        export.folder = "Breeding/Male/")

# Now you can take a look at new plots exported in the defined folder
# Also if you want to take a look how it is made, go to the script Linear_regression_functions.R
# and take a look at function <linear_regression_plots_with_error_bars>





# -------------

# Then the procedure is the same, 
# objects are defined for each of the periods per sex, 
# the function is called 
# and the results are saved in the defined folder

# -------------



# Breeding - Females
# ------------------------------------------------------------------------------

t.obj <- readxl::read_xlsx("Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj <- readxl::read_xlsx("Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


linear_regression_plots(t.obj = t.obj,
                        c.obj = c.obj,
                        export.folder = "Breeding/Female/")

# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj,
                                        c.obj = c.obj, 
                                        export.folder = "Breeding/Female/")


# Post Breeding - Males
# ------------------------------------------------------------------------------

t.obj <- readxl::read_xlsx("Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj <- readxl::read_xlsx("Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()


linear_regression_plots(t.obj = t.obj,
                        c.obj = c.obj,
                        export.folder = "Post Breeding/Male/")

# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj,
                                        c.obj = c.obj, 
                                        export.folder = "Post Breeding/Male/")


# Post Breeding - Females
# ------------------------------------------------------------------------------

t.obj <- readxl::read_xlsx("Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj <- readxl::read_xlsx("Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


linear_regression_plots(t.obj = t.obj,
                        c.obj = c.obj,
                        export.folder = "Post Breeding/Female/")

# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj,
                                        c.obj = c.obj, 
                                        export.folder = "Post Breeding/Female/")



# Baseline - Males
# ------------------------------------------------------------------------------

t.obj <- readxl::read_xlsx("Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj <- readxl::read_xlsx("Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()


linear_regression_plots(t.obj = t.obj,
                        c.obj = c.obj,
                        export.folder = "Baseline/Male/")

# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj,
                                        c.obj = c.obj, 
                                        export.folder = "Baseline/Male/")



# Baseline - Females
# ------------------------------------------------------------------------------

t.obj <- readxl::read_xlsx("Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj <- readxl::read_xlsx("Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


linear_regression_plots(t.obj = t.obj,
                        c.obj = c.obj,
                        export.folder = "Baseline/Female/")

# -------------

linear_regression_plots_with_error_bars(t.obj = t.obj, 
                                        c.obj = c.obj, 
                                        export.folder = "Baseline/Female/")



# ------------------------------------------------------------------------------




# Here we made joint plots per period for both males and females
# First part is without error bars and second with error bars added


# Linear regression - males vs females
# ------------------------------------------------------------------------------

# Without error bars

# Breeding

# Define and load treatment and control summary tables for both, males and females

t.obj.m <- readxl::read_xlsx("Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

# call the function which will made joint plot per period - define period and define export folder
g1 <- linear_regression_plots_per_period(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Breeding", export.folder = "Breeding/")


# Post Breeding

t.obj.m <- readxl::read_xlsx("Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g2 <- linear_regression_plots_per_period(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Post Breeding", export.folder = "Post Breeding/")


# Baseline

t.obj.m <- readxl::read_xlsx("Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g3 <- linear_regression_plots_per_period(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Baseline", export.folder = "Baseline/")

# Extra - make joint plot for all of three periods

gr1 <- grid.arrange(g1, g2, g3, ncol = 1)

# Save the joint plot of all 

ggsave(plot = gr1,
       filename = "lin_reg_treatment_vs_control_per_period_per_sex.jpg",
       width = 28,
       height = 50,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# ------------------------------------------------------------------------------


# With error bars - it is the same as previous but with added error bars

# Breeding

t.obj.m <- readxl::read_xlsx("Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g1 <- linear_regression_plots_per_period_with_error_bars(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Breeding", export.folder = "Breeding/")


# Post Breeding

t.obj.m <- readxl::read_xlsx("Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g2 <- linear_regression_plots_per_period_with_error_bars(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Post Breeding", export.folder = "Post Breeding/")


# Baseline

t.obj.m <- readxl::read_xlsx("Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.m <- readxl::read_xlsx("Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.obj.f <- readxl::read_xlsx("Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.obj.f <- readxl::read_xlsx("Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g3 <- linear_regression_plots_per_period_with_error_bars(t.obj.m = t.obj.m, c.obj.m = c.obj.m, t.obj.f = t.obj.f, c.obj.f = c.obj.f, period = "Baseline", export.folder = "Baseline/")


# Extra - make joint plot for all of three periods

gr1 <- grid.arrange(g1, g2, g3, ncol = 1)

# Save the joint plot of all 

ggsave(plot = gr1,
       filename = "lin_reg_treatment_vs_control_with_error_bars_per_period_per_sex.jpg",
       width = 45,
       height = 90,
       units = "cm",
       device = "jpeg",
       dpi = 700)





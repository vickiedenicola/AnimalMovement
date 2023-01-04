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

source("R_scripts/functions.R")

# new period are starting from line 335



# Pre Breeding - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# The main results are stored in RDS 
period.result <- readRDS("Results/Pre Breeding/Male/Pre Breeding_M_period_results.RDS")
period.result <- readRDS("Results/NEW_periods/Breeding/Female/Breeding_F_period_results.RDS")

# It contains two lists
period.result$treatment
period.result$control

# Both lists have 5 objects (map object must be removed)
period.result$treatment$telemetry.objects
period.result$treatment$akde.objects
period.result$treatment$fit.objects
period.result$treatment$speed.objects
period.result$treatment$variograms
# and also the same for control site

period.result$control$telemetry.objects
period.result$control$akde.objects
period.result$control$fit.objects
period.result$control$speed.objects
period.result$control$variograms

# names - individuals 
names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

# and if you want results from lets say individaul "1074" it is in every object - list first element in that list i = 1
# Example
plot(period.result$treatment$telemetry.objects[[1]]) # deer 1074
plot(period.result$treatment$variograms[[1]]) # deer 1074
plot(period.result$treatment$akde.objects[[1]]) # deer 1074
summary(period.result$treatment$akde.objects[[1]]) # deer 1074
summary(period.result$treatment$fit.objects[[1]]) # deer 1074
period.result$treatment$speed.objects[[1]] # deer 1074

# and this can be done for every individual from control or treatment site 
# (just need to be sure that you specify correct i for that individual in that list)


# Summary tables
# ------------------------------------------------------------------------------

summ.tables <- period.summary(period_result = period.result, export.folder = NA)

# treatment
summ.tables$treatment$summary
summ.tables$treatment$akde
summ.tables$treatment$speed.dist

# control
summ.tables$control$summary
summ.tables$control$akde
summ.tables$control$speed.dist

# now with export to folder (you need absolute path)
period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Male")

# Meta analysis
# ------------------------------------------------------------------------------

# Home range areas - plots

t.akde <- period.result$treatment$akde.objects
# png("Results/Pre Breeding/Male/Rplot_treatment_AKDE.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
# png("Results/Pre Breeding/Male/Rplot_control_AKDE.png", width = 25, height = 30, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()


# META - home range area
# png("Results/Pre Breeding/Male/Rplot_META_results_AKDE.png", width = 25, height = 18, units='cm', res = 600)

par(mfrow = c(1, 2))

ctmm::meta(period.result$control$akde.objects, 
           sort = TRUE, 
           col = viridis::viridis(length(period.result$control$akde.objects))) + 
  title("AKDE Home range area\nRockefeller population (control site)", sub = "Pre Breeding period - MALE population") 

ctmm::meta(period.result$treatment$akde.objects, 
           sort = TRUE, 
           col = viridis::viridis(length(period.result$treatment$akde.objects))) +
  title("AKDE Home range area\nStaten Island population (treatment site)", sub = "Pre Breeding period - MALE population")

dev.off()

par(mfrow = c(1, 1))

# META - home range area - all in one

# png("Results/Pre Breeding/Male/Rplot_META_results_AKDE_all.png", width = 25, height = 18, units='cm', res = 600)
par(mfrow = c(1, 1))
ctmm::meta(c(period.result$control$akde.objects, period.result$treatment$akde.objects), 
           sort = TRUE, 
           col = viridis::viridis(length(c(period.result$control$akde.objects, period.result$treatment$akde.objects)))) 
dev.off()


# Plots

#png("Results/Pre Breeding/Male/Rplot_maps_AKDE.png", width = 25, height = 18, units='cm', res = 600)

par(mfrow = c(1, 2))

plot(period.result$control$akde.objects, 
     col.DF = viridis::viridis(length(period.result$control$akde.objects)), 
     col.level = viridis::viridis(length(period.result$control$akde.objects)), 
     col.grid = NA, 
     level = NA, 
     labels = names(period.result$control$akde.objects)) +
  title("AKDE Home range area\nRockefeller population (control site)", sub = "Pre Breeding period - MALE population") 

plot(period.result$treatment$akde.objects, 
     col.DF = viridis::viridis(length(period.result$treatment$akde.objects)), 
     col.level = viridis::viridis(length(period.result$treatment$akde.objects)), 
     col.grid = NA, 
     level = NA, 
     labels = names(period.result$treatment$akde.objects)) +
  title("AKDE Home range area\nStaten Island population (treatment site)", sub = "Pre Breeding period - MALE population")


dev.off()

par(mfrow = c(1, 1))





# And all of this with function:

period.meta.plots(period_result = period.result, period = "Pre Breeding", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Male")





# Pre Breeding - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/Pre Breeding/Female/Pre Breeding_F_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Female")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Pre Breeding", male_female = "Female", export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Female/")




# Breeding - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/Breeding/Male/Breeding_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Breeding", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Male/")



# Breeding - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/Breeding/Female/Breeding_F_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Female")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Breeding", male_female = "Female", export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Female/")




# Post Breeding - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/Post Breeding/Male/Post Breeding_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Post Breeding/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Post Breeding", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/Post Breeding/Male/")


# Post Breeding - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Post Breeding", male_female = "Female", export.folder = "")





# Baseline - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/Baseline/Male/Baseline_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/Baseline/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Baseline", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/Baseline/Male")




# Baseline - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Baseline", male_female = "Female", export.folder = "")










# NEW PERIODS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------



# Breeding - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Male/Breeding_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Breeding", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Male/")



# Breeding - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Female/Breeding_F_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Female")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Breeding", male_female = "Female", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Female/")




# Post Breeding - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Male/Post Breeding_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Post Breeding", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Male/")


# Post Breeding - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Female/Post Breeding_F_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Female")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Post Breeding", male_female = "Female", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Female")





# Baseline - Males
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Male/Baseline_M_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Male")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Baseline", male_female = "Male", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Male")




# Baseline - Females
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Female/Baseline_F_period_results.RDS")

# Summary tables
# ------------------------------------------------------------------------------

period.summary(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Female")

# when tables are exported it needs to be checked - summary tables and if needed move some values from rows to the right position (where speed is not available)

# Meta analysis
# ------------------------------------------------------------------------------

period.meta.plots(period_result = period.result, period = "Baseline", male_female = "Female", export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Female")






# New periods - semivariograms per individual and AKDE plots
# ------------------------------------------------------------------------------

# Breeding - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Male/Breeding_M_period_results.RDS")

png("Results/NEW_periods/Breeding/Male/variograms_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Breeding/Male/variograms_control.png", width = 25, height = 40, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()


t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Breeding/Male/AKDE_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Breeding/Male/AKDE_control.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()





# Breeding - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Female/Breeding_F_period_results.RDS")
names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

png("Results/NEW_periods/Breeding/Female/variograms_treatment.png", width = 25, height = 70, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Breeding/Female/variograms_control.png", width = 25, height = 70, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()


t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Breeding/Female/AKDE_treatment.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Breeding/Female/AKDE_control.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()



# Post Breeding - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Male/Post Breeding_M_period_results.RDS")

names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

png("Results/NEW_periods/Post Breeding/Male/variograms_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Post Breeding/Male/variograms_control.png", width = 25, height = 40, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()



t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Post Breeding/Male/AKDE_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Post Breeding/Male/AKDE_control.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()



# Post Breeding - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Female/Post Breeding_F_period_results.RDS")

names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

png("Results/NEW_periods/Post Breeding/Female/variograms_treatment.png", width = 25, height = 70, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Post Breeding/Female/variograms_control.png", width = 25, height = 60, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()

t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Post Breeding/Female/AKDE_treatment.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Post Breeding/Female/AKDE_control.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()



# Baseline - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Male/Baseline_M_period_results.RDS")

names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

png("Results/NEW_periods/Baseline/Male/variograms_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Baseline/Male/variograms_control.png", width = 25, height = 40, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()


t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Baseline/Male/AKDE_treatment.png", width = 25, height = 50, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Baseline/Male/AKDE_control.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()




# Baseline - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Female/Baseline_F_period_results.RDS")

names(period.result$treatment$telemetry.objects)
names(period.result$control$telemetry.objects)

png("Results/NEW_periods/Baseline/Female/variograms_treatment.png", width = 25, height = 70, units='cm', res = 600)
plot_vario(period.result$treatment$variograms, period.result$treatment$fit.objects)
dev.off()

png("Results/NEW_periods/Baseline/Female/variograms_control.png", width = 25, height = 60, units='cm', res = 600)
plot_vario(period.result$control$variograms, period.result$control$fit.objects)
dev.off()



t.akde <- period.result$treatment$akde.objects
png( "Results/NEW_periods/Baseline/Female/AKDE_treatment.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$treatment$telemetry.objects) 
dev.off()

c.akde <- period.result$control$akde.objects
png("Results/NEW_periods/Baseline/Female/AKDE_control.png", width = 35, height = 90, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$control$telemetry.objects) 
dev.off()












# New periods - META analysis plots per individual - area, speed, distance and diffusion 
# (treatment vs control site)
# ------------------------------------------------------------------------------




# Breeding - Males
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Breeding/Male/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Breeding/Male/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Breeding/Male/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Breeding/Male/", 
                   plot.width = 45, 
                   plot.height = 25)


# Breeding - Females
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Breeding/Female/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Breeding/Female/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Breeding/Female/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                    c.akde = c.akde, 
                    export.folder = "Results/NEW_periods/Breeding/Female/", 
                    plot.width = 45, 
                    plot.height = 25)


# Post Breeding - Males
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Post Breeding/Male/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Post Breeding/Male/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Post Breeding/Male/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                    c.akde = c.akde, 
                    export.folder = "Results/NEW_periods/Post Breeding/Male/", 
                    plot.width = 45, 
                    plot.height = 25)


# Post Breeding - Females
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Post Breeding/Female/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Post Breeding/Female/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Post Breeding/Female/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                    c.akde = c.akde, 
                    export.folder = "Results/NEW_periods/Post Breeding/Female/", 
                    plot.width = 45, 
                    plot.height = 25)


# Baseline - Males
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Baseline/Male/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Baseline/Male/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Baseline/Male/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                    c.akde = c.akde, 
                    export.folder = "Results/NEW_periods/Baseline/Male/", 
                    plot.width = 45, 
                    plot.height = 25)



# Baseline - Females
# ------------------------------------------------------------------------------

# AKDE area plot
t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_akde.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_akde.xlsx") %>% 
  as.data.frame()

akde_meta_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Baseline/Female/",
               plot.width = 45, 
               plot.height = 25)

# Speed plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

speed_meta_plot(t.akde = t.akde, 
                c.akde = c.akde, 
                export.folder = "Results/NEW_periods/Baseline/Female/", 
                plot.width = 45, 
                plot.height = 25)


# Distance plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

distance_meta_plot(t.akde = t.akde, 
                   c.akde = c.akde, 
                   export.folder = "Results/NEW_periods/Baseline/Female/", 
                   plot.width = 45, 
                   plot.height = 25)


# Diffusion plot

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

diffusion_meta_plot(t.akde = t.akde, 
                    c.akde = c.akde, 
                    export.folder = "Results/NEW_periods/Baseline/Female/", 
                    plot.width = 45, 
                    plot.height = 25)




# TODO meta analysis - DONE
# TODO meta analysis plots - DONE
# TODO corellation (area, speed, diffusion, tau position, tau velocity) - DONE
# TODO results for akde/diffusion animals + new ndays plot - DONE
# TODO annotate code - Data exploration and Error model files
# TODO push to github - DONE



# meta employs a custom χ^2-IG hierarchical model to calculate debiased population mean estimates 
# of positive scale parameters, including home-range areas, diffusion rates, mean speeds, 
# and autocorrelation timescales. Population coefficient of variation (CoV) estimates are also provided.


# Breeding - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Male/Breeding_M_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Breeding/Male")

# Breeding - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Breeding/Female/Breeding_F_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Breeding/Female")

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = NA)



# Post Breeding - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Male/Post Breeding_M_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Post Breeding/Male")

# Post Breeding - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Post Breeding/Female/Post Breeding_F_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Post Breeding/Female")




# Baseline - Males
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Male/Baseline_M_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Baseline/Male")

# Baseline - Females
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Baseline/Female/Baseline_F_period_results.RDS")

t.speed <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_speed_dist.xlsx") %>% 
  as.data.frame()

c.speed <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_speed_dist.xlsx") %>% 
  as.data.frame()

meta_analysis(period.result = period.result, t.speed = t.speed, c.speed = c.speed, export.folder = "Results/NEW_periods/Baseline/Female")






# All in one
# ------------------------------------------------------------------------------

# Area meta analysis

meta.area <- readxl::read_xlsx("Results/NEW_periods/Meta_analysis.xlsx", sheet = "area_all") %>%
  as.data.frame()

meta.area %<>% dplyr::rename(Period = 1)

meta.area.t <- meta.area %>% dplyr::filter(Site == "Staten Island")
meta.area.c <- meta.area %>% dplyr::filter(Site == "Rockefeller")


g1 <- ggplot(meta.area.t) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.area.t, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 80, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$diffusion_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "E", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.area$high)+12)) +
  ggtitle("Meta analysis - population-level mean AKDE home range area\nStaten Island population") +
  xlab("95% Area [hm²]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)

g2 <- ggplot(meta.area.c) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.area.c, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 80, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$diffusion_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "E", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.area$high)+12)) +
  ggtitle("Meta analysis - population-level mean AKDE home range area\nRockefeller population") +
  xlab("95% Area [hm²]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)


gr1 <- grid.arrange(g1, g2, ncol = 1)

ggsave(plot = gr1,
       filename = "Results/NEW_periods/meta_analysis_mean_home_range_area.jpg",
       width = 40,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# Diffusion meta analysis

meta.diffusion <- readxl::read_xlsx("Results/NEW_periods/Meta_analysis.xlsx", sheet = "diffusion_all") %>%
  as.data.frame()

meta.diffusion %<>% dplyr::rename(Period = 1)

meta.diffusion.t <- meta.diffusion %>% dplyr::filter(Site == "Staten Island")
meta.diffusion.c <- meta.diffusion %>% dplyr::filter(Site == "Rockefeller")


g1 <- ggplot(meta.diffusion.t) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.diffusion.t, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 12, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$diffusion_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "A", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.diffusion$high)+12)) +
  ggtitle("Meta analysis - debiased population mean estimates of Diffusion rates\nStaten Island population") +
  xlab("95% Diffusion [hm²/day]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)

g2 <- ggplot(meta.diffusion.c) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.diffusion.c, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 12, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$diffusion_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "A", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.diffusion$high)+12)) +
  ggtitle("Meta analysis - debiased population mean estimates of Diffusion rates\nRockefeller population") +
  xlab("95% Diffusion [hm²/day]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)


gr1 <- grid.arrange(g1, g2, ncol = 1)

ggsave(plot = gr1,
       filename = "Results/NEW_periods/meta_analysis_mean_diffusion.jpg",
       width = 40,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)



# Tau position analysis

meta.tau <- readxl::read_xlsx("Results/NEW_periods/Meta_analysis.xlsx", sheet = "tau_all") %>%
  as.data.frame()

meta.tau %<>% dplyr::rename(Period = 1)

meta.tau.t <- meta.tau %>% dplyr::filter(Site == "Staten Island")
meta.tau.c <- meta.tau %>% dplyr::filter(Site == "Rockefeller")


g1 <- ggplot(meta.tau.t) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.tau.t, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 3, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$tau_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "C", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.tau$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates of Tau position\nStaten Island population") +
  xlab("95% tau [hours]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)

g2 <- ggplot(meta.tau.c) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.tau.c, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 3, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$tau_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "C", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.tau$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates of Tau position\nRockefeller population") +
  xlab("95% tau [hours]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)


gr1 <- grid.arrange(g1, g2, ncol = 1)

ggsave(plot = gr1,
       filename = "Results/NEW_periods/meta_analysis_mean_tau_position.jpg",
       width = 40,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)

# Speed analysis

meta.speed <- readxl::read_xlsx("Results/NEW_periods/Meta_analysis.xlsx", sheet = "speed_all") %>%
  as.data.frame()

meta.speed %<>% dplyr::rename(Period = 1)

meta.speed.t <- meta.speed %>% dplyr::filter(Site == "Staten Island")
meta.speed.c <- meta.speed %>% dplyr::filter(Site == "Rockefeller")


g1 <- ggplot(meta.speed.t) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.speed.t, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 1.2, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$speed_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.speed$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates of Speed\nStaten Island population") +
  xlab("95% Speed [km/day]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)

g2 <- ggplot(meta.speed.c) +
  geom_bar(aes(y = Period, x = est, fill = Period), stat = "identity", alpha=0.7) +
  geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  geom_text(data = meta.speed.c, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 1.2, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$speed_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  scale_x_continuous(limits = c(0, max(meta.speed$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates of Speed\nRockefeller population") +
  xlab("95% Speed [km/day]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)


gr1 <- grid.arrange(g1, g2, ncol = 1)

ggsave(plot = gr1,
       filename = "Results/NEW_periods/meta_analysis_mean_speed.jpg",
       width = 40,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# Combined

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))}


meta.comb <- readxl::read_xlsx("Results/NEW_periods/Meta_analysis.xlsx", sheet = "combined") %>%
  as.data.frame()

meta.comb %<>% dplyr::rename(Period = 1)

meta.comb.t <- meta.comb %>% dplyr::filter(Site == "Staten Island")
meta.comb.c <- meta.comb %>% dplyr::filter(Site == "Rockefeller")


meta.comb.ts <- as.data.frame(scale(meta.comb.t %>% select_if(., is.numeric)))

meta.comb.ts <- range01(meta.comb.ts)

meta.comb.t %<>% dplyr::mutate(low = meta.comb.ts$low,
                              est = meta.comb.ts$est,
                              high = meta.comb.ts$high)


meta.comb.cs <- as.data.frame(scale(meta.comb.c %>% select_if(., is.numeric)))

meta.comb.cs <- range01(meta.comb.cs)

meta.comb.c %<>% dplyr::mutate(low = meta.comb.cs$low,
                               est = meta.comb.cs$est,
                               high = meta.comb.cs$high)


g1 <- ggplot(meta.comb.t) +
  geom_bar(aes(y = Period, x = est, fill = Variable, group = Variable), stat = "identity", position = position_dodge(preserve = "single"), alpha=0.7) +
  #geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3, position = position_dodge(preserve = "single")) +
  #geom_text(data = meta.comb.t, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 1.2, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$comb_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "D", discrete = T, "Variable: ") +
  # scale_x_continuous(limits = c(0, max(meta.comb$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates - Relative Ratio\nStaten Island population") +
  xlab("Standardized values [0-1]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)


g2 <- ggplot(meta.comb.c) +
  geom_bar(aes(y = Period, x = est, fill = Variable, group = Variable), stat = "identity", position = position_dodge(preserve = "single"), alpha=0.7) +
  #geom_errorbar(aes(y = Period, xmin = low, xmax = high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  #geom_text(data = meta.speed.c, aes(y = Period, x = est, label=round(est, 2)), nudge_y = 0.3, nudge_x = 1.2, size = 4, color = "black", fontface = "bold") +
  #geom_vline(xintercept = mean(t.akde$speed_mean), color = "red", size = 1) +
  scale_fill_viridis(option = "F", discrete = T, "Variable: ") +
  #scale_x_continuous(limits = c(0, max(meta.speed$high)+5)) +
  ggtitle("Meta analysis - debiased population mean estimates - Relative Ratio\nRockefeller population") +
  xlab("Standardized values [0-1]")+
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(face="bold")) +
  facet_wrap(~Sex)

gr1 <- grid.arrange(g1, g2, ncol = 1)


ggsave(plot = gr1,
       filename = "Results/NEW_periods/meta_analysis_relative_ratio_all_variables.jpg",
       width = 40,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)



# Correlation
# ------------------------------------------------------------------------------


# Breeding - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
               c.akde = c.akde, 
               export.folder = "Results/NEW_periods/Breeding/Male/",
               plot.width = 45, 
               plot.height = 25)


# Breeding - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
                 c.akde = c.akde, 
                 export.folder = "Results/NEW_periods/Breeding/Female/",
                 plot.width = 45, 
                 plot.height = 25)


# Post Breeding - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
                 c.akde = c.akde, 
                 export.folder = "Results/NEW_periods/Post Breeding/Male/",
                 plot.width = 45, 
                 plot.height = 25)


# Post Breeding - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
                 c.akde = c.akde, 
                 export.folder = "Results/NEW_periods/Post Breeding/Female/",
                 plot.width = 45, 
                 plot.height = 25)


# Baseline - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
                 c.akde = c.akde, 
                 export.folder = "Results/NEW_periods/Baseline/Male/",
                 plot.width = 45, 
                 plot.height = 25)


# Baseline - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()

correlation_plot(t.akde = t.akde, 
                 c.akde = c.akde, 
                 export.folder = "Results/NEW_periods/Baseline/Female/",
                 plot.width = 45, 
                 plot.height = 25)





# ------------------------------------------------------------------------------



# Period analysis - just AKDE/Diffusion individuals
# ------------------------------------------------------------------------------

# Breeding 
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Just AKDE_DIFF/Breeding/Breeding_period_results.RDS")

period.summary_just_akde_diff(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Breeding/")

png("Results/NEW_periods/Just AKDE_DIFF/Breeding/variograms.png", width = 25, height = 30, units='cm', res = 600)
plot_vario(period.result$variograms[3:5], period.result$fit.objects[3:5])
dev.off()


t.akde <- period.result$akde.objects
png( "Results/NEW_periods/Just AKDE_DIFF/Breeding/AKDE_plots.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$telemetry.objects) 
dev.off()


# Post Breeding 
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Just AKDE_DIFF/Post Breeding/Post Breeding_period_results.RDS")

period.summary_just_akde_diff(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Post Breeding/")

png("Results/NEW_periods/Just AKDE_DIFF/Post Breeding/variograms.png", width = 25, height = 30, units='cm', res = 600)
plot_vario(period.result$variograms, period.result$fit.objects)
dev.off()


t.akde <- period.result$akde.objects
png( "Results/NEW_periods/Just AKDE_DIFF/Post Breeding/AKDE_plots.png", width = 25, height = 30, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$telemetry.objects) 
dev.off()


# Baseline
# ------------------------------------------------------------------------------

period.result <- readRDS("Results/NEW_periods/Just AKDE_DIFF/Baseline/Baseline_period_results.RDS")

period.summary_just_akde_diff(period_result = period.result, export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Baseline/")

png("Results/NEW_periods/Just AKDE_DIFF/Baseline/variograms.png", width = 25, height = 30, units='cm', res = 600)
plot_vario(period.result$variograms, period.result$fit.objects)
dev.off()


t.akde <- period.result$akde.objects
png( "Results/NEW_periods/Just AKDE_DIFF/Baseline/AKDE_plots.png", width = 25, height = 30, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = period.result$telemetry.objects) 
dev.off()







# Linear regression speed vs diffusion
# ------------------------------------------------------------------------------


# Breeding - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde, 
#                         c.akde = c.akde, 
#                         export.folder = "Results/NEW_periods/Breeding/Male/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde,
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Breeding/Male/")

                        



# Breeding - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde, 
#                         c.akde = c.akde, 
#                         export.folder = "Results/NEW_periods/Breeding/Female/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde,
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Breeding/Female/")


# Post Breeding - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde, 
#                         c.akde = c.akde, 
#                         export.folder = "Results/NEW_periods/Post Breeding/Male/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde,
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Post Breeding/Male/")


# Post Breeding - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde, 
#                         c.akde = c.akde, 
#                         export.folder = "Results/NEW_periods/Post Breeding/Female/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde,
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Post Breeding/Female/")

                        

# Baseline - Males
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde,
#                         c.akde = c.akde,
#                         export.folder = "Results/NEW_periods/Baseline/Male/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde,
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Baseline/Male/")

                        

# Baseline - Females
# ------------------------------------------------------------------------------

t.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


# linear_regression_plots(t.akde = t.akde,
#                         c.akde = c.akde,
#                         export.folder = "Results/NEW_periods/Baseline/Female/")

# -------------

linear_regression_plots_with_error_bars(t.akde = t.akde, 
                                        c.akde = c.akde, 
                                        export.folder = "Results/NEW_periods/Baseline/Female/")





# ------------------------------------------------------------------------------




# Linear regression - males vs females
# ------------------------------------------------------------------------------

# Breeding

t.akde.m <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.m <- readxl::read_xlsx("Results/NEW_periods/Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.akde.f <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.f <- readxl::read_xlsx("Results/NEW_periods/Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g1 <- linear_regression_plots_per_period(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Breeding", export.folder = "Results/NEW_periods/Breeding/")

g1 <- linear_regression_plots_per_period_with_error_bars(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Breeding", export.folder = "Results/NEW_periods/Breeding/")


# Post Breeding

t.akde.m <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.m <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.akde.f <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.f <- readxl::read_xlsx("Results/NEW_periods/Post Breeding/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g2 <- linear_regression_plots_per_period(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Post Breeding", export.folder = "Results/NEW_periods/Post Breeding/")

g2 <- linear_regression_plots_per_period_with_error_bars(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Post Breeding", export.folder = "Results/NEW_periods/Post Breeding/")


# Baseline

t.akde.m <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.m <- readxl::read_xlsx("Results/NEW_periods/Baseline/Male/control.site_summary.xlsx") %>% 
  as.data.frame()

t.akde.f <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/treatment.site_summary.xlsx") %>% 
  as.data.frame()

c.akde.f <- readxl::read_xlsx("Results/NEW_periods/Baseline/Female/control.site_summary.xlsx") %>% 
  as.data.frame()


g3 <- linear_regression_plots_per_period(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Baseline", export.folder = "Results/NEW_periods/Baseline/")

g3 <- linear_regression_plots_per_period_with_error_bars(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, period = "Baseline", export.folder = "Results/NEW_periods/Baseline/")


gr1 <- grid.arrange(g1, g2, g3, ncol = 1)

ggsave(plot = gr1,
       filename = "Results/NEW_periods/lin_reg_treatment_vs_control_per_period_per_sex.jpg",
       width = 28,
       height = 50,
       units = "cm",
       device = "jpeg",
       dpi = 700)


ggsave(plot = gr1,
       filename = "Results/NEW_periods/lin_reg_treatment_vs_control_with_error_bars_per_period_per_sex.jpg",
       width = 45,
       height = 90,
       units = "cm",
       device = "jpeg",
       dpi = 700)







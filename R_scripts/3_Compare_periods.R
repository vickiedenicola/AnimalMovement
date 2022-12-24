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

# Movement packages

library(move)
library(ctmm)
library(ctmmweb)

source("R_scripts/functions.R")

# Read the data
# ------------------------------------------------------------------------------

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
  as.data.frame()

UERE <- readRDS("Data/undep/calibration.error.model.rds")

calibration_model <- UERE

# Compare periods
# ------------------------------------------------------------------------------

breeding.period <- compare_periods(input.data = data.raw, 
                                   period = "Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                   male_female = "M", # c("M", "F")
                                   use.ctmm = "fit", # c("fit", "select")
                                   cal.model = calibration_model)

# ------------------------------------------------------------------------------

breeding.period <- readRDS("Results/breeding.period.RDS")


# writexl::write_xlsx(breeding.period$treatment$summary, "Results/comparasion.breeding.period_treatment.site_summary.xlsx")
# writexl::write_xlsx(breeding.period$treatment$akde, "Results/comparasion.breeding.period_treatment.site_akde.xlsx")
# writexl::write_xlsx(breeding.period$treatment$speed.dist, "Results/comparasion.breeding.period_treatment.site_speed_dist.xlsx")
# 
# 
# writexl::write_xlsx(breeding.period$control$summary, "Results/comparasion.breeding.period_control.site_summary.xlsx")
# writexl::write_xlsx(breeding.period$control$akde, "Results/comparasion.breeding.period_control.site_akde.xlsx")
# writexl::write_xlsx(breeding.period$control$speed.dist, "Results/comparasion.breeding.period_control.site_speed_dist.xlsx")
# 
# saveRDS(breeding.period, "Results/breeding.period.RDS")

# grid.arrange(breeding.period$treatment$plot, breeding.period$control$plot, ncol = 2)

# breeding.period$treatment$map
# breeding.period$control$map

EXT <- extent(breeding.period$treatment$akde.objects[[3]], level = 0.95)
plot(breeding.period$treatment$telemetry.objects[[3]], UD = breeding.period$treatment$akde.objects[[3]], xlim = EXT$x, ylim = EXT$y)
title("OUF AKDE")

EXT <- extent(breeding.period$control$akde.objects[[3]], level = 0.95)
plot(breeding.period$control$telemetry.objects[[3]], UD = breeding.period$control$akde.objects[[3]], xlim = EXT$x, ylim = EXT$y)
title("OUF AKDE")


# optional - remove 53
# ------------------------------------------------------------------------------
control.list <- list()

for(i in 1:length(breeding.period$control$akde.objects)){
  if(!(breeding.period$control$akde.objects[[i]]@info$identity == "53")){
    control.list[[i]] <- breeding.period$control$akde.objects[[i]]
  }
}


names(control.list) <- seq_along(control.list)

## Using some higher-order convenience functions
control.list <- Filter(Negate(is.null), control.list)
length(control.list)

breeding.period$control$akde.objects <- control.list

# ------------------------------------------------------------------------------



names.list_c <- list()
for(i in 1:length(breeding.period$control$akde.objects)){
  names.list_c[[i]] <- breeding.period$control$akde.objects[[i]]@info$identity
}

names(breeding.period$control$akde.objects) <- names.list_c

names.list_t <- list()
for(i in 1:length(breeding.period$treatment$akde.objects)){
  names.list_t[[i]] <- breeding.period$treatment$akde.objects[[i]]@info$identity
}

names(breeding.period$treatment$akde.objects) <- names.list_t

plot_ud(UD_list = breeding.period$control$akde.objects[1],
        level_vec = 0.95,
        color_vec = viridis::viridis(length(breeding.period$control$akde.objects[1])),
        # option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 1,
        cex = 0.65,
        tele_list = breeding.period$control$telemetry.objects[1])


plot_ud(UD_list = breeding.period$control$akde.objects[5:7],
        level_vec = 0.95,
        color_vec = viridis::viridis(length(breeding.period$control$akde.objects)),
        # option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 1,
        cex = 0.65,
        tele_list = breeding.period$control$telemetry.objects[5:7])


c.akde <- breeding.period$control$akde.objects
names(c.akde) <- names.list_c
# png("Results/Rplot_control_AKDE_breeding_males.png", width = 25, height = 30, units='cm', res = 600)
plot_ud(UD_list = c.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(c.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = breeding.period$control$telemetry.objects) 
dev.off()


t.akde <- breeding.period$treatment$akde.objects
names(t.akde) <- names.list_t

# png("Results/Rplot_treatment_AKDE_breeding_males.png", width = 25, height = 40, units='cm', res = 600)
plot_ud(UD_list = t.akde,
        level_vec = 0.95,
        color_vec = viridis::viridis(length(t.akde)),
        option = c("contour", "interval", "location"), #  "contour", "interval", "location"
        columns = 2,
        cex = 0.65,
        tele_list = breeding.period$treatment$telemetry.objects) 
dev.off()


png("Results/Rplot_results_AKDE_breeding_males_without53.png", width = 25, height = 18, units='cm', res = 600)

par(mfrow = c(1, 2))

# home range area
ctmm::meta(breeding.period$control$akde.objects, 
           sort = TRUE, 
           col = viridis::viridis(length(breeding.period$control$akde.objects))) + 
  title("AKDE Home range area\nRockefeller population (control site)", sub = "Breeding period - MALE population") 

ctmm::meta(breeding.period$treatment$akde.objects, 
           sort = TRUE, 
           col = viridis::viridis(length(breeding.period$treatment$akde.objects))) +
  title("AKDE Home range area\nStaten Island population (treatment site)", sub = "Breeding period - MALE population")

dev.off()

par(mfrow = c(1, 1))

summary(breeding.period$treatment$akde.objects[[5]])

# TODO fix period dates - DONE
# TODO save from compare functions fit or select objects in list - DONE
# TODO fix adding names - iz liste iz identity i onda to dodaj i u names liste - DONE
# TODO fix units in summary (upit po rowname ili row name dodaj kao novu kolonu) - DONE
# TODO save plots (make unique folder) - DONE

# TODO analyse units in tables - results and make function to convert all in the same units
# TODO meta - variable = "speed"
# TODO make ggplot function - compare speed, distance and area (first fix function to work on top of excel results)
# TODO from meta export also mean values per periods and sites

# TODO make function to export meta results per period in the same folder (and also from custom function)

# TODO maybe try with tryCatch for ctmm::fit and isotropic = TRUE/FALSE
# TODO check for including variogram as parameter in ctmm::fit 
# TODO cores = 0 in ctmm::select

# all

png("Results/Rplot_results_AKDE_breeding_males_all.png", width = 25, height = 18, units='cm', res = 600)
par(mfrow = c(1, 1))
ctmm::meta(c(breeding.period$control$akde.objects,breeding.period$treatment$akde.objects), 
           sort = TRUE, 
           col = viridis::viridis(length(c(breeding.period$control$akde.objects,breeding.period$treatment$akde.objects)))) 
dev.off()

# Plots

png("Results/Rplot_maps_AKDE_breeding_males_without53.png", width = 25, height = 18, units='cm', res = 600)

par(mfrow = c(1, 2))

plot(breeding.period$control$akde.objects, 
     col.DF = viridis::viridis(length(breeding.period$control$akde.objects)), 
     col.level = viridis::viridis(length(breeding.period$control$akde.objects)), 
     col.grid = NA, 
     level = NA, 
     labels = names.list_c) +
  title("AKDE Home range area\nRockefeller population (control site)", sub = "Breeding period - MALE population") 

plot(breeding.period$treatment$akde.objects, 
     col.DF = viridis::viridis(length(breeding.period$treatment$akde.objects)), 
     col.level = viridis::viridis(length(breeding.period$treatment$akde.objects)), 
     col.grid = NA, 
     level = NA, 
     labels = names.list_t) +
  title("AKDE Home range area\nStaten Island population (treatment site)", sub = "Breeding period - MALE population")


dev.off()

par(mfrow = c(1, 1))


# Periods - processing 
# First make export folder
# Second define period 
# Third run
# ------------------------------------------------------------------------------

# Pre Breeding	15 Sep - 15 Oct
# Breeding	16 Oct- 31 Dec
# Post Breeding	1 Jan - 15 April
# Baseline	16 Apr - 30 May




# Pre Breeding	15 Sep - 15 Oct
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw, 
                                     period = "Pre Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                     male_female = "F", # c("M", "F")
                                     use.ctmm = "fit", # c("fit", "select")
                                     cal.model = calibration_model,
                                     export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                     period = "Pre Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                     male_female = "M", # c("M", "F")
                                     use.ctmm = "fit", # c("fit", "select")
                                     cal.model = calibration_model,
                                     export.folder = "C:/R_projects/AnimalMovement/Results/Pre Breeding/Male") # absolute path

# Breeding	16 Oct- 31 Dec
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "F", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "M", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Breeding/Male") # absolute path

# Post Breeding	1 Jan - 15 April
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Post Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "F", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Post Breeding/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Post Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "M", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Post Breeding/Male") # absolute path

# Baseline	16 Apr - 30 May
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Baseline", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "F", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Baseline/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Baseline", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                  male_female = "M", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/Baseline/Male") # absolute path




# New periods
# ------------------------------------------------------------------------------
# Breeding 16 OCT - 31 DEC
# Post Breeding 1 JAN - 15 MAR
# Baseline 16 MAR - 30 MAY

source("R_scripts/functions.R")

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered_NEW.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  as.data.frame()

check_table <- readxl::read_xlsx("Analysis/stats_ndays_per_period_per_site_per_sex_NEW.xlsx") %>%
  as.data.frame()

UERE <- readRDS("Data/undep/calibration.error.model.rds")
calibration_model <- UERE


# Breeding	16 Oct- 31 Dec
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw,
                                  period = "Breeding", # c("Breeding", "Post Breeding", "Baseline")
                                  male_female = "F", # c("M", "F")
                                  use.ctmm = "fit", # c("fit", "select")
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Breeding", 
                                  male_female = "M", 
                                  use.ctmm = "fit", 
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Breeding/Male") # absolute path

# Post Breeding	1 Jan - 15 March
# ------------------------------------------------------------------------------
period.results <- compare_periods(input.data = data.raw, 
                                  period = "Post Breeding", 
                                  male_female = "M", 
                                  use.ctmm = "fit", 
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Male") # absolute path


period.results <- compare_periods(input.data = data.raw, 
                                  period = "Post Breeding", 
                                  male_female = "F", 
                                  use.ctmm = "fit", 
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Post Breeding/Female") # absolute path


# Baseline	16 March - 30 May
# ------------------------------------------------------------------------------

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Baseline", 
                                  male_female = "F", 
                                  use.ctmm = "fit", 
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Female") # absolute path

period.results <- compare_periods(input.data = data.raw, 
                                  period = "Baseline", 
                                  male_female = "M",
                                  use.ctmm = "fit", 
                                  cal.model = calibration_model,
                                  check_table = check_table,
                                  export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Baseline/Male") # absolute path












# Process individuals with less data
# ------------------------------------------------------------------------------
source("R_scripts/functions.R")

UERE <- readRDS("Data/undep/calibration.error.model.rds")
calibration_model <- UERE

check.akde.diff <- readxl::read_xlsx("Analysis/stats_only_akde_diff_calc.xlsx") %>% as.data.frame()

data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_new.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

study_animals <- readxl::read_xlsx("Data/Study_Animals.xlsx" , sheet = "Sheet2") %>%
  as.data.frame()


study_animals %<>%
  dplyr::mutate(Color = str_sub(ID, - 1, - 1),
                ID = tolower(sub('[YW]$', '', ID)))

# data.raw %<>% dplyr::filter(`individual-local-identifier` %in% study_animals$ID)
data.raw %<>% left_join(., study_animals %>% dplyr::select(ID, `study area`, sex, Color), by = c("individual-local-identifier" = "ID"))
data.raw %<>% dplyr::rename(study.area = `study area`)

data.raw %<>% dplyr::mutate(
  Date = as.Date(timestamp))

data.raw %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0))

data.raw %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))


# Breeding	16 Oct- 31 Dec
# ------------------------------------------------------------------------------

period.results <- calc_periods_akde_diff(input.data = data.raw, 
                                         period = "Breeding", 
                                         use.ctmm = "fit", 
                                         cal.model = calibration_model, 
                                         check_table = check.akde.diff, 
                                         export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Breeding/")

# Post Breeding	1 Jan - 15 March
# ------------------------------------------------------------------------------

period.results <- calc_periods_akde_diff(input.data = data.raw, 
                                         period = "Post Breeding", 
                                         use.ctmm = "fit", 
                                         cal.model = calibration_model, 
                                         check_table = check.akde.diff, 
                                         export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Post Breeding/")


# Baseline	16 March - 30 May
# ------------------------------------------------------------------------------

period.results <- calc_periods_akde_diff(input.data = data.raw, 
                                         period = "Baseline", 
                                         use.ctmm = "fit", 
                                         cal.model = calibration_model, 
                                         check_table = check.akde.diff, 
                                         export.folder = "C:/R_projects/AnimalMovement/Results/NEW_periods/Just AKDE_DIFF/Baseline")

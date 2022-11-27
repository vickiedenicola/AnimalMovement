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

# Additional packages

library(move)
library(ctmm)

source("R_scripts/functions.R")

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
  as.data.frame()

UERE <- readRDS("Data/undep/calibration.error.model.rds")

calibration_model <- UERE

breeding.period <- compare_periods(input.data = data.raw, 
                                   period = "Breeding", # c("Pre Breeding", "Breeding", "Post Breeding", "Baseline")
                                   male_female = "M", # c("M", "F")
                                   use.ctmm = "fit", # c("fit", "select")
                                   cal.model = calibration_model)

writexl::write_xlsx(breeding.period$treatment$summary, "Results/comparasion.breeding.period_treatment.site.xlsx", sheet = "summary")
writexl::write_xlsx(breeding.period$treatment$akde, "Results/comparasion.breeding.period_treatment.site.xlsx", sheet = "akde")
writexl::write_xlsx(breeding.period$treatment$speed.dist, "Results/comparasion.breeding.period_treatment.site.xlsx", sheet = "speed_dist")


writexl::write_xlsx(breeding.period$control$summary, "Results/comparasion.breeding.period_control.site.xlsx", sheet = "summary")
writexl::write_xlsx(breeding.period$control$akde, "Results/comparasion.breeding.period_control.site.xlsx", sheet = "akde")
writexl::write_xlsx(breeding.period$control$speed.dist, "Results/comparasion.breeding.period_control.site.xlsx", sheet = "speed_dist")

saveRDS(breeding.period, "Results/breeding.period.RDS")

grid.arrange(breeding.period$treatment$plot, breeding.period$control$plot, ncol = 2)

breeding.period$treatment$map
breeding.period$control$map

EXT <- extent(breeding.period$treatment$akde.objects[[1]], level = 0.95)
plot(breeding.period$treatment$telemetry.objects[[1]], UD = breeding.period$treatment$akde.objects[[1]], xlim = EXT$x, ylim = EXT$y)
title("OUF AKDE")

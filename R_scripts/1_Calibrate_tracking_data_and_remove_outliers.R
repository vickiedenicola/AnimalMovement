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

# Additional packages

library(move)
library(ctmm)

source("R_scripts/functions.R")

# Read the cleaned dataset
# ------------------------------------------------------------------------------

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% as.data.frame()

range(data.raw$timestamp)
dim(data.raw)

# Split dataset to control and treatment site
# ------------------------------------------------------------------------------
data.SI <- data.raw %>%
  dplyr::filter(study.area == "Staten Island")

data.RF <- data.raw %>%
  dplyr::filter(study.area == "Rockefeller")

# or use subset function
# ------------------------------------------------------------------------------
subset_func(dataset = data.raw,
            time_period = "Breeding",
            male_female = "M",
            study_area = "Staten Island",
            animal_color = "W")

# Some basic visualization
# ------------------------------------------------------------------------------
plot_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"))
plot_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44"))

map_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"), burst = TRUE)
map_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44", "62", "47"), burst = TRUE)


# As telemetry object
# --------------------------------------------------------

data.SI.tel <- as.telemetry(data.SI, datum = 'EPSG:4326')
data.RF.tel <- as.telemetry(data.RF, datum = 'EPSG:4326')


# Calibrate tracking data and remove outliers
# ------------------------------------------------------------------------------

# Read the calibration error model
# `User equivalent range error', which is location error partially standardized by DOP value.

UERE <- readRDS("Data/undep/calibration.error.model.rds")
summary(UERE)

UERE$DOF[] <- UERE$UERE # need to be checked

uere(data.SI.tel) <- UERE
summary(uere(data.SI.tel)) # this should be the same as summary(UERE)

uere(data.RF.tel) <- UERE
summary(uere(data.RF.tel)) # this should be the same as summary(UERE)



# Outlier detection
# --------------------------------------------------------

outliers.SI <- outlie(data.SI.tel, plot = FALSE)
outliers.RF <- outlie(data.RF.tel, plot = FALSE)

plot(outliers.SI[[5]]) # Units speed:: m/s
max(outliers.SI[[5]]$speed) * 100

# Filter errors based on speed.
outliers.SI[[5]]$speed

# Datasets may have multiple outliers.
# In pathological situations, there may be no clear separation between the normative data and the outliers.
# This necessitates a better error model, either by improving inadequate (or absent)
# HDOP estimates or by employing a heavier tailed error distribution (not yet supported).


# Check the speed: calculate as distance/time interval

# Staten Island
# --------------------------------------------------------

data.SI.sf <- st_as_sf(data.SI, coords = c("location-long", "location-lat"), crs = 4326)

speed.SI <- data.SI.sf %>%
  group_by(`individual-local-identifier`) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
    time_diff = timestamp - lag(timestamp)) %>%
  ungroup() %>%
  dplyr::select(`individual-local-identifier`, timestamp, dist, time_diff) %>%
  st_drop_geometry() %>%
  as.data.frame()

speed.SI %<>% replace(is.na(.), 0)

speed.SI %<>% dplyr::mutate(speed_m_s = as.numeric(dist)/as.numeric(time_diff),
                            speed_km_h = speed_m_s*3.6,
                            speed_cm_s = speed_m_s*100)

is.na(speed.SI)<-sapply(speed.SI, is.infinite)
speed.SI[is.na(speed.SI)]<-0

summary(speed.SI)

# Rockefeller
# --------------------------------------------------------

data.RF.sf <- st_as_sf(data.RF, coords = c("location-long", "location-lat"), crs = 4326)

speed.RF <- data.RF.sf %>%
  group_by(`individual-local-identifier`) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
    time_diff = timestamp - lag(timestamp)) %>%
  ungroup() %>%
  dplyr::select(`individual-local-identifier`, timestamp, dist, time_diff) %>%
  st_drop_geometry() %>%
  as.data.frame()

speed.RF %<>% replace(is.na(.), 0)

speed.RF %<>% dplyr::mutate(speed_m_s = as.numeric(dist)/as.numeric(time_diff),
                            speed_km_h = speed_m_s*3.6,
                            speed_cm_s = speed_m_s*100)

is.na(speed.RF)<-sapply(speed.RF, is.infinite)
speed.RF[is.na(speed.RF)]<-0

summary(speed.RF)


# Remove outliers
# --------------------------------------------------------

for(i in 1:length(outliers.SI)){
  print(paste0("Max speed value: ", max(outliers.SI[[i]]$speed) * 100, " cm/s"))
  x <- which(outliers.SI[[i]]$speed * 100 >= 16.1)
  print(paste0("Rows to remove: ", length(x)))
  if(length(x) > 0){
    data.SI.tel[[i]] <- data.SI.tel[[i]][-x, ]
  }
  print(paste0("Processed: ", i, "/", length(outliers.SI), " individuals"))
  print("------------------------------------------------------------------")
}

for(i in 1:length(outliers.RF)){
  print(paste0("Max speed value: ", max(outliers.RF[[i]]$speed) * 100, " cm/s"))
  x <- which(outliers.RF[[i]]$speed * 100 >= 16.1)
  print(paste0("Rows to remove: ", length(x)))
  if(length(x) > 0){
    data.RF.tel[[i]] <- data.RF.tel[[i]][-x, ]
  }
  print(paste0("Processed: ", i, "/", length(outliers.RF), " individuals"))
  print("------------------------------------------------------------------")
}


# Check now
outliers.SI <- outlie(data.SI.tel, plot = FALSE)
outliers.RF <- outlie(data.RF.tel, plot = FALSE)

plot(outliers.SI[[5]]) # Units speed:: m/s
max(outliers.SI[[5]]$speed) * 100


# saveRDS(data.SI.tel, "Data/processed/telemetry.SI.cleaned.rds")
# saveRDS(data.RF.tel, "Data/processed/telemetry.RF.cleaned.rds")

data.SI.tel <- readRDS("Data/processed/telemetry.SI.cleaned.rds")
data.RF.tel <- readRDS("Data/processed/telemetry.RF.cleaned.rds")






# Example
# --------------------------------------------------------

# Fitting with calibrated data
# --------------------------------------------------------

# Visually fit a movement model to a variogram

# automated guesstimate for calibrated data
GUESS <- ctmm.guess(data.SI.tel[[1]], # ctmm.guess needs to be run individually
                    CTMM = ctmm(error = TRUE),
                    interactive = FALSE)

# stepwise fitting # CRAN policy limits us to 2 cores
FIT <- ctmm.select(data.SI.tel[[1]],
                   GUESS,
                   trace = TRUE,
                   cores = 10) # if you get errors on your platform, then try cores=1

summary(FIT)

saveRDS(FIT, "Data/model_fitting_result.rds")
FIT <- readRDS("Data/model_fitting_result.rds")

summary(FIT)

# Since we started with a finite amount of calibration data,
# the location-error parameter estimates were updated by the tracking data,
# and we can compare the results here with the previous estimates in summary(UERE).



# Calculate convex hull
# --------------------------------------------------------


# https://ctmm-initiative.github.io/ctmm/reference/export.html

deer <- data.SI.tel[[1]] %>% as.data.frame()
deer <- as.sf(data.SI.tel[[1]], 
              error = FALSE)

deer %<>% st_transform(4326)
mapview(deer)

library(tmap)
library(sf)

qtm(deer)

ch <- st_convex_hull(st_union(deer)) 
qtm(ch)

mapview(deer) + mapview(ch, col.regions = "red")

area.m2 <- ch %>% st_area(.) # 1134 ha
as.numeric(area.m2)/1000000 # 11.3 km2

# in CRS in projection
deer_proj <- deer %<>% st_transform(3857)
ch_proj <- st_convex_hull(st_union(deer_proj)) 

mapview(deer_proj) + mapview(ch_proj, col.regions = "red")

area.m2 <- ch_proj %>% st_area(.) # 1970 ha
as.numeric(area.m2)/1000000 # 19.7 km2







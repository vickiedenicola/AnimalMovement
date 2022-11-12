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

# Additional packages

library(move)
library(ctmm)

# Calibration data
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

cal.data <- data.table::fread("Data/undep/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

unique(cal.data$`tag-local-identifier`)
range(cal.data$timestamp) # strange, need to be checked
# 3 august - 10 august calibration data

cal.data %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))


# start: 2021-08-01 00:00:00
# end: 2021-09-15 00:00:00

# cal.data %<>% dplyr::filter(between(Date, as.Date('2021-08-01'), as.Date('2021-09-07')))
cal.data %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0)) # - 6556  (366394)

# cal.data %<>% dplyr::filter(`gps:fix-type-raw` == "3D valid GPS Fix") %>%
#   dplyr::filter(!is.na(`gps:satellite-count`))

cal.data.sf <- st_as_sf(cal.data, coords = c("location-long", "location-lat"), crs = 4326)
# mapview(cal.data.sf)

cal.ext <- sf::st_read("Data/undep/calibration_data_extent.gpkg")

cal.data.sf <- cal.data.sf[cal.ext, ]

cal.data.sf %<>% dplyr::filter(`gps:satellite-count` >= 3)

dim(cal.data.sf)
rem <- c(23489397510, 23489397511, 23522097486, 23487492456)
cal.data.sf %<>% dplyr::filter(!(as.character(`event-id`) %in% as.character(rem)))

cal.data.sf
mapview(cal.data.sf)

range(cal.data.sf$timestamp)
cal.data.sf %<>% dplyr::filter(between(Date, as.Date('2021-08-01'), as.Date('2021-09-15')))

unique(cal.data.sf$`gps:fix-type-raw`)

cal.data.sf %>% dplyr::group_by(`gps:fix-type-raw`) %>%
  dplyr::summarise(n_per_type = n())

calibration.data <- cal.data.sf %>% dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                                                  `location-lat` = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  as.data.frame()

# write.csv(calibration.data, "Data/undep/calibration_data.filtered.csv")

# ------------------------------------------------------------------------------

calibration.data1 <- read.csv(file = "Data/undep/calibration_data.filtered.csv",
                             stringsAsFactors = FALSE)

unique(calibration.data$individual.local.identifier)
unique(calibration.data$tag.local.identifier)
unique(calibration.data$transmission.protocol)

calibration.data %<>% dplyr::rename(`location-long` = location.long,
                                    `location-lat` = location.lat)

cal.data.sf <- st_as_sf(calibration.data, coords = c("location-long", "location-lat"), crs = 4326)

mapview(cal.data.sf, zcol = "tag.local.identifier", layer.name = "Tag ID")

mapview(cal.data.sf, zcol = "tag.local.identifier", layer.name = "Tag ID", burst = TRUE)


range(calibration.data$timestamp)

calibration.data %>% dplyr::filter(tag.local.identifier == 47781)

co.data <- calibration.data %>% dplyr::filter(transmission.protocol == "Collar")
co.data %>% dplyr::filter(tag.local.identifier == 47781)
ir.data <- calibration.data %>% dplyr::filter(transmission.protocol == "Iridium TCP")
ir.data %>% dplyr::filter(tag.local.identifier == 47781)

ir.data$tag.local.identifier

# write.csv(calibration.data %>% dplyr::filter(tag.local.identifier == 47781),
#           "Data/undep/calibration_data.filtered_example_47781.csv")


head(co.data)

unique(co.data$gps.fix.type.raw)



# Error calibration
# ------------------------------------------------------------------------------

# Calibration data must be collected from the same model device as the tracking data
# Data’s “dilution of precision” (DOP) and error columns import correctly into ctmm
# Data formatting bust be similar enough that the same location classes are detected by as.telemetry

# Two approaches:
#           1. try with all dop values, fix types and satellite counts
#           2. some previous data cleaning - remove outliers


cal.data <- data.table::fread("Data/undep/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

# Make Date columns
cal.data %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))

# Remove events without coordinates
cal.data %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0)) # - 6556  (366394)

# Spatial intersect with extent
cal.data.sf <- st_as_sf(cal.data, coords = c("location-long", "location-lat"), crs = 4326)
cal.ext <- sf::st_read("Data/undep/calibration_data_extent.gpkg")
cal.data.sf <- cal.data.sf[cal.ext, ]

# Remove some outliers
rem <- c(23489397510, 23489397511, 23522097486, 23487492456)
cal.data.sf %<>% dplyr::filter(!(as.character(`event-id`) %in% as.character(rem)))

# Filter by date
cal.data.sf %<>% dplyr::filter(between(Date, as.Date('2021-08-01'), as.Date('2021-09-15')))
mapview(cal.data.sf)

# Make data.frame
calibration.data <- cal.data.sf %>% dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                                                  `location-lat` = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  as.data.frame()

# dim(calibration.data)
# unique(calibration.data$`gps:fix-type-raw`)
# range(calibration.data$`gps:dop`)
# calibration.data %>% dplyr::filter(`gps:dop` > 5)

# Check for duplicated events
dupl.check <- calibration.data %>%
  dplyr::group_by(`tag-local-identifier`) %>%
  dplyr::summarize(dupl_count = sum(duplicated(timestamp)))

range(dupl.check$dupl_count)

calibration.data %<>%
  dplyr::group_by(`tag-local-identifier`) %>%
  distinct(timestamp, .keep_all = TRUE) %>%
  ungroup() %>%
  as.data.frame()

unique(calibration.data$`individual-local-identifier`)

calibration.data %<>% dplyr::select(-`individual-local-identifier`) # individual id must be dropped,
# in that case tag id will not be recognized all events will be treated as one telemetry object

calibration.data %<>% dplyr::mutate(class = `gps:fix-type-raw`)

# 1. First approach
# ------------------------------------------------------------------------------

calibration.tel <- as.telemetry(calibration.data, datum = 'EPSG:4326')

plot(calibration.tel)
names(calibration.tel)

plot(calibration.tel, col=rainbow(length(unique(calibration.data$`tag-local-identifier`))))

# The uere command is used to estimate the RMS UERE parameter(s) from calibration data.
# Do not use this command on animal tracking data.

UERE <- uere.fit(calibration.tel)
summary(UERE)

UERE$DOF
# The UERE parameters can then be assigned to a dataset with the uere()<- command.

# Error model selection

t.noHDOP  <- lapply(calibration.tel, function(t){ t$HDOP  <- NULL; t })
t.noclass <- lapply(calibration.tel, function(t){ t$class <- NULL; t })
t.nothing <- lapply(calibration.tel, function(t){ t$HDOP  <- NULL; t$class <- NULL; t })

length(t.noHDOP)

UERE.noHDOP  <- uere.fit(t.noHDOP[1:5])
UERE.noclass <- uere.fit(t.noclass[1:5])
UERE.nothing <- uere.fit(t.nothing[1:5])

summary(list(HDOP.class=UERE,class=UERE.noHDOP,HDOP=UERE.noclass,homoskedastic=UERE.nothing))
# HDOP values yield the best error model

UERES <- lapply(calibration.tel, uere.fit)
summary(list(joint = UERE, individual = UERES)) #  performance of the individualized and joint error models

# ------------------------------------------------------------------------------


# 2. Second approach - remove outliers
# ------------------------------------------------------------------------------
# Fit plausible error models and Select best error models

calibration.data_2 <- calibration.data %>% dplyr::filter(`gps:satellite-count` >= 3)

unique(calibration.data_2$`gps:fix-type-raw`)
calibration.data_2 %<>% dplyr::mutate(class = "3D")

calibration.tel_2 <- as.telemetry(calibration.data_2, datum = 'EPSG:4326')

plot(calibration.tel_2)

plot(calibration.tel_2, col=rainbow(length(unique(calibration.data_2$`tag-local-identifier`))))


UERE <- uere.fit(calibration.tel_2)
summary(UERE)

UERE$UERE
UERE$DOF

# calculate residuals of calibration data
RES <- lapply(calibration.tel_2, residuals)

# scatter plot of residuals with 50%, 95%, and 99.9% coverage areas
plot(RES, col.DF = NA, level.UD = c(0.50,0.95,0.999))

# check calibration data for autocorrelation using fast=FALSE because samples are small
ACFS <- lapply(RES, function(R){correlogram(R, fast = FALSE, dt = 10 %#% 'min')})

# pooling ACFs
ACF <- mean(ACFS)

plot(ACF)


# Error model selection

t.noHDOP  <- lapply(calibration.tel_2, function(t){ t$HDOP  <- NULL; t })
t.noclass <- lapply(calibration.tel_2, function(t){ t$class <- NULL; t })
t.nothing <- lapply(calibration.tel_2, function(t){ t$HDOP  <- NULL; t$class <- NULL; t })

UERE.noHDOP  <- uere.fit(t.noHDOP)
UERE.noclass <- uere.fit(t.noclass)
UERE.nothing <- uere.fit(t.nothing)

summary(list(HDOP.class=UERE,class=UERE.noHDOP,HDOP=UERE.noclass,homoskedastic=UERE.nothing))
# We can see that the combination of location class and HDOP values yield the best error model,
# both in terms of AICc and in terms of reduced Z squared

UERES <- lapply(calibration.tel_2, uere.fit) # Sample size too small for AICc to exist
summary(list(joint = UERE, individual = UERES)) #  performance of the individualized and joint error models

# joint model!

# Save the calibration error model
# ------------------------------------------------------------------------------

# saveRDS(UERE, "Data/undep/calibration.error.model.rds")

UERE <- readRDS("Data/undep/calibration.error.model.rds")

summary(UERE)


# Next step: Calibrate tracking data and remove outliers
# and then: Fit movement parameters with error-informed analysis

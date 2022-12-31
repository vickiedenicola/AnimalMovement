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


# This script is used mainly to explore and create calibration dataset from deployed dataset on movebank.
# Main goal is to produce calibration error model 
# Next step is to calibrate tracking data and remove outliers
# and with final goal to Fit movement parameters with error-informed analysis

# Workflow is available in paper A comprehensive framework for handling location error in animal tracking data, Fleming et. al, 2021 - Figure 1

# Calibration data
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Read the dataset downloaded from movebank
cal.data <- data.table::fread("Data/undep/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

# now many unique tags - collars are there
unique(cal.data$`tag-local-identifier`)
range(cal.data$timestamp) # strange, need to be checked

# Based on our data analysis, we determine that calibration data are available between these days
# 3 august - 10 august calibration data

# divide timestamp column to Date and Time and then Date to Year, Month and Day
cal.data %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4), # with substring you need to specify position of first and last character of interest
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))


# start: 2021-08-01 00:00:00
# end: 2021-09-15 00:00:00
# Just check
# cal.data %>% dplyr::filter(between(Date, as.Date('2021-08-01'), as.Date('2021-09-07')))

# Remove events without coordinates (longitude or latitude)
cal.data %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0)) # - 6556  (366394)

# cal.data %<>% dplyr::filter(`gps:fix-type-raw` == "3D valid GPS Fix") %>%
#   dplyr::filter(!is.na(`gps:satellite-count`))

# Make spatial class from data
cal.data.sf <- st_as_sf(cal.data, coords = c("location-long", "location-lat"), crs = 4326)
# mapview(cal.data.sf) # view it interactivly

# Read extent of calibration data (this extent - polygon was drawn manullay in QGIS)
cal.ext <- sf::st_read("Data/undep/calibration_data_extent.gpkg") # read as "sf" spatial object

# Spatial intersection between points and spatial extent - polygon 
# Result is that we keep only points inside the polygon
cal.data.sf <- cal.data.sf[cal.ext, ] 

# Filter out events with nuber of satellites less than 3 (we need to do this because it can affect data reliability)
cal.data.sf %<>% dplyr::filter(`gps:satellite-count` >= 3)

# Remove manually some events that is far away from centroid of events (group of events)
dim(cal.data.sf)
rem <- c(23489397510, 23489397511, 23522097486, 23487492456)
cal.data.sf %<>% dplyr::filter(!(as.character(`event-id`) %in% as.character(rem)))

cal.data.sf
mapview(cal.data.sf) # view data on the map interactivly

# Filter data between dates for which the data is considered to have been collected
range(cal.data.sf$timestamp)
cal.data.sf %<>% dplyr::filter(between(Date, as.Date('2021-08-01'), as.Date('2021-09-15'))) 

# List unique values for GPS fix type
unique(cal.data.sf$`gps:fix-type-raw`)

# and create summary table with n - number of events per fix type collected
cal.data.sf %>% dplyr::group_by(`gps:fix-type-raw`) %>%
  dplyr::summarise(n_per_type = n())

# Make data frame back from sf - spatial class points
calibration.data <- cal.data.sf %>% dplyr::mutate(`location-long` = st_coordinates(.)[, 1], # create columns - attributes with coordinates
                                                  `location-lat` = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>% # remove spatial geometry column
  as.data.frame() # make dataframe

# write.csv(calibration.data, "Data/undep/calibration_data.filtered.csv") # write result to CSV file

# ------------------------------------------------------------------------------

# Read back calibration dataset - events that are collected form GPS collar from few days at Vickies house - yard 
calibration.data1 <- read.csv(file = "Data/undep/calibration_data.filtered.csv",
                             stringsAsFactors = FALSE)

# Check the unique values 
unique(calibration.data$individual.local.identifier)
unique(calibration.data$tag.local.identifier)
unique(calibration.data$transmission.protocol)

# Rename columns
calibration.data %<>% dplyr::rename(`location-long` = location.long,
                                    `location-lat` = location.lat)

# Make sf - spatial class object from data
cal.data.sf <- st_as_sf(calibration.data, # specify dataset
                        coords = c("location-long", # specify columns with coordinates
                                   "location-lat"), # specify Coordinate reference system
                        crs = 4326)

# view data interactive on web map
mapview(cal.data.sf, # specify data - sf spatial object 
        zcol = "tag.local.identifier", # specify column with which you want to color the points by value
        layer.name = "Tag ID") # optional: specify layer name which will be displayed in map legend

mapview(cal.data.sf, # specify data - sf spatial object
        zcol = "tag.local.identifier", # specify column with which you want to color the points by value
        layer.name = "Tag ID", # optional: specify layer name which will be displayed in map legend
        burst = TRUE) # specify if you want to display the points on the map as one layer or separately per tag.local.identifier values


# some analysis
# ------------------------------------------------------------------------------

range(calibration.data$timestamp)

calibration.data %>% dplyr::filter(tag.local.identifier == 47781)

# Filter data by transmission protocol
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
#           2. some previous data cleaning - remove outliers - USED and produced final Error model


# This is second part, first part (above) was mainly focused on exploration and analysis

# Read the dataset 
cal.data <- data.table::fread("Data/undep/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame() # make dataframe

# Make Date columns
cal.data %<>% dplyr::mutate(
  Date = as.Date(timestamp), # Divide timestamp column to Date and Time columns
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4), # Divide Date column to Year, Month and Day columns
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
mapview(cal.data.sf, zcol = "Day") # view the data on web map

# Make data.frame back from spatial class object
calibration.data <- cal.data.sf %>% dplyr::mutate(`location-long` = st_coordinates(.)[, 1], # make coordinates column
                                                  `location-lat` = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>% # remove spatial geometry column
  as.data.frame() # make dataframe

# Some analysis - not necessary
# dim(calibration.data)
# unique(calibration.data$`gps:fix-type-raw`)
# range(calibration.data$`gps:dop`)
# calibration.data %>% dplyr::filter(`gps:dop` > 5)

# Check for duplicated events
dupl.check <- calibration.data %>%
  dplyr::group_by(`tag-local-identifier`) %>% # group the data by tag id
  dplyr::summarize(dupl_count = sum(duplicated(timestamp))) # summarise and make column with sum of duplicated events

range(dupl.check$dupl_count)

# Remove duplicated events and keep only distinct events
calibration.data %<>% 
  dplyr::group_by(`tag-local-identifier`) %>% # group by tag - collar id 
  distinct(timestamp, .keep_all = TRUE) %>% # distinct - remove duplicated events
  ungroup() %>% # ungroup the data - bind all groups together
  as.data.frame() # make dataframe 

# get unique values of individuals
unique(calibration.data$`individual-local-identifier`)

# IMPORTANT
calibration.data %<>% dplyr::select(-`individual-local-identifier`) # individual id must be dropped,
# in that case tag id will not be recognized all events will be treated as one telemetry object

calibration.data %<>% dplyr::mutate(class = `gps:fix-type-raw`) # make class column


# 1. First approach
# ------------------------------------------------------------------------------

# make telemetry object from data
calibration.tel <- as.telemetry(calibration.data, # specify dataset with tag id as main column (it will separate data - like unique objects in telemetry class)
                                datum = 'EPSG:4326') # specify coordinate reference system datum

plot(calibration.tel)
names(calibration.tel)

# Plot the data
plot(calibration.tel, col=rainbow(length(unique(calibration.data$`tag-local-identifier`))))

# The uere command is used to estimate the RMS UERE parameter(s) from calibration data.
# Do not use this command on animal tracking data.

# Fit the model
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

# Filter by number of satellites - very important because it can affect reliablity and data precision
calibration.data_2 <- calibration.data %>% dplyr::filter(`gps:satellite-count` >= 3)

# Assign class
unique(calibration.data_2$`gps:fix-type-raw`)
calibration.data_2 %<>% dplyr::mutate(class = "3D")

# Make telemetry object
calibration.tel_2 <- as.telemetry(calibration.data_2, datum = 'EPSG:4326')

# Plots
plot(calibration.tel_2)
plot(calibration.tel_2, col=rainbow(length(unique(calibration.data_2$`tag-local-identifier`))))

# Fit the model
UERE <- uere.fit(calibration.tel_2)
summary(UERE) # better results

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

# We choose joint error model!

# Save the calibration error model
# ------------------------------------------------------------------------------

# saveRDS(UERE, "Data/undep/calibration.error.model.rds")

UERE <- readRDS("Data/undep/calibration.error.model.rds")

summary(UERE)


# Next step: Calibrate tracking data and remove outliers
# and then: Fit movement parameters with error-informed analysis





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


# Read the dataset
# ------------------------------------------------------------------------------

data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_new.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()



names(data.raw)


length(unique(data.raw$`tag-local-identifier`)) 
length(unique(data.raw$`individual-local-identifier`)) 


# Remove events without coordinates
# ------------------------------------------------------------------------------
data.raw %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0))
data.raw %<>% dplyr::filter(!(`gps:fix-type-raw` == "NO FIX"))

# Add date column and filter between dates of interest for this study
# ------------------------------------------------------------------------------
data.raw %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))

data.raw %<>% dplyr::filter(between(Date, as.Date('2021-09-15'), as.Date('2022-05-30'))) # 341456 events


# Read table with additional information per individual
# ------------------------------------------------------------------------------
study_animals <- readxl::read_xlsx("Data/Study_Animals.xlsx" , sheet = "Sheet2") %>%
  as.data.frame()


study_animals %<>%
  dplyr::mutate(Color = str_sub(ID, - 1, - 1),
                ID = tolower(sub('[YW]$', '', ID)))

# Filter individual ids and Join
# ------------------------------------------------------------------------------

unique(data.raw$`individual-local-identifier`) %in% study_animals$ID
sum(unique(data.raw$`individual-local-identifier`) %in% study_animals$ID)

data.raw %<>% dplyr::filter(`individual-local-identifier` %in% study_animals$ID)
data.raw %<>% left_join(., study_animals %>% dplyr::select(ID, `study area`, sex, Color), by = c("individual-local-identifier" = "ID"))
data.raw %<>% dplyr::rename(study.area = `study area`)

# Remove null GPS fixes, Remove obvious outliers, Remove 2D fixes
# ------------------------------------------------------------------------------

bad_gps_fixes <- c("3D Validated", "3D", "2D GPS Fix", "2D", "1 GPS Sat", "2 GPS Sat")
good_gps_fixes <- c("3D valid GPS Fix", "3D GPS Fix")

data.raw %<>% dplyr::filter(!(`gps:fix-type-raw` %in% bad_gps_fixes))

# Filter by altitude
# ------------------------------------------------------------------------------
data.raw %<>% dplyr::filter(!(`height-above-ellipsoid` < -500))
data.raw %<>% dplyr::filter(!(`height-above-ellipsoid` > 500))

# Filter by gps:sat and gps:dop
# ------------------------------------------------------------------------------
range(data.raw$`gps:satellite-count`)
range(data.raw$`gps:dop`)

data.raw %>% dplyr::group_by(`gps:satellite-count`) %>% dplyr::summarize(n_count = n())
data.raw %>% dplyr::group_by(`gps:dop`) %>% dplyr::summarize(n_count = n())

dim(data.raw %>% dplyr::filter(`gps:dop` > 5)) # more than 32k events... not for now

data.raw %<>% dplyr::filter(`gps:satellite-count` >= 3)

# Remove any duplicate fixes
# ------------------------------------------------------------------------------
# Check for duplicated events
dupl.check <- data.raw %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(dupl_count = sum(duplicated(timestamp)))

range(dupl.check$dupl_count) # 0 duplicated timestamps per individual

# data.raw %<>%
#   dplyr::group_by(`individual-local-identifier`) %>%
#   distinct(timestamp, .keep_all = TRUE) %>%
#   ungroup() %>%
#   as.data.frame()

# Check for duplicated events
dupl.check_tag <- data.raw %>%
  dplyr::group_by(`tag-local-identifier`) %>%
  dplyr::summarize(dupl_count = sum(duplicated(timestamp)))

range(dupl.check_tag$dupl_count) # 0 duplicated timestamps per tag (collar) id

# Remove fixes that are less than 5 minutes apart
# ------------------------------------------------------------------------------
elapsed <- function(x) {
  y <- abs(as.duration(x[2:length(x)] %--% x[1:(length(x)-1)]))
  y >= 5*60}

dim(data.raw)

data.raw %<>%
  tibble::rowid_to_column(., "Row") %>%
  group_split(`individual-local-identifier`) %>%
  map_dfr(~ .[c(T, if (nrow(.) > 1) elapsed(.$timestamp)),]) %>%
  arrange(Row) %>%
  as.data.frame()
# 8 events removed

# Format timestamp attribute and add class column
# ------------------------------------------------------------------------------
data.raw %<>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
                class = "3D")


# Additional check for: GPS collars are set to collect 1 fix/hour
# ------------------------------------------------------------------------------
# be sure that there is record for each hour
timeline <- seq(
  from = as.POSIXct("2021-09-15 00:00", tz = "UTC"),
  to = as.POSIXct("2022-05-30 24:00", tz = "UTC"),
  by = "hour")

hour.check <- data.raw %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(n_hours_recorded = n_distinct(timestamp),
                   start = min(timestamp),
                   end = max(timestamp),
                   n_hours_total = round(as.numeric(difftime(end, start, units = "hours")), 0),
                   diff_hours = n_hours_total - n_hours_recorded,
                   diff_days = round(diff_hours/24, 0)) %>%
  as.data.frame()

hour.check # for some ids there is a lot of missing hours

# hour check per collar/tag id
hour.check.tag <- data.raw %>%
  dplyr::group_by(`tag-local-identifier`) %>%
  dplyr::summarize(n_hours_recorded = n_distinct(timestamp),
                   start = min(timestamp),
                   end = max(timestamp),
                   n_hours_total = round(as.numeric(difftime(end, start, units = "hours")), 0),
                   diff_hours = n_hours_total - n_hours_recorded,
                   diff_days = round(diff_hours/24, 0)) %>%
  as.data.frame()

hour.check.tag # for some ids there is a lot of missing hours
hour.check.tag_old <- hour.check.tag
hour.check.tag_new <- hour.check.tag

writexl::write_xlsx(hour.check.tag_old, "Data/hour.check_per.tag.id_old.xlsx")
writexl::write_xlsx(hour.check.tag_new, "Data/hour.check_per.tag.id_new.xlsx")



# Which devices are still missing data from the collar (ie., devices that only have 1 fix/every 4 hours)
# ------------------------------------------------------------------------------
# need to check missing data for those 4 collars and to be sure that there is data every one hour
# and check collars for others and 1hour fix 

# Which collars only have 1 fix/every 4 hours (and are still missing data)

# 22/47475
# 57/47474
# 38/47790
# 33/47809


length(unique(data.raw$`tag-local-identifier`)) # 54 3 missing - when it is filtered
length(unique(data.raw$`individual-local-identifier`)) # 58 1 added - when it is filtered


unique(data.raw$`transmission-protocol`) # ""            "Iridium TCP" "Collar"

count.per.transmission.protocol <- data.raw %>% dplyr::group_by(`tag-local-identifier`) %>%
  dplyr::summarize(colar_count = sum(`transmission-protocol` == "Collar"), 
                   iridium_count = sum(`transmission-protocol` == "Iridium TCP"), 
                   blank_count = sum(`transmission-protocol` == ""))

count.per.transmission.protocol %>% dplyr::filter(colar_count == 0) %>% as.data.frame()

# calculating average time between events by group 

# data.raw %>%
#   slice(1:100000) %>%
#   arrange(`tag-local-identifier`, timestamp) %>%
#   group_by(`tag-local-identifier`) %>%
#   mutate(group = rep(row_number(), each = 2, length.out = n())) %>%
#   group_by(group, .add = TRUE) %>%
#   summarise(avg_sec = difftime(timestamp[2], timestamp[1], units = 'secs')) %>%
#   mutate(avg_sec = as.numeric(mean(avg_sec)), 
#          avg_min = avg_sec/60, 
#          avg_hour = avg_min/60, 
#          avg_day = avg_hour/24) -> result
# 
# result %>% dplyr::filter(`tag-local-identifier` == 47457)
# 
# result %>% dplyr::summarise(avg_hour = mean(avg_hour, na.rm = TRUE)) -> result_tag


average_time <-
  data.raw %>% 
  group_by(`tag-local-identifier`) %>% 
  mutate(timestamp_Next = lead(timestamp)) %>% 
  # ungroup() %>% 
  mutate(
    diff_days = difftime(timestamp_Next, timestamp, units = 'days'),
    diff_hours = difftime(timestamp_Next, timestamp, units = 'hours'),
    diff_mins = difftime(timestamp_Next, timestamp, units = 'mins'),
    diff_secs = difftime(timestamp_Next, timestamp, units = 'secs')
  ) %>% 
  dplyr::summarise(avg_hour = mean(diff_hours, na.rm = TRUE))

average_time %>% dplyr::filter(avg_hour > 1.5)

average_time %>% 
  ggplot() +
  geom_density(aes(x = avg_hour), lwd = 1)

ggplot() +
  geom_point(data = average_time, aes(y = avg_hour, x = `tag-local-identifier`)) +
  geom_text(data = average_time %>% dplyr::filter(avg_hour > 2), aes(y = avg_hour, x = `tag-local-identifier`, label = `tag-local-identifier`), color = "red", nudge_y = 0.2, nudge_x = 0.2)


# data.raw %>% dplyr::filter(`tag-local-identifier` == 18)


# TODO check undeployed data


# Write the results
# ------------------------------------------------------------------------------

# write.csv(data.raw, "Data/processed/dataset.Odocoileus.virginianus.filtered.csv")

# ------------------------------------------------------------------------------



# Read the cleaned dataset
# ------------------------------------------------------------------------------

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% as.data.frame()

range(data.raw$timestamp)

# Split dataset to control and treatment site
# ------------------------------------------------------------------------------
data.SI <- data.raw %>%
  dplyr::filter(study.area == "Staten Island")

data.RF <- data.raw %>%
  dplyr::filter(study.area == "Rockefeller")

plot_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"))
plot_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44"))

map_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"), burst = TRUE)
map_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44", "62", "47"), burst = TRUE)

length(unique(data.raw$`tag-local-identifier`))
length(unique(data.raw$`individual-local-identifier`))




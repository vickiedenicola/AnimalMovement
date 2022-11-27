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


# Read the dataset
# ------------------------------------------------------------------------------

# Which devices are still missing data from the collar (ie., devices that only have 1 fix/every 4 hours)
# ------------------------------------------------------------------------------
# need to check missing data for those 4 collars and to be sure that there is data every one hour
# and check collars for others and 1hour fix 

# Which collars only have 1 fix/every 4 hours (and are still missing data)

# 22/47475
# 57/47474
# 38/47790
# 33/47809

# New deployed data
# ------------------------------------------------------------------------------
data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_new.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()


# New undeployed data
# ------------------------------------------------------------------------------
data.raw <- data.table::fread("Data/undep/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_undep_new.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()


# New undeployed data with UTM
# ------------------------------------------------------------------------------
data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_new_with_UTM.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE) %>%
  as.data.frame()

# EPSG:32618 WGS 84 / UTM zone 18N



length(unique(data.raw$`tag-local-identifier`)) # 54 3 missing - when it is filtered
length(unique(data.raw$`individual-local-identifier`)) # 58 1 added - when it is filtered
dim(data.raw)

unique(data.raw$`transmission-protocol`) # ""            "Iridium TCP" "Collar"

count.per.transmission.protocol <- data.raw %>% dplyr::group_by(`tag-local-identifier`) %>%
  dplyr::summarize(colar_count = sum(`transmission-protocol` == "Collar"), 
                   iridium_count = sum(`transmission-protocol` == "Iridium TCP"), 
                   blank_count = sum(`transmission-protocol` == ""))

count.per.transmission.protocol %>% dplyr::filter(colar_count == 0) %>% as.data.frame()

count.per.transmission.protocol %>% dplyr::filter(`tag-local-identifier` %in% c(47474, 47475, 47778, 47790, 47809)) %>% as.data.frame()

# ------------------------------------------------------------------------------


analysis.table <- data.raw %>% dplyr::group_by(`tag-local-identifier`, `transmission-protocol`) %>%
  # dplyr::summarize(colar_count = sum(`transmission-protocol` == "Collar"), 
  #                  iridium_count = sum(`transmission-protocol` == "Iridium TCP"), 
  #                  blank_count = sum(`transmission-protocol` == "")) %>% 
  mutate(timestamp_Next = lead(timestamp)) %>% 
  
  mutate(
    diff_days = difftime(timestamp_Next, timestamp, units = 'days'),
    diff_hours = difftime(timestamp_Next, timestamp, units = 'hours'),
    diff_mins = difftime(timestamp_Next, timestamp, units = 'mins'),
    diff_secs = difftime(timestamp_Next, timestamp, units = 'secs')
  ) %>% 
  dplyr::summarise(avg_hour = mean(diff_hours, na.rm = TRUE), 
                   n_count = n())


analysis.table %>% dplyr::filter(`tag-local-identifier` %in% c(47474, 47475, 47778, 47790, 47809)) %>% as.data.frame() -> analysis.table_1

analysis.table_1

writexl::write_xlsx(analysis.table_1, "Data/missing_collar_data_stats.xlsx")

# ------------------------------------------------------------------------------

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

# writexl::write_xlsx(average_time, "Data/missing_collar_data.xlsx")

average_time %>% dplyr::filter(avg_hour > 2)

average_time %>% 
  ggplot() +
  geom_density(aes(x = avg_hour), lwd = 1)

ggplot() +
  geom_point(data = average_time, aes(y = avg_hour, x = `tag-local-identifier`)) +
  geom_text(data = average_time %>% dplyr::filter(avg_hour > 2), aes(y = avg_hour, x = `tag-local-identifier`, label = `tag-local-identifier`), color = "red", nudge_y = 0.2, nudge_x = 0.2)


# data.raw %>% dplyr::filter(`tag-local-identifier` == 18)



data.raw



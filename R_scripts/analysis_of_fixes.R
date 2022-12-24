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

average_time$`individual-local-identifier` <- data.raw$`individual-local-identifier`[match(average_time$`tag-local-identifier`, data.raw$`tag-local-identifier`)]

average_time %>% 
  ggplot() +
  geom_density(aes(x = avg_hour), lwd = 1)

ggplot() +
  geom_point(data = average_time, aes(y = avg_hour, x = `tag-local-identifier`)) +
  geom_text(data = average_time %>% dplyr::filter(avg_hour > 2), aes(y = avg_hour, x = `tag-local-identifier`, label = `individual-local-identifier`), color = "red", nudge_y = 0.2, nudge_x = 0.2)


# data.raw %>% dplyr::filter(`tag-local-identifier` == 18)



data.raw


names(data.raw)






# Analysis - 2022-12-16
# ------------------------------------------------------------------------------


# data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
#   as.data.frame()

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered_NEW.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  as.data.frame()


range(data.raw$timestamp)


# Which individuals has more that one collar

n_collars <- data.raw %>% 
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(n_collars = length(unique(`tag-local-identifier`)),
                   tag = unique(`tag-local-identifier`))


n_collars %>% dplyr::filter(n_collars > 1)

# ==> all individuals has unique collar 


# ------------------------------------------------------------------------------

# New periods

# Breeding 16 OCT - 31 DEC
# Post Breeding 1 JAN - 15 MAR
# Baseline 16 MAR - 30 MAY


# Check per individual - how many days are there per period

time_periods <- data.frame(period_name = c("Breeding", "Post Breeding", "Baseline"),
                           start_date = c("2021-10-16", "2022-01-01", "2022-03-16"),
                           end_date = c("2021-12-31", "2022-03-15", "2022-05-30"))

tm <- time_periods %>% dplyr::filter(period_name == "Breeding")

n_dates_breeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(n_dates_breeding = length(unique(Date)))

tm <- time_periods %>% dplyr::filter(period_name == "Post Breeding")

n_dates_postbreeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(n_dates_postbreeding = length(unique(Date)))

tm <- time_periods %>% dplyr::filter(period_name == "Baseline")

n_dates_baseline <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(n_dates_baseline = length(unique(Date)))


n_dates_per_period <- n_dates_breeding %>%
  left_join(n_dates_postbreeding, by = "individual-local-identifier") %>%
  left_join(n_dates_baseline, by = "individual-local-identifier") %>%
  as.data.frame()


n_dates_per_period %<>% replace(is.na(.), 0)
# Add study area
n_dates_per_period$study.area <- data.raw$study.area[match(n_dates_per_period$`individual-local-identifier`, data.raw$`individual-local-identifier`)]
# Add sex
n_dates_per_period$sex <- data.raw$sex[match(n_dates_per_period$`individual-local-identifier`, data.raw$`individual-local-identifier`)]

library(viridis)

gg.si.m <- n_dates_per_period %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [MALES]") +
  xlab("")+
  ylab("")

gg.si.f <- n_dates_per_period %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [FEMALES]") +
  xlab("")+
  ylab("")

gg.rf.m <- n_dates_per_period %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [MALES]") +
  xlab("") +
  ylab("")


gg.rf.f <- n_dates_per_period %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [FEMALES]") +
  xlab("")+
  ylab("")


gg1 <- gridExtra::grid.arrange(gg.si.m, gg.si.f, gg.rf.m, gg.rf.f, ncol = 2)


ggsave(plot = gg1,
       filename = "Analysis/ndays_per_period_per_site_per_sex_before_removing_first5andlast2.jpg",
       width = 47,
       height = 35,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# ------------------------------------------------------------------------------

# Check per individual - how many days are there per period if we remove -5 and -2 days
# ------------------------------------------------------------------------------

tm <- time_periods %>% dplyr::filter(period_name == "Breeding")

n_dates_breeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::arrange(Date) %>%
  dplyr::distinct(Date) %>%
  dplyr::summarize(date_first_breeding = min(Date),
                date_last_breeding = max(Date),
                date_first_plus_five_breeding = Date[6],
                date_last_minus_two_breeding = Date[length(Date) - 2],
                n_dates_breeding = length(Date[6:(length(Date) - 2)]))

avg_h_breeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_breeding = mean(diff_hours, na.rm = TRUE))


tm <- time_periods %>% dplyr::filter(period_name == "Post Breeding")

n_dates_postbreeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>% 
  dplyr::arrange(Date) %>%
  dplyr::distinct(Date) %>%
  dplyr::summarize(date_first_postbreeding = min(Date),
                   date_last_postbreeding = max(Date),
                   date_first_plus_five_postbreeding = Date[6],
                   date_last_minus_two_postbreeding = Date[length(Date) - 2],
                   n_dates_postbreeding = length(Date[6:(length(Date) - 2)]))

avg_h_postbreeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_postbreeding = mean(diff_hours, na.rm = TRUE))


tm <- time_periods %>% dplyr::filter(period_name == "Baseline")

n_dates_baseline <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>% 
  dplyr::arrange(Date) %>%
  dplyr::distinct(Date) %>%
  dplyr::summarize(date_first_baseline = min(Date),
                   date_last_baseline = max(Date),
                   date_first_plus_five_baseline = Date[6],
                   date_last_minus_two_baseline = Date[length(Date) - 2],
                   n_dates_baseline = length(Date[6:(length(Date) - 2)]))


avg_h_baseline <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_baseline = mean(diff_hours, na.rm = TRUE))


n_dates_per_period_new <- n_dates_breeding %>%
  left_join(n_dates_postbreeding, by = "individual-local-identifier") %>%
  left_join(n_dates_baseline, by = "individual-local-identifier") %>%
  as.data.frame()


n_dates_per_period_new %<>% replace(is.na(.), 0)

# Add study area
n_dates_per_period_new$study.area <- data.raw$study.area[match(n_dates_per_period_new$`individual-local-identifier`, data.raw$`individual-local-identifier`)]
# Add sex
n_dates_per_period_new$sex <- data.raw$sex[match(n_dates_per_period_new$`individual-local-identifier`, data.raw$`individual-local-identifier`)]

# Add average hours
n_dates_per_period_new$avg_hour_breeding <- avg_h_breeding$avg_hour_breeding[match(n_dates_per_period_new$`individual-local-identifier`, avg_h_breeding$`individual-local-identifier`)]
n_dates_per_period_new$avg_hour_postbreeding <- avg_h_postbreeding$avg_hour_postbreeding[match(n_dates_per_period_new$`individual-local-identifier`, avg_h_postbreeding$`individual-local-identifier`)]
n_dates_per_period_new$avg_hour_baseline <- avg_h_baseline$avg_hour_baseline[match(n_dates_per_period_new$`individual-local-identifier`, avg_h_baseline$`individual-local-identifier`)]




names(n_dates_per_period_new)
n_dates_per_period_new %<>% dplyr::select(`individual-local-identifier`, n_dates_breeding, n_dates_postbreeding, n_dates_baseline, study.area, sex)

gg.si.m <- n_dates_per_period_new %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [MALES]") +
  xlab("")+
  ylab("")

gg.si.f <- n_dates_per_period_new %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [FEMALES]") +
  xlab("")+
  ylab("")

gg.rf.m <- n_dates_per_period_new %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [MALES]") +
  xlab("") +
  ylab("")


gg.rf.f <- n_dates_per_period_new %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [FEMALES]") +
  xlab("")+
  ylab("")


gg2 <- gridExtra::grid.arrange(gg.si.m, gg.si.f, gg.rf.m, gg.rf.f, ncol = 2)


ggsave(plot = gg2,
       filename = "Analysis/ndays_per_period_per_site_per_sex_after_removing_first5andlast2_NEW.jpg",
       width = 47,
       height = 35,
       units = "cm",
       device = "jpeg",
       dpi = 700)

# ------------------------------------------------------------------------------





# generate new animal list per period (they pass these checks)
# ------------------------------------------------------------------------------
n_dates_per_period_new$n_dates_breeding_before <- n_dates_per_period$n_dates_breeding[match(n_dates_per_period_new$`individual-local-identifier`, n_dates_per_period$`individual-local-identifier`)]
n_dates_per_period_new$n_dates_postbreeding_before <- n_dates_per_period$n_dates_postbreeding[match(n_dates_per_period_new$`individual-local-identifier`, n_dates_per_period$`individual-local-identifier`)]
n_dates_per_period_new$n_dates_baseline_before <- n_dates_per_period$n_dates_baseline[match(n_dates_per_period_new$`individual-local-identifier`, n_dates_per_period$`individual-local-identifier`)]

n_dates_per_period_new %<>% replace(is.na(.), 0)


n_dates_per_period_new %<>% dplyr::rename(n_dates_breeding_after = n_dates_breeding,
                                          n_dates_postbreeding_after = n_dates_postbreeding,
                                          n_dates_baseline_after = n_dates_baseline) %>%
  dplyr::select(`individual-local-identifier`, 
                n_dates_breeding_before, n_dates_breeding_after, 
                avg_hour_breeding,
                n_dates_postbreeding_before, n_dates_postbreeding_after, 
                avg_hour_postbreeding,
                n_dates_baseline_before, n_dates_baseline_after, 
                avg_hour_baseline,
                study.area,
                sex,
                everything()) %>%
  dplyr::arrange(study.area, sex)

writexl::write_xlsx(n_dates_per_period_new, path = "Analysis/stats_ndays_per_period_per_site_per_sex_NEW.xlsx")

# ------------------------------------------------------------------------------

# How many are there 1 fix / 4 hours
# ------------------------------------------------------------------------------
n_dates_per_period_new %>% dplyr::filter(avg_hour_breeding > 1.5 | avg_hour_postbreeding > 1.5 | avg_hour_baseline > 1.5)




# and how many of them have less than 14 days of data per period
# ------------------------------------------------------------------------------

n_dates_per_period_new %>% dplyr::filter(n_dates_breeding_after %in% c(1:14) | n_dates_postbreeding_after %in% c(1:14) | n_dates_baseline_after %in% c(1:14))


# ------------------------------------------------------------------------------







# Is there any data for these ear tags (maybe they are the ones we are missing data for)?
# 629, 57, 38, 41, 22, 33
# ------------------------------------------------------------------------------

# New deployed data
# ------------------------------------------------------------------------------
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


tm <- time_periods %>% dplyr::filter(period_name == "Breeding")

avg_h_breeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_breeding = mean(diff_hours, na.rm = TRUE),
                   n_events_breeding = n(),
                   n_dates_breeding = length(unique(Date)))


tm <- time_periods %>% dplyr::filter(period_name == "Post Breeding")

avg_h_postbreeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_postbreeding = mean(diff_hours, na.rm = TRUE),
                   n_events_postbreeding = n(),
                   n_dates_postbreeding = length(unique(Date)))


tm <- time_periods %>% dplyr::filter(period_name == "Baseline")

avg_h_baseline <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_baseline = mean(diff_hours, na.rm = TRUE),
                   n_events_baseline = n(),
                   n_dates_baseline = length(unique(Date)))


avg_h <- avg_h_breeding %>%
  left_join(avg_h_postbreeding, by = "individual-local-identifier") %>%
  left_join(avg_h_baseline, by = "individual-local-identifier") %>%
  as.data.frame()

avg_h %<>% replace(is.na(.), 0)

miss <- c(629, 57, 38, 41, 22, 33)

avg_h %<>% 
  dplyr::filter(`individual-local-identifier` %in% miss)

# Add study area
avg_h$study.area <- data.raw$study.area[match(avg_h$`individual-local-identifier`, data.raw$`individual-local-identifier`)]
# Add sex
avg_h$sex <- data.raw$sex[match(avg_h$`individual-local-identifier`, data.raw$`individual-local-identifier`)]

writexl::write_xlsx(avg_h, "Analysis/individual.missing.stats.xlsx")

avg_h %<>% dplyr::select(n_dates_breeding, n_dates_postbreeding, n_dates_baseline, study.area, sex, `individual-local-identifier`)

gg.si.m <- avg_h %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [MALES]") +
  xlab("")+
  ylab("")

gg.si.f <- avg_h %>% 
  dplyr::filter(study.area == "Staten Island") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Staten Island [FEMALES]") +
  xlab("")+
  ylab("")

gg.rf.m <- avg_h %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "M") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "G", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [MALES]") +
  xlab("") +
  ylab("")


gg.rf.f <- avg_h %>% 
  dplyr::filter(study.area == "Rockefeller") %>%
  dplyr::filter(sex == "F") %>%
  dplyr::select(-study.area, -sex) %>%
  pivot_longer(
    -`individual-local-identifier`
  ) %>% 
  ggplot(aes(x = `individual-local-identifier`, y = value, fill = factor(name)))+
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color = "red") +
  scale_fill_viridis(option = "D", discrete = T, "Period: ") +
  ggtitle("Number of available dates per period - Rockefeller [FEMALES]") +
  xlab("")+
  ylab("")


gg1 <- gridExtra::grid.arrange(gg.si.m, gg.si.f, gg.rf.m, gg.rf.f, ncol = 2)

ggsave(plot = gg1,
       filename = "Analysis/ndays_per_period_per_site_per_sex_missing.jpg",
       width = 35,
       height = 25,
       units = "cm",
       device = "jpeg",
       dpi = 700)


count.per.transmission.protocol <- data.raw %>% dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::summarize(colar_count = sum(`transmission-protocol` == "Collar"), 
                   iridium_count = sum(`transmission-protocol` == "Iridium TCP"), 
                   blank_count = sum(`transmission-protocol` == ""))

count.per.transmission.protocol %>% dplyr::filter(colar_count == 0) %>% as.data.frame()

miss.count <- count.per.transmission.protocol %>% dplyr::filter(`individual-local-identifier` %in% miss) %>% as.data.frame()

writexl::write_xlsx(miss.count, "Analysis/data.missing_collar.stats.xlsx")


data.mis <- data.raw %>% dplyr::filter(`individual-local-identifier` %in% miss)
writexl::write_xlsx(data.mis, "Analysis/data.missing_collar.xlsx")

# 629 is added to data!


# Check individual 63 AKDE/Diffusion analysis (Baseline, male, control site)
# ------------------------------------------------------------------------------
source("R_scripts/functions.R")

data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered_NEW.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  as.data.frame()

check_table <- readxl::read_xlsx("Analysis/stats_ndays_per_period_per_site_per_sex_NEW.xlsx") %>%
  as.data.frame()

UERE <- readRDS("Data/undep/calibration.error.model.rds")
calibration_model <- UERE


time_periods <- data.frame(period_name = c("Breeding", "Post Breeding", "Baseline"),
                           start_date = c("2021-10-16", "2022-01-01", "2022-03-16"),
                           end_date = c("2021-12-31", "2022-03-15", "2022-05-30"))


dataset <- data.raw %>%
  dplyr::filter(study.area == "Rockefeller")

dataset %<>%
  dplyr::filter(sex == "M")

dataset %<>%
  dplyr::filter(`individual-local-identifier` %in% "63") %>%
  dplyr::filter(between(Date, as.Date(time_periods$start_date[3]), as.Date(time_periods$end_date[3])))
mapviewOptions(fgb = FALSE)
map_individual(df.site = dataset, ind.id = unique(dataset$`individual-local-identifier`), burst = TRUE)


# ------------------------------------------------------------------------------





# AKDE/Diffusion calculation for remaining individuals
# ------------------------------------------------------------------------------

missing.ind <- c(63, 57, 38, 41, 22, 33)

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

data.raw %<>% dplyr::filter(`individual-local-identifier` %in% missing.ind)

time_periods <- data.frame(period_name = c("Breeding", "Post Breeding", "Baseline"),
                           start_date = c("2021-10-16", "2022-01-01", "2022-03-16"),
                           end_date = c("2021-12-31", "2022-03-15", "2022-05-30"))


tm <- time_periods %>% dplyr::filter(period_name == "Breeding")

avg_h_breeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_breeding = mean(diff_hours, na.rm = TRUE),
                   n_events_breeding = n(),
                   n_dates_breeding = length(unique(Date)))


tm <- time_periods %>% dplyr::filter(period_name == "Post Breeding")

avg_h_postbreeding <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_postbreeding = mean(diff_hours, na.rm = TRUE),
                   n_events_postbreeding = n(),
                   n_dates_postbreeding = length(unique(Date)))


tm <- time_periods %>% dplyr::filter(period_name == "Baseline")

avg_h_baseline <- data.raw %>% 
  dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date))) %>%
  dplyr::group_by(`individual-local-identifier`) %>%
  dplyr::mutate(timestamp_Next = lead(timestamp)) %>% 
  dplyr::mutate(diff_hours = difftime(timestamp_Next, timestamp, units = 'hours')) %>% 
  dplyr::summarise(avg_hour_baseline = mean(diff_hours, na.rm = TRUE),
                   n_events_baseline = n(),
                   n_dates_baseline = length(unique(Date)))


avg_h <- avg_h_breeding %>%
  left_join(avg_h_postbreeding, by = "individual-local-identifier") %>%
  left_join(avg_h_baseline, by = "individual-local-identifier") %>%
  as.data.frame()

avg_h %<>% replace(is.na(.), 0)


# Add study area
avg_h$study.area <- data.raw$study.area[match(avg_h$`individual-local-identifier`, data.raw$`individual-local-identifier`)]
# Add sex
avg_h$sex <- data.raw$sex[match(avg_h$`individual-local-identifier`, data.raw$`individual-local-identifier`)]

# avg_h %<>% dplyr::select(`individual-local-identifier`, n_dates_breeding, n_dates_postbreeding, n_dates_baseline, study.area, sex)

avg_h

writexl::write_xlsx(avg_h, "Analysis/stats_only_akde_diff_calc.xlsx")




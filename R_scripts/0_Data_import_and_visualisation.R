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

# Read the dataset
# ------------------------------------------------------------------------------

data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE) %>%
  as.data.frame()


names(data.raw)

# Select and rename variables of interests

data.raw %<>% dplyr::select(`event-id`,
                           visible,
                           timestamp,
                           `location-long`,
                           `location-lat`,
                           `gps:fix-type-raw`,
                           `gps:satellite-count`,
                           `gps:dop`,
                           `height-above-ellipsoid`,
                           `mortality-status`,
                           `individual-taxon-canonical-name`,
                           `tag-local-identifier`,
                           `individual-local-identifier`)


data.raw %<>% dplyr::rename(event.id = `event-id`,
                            long = `location-long`,
                            lat = `location-lat`,
                            gps.fix = `gps:fix-type-raw`,
                            gps.sat = `gps:satellite-count`,
                            gps.dop = `gps:dop`,
                            altitude = `height-above-ellipsoid`,
                            mortality = `mortality-status`,
                            canonical.name = `individual-taxon-canonical-name`,
                            tag.local.id = `tag-local-identifier`,
                            individual.local.id = `individual-local-identifier`)


names(data.raw)

unique(data.raw$canonical.name) # Odocoileus virginianus

data.raw %<>% dplyr::select(-canonical.name)


# Remove events without coordinates / gps:NO FIX
# ------------------------------------------------------------------------------
summary(data.raw$timestamp)

dim(data.raw) # 373014     11
data.raw %<>% dplyr::filter(!(long == 0 | lat == 0)) # - 6556  (366394)
data.raw %<>% dplyr::filter(!(gps.fix == "NO FIX")) # - 9 (366385)

unique(data.raw$gps.sat)
data.raw %>% dplyr::filter(is.na(long)) # 0
data.raw %>% dplyr::filter(is.na(lat)) # 0

unique(data.raw$tag.local.id)
unique(data.raw$individual.local.id)

# Example
# --------------------------------------------
d.1 <- data.raw %>% dplyr::filter(individual.local.id %in% c("20a", "11", "15b"))
d.1.sf <- st_as_sf(d.1, coords = c("long", "lat"), crs = 4326)

mapview(d.1.sf, zcol = "individual.local.id")

# --------------------------------------------

# TODO:
# 1. Filter data by date range for this study
# 2. Filter data by individual.local.id which is of interest for this study (read another excel dataset and match individuals)
# 3. Add column for study area: control site - Rockefeller, treatment site - Staten Island and other columns
# 4. Also:
# - Remove null GPS fixes
# - Remove obvious outliers
# - Remove 2D fixes
# - Remove fixes that are less than 5 minutes apart
# - Remove any duplicate fixes
# --- GPS collars are set to collect 1 fix/hour

# 5. Add polygon boundaries for this sites
# 6. Visualisation and maps

# 1. Filter data by date range for this study
# ------------------------------------------------------------------------------

range(data.raw$timestamp)

data.raw %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))


# start: 2021-09-15 00:00:00
# end: 2022-05-30 00:00:00

data.raw %<>% dplyr::filter(between(Date, as.Date('2021-09-15'), as.Date('2022-05-30'))) # 341456 events




# 2. Filter data by individual.local.id which is of interest for this study (read another excel dataset and match individuals)
# ------------------------------------------------------------------------------

study_animals <- readxl::read_xlsx("Data/Study_Animals.xlsx" , sheet = "Sheet2") %>%
  as.data.frame()

library(stringr)

study_animals %<>%
  dplyr::mutate(Color = str_sub(ID, - 1, - 1),
                ID = tolower(sub('[YW]$', '', ID)))

dim(data.raw)
data.raw %<>% dplyr::filter(individual.local.id %in% study_animals$ID) # 317468 events
# control
length(unique(data.raw$individual.local.id)) # 57 ids # 63 ids new added
length(unique(study_animals$ID)) # 57 ids # 63 ids new added



# 3. Add column for study area: control site - Rockefeller, treatment site - Staten Island and other columns
# ------------------------------------------------------------------------------

data.raw %<>% left_join(., study_animals %>% dplyr::select(ID, `study area`, sex, Color), by = c("individual.local.id" = "ID"))
data.raw %<>% dplyr::rename(study.area = `study area`)

# data.SI <- data.raw %>%
#   dplyr::filter(study.area == "Staten Island")
#
# data.RF <- data.raw %>%
#   dplyr::filter(study.area == "Rockefeller")



# 4. Also: Remove null GPS fixes, Remove obvious outliers, Remove 2D fixes
# ------------------------------------------------------------------------------

unique(data.raw$gps.fix)

# check just 3D
d3 <- data.raw %>% dplyr::filter(gps.fix == "3D")
all(is.na(d3$altitude))

# check just 3D GPS Fix
d3 <- data.raw %>% dplyr::filter(gps.fix == "3D GPS Fix")
all(is.na(d3$altitude))
summary(d3$altitude) # it looks good

bad_gps_fixes <- c("3D Validated", "3D", "2D GPS Fix", "2D", "1 GPS Sat", "2 GPS Sat")
good_gps_fixes <- c("3D valid GPS Fix", "3D GPS Fix")

data.raw %<>% dplyr::filter(!(gps.fix %in% bad_gps_fixes)) # 289039 events

# Filter by altitude -20000  20000
summary(data.raw$altitude)
data.raw %<>% dplyr::filter(!(altitude < -500))
data.raw %<>% dplyr::filter(!(altitude > 500)) # 289024 events

# Filter by gps.sat
data.raw %>% dplyr::filter(gps.sat < 3) # 8509 events ? --- additional check

# Filter by gps.dop
data.raw %>% dplyr::filter(gps.dop > 5) # 30117 events ? --- additional check


# 4. Remove any duplicate fixes
# ------------------------------------------------------------------------------

# Check for duplicated events
dupl.check <- data.raw %>%
  dplyr::group_by(individual.local.id) %>%
  dplyr::summarize(dupl_count = sum(duplicated(timestamp)))

range(dupl.check$dupl_count) # great! there is no duplicated events by timestamp

# getDuplicatedTimestamps() # from move library (MoveBank)

# 4. Remove fixes that are less than 5 minutes apart
# ------------------------------------------------------------------------------

elapsed <- function(x) {
  y <- abs(as.duration(x[2:length(x)] %--% x[1:(length(x)-1)]))
  y >= 5*60}

data.raw %<>%
  tibble::rowid_to_column(., "Row") %>%
  group_split(individual.local.id) %>%
  map_dfr(~ .[c(T, if (nrow(.) > 1) elapsed(.$timestamp)),]) %>%
  arrange(Row) %>%
  as.data.frame()

# 289019 events (just 5 events less than 5 minutes apart)


# control - not working
temp.df <- data.raw %>% mutate(t_diff = timestamp - lag(timestamp))
if(units(temp.df$t_diff) == "secs"){
  units(temp.df$t_diff) <- "mins"
}

dim(temp.df)
temp.df <- temp.df %>% filter(t_diff > 5) # 56 events


# TODO: additional check for: GPS collars are set to collect 1 fix/hour
# ------------------------------------------------------------------------------
# be sure that there is record for each hour

timeline <- seq(
  from = as.POSIXct("2021-09-15 00:00", tz = "UTC"),
  to = as.POSIXct("2022-05-30 24:00", tz = "UTC"),
  by = "hour")

hour.check <- data.raw %>%
  dplyr::group_by(individual.local.id) %>%
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
  dplyr::group_by(tag.local.id) %>%
  dplyr::summarize(n_hours_recorded = n_distinct(timestamp),
                   start = min(timestamp),
                   end = max(timestamp),
                   n_hours_total = round(as.numeric(difftime(end, start, units = "hours")), 0),
                   diff_hours = n_hours_total - n_hours_recorded,
                   diff_days = round(diff_hours/24, 0)) %>%
  as.data.frame()

hour.check.tag # for some ids there is a lot of missing hours

writexl::write_xlsx(hour.check.tag, "Data/hour.check_per.tag.id.xlsx")


# TODO: additional check for: Remove obvious outliers
# TODO: Run basic analysis to ensure the data is not corrupted
# ------------------------------------------------------------------------------

# https://ctmm-initiative.github.io/ctmm/reference/outlie.html
# https://ctmm-initiative.github.io/ctmm/articles/error.html

# https://github.com/ctmm-initiative/ctmmweb

library(move)
library(ctmm)

#  Sufficient MoveBank columns include individual.local.identifier (or tag.local.identifier),
# timestamp, location.long and location.lat


data.raw %<>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
                class = "3D") %>%
  dplyr::rename(individual.local.identifier = individual.local.id,
                tag.local.identifier = tag.local.id,
                location.long = long,
                location.lat = lat,
                GPS.DOP = gps.dop)

# Split dataset to control and treatment site
data.SI <- data.raw %>%
  dplyr::filter(study.area == "Staten Island")

data.RF <- data.raw %>%
  dplyr::filter(study.area == "Rockefeller")

# Spatial class
# data.SI.sf <- st_as_sf(data.SI,
#                        coords = c("long", "lat"),
#                        crs = 4326)
# data.SI.sf %<>% st_transform(3857)
# data.SI <- data.SI.sf %>% dplyr::mutate(East = st_coordinates(.)[, 1],
#                                         North = st_coordinates(.)[, 2]) %>%
#   st_drop_geometry() %>%
#   as.data.frame()
#
# # data.SI.sp <- as(data.SI.sf, "Spatial")
#
#
# data.RF.sf <- st_as_sf(data.RF,
#                        coords = c("long", "lat"),
#                        crs = 4326)
# data.RF.sf %<>% st_transform(3857)
# data.RF <- data.RF.sf %>% dplyr::mutate(East = st_coordinates(.)[, 1],
#                                         North = st_coordinates(.)[, 2]) %>%
#   st_drop_geometry() %>%
#   as.data.frame()
# data.RF.sp <- as(data.RF.sf, "Spatial")


data.SI.tel <- as.telemetry(data.SI, datum = 'EPSG:4326')
data.RF.tel <- as.telemetry(data.RF)

# projection = 'EPSG:4326'

# colours
rainbow(length(data.SI.tel))
viridisLite::viridis(length(data.SI.tel), direction = 1)

plot(data.SI.tel, col = rainbow(length(data.SI.tel)))
plot(data.RF.tel, col = rainbow(length(data.RF.tel)))

# -------------------------------------------------
# Filter by HDOP
sapply(data.SI.tel, dim)
for(i in 1:length(data.SI.tel)){
  data.SI.tel[[i]] <- subset(data.SI.tel[[i]], data.SI.tel[[i]]$HDOP <= 5)
}

sapply(data.RF.tel, dim)
for(i in 1:length(data.RF.tel)){
  data.RF.tel[[i]] <- subset(data.RF.tel[[i]], data.RF.tel[[i]]$HDOP <= 5)
}

# -------------------------------------------------

outliers.SI <- outlie(data.SI.tel, plot = FALSE)
outliers.RF <- outlie(data.RF.tel, plot = FALSE)

plot(outliers.SI[[1]]) # 1 cm/s = 0.036 km/h   Units here:: m/s

for(i in 1:length(outliers.SI)){
  print(paste0("Max speed value: ", max(outliers.SI[[i]]$speed)))
  x <- which(outliers.SI[[i]]$speed >= 13.88889)
  print(length(x))
  if(length(x) > 0){
    data.SI.tel[[i]] <- data.SI.tel[[i]][-x, ]
  }
  print(paste0("Processed: ", i, "/", length(outliers.SI), " individuals"))
}

for(i in 1:length(outliers.RF)){
  print(paste0("Max speed value: ", max(outliers.RF[[i]]$speed)))
  x <- which(outliers.RF[[i]]$speed >= 13.88889)
  print(length(x))
  if(length(x) > 0){
    data.RF.tel[[i]] <- data.RF.tel[[i]][-x, ]
  }
  print(paste0("Processed: ", i, "/", length(outliers.RF), " individuals"))
}



# Visualisation
# ------------------------------------------------------------------------------
library(ggspatial)

unique(data.SI$individual.local.identifier)
unique(data.RF$individual.local.identifier)

# temp.SI <- data.SI %>% dplyr::filter(individual.local.identifier %in% c("17b", "20b"))
#
# temp.SI.sf <- st_as_sf(temp.SI, coords = c("location.long", "location.lat"), crs = 4326)
#
#
# pal1 <- viridisLite::viridis(length(unique(temp.SI.sf$individual.local.identifier)), direction = 1)
#
# tac_map <- ggplot() +
#   geom_sf(data = temp.SI.sf,
#           aes(color = individual.local.identifier), alpha = 0.3, size = 3) +
#   scale_color_manual(values = pal1,
#                      name = "ID: ") +
#   labs(# title = "",
#     # subtitle = "",
#     xlab = "Longitude [°]",
#     ylab = "Latitude [°]")+
#   theme(panel.grid = element_line(color = "black"),
#         panel.background = element_rect(fill = "white"),
#         axis.text.x = element_text(colour = "black"),
#         axis.text.y = element_text(colour = "black"),
#         legend.position="bottom")+
#   # geom_sf(data = gran_sf, color = "black", fill = NA) +
#   annotation_north_arrow(location = "tr", which_north = "true",
#                          pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering)+
#   coord_sf(crs = 4326)+
#   my_theme()
#
# tac_map

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 13),
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5),
      legend.title=element_text(size=11),
      legend.text=element_text(size=9)
    )
}

# Function to plot the spatial distribution of individuals
# --------------------------------------------------------

plot_individual <- function(df.site = df.site, ind.id = c()){

  df.site <- df.site %>% dplyr::filter(individual.local.identifier %in% ind.id)

  df.site.sf <- st_as_sf(df.site, coords = c("location.long", "location.lat"), crs = 4326)

  pal1 <- viridisLite::viridis(length(unique(df.site.sf$individual.local.identifier)), direction = 1)

  tac_map <- ggplot() +
    geom_sf(data = df.site.sf,
            aes(color = individual.local.identifier), alpha = 0.3, size = 3) +
    scale_color_manual(values = pal1,
                       name = "Individual ID: ") +
    labs(title = "[Animal spatial occurence] - Odocoileus virginianus",
      # subtitle = "",
      xlab = "Longitude [°]",
      ylab = "Latitude [°]")+
    theme(panel.grid = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          legend.position="bottom")+
    # geom_sf(data = gran_sf, color = "black", fill = NA) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(crs = 4326)+
    my_theme()

  return(tac_map)
}

plot_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"))
plot_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44"))


map_individual <- function(df.site = df.site, ind.id = c(), burst = FALSE){

  df.site <- df.site %>% dplyr::filter(individual.local.identifier %in% ind.id) %>%
    dplyr::select(-c(timestamp, Date, Time))

  df.site.sf <- st_as_sf(df.site, coords = c("location.long", "location.lat"), crs = 4326)

  map_tac <- mapview(df.site.sf,
                     zcol = "individual.local.identifier",
                     layer.name = "Individual ID",
                     burst = burst)

  return(map_tac)
}


map_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"))
map_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"), burst = TRUE)

map_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44", "62", "47"), burst = TRUE)



# --------------------------------------------------------
# Error models
# --------------------------------------------------------
# --------------------------------------------------------
# https://ctmm-initiative.github.io/ctmm/articles/error.html


# Process again with all columns
# (beacuse of as.telemetry function and calculation of precision parameters)
# It is the same as previous, right columns are taken into the account also with previous approach

data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY.csv",
                             stringsAsFactors = FALSE,
                             header = TRUE) %>%
  as.data.frame()

names(data.raw)


data.raw %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0)) # - 6556  (366394)
data.raw %<>% dplyr::filter(!(`gps:fix-type-raw` == "NO FIX")) # - 9 (366385)
data.raw %<>% dplyr::mutate(
  Date = as.Date(timestamp),
  Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
  # dplyr::select(-timestamp) %>%
  dplyr::mutate(Year = substr(Date, 1, 4),
                Month = substr(Date, 6, 7),
                Day = substr(Date, 9, 10))

data.raw %<>% dplyr::filter(between(Date, as.Date('2021-09-15'), as.Date('2022-05-30'))) # 341456 events

study_animals <- readxl::read_xlsx("Data/Study_Animals.xlsx" , sheet = "Sheet2") %>%
  as.data.frame()

library(stringr)

study_animals %<>%
  dplyr::mutate(Color = str_sub(ID, - 1, - 1),
                ID = tolower(sub('[YW]$', '', ID)))

data.raw %<>% dplyr::filter(`individual-local-identifier` %in% study_animals$ID) # 317468 events
data.raw %<>% left_join(., study_animals %>% dplyr::select(ID, `study area`, sex, Color), by = c("individual-local-identifier" = "ID"))
data.raw %<>% dplyr::rename(study.area = `study area`)

bad_gps_fixes <- c("3D Validated", "3D", "2D GPS Fix", "2D", "1 GPS Sat", "2 GPS Sat")
good_gps_fixes <- c("3D valid GPS Fix", "3D GPS Fix")

data.raw %<>% dplyr::filter(!(`gps:fix-type-raw` %in% bad_gps_fixes)) # 289039 events

data.raw %<>% dplyr::filter(!(`height-above-ellipsoid` < -500))
data.raw %<>% dplyr::filter(!(`height-above-ellipsoid` > 500)) # 289024 events

elapsed <- function(x) {
  y <- abs(as.duration(x[2:length(x)] %--% x[1:(length(x)-1)]))
  y >= 5*60}

data.raw %<>%
  tibble::rowid_to_column(., "Row") %>%
  group_split(`individual-local-identifier`) %>%
  map_dfr(~ .[c(T, if (nrow(.) > 1) elapsed(.$timestamp)),]) %>%
  arrange(Row) %>%
  as.data.frame()


data.raw %<>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"),
                class = "3D")

# Split dataset to control and treatment site
data.SI <- data.raw %>%
  dplyr::filter(study.area == "Staten Island")

data.RF <- data.raw %>%
  dplyr::filter(study.area == "Rockefeller")



# As telemetry object
# --------------------------------------------------------

data.SI.tel <- as.telemetry(data.SI, datum = 'EPSG:4326')
data.RF.tel <- as.telemetry(data.RF, datum = 'EPSG:4326')


# Staten Island Error Model
# --------------------------------------------------------
t.noHDOP  <- lapply(data.SI.tel, function(t){ t$HDOP  <- NULL; t })
t.noclass <- lapply(data.SI.tel, function(t){ t$class <- NULL; t })
t.nothing <- lapply(data.SI.tel, function(t){ t$HDOP  <- NULL; t$class <- NULL; t })


sapply(t.noHDOP, dim)
sapply(t.noclass, dim)
sapply(t.nothing, dim)

UERE <- uere.fit(data.SI.tel)
UERE.noHDOP  <- uere.fit(t.noHDOP)
UERE.noclass <- uere.fit(t.noclass)
UERE.nothing <- uere.fit(t.nothing)

# We can now apply model selection to the UERE model fits by summarizing them in a list.

summary(list(HDOP.class = UERE, class = UERE.noHDOP, HDOP = UERE.noclass, homoskedastic = UERE.nothing))

# Rockefeller Error Model
# --------------------------------------------------------
t.noHDOP  <- lapply(data.RF.tel, function(t){ t$HDOP  <- NULL; t })
t.noclass <- lapply(data.RF.tel, function(t){ t$class <- NULL; t })
t.nothing <- lapply(data.RF.tel, function(t){ t$HDOP  <- NULL; t$class <- NULL; t })


sapply(t.noHDOP, dim)
sapply(t.noclass, dim)
sapply(t.nothing, dim)

UERE <- uere.fit(data.RF.tel)
UERE.noHDOP  <- uere.fit(t.noHDOP)
UERE.noclass <- uere.fit(t.noclass)
UERE.nothing <- uere.fit(t.nothing)

# We can now apply model selection to the UERE model fits by summarizing them in a list.

summary(list(HDOP.class = UERE, class = UERE.noHDOP, HDOP = UERE.noclass, homoskedastic = UERE.nothing))

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

speed.SI %>% dplyr::filter(is.na(time_diff))

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

speed.RF %>% dplyr::filter(is.na(time_diff))

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
  print(paste0("Max speed value: ", max(outliers.SI[[i]]$speed * 100), " cm/s"))
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


# Model fitting
# --------------------------------------------------------

uere(data.SI.tel) <- NULL

# automated guesstimate for uncalibrated data (with 10 meter RMS UERE guess)
GUESS <- ctmm.guess(data.SI.tel[[3]],
                    CTMM = ctmm(error = 10),
                    interactive = FALSE)

# fit and select models # CRAN policy limits us to 2 cores
FIT <- ctmm.select(data.SI.tel[[3]],
                   GUESS,
                   trace = TRUE,
                   cores = 2)

saveRDS(FIT, "Data/model_fitting_result.rds")
FIT <- readRDS("Data/model_fitting_result.rds")

summary(FIT)

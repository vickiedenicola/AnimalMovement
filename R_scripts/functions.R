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


# Custom functions for visualization
# ------------------------------------------------------------------------------

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

plot_individual <- function(df.site = df.site, ind.id = c()){

  df.site <- df.site %>% dplyr::filter(`individual-local-identifier` %in% ind.id)

  df.site.sf <- st_as_sf(df.site, coords = c("location-long", "location-lat"), crs = 4326)

  pal1 <- viridisLite::viridis(length(unique(df.site.sf$`individual-local-identifier`)), direction = 1)

  tac_map <- ggplot() +
    geom_sf(data = df.site.sf,
            aes(color = `individual-local-identifier`), alpha = 0.3, size = 3) +
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

# plot_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"))
# plot_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44"))


map_individual <- function(df.site = df.site, ind.id = c(), burst = FALSE){

  df.site <- df.site %>% dplyr::filter(`individual-local-identifier` %in% ind.id) %>%
    dplyr::select(-c(timestamp, Date, Time))

  df.site.sf <- st_as_sf(df.site, coords = c("location-long", "location-lat"), crs = 4326)

  map_tac <- mapview(df.site.sf,
                     zcol = "individual-local-identifier",
                     layer.name = "Individual ID",
                     burst = burst)

  return(map_tac)
}

# map_individual(df.site = data.SI, ind.id = c("17b", "20b", "1319", "148", "14b"), burst = TRUE)
# map_individual(df.site = data.RF, ind.id = c("31", "57", "43", "44", "62", "47"), burst = TRUE)


# Function to subset the data
# ------------------------------------------------------------------------------

# Pre Breeding	15 Sep - 15 Oct
# Breeding	15 Oct- 31 Dec
# Post Breeding	1 Jan - 15 April
# Baseline	15 Apr - 30 May


# Parameters:
## dataset = data.raw
## time_period = NA or "Pre Breeding" or "Breeding" or "Post Breeding" or "Baseline")
## sex = NA or "M" or "F"
## study_area = NA or "Staten Island" or "Rockefeller"
## animal_color = NA or "W" or "Y"

subset_func <- function(dataset = dataset, time_period = NA, male_female = NA, study_area = NA, animal_color = NA){

  time_periods <- data.frame(period_name = c("Pre Breeding", "Breeding", "Post Breeding", "Baseline"),
                             start_date = c("2021-09-15", "2021-10-15", "2022-01-01", "2022-04-15"),
                             end_date = c("2021-10-15", "2021-12-31", "2022-04-15", "2022-05-30"))

  if(!is.na(study_area)){
    dataset %<>%
      dplyr::filter(study.area == study_area)
  }

  if(!is.na(male_female)){
    dataset %<>%
      dplyr::filter(sex == male_female)
  }

  if(!is.na(animal_color)){
    dataset %<>%
      dplyr::filter(Color == animal_color)
  }

  if(!is.na(time_period)){

    tm <- time_periods %>% dplyr::filter(period_name == time_period)

    dataset %<>% dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date)))

  }

  return(dataset)
}

# sb <- subset_func(dataset = data.raw, time_period = "Breeding", male_female = "M", study_area = "Staten Island", animal_color = "W")
# range(sb$timestamp)




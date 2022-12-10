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
library(gridExtra)

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
# Breeding	16 Oct- 31 Dec
# Post Breeding	1 Jan - 15 April
# Baseline	16 Apr - 30 May


# Parameters:
## dataset = data.raw
## time_period = NA or "Pre Breeding" or "Breeding" or "Post Breeding" or "Baseline")
## sex = NA or "M" or "F"
## study_area = NA or "Staten Island" or "Rockefeller"
## animal_color = NA or "W" or "Y"

subset_func <- function(dataset = dataset, time_period = NA, male_female = NA, study_area = NA, animal_color = NA){

  time_periods <- data.frame(period_name = c("Pre Breeding", "Breeding", "Post Breeding", "Baseline"),
                             start_date = c("2021-09-15", "2021-10-16", "2022-01-01", "2022-04-16"),
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

# Function to compare individulas from control vs treatment site
# ------------------------------------------------------------------------------

# cal.model <- UERE <- readRDS("Data/undep/calibration.error.model.rds")

compare_periods <- function(input.data = input.data, period = c("Pre Breeding", "Breeding", "Post Breeding", "Baseline"), male_female = c("M", "F"), use.ctmm = c("fit", "select"), cal.model = NA, export.folder = ""){
  
  print("Subseting data to treatment and control sites.")
  
  data.subset.t <- subset_func(dataset = input.data,
                             time_period = period,
                             male_female = male_female,
                             study_area = "Staten Island")
                             #animal_color = "W")
  
  data.subset.c <- subset_func(dataset = input.data,
                               time_period = period,
                               male_female = male_female,
                               study_area = "Rockefeller")
  
  
  plot.t <- plot_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`))
  plot.c <- plot_individual(df.site = data.subset.c, ind.id = unique(data.subset.c$`individual-local-identifier`))
  
  if(!is.na(export.folder)){
    # export plots
    # gr1 <- grid.arrange(plot.t, plot.c, ncol = 2)
    ggsave(plot = plot.t,
           filename = paste0(export.folder, "/", period, "_", male_female, "_plot.treatment.jpg"),
           width = 45,
           height = 40,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
    ggsave(plot = plot.c,
           filename = paste0(export.folder, "/", period, "_", male_female, "_plot.control.jpg"),
           width = 45,
           height = 40,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  # mapviewOptions(fgb = FALSE)
  map.t <- map_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`), burst = FALSE)
  map.c <- map_individual(df.site = data.subset.c, ind.id = unique(data.subset.c$`individual-local-identifier`), burst = FALSE)
  
  data.tel.t <- as.telemetry(data.subset.t, datum = 'EPSG:4326')
  data.tel.c <- as.telemetry(data.subset.c, datum = 'EPSG:4326')
  
  
  if(!is.null(cal.model)){
    UERE <- cal.model
    UERE$DOF[] <- UERE$UERE
    
    uere(data.tel.t) <- UERE
    uere(data.tel.c) <- UERE
  }
  
  
  print("Detecting and removing outliers!")
  
  # Outlier detection
  # ------------------------------------------------------------------------------
  outliers.tel.t <- outlie(data.tel.t, plot = FALSE)
  outliers.tel.c <- outlie(data.tel.c, plot = FALSE)
  
  
  # Remove outliers
  # ------------------------------------------------------------------------------
  
  for(i in 1:length(outliers.tel.t)){
    print(paste0("Max speed value: ", max(outliers.tel.t[[i]]$speed) * 100, " cm/s"))
    x <- which(outliers.tel.t[[i]]$speed * 100 >= 16.1)
    print(paste0("Rows to remove: ", length(x)))
    if(length(x) > 0){
      data.tel.t[[i]] <- data.tel.t[[i]][-x, ]
    }
    print(paste0("Processed: ", i, "/", length(outliers.tel.t), " individuals"))
    print("------------------------------------------------------------------")
  }
  
  for(i in 1:length(outliers.tel.c)){
    print(paste0("Max speed value: ", max(outliers.tel.c[[i]]$speed) * 100, " cm/s"))
    x <- which(outliers.tel.c[[i]]$speed * 100 >= 16.1)
    print(paste0("Rows to remove: ", length(x)))
    if(length(x) > 0){
      data.tel.c[[i]] <- data.tel.c[[i]][-x, ]
    }
    print(paste0("Processed: ", i, "/", length(outliers.tel.c), " individuals"))
    print("------------------------------------------------------------------")
  }
  
  
  # Assign names
  
  names.list_t <- list()
  for(i in 1:length(data.tel.t)){
    names.list_t[[i]] <- data.tel.t[[i]]@info$identity
  }
  
  names.list_c <- list()
  for(i in 1:length(data.tel.c)){
    names.list_c[[i]] <- data.tel.c[[i]]@info$identity
  }
  
  
  # names(data.tel.t) <- names.list_t
  # names(data.tel.c) <- names.list_c
  
  
  print("Calculating variograms.")
  
  variogram_list.t <- list()
  variogram_list.c <- list()
  
  for(i in 1:length(data.tel.t)){
    variogram_list.t[[i]] <- variogram(data.tel.t[[i]], error = TRUE)
  }
  
  for(i in 1:length(data.tel.c)){
    variogram_list.c[[i]] <- variogram(data.tel.c[[i]], error = TRUE)
  }
  
  names(variogram_list.t) <- names.list_t
  names(variogram_list.c) <- names.list_c
  
  print(paste0("Model fitting using ctmm: ", use.ctmm, " method."))
  
  guess_list.t <- list()
  guess_list.c <- list()
  
  for(i in 1:length(data.tel.t)){
    guess_list.t[[i]] <- ctmm.guess(data.tel.t[[i]], variogram = variogram_list.t[[i]], CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)
  }
  
  for(i in 1:length(data.tel.c)){
    guess_list.c[[i]] <- ctmm.guess(data.tel.c[[i]], variogram = variogram_list.c[[i]], CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)
  }

  names(guess_list.t) <- names.list_t
  names(guess_list.c) <- names.list_c
  
  if(use.ctmm == "fit"){
    
    fit_list.t <- list()
    fit_list.c <- list()
    
    for(i in 1:length(data.tel.t)){
      fit_list.t[[i]] <- tryCatch(
        {
          ctmm.fit(data.tel.t[[i]], guess_list.t[[i]])
        },
        error = function(e){
          NA
        }
      )
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
    }
    
    for(i in 1:length(data.tel.c)){
      fit_list.c[[i]] <- tryCatch(
        {
          ctmm.fit(data.tel.c[[i]], guess_list.c[[i]])
        },
        error = function(e){
          NA
        }
      )
        
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
    }
    
    names(fit_list.t) <- names.list_t
    names(fit_list.c) <- names.list_c
    
  } else if(use.ctmm == "select"){
    
    
    fit_list.t <- list()
    fit_list.c <- list()
    
    for(i in 1:length(data.tel.t)){
      fit_list.t[[i]] <- ctmm.select(data.tel.t[[i]], guess_list.t[[i]], trace = 3, verbose = TRUE, cores = 15)
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
    }
    
    for(i in 1:length(data.tel.c)){
      fit_list.c[[i]] <- ctmm.select(data.tel.c[[i]], guess_list.c[[i]], trace = 3, verbose = TRUE, cores = 15)
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
    }
    
    names(fit_list.t) <- names.list_t
    names(fit_list.c) <- names.list_c
    
  } else{
    print("Please provide valid method.")
    return()
  }
  
  print("Model fitting DONE!")
  
  # ind.names.t <- unique(data.subset.t$`individual-local-identifier`)
  # ind.names.c <- unique(data.subset.c$`individual-local-identifier`)
  
  print("Calculating summary statistics.")
  
  summ_list.t <- list()
  
  for(i in 1:length(data.tel.t)){
    
    if(!is.na(fit_list.t[[i]])){
      summ.model <- summary(fit_list.t[[i]])$CI %>% as.data.frame()
      summ_list.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                     
                                     area_low = summ.model[1, 1],
                                     area_mean = summ.model[1, 2],
                                     area_high = summ.model[1, 3],
                                     area.units = rownames(summary(fit_list.t[[i]])$CI)[1],
                                     
                                     t_position_low = summ.model[2, 1],
                                     t_position_mean = summ.model[2, 2],
                                     t_position_high = summ.model[2, 3],
                                     t_position.units = rownames(summary(fit_list.t[[i]])$CI)[2],
                                     
                                     t_velocity_low = summ.model[3, 1],
                                     t_velocity_low = summ.model[3, 2],
                                     t_velocity_low = summ.model[3, 3],
                                     t_velocity.units = rownames(summary(fit_list.t[[i]])$CI)[3],
                                     
                                     speed_low = summ.model[4, 1],
                                     speed_mean = summ.model[4, 2],
                                     speed_high = summ.model[4, 3],
                                     speed.units = rownames(summary(fit_list.t[[i]])$CI)[4],
                                     
                                     diffusion_low = summ.model[5, 1],
                                     diffusion_mean = summ.model[5, 2],
                                     diffusion_high = summ.model[5, 3],
                                     diffusion.units = rownames(summary(fit_list.t[[i]])$CI)[5],
                                     
                                     error_gps_low = summ.model[6, 1],
                                     error_gps_mean = summ.model[6, 2],
                                     error_gps_high = summ.model[6, 3],
                                     error_gps.units = rownames(summary(fit_list.t[[i]])$CI)[6])
    } else{
      summ_list.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                     
                                     area_low = NA,
                                     area_mean = NA,
                                     area_high = NA,
                                     area.units = NA,
                                     
                                     t_position_low = NA,
                                     t_position_mean = NA,
                                     t_position_high = NA,
                                     t_position.units = NA,
                                     
                                     t_velocity_low = NA,
                                     t_velocity_low = NA,
                                     t_velocity_low = NA,
                                     t_velocity.units = NA,
                                     
                                     speed_low = NA,
                                     speed_mean = NA,
                                     speed_high = NA,
                                     speed.units = NA,
                                     
                                     diffusion_low = NA,
                                     diffusion_mean = NA,
                                     diffusion_high = NA,
                                     diffusion.units = NA,
                                     
                                     error_gps_low = NA,
                                     error_gps_mean = NA,
                                     error_gps_high = NA,
                                     error_gps.units = NA)
      
    }
    
    
  }
  
  
  # summ.model %<>% tibble::rownames_to_column("Attribute")
  # 
  # summ.model1 <- summ.model %>% dplyr::select(-Attribute) %>% t() %>% as_tibble() %>%
  #   magrittr::set_colnames(c(summ.model$Attribute)) %>%
  #   mutate(var = subset(colnames(summ.model), !colnames(summ.model) %in% c("Attribute"))) 
  
  sum.res.t <- as.data.frame(do.call(rbind, summ_list.t))
  
  summ_list.c <- list()
  
  for(i in 1:length(data.tel.c)){
    
    if(!is.na(fit_list.c[[i]])){
      summ.model <- summary(fit_list.c[[i]])$CI %>% as.data.frame()
      summ_list.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                     area_low = summ.model[1, 1],
                                     area_mean = summ.model[1, 2],
                                     area_high = summ.model[1, 3],
                                     area.units = rownames(summary(fit_list.t[[i]])$CI)[1],
                                     t_position_low = summ.model[2, 1],
                                     t_position_mean = summ.model[2, 2],
                                     t_position_high = summ.model[2, 3],
                                     t_position.units = rownames(summary(fit_list.t[[i]])$CI)[2],
                                     t_velocity_low = summ.model[3, 1],
                                     t_velocity_low = summ.model[3, 2],
                                     t_velocity_low = summ.model[3, 3],
                                     t_velocity.units = rownames(summary(fit_list.t[[i]])$CI)[3],
                                     speed_low = summ.model[4, 1],
                                     speed_mean = summ.model[4, 2],
                                     speed_high = summ.model[4, 3],
                                     speed.units = rownames(summary(fit_list.t[[i]])$CI)[4],
                                     diffusion_low = summ.model[5, 1],
                                     diffusion_mean = summ.model[5, 2],
                                     diffusion_high = summ.model[5, 3],
                                     diffusion.units = rownames(summary(fit_list.t[[i]])$CI)[5],
                                     error_gps_low = summ.model[6, 1],
                                     error_gps_mean = summ.model[6, 2],
                                     error_gps_high = summ.model[6, 3],
                                     error_gps.units = rownames(summary(fit_list.t[[i]])$CI)[6])
      
    } else{
      summ_list.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                     area_low = NA,
                                     area_mean = NA,
                                     area_high = NA,
                                     area.units = NA,
                                     t_position_low = NA,
                                     t_position_mean = NA,
                                     t_position_high = NA,
                                     t_position.units = NA,
                                     t_velocity_low = NA,
                                     t_velocity_low = NA,
                                     t_velocity_low = NA,
                                     t_velocity.units = NA,
                                     speed_low = NA,
                                     speed_mean = NA,
                                     speed_high = NA,
                                     speed.units = NA,
                                     diffusion_low = NA,
                                     diffusion_mean = NA,
                                     diffusion_high = NA,
                                     diffusion.units = NA,
                                     error_gps_low = NA,
                                     error_gps_mean = NA,
                                     error_gps_high = NA,
                                     error_gps.units = NA)
      
    }
    
    
  }
  
  sum.res.c <- as.data.frame(do.call(rbind, summ_list.c))
  
  akde.list.t <- list()
  akde.list.c <- list()
  
  print("Calculating AKDE - home range area.")
  
  for(i in 1:length(data.tel.t)){
    akde.list.t[[i]] <- tryCatch(
      {
        ctmm::akde(data = data.tel.t[[i]], CTMM = fit_list.t[[i]])
      },
      error = function(e){
        NA
      }
    )
    print(paste0("Calculating AKDE DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
  }
  
  for(i in 1:length(data.tel.c)){
    akde.list.c[[i]] <- tryCatch(
      {
        ctmm::akde(data = data.tel.c[[i]], CTMM = fit_list.c[[i]])
      },
      error = function(e){
        NA
      }
    )
    print(paste0("Calculating AKDE DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
  }
  
  names(akde.list.t) <- names.list_t
  names(akde.list.c) <- names.list_c
  
  akde.area.t <- list()
  
  for(i in 1:length(data.tel.t)){
    
    if(!is.na(akde.list.t[[i]])){
      akde.area <- summary(akde.list.t[[i]])$CI
      akde.area.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                     area_low = akde.area[1, 1],
                                     area_mean = akde.area[1, 2],
                                     area_high = akde.area[1, 3],
                                     area.units = rownames(summary(akde.list.t[[i]])$CI)[1]) 
    } else{
      akde.area.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                     area_low = NA,
                                     area_mean = NA,
                                     area_high = NA,
                                     area.units = NA)
    }
    
    
  }
  
  akde.area.df.t <- as.data.frame(do.call(rbind, akde.area.t))
  
  akde.area.c <- list()
  
  for(i in 1:length(data.tel.c)){
    
    if(!is.na(akde.list.c[[i]])){
      akde.area <- summary(akde.list.c[[i]])$CI
      akde.area.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                     area_low = akde.area[1, 1],
                                     area_mean = akde.area[1, 2],
                                     area_high = akde.area[1, 3],
                                     area.units = rownames(summary(akde.list.c[[i]])$CI)[1]) 
    } else{
      akde.area.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                     area_low = NA,
                                     area_mean = NA,
                                     area_high = NA,
                                     area.units = NA) 
      
    }
    
    
  }
  
  akde.area.df.c <- as.data.frame(do.call(rbind, akde.area.c))
  
  speed_list.t <- list()
  speed_list.c <- list()
  
  print("Calculating SPEED.")
  
  for(i in 1:length(data.tel.t)){
    speed_list.t[[i]] <- tryCatch(
      {
        ctmm::speed(object = data.tel.t[[i]], CTMM = fit_list.t[[i]], units = FALSE, cores = 0)
      },
      error = function(e){
        NA
      }
    )
    print(paste0("Calculating SPEED DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
  }
  
  for(i in 1:length(data.tel.c)){
    speed_list.c[[i]] <- tryCatch(
      {
        ctmm::speed(object = data.tel.c[[i]], CTMM = fit_list.c[[i]], units = FALSE, cores = 0)
      },
      error = function(e){
        NA
      }
    )
    print(paste0("Calculating SPEED DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
  }
  
  names(speed_list.t) <- names.list_t
  names(speed_list.c) <- names.list_c
  
  print("Calculating DISTANCE and summary statistics.")
  
  speed.stat.t <- list()
  
  for(i in 1:length(data.tel.t)){
    days <- cut(data.tel.t[[i]]$timestamp, breaks = "day")
    days <- unique(days)
    d.length <- length(days)
    
    if(!is.na(speed_list.t[[i]])){
      summ.speed <- speed_list.t[[i]]$CI
      summ.dist <- summ.speed * d.length * 24 * 3600 / 1000
      summ.speed <- summ.speed * 3.6
      
      speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = d.length,
                                      distance.km_low = summ.dist[1, 1],
                                      distance.km_mean = summ.dist[1, 2],
                                      distance.km_high = summ.dist[1, 3],
                                      distance.units = "meters",# rownames(speed_list.t[[i]]$CI)[1],
                                      speed.km.h_low = summ.speed[1, 1],
                                      speed.km.h_low = summ.speed[1, 2],
                                      speed.km.h_low = summ.speed[1, 3],
                                      speed.units = rownames(speed_list.t[[i]]$CI)[1])
    } else{
      speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = NA,
                                      distance.km_low = NA,
                                      distance.km_mean = NA,
                                      distance.km_high = NA,
                                      distance.units = NA,
                                      speed.km.h_low = NA,
                                      speed.km.h_low = NA,
                                      speed.km.h_low = NA,
                                      speed.units = NA)
    }
    
    
  }
  
  speed.dist.df.t <- as.data.frame(do.call(rbind, speed.stat.t))
  
  speed.stat.c <- list()
  
  for(i in 1:length(data.tel.c)){
    days <- cut(data.tel.c[[i]]$timestamp, breaks = "day")
    days <- unique(days)
    d.length <- length(days)
    
    if(!is.na(speed_list.c[[i]])){
      summ.speed <- speed_list.c[[i]]$CI
      summ.dist <- summ.speed * d.length * 24 * 3600 / 1000
      summ.speed <- summ.speed * 3.6
      
      speed.stat.c[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = d.length,
                                      distance_low = summ.dist[1, 1],
                                      distance_mean = summ.dist[1, 2],
                                      distance_high = summ.dist[1, 3],
                                      distance.units = rownames(speed_list.c[[i]]$CI)[1],
                                      speed_low = summ.speed[1, 1],
                                      speed_low = summ.speed[1, 2],
                                      speed_low = summ.speed[1, 3],
                                      speed.units = rownames(speed_list.c[[i]]$CI)[1])
      
    } else{
      speed.stat.c[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = NA,
                                      distance_low = NA,
                                      distance_mean = NA,
                                      distance_high = NA,
                                      distance.units = NA,
                                      speed_low = NA,
                                      speed_low = NA,
                                      speed_low = NA,
                                      speed.units = NA)
    }
    
    
  }
  
  speed.dist.df.c <- as.data.frame(do.call(rbind, speed.stat.c))
  
  # TODO include variograms as result - DONE
  
  res.list_treatment <- list(map = map.t, telemetry.objects = data.tel.t, summary = sum.res.t, akde = akde.area.df.t, akde.objects = akde.list.t, speed.dist = speed.dist.df.t, fit.objects = fit_list.t, speed.objects = speed_list.t, variograms = variogram_list.t)
  res.list_control <- list(map = map.c, telemetry.objects = data.tel.c, summary = sum.res.c, akde = akde.area.df.c, akde.objects = akde.list.c, speed.dist = speed.dist.df.c, fit.objects = fit_list.c, speed.objects = speed_list.c, variograms = variogram_list.c)

  res.list <- list(treatment = res.list_treatment, control = res.list_control)
  print("Comparasion DONE!")
  
  if(!is.na(export.folder)){
    print(paste0("Saving results to: ", export.folder))
    
    saveRDS(res.list, paste0(export.folder, "/", period, "_", male_female, "_period_results.RDS"))
    
    writexl::write_xlsx(res.list$treatment$summary, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_summary.xlsx"))
    writexl::write_xlsx(res.list$treatment$akde, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_akde.xlsx"))
    writexl::write_xlsx(res.list$treatment$speed.dist, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_speed_dist.xlsx"))
    
    
    writexl::write_xlsx(res.list$control$summary, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_summary.xlsx"))
    writexl::write_xlsx(res.list$control$akde, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_akde.xlsx"))
    writexl::write_xlsx(res.list$control$speed.dist, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_speed_dist.xlsx"))
    
  }
  
  return(res.list)
  
}


# summ.model <- summary(M.OUF.e)$CI %>% as.data.frame()
# akde_area <- ctmm::akde(data = first_deer, CTMM = M.OUF.e)
# akde_area <- summary(akde_area)
# akde_area <- akde_area$CI
# ctmm_speed <- ctmm::speed(object = first_deer,
#                           CTMM = M.OUF.e,
#                           units = FALSE)
# ctmm_dist <- ctmm_speed$CI * length(days) * 24 * 3600 / 1000
# rownames(ctmm_dist) <- "distance (km)"
# ctmm_speed <- ctmm_speed$CI * 3.6
# rownames(ctmm_speed) <- "speed (km/h)"



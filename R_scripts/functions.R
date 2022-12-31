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
library(viridis)

library(ggpubr)
library(jtools)
library(ggfortify)
library(grid)
library(gridExtra)

# Additional packages

library(move)
library(ctmm)
library(ctmmweb)


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


# Function to subset the data - ******* OLD *******
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

compare_periods <- function(input.data = input.data, period = c("Breeding", "Post Breeding", "Baseline"), male_female = c("M", "F"), use.ctmm = c("fit", "select"), cal.model = NA, check_table = check_table, export.folder = ""){
  
  print("Subseting data to treatment and control sites.")
  
  subset_func_new(dataset = input.data, check_table = check_table, time_period = period, male_female = male_female, study_area = "Staten Island")
  
  data.subset.t <- subset_func_new(dataset = input.data, 
                                   check_table = check_table, 
                                   time_period = period, 
                                   male_female = male_female, 
                                   study_area = "Staten Island")
  
  data.subset.c <- subset_func_new(dataset = input.data,
                                   check_table = check_table,
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
  # map.t <- map_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`), burst = FALSE)
  # map.c <- map_individual(df.site = data.subset.c, ind.id = unique(data.subset.c$`individual-local-identifier`), burst = FALSE)
  
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
    x <- which(outliers.tel.t[[i]]$speed * 100 >= 17.8816)
    print(paste0("Rows to remove: ", length(x)))
    if(length(x) > 0){
      data.tel.t[[i]] <- data.tel.t[[i]][-x, ]
    }
    print(paste0("Processed: ", i, "/", length(outliers.tel.t), " individuals"))
    print("------------------------------------------------------------------")
  }
  
  for(i in 1:length(outliers.tel.c)){
    print(paste0("Max speed value: ", max(outliers.tel.c[[i]]$speed) * 100, " cm/s"))
    x <- which(outliers.tel.c[[i]]$speed * 100 >= 17.8816)
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
  
  #print("Calculating summary statistics.")
  
  #summ_list.t <- list()
  
  # for(i in 1:length(data.tel.t)){
  #   
  #   if(!is.na(fit_list.t[[i]])){
  #     summ.model <- summary(fit_list.t[[i]])$CI %>% as.data.frame()
  #     summ_list.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                    
  #                                    area_low = summ.model[1, 1],
  #                                    area_mean = summ.model[1, 2],
  #                                    area_high = summ.model[1, 3],
  #                                    area.units = rownames(summary(fit_list.t[[i]])$CI)[1],
  #                                    
  #                                    t_position_low = summ.model[2, 1],
  #                                    t_position_mean = summ.model[2, 2],
  #                                    t_position_high = summ.model[2, 3],
  #                                    t_position.units = rownames(summary(fit_list.t[[i]])$CI)[2],
  #                                    
  #                                    t_velocity_low = summ.model[3, 1],
  #                                    t_velocity_low = summ.model[3, 2],
  #                                    t_velocity_low = summ.model[3, 3],
  #                                    t_velocity.units = rownames(summary(fit_list.t[[i]])$CI)[3],
  #                                    
  #                                    speed_low = summ.model[4, 1],
  #                                    speed_mean = summ.model[4, 2],
  #                                    speed_high = summ.model[4, 3],
  #                                    speed.units = rownames(summary(fit_list.t[[i]])$CI)[4],
  #                                    
  #                                    diffusion_low = summ.model[5, 1],
  #                                    diffusion_mean = summ.model[5, 2],
  #                                    diffusion_high = summ.model[5, 3],
  #                                    diffusion.units = rownames(summary(fit_list.t[[i]])$CI)[5],
  #                                    
  #                                    error_gps_low = summ.model[6, 1],
  #                                    error_gps_mean = summ.model[6, 2],
  #                                    error_gps_high = summ.model[6, 3],
  #                                    error_gps.units = rownames(summary(fit_list.t[[i]])$CI)[6])
  #   } else{
  #     summ_list.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                    
  #                                    area_low = NA,
  #                                    area_mean = NA,
  #                                    area_high = NA,
  #                                    area.units = NA,
  #                                    
  #                                    t_position_low = NA,
  #                                    t_position_mean = NA,
  #                                    t_position_high = NA,
  #                                    t_position.units = NA,
  #                                    
  #                                    t_velocity_low = NA,
  #                                    t_velocity_low = NA,
  #                                    t_velocity_low = NA,
  #                                    t_velocity.units = NA,
  #                                    
  #                                    speed_low = NA,
  #                                    speed_mean = NA,
  #                                    speed_high = NA,
  #                                    speed.units = NA,
  #                                    
  #                                    diffusion_low = NA,
  #                                    diffusion_mean = NA,
  #                                    diffusion_high = NA,
  #                                    diffusion.units = NA,
  #                                    
  #                                    error_gps_low = NA,
  #                                    error_gps_mean = NA,
  #                                    error_gps_high = NA,
  #                                    error_gps.units = NA)
  #     
  #   }
  #   
  #   
  # }
  # 
  
  # summ.model %<>% tibble::rownames_to_column("Attribute")
  # 
  # summ.model1 <- summ.model %>% dplyr::select(-Attribute) %>% t() %>% as_tibble() %>%
  #   magrittr::set_colnames(c(summ.model$Attribute)) %>%
  #   mutate(var = subset(colnames(summ.model), !colnames(summ.model) %in% c("Attribute"))) 
  
  # sum.res.t <- as.data.frame(do.call(rbind, summ_list.t))
  
  # summ_list.c <- list()
  
  # for(i in 1:length(data.tel.c)){
  #   
  #   if(!is.na(fit_list.c[[i]])){
  #     summ.model <- summary(fit_list.c[[i]])$CI %>% as.data.frame()
  #     summ_list.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                    area_low = summ.model[1, 1],
  #                                    area_mean = summ.model[1, 2],
  #                                    area_high = summ.model[1, 3],
  #                                    area.units = rownames(summary(fit_list.c[[i]])$CI)[1],
  #                                    t_position_low = summ.model[2, 1],
  #                                    t_position_mean = summ.model[2, 2],
  #                                    t_position_high = summ.model[2, 3],
  #                                    t_position.units = rownames(summary(fit_list.c[[i]])$CI)[2],
  #                                    t_velocity_low = summ.model[3, 1],
  #                                    t_velocity_low = summ.model[3, 2],
  #                                    t_velocity_low = summ.model[3, 3],
  #                                    t_velocity.units = rownames(summary(fit_list.c[[i]])$CI)[3],
  #                                    speed_low = summ.model[4, 1],
  #                                    speed_mean = summ.model[4, 2],
  #                                    speed_high = summ.model[4, 3],
  #                                    speed.units = rownames(summary(fit_list.c[[i]])$CI)[4],
  #                                    diffusion_low = summ.model[5, 1],
  #                                    diffusion_mean = summ.model[5, 2],
  #                                    diffusion_high = summ.model[5, 3],
  #                                    diffusion.units = rownames(summary(fit_list.c[[i]])$CI)[5],
  #                                    error_gps_low = summ.model[6, 1],
  #                                    error_gps_mean = summ.model[6, 2],
  #                                    error_gps_high = summ.model[6, 3],
  #                                    error_gps.units = rownames(summary(fit_list.c[[i]])$CI)[6])
  #     
  #   } else{
  #     summ_list.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                    area_low = NA,
  #                                    area_mean = NA,
  #                                    area_high = NA,
  #                                    area.units = NA,
  #                                    t_position_low = NA,
  #                                    t_position_mean = NA,
  #                                    t_position_high = NA,
  #                                    t_position.units = NA,
  #                                    t_velocity_low = NA,
  #                                    t_velocity_low = NA,
  #                                    t_velocity_low = NA,
  #                                    t_velocity.units = NA,
  #                                    speed_low = NA,
  #                                    speed_mean = NA,
  #                                    speed_high = NA,
  #                                    speed.units = NA,
  #                                    diffusion_low = NA,
  #                                    diffusion_mean = NA,
  #                                    diffusion_high = NA,
  #                                    diffusion.units = NA,
  #                                    error_gps_low = NA,
  #                                    error_gps_mean = NA,
  #                                    error_gps_high = NA,
  #                                    error_gps.units = NA)
  #     
  #   }
  #   
  #   
  # }
  # 
  # sum.res.c <- as.data.frame(do.call(rbind, summ_list.c))
  
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
  
  # akde.area.t <- list()
  # 
  # for(i in 1:length(data.tel.t)){
  #   
  #   if(!is.na(akde.list.t[[i]])){
  #     akde.area <- summary(akde.list.t[[i]])$CI
  #     akde.area.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                    area_low = akde.area[1, 1],
  #                                    area_mean = akde.area[1, 2],
  #                                    area_high = akde.area[1, 3],
  #                                    area.units = rownames(summary(akde.list.t[[i]])$CI)[1]) 
  #   } else{
  #     akde.area.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                    area_low = NA,
  #                                    area_mean = NA,
  #                                    area_high = NA,
  #                                    area.units = NA)
  #   }
  #   
  #   
  # }
  # 
  # akde.area.df.t <- as.data.frame(do.call(rbind, akde.area.t))
  # 
  # akde.area.c <- list()
  # 
  # for(i in 1:length(data.tel.c)){
  #   
  #   if(!is.na(akde.list.c[[i]])){
  #     akde.area <- summary(akde.list.c[[i]])$CI
  #     akde.area.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                    area_low = akde.area[1, 1],
  #                                    area_mean = akde.area[1, 2],
  #                                    area_high = akde.area[1, 3],
  #                                    area.units = rownames(summary(akde.list.c[[i]])$CI)[1]) 
  #   } else{
  #     akde.area.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                    area_low = NA,
  #                                    area_mean = NA,
  #                                    area_high = NA,
  #                                    area.units = NA) 
  #     
  #   }
  #   
  #   
  # }
  # 
  # akde.area.df.c <- as.data.frame(do.call(rbind, akde.area.c))
  
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
  
  # print("Calculating DISTANCE and summary statistics.")
  
  # speed.stat.t <- list()
  # 
  # for(i in 1:length(data.tel.t)){
  #   days <- cut(data.tel.t[[i]]$timestamp, breaks = "day")
  #   days <- unique(days)
  #   d.length <- length(days)
  #   
  #   if(!is.na(speed_list.t[[i]])){
  #     summ.speed <- speed_list.t[[i]]$CI
  #     summ.dist <- summ.speed * d.length * 24 * 3600 / 1000
  #     summ.speed <- summ.speed * 3.6
  #     
  #     speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                     days = d.length,
  #                                     distance_low = summ.dist[1, 1],
  #                                     distance_mean = summ.dist[1, 2],
  #                                     distance_high = summ.dist[1, 3],
  #                                     distance.units = "meters",# rownames(speed_list.t[[i]]$CI)[1],
  #                                     speed_low = summ.speed[1, 1],
  #                                     speed_low = summ.speed[1, 2],
  #                                     speed_low = summ.speed[1, 3],
  #                                     speed.units = rownames(speed_list.t[[i]]$CI)[1])
  #   } else{
  #     speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
  #                                     days = NA,
  #                                     distance.km_low = NA,
  #                                     distance.km_mean = NA,
  #                                     distance.km_high = NA,
  #                                     distance.units = NA,
  #                                     speed.km.h_low = NA,
  #                                     speed.km.h_low = NA,
  #                                     speed.km.h_low = NA,
  #                                     speed.units = NA)
  #   }
  #   
  #   
  # }
  # 
  # speed.dist.df.t <- as.data.frame(do.call(rbind, speed.stat.t))
  # 
  # speed.stat.c <- list()
  # 
  # for(i in 1:length(data.tel.c)){
  #   days <- cut(data.tel.c[[i]]$timestamp, breaks = "day")
  #   days <- unique(days)
  #   d.length <- length(days)
  #   
  #   if(!is.na(speed_list.c[[i]])){
  #     summ.speed <- speed_list.c[[i]]$CI
  #     summ.dist <- summ.speed * d.length * 24 * 3600 / 1000
  #     summ.speed <- summ.speed * 3.6
  #     
  #     speed.stat.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                     days = d.length,
  #                                     distance_low = summ.dist[1, 1],
  #                                     distance_mean = summ.dist[1, 2],
  #                                     distance_high = summ.dist[1, 3],
  #                                     distance.units = rownames(speed_list.c[[i]]$CI)[1],
  #                                     speed_low = summ.speed[1, 1],
  #                                     speed_mean = summ.speed[1, 2],
  #                                     speed_high = summ.speed[1, 3],
  #                                     speed.units = rownames(speed_list.c[[i]]$CI)[1])
  #     
  #   } else{
  #     speed.stat.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
  #                                     days = NA,
  #                                     distance_low = NA,
  #                                     distance_mean = NA,
  #                                     distance_high = NA,
  #                                     distance.units = NA,
  #                                     speed_low = NA,
  #                                     speed_mean = NA,
  #                                     speed_high = NA,
  #                                     speed.units = NA)
  #   }
  #   
  #   
  # }
  # 
  # speed.dist.df.c <- as.data.frame(do.call(rbind, speed.stat.c))
  
  res.list_treatment <- list(telemetry.objects = data.tel.t, akde.objects = akde.list.t, fit.objects = fit_list.t, speed.objects = speed_list.t, variograms = variogram_list.t) # summary = sum.res.t, akde = akde.area.df.t, speed.dist = speed.dist.df.t,
  res.list_control <- list(telemetry.objects = data.tel.c, akde.objects = akde.list.c, fit.objects = fit_list.c, speed.objects = speed_list.c, variograms = variogram_list.c)

  res.list <- list(treatment = res.list_treatment, control = res.list_control)
  print("Comparasion DONE!")
  
  if(!is.na(export.folder)){
    print(paste0("Saving results to: ", export.folder))
    
    saveRDS(res.list, paste0(export.folder, "/", period, "_", male_female, "_period_results.RDS"))
    
    # writexl::write_xlsx(res.list$treatment$summary, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_summary.xlsx"))
    # writexl::write_xlsx(res.list$treatment$akde, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_akde.xlsx"))
    # writexl::write_xlsx(res.list$treatment$speed.dist, paste0(export.folder, "/", period, "_", male_female, "_period_treatment.site_speed_dist.xlsx"))
    # 
    # 
    # writexl::write_xlsx(res.list$control$summary, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_summary.xlsx"))
    # writexl::write_xlsx(res.list$control$akde, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_akde.xlsx"))
    # writexl::write_xlsx(res.list$control$speed.dist, paste0(export.folder, "/", period, "_", male_female, "_period_control.site_speed_dist.xlsx"))
    
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


period.summary <- function(period_result = period_result, export.folder = ""){
  
  
  data.tel.t <- period_result$treatment$telemetry.objects
  data.tel.c <- period_result$control$telemetry.objects
  
  fit_list.t <- period_result$treatment$fit.objects
  fit_list.c <- period_result$control$fit.objects
  
  akde.list.t <- period_result$treatment$akde.objects
  akde.list.c <- period_result$control$akde.objects
  
  speed_list.t <- period_result$treatment$speed.objects
  speed_list.c <- period_result$control$speed.objects
  
  print("Calculating summary statistics.")
  
  # Assign names
  
  names.list_t <- list()
  for(i in 1:length(data.tel.t)){
    names.list_t[[i]] <- data.tel.t[[i]]@info$identity
  }
  
  names.list_c <- list()
  for(i in 1:length(data.tel.c)){
    names.list_c[[i]] <- data.tel.c[[i]]@info$identity
  }
  
  
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
                                     error_gps.units = rownames(summary(fit_list.t[[i]])$CI)[6]
                                     )
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
                                     error_gps.units = NA
                                     )
      
    }
    
    
  }
  
  sum.res.t <- as.data.frame(do.call(rbind, summ_list.t))
  
  summ_list.c <- list()
  
  # for(i in 1:length(data.tel.c)){
  #   print(summary(fit_list.c[[i]])$CI %>% as.data.frame())
  # }
  # 
  # for(i in 1:length(data.tel.c)){
  #   print(dim(summary(fit_list.c[[i]])$CI %>% as.data.frame()))
  # }
  # 
  # i = 16
  
  for(i in 1:length(data.tel.c)){
    
    if(!is.na(fit_list.c[[i]])){
      summ.model <- summary(fit_list.c[[i]])$CI %>% as.data.frame()
      
      summ_list.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                     area_low = summ.model[1, 1],
                                     area_mean = summ.model[1, 2],
                                     area_high = summ.model[1, 3],
                                     area.units = rownames(summary(fit_list.c[[i]])$CI)[1],
                                     t_position_low = summ.model[2, 1],
                                     t_position_mean = summ.model[2, 2],
                                     t_position_high = summ.model[2, 3],
                                     t_position.units = rownames(summary(fit_list.c[[i]])$CI)[2],
                                     t_velocity_low = summ.model[3, 1],
                                     t_velocity_low = summ.model[3, 2],
                                     t_velocity_low = summ.model[3, 3],
                                     t_velocity.units = rownames(summary(fit_list.c[[i]])$CI)[3],
                                     speed_low = summ.model[4, 1],
                                     speed_mean = summ.model[4, 2],
                                     speed_high = summ.model[4, 3],
                                     speed.units = rownames(summary(fit_list.c[[i]])$CI)[4],
                                     diffusion_low = summ.model[5, 1],
                                     diffusion_mean = summ.model[5, 2],
                                     diffusion_high = summ.model[5, 3],
                                     diffusion.units = rownames(summary(fit_list.c[[i]])$CI)[5],
                                     error_gps_low = summ.model[6, 1],
                                     error_gps_mean = summ.model[6, 2],
                                     error_gps_high = summ.model[6, 3],
                                     error_gps.units = rownames(summary(fit_list.c[[i]])$CI)[6])
      
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
                                     error_gps.units = NA
                                     )
      
    }
    
    
  }
  
  sum.res.c <- as.data.frame(do.call(rbind, summ_list.c))
  
  
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
  
  
  print("Calculating DISTANCE and summary statistics.")
  
  speed.stat.t <- list()
  
  for(i in 1:length(data.tel.t)){
    days <- cut(data.tel.t[[i]]$timestamp, breaks = "day")
    days <- unique(days)
    d.length <- length(days)
    
    if(!is.na(speed_list.t[[i]])){
      summ.speed <- speed_list.t[[i]]$CI
      summ.dist <- summ.speed * d.length * 24 * 3600 / 1000
      summ.speed <- summ.speed * 3.6 # m/s to km/h
      
      speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = d.length,
                                      distance_low = summ.dist[1, 1],
                                      distance_mean = summ.dist[1, 2],
                                      distance_high = summ.dist[1, 3],
                                      distance.units = "km",# rownames(speed_list.t[[i]]$CI)[1],
                                      speed_low = summ.speed[1, 1],
                                      speed_mean = summ.speed[1, 2],
                                      speed_high = summ.speed[1, 3],
                                      speed.units = rownames(speed_list.t[[i]]$CI)[1])
    } else{
      speed.stat.t[[i]] <- data.frame(ind.id = names.list_t[[i]],
                                      days = NA,
                                      distance_low = NA,
                                      distance_mean = NA,
                                      distance_high = NA,
                                      distance.units = "km",
                                      speed_low = NA,
                                      speed_mean = NA,
                                      speed_high = NA,
                                      speed.units = "km/h")
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
      summ.speed <- summ.speed * 3.6 # m/s to km/h
      
      speed.stat.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                      days = d.length,
                                      distance_low = summ.dist[1, 1],
                                      distance_mean = summ.dist[1, 2],
                                      distance_high = summ.dist[1, 3],
                                      distance.units = "km", #rownames(speed_list.c[[i]]$CI)[1],
                                      speed_low = summ.speed[1, 1],
                                      speed_mean = summ.speed[1, 2],
                                      speed_high = summ.speed[1, 3],
                                      speed.units = rownames(speed_list.c[[i]]$CI)[1])
      
    } else{
      speed.stat.c[[i]] <- data.frame(ind.id = names.list_c[[i]],
                                      days = NA,
                                      distance_low = NA,
                                      distance_mean = NA,
                                      distance_high = NA,
                                      distance.units = "km",
                                      speed_low = NA,
                                      speed_mean = NA,
                                      speed_high = NA,
                                      speed.units = "km/h")
    }
    
    
  }
  
  speed.dist.df.c <- as.data.frame(do.call(rbind, speed.stat.c))
  
  t.list <- list(summary = sum.res.t, akde = akde.area.df.t, speed.dist = speed.dist.df.t)
  c.list <- list(summary = sum.res.c, akde = akde.area.df.c, speed.dist = speed.dist.df.c)
  
  res.list <- list(treatment = t.list, control = c.list)
  
  if(!is.na(export.folder)){
    print(paste0("Saving results to: ", export.folder))
    
    writexl::write_xlsx(res.list$treatment$summary, paste0(export.folder, "/treatment.site_summary.xlsx"))
    writexl::write_xlsx(res.list$treatment$akde, paste0(export.folder, "/treatment.site_akde.xlsx"))
    writexl::write_xlsx(res.list$treatment$speed.dist, paste0(export.folder, "/treatment.site_speed_dist.xlsx"))
    
    writexl::write_xlsx(res.list$control$summary, paste0(export.folder, "/control.site_summary.xlsx"))
    writexl::write_xlsx(res.list$control$akde, paste0(export.folder, "/control.site_akde.xlsx"))
    writexl::write_xlsx(res.list$control$speed.dist, paste0(export.folder, "/control.site_speed_dist.xlsx"))
    
  }
  
  return(res.list)
  
}


period.summary_just_akde_diff <- function(period_result = period_result, export.folder = ""){
  
  
  data.tel.t <- period_result$telemetry.objects
  
  fit_list.t <- period_result$fit.objects
  
  akde.list.t <- period_result$akde.objects
  
  
  print("Calculating summary statistics.")
  
  # Assign names
  
  names.list_t <- list()
  for(i in 1:length(data.tel.t)){
    names.list_t[[i]] <- data.tel.t[[i]]@info$identity
  }
  
  
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
                                     error_gps.units = rownames(summary(fit_list.t[[i]])$CI)[6]
      )
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
                                     error_gps.units = NA
      )
      
    }
    
    
  }
  
  sum.res.t <- as.data.frame(do.call(rbind, summ_list.t))
  
  
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

  t.list <- list(summary = sum.res.t, akde = akde.area.df.t)
  
  res.list <- list(results = t.list)
  
  if(!is.na(export.folder)){
    print(paste0("Saving results to: ", export.folder))
    
    writexl::write_xlsx(res.list$results$summary, paste0(export.folder, "/summary_all.xlsx"))
    writexl::write_xlsx(res.list$results$akde, paste0(export.folder, "/akde_summary.xlsx"))
    
  }
  
  return(res.list)
  
}



period.meta.plots <- function(period_result = period_result, period = c("Pre Breeding", "Breeding", "Post Breeding", "Baseline"), male_female = c("Male", "Female"), export.folder = ""){
  
  print("Export Home range areas - plots")
  # Home range areas - plots
  
  t.akde <- period_result$treatment$akde.objects
  png(paste0(export.folder, "/Rplot_treatment_AKDE.png"), width = 25, height = 40, units='cm', res = 600)
  plot_ud(UD_list = t.akde,
          level_vec = 0.95,
          color_vec = viridis::viridis(length(t.akde)),
          option = c("contour", "interval", "location"), #  "contour", "interval", "location"
          columns = 2,
          cex = 0.65,
          tele_list = period_result$treatment$telemetry.objects) 
  dev.off()
  
  c.akde <- period_result$control$akde.objects
  png(paste0(export.folder, "/Rplot_control_AKDE.png"), width = 25, height = 30, units='cm', res = 600)
  plot_ud(UD_list = c.akde,
          level_vec = 0.95,
          color_vec = viridis::viridis(length(c.akde)),
          option = c("contour", "interval", "location"), #  "contour", "interval", "location"
          columns = 2,
          cex = 0.65,
          tele_list = period_result$control$telemetry.objects) 
  dev.off()
  
  print("Export META - home range area - plots")
  # META - home range area
  png(paste0(export.folder, "/Rplot_META_results_AKDE.png"), width = 25, height = 18, units='cm', res = 600)
  
  par(mfrow = c(1, 2))
  
  ctmm::meta(period_result$control$akde.objects, 
             sort = TRUE, 
             col = viridis::viridis(length(period_result$control$akde.objects))) + 
    title("AKDE Home range area\nRockefeller population (control site)", sub = paste0(period, " period - ", male_female," population")) 
  
  ctmm::meta(period_result$treatment$akde.objects, 
             sort = TRUE, 
             col = viridis::viridis(length(period_result$treatment$akde.objects))) +
    title("AKDE Home range area\nStaten Island population (treatment site)", sub = paste0(period, " period - ", male_female," population"))
  
  dev.off()
  
  par(mfrow = c(1, 1))
  
  print("Export META - home range area - all in one plot")
  
  # META - home range area - all in one
  
  png(paste0(export.folder, "/Rplot_META_results_AKDE_all.png"), width = 25, height = 18, units='cm', res = 600)
  par(mfrow = c(1, 1))
  ctmm::meta(c(period_result$control$akde.objects, period_result$treatment$akde.objects), 
             sort = TRUE, 
             col = viridis::viridis(length(c(period_result$control$akde.objects, period_result$treatment$akde.objects)))) 
  dev.off()
  
  print("Export AKDE - home range area - maps plot")
  
  # Plots
  
  png(paste0(export.folder, "/Rplot_maps_AKDE.png"), width = 25, height = 18, units='cm', res = 600)
  
  par(mfrow = c(1, 2))
  
  plot(period_result$control$akde.objects, 
       col.DF = viridis::viridis(length(period_result$control$akde.objects)), 
       col.level = viridis::viridis(length(period_result$control$akde.objects)), 
       col.grid = NA, 
       level = NA, 
       labels = names(period_result$control$akde.objects)) +
    title("AKDE Home range area\nRockefeller population (control site)", sub = paste0(period, " period - ", male_female," population")) 
  
  plot(period_result$treatment$akde.objects, 
       col.DF = viridis::viridis(length(period_result$treatment$akde.objects)), 
       col.level = viridis::viridis(length(period_result$treatment$akde.objects)), 
       col.grid = NA, 
       level = NA, 
       labels = names(period_result$treatment$akde.objects)) +
    title("AKDE Home range area\nStaten Island population (treatment site)", sub = paste0(period, " period - ", male_female," population"))
  
  
  dev.off()
  
  par(mfrow = c(1, 1))
  
  
  return()
  
}




# Function to subset the data - NEW periods
# ------------------------------------------------------------------------------

# Breeding	16 Oct- 31 Dec
# Post Breeding	1 Jan - 15 March
# Baseline	16 March - 30 May

# Parameters:
## dataset = data.raw
## time_period = NA or "Breeding" or "Post Breeding" or "Baseline")
## sex = NA or "M" or "F"
## study_area = NA or "Staten Island" or "Rockefeller"
## animal_color = NA or "W" or "Y"


# data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered_NEW.csv", stringsAsFactors = FALSE, header = TRUE) %>%
#   as.data.frame()
# 
# check_table <- readxl::read_xlsx("Analysis/stats_ndays_per_period_per_site_per_sex_NEW.xlsx") %>%
#   as.data.frame()



subset_func_new <- function(dataset = dataset, check_table = check_table, time_period = NA, male_female = NA, study_area = NA, animal_color = NA){
  
  time_periods <- data.frame(period_name = c("Breeding", "Post Breeding", "Baseline"),
                             start_date = c("2021-10-16", "2022-01-01", "2022-03-16"),
                             end_date = c("2021-12-31", "2022-03-15", "2022-05-30"))
  
  if(!is.na(study_area)){
    dataset %<>%
      dplyr::filter(study.area == study_area)
    
    check_table %<>% 
      dplyr::filter(study.area == study_area)
  }
  
  if(!is.na(male_female)){
    dataset %<>%
      dplyr::filter(sex == male_female)
    
    check_table %<>% 
      dplyr::filter(sex == male_female)
  }
  
  # if(!is.na(animal_color)){
  #   dataset %<>%
  #     dplyr::filter(Color == animal_color)
  # }
  
  if(time_period == "Breeding"){
    
    check_table %<>% 
      dplyr::filter(Use_breeding == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_breeding, date_first_plus_five_breeding, date_last_minus_two_breeding)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      dataset_ind <- dataset %>%
          dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
          dplyr::filter(between(Date, as.Date(check_table_ind$date_first_plus_five_breeding), as.Date(check_table_ind$date_last_minus_two_breeding)))
        
      ind_list[[i]] <- dataset_ind 
    }
     
    
  } else if(time_period == "Post Breeding"){
    
    check_table %<>% 
      dplyr::filter(Use_postbreeding == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_postbreeding, date_first_plus_five_postbreeding, date_last_minus_two_postbreeding)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      if(check_table_ind$`individual-local-identifier` == "28"){
        
        dataset_ind <- dataset %>%
          dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
          dplyr::filter(between(Date, as.Date(check_table_ind$date_first_plus_five_postbreeding), as.Date(check_table_ind$date_last_minus_two_postbreeding)))
        
        dataset_ind_sf <- st_as_sf(dataset_ind, coords = c("location-long", "location-lat"), crs = 4326)
        
        bound28 <- st_read("Analysis/individual28_postbreeding_boundary.gpkg")
        
        dataset_ind_sf_sub <- dataset_ind_sf[bound28, ]
        
        dataset_ind <- dataset_ind_sf_sub %>%
          dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                        `location-lat` =  st_coordinates(.)[, 2]) %>%
          st_drop_geometry() %>%
          as.data.frame()
        
      } else{
        dataset_ind <- dataset %>%
          dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
          dplyr::filter(between(Date, as.Date(check_table_ind$date_first_plus_five_postbreeding), as.Date(check_table_ind$date_last_minus_two_postbreeding)))
        
      }
      
            
      ind_list[[i]] <- dataset_ind 
    }
    
    
  } else if(time_period == "Baseline"){
    
    check_table %<>% 
      dplyr::filter(Use_baseline == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_baseline, date_first_plus_five_baseline, date_last_minus_two_baseline)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      dataset_ind <- dataset %>%
        dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
        dplyr::filter(between(Date, as.Date(check_table_ind$date_first_plus_five_baseline), as.Date(check_table_ind$date_last_minus_two_baseline)))
      
      ind_list[[i]] <- dataset_ind 
    }
    
  } else{
    print("Period must be 'Breeding' or 'Post Breeding' or 'Baseline'.")
    return()
  }
  
  
  dataset_subset <- rbindlist(ind_list, use.names = TRUE) %>% 
    as.data.frame()
  
  
  return(dataset_subset)
}

# mapviewOptions(fgb = FALSE)
# subData <- subset_func_new(dataset = data.raw, check_table = check_table, time_period = "Breeding", male_female = "F", study_area = "Staten Island")
# unique(subData$`individual-local-identifier`)

# subData <- subset_func_new(dataset = data.raw, check_table = check_table, time_period = "Post Breeding", male_female = "F", study_area = "Rockefeller")
#
# map_individual(df.site = subData, ind.id = unique(subData$`individual-local-identifier`), burst = FALSE)
#
# subData28 <- subData %>% dplyr::filter(`individual-local-identifier` == "28")
# length(unique(subData28$Date))
#
# df.site <- subData %>%
#   dplyr::filter(`individual-local-identifier` == "28") %>%
#   dplyr::select(-c(timestamp, Date, Time))
#
# df.site.sf <- st_as_sf(df.site, coords = c("location-long", "location-lat"), crs = 4326)
#
# sf::st_write(df.site.sf, "Analysis/individual28_postbreeding.gpkg")
#
# bound28 <- st_read("Analysis/individual28_postbreeding_boundary.gpkg")
#
# mapview(df.site.sf, layer.name = "Ind 28 postbreeding") + mapview(bound28, col.regions = "red")
#
# df.site.sf_sub <- df.site.sf[bound28, ]
# mapview(df.site.sf_sub)




subset_func_akde_diff <- function(dataset = dataset, check_table = check_table, time_period = NA){
  
  time_periods <- data.frame(period_name = c("Breeding", "Post Breeding", "Baseline"),
                             start_date = c("2021-10-21", "2022-01-05", "2022-03-21"),
                             end_date = c("2021-12-29", "2022-03-13", "2022-05-28"))
  
  tm <- time_periods %>% dplyr::filter(period_name == time_period)
  
  bound <- st_read("Analysis/individuals_akde_diff_boundary.gpkg")
  
  if(time_period == "Breeding"){
    
    check_table %<>% 
      dplyr::filter(Use_breeding == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_breeding)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      dataset_ind <- dataset %>%
        dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
        dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date)))
      
      dataset_ind_sf <- st_as_sf(dataset_ind, coords = c("location-long", "location-lat"), crs = 4326)
      
      dataset_ind_sf_sub <- dataset_ind_sf[bound, ]
      
      dataset_ind <- dataset_ind_sf_sub %>%
        dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                      `location-lat` =  st_coordinates(.)[, 2]) %>%
        st_drop_geometry() %>%
        as.data.frame()

      ind_list[[i]] <- dataset_ind 
    }
    
    
  } else if(time_period == "Post Breeding"){
    
    check_table %<>% 
      dplyr::filter(Use_postbreeding == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_postbreeding)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      dataset_ind <- dataset %>%
        dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
        dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date)))
      
      dataset_ind_sf <- st_as_sf(dataset_ind, coords = c("location-long", "location-lat"), crs = 4326)
      
      dataset_ind_sf_sub <- dataset_ind_sf[bound, ]
      
      dataset_ind <- dataset_ind_sf_sub %>%
        dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                      `location-lat` =  st_coordinates(.)[, 2]) %>%
        st_drop_geometry() %>%
        as.data.frame()
      
      ind_list[[i]] <- dataset_ind 
    }
    
  } else if(time_period == "Baseline"){
    
    check_table %<>% 
      dplyr::filter(Use_baseline == "TRUE") %>%
      dplyr::select(`individual-local-identifier`, Use_baseline)
    
    ind_list <- list()
    
    for(i in 1:length(check_table$`individual-local-identifier`)){
      check_table_ind <- check_table[i, ]
      
      dataset_ind <- dataset %>%
        dplyr::filter(`individual-local-identifier` %in% check_table_ind$`individual-local-identifier`) %>%
        dplyr::filter(between(Date, as.Date(tm$start_date), as.Date(tm$end_date)))
      
      dataset_ind_sf <- st_as_sf(dataset_ind, coords = c("location-long", "location-lat"), crs = 4326)
      
      
      
      dataset_ind_sf_sub <- dataset_ind_sf[bound, ]
      
      dataset_ind <- dataset_ind_sf_sub %>%
        dplyr::mutate(`location-long` = st_coordinates(.)[, 1],
                      `location-lat` =  st_coordinates(.)[, 2]) %>%
        st_drop_geometry() %>%
        as.data.frame()
      
      ind_list[[i]] <- dataset_ind 
    }
    
  } else{
    print("Period must be 'Breeding' or 'Post Breeding' or 'Baseline'.")
    return()
  }
  
  dataset_subset <- rbindlist(ind_list, use.names = TRUE) %>% 
    as.data.frame()
  
  
}

# --------------
# check.akde.diff <- readxl::read_xlsx("Analysis/stats_only_akde_diff_calc.xlsx") %>% as.data.frame()
# 
# data.raw <- data.table::fread("Data/Odocoileus virginianus DeNicola Staten Island, NY and Rockefeller Park, NY_new.csv",
#                               stringsAsFactors = FALSE,
#                               header = TRUE) %>%
#   as.data.frame()
# 
# study_animals <- readxl::read_xlsx("Data/Study_Animals.xlsx" , sheet = "Sheet2") %>%
#   as.data.frame()
# 
# 
# study_animals %<>%
#   dplyr::mutate(Color = str_sub(ID, - 1, - 1),
#                 ID = tolower(sub('[YW]$', '', ID)))
# 
# # data.raw %<>% dplyr::filter(`individual-local-identifier` %in% study_animals$ID)
# data.raw %<>% left_join(., study_animals %>% dplyr::select(ID, `study area`, sex, Color), by = c("individual-local-identifier" = "ID"))
# data.raw %<>% dplyr::rename(study.area = `study area`)
# 
# data.raw %<>% dplyr::mutate(
#   Date = as.Date(timestamp))
# 
# data.raw %<>% dplyr::filter(!(`location-long` == 0 | `location-lat` == 0))
# 
# data.raw %<>% dplyr::mutate(
#   Date = as.Date(timestamp),
#   Time = format(as.POSIXct(timestamp), format = "%H:%M:%S")) %>%
#   # dplyr::select(-timestamp) %>%
#   dplyr::mutate(Year = substr(Date, 1, 4),
#                 Month = substr(Date, 6, 7),
#                 Day = substr(Date, 9, 10))
# 

# --------------

# data.akde.diff.subset <- subset_func_akde_diff(dataset = data.raw, check_table = check.akde.diff, time_period = "Post Breeding")
# 
# unique(data.akde.diff.subset$`individual-local-identifier`)
# mapviewOptions(fgb = FALSE)
# map_individual(df.site = data.akde.diff.subset, ind.id = unique(data.akde.diff.subset$`individual-local-identifier`), burst = TRUE)


# --------------

# dataset_ind1 <- data.raw %>%
#   dplyr::filter(`individual-local-identifier` %in% missing.ind)
# df.site.sf <- st_as_sf(dataset_ind1, coords = c("location-long", "location-lat"), crs = 4326)
# map_individual(df.site = df.site.sf, ind.id = unique(df.site.sf$`individual-local-identifier`), burst = TRUE)
# 
# sf::st_write(df.site.sf, "Analysis/individuals_akde_diff.gpkg")

# --------------




# Function to AKDE/Diffusion from individuals with less data
# ------------------------------------------------------------------------------

# cal.model <- UERE <- readRDS("Data/undep/calibration.error.model.rds")

calc_periods_akde_diff <- function(input.data = input.data, period = c("Breeding", "Post Breeding", "Baseline"), use.ctmm = c("fit", "select"), cal.model = NA, check_table = check_table, export.folder = ""){
  
  print("Subseting data to treatment and control sites.")
  
  
  data.subset.t <- subset_func_akde_diff(dataset = input.data, 
                                   check_table = check_table, 
                                   time_period = period)
  
  
  plot.t <- plot_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`))
  
  if(!is.na(export.folder)){
    # export plots
    
    ggsave(plot = plot.t,
           filename = paste0(export.folder, "/", period, "_plot.akde_diff.jpg"),
           width = 45,
           height = 40,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }

  data.tel.t <- as.telemetry(data.subset.t, datum = 'EPSG:4326')
  
  
  if(!is.null(cal.model)){
    UERE <- cal.model
    UERE$DOF[] <- UERE$UERE
    
    uere(data.tel.t) <- UERE
  }
  
  
  print("Detecting and removing outliers!")
  
  # Outlier detection
  # ------------------------------------------------------------------------------
  outliers.tel.t <- outlie(data.tel.t, plot = FALSE)

  # Remove outliers
  # ------------------------------------------------------------------------------
  
  for(i in 1:length(outliers.tel.t)){
    print(paste0("Max speed value: ", max(outliers.tel.t[[i]]$speed) * 100, " cm/s"))
    x <- which(outliers.tel.t[[i]]$speed * 100 >= 17.8816)
    print(paste0("Rows to remove: ", length(x)))
    if(length(x) > 0){
      data.tel.t[[i]] <- data.tel.t[[i]][-x, ]
    }
    print(paste0("Processed: ", i, "/", length(outliers.tel.t), " individuals"))
    print("------------------------------------------------------------------")
  }
  
  # Assign names
  
  names.list_t <- list()
  for(i in 1:length(data.tel.t)){
    names.list_t[[i]] <- data.tel.t[[i]]@info$identity
  }
  
  
  print("Calculating variograms.")
  
  variogram_list.t <- list()
  
  for(i in 1:length(data.tel.t)){
    variogram_list.t[[i]] <- variogram(data.tel.t[[i]], error = TRUE)
  }
  
  names(variogram_list.t) <- names.list_t
  
  print(paste0("Model fitting using ctmm: ", use.ctmm, " method."))
  
  guess_list.t <- list()

  for(i in 1:length(data.tel.t)){
    guess_list.t[[i]] <- ctmm.guess(data.tel.t[[i]], variogram = variogram_list.t[[i]], CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)
  }
  
  names(guess_list.t) <- names.list_t
  
  if(use.ctmm == "fit"){
    
    fit_list.t <- list()

    for(i in 1:length(data.tel.t)){
      fit_list.t[[i]] <- tryCatch(
        {
          ctmm.fit(data.tel.t[[i]], guess_list.t[[i]])
        },
        error = function(e){
          NA
        }
      )
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.t), " individuals with less data!"))
    }
    
    names(fit_list.t) <- names.list_t
    
  } else if(use.ctmm == "select"){
    
    
    fit_list.t <- list()
    
    for(i in 1:length(data.tel.t)){
      fit_list.t[[i]] <- tryCatch(
        {
          ctmm.select(data.tel.t[[i]], guess_list.t[[i]], trace = 3, verbose = TRUE, cores = 15)
        },
        error = function(e){
          NA
        }
      )
        
      print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.t), " individuals with less data!"))
    }
    
    names(fit_list.t) <- names.list_t
    
  } else{
    print("Please provide valid method.")
    return()
  }
  
  print("Model fitting DONE!")
  
  akde.list.t <- list()
  
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
  
  
  names(akde.list.t) <- names.list_t

  # speed_list.t <- list()
  # 
  # print("Calculating SPEED.")
  # 
  # for(i in 1:length(data.tel.t)){
  #   speed_list.t[[i]] <- tryCatch(
  #     {
  #       ctmm::speed(object = data.tel.t[[i]], CTMM = fit_list.t[[i]], units = FALSE, cores = 0)
  #     },
  #     error = function(e){
  #       NA
  #     }
  #   )
  #   print(paste0("Calculating SPEED DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
  # }
  # 
  # names(speed_list.t) <- names.list_t


  res.list_less <- list(telemetry.objects = data.tel.t, akde.objects = akde.list.t, fit.objects = fit_list.t, variograms = variogram_list.t) # speed.objects = speed_list.t, 

  print("Calculation DONE!")
  
  if(!is.na(export.folder)){
    print(paste0("Saving results to: ", export.folder))
    
    saveRDS(res.list_less, paste0(export.folder, "/", period, "_period_results.RDS"))
    
  }
  
  return(res.list_less)
  
}

# Make AKDE area meta analysis plot

akde_meta_plot <- function(t.akde = t.akde, c.akde = c.akde, export.folder = "", plot.width = 45, plot.height = 25){
  
  # if(area_units == "km2"){
  #   
  # } else if(area_units == "hectares"){
  #   
  # } else {
  #   return()
  # }
  
  t.akde %<>% dplyr::mutate(area_low = case_when(area.units == "area (square kilometers)" ~ area_low,
                                                 area.units == "area (hectares)" ~ area_low/100),
                            area_mean = case_when(area.units == "area (square kilometers)" ~ area_mean,
                                                  area.units == "area (hectares)" ~ area_mean/100),
                            area_high = case_when(area.units == "area (square kilometers)" ~ area_high,
                                                  area.units == "area (hectares)" ~ area_high/100),
                            area.units = "square kilometers")
  
  
  c.akde %<>% dplyr::mutate(area_low = case_when(area.units == "area (square kilometers)" ~ area_low,
                                                 area.units == "area (hectares)" ~ area_low/100),
                            area_mean = case_when(area.units == "area (square kilometers)" ~ area_mean,
                                                  area.units == "area (hectares)" ~ area_mean/100),
                            area_high = case_when(area.units == "area (square kilometers)" ~ area_high,
                                                  area.units == "area (hectares)" ~ area_high/100),
                            area.units = "square kilometers")
  
  
  t.akde_mean <- data.frame(ind.id = "mean", 
                            area_low = mean(t.akde$area_low),
                            area_mean = mean(t.akde$area_mean),
                            area_high = mean(t.akde$area_high),
                            area.units = "square kilometers")
  
  t.akde <- rbind(t.akde, t.akde_mean)
  
  c.akde_mean <- data.frame(ind.id = "mean", 
                            area_low = mean(c.akde$area_low),
                            area_mean = mean(c.akde$area_mean),
                            area_high = mean(c.akde$area_high),
                            area.units = "square kilometers")
  
  c.akde <- rbind(c.akde, c.akde_mean)
  
  g1 <- ggplot(t.akde) +
    geom_bar(aes(y = ind.id, x = area_mean, fill = ind.id), stat="identity", alpha=0.7) +
    geom_errorbar(aes(y = ind.id, xmin = area_low, xmax = area_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = t.akde, aes(y = ind.id, x = area_mean, label=round(area_mean, 2)), nudge_y = 0.4, nudge_x = 0.15, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(t.akde$area_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "D", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$area_high, c.akde$area_high))+0.5)) +
    ggtitle("AKDE Home Range Area\nStaten Island population") +
    xlab("95% Area [km2]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  
  g2 <- ggplot(c.akde) +
    geom_bar(aes(y = ind.id, x = area_mean, fill = ind.id), alpha=0.7, stat="identity") +
    geom_errorbar(aes(y = ind.id, xmin = area_low, xmax = area_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = c.akde, aes(y = ind.id, x = area_mean, label=round(area_mean, 2)), nudge_y = 0.4, nudge_x = 0.15, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(c.akde$area_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "D", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$area_high, c.akde$area_high))+0.5)) +
    ggtitle("AKDE Home Range Area\nRockefeller population") +
    xlab("95% Area [km2]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))

  gr1 <- grid.arrange(g1, g2, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gr1,
           filename = paste0(export.folder, "/", "akde_meta_treatment_vs_control.jpg"),
           width = plot.width,
           height = plot.height,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }
  
  return(gr1)
}

# Make Speed meta analysis plot 

speed_meta_plot <- function(t.akde = t.akde, c.akde = c.akde, export.folder = "", plot.width = 45, plot.height = 25){
  
  t.akde %<>% replace(is.na(.), 0) 
  t.akde %<>% dplyr::select(ind.id, speed_low, speed_mean, speed_high, speed.units)
  
  c.akde %<>% replace(is.na(.), 0)
  c.akde %<>% dplyr::select(ind.id, speed_low, speed_mean, speed_high, speed.units)
  
  
  t.akde_mean <- data.frame(ind.id = "mean", 
                            speed_low = mean(t.akde$speed_low),
                            speed_mean = mean(t.akde$speed_mean),
                            speed_high = mean(t.akde$speed_high),
                            speed.units = "km/h")
  
  t.akde <- rbind(t.akde, t.akde_mean)
  
  c.akde_mean <- data.frame(ind.id = "mean", 
                            speed_low = mean(c.akde$speed_low),
                            speed_mean = mean(c.akde$speed_mean),
                            speed_high = mean(c.akde$speed_high),
                            speed.units = "km/h")
  
  c.akde <- rbind(c.akde, c.akde_mean)
  
  g1 <- ggplot(t.akde) +
    geom_bar(aes(y = ind.id, x = speed_mean, fill = ind.id), stat="identity", alpha=0.7) +
    geom_errorbar(aes(y = ind.id, xmin = speed_low, xmax = speed_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = t.akde, aes(y = ind.id, x = speed_mean, label=round(speed_mean, 2)), nudge_y = 0.4, nudge_x = 0.017, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(t.akde$speed_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "G", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$speed_high, c.akde$speed_high))+0.05)) +
    ggtitle("Traveling speed per day\nStaten Island population") +
    xlab("95% Speed [km/h]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  
  g2 <- ggplot(c.akde) +
    geom_bar(aes(y = ind.id, x = speed_mean, fill = ind.id), alpha=0.7, stat="identity") +
    geom_errorbar(aes(y = ind.id, xmin = speed_low, xmax = speed_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = c.akde, aes(y = ind.id, x = speed_mean, label=round(speed_mean, 2)), nudge_y = 0.4, nudge_x = 0.017, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(c.akde$speed_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "G", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$speed_high, c.akde$speed_high))+0.05)) +
    ggtitle("Traveling speed per day\nRockefeller population") +
    xlab("95% Speed [km/h]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  gr1 <- grid.arrange(g1, g2, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gr1,
           filename = paste0(export.folder, "/", "speed_meta_treatment_vs_control.jpg"),
           width = plot.width,
           height = plot.height,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }
  
  return(gr1)
}

distance_meta_plot <- function(t.akde = t.akde, c.akde = c.akde, export.folder = "", plot.width = 45, plot.height = 25){
  
  t.akde %<>% replace(is.na(.), 0) 
  t.akde %<>% dplyr::select(ind.id, distance_low, distance_mean, distance_high, distance.units)
  
  c.akde %<>% replace(is.na(.), 0)
  c.akde %<>% dplyr::select(ind.id, distance_low, distance_mean, distance_high, distance.units)
  
  
  t.akde_mean <- data.frame(ind.id = "mean", 
                            distance_low = mean(t.akde$distance_low),
                            distance_mean = mean(t.akde$distance_mean),
                            distance_high = mean(t.akde$distance_high),
                            distance.units = "km")
  
  t.akde <- rbind(t.akde, t.akde_mean)
  
  c.akde_mean <- data.frame(ind.id = "mean", 
                            distance_low = mean(c.akde$distance_low),
                            distance_mean = mean(c.akde$distance_mean),
                            distance_high = mean(c.akde$distance_high),
                            distance.units = "km")
  
  c.akde <- rbind(c.akde, c.akde_mean)
  
  g1 <- ggplot(t.akde) +
    geom_bar(aes(y = ind.id, x = distance_mean, fill = ind.id), stat="identity", alpha=0.7) +
    geom_errorbar(aes(y = ind.id, xmin = distance_low, xmax = distance_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = t.akde, aes(y = ind.id, x = distance_mean, label=round(distance_mean, 2)), nudge_y = 0.4, nudge_x = 40, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(t.akde$distance_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "F", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$distance_high, c.akde$distance_high))+45)) +
    ggtitle("Traveled distance per period\nStaten Island population") +
    xlab("95% Distance [km]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  
  g2 <- ggplot(c.akde) +
    geom_bar(aes(y = ind.id, x = distance_mean, fill = ind.id), alpha=0.7, stat="identity") +
    geom_errorbar(aes(y = ind.id, xmin = distance_low, xmax = distance_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = c.akde, aes(y = ind.id, x = distance_mean, label=round(distance_mean, 2)), nudge_y = 0.4, nudge_x = 40, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(c.akde$distance_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "F", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$distance_high, c.akde$distance_high))+45)) +
    ggtitle("Traveled distance per period\nRockefeller population") +
    xlab("95% Distance [km]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))

  gr1 <- grid.arrange(g1, g2, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gr1,
           filename = paste0(export.folder, "/", "distance_meta_treatment_vs_control.jpg"),
           width = plot.width,
           height = plot.height,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }
  
  return(gr1)
}
  
diffusion_meta_plot <- function(t.akde = t.akde, c.akde = c.akde, export.folder = "", plot.width = 45, plot.height = 25){
  
  t.akde %<>% dplyr::select(ind.id, diffusion_low, diffusion_mean, diffusion_high, diffusion.units)
  t.akde %<>% replace(is.na(.), 0) 
  
  c.akde %<>% dplyr::select(ind.id, diffusion_low, diffusion_mean, diffusion_high, diffusion.units)
  c.akde %<>% replace(is.na(.), 0)
  
  
  t.akde %<>% dplyr::mutate(diffusion_low = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_low,
                                                      diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_low*100),
                            diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean*100),
                            diffusion_high = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_high,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_high*100),
                            diffusion.units = "hectares/day")
  
  
  c.akde %<>% dplyr::mutate(diffusion_low = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_low,
                                                      diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_low*100),
                            diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean*100),
                            diffusion_high = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_high,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_high*100),
                            diffusion.units = "hectares/day")
  
  
  t.akde_mean <- data.frame(ind.id = "mean", 
                            diffusion_low = mean(t.akde$diffusion_low),
                            diffusion_mean = mean(t.akde$diffusion_mean),
                            diffusion_high = mean(t.akde$diffusion_high),
                            diffusion.units = "hectares/day")
  
  t.akde <- rbind(t.akde, t.akde_mean)
  
  c.akde_mean <- data.frame(ind.id = "mean", 
                            diffusion_low = mean(c.akde$diffusion_low),
                            diffusion_mean = mean(c.akde$diffusion_mean),
                            diffusion_high = mean(c.akde$diffusion_high),
                            diffusion.units = "hectares/day")
  
  c.akde <- rbind(c.akde, c.akde_mean)
  
  
  g1 <- ggplot(t.akde) +
    geom_bar(aes(y = ind.id, x = diffusion_mean, fill = ind.id), stat="identity", alpha=0.7) +
    geom_errorbar(aes(y = ind.id, xmin = diffusion_low, xmax = diffusion_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = t.akde, aes(y = ind.id, x = diffusion_mean, label=round(diffusion_mean, 2)), nudge_y = 0.3, nudge_x = 8, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(t.akde$diffusion_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "E", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$diffusion_high, c.akde$diffusion_high))+12)) +
    ggtitle("Diffusion estimaed per day\nStaten Island population") +
    xlab("95% Diffusion [hectares/day]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  
  g2 <- ggplot(c.akde) +
    geom_bar(aes(y = ind.id, x = diffusion_mean, fill = ind.id), alpha=0.7, stat="identity") +
    geom_errorbar(aes(y = ind.id, xmin = diffusion_low, xmax = diffusion_high), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
    geom_text(data = c.akde, aes(y = ind.id, x = diffusion_mean, label=round(diffusion_mean, 2)), nudge_y = 0.3, nudge_x = 8, size = 4, color = "black", fontface = "bold") +
    geom_vline(xintercept = mean(c.akde$diffusion_mean), color = "red", size = 1) +
    scale_fill_viridis(option = "E", discrete = T, "Individual: ") +
    scale_x_continuous(limits = c(0, max(c(t.akde$diffusion_high, c.akde$diffusion_high))+12)) +
    ggtitle("Diffusion estimaed per day\nRockefeller population") +
    xlab("95% Diffusion [hectares/day]")+
    ylab("") +
    theme_bw() +
    theme(axis.text = element_text(face="bold"))
  
  gr1 <- grid.arrange(g1, g2, ncol = 2)
  if(!is.na(export.folder)){
    ggsave(plot = gr1,
           filename = paste0(export.folder, "/", "diffusion_meta_treatment_vs_control.jpg"),
           width = plot.width,
           height = plot.height,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }
  
  return(gr1)

}



meta_analysis <- function(period.result = period.result, export.folder = ""){
  
  # Treatment
  
  meta.akde.df <- ctmm::meta(period.result$treatment$akde.objects, variable = "area", plot = FALSE) 
  meta.akde.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.a <- data.frame(variable = "AKDE area meta analysis", low = NA, est = NA, high = NA)
  
  meta.diff.df <- ctmm::meta(period.result$treatment$fit.objects, variable = "diffusion", plot = FALSE)
  meta.diff.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.d <- data.frame(variable = "Diffusion meta analysis", low = NA, est = NA, high = NA)
  
  meta.tau.df <- ctmm::meta(period.result$treatment$fit.objects, variable = "tau position", plot = FALSE)
  meta.tau.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.t <- data.frame(variable = "Tau position meta analysis", low = NA, est = NA, high = NA)
  
  df.meta.t <- rbind(df.a, meta.akde.df, df.d, meta.diff.df, df.t, meta.tau.df)
  
  
  # Control
  
  meta.akde.df <- ctmm::meta(period.result$control$akde.objects, variable = "area", plot = FALSE) 
  meta.akde.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.a <- data.frame(variable = "AKDE area meta analysis", low = NA, est = NA, high = NA)
  
  meta.diff.df <- ctmm::meta(period.result$control$fit.objects, variable = "diffusion", plot = FALSE)
  meta.diff.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.d <- data.frame(variable = "Diffusion meta analysis", low = NA, est = NA, high = NA)
  
  meta.tau.df <- ctmm::meta(period.result$control$fit.objects, variable = "tau position", plot = FALSE)
  meta.tau.df %<>% as.data.frame() %>%  tibble::rownames_to_column("variable") %>%
    as.data.frame()
  
  df.t <- data.frame(variable = "Tau position meta analysis", low = NA, est = NA, high = NA)
  
  df.meta.c <- rbind(df.a, meta.akde.df, df.d, meta.diff.df, df.t, meta.tau.df)
  
  meta.list <- list(treatment = df.meta.t, control = df.meta.c)
  
  if(!is.na(export.folder)){
    writexl::write_xlsx(meta.list, paste0(export.folder, "/", "meta_analysis_treatment_vs_control.xlsx"))
  }
  
  return(meta.list)
}


# Correaltion
# ------------------------------------------------------------------------------
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


correlation_plot <- function(t.akde = t.akde, c.akde = c.akde, export.folder = "", plot.width = 45, plot.height = 25){
  
  t.akde %<>% replace(is.na(.), 0) 
  c.akde %<>% replace(is.na(.), 0)
  
  t.akde %<>% dplyr::mutate(t_position.units = str_extract_all(t_position.units, "\\([^()]+\\)"),
                            t_velocity.units = str_extract_all(t_velocity.units, "\\([^()]+\\)")) 
  
  c.akde %<>% dplyr::mutate(t_position.units = str_extract_all(t_position.units, "\\([^()]+\\)"),
                            t_velocity.units = str_extract_all(t_velocity.units, "\\([^()]+\\)")) 
  
  
  t.akde %<>% dplyr::mutate(area_mean = case_when(area.units == "area (square kilometers)" ~ area_mean,
                                                  area.units == "area (hectares)" ~ area_mean/100),
                            
                            diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean*100),
                            
                            t_position_mean = case_when(t_position.units == "(hours)" ~ t_position_mean,
                                                        t_position.units == "(days)" ~ t_position_mean*24),
                            
                            t_velocity_mean = case_when(t_velocity.units == "(minutes)" ~ t_velocity_low.1,
                                                        t_velocity.units == "(hours)" ~ t_velocity_low.1*60,
                                                        t_velocity.units == "(seconds)" ~ t_velocity_low.1/60),
                            speed_mean = speed_mean)
  
  t.akde %<>% dplyr::select(area_mean, speed_mean, diffusion_mean, t_position_mean, t_velocity_mean)
  
  
  c.akde %<>% dplyr::mutate(area_mean = case_when(area.units == "area (square kilometers)" ~ area_mean,
                                                  area.units == "area (hectares)" ~ area_mean/100),
                            
                            diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean*100),
                            
                            t_position_mean = case_when(t_position.units == "(hours)" ~ t_position_mean,
                                                        t_position.units == "(days)" ~ t_position_mean*24),
                            
                            t_velocity_mean = case_when(t_velocity.units == "(minutes)" ~ t_velocity_low.1,
                                                        t_velocity.units == "(hours)" ~ t_velocity_low.1*60,
                                                        t_velocity.units == "(seconds)" ~ t_velocity_low.1/60),
                            speed_mean = speed_mean)
  
  c.akde %<>% dplyr::select(area_mean, speed_mean, diffusion_mean, t_position_mean, t_velocity_mean)
  
  
  
  cormat <- round(cor(t.akde),2)
  
  # Reorder the correlation matrix
  upper_tri <- get_upper_tri(cormat)
  
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    labs(title = "Pearson Correlation - Staten Island population") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() 
  
  gg1 <- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4.5) +
    theme(
      plot.title = element_text(size = 14, face = "bold", colour = "black" ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
      axis.text.x = element_text(size = 12, face = "bold", colour = "black"),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.5, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  
  
  
  cormat <- round(cor(c.akde),2)
  
  # Reorder the correlation matrix
  upper_tri <- get_upper_tri(cormat)
  
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    labs(title = "Pearson Correlation - Rockefeller population") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() 
  
  gg2 <- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4.5) +
    theme(
      plot.title = element_text(size = 14, face = "bold", colour = "black" ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
      axis.text.x = element_text(size = 12, face = "bold", colour = "black"),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.5, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  
  
  gr1 <- grid.arrange(gg1, gg2, ncol=2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gr1,
           filename = paste0(export.folder, "/", "correlation_treatment_vs_control.jpg"),
           width = plot.width,
           height = plot.height,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
  }
  
  return(gr1)
  
  
}


linear_regression_plots <- function(t.akde = t.akde, c.akde = c.akde, export.folder = NA){
  
  t.akde %<>% replace(is.na(.), 0) 
  c.akde %<>% replace(is.na(.), 0)
  
  t.akde %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean)
  
  t.akde %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.akde %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean)
  
  c.akde %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  scc_plot_t <- ggscatter(t.akde, x = "speed_mean", y = "diffusion_mean",
                          color = "dodgerblue3", #alpha = "speed_mean",
                          add = "reg.line",
                          add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = t.akde, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.01, nudge_x = 0.2, size = 4, color = "dodgerblue3", fontface = "bold") + 
    scale_x_continuous(limits = c(min(c(t.akde$speed_mean, c.akde$speed_mean)), max(c(t.akde$speed_mean, c.akde$speed_mean))+0.5)) +
    scale_y_continuous(limits = c(min(c(t.akde$diffusion_mean, c.akde$diffusion_mean)), max(c(t.akde$diffusion_mean, c.akde$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = 'none')

  
  LRmodel_t <- lm(speed_mean ~ diffusion_mean, t.akde)
  
  # summ(LRmodel)
  # plot_summs(LRmodel, scale = TRUE, plot.distributions = TRUE)
  
  reg_plot_t <- autoplot(LRmodel_t, 
                         which = 1:6, 
                         colour = 'dodgerblue3',
                         smooth.colour = 'black', 
                         smooth.linetype = 'dashed',
                         ad.colour = 'blue',
                         label.size = 3, 
                         label.n = 5, 
                         label.colour = 'blue',
                         ncol = 2) + 
    labs(caption = "Linear regression - Staten Island population")
  
  scc_plot_c <- ggscatter(c.akde, x = "speed_mean", y = "diffusion_mean",
                          color = "red",
                          add = "reg.line",
                          add.params = list(color = "red", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = c.akde, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.01, nudge_x = 0.2, size = 4, color = "red", fontface = "bold") + 
    scale_x_continuous(limits = c(min(c(t.akde$speed_mean, c.akde$speed_mean)), max(c(t.akde$speed_mean, c.akde$speed_mean))+0.5)) +
    scale_y_continuous(limits = c(min(c(t.akde$diffusion_mean, c.akde$diffusion_mean)), max(c(t.akde$diffusion_mean, c.akde$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nRockefeller population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    theme(legend.position = 'none')

  
  LRmodel_c <- lm(speed_mean ~ diffusion_mean, c.akde)

  reg_plot_c <- autoplot(LRmodel_c, 
                         which = 1:6, 
                         colour = 'red',
                         smooth.colour = 'black', 
                         smooth.linetype = 'dashed',
                         ad.colour = 'red',
                         label.size = 3, 
                         label.n = 5, 
                         label.colour = 'red',
                         ncol = 2) + 
    labs(caption = "Linear regression - Rockefeller population")
  
  
  gg1 <- grid.arrange(scc_plot_t, scc_plot_c, ncol = 2)
  
  gg2 <- reg_plot_t + reg_plot_c
 
  gg2 <- grid.arrange(arrangeGrob(grobs = gg2@plots, ncol = 2))
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "scatter_plots_treatment_vs_control.jpg"),
           width = 35,
           height = 22,
           units = "cm",
           device = "jpeg",
           dpi = 700)
    
    ggsave(plot = gg2,
           filename = paste0(export.folder, "/", "regression_plots_treatment_vs_control.jpg"),
           width = 28,
           height = 55,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(list(scater_plot = gg1, reg_plot = gg2))
}



linear_regression_plots_per_period <- function(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, export.folder = NA, period = NA){
  
  t.akde.m %<>% replace(is.na(.), 0) 
  c.akde.m %<>% replace(is.na(.), 0)
  
  t.akde.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  t.akde.m %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.akde.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  c.akde.m %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  t.akde.m %<>% dplyr::mutate(Site = "treatment")
  c.akde.m %<>% dplyr::mutate(Site = "control")
  
  
  t.akde.f %<>% replace(is.na(.), 0) 
  c.akde.f %<>% replace(is.na(.), 0)
  
  t.akde.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  t.akde.f %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  c.akde.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean)
  
  c.akde.f %<>% dplyr::select(ind.id, speed_mean, diffusion_mean)
  
  t.akde.f %<>% dplyr::mutate(Site = "treatment")
  c.akde.f %<>% dplyr::mutate(Site = "control")
  
  # --------------------------------
  
  
  tc.akde.m <- rbind(t.akde.m, c.akde.m)
  tc.akde.f <- rbind(t.akde.f, c.akde.f)
  
  
  gg.m <- ggscatter(tc.akde.m, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = tc.akde.m, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.akde.m$speed_mean, c.akde.m$speed_mean)), max(c(t.akde.m$speed_mean, c.akde.m$speed_mean))+0.5)) +
    #scale_y_continuous(limits = c(min(c(t.akde.m$diffusion_mean, c.akde.m$diffusion_mean)), max(c(t.akde.m$diffusion_mean, c.akde.m$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [MALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  
  
  gg.f <- ggscatter(tc.akde.f, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_text(data = tc.akde.f, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.akde.f$speed_mean, c.akde.f$speed_mean)), max(c(t.akde.f$speed_mean, c.akde.f$speed_mean))+0.5)) +
    #scale_y_continuous(limits = c(min(c(t.akde.f$diffusion_mean, c.akde.f$diffusion_mean)), max(c(t.akde.f$diffusion_mean, c.akde.f$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [FEMALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  gg1 <- grid.arrange(gg.m, gg.f, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "lin_reg_treatment_vs_control_per_sex.jpg"),
           width = 40,
           height = 25,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(gg1)
  
}




linear_regression_plots_with_error_bars <- function(t.akde = t.akde, c.akde = c.akde, export.folder = NA){
  
  t.akde %<>% replace(is.na(.), 0) 
  c.akde %<>% replace(is.na(.), 0)
  
  t.akde %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            # diffusion_low = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_low/100,
                            #                            diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_low),
                            # diffusion_high = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_high/100,
                            #                            diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_high),
                            
                            speed_mean = speed_mean,
                            speed_low = speed_low,
                            speed_high = speed_high)
  
  t.akde %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.akde %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                       diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                            
                            speed_mean = speed_mean,
                            speed_low = speed_low,
                            speed_high = speed_high)
  
  c.akde %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  scc_plot_t <- ggscatter(t.akde, x = "speed_mean", y = "diffusion_mean",
                          color = "dodgerblue3", #alpha = "speed_mean",
                          add = "reg.line",
                          add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.1, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = t.akde, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.05, nudge_x = 0.18, size = 4, color = "dodgerblue3", fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.akde$speed_mean, c.akde$speed_mean)) - 2, max(c(t.akde$speed_mean, c.akde$speed_mean))+2)) +
    #scale_y_continuous(limits = c(min(c(t.akde$diffusion_mean, c.akde$diffusion_mean)) - 0.5, max(c(t.akde$diffusion_mean, c.akde$diffusion_mean))+0.5)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = 'none')
  
  
  scc_plot_c <- ggscatter(c.akde, x = "speed_mean", y = "diffusion_mean",
                          color = "red",
                          add = "reg.line",
                          add.params = list(color = "red", fill = "lightgray"),
                          conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.1, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = c.akde, aes(y = diffusion_mean, x = speed_mean, label = ind.id), nudge_y = 0.05, nudge_x = 0.18, size = 4, color = "red", fontface = "bold") + 
    # scale_x_continuous(limits = c(min(c(t.akde$speed_mean, c.akde$speed_mean)) - 2, max(c(t.akde$speed_mean, c.akde$speed_mean))+2)) +
    # scale_y_continuous(limits = c(min(c(t.akde$diffusion_mean, c.akde$diffusion_mean)) - 0.5, max(c(t.akde$diffusion_mean, c.akde$diffusion_mean))+0.5)) +
    labs(title = "Linear regression: speed ~ diffusion\nRockefeller population",
         caption = "Pearson method",
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    theme(legend.position = 'none')
  
  gg1 <- grid.arrange(scc_plot_t, scc_plot_c, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "scatter_plots_with_error_bars_treatment_vs_control_no_scale.jpg"),
           width = 85,
           height = 45,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(list(scater_plot = gg1))
}


linear_regression_plots_per_period_with_error_bars <- function(t.akde.m = t.akde.m, c.akde.m = c.akde.m, t.akde.f = t.akde.f, c.akde.f = c.akde.f, export.folder = NA, period = NA){
  
  t.akde.m %<>% replace(is.na(.), 0) 
  c.akde.m %<>% replace(is.na(.), 0)
  
  t.akde.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  t.akde.m %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.akde.m %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  c.akde.m %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  t.akde.m %<>% dplyr::mutate(Site = "treatment")
  c.akde.m %<>% dplyr::mutate(Site = "control")
  
  
  t.akde.f %<>% replace(is.na(.), 0) 
  c.akde.f %<>% replace(is.na(.), 0)
  
  t.akde.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  t.akde.f %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  c.akde.f %<>% dplyr::mutate(diffusion_mean = case_when(diffusion.units == "diffusion (hectares/day)" ~ diffusion_mean/100,
                                                         diffusion.units == "diffusion (square kilometers/day)" ~ diffusion_mean),
                              
                              speed_mean = speed_mean,
                              speed_low = speed_low,
                              speed_high = speed_high)
  
  c.akde.f %<>% dplyr::select(ind.id, speed_mean, speed_low, speed_high, diffusion_mean)
  
  t.akde.f %<>% dplyr::mutate(Site = "treatment")
  c.akde.f %<>% dplyr::mutate(Site = "control")
  
  # --------------------------------
  
  
  tc.akde.m <- rbind(t.akde.m, c.akde.m)
  tc.akde.f <- rbind(t.akde.f, c.akde.f)
  
  
  gg.m <- ggscatter(tc.akde.m, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.025, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = tc.akde.m, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.akde.m$speed_mean, c.akde.m$speed_mean)), max(c(t.akde.m$speed_mean, c.akde.m$speed_mean))+0.5)) +
    #scale_y_continuous(limits = c(min(c(t.akde.m$diffusion_mean, c.akde.m$diffusion_mean)), max(c(t.akde.m$diffusion_mean, c.akde.m$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [MALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  
  
  gg.f <- ggscatter(tc.akde.f, x = "speed_mean", y = "diffusion_mean",
                    color = "Site", #alpha = "speed_mean",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE)+
    stat_cor(method = "pearson") +
    geom_errorbar(aes(xmin = speed_low, xmax = speed_high), width = 0.025, colour = "black", alpha = 0.9, size = 1) +
    geom_text(data = tc.akde.f, aes(y = diffusion_mean, x = speed_mean, label = ind.id, color = Site), nudge_y = 0.01, nudge_x = 0.2, size = 4, fontface = "bold") + 
    #scale_x_continuous(limits = c(min(c(t.akde.f$speed_mean, c.akde.f$speed_mean)), max(c(t.akde.f$speed_mean, c.akde.f$speed_mean))+0.5)) +
    #scale_y_continuous(limits = c(min(c(t.akde.f$diffusion_mean, c.akde.f$diffusion_mean)), max(c(t.akde.f$diffusion_mean, c.akde.f$diffusion_mean))+0.2)) +
    labs(title = "Linear regression: speed ~ diffusion\nStaten Island and Rockefeller population [FEMALES]",
         caption = period,
         x = "Speed [kilometers/day]",
         y = "Diffusion [square kilometers/day]")+
    # scale_color_gradient(low = "blue", high = "red") +
    # scale_alpha(range = c(.05, .25)) +
    theme(legend.position = "bottom")
  
  gg1 <- grid.arrange(gg.m, gg.f, ncol = 2)
  
  if(!is.na(export.folder)){
    ggsave(plot = gg1,
           filename = paste0(export.folder, "/", "lin_reg_treatment_vs_control_with_error_bars_per_sex.jpg"),
           width = 85,
           height = 45,
           units = "cm",
           device = "jpeg",
           dpi = 700)
  }
  
  return(gg1)
  
}


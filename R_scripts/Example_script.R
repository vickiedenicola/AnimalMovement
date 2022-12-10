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


# Read the data
# ------------------------------------------------------------------------------

# Pre Breeding period:	15 Sep - 15 Oct

# Sufix "t" - data from treatment site - Staten Island
# Sufix "c" - data from control site - Rockefeller

data.subset.t <- readxl::read_xlsx(path = "Data/sample_data/data.female.preBreeding_treatmentSite.xlsx")
data.subset.c <- readxl::read_xlsx(path = "Data/sample_data/data.female.preBreeding_controlSite.xlsx")


# Calibration model
UERE <- readRDS("Data/sample_data/calibration.error.model.rds")


# Visualisation
# ------------------------------------------------------------------------------

plot.t <- plot_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`))
plot.c <- plot_individual(df.site = data.subset.c, ind.id = unique(data.subset.c$`individual-local-identifier`))

mapviewOptions(fgb = FALSE)
map.t <- map_individual(df.site = data.subset.t, ind.id = unique(data.subset.t$`individual-local-identifier`), burst = TRUE)
map.c <- map_individual(df.site = data.subset.c, ind.id = unique(data.subset.c$`individual-local-identifier`), burst = TRUE)

# As telemetry objects
# ------------------------------------------------------------------------------

data.tel.t <- as.telemetry(data.subset.t, datum = 'EPSG:4326')
data.tel.c <- as.telemetry(data.subset.c, datum = 'EPSG:4326')

UERE 
UERE$DOF[] <- UERE$UERE # it produce very small difference in model values and akde area value

uere(data.tel.t) <- UERE
uere(data.tel.c) <- UERE


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


# Get names of individuals
# ------------------------------------------------------------------------------

names.list_t <- list()
for(i in 1:length(data.tel.t)){
  names.list_t[[i]] <- data.tel.t[[i]]@info$identity
}

names.list_c <- list()
for(i in 1:length(data.tel.c)){
  names.list_c[[i]] <- data.tel.c[[i]]@info$identity
}


# Variograms
# ------------------------------------------------------------------------------

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

# Movement models
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
i = 3
# Calculate only for individual i = 3 (ID = 12 from treatment site) 
# We have problems with individuals like that one
# Speed can not be calculated

plot_individual(df.site = data.subset.t, ind.id = "12")
plot(data.tel.t[[i]])
plot(variogram_list.t[[i]])

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


fit_list.t <- list()
# fit_list.c <- list()


#for(i in 1:length(data.tel.t)){
  fit_list.t[[i]] <- tryCatch(
    {
      ctmm.fit(data.tel.t[[i]], guess_list.t[[i]])
    },
    error = function(e){
      NA
    }
  )
#  print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
#}

summary(fit_list.t[[i]])
  
# for(i in 1:length(data.tel.c)){
#   fit_list.c[[i]] <- tryCatch(
#     {
#       ctmm.fit(data.tel.c[[i]], guess_list.c[[i]])
#     },
#     error = function(e){
#       NA
#     }
#   )
#   
#   print(paste0("Model fitting DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
# }

# names(fit_list.t) <- names.list_t
# names(fit_list.c) <- names.list_c


# Estimate AKDE- Home Range Area
# ------------------------------------------------------------------------------

akde.list.t <- list()
# akde.list.c <- list()

# for(i in 1:length(data.tel.t)){
  akde.list.t[[i]] <- tryCatch(
    {
      ctmm::akde(data = data.tel.t[[i]], CTMM = fit_list.t[[i]])
    },
    error = function(e){
      NA
    }
  )
#  print(paste0("Calculating AKDE DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
# }

summary(akde.list.t[[i]])
  
# for(i in 1:length(data.tel.c)){
#   akde.list.c[[i]] <- tryCatch(
#     {
#       ctmm::akde(data = data.tel.c[[i]], CTMM = fit_list.c[[i]])
#     },
#     error = function(e){
#       NA
#     }
#   )
#   print(paste0("Calculating AKDE DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
# }

# names(akde.list.t) <- names.list_t
# names(akde.list.c) <- names.list_c


# Estimate speed
# ------------------------------------------------------------------------------

speed_list.t <- list()
# speed_list.c <- list()

# for(i in 1:length(data.tel.t)){
  speed_list.t[[i]] <- tryCatch(
    {
      ctmm::speed(object = data.tel.t[[i]], CTMM = fit_list.t[[i]], units = FALSE, cores = 0)
    },
    error = function(e){
      NA
    }
  )
#  print(paste0("Calculating SPEED DONE for: ", i , " / ", length(data.tel.t), " individuals from treatment site!"))
#}

speed_list.t[[i]]

ctmm::speed(object = data.tel.t[[i]], CTMM = fit_list.t[[i]], units = FALSE)

# Error in sqrt(diff(data$x)^2 + diff(data$y)^2)/DT/SPD : 
#   non-numeric argument to binary operator

ctmm::speed(object = data.tel.t[[i]], CTMM = fit_list.t[[i]], units = FALSE, cores = 0, robust = TRUE)

# for(i in 1:length(data.tel.c)){
#   speed_list.c[[i]] <- tryCatch(
#     {
#       ctmm::speed(object = data.tel.c[[i]], CTMM = fit_list.c[[i]], units = FALSE, cores = 0)
#     },
#     error = function(e){
#       NA
#     }
#   )
#   print(paste0("Calculating SPEED DONE for: ", i , " / ", length(data.tel.c), " individuals from control site!"))
# }

# names(speed_list.t) <- names.list_t
# names(speed_list.c) <- names.list_c



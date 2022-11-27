
# https://cran.r-project.org/web/packages/ctmm/vignettes/

# ctmm learning materials
# https://github.com/ctmm-initiative/ctmmlearn

# Speed, distance, diffusion
# https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_speed.R


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


data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered.csv", stringsAsFactors = FALSE, header = TRUE) %>% as.data.frame()

range(data.raw$timestamp)
dim(data.raw)

# Subset data
# ------------------------------------------------------------------------------

data.subset <- subset_func(dataset = data.raw,
            time_period = "Breeding",
            male_female = "M",
            study_area = "Rockefeller",
            )# animal_color = "W"

unique(data.subset$`individual-local-identifier`)
unique(data.subset$`tag-local-identifier`)

plot_individual(df.site = data.subset, ind.id = unique(data.subset$`individual-local-identifier`))
mapviewOptions(fgb = FALSE)

map_individual(df.site = data.subset, ind.id = unique(data.subset$`individual-local-identifier`), burst = FALSE)
map_individual(df.site = data.subset, ind.id = c("63"), burst = FALSE)

# As telemetry object
# ------------------------------------------------------------------------------

data.tel <- as.telemetry(data.subset, datum = 'EPSG:4326')
names(data.tel)

# Calibrate tracking data and remove outliers
# ------------------------------------------------------------------------------
UERE <- readRDS("Data/undep/calibration.error.model.rds")
summary(UERE)

UERE$DOF[] <- UERE$UERE # need to be checked

uere(data.tel) <- UERE
summary(uere(data.tel)) # this should be the same as summary(UERE)

# Outlier detection
# ------------------------------------------------------------------------------
outliers.tel <- outlie(data.tel, plot = FALSE)


# Remove outliers
# ------------------------------------------------------------------------------

for(i in 1:length(outliers.tel)){
  print(paste0("Max speed value: ", max(outliers.tel[[i]]$speed) * 100, " cm/s"))
  x <- which(outliers.tel[[i]]$speed * 100 >= 16.1)
  print(paste0("Rows to remove: ", length(x)))
  if(length(x) > 0){
    data.tel[[i]] <- data.tel[[i]][-x, ]
  }
  print(paste0("Processed: ", i, "/", length(outliers.tel), " individuals"))
  print("------------------------------------------------------------------")
}


summary(data.tel)

# Calculate convex hull
# ------------------------------------------------------------------------------
deer <- as.sf(data.tel[[7]], 
              error = FALSE)

deer %<>% st_transform(4326)
mapview(deer)

qtm(deer)

ch <- st_convex_hull(st_union(deer)) 
qtm(ch)

mapview(deer) + mapview(ch, col.regions = "red")

area.m2 <- ch %>% st_area(.) # 67.1 ha
as.numeric(area.m2)/1000000 # 0.67 km2

# in CRS in projection
deer_proj <- deer %<>% st_transform(3857)
ch_proj <- st_convex_hull(st_union(deer_proj)) 

mapview(deer_proj) + mapview(ch_proj, col.regions = "red")

area.m2 <- ch_proj %>% st_area(.) # 116.6 ha
as.numeric(area.m2)/1000000 # 1.165759 km2

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

first_deer <- data.tel[[7]]
plot(first_deer)

# Variograms
# ------------------------------------------------------------------------------

# Calculate an empirical variogram from movement data
SVF <- variogram(first_deer, error = TRUE) # error - Adjust for the effect of calibrated errors.

level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window

plot(SVF,xlim=xlim,level=level)
title("zoomed in")

plot(SVF,fraction=0.65,level=level)
title("zoomed out")


# Variogram fitting
# ------------------------------------------------------------------------------

GUESS_manual <- variogram.fit(SVF)
# there is a option to manualy fit the variogram


# Without error model included
# ------------------------------------------------------------------------------

# ctmm.select considers the initial guess (hypothesis) and then iterates 
# this model to select the best model based upon an information criteria.

M.IID <- ctmm.fit(first_deer) # no autocorrelation timescales
GUESS <- ctmm.guess(first_deer, interactive = FALSE, variogram = SVF, ctmm(isotropic = TRUE)) # automated model guess CTMM = ctmm(error = TRUE)
M.OUF <- ctmm.fit(first_deer, GUESS) # the animal's covariance is circular isotropic = TRUE

M.OUF <- ctmm.fit(first_deer, ctmm(isotropic = FALSE)) # the animal's covariance is elliptical isotropic = FALSE

plot(first_deer, M.OUF)

FIT <- ctmm.select(first_deer, GUESS, trace = 3, verbose = TRUE, cores = 15)

summary(GUESS)
summary(M.OUF)
summary(M.OUF)$CI %>% as.data.frame()
summary(FIT)
summary(FIT$`OUF anisotropic`)
summary(FIT$OUF)

plot(first_deer, FIT$`OUF isotropic`)
plot(first_deer, FIT$`OUF anisotropic`)
plot(first_deer, FIT$OUF)


plot(SVF, CTMM = M.OUF, 
     col.CTMM = "red", 
     fraction = 0.65, 
     level = 0.5)
title("zoomed out")

plot(SVF, CTMM = FIT, 
     col.CTMM = palette.colors(length(FIT)), 
     fraction = 0.65, 
     level = 0.5)
title("zoomed out")

plot(SVF, CTMM = FIT$OUF, 
     col.CTMM = "green", 
     fraction = 0.65, 
     level = 0.5)
title("zoomed out")


# With error model included
# ------------------------------------------------------------------------------

GUESS.e <- ctmm.guess(first_deer, CTMM = ctmm(error = TRUE), interactive = FALSE) # automated model guess CTMM = ctmm(error = TRUE)
GUESS.e <- ctmm.guess(first_deer, CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE) 
M.OUF.e <- ctmm.fit(first_deer, GUESS.e) # in general, use ctmm.select instead
FIT.e <- ctmm.select(first_deer, GUESS.e, trace = 3, verbose = TRUE, cores = 15)

# with base uere dof error
GUESS.e1 <- ctmm.guess(first_deer, CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE) # automated model guess CTMM = ctmm(error = TRUE)
M.OUF.e1 <- ctmm.fit(first_deer, GUESS.e1) # in general, use ctmm.select instead
FIT.e1 <- ctmm.select(first_deer, GUESS.e1, trace = 3, verbose = TRUE)


summary(GUESS.e)
summary(M.OUF.e)
summary(FIT.e)
summary(FIT.e$`OUF error`)
summary(FIT.e$`OUF anisotropic error`)

summary(GUESS.e1)
summary(M.OUF.e1)
summary(FIT.e1)
summary(FIT.e1$`OUF anisotropic error`)


speed(first_deer, M.OUF) # speed (kilometers/day) 3.383218 3.640395 3.902139
speed(first_deer, M.OUF.e) # speed (kilometers/day) 2.856155 3.110778 3.37066


# fit

AKDE <- akde(first_deer, M.OUF) 
summary(AKDE)
AKDE.e <- akde(first_deer, M.OUF.e) 
summary(AKDE.e)

# AKDE.e1 <- akde(first_deer, M.OUF.e1) 
# summary(AKDE.e1)

EXT <- extent(AKDE, level = 0.95)
plot(first_deer, UD = AKDE, xlim = EXT$x, ylim = EXT$y)
title(expression("OUF AKDE"["C"]))

EXT <- extent(AKDE.e, level = 0.95)
plot(first_deer, UD = AKDE.e, xlim = EXT$x, ylim = EXT$y)
title(expression("OUF error AKDE"["C"]))


# select

AKDE_f <- akde(first_deer, FIT) 
summary(AKDE_f)
AKDE_f.e <- akde(first_deer, FIT.e) 
summary(AKDE_f.e)

AKDE_f.e1 <- akde(first_deer, FIT.e1) 
summary(AKDE_f.e1)

summary(data.tel)
info <- summary(data.tel)
knitr::kable(info)

plot(SVF, CTMM = FIT, 
     col.CTMM = palette.colors(length(FIT)), 
     fraction = 0.65, 
     level = 0.5)
title("zoomed out")

plot(SVF, CTMM = FIT.e,
     col.CTMM = palette.colors(length(FIT.e)), 
     fraction = 0.65,
     level = 0.5)
title("zoomed out")

plot(SVF, CTMM = FIT.e1,
     col.CTMM = palette.colors(length(FIT.e1)), 
     fraction = 0.65,
     level = 0.5)
title("zoomed out")

plot(SVF, CTMM = M.OUF.e,
     col.CTMM = "red", 
     fraction = 0.65,
     level = 0.5)
title("zoomed out")



# https://ctmm-initiative.github.io/ctmmwebdoc/articles/package_usage.html


tel <- first_deer

tel$day <- cut(tel$timestamp, breaks = "day")
days <- unique(tel$day)
length(days)

res <- list()
j = 3
for(j in 1:length(days)){
  
  message("Estimating distance travelled on day ", j, ": ", days[j])
  
  DATA.SUBSET <- tel[which(tel$day == days[j]), ]
  
  SAMP.TIME <- diff(c(DATA.SUBSET$t[1],
                      DATA.SUBSET$t[nrow(DATA.SUBSET)]))
  
  GUESS <- ctmm.guess(tel, CTMM=ctmm(error=TRUE, isotropic = TRUE), interactive = FALSE)
  
  FITS <- ctmm.select(DATA.SUBSET, CTMM = GUESS, trace = TRUE, cores = 15)
  
  ctmm_speed <- ctmm::speed(object = DATA.SUBSET,
                            CTMM = FITS,
                            units = FALSE)
  
  ctmm_dist <- ctmm_speed$CI * SAMP.TIME
  
  rownames(ctmm_dist) <- "distance (meters)"
  
  x <- c(j, 
         ctmm_dist[2],
         ctmm_dist[1],
         ctmm_dist[3])
  
  names(x) <- c("date", "dist.ML", "dist.Min", "dist.Max")
  
  res[[j]] <- x
  
}

res <- as.data.frame(do.call(rbind, res))
res$date <- as.Date(days)
head(res)


# area (akde) speed (speed) sum distance (speed * number of days)


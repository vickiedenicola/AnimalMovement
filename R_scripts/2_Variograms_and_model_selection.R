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



data.SI.tel <- readRDS("Data/processed/telemetry.SI.cleaned.rds")
data.RF.tel <- readRDS("Data/processed/telemetry.RF.cleaned.rds")

first_deer <- data.SI.tel[[1]]


# Variograms
# --------------------------------------------------------

SVF <- variogram(first_deer)
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(SVF,xlim=xlim,level=level)
title("zoomed in")
plot(SVF,fraction=0.65,level=level)
title("zoomed out")




# Variogram fitting
# --------------------------------------------------------

# next step

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

projection(data.SI.tel) <- median(data.SI.tel) # north-up projection

first_deer <- data.SI.tel[[1]]

# load model fits from ctmm.select
FIT <- readRDS("Data/processed/model_first.deer.rds")
summary(FIT)

# --------------------------------------------------------
# units operator
?`%#%`

1 %#% 'day' # day in seconds
1 %#% 'year' # year in seconds


# for time,  will consider the first month of data
first_deer <- first_deer[first_deer$t <= first_deer$t[1] + 1%#%'week',]
# fit to first month only
FIT_f <- ctmm.select(first_deer, FIT, trace=3)


# the speed estimate here is RMS Gaussian
summary(FIT_f)

# Gaussian (regular speed - not RMS)
sp.1 <- speed(FIT_f)

# non-parametric speed estimation
# "2019 Noonan Fleming Akre ... Calabrese.pdf" in Readings/Continuous_Time folder
sp.2 <- speed(first_deer, FIT_f)

# --------------------------------------------------------

# Impact of coarsening the data
SUB <- first_deer
FIT.SUB <- FIT_f

# remove every other time
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
FIT.SUB <- ctmm.select(SUB, FIT.SUB, trace=3)

# the speed estimate here is RMS Gaussian
summary(FIT.SUB)

# Gaussian (regular speed - not RMS)
sp.sub.1 <- speed(FIT.SUB)
# non-parametric speed estimation
sp.sub.2 <- speed(SUB, FIT.SUB)

# keep in mind the stationary assumption of the model


# For all population
# --------------------------------------------------------

# TODO

help('meta')

#Load in the fitted movement models
load("data/buffalo.rda")

#Estimate mean spead for each animal
SPEEDS <- list()
for(i in 1:length(buffalo)){
  SPEEDS[[i]] <- speed(buffalo[[i]],FITS[[i]])
}
names(SPEEDS) <- names(buffalo)
# save(SPEEDS,file="data/buffalo_speeds.rda")
load("data/buffalo_speeds.rda")

meta(SPEEDS)

# Instantaneous speeds
# --------------------------------------------------------

INST_SPEEDS <- speeds(data.SI.tel[[1]], FIT)
summary(INST_SPEEDS)
head(INST_SPEEDS)




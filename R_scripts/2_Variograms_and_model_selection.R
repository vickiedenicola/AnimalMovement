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


data.SI.tel <- readRDS("Data/processed/telemetry.SI.cleaned.rds")
data.RF.tel <- readRDS("Data/processed/telemetry.RF.cleaned.rds")

first_deer <- data.SI.tel[[1]]


# Variograms
# --------------------------------------------------------

# Calculate an empirical variogram from movement data
SVF <- variogram(first_deer, error = TRUE) # error - Adjust for the effect of calibrated errors.

level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window

plot(SVF,xlim=xlim,level=level)
title("zoomed in")

plot(SVF,fraction=0.65,level=level)
title("zoomed out")

# Population variogram
# --------------------------------------------------------

# Staten Island 
SVF_all.si <- lapply(data.SI.tel, variogram)
SVF_mean.si <- mean(SVF_all.si)
plot(SVF_mean.si, fraction = 0.35, level = level)
title("Population variogram")

# Staten Island 
SVF_all.rf <- lapply(data.RF.tel, variogram)
SVF_mean.rf <- mean(SVF_all.rf)
plot(SVF_mean.rf, fraction = 0.35, level = level)
title("Population variogram") 
# You should be careful though, if the individual movement behaviors and sampling schedules are not identical, then there will be discontinuities at lags where one timeseries runs out of data.

# Variogram fitting
# --------------------------------------------------------
# https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html

GUESS_manual <- variogram.fit(SVF)


# ctmm.select considers the initial guess (hypothesis) and then iterates 
# this model to select the best model based upon an information criteria.

M.IID <- ctmm.fit(first_deer) # no autocorrelation timescales
GUESS <- ctmm.guess(first_deer, interactive=FALSE) # automated model guess CTMM = ctmm(error = TRUE)
M.OUF <- ctmm.fit(first_deer, GUESS) # in general, use ctmm.select instead
FIT <- ctmm.select(first_deer, GUESS, trace = 3, verbose=TRUE)

summary(M.OUF)
summary(FIT)

# With error model included

# M.IID.e <- ctmm.fit(first_deer) # no autocorrelation timescales
GUESS.e <- ctmm.guess(first_deer, CTMM = ctmm(error = TRUE), interactive=FALSE) # automated model guess CTMM = ctmm(error = TRUE)
M.OUF.e <- ctmm.fit(first_deer, GUESS.e) # in general, use ctmm.select instead
FIT.d1.e <- ctmm.select(first_deer, GUESS.e, trace = 3, verbose=TRUE)
FIT.d1.e1 <- ctmm.select(first_deer, GUESS.e, trace = TRUE)

summary(M.OUF.e)
summary(FIT.d1.e)
summary(FIT.d1.e1)

AKDE.e <- akde(first_deer, FIT.d1.e) # AKDE
summary(AKDE.e)
summary(AKDE)
# saveRDS(M.OUF, "Data/processed/model_first.deer.rds")


plot(SVF,CTMM=M.OUF,col.CTMM=c("red"),fraction=0.65,level=0.5)
title("zoomed out")
plot(SVF,CTMM=M.OUF,col.CTMM=c("red"),xlim=xlim,level=0.5)
title("zoomed in")

plot(SVF,CTMM=FIT,col.CTMM=c("red","purple","blue"),fraction=0.65,level=0.5)
title("zoomed out")
plot(SVF,CTMM=FIT,col.CTMM=c("red","purple","blue"),xlim=xlim,level=0.5)
title("zoomed in")

plot(SVF,CTMM=M.OUF.e,col.CTMM=c("red"),fraction=0.65,level=0.5)
title("zoomed out")
plot(SVF,CTMM=M.OUF.e,col.CTMM=c("red"),xlim=xlim,level=0.5)
title("zoomed in")

plot(SVF,CTMM=FIT.d1.e,col.CTMM=c("red","purple","blue"),fraction=0.65,level=0.5)
title("zoomed out")
plot(SVF,CTMM=FIT.d1.e,col.CTMM=c("red","purple","blue"),xlim=xlim,level=0.5)
title("zoomed in")

plot(SVF,CTMM=FIT.d1.e1,fraction=0.65,level=0.5)
title("zoomed out")
plot(SVF,CTMM=FIT.d1.e1,col.CTMM=c("red","purple","blue"),xlim=xlim,level=0.5)
title("zoomed in")


FIT <- list()
for(i in 1:length(data.SI.tel)){
  print(i)
  GUESS <- ctmm.guess(data.SI.tel[[i]], interactive=FALSE)
  FIT[[i]] <- ctmm.select(data.SI.tel[[i]],GUESS,trace=3,verbose=TRUE)
}
names(FIT) <- names(data.SI.tel)
FIT[[1]] 

# TODO calculate area of convex hull around points of one deer
# --------------------------------------------------------





# Autocorrelated Kernel Density Estimation
# --------------------------------------------------------
# https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html


KDE <- akde(first_deer, M.IID) # KDE
AKDE <- akde(first_deer, M.OUF) # AKDE
wAKDE <- akde(first_deer, M.OUF, weights=TRUE) # weighted AKDE

summary(KDE)
summary(AKDE)
summary(wAKDE)

# calculate one extent for all UDs
EXT <- extent(list(KDE, AKDE, wAKDE),level=0.95)

plot(first_deer, UD = KDE, xlim=EXT$x, ylim=EXT$y)
title(expression("IID KDE"["C"]))
plot(first_deer, UD = AKDE, xlim=EXT$x, ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(first_deer, UD = wAKDE, xlim=EXT$x, ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))


# Example
# --------------------------------------------------------

library(ctmm)
data(buffalo)
Pepper <- buffalo$Pepper
M.IID <- ctmm.fit(Pepper) # no autocorrelation timescales
GUESS <- ctmm.guess(Pepper,interactive=FALSE) # automated model guess
M.OUF <- ctmm.fit(Pepper,GUESS) # in general, use ctmm.select instead

KDE <- akde(Pepper,M.IID) # KDE
AKDE <- akde(Pepper,M.OUF) # AKDE
wAKDE <- akde(Pepper,M.OUF,weights=TRUE) # weighted AKDE

summary(KDE)
summary(AKDE)
summary(wAKDE)

# calculate one extent for all UDs
EXT <- extent(list(KDE,AKDE,wAKDE),level=0.95)

plot(Pepper,UD=KDE,xlim=EXT$x,ylim=EXT$y)
title(expression("IID KDE"["C"]))
plot(Pepper,UD=AKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(Pepper,UD=wAKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))

# Author: Petar Bursac
# Date: 5 January 2023
# This script is used to map individuals filtered by period, site and sex on base maps.

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
library(writexl)
library(gridExtra)

# Movement packages

library(move)
library(ctmm)
library(ctmmweb)

# Read the available functions
source("R_scripts/functions.R")

# Set mapview options
mapviewOptions(fgb = FALSE)

# Read xlsx file with new filtered data 
data.raw <- data.table::fread("Data/processed/dataset.Odocoileus.virginianus.filtered_NEW.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  as.data.frame() # make a dataframe instead of tibble

# Read additional table with available informations and constraints per individual
check_table <- readxl::read_xlsx("Analysis/stats_ndays_per_period_per_site_per_sex_NEW.xlsx") %>%
  as.data.frame()

# Take a look at data and check_table
data.raw
check_table

# Define period of interest (replace the value with the required ones)
period <- "Breeding" # "Breeding" or "Post Breeding" or "Baseline"

# Define sex of interest (replace the value with the required ones)
male_female <- "M" # "M" for males or "F" for females

# Define stydy area - site of interest (replace the value with the required ones)
study_area <- "Staten Island" # "Staten Island" or "Rockefeller"

# Subset data with defined criterias
data.subset <- subset_func_new(dataset = data.raw,
                               check_table = check_table, 
                               time_period = period, 
                               male_female = male_female, 
                               study_area = study_area)


# Take a look at result of subbseting
data.subset

# Take a look at available individuals - their ID for this period
unique(data.subset$`individual-local-identifier`)

# Map individual on base map
map_individual(df.site = data.subset, 
               ind.id = c("133")) # Here you only need to specify ID of individual


# At top left corner - there is a option to change base map
# Esri.WorldImagery - is a sattelite image base map


# Also you can Map more than one individual on base map
map_individual(df.site = data.subset, 
               ind.id = c("133", "148", "219")) # Here you just need to specify IDs of individuals
# it needs to be specified in vector c(), delimited with , and under double " "


# And also you can Map all available individuals on base map from specified period 
map_individual(df.site = data.subset, 
               ind.id = unique(data.subset$`individual-local-identifier`)) # Here you just need to specify all unique IDs of individuals



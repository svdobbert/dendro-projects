# renv::init #initialize renv
# renv::activate() #activate renv
# synchronise packages from lock file if needed
# renv::restore() 

# system("R")

options(scipen = 999)  # turns of scientific notations

## set working directory
dir <- "~/Documents/Data/Betula_BRT/output/" # set directory for output
setwd(dir)

# load all dependencies
source("../dependencies.R")

## Input
# load data
load("../input-data/input") #data
growth <- data
load("../input-data/Betula_all.gz") #merge
data_sites <- merge
load("../input-data/ERA5.gz") #era5
data_era5 <- era5
load("../input-data/climate_Tafjord_Fokstugu.gz")
data_region <- data
load("../input-data/climate_EW.gz") #data_EW


## run data preperation scipts
# get constants
source("../preperation/constants.R")
# detrending
source("../preperation/detrending.R")
# create input
source("../preperation/create-data.R")

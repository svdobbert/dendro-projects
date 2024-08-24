# renv::init #initialize renv
# renv::activate() #activate renv
# synchronise packages from lock file if needed
# renv::restore()

# system("R")

options(scipen = 999) # turns of scientific notations

## set working directory
dir <- "~/workspaces/dendro/norway-dendro/detrending/output" # set directory for output
setwd(dir)

# load all dependencies
source("../dependencies.R")

## Input
# load data
# Betula nana
load("../input-data/inputBet.gz") # file: inputBet
#inputBet$value <- scale(inputBet$value)
# Cytisus galianoi
load("../input-data/inputCyt.gz") # file: inputCyt
#inputCyt$value <- scale(inputCyt$value)
# Salix herbacea
load("../input-data/inputSal.gz") # file: inputSal
#inputSal$value <- scale(inputSal$value)
# Rhododendron ferrugineum
load("../input-data/inputRho.gz") # file: inputRho
#inputRho$value <- scale(as.numeric(inputRho$value))
inputRho$value <- as.numeric(inputRho$value)*1000 #convert to micrometers
mean(inputRho$value, na.rm = TRUE)
## Combine Input
input <- as.data.frame(rbind(
    inputBet,
    inputCyt,
    inputSal,
    inputRho
))

## run data preperation scripts
# get sample size
source("../get-sample-size.R")
# get constants
source("../constants.R")

## run main scripts
# calculate age
source("../calculate-age.R")
# calculate trends
source("../calculate-trends.R")

##
# plot age trends
source("../plot-age-trend.R")
# plot detrending
source("../detrending-plots.R")

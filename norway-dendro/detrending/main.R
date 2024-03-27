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
# Cytisus galianoi
load("../input-data/inputCyt.gz") # file: inputCyt
# Betula nana
load("../input-data/inputSal.gz") # file: inputSal
# Rhododendron ferrugineum
load("../input-data/inputRho.gz") # file: inputRho

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
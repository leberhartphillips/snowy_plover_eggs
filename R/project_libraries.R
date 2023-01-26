## install dependent packages

# a vector of all the packages needed in the project's scripts
packages_required_in_project <- 
  c("arm", "aspace","BaSTA","bayestestR","broom.mixed", "coefplot2", "cowplot",
    "data.table", "doParallel", "effects", "extrafont", "flexmix", "fGarch", 
    "ggmap", "ggpubr", "ggridges", "ggthemes", "grid", "gt", "kableExtra", 
    "leaflet", "lme4", "magick", "mapview", "RMark", "merDeriv", "mgcv", "moveVis", 
    "multcomp", "multipanelfigure", "parallel", "parameters", "partR2", 
    "patchwork", "RColorBrewer", "rptR", "RSQLite", "scales", "snowfall", "sp", 
    "standardize", "tidybayes", "tidyverse", "rmdformats", "webshot2")

# of the required packages, check if some need to be installed
new.packages <- 
  packages_required_in_project[!(packages_required_in_project %in% 
                                   installed.packages()[,"Package"])]

# install all packages that are not locally available
if(length(new.packages)) install.packages(new.packages)

# load all the packages into the current R session
lapply(packages_required_in_project, require, character.only = TRUE)

# connect to CeutaCLOSED
CeutaCLOSED <- 
  dbConnect(SQLite(), 
            dbname = "/Users/luketheduke2/Documents/Academic/Mexico/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v2-0-0.sqlite")
#project dNBR to NAD83

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")

#1. Resample rasters -----------------------------------------------------------
# Read in the rasters
variable_list <- list.files("./Inputs/Rasters/dNBR/",
                            pattern =  paste(paste0(FiresOfInterest,".tif"), 
                                             sep = "", collapse = "|"), # only import the fires of interest
                            recursive = TRUE,
                            full.names = TRUE)

variables <- sapply(variable_list, raster)

# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]

names(variables) <- variable.name


for(i in 1:length(FiresOfInterest)){
  baseFireRast <- raster(paste0(SpatialFilesPath, "/Inputs/Rasters/BaseRasters/BaseRaster_",
                                FiresOfInterest[i],".tif"))
  
  a <- raster::resample(variables[[i]], baseFireRast, method = "bilinear")
  Fire <- st_as_sf(StudyFirePerims %>% dplyr::filter(FIRE_NUMBE == FiresOfInterest[i]))
  b <- mask(a,Fire)*1000
  writeRaster(b,paste0("./Inputs/Rasters/dNBR/",FiresOfInterest[i],
                       "dNBR_ReSamp.tif"),overwrite=TRUE)
}


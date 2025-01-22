# Make base rasters
# Ingrid Farnell & A. Clason
# Jan 18, 2021

# This script makes the base raster for each fire that has the extent and resolution (30m x 30m) 
# to be used to stack predictor and response rasters. 

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#---------------- Load data --------------------#
# DOB - use for extent 
DOB_list <- list.files("./Inputs/Rasters/DOB/DOBrounded/",
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(DOB_list), "covariates in the list"))
DOB_list <- DOB_list[!grepl("xml", DOB_list)]

# dNBR - use for resolution
dNBR_list <- list.files("./Inputs/Rasters/dNBR/",
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(dNBR_list), "covariates in the list"))
dNBR_list <- dNBR_list[!grepl("xml", dNBR_list)]

file.name <- c("G41607", "G51632", "R11498", "R11796", "R11921", "R21721")

#------------------ Create base rasters -----------------#
for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[i])
  dNBR <- raster(dNBR_list[i])
  # Clip extent to DOB rasters
  base_ras <- crop(dNBR, DOB)
  # Make raster blank
  base_ras[base_ras] <- NA
  # Write rasters
  writeRaster(base_ras, paste0("./Inputs/BaseRasters/",
                               "BaseRaster_", file.name[i]), format = "GTiff", 
              overwrite = TRUE)
}

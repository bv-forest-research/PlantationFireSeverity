# Make historic fires rasters
# Ingrid Farnell
# Jan 19, 2021

# This script makes a raster of time since most recent fire for each study fire.


#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#------------ Load data--------------------#
# Historic fires - all BC
hist_fires <- read_sf("./Inputs/Vectors/Historic fire/BC Wildfire Historical Fire Perimeters.shp")

# Study fire perimeters
study_fires <- read_sf( "./Inputs/Vectors/Study_fire_perimeters.shp")

# Base rasters
base_list <- list.files(paste0(SpatialFilesPath,"./Inputs/BaseRasters/"),
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)
print(paste("there are", length(base_list), "covariates in the list"))
base_list <- base_list[!grepl("xml", base_list)]

file.name <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")


#---------------- Create historic fires rasters---------------#
for (i in 1:length(base_list)){
  base <- raster(base_list[i])
  # Only keep fires between 1960 and 2017
  hist_fires <- hist_fires %>% filter(FIRE_YEAR >= 1960 & FIRE_YEAR < 2017)
  # Rasterize - keeping youngest fire i.e. max fire year
  youngest_fire_ras <- rasterize(hist_fires, base, field = "FIRE_YEAR", fun = max)
  # Get years since most recent fire ( - 2018)
  YrSincFire_ras <- calc(youngest_fire_ras, function(x) {2018 - x})
  # Separate study fires into individual fires
  unique_fires <- study_fires[study_fires$FIRE_NUMBE == file.name[i], ]
  # Mask historic fires with individual study fire
  hist_ras_mask <- mask(YrSincFire_ras, mask = unique_fires)
  # Make NAs 0
  hist_ras_mask[is.na(hist_ras_mask[])] <- 0
  # Make NA outside the fire poly mask
  hist_ras_mask <- mask(hist_ras_mask, mask = unique_fires)
  # Write rasters
  writeRaster(hist_ras_mask, paste0("./Inputs/HistoricFires/", "HistoricFires_", file.name[i]),
              format = "GTiff", overwrite = TRUE)
}


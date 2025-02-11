# DOB data prep
# Ingrid Farnell 
# Jan 21, 2022

library(raster)

# This script copies the dob tiffs created by Sean Parkes code (04. DOB_interpolation.R), 
#renames into the main folder in order to be re-projected below.

FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")
for(i in 1:length(FiresOfInterest)){
  r <- raster(paste0("./Inputs/Rasters/DOB/",FiresOfInterest[i],"/dob.tif"))
  writeRaster(r,paste0("./Inputs/Rasters/DOB/dob_",FiresOfInterest[i],"n83.tif"))
}


# This script reprojects the DOB rasters from NAD83 conus Albers to NAD83 BC Albers

#---------------- Load data --------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# DOB rasters
DOB_list <- list.files(paste0(SpatialFilesPath,"./Inputs/DOB/"),
                       pattern = "n83", 
                       recursive = FALSE, 
                       full.names=TRUE)


#-----------------Reproject-----------------#
for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[i])
  # Reproject rasters to NAD83 BC Albers
  DOB_rpjct <- projectRaster(DOB, crs="+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs",
                             method="ngb")
  # Write rasters
  writeRaster(DOB_rpjct, paste0("./Inputs/Rasters/DOB/",
                                "dob_", FiresOfInterest[i]), format = "GTiff", overwrite = TRUE)
}

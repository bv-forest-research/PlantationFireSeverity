# Classify dNBR based on calibrated Key and Benson thresholds
# Ingrid Farnell & A. Clason


#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.,

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

# Set the fires of interest - all 2018 fires with openings
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")
# all values > -Inf and <= 120 become class 1; > 120  <= 270 is class 2, >279 <= 269
#these are the calibrated classes from CBI analyses:
m <- c(-Inf, 120, 1,  120, 270, 2,  270, 635, 3, 635,Inf,4)
#m <- c(-Inf, 99, 1, 99, 269, 2, 269, 659, 3, 659, Inf, 4)
rclmat <- matrix(m, ncol=3, byrow=TRUE)


# Classify (burn severity categories) --------------------------------------
# Read in the rasters
variable_list <- list.files("./Inputs/Rasters/dNBR/",
                            pattern =  paste(paste0(FiresOfInterest,"dNBR_ReSamp")
                                             , sep = "", 
                                             collapse = "|"), # only import the fires of interest
                            recursive = TRUE,
                            full.names = TRUE)

variables <- sapply(variable_list, raster)

# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]

names(variables) <- variable.name

for(i in 1:length(FiresOfInterest)){
  dnbr_cat <- raster::reclassify(variables[[i]],rclmat)
  writeRaster(dnbr_cat,paste0("./Inputs/Rasters/dNBR/",
                              FiresOfInterest[i],
                       "dNBR_cla.tif"), overwrite=TRUE)
  
}


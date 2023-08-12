# A. Clason & Ingrid Farnell
# March, 2022

# This generates Table 1 and Supplementary Table 2

#--libraries
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.,
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-----------------1. Load data from RF analysis ----------------------------------------#
datPath <- "./Inputs/" 
SpatialFilesPath <- "./Inputs/" 

csvs_names <- list.files(path = datPath, pattern = "dat270.csv", full.names = TRUE)
dat_csvs <- purrr::map(csvs_names, fread)
fires_names <- str_split(string = list.files(path = datPath, pattern = "dat270.csv"),
                         pattern="dat270.csv", simplify = TRUE)[,1]
dat_list <- Map(cbind, dat_csvs, FireID = fires_names)
dat_dt <- rbindlist(dat_list, fill=TRUE)
dat_dt[is.na(DebrisPiled), DebrisPiled:=0]
dat_dt[is.na(Prune), Prune:=0]
dat_dt[is.na(SpotBurn), SpotBurn:=0]
dat_dt[is.na(DebrisMade), DebrisMade:=0]

studyFires <- fread("./Inputs/StudyFireList.csv")
dat_dt <- merge(dat_dt, studyFires[,.(FireID,FireName)], by="FireID")
setkey(dat_dt,FireName)
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")

#-----------------------------2. Fires ----------------------------------------#
# Study fire perimeters
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
study_fires <- read_sf(paste0(SpatialFilesPath, "Vectors/StudyFires.shp"))

plant_list <- list.files(paste0(SpatialFilesPath,"Vectors/"),
                         pattern = "Plantations.shp", 
                         recursive = FALSE, 
                         full.names=TRUE)
# dNBR rasters
dNBR_list <- list.files(paste0(SpatialFilesPath,"Rasters/dNBR"),
                       pattern = "CAT.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)


# Plantation pred rasters
variable_list <- list.files(paste0(SpatialFilesPath, "Rasters/PlantationPreds/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"),
                            recursive = TRUE,
                            full.names = TRUE)
variable_list <- grep("tif", variable_list, value=TRUE)
# Drop OpenID, None and SitePrepped
variable_list <- grep("OpenID|None|SitePrepped|OPENING_ID", variable_list, value = TRUE, invert = TRUE)
# For now remove this one because the raster contains only NA's -- Alana to fix and then remove this line
#variable_list <- grep("R11498_SpotBurn", variable_list, value = TRUE, invert = TRUE)
variables <- sapply(variable_list, raster)
# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]
names(variables) <- variable.name



#----------------------------Data Summaries------------------------------------#
#--- TABLE 1: Percent burn severity class in fire and within plantations
# this is at the pixel level, area within plantations (pixels in whole fire)

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
study_fireTable <- fread("./Inputs/StudyFireList.csv")

sevTable <- c()
for(i in 1:length(FiresOfInterest)){
  dNBR <- raster(paste0("./Inputs/Rasters/dNBR/",FiresOfInterest[[i]],"dNBR_CAT.tif"))
  plant <- read_sf(paste0("./Inputs/Vectors/",FiresOfInterest[[i]],"_Plantations.shp"))
  
  # % in each burn category
  frqTab <- freq(dNBR)
  haUnb <- unname(frqTab[1,2])*0.09 #in ha
  haLow <- unname(frqTab[2,2])*0.09 #in ha
  haMod <- unname(frqTab[3,2])*0.09 #in ha
  haHig <- unname(frqTab[4,2])*0.09 #in ha
  checkTotFireArea <- haUnb + haLow + haMod + haHig
  print(paste0("ha area estimated from raster ",checkTotFireArea," for ",FiresOfInterest[[i]]))
  propSev <- as.data.table(frqTab)
  propSev <- propSev[!is.na(value)]
  totPix <- sum(propSev$count)
  propSev[,propSev := count/totPix, by="value"]
  
  # clip dNBR to plantation boundaries
  dNBRPlant <- mask(dNBR, plant)
  propSevPlant <- as.data.table(freq(dNBRPlant))
  propSevPlant <- propSevPlant[!is.na(value)]
  totPix <- sum(propSevPlant$count)
  propSevPlant[,propSev := count/totPix, by="value"]
  
  sevTable <- rbind(sevTable, data.table(FireID = FiresOfInterest[i],
                                         Sev = propSev$value,
                                         PropTotFire = round(propSev$propSev*100,0),
                                         PropPlant = round(propSevPlant$propSev*100,0)))
}

sevTable <- merge(sevTable, study_fireTable[,.(FireID,FireName)])
#fwrite(sevTable,"./Outputs/Tables/Table1b.csv")

study_fires <- read_sf(paste0(SpatialFilesPath, "Vectors/StudyFires.shp"))
Fire_perims <- study_fires %>% filter(FIRE_NUMBE %in% FiresOfInterest)

AreaTable <- c()
for(ii in 1:length(FiresOfInterest)){
  #--- total area of the fire
  fp <- study_fires %>% filter(FIRE_NUMBE == FiresOfInterest[[ii]])
  fp$TotFireArea <- st_area(fp)
  fpDT <- as.data.table(fp)
  fpDT[,TotFireArea := unclass(TotFireArea)/10000]
  fire_area <- fpDT[,sum(TotFireArea)]
  
  #--- area in plantations
  fpl <- st_read(paste0(SpatialFilesPath,"Vectors/",FiresOfInterest[[ii]],"_Plantations.shp"))
  fpl$PlantArea <- st_area(fpl)
  fplDT <- as.data.table(fpl)
  fplDT[,PlantArea := unclass(PlantArea)/10000]
  Plantation_area <- fplDT[,sum(PlantArea)]
  
  AreaTable <- rbind(AreaTable,data.table(FireID = FiresOfInterest[[ii]], FireArea = round(fire_area,0),
                                          PlantationArea = round(Plantation_area,0), 
                                          PropPlantation = round((Plantation_area/fire_area)*100,0)))
}

table1_manuscript <- merge(AreaTable, 
                           study_fireTable[,.(FireID,FireName,StartDate,OfficialEndDate)], 
                           by="FireID")
fwrite(table1_manuscript, "./Outputs/Tables/Table1.csv")


#------Supp Material - Table 2 - proportion of treatments in fire and in analysis ------
#--- 1. Percent silvicultural treatment / fire 
#-- a. this is based off pixels that were analysed

Silvic_Vars <- c("BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "PileBurn", 
                 "Prune", "Soil", "Spaced", "SpotBurn", "TotPixAnalys")
Fires <- c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron")
#dat_list <- list(Chutanli, Nadina, Shovel, Island, Verdun, Tezzeron)
SilvicTable <- list()

# Count number of pixels that == 1 (by summing) in each treatment per fire

for(i in 1:length(Fires)){
  #dat <- as.data.table(dat_list[i])
  dat <- dat_dt[FireName == Fires[i]]
  dat[,TotPixAnalys:= 1] #total number of pixels in the analysis
  Silvic <- dat[, lapply(.SD, sum), .SDcols = (colnames(dat) %in% Silvic_Vars)]
  SilvicTable[[i]] <- Silvic[,Fire:=Fires[[i]]]
}

# number of pixels of each treatment, or Percent of pixels in each 
#treatment out of all the pixels that were included in the analysis
SilvicPCTable <- SilvicTable %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) %>% # move "Fire" to first column
  # Divide  by total rows to get % silviculture type/fire
  mutate_at(vars(-c(TotPixAnalys,Fire)), ~round(./TotPixAnalys*100, 1)) %>% 
  dplyr::select(-c(TotPixAnalys)) # Drop TotPixAnalys


#-- b. this is based off whole fire

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
SilvicFire <- list()

for(i in 1:length(FiresOfInterest)){
  # Bring in plantation preds to stack in each fire
  allPlantRasts <- variables[c(grep(FiresOfInterest[i],variables))]
  fireID <- str_extract(names(allPlantRasts[1]),FiresOfInterest[i])
  SimpleRastnames <- str_remove(str_remove(names(allPlantRasts),FiresOfInterest[i]),"_")
  names(allPlantRasts) <- SimpleRastnames
  #stack the simplified names and assign to fire id rast name
  assign(paste0(fireID,"rasts"), stack(allPlantRasts))
  PlantStack <- stack(allPlantRasts)
  
  ### Get total number of non NA cells in the fire 
  # the total number of cells in a fire that were in harvested units
  TotalCells <- ncell(PlantStack)-freq(PlantStack, value=NA)
  Total <- as.data.table(as.list(TotalCells)) 
  Total[, Fire := FiresOfInterest[i]]
  Total <- melt(Total, id.vars=c("Fire"))
  setnames(Total, "value", "total_Harv")
  
  # The total number of cells that fall within a given treatment 
  SilvicFrq <- freq(PlantStack, value=1, merge=TRUE)
  Silvic <- as.data.table(as.list(SilvicFrq))
  Silvic[, Fire := FiresOfInterest[i]]
  Silvic <- melt(Silvic, id.vars=c("Fire"))
  setnames(Silvic, "value", "tot_treat")
  
  # Join tables & calculate % of the fire area that 
  #had each silvicultural treatment applied
  Silvic <- full_join(Silvic, Total)
  SilvicFire[[i]] <- Silvic # final table below
}


#1a. Pixels used in analysis
AnalysisTable <- SilvicTable %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything())

#1b. Pixels from the whole harvested fire
SilvicFirePCtable <- SilvicFire %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) # move "Fire" to first column
#re-calculate the number of potential pixels in the fire (all harvested pixels)
SilvicFirePCtable[, totHarv:=max(total_Harv),by="Fire"]

#combine 1a and 1b
#1a
A_dt <- melt(AnalysisTable, id = c("Fire","TotPixAnalys"))
setnames(A_dt, "value","TreatPixAnalys")
A_dt[is.na(TreatPixAnalys),TreatPixAnalys:=0]

FireNum_Nam <- fread("./Inputs/StudyFireList.csv")
#1b
FireNum_Nam <- data.table(fireNam = c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron"),
                          fireNum = c("G41607","R21721","R11498","R11921","R11796","G51632"))
F_dt <- merge(SilvicFirePCtable, FireNum_Nam[,.(fireNum,fireNam)], by.x="Fire", by.y="fireNum")
F_dt[,Fire:=NULL][,total_Harv:=NULL]#[,PC:=NULL]
setnames(F_dt,c("fireNam"),c("Fire"))

a_f_dt <- merge(F_dt,A_dt, by=c("Fire","variable"), all=TRUE)
a_f_dt <- merge(F_dt,A_dt, by=c("Fire","variable"))
a_f_dt[,PC_Fire:=round(tot_treat/totHarv*100,1), by=c("Fire","variable")]
a_f_dt[,PC_Analysed:=round(TreatPixAnalys/TotPixAnalys*100,1), by=c("Fire","variable")]
a_f_dt <- a_f_dt[order(variable,Fire),.(variable,Fire,tot_treat,totHarv,PC_Fire,
                                        TreatPixAnalys,TotPixAnalys,PC_Analysed)]
write.csv(a_f_dt, "./Outputs/Tables/Supp-Table2.csv", row.names = FALSE)
#convert to hectares:
a_f_dt[,tot_treat_ha := tot_treat*0.09]
a_f_dt[,totHarv_ha := totHarv*0.09]
a_f_dt[,TreatPixAnalys_ha := TreatPixAnalys*0.09]
a_f_dt[,TotPixAnalys_ha := TotPixAnalys *0.09]

write.csv(a_f_dt[,.(variable,Fire,tot_treat_ha,totHarv_ha, PC_Fire,
                   TreatPixAnalys_ha, TotPixAnalys_ha,PC_Analysed)],
          "./Outputs/Tables/Supp-Table2_ha.csv", row.names = FALSE)



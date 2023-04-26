### Data summaries and visualizations 
# A. Clason & Ingrid Farnell
# March, 2022

# This script gathers summary statistics for the manuscript
# 1. Percent silvicultural treatment per fire
#    a. only pixels analysed
#    b. the whole fire area
# 2. Percent burn severity class within plantations
# 3. Burn severity variability within plantations (% burn severity in each class in each opening ID)


#--libraries
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.,
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-----------------------------1. Load data from RF analysis ----------------------------------------#
datPath <- "../BVRCfire/Inputs/"  #"C:/Users/farne/Documents/" # 
SpatialFilesPath <- "../BVRCfire/Inputs/" #"E:/Ingrid/Borealis/BVRCfire/Inputs/Study_fire_perimeters/"

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
#study_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Study_fire_perimeters/Study_fire_perimeters.shp"))
study_fires <- read_sf(paste0(SpatialFilesPath,"Shapefiles/", "Study_fire_perimeters.shp"))

plant_list <- list.files(paste0(SpatialFilesPath,"Shapefiles/"),
                         pattern = "Plantations.shp", 
                         recursive = FALSE, 
                         full.names=TRUE)
# dNBR rasters
dNBR_list <- list.files(paste0(SpatialFilesPath,"Rasters/dNBR/"),
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

# number of pixels of each treatment, or Percent of pixels in each treatment out of all the pixels that were included in the analysis
SilvicPCTable <- SilvicTable %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) %>% # move "Fire" to first column
  mutate_at(vars(-c(TotPixAnalys,Fire)), ~round(./TotPixAnalys*100, 1)) %>% # Divide  by total rows to get % silviculture type/fire
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
  Total <- as.data.table(as.list(TotalCells)) #for some reason, not every plant pred has the same total number of pixels - shouldn't they be all the same?
  #Ingrid - can you look into why this happened?
  Total[, Fire := FiresOfInterest[i]]
  Total <- melt(Total, id.vars=c("Fire"))
  setnames(Total, "value", "total_Harv")
  
  # The total number of cells that fall within a given treatment 
  SilvicFrq <- freq(PlantStack, value=1, merge=TRUE)
  Silvic <- as.data.table(as.list(SilvicFrq))
  Silvic[, Fire := FiresOfInterest[i]]
  Silvic <- melt(Silvic, id.vars=c("Fire"))
  setnames(Silvic, "value", "tot_treat")
  
  # Join tables & calculate % of the fire area that had each silvicultural treatment applied
  Silvic <- full_join(Silvic, Total)
  #Silvic[, PC:= round((tot_treat/total_Harv)*100, 1)]
  SilvicFire[[i]] <- Silvic # final table below
}
SilvicFirePCtable <- SilvicFire %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything())

#SilvicFire <- dcast(SilvicFirePCtable, 
 #                   Fire ~ variable,
  #                  value.var = c("tot_treat"))



#------Table 3 - proportion of treatments in fire and in analysis ------

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
F_dt <- merge(SilvicFirePCtable, FireNum_Nam[,.(FireID,FireName)], by.x="Fire", by.y="FireID")
F_dt[,Fire:=NULL][,total_Harv:=NULL]#[,PC:=NULL]
setnames(F_dt,c("FireName"),c("Fire"))

a_f_dt <- merge(F_dt,A_dt, by=c("Fire","variable"), all=TRUE)
a_f_dt <- merge(F_dt,A_dt, by=c("Fire","variable"))
a_f_dt[,PC_Fire:=round(tot_treat/totHarv*100,1), by=c("Fire","variable")]
a_f_dt[,PC_Analysed:=round(TreatPixAnalys/TotPixAnalys*100,1), by=c("Fire","variable")]
a_f_dt <- a_f_dt[order(variable,Fire),.(variable,Fire,tot_treat,totHarv,PC_Fire,
                                        TreatPixAnalys,TotPixAnalys,PC_Analysed)]
write.csv(a_f_dt, "./Outputs/Tables/Table2.csv", row.names = FALSE)
#convert to hectares:
a_f_dt[,tot_treat_ha := tot_treat*0.09]
a_f_dt[,totHarv_ha := totHarv*0.09]
a_f_dt[,TreatPixAnalys_ha := TreatPixAnalys*0.09]
a_f_dt[,TotPixAnalys_ha := TotPixAnalys *0.09]

write.csv(a_f_dt[,.(variable,Fire,tot_treat_ha,totHarv_ha, PC_Fire,
                   TreatPixAnalys_ha, TotPixAnalys_ha,PC_Analysed)],
          "./Outputs/Tables/Table2_ha.csv", row.names = FALSE)
a_f_dt[,mean(na.omit(PC_Fire)),by="variable"]
a_f_dt[,sd(na.omit(PC_Fire)),by="variable"]

#get the stats for plantation age:

#range of plantation ages analyzed:
#PixUse_DT <- rbind(Chutanli, Island,Nadina,Shovel,Tezzeron,Verdun, fill=TRUE)
Pl_age_use <- dat_dt[,.(minAge = min(PlantAge), maxAge = max(PlantAge)), by="FireID"]

#range of plantation ages across rasters:
PlAge_rasts <- variables[c(grep("PlantAge",variables))]
Pl_age_all <- as.data.table(do.call(rbind,lapply(PlAge_rasts, function(x)cellStats(x,"range"))),
                            row.names=TRUE)
FireNum_Nam <- data.table(fireNam = c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron"),
                          fireNum = c("G41607_PlantAge","R21721_PlantAge","R11498_PlantAge",
                                      "R11921_PlantAge","R11796_PlantAge","G51632_PlantAge"))
merge(Pl_age_all,FireNum_Nam, by="")


#-------------------------------------------------------------------------


#--- 2. Percent burn severity class within plantations
# this is at the pixel level, area within plantations (pixels in whole fire)

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
study_fireTable <- fread("./Inputs/StudyFireList.csv")

sevTable <- c()
for(i in 1:length(FiresOfInterest)){
  dNBR <- raster(paste0("../BVRCfire/Inputs/Rasters/dNBR/",FiresOfInterest[[i]],"dNBR_CAT.tif"))
  plant <- read_sf(paste0("../BVRCfire/Inputs/Shapefiles/",FiresOfInterest[[i]],"_Plantations.shp"))
  
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
fwrite(sevTable,"./Outputs/Tables/Table1b.csv")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#----------------- Fire level summary - area in plantations -------------------#
#------------------------------------------------------------------------------#
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")

study_fires <- read_sf(paste0(SpatialFilesPath,"Shapefiles/", "Study_fire_perimeters.shp"))
Fire_perims <- study_fires %>% filter(FIRE_NUMBE %in% FiresOfInterest)
study_fireTable <- fread("./Inputs/StudyFireList.csv")

AreaTable <- c()
for(ii in 1:length(FiresOfInterest)){
  #--- total area of the fire
  fp <- Fire_perims %>% filter(FIRE_NUMBE == FiresOfInterest[[ii]])
  fp$TotFireArea <- st_area(fp)
  fpDT <- as.data.table(fp)
  fpDT[,TotFireArea := unclass(TotFireArea)/10000]
  fire_area <- fpDT[,sum(TotFireArea)]
  
  #--- area in plantations
  fpl <- st_read(paste0(SpatialFilesPath,"Shapefiles/",FiresOfInterest[[ii]],"_Plantations.shp"))
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

############## All plantations summed ##########
#figuring out area wthin each fire severity category for the whole fire  - might have done this already??
Fire_Sev <- severity_sel %>% filter(FIRE_NUMBE %in% Fire_shortList)
#For every fire, calculate the area (ha) of each severity polygon
Fire_Sev$FireSevArea <- st_area(Fire_Sev)
Fire_SevDT <- as.data.table(Fire_Sev)
FireSevSum <- Fire_SevDT[,round(sum(unclass(FireSevArea)/10000),0),by=c("FIRE_NUMBE","BURN_SEVER")]
setnames(FireSevSum,"V1","FireSev")
#Openings and fire severity
PlantAgeLong <- melt(PlantationsAge[,.(FireID,OPENING_ID,High,Medium,Low,Unburned,TotOpenArea)],
                     id.vars = c("FireID","OPENING_ID","TotOpenArea"))
PlantAgeLong[,PropArea:= value*TotOpenArea]
PlantSevSum <- PlantAgeLong[,round(sum(na.omit(PropArea)),0),by=c("FireID","variable")]
setnames(PlantSevSum,"V1","PlantSev")
PlantFire_Sev <- merge(FireSevSum,PlantSevSum, by.x=c("FIRE_NUMBE","BURN_SEVER"),by.y=c("FireID","variable"))
PlantFire_Sev_Tot <- merge(PlantFire_Sev,firesDTmerge, by="FIRE_NUMBE")
PlantFire_Sev_Tot[,sum(PlantSev),by="FIRE_NUMBE"]
PlantFire_Sev_Tot[,sum(PlantSev)]
PlantFire_Sev_Tot[,sum(PlantSev)]



############### Unioned together ##############
#versus when unioned together (accounting for spatial overlap)
Plant_union <- fread("./Outputs/IndividualFires/PropBurnDat.csv")
Plant_union <- Plant_union %>% filter(FIRE_NUMBE %in% Fire_shortList)
Plant_union[,sum(PlantAreaSev),by="FIRE_NUMBE"]
Plant_union[,sum(na.omit(PlantAreaSev))] #unioned ==22713 - can't be right


PlantFire_Sev_PlUnion <-merge(PlantFire_Sev,Plant_union, by=c("FIRE_NUMBE","BURN_SEVER"))










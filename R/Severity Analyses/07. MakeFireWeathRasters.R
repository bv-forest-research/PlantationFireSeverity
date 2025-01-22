#Make fire weather and fire weather indices rasters
#based on the day of burn raster

#Alana Clason

#January 21, 2022

library(data.table)
library(raster)
library(dplyr)
library(ggplot2)
# Using BCWS fire weather data now:

#Nadina fire: Nadina, Parrott, Houston, Peden
#Verdun fire: Grassy Plains, Parrott
#Island fire: Holy Cross, East Ootsa
#Shovel fire: Augier, Holy Cross, Vanderhoof
#Chutanli fire: Kluskus, Moose Lake
#Tezzeron fire:Fort st james, north chilko
#Shag Creek: Baldface, moose lake, nazko
#North Baezaeko: Nazko, Tautri
#Baldface Mountain: Baldface, Anahim Lake
#Remianing 2018 fires: R12068 (Pondosy), R12315 (Tesla), VA1964 (Dean River) and VA1787 (Ramsey Creek) not included
#as they are in the park

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
StudyFireList <- fread("./Inputs/StudyFireList.csv")
sfl <- StudyFireList[FireID %in% FiresOfInterest]
Stations <- c("Nadina","Parrott","Houston","Peden","GrassyPlains","HolyCross2",
              "EastOotsa","AugierLake","Vanderhoof","Kluskus","FortStJames","NorthChilco")
fw_sts <- data.table()
for(ii in 1:length(Stations)){
  fw_st <- fread(paste0("./Inputs/Fireweather/2018_",Stations[ii],".csv"),header=TRUE)
  fw_st[, ':='(stationID = Stations[ii])]
  fw_sts <- rbind(fw_sts,fw_st)
}
sf_sts <- data.table(FireName=c("Nadina","Nadina","Nadina","Nadina",
                                "Verdun","Verdun",
                                "Island","Island",
                                "Shovel","Shovel","Shovel",
                                "Chutanli",
                                "Tezzeron","Tezzeron"),
                     stationID=c("Nadina","Parrott","Houston","Peden",
                                 "GrassyPlains","Parrott",
                                 "HolyCross2","EastOotsa",
                                 "AugierLake","HolyCross2","Vanderhoof",
                                 "Kluskus",
                                 "FortStJames","NorthChilco"))
sf_sts <- merge(sfl[,.(FireID,FireName)],sf_sts,by="FireName", all.y=TRUE)
sf_sts <- merge(sf_sts, fw_sts, by=c("stationID"), allow.cartesian=TRUE)
sf_sts[,dateNoHr:= as.Date(as.character(weather_date),format="%Y%m%d")]
sf_sts[,jDay:=yday(dateNoHr)] #julian day
min(sf_sts[,dateNoHr])
#calculate the daily max temp and winds, and min Rh for each station for each fire
sts_Dailies <- sf_sts[,.(maxTemp=max(na.omit(temperature)),minRH=min(na.omit(relative_humidity)),
                         maxWind=max(na.omit(wind_speed))), 
                      by=c("FireID","stationID","dateNoHr","jDay")]
fwi_sts <- data.table()
for(ii in 1:length(Stations)){
  fwi_st <- fread(paste0("./Inputs/Fireweather/2018_",Stations[ii],"_Daily.csv"),header=TRUE)
  fwi_st[, ':='(stationID = Stations[ii])]
  fwi_sts <- rbind(fwi_sts,fwi_st)
}
fwi_sts[,dateNoHr:= as.Date(as.character(weather_date),format="%Y%m%d")][,jDay:=yday(dateNoHr)] #julian day
fwi_dailies <- merge(fwi_sts[,.(stationID,dateNoHr,jDay,gc,ffmc,dmc,dc,isi,bui,fwi)],sts_Dailies,
                     by=c("stationID","dateNoHr","jDay"), all.y=TRUE)

#calculate average daily weather for each fire
sts_Mns <- fwi_dailies[,.(MnMaxTemp = mean(maxTemp),
                          MnMinRH = mean(minRH),
                          MnMaxWind = mean(maxWind),
                          MnBUI = mean(bui),
                          MnISI = mean(isi),
                          MnDMC = mean(dmc),
                          MnDC = mean(dc),
                          MnFWI = mean(fwi)),
                       by=c("FireID","dateNoHr","jDay")]
fwi_d <- merge(fwi_dailies, data.table(FireID = c("G41607","R11921","R21721","R11498","G51632","R11796"),
                              FireName = c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun")),
               by="FireID")
ggplot(fwi_d)+
  geom_line(aes(x=jDay,y=fwi, colour=FireName),linewidth=1)+
  scale_colour_viridis_d(name="Wildfire name",
                         labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
  xlab("Julian date")+
  ylab("Fire Weather Index")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
ggsave(filename = "Fig1a_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")



ggplot()+
  geom_point(data = fwi_dailies[FireID=="R11498"], aes(x=jDay,y=dc))+
  geom_point(data = sts_Mns[FireID=="R11498"], aes(x=jDay,y=MnDC, colour="red"))+
  facet_wrap(~FireID)
#mean chutlanli is the same as raw data because there's only one weather station
ggplot()+
  geom_point(data = fwi_dailies[FireID=="R11498"], aes(x=jDay,y=maxTemp))+
  geom_point(data = sts_Mns[FireID=="R11498"], aes(x=jDay,y=MnMaxTemp, colour="red"))
  
for(ii in 1:length(FiresOfInterest)){
  #read in DOB rasters
  dob <- raster(paste0("./Inputs/Rasters/DOB/dob_",FiresOfInterest[ii],".tif"))
  DaysOfInterest <- seq(min(na.omit(dob[])), max(na.omit(dob[])))
  bui_rast <- dob
  isi_rast <- dob
  dmc_rast <- dob
  dc_rast <- dob
  fwi_rast <- dob
  maxT_rast <- dob
  minRH_rast <- dob
  maxW_rast <- dob
  for(i in 1:length(DaysOfInterest)){
    bui_rast[bui_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnBUI
    isi_rast[isi_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnISI
    dmc_rast[dmc_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnDMC
    dc_rast[dc_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnDC
    fwi_rast[fwi_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnFWI
    maxT_rast[maxT_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMaxTemp
    minRH_rast[minRH_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMinRH
    maxW_rast[maxW_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMaxWind
  }
  writeRaster(bui_rast,paste0("./Inputs/Rasters/FireWeather/bui_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(isi_rast,paste0("./Inputs/Rasters/FireWeather/isi_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(dmc_rast,paste0("./Inputs/Rasters/FireWeather/dmc_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(dc_rast,paste0("./Inputs/Rasters/FireWeather/dc_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(fwi_rast,paste0("./Inputs/Rasters/FireWeather/fwi_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(maxT_rast,paste0("./Inputs/Rasters/FireWeather/maxT_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(minRH_rast,paste0("./Inputs/Rasters/FireWeather/minRH_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(maxW_rast,paste0("./Inputs/Rasters/FireWeather/maxW_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
}


#supp figure - 2018 fire weather compared to other years
library(lubridate)
gp_weath <- fread("../BVRCfire/Inputs/Fireweather/1995_2021_Grassy_Plains.csv")
gp_weath[, `:=`(YEAR = as.integer(substr(DATE_TIME, 1, 4)),
                MONTH_DAY = substr(DATE_TIME, 5, 8),
                TIME = as.integer(substr(DATE_TIME, 9, 10)))]#[,yr_col := ifelse(YEAR == 2018, "red", "grey")]
gp_weath[, JULIAN_DAY := yday(ymd(paste0(YEAR, "-", 
                                         substr(MONTH_DAY, 1, 2), "-", substr(MONTH_DAY, 3, 4))))]
gp_max <- gp_weath[,max(FIRE_WEATHER_INDEX, na.rm = TRUE), by = .(YEAR, JULIAN_DAY)]

gp_max <- gp_max[!is.na(V1)]
ggplot()+
  geom_point(data = gp_max, aes(x = JULIAN_DAY, y = V1, group = YEAR), colour = "grey", alpha = 0.5)+
  geom_line(data = gp_max, aes(x = JULIAN_DAY, y = V1, group = YEAR), colour = "grey", alpha = 0.5) +
  geom_point(data = gp_max[YEAR == 2018], aes(x = JULIAN_DAY, y = V1, group = YEAR), colour = "purple")+
  geom_line(data = gp_max[YEAR == 2018], aes(x = JULIAN_DAY, y = V1, group = YEAR), colour = "purple")+
  ylab("max daily FWI")+
  xlab("julian date")


  scale_color_manual(values = c("red" = "red", "grey" = alpha("grey", 50)))


 ggplot(gp_weath[!is.na(FIRE_WEATHER_INDEX)])+
   geom_point(aes(x = MONTH_DAY, y = FIRE_WEATHER_INDEX, colour = yr_col))+
   scale_color_manual(values = c("red" = "red", "grey" = alpha("grey", 50)))
 
 ggplot(TrainImp_dt, aes(x = PartialScore, y = reorder(VarNam,PartialScore))) +  
   geom_point()
 #geom_point(aes(colour=FireID), size=3) +
 #scale_color_viridis_d(name="Fire",labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
 xlab("Variable importance")+
   ylab("Variables")+
   theme_minimal()+
   theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
 


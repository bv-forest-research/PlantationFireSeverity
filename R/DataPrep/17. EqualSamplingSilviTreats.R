### Create equal sampling of silviculture treatments of interest
# A. Clason 
# February, 2023

#because this involves random sampling the "no treatment" pixels, we do the sampling here, and save the
# csvs to make the results reproducible. If run again, the W and p-values for the MannKendall analyses will
# vary slightly from those reported in the paper, but the results remain the same
#------------------------------ Load libraries----------------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------------1. read in csvs of silvicultural treatments and dNBR--------------------#
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")

sfiles <- list.files("../BVRCfire/Inputs/", "silv", full.names = TRUE)
silv_csv_l <- map(sfiles,fread)
silv_csv_n <- str_extract(sfiles, FiresOfInterest)
for(i in 1:length(FiresOfInterest)){
  silv_csv_l[[i]] <- silv_csv_l[[i]][,ID := silv_csv_n[i]]
}
silv_csv <- rbindlist(silv_csv_l, fill=TRUE)
ID_dt <- data.table(FireName = c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron"),
                    FireID = c("G41607","R21721","R11498","R11921","R11796","G51632"))
silv_csv <- merge(silv_csv,ID_dt, by.x="ID",by.y="FireID", all = TRUE)


#--------------2. select treatments of interest and create equal sampling --------------------#

#-------- BROADCAST BURNING --------
bb_all <- list()
for(ii in 1:length(FiresOfInterest)){
  bb_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="BroadBurn"]
  bb_0 <- silv_csv[ID==FiresOfInterest[ii]& BroadBurn==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & BroadBurn==0]),
                                                                 bb_N[BroadBurn==1]$N),]
  bb_1 <- silv_csv[ID==FiresOfInterest[ii] & BroadBurn==1]
  bb_all[[ii]] <- rbind(bb_0[,.(dNBR,BroadBurn,FireName,ID)],bb_1[,.(dNBR,BroadBurn,FireName,ID)])
}
bb_all <- do.call(rbind,bb_all)
bb_all[,BroadBurn := as.factor(BroadBurn)]
bb_all[, shapiro.test(dNBR), by=c("FireName","BroadBurn")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(bb_all, "../BVRCfire/Inputs/bb_all.csv")
#-------- DISC TRENCHING --------
d_all <- list()
for(ii in 1:length(FiresOfInterest)){
  d_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Disc"]
  if(nrow(d_N[Disc==1])>0){
    d_0 <- silv_csv[ID==FiresOfInterest[ii]& Disc==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Disc==0]),
                                                             d_N[Disc==1]$N), ]
    d_1 <- silv_csv[ID==FiresOfInterest[ii] & Disc==1]
  }
  d_all[[ii]] <- rbind(d_0[,.(dNBR,Disc,FireName,ID)],d_1[,.(dNBR,Disc,FireName,ID)])
}
d_all <- do.call(rbind,d_all)
d_all[,Disc := as.factor(Disc)]
d_all[, shapiro.test(dNBR), by=c("FireName","Disc")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(d_all, "../BVRCfire/Inputs/d_all.csv")
#-------- BRUSHING --------
b_all <- list()
for(ii in 1:length(FiresOfInterest)){
  b_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Brushed"]
  if(nrow(b_N[Brushed==1])>0){
    b_0 <- silv_csv[ID==FiresOfInterest[ii]& Brushed==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Brushed==0]),
                                                                b_N[Brushed==1]$N), ]
    b_1 <- silv_csv[ID==FiresOfInterest[ii] & Brushed==1]
  }
  b_all[[ii]] <- rbind(b_0[,.(dNBR,Brushed,FireName,ID)],b_1[,.(dNBR,Brushed,FireName,ID)])
}
b_all <- do.call(rbind,b_all)
b_all[,Brushed := as.factor(Brushed)]
b_all[, shapiro.test(dNBR), by=c("FireName","Brushed")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(b_all, "../BVRCfire/Inputs/b_all.csv")
#-------- SPACING --------
s_all <- list()
for(ii in 1:length(FiresOfInterest)){
  s_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Spaced"]
  if(nrow(s_N[Spaced==1])>0){
    s_0 <- silv_csv[ID==FiresOfInterest[ii]& Spaced==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Spaced==0]),
                                                               s_N[Spaced==1]$N), ]
    s_1 <- silv_csv[ID==FiresOfInterest[ii] & Spaced==1]
  }
  s_all[[ii]] <- rbind(s_0[,.(dNBR,Spaced,FireName,ID)],s_1[,.(dNBR,Spaced,FireName,ID)])
}
s_all <- do.call(rbind,s_all)
s_all[,Spaced := as.factor(Spaced)]
s_all[, shapiro.test(dNBR), by=c("FireName","Spaced")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(s_all, "../BVRCfire/Inputs/s_all.csv")

#-------- PILE BURNING --------
pb_all <- list()
for(ii in 1:length(FiresOfInterest)){
  pb_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="PileBurn"]
  if(nrow(pb_N[PileBurn==1])>0){
    pb_0 <- silv_csv[ID==FiresOfInterest[ii]& PileBurn==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] &
                                                                                  PileBurn==0]),
                                                                  pb_N[PileBurn==1]$N), ]
    pb_1 <- silv_csv[ID==FiresOfInterest[ii] & PileBurn==1]
  }
  pb_all[[ii]] <- rbind(pb_0[,.(dNBR,PileBurn,FireName,ID)],pb_1[,.(dNBR,PileBurn,FireName,ID)])
}
pb_all <- do.call(rbind,pb_all)
pb_all[,PileBurn := as.factor(PileBurn)]
pb_all[, shapiro.test(dNBR), by=c("FireName","PileBurn")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(pb_all, "../BVRCfire/Inputs/pb_all.csv")

#-------- ALL BURNING --------
ab_all <- list()
silv_csv[,Anyburns:= rowSums(.SD, na.rm=TRUE), .SDcols = c("BroadBurn","SpotBurn","PileBurn")]
silv_csv[,Anyburns:=ifelse(Anyburns>0,1,0)]

for(ii in 1:length(FiresOfInterest)){
  ab_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Anyburns"]
  if(nrow(ab_N[Anyburns==1])>0){
    ab_0 <- silv_csv[ID==FiresOfInterest[ii]& Anyburns==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] &
                                                                                  Anyburns==0]),
                                                                  ab_N[Anyburns==1]$N), ]
    ab_1 <- silv_csv[ID==FiresOfInterest[ii] & Anyburns==1]
  }
  ab_all[[ii]] <- rbind(ab_0[,.(dNBR,Anyburns,FireName,ID)],ab_1[,.(dNBR,Anyburns,FireName,ID)])
}
ab_all <- do.call(rbind,ab_all)
ab_all[,Anyburns := as.factor(Anyburns)]
ab_all[, shapiro.test(dNBR), by=c("FireName","Anyburns")]
#distributions pretty much non-normal, so non-parametric Wilcox test
fwrite(ab_all, "../BVRCfire/Inputs/ab_all.csv")




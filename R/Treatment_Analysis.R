### Analysis of silviculture (equal sample size, with and without) treatments
# A. Clason 
# February, 2023

#Get, sample and analyze the silviculture treatments data
# Figure 2
# Figure 6
# Figures S6 - S13
# Table 3

#------------------------------ Load libraries----------------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

#------------------------------ 1. read in csvs  ----------------------------------#
csvs_names <- list.files(path="../BVRCfire/Inputs/", pattern = "all.csv", full.names = TRUE)
eq_csvs <- purrr::map(csvs_names, fread)
tr_names <- str_split(list.files(path="../BVRCfire/Inputs/", pattern = "all.csv"), 
                      pattern = ".csv", simplify = TRUE)[,1]
names(eq_csvs) <- tr_names
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")

paper_pal <- c("#E69F00", "#56B4E9")

#------------------------------ 2. Analyse and plot  ------------------------------#
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

silv_csv[,Anyburns:= rowSums(.SD, na.rm=TRUE), .SDcols = c("BroadBurn","SpotBurn","PileBurn")]
silv_csv[,Anyburns:=ifelse(Anyburns>0,1,0)]

#-------- BROADCAST BURNING --------
bb_all <- eq_csvs$bb_all
bb_all[,BroadBurn:=as.factor(BroadBurn)]
bb_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(bb_all[ID==FiresOfInterest[[j]]])>0){
    bb_WT <- wilcox.test(dNBR ~ BroadBurn, data=bb_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = bb_WT$statistic, p =  bb_WT$p.value, 
                      ID = unique(bb_all[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "equal")
    bb_WT_a <- wilcox.test(dNBR ~ BroadBurn, data=silv_csv[ID==FiresOfInterest[[j]]],
                         paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt_a <- data.table(W = bb_WT_a$statistic, p =  bb_WT_a$p.value, 
                      ID = unique(silv_csv[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "all")
    bb_WT_dt <- rbind(bb_WT_dt,wdt,wdt_a)
  }
}
setkey(bb_WT_dt, ID)
knitr::kable(bb_WT_dt)

ggplot(bb_all)+
  geom_boxplot(aes(x=BroadBurn, y= dNBR, group=BroadBurn, fill = BroadBurn))+
  xlab("Broadcast burning")+
  ylab("Fire severity(dNBR)")+
  ylim(c(-500,1300))+
  theme_bw()+
  scale_fill_manual(values= paper_pal[1:2])+
  scale_colour_manual(values= paper_pal[1:2])+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text.x=element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig6a.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Chutanli, Verdun, Island, Nadina sig diff, Tezzeron and Shovel not.
bb_all[,.N, by=c("FireName","BroadBurn")]

#-------- DISC TRENCHING --------
d_all <- eq_csvs$d_all
d_all[,Disc:=as.factor(Disc)]
d_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(d_all[ID==FiresOfInterest[[j]]])>0){
    d_WT <- wilcox.test(dNBR ~ Disc, data=d_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = d_WT$statistic,p =  d_WT$p.value, 
                      ID = unique(d_all[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "equal")
    dt_WT_a <- wilcox.test(dNBR ~ Disc, data=silv_csv[ID==FiresOfInterest[[j]]],
                           paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt_a <- data.table(W = dt_WT_a$statistic, p =  dt_WT_a$p.value, 
                        ID = unique(silv_csv[ID==FiresOfInterest[[j]]]$FireName),
                        Dat = "all")
    d_WT_dt <- rbind(d_WT_dt,wdt,wdt_a)
  }
}
setkey(d_WT_dt, ID)
knitr::kable(d_WT_dt)
ggplot(d_all)+
  geom_boxplot(aes(x=Disc, y= dNBR, group=Disc, fill = Disc))+
  xlab("Disc trenching")+
  ylab("Fire severity(dNBR)")+
  ylim(c(-500,1300))+
  theme_bw()+
  scale_fill_manual(values= paper_pal)+
  scale_colour_manual(values= paper_pal)+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text = element_blank(),
        axis.title.y = element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig6b.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Chutanli and Nadina significant decline in dNBR, others no difference
d_all[,.N, by=c("FireName","Disc")]

#-------- BRUSHING --------
b_all <- eq_csvs$b_all
b_all[,Brushed:=as.factor(Brushed)]
b_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(b_all[ID==FiresOfInterest[[j]]])>0){
    b_WT <- wilcox.test(dNBR ~ Brushed, data=b_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = b_WT$statistic,p =  b_WT$p.value, 
                      ID = unique(b_all[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "equal")
    b_WT_a <- wilcox.test(dNBR ~ Brushed, data=silv_csv[ID==FiresOfInterest[[j]]],
                           paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt_a <- data.table(W = b_WT_a$statistic, p =  b_WT_a$p.value, 
                        ID = unique(silv_csv[ID==FiresOfInterest[[j]]]$FireName),
                        Dat = "all")
    b_WT_dt <- rbind(b_WT_dt,wdt,wdt_a)
  }
}
setkey(b_WT_dt, ID)
knitr::kable(b_WT_dt)

ggplot(b_all)+
  geom_boxplot(aes(x=Brushed, y= dNBR, group=Brushed, fill = Brushed))+
  xlab("Brushing")+
  ylab("Fire severity(dNBR)")+
  ylim(c(-500,1300))+
  theme_bw()+
  scale_fill_manual(values= paper_pal)+
  scale_colour_manual(values= paper_pal)+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text.x=element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig6c.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#All fires except Shovel had significant decline in dNBR
b_all[,.N, by=c("FireName","Brushed")]

#-------- SPACING --------
s_all <- eq_csvs$s_all
s_all[,Spaced:=as.factor(Spaced)]
s_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(s_all[ID==FiresOfInterest[[j]]])>0){
    s_WT <- wilcox.test(dNBR ~ Spaced, data=s_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = s_WT$statistic,p =  s_WT$p.value, 
                      ID = unique(s_all[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "equal")
    s_WT_a <- wilcox.test(dNBR ~ Spaced, data=silv_csv[ID==FiresOfInterest[[j]]],
                          paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt_a <- data.table(W = s_WT_a$statistic, p =  s_WT_a$p.value, 
                        ID = unique(silv_csv[ID==FiresOfInterest[[j]]]$FireName),
                        Dat = "all")
    s_WT_dt <- rbind(s_WT_dt,wdt,wdt_a)
  }
}
setkey(s_WT_dt, ID)
knitr::kable(s_WT_dt)

ggplot(s_all)+
  geom_boxplot(aes(x=Spaced, y= dNBR, group=Spaced, fill = Spaced))+
  xlab("Spacing")+
  ylab("Fire severity(dNBR)")+
  ylim(c(-500,1300))+
  theme_bw()+
  scale_fill_manual(values= paper_pal)+
  scale_colour_manual(values= paper_pal)+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text = element_blank(),
        axis.title.y = element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig6d.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Shovel had a higher dNBR with spacing, Chutanli, Verdun, Nadina and Island all had significant decline in dNBR
s_all[,.N, by=c("FireName","Spaced")]

#-------- PILE BURNING --------
pb_all <- eq_csvs$pb_all
pb_all[,PileBurn:=as.factor(PileBurn)]
pb_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(pb_all[ID==FiresOfInterest[[j]]])>0){
    pb_WT <- wilcox.test(dNBR ~ PileBurn, data = pb_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = pb_WT$statistic,p =  pb_WT$p.value, 
                      ID = unique(s_all[ID==FiresOfInterest[[j]]]$FireName))
    pb_WT_dt <- rbind(pb_WT_dt,wdt)
  }
}
ggplot(pb_all)+
  geom_boxplot(aes(x=PileBurn, y= dNBR, group=PileBurn, fill = PileBurn))+
  xlab("Piles burned")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal)+
  scale_colour_manual(values= paper_pal)+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text.x=element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig9e_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Shovel had a higher dNBR with spacing, Chutanli, Verdun, Nadina and Island all had significant decline in dNBR
pb_all[,.N, by=c("FireName","PileBurn")]

#-------- ANY BURNING --------
ab_all <- eq_csvs$ab_all
ab_all[,Anyburns:=as.factor(Anyburns)]
ab_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(ab_all[ID==FiresOfInterest[[j]]])>0){
    ab_WT <- wilcox.test(dNBR ~ Anyburns, data = ab_all[ID==FiresOfInterest[[j]]],
                         paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = ab_WT$statistic,p =  ab_WT$p.value, 
                      ID = unique(ab_all[ID==FiresOfInterest[[j]]]$FireName),
                      Dat = "equal")
    ab_WT_a <- wilcox.test(dNBR ~ Anyburns, data=silv_csv[ID==FiresOfInterest[[j]]],
                          paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt_a <- data.table(W = ab_WT_a$statistic, p =  ab_WT_a$p.value, 
                        ID = unique(silv_csv[ID==FiresOfInterest[[j]]]$FireName),
                        Dat = "all")
    ab_WT_dt <- rbind(ab_WT_dt,wdt,wdt_a)
  }
}
setkey(ab_WT_dt, ID)
knitr::kable(ab_WT_dt)
ggplot(ab_all)+
  geom_boxplot(aes(x=Anyburns, y= dNBR, group=Anyburns, fill = Anyburns))+
  xlab("Any prescribed fire")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal)+
  scale_colour_manual(values= paper_pal)+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none",axis.text.x=element_blank())+
  facet_wrap("FireName")
ggsave(filename = "Fig9f_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Shovel had a higher dNBR with spacing, Chutanli, Verdun, Nadina and Island all had significant decline in dNBR
ab_all[,.N, by=c("FireName","Anyburns")]

#checking if any of the treatments change results with all samples included
knitr::kable(bb_WT_dt)
knitr::kable(ab_WT_dt)
knitr::kable(d_WT_dt)
knitr::kable(b_WT_dt)
knitr::kable(s_WT_dt)


#--------------------------------Percent silviculture treatments figures-----------#
#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#------------------------------ 1. read in csvs  ----------------------------------#

a_f_dt <- fread("./Outputs/Tables/Supp_Table2.csv")

a_f_dt[,sum(PC_Fire),by="Fire"]
a_f_dt[,VarNam:=ifelse(variable =="Soil","Soil disturbance",
                  ifelse(variable =="BroadBurn","Broadcast burn",
                   ifelse(variable =="PileBurn","Pile burn",
                    ifelse(variable =="SpotBurn","Spot burn",
                     ifelse(variable =="DebrisMade","Debris made",
                      ifelse(variable =="DebrisPiled","Debris piled",
                        ifelse(variable == "Prune", "Pruned",
                          ifelse(variable == "Spaced", "Spaced",
                           ifelse(variable=="Brushed","Brushed",
                                 "Fertilized")))))))))]


#------------------------------ 2. plot  -------------------------------------------#

ggplot(a_f_dt,aes(x="",y=PC_Fire, fill= Fire))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_viridis_d(name="Fire",labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
  xlab("")+
  ylab("Percent harvested area treated")+
  theme_minimal()+
  #theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),strip.text.x = element_text(face="bold"), 
        text=element_text(size=18))+ #for presentations
  facet_wrap(~VarNam, strip.position = "bottom")

ggsave(filename = "Fig2.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")
ggsave(filename = "Fig2.jpg",
       path = "D:/Sync/BVRC/Fire program/communication/PlantationSeverity/",
       device='jpeg', dpi=500, bg="white")



#------------------------------Date range of silviculture treatments -----------------------------------#
#-------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#--------------------------------------- 1. read in csvs  ---------------------------------------------#

csvs_names <- list.files(path="../BVRCfire/Inputs/", pattern = "SitePrepDates", full.names = TRUE)
tr_csvs <- purrr::map(csvs_names, fread)
tr_names <- str_extract(str_split(list.files(path="../BVRCfire/Inputs/", pattern = "SitePrepDates"), 
                      pattern = ".csv", simplify = TRUE)[,1], paste0(FiresOfInterest,collapse = "|"))

tr_dt <- data.table(do.call(rbind, Map(cbind, tr_csvs, FireID = tr_names)))
studyFires <- fread("./Inputs/StudyFireList.csv")
tr_dt <- merge(tr_dt, studyFires[,.(FireID,FireName)], by="FireID")
#change disc back to soil
tr_dt[SP_Method=="DISC", GroupName:="Soil"]

#subset types of interest
tr_soil <- tr_dt[SP_Method== "SHARK"| SP_Method== "MOUND"| SP_Method== "CHAIN"| SP_Method== "DISC"]
tr_soil[,SP_name:= ifelse(SP_Method=="SHARK","Shark",
                           ifelse(SP_Method=="MOUND","Mound",
                                  ifelse(SP_Method=="CHAIN","Chain","Disc")))]

ggplot(tr_soil, aes(x=minD, xend=maxD, y=SP_name, yend=SP_name, color=SP_name)) +
  scale_colour_brewer(type = "seq", palette = "Spectral", name="Site treatments")+
  geom_segment(linewidth=5)+
  theme_minimal(base_size = 18)+
  theme(strip.text.x = element_text(face="bold"),
        axis.text = element_text(size = 14, face="bold"),
        axis.text.x = element_text(angle = 50, hjust = 1),
        legend.position = "none")+
  xlab("")+
  ylab("")+
  facet_wrap(~FireName)
ggsave(filename = "Fig10_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")

#Burning
tr_bu <- tr_dt[SP_Method== "BROAD"| GroupName== "PileBurn"| SP_Method== "SPOT"]
tr_bu[,SP_name:= ifelse(SP_Method=="BROAD","Broadcast",
                          ifelse(SP_Method=="SPOT","Spot","Pile"))]

ggplot(tr_bu, aes(x=minD, xend=maxD, y=SP_name, yend=SP_name, color=SP_name)) +
  scale_colour_brewer(type = "seq", palette = "Spectral", name="Burn treatments")+
  geom_segment(linewidth=5)+
  theme_minimal(base_size = 18)+
  theme(strip.text.x = element_text(face="bold"),
        axis.text = element_text(size = 14, face="bold"),
        axis.text.x = element_text(angle = 50, hjust = 1),
        legend.position = "none")+
  xlab("")+
  ylab("")+
  facet_wrap(~FireName)
ggsave(filename = "Fig11_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")

#Brushing, Spacing, Pruning, Fertilizing
tr_br <- tr_dt[GroupName== "Brushed"| GroupName== "Spaced"|
                 GroupName== "Pruned"| GroupName== "Fertilized"]

ggplot(tr_br, aes(x=minD, xend=maxD, y=GroupName, yend=GroupName, color=GroupName)) +
  scale_colour_brewer(type = "seq", palette = "Spectral")+
  geom_segment(linewidth=5)+
  theme_minimal(base_size = 18)+
  theme(strip.text.x = element_text(face="bold"),
        axis.text = element_text(size = 14, face="bold"),
        axis.text.x = element_text(angle = 50, hjust = 1),
        legend.position = "none")+
  xlab("")+
  ylab("")+
  facet_wrap(~FireName)
ggsave(filename = "Fig12_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")

#--------------------------------------- Brushing type  ---------------------------------------------#

csvs_names <- list.files(path="../BVRCfire/Inputs/", pattern = "Brush_type", full.names = TRUE)
tr_csvs <- purrr::map(csvs_names, fread)
tr_names <- str_extract(str_split(list.files(path="../BVRCfire/Inputs/", pattern = "Brush_type"), 
                                  pattern = ".csv", simplify = TRUE)[,1], paste0(FiresOfInterest,collapse = "|"))

br_dt <- data.table(do.call(rbind, Map(cbind, tr_csvs, FireID = tr_names)))
studyFires <- fread("./Inputs/StudyFireList.csv")
br_dt <- merge(br_dt, studyFires[,.(FireID,FireName)], by="FireID")
br_dt <- br_dt[BRUSHING_T != ""]

ggplot(br_dt)+
  geom_bar(aes(x="",y=N, fill= BRUSHING_T),stat="identity", position = "dodge")+
  scale_fill_brewer(type = "seq", palette = "Spectral",name="Brushing Type",
                    labels=c("Biological","Chemical-Air","Chemical-Ground","Manual","Mechanical"))+
  theme_minimal(base_size = 18)+
  theme(strip.text.x = element_text(face="bold"))+
  xlab("")+
  ylab("Number of applications")+
  facet_wrap(~FireName)
ggsave(filename = "Fig13_supp.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=500, bg="white")







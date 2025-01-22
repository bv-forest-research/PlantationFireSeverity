#This script adds spatial autocorrelation variables, cleans the data a bit more and saves the shapefiles that
# will be used for RF analysis

# scale dNBR *1000
# set fire history == 0 to 100
# add principal coordinate analysis variables to predictors


#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table", "magrittr") # Data Management and Manipulation
ls <- append(ls, c("sf")) # geo comp.
ls <- append(ls,c("vegan","corrplot"))

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

#------------------------------ Load data ---------------------------------#
ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn","dNBRCAT")

Chutanli <- fread("./Inputs/Datasets/G41607dat270.csv")
Chutanli <- Chutanli %>%
  mutate_at((colnames(Chutanli)[colnames(Chutanli) %in% ctg_variables]), factor) %>%
  dplyr::select(-c("dNBRReSamp")) 
Chutanli[,dNBR := dNBR*1000]
Chutanli[HistoricFires==0 ,HistoricFires:=100]
Chutanli_dist <- dist(Chutanli[,.(x,y)], method = "euclidean")
Chutanli_pcnm <- pcnm(Chutanli_dist, threshold = 10000)
Chutanli[, c("PCNM1","PCNM2", "PCNM3",
             "PCNM4", "PCNM5","PCNM6") := .(Chutanli_pcnm$vectors[,"PCNM1"],
                                            Chutanli_pcnm$vectors[,"PCNM2"],
                                            Chutanli_pcnm$vectors[,"PCNM3"],
                                            Chutanli_pcnm$vectors[,"PCNM4"],
                                            Chutanli_pcnm$vectors[,"PCNM5"],
                                            Chutanli_pcnm$vectors[,"PCNM6"])]

Tezzeron <- fread("./Inputs/Datasets/G51632dat270.csv")
Tezzeron <- Tezzeron %>%
  mutate_at((colnames(Tezzeron)[colnames(Tezzeron) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Tezzeron[,dNBR := dNBR*1000]
#Tezzeron[HistoricFires==0 ,Tezzeron:=100] # no historic fires
Tezzeron_dist <- dist(Tezzeron[,.(x,y)], method = "euclidean")
Tezzeron_pcnm <- pcnm(Tezzeron_dist, threshold = 10000)
Tezzeron[, c("PCNM1","PCNM2", "PCNM3",
             "PCNM4", "PCNM5","PCNM6") := .(Tezzeron_pcnm$vectors[,"PCNM1"],
                                            Tezzeron_pcnm$vectors[,"PCNM2"],
                                            Tezzeron_pcnm$vectors[,"PCNM3"],
                                            Tezzeron_pcnm$vectors[,"PCNM4"],
                                            Tezzeron_pcnm$vectors[,"PCNM5"],
                                            Tezzeron_pcnm$vectors[,"PCNM6"])]

Shovel <- fread("./Inputs/Datasets/R11498dat270.csv")
Shovel <- Shovel %>%
  mutate_at((colnames(Shovel)[colnames(Shovel) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Shovel[,dNBR := dNBR*1000]
Shovel[HistoricFires==0 ,HistoricFires:=100]
Shovel_dist <- dist(Shovel[,.(x,y)], method = "euclidean")
Shovel_pcnm <- pcnm(Shovel_dist, threshold = 10000)
Shovel[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Shovel_pcnm$vectors[,"PCNM1"],
                                          Shovel_pcnm$vectors[,"PCNM2"],
                                          Shovel_pcnm$vectors[,"PCNM3"],
                                          Shovel_pcnm$vectors[,"PCNM4"],
                                          Shovel_pcnm$vectors[,"PCNM5"],
                                          Shovel_pcnm$vectors[,"PCNM6"])]

Verdun <- fread("./Inputs/Datasets/R11796dat270.csv")
Verdun <- Verdun %>%
  mutate_at((colnames(Verdun)[colnames(Verdun) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Verdun[,dNBR := dNBR*1000]
Verdun[HistoricFires==0 ,HistoricFires:=100]
Verdun_dist <- dist(Verdun[,.(x,y)], method = "euclidean")
Verdun_pcnm <- pcnm(Verdun_dist, threshold = 10000)
Verdun[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Verdun_pcnm$vectors[,"PCNM1"],
                                          Verdun_pcnm$vectors[,"PCNM2"],
                                          Verdun_pcnm$vectors[,"PCNM3"],
                                          Verdun_pcnm$vectors[,"PCNM4"],
                                          Verdun_pcnm$vectors[,"PCNM5"],
                                          Verdun_pcnm$vectors[,"PCNM6"])]

Island <- fread("./Inputs/Datasets/R11921dat270.csv")
Island <- Island %>%
  mutate_at((colnames(Island)[colnames(Island) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Island[,dNBR := dNBR*1000]
Island[HistoricFires==0 ,HistoricFires:=100]
Island_dist <- dist(Island[,.(x,y)], method = "euclidean")
Island_pcnm <- pcnm(Island_dist, threshold = 10000)
Island[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Island_pcnm$vectors[,"PCNM1"],
                                          Island_pcnm$vectors[,"PCNM2"],
                                          Island_pcnm$vectors[,"PCNM3"],
                                          Island_pcnm$vectors[,"PCNM4"],
                                          Island_pcnm$vectors[,"PCNM5"],
                                          Island_pcnm$vectors[,"PCNM6"])]

Nadina <- fread("./Inputs/Datasets/R21721dat270.csv")
Nadina <- Nadina %>%
  mutate_at((colnames(Nadina)[colnames(Nadina) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Nadina[,dNBR := dNBR*1000]
Nadina[HistoricFires==0 ,HistoricFires:=100]
Nadina_dist <- dist(Nadina[,.(x,y)], method = "euclidean")
Nadina_pcnm <- pcnm(Nadina_dist, threshold = 10000)
Nadina[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Nadina_pcnm$vectors[,"PCNM1"],
                                          Nadina_pcnm$vectors[,"PCNM2"],
                                          Nadina_pcnm$vectors[,"PCNM3"],
                                          Nadina_pcnm$vectors[,"PCNM4"],
                                          Nadina_pcnm$vectors[,"PCNM5"],
                                          Nadina_pcnm$vectors[,"PCNM6"])]

#ordisurf(Chutanli_xy, scores(Chutanli_pcnm, choices=1), bubble = 4, main = "PCNM 1")
#plot(Chutanli_pcnm$values) #most the variation is in the first 20 or so eigenvectors

#ggplot()+
# geom_point(aes(y=Chutanli$dNBR, x= Chutanli_pcnm$vectors[,17]))+
#geom_smooth(aes(y=Chutanli$dNBR, x= Chutanli_pcnm$vectors[,17]), method="gam")


#update dnbr categories based on re-calibration post-CBI work
m <- c(-Inf,120, 270, 635,Inf)
#m <- c(-Inf, 99, 269, 659, Inf)

Chutanli[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                           right = TRUE, include.lowest = TRUE)]
Tezzeron[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                           right = TRUE, include.lowest = TRUE)]
Shovel[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                         right = TRUE, include.lowest = TRUE)]
Verdun[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                         right = TRUE, include.lowest = TRUE)]
Island[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                         right = TRUE, include.lowest = TRUE)]
Nadina[, dNBR_cal := cut(dNBR, breaks = m, labels = c("1","2","3","4"),
                         right = TRUE, include.lowest = TRUE)]

#create datasets with variables to include in analysis. We are keeping them (cat response and continuous response) as seperate datasets, because it's imbedded in the code below to pass the entire object and not specify which columns to ignore
Chutanli_sf_con <- sf::st_as_sf(Chutanli[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Chutanli_sf_cat <- sf::st_as_sf(Chutanli[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Chutanli_sf_cal <- sf::st_as_sf(Chutanli[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Chutanli_sf_cat, "./Inputs/Vectors/Chutanli_cat.gpkg")
write_sf(Chutanli_sf_cal, "./Inputs/Vectors/Chutanli_cal.gpkg")

Tezzeron_sf_con <- sf::st_as_sf(Tezzeron[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Tezzeron_sf_cat <- sf::st_as_sf(Tezzeron[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Tezzeron_sf_cal <- sf::st_as_sf(Tezzeron[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Tezzeron_sf_cat, "./Inputs/Vectors/Tezzeron_cat.gpkg")
write_sf(Tezzeron_sf_cal, "./Inputs/Vectors/Tezzeron_cal.gpkg")

Shovel_sf_con <- sf::st_as_sf(Shovel[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Shovel_sf_cat <- sf::st_as_sf(Shovel[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Shovel_sf_cal <- sf::st_as_sf(Shovel[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Shovel_sf_cat, "./Inputs/Vectors/Shovel_cat.gpkg")
write_sf(Shovel_sf_cal, "./Inputs/Vectors/Shovel_cal.gpkg")

Verdun_sf_con <- sf::st_as_sf(Verdun[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Verdun_sf_cat <- sf::st_as_sf(Verdun[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Verdun_sf_cal <- sf::st_as_sf(Verdun[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Verdun_sf_cat, "./Inputs/Vectors/Verdun_cat.gpkg")
write_sf(Verdun_sf_cal, "./Inputs/Vectors/Verdun_cal.gpkg")

Island_sf_con <- sf::st_as_sf(Island[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Island_sf_cat <- sf::st_as_sf(Island[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Island_sf_cal <- sf::st_as_sf(Island[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Island_sf_cat, "./Inputs/Vectors/Island_cat.gpkg")
write_sf(Island_sf_cal, "./Inputs/Vectors/Island_cal.gpkg")

Nadina_sf_con <- sf::st_as_sf(Nadina[,-c("dNBRCAT","dNBR_cal")], coords = c("x", "y"))
Nadina_sf_cat <- sf::st_as_sf(Nadina[,-c("dNBR","dNBR_cal")], coords = c("x", "y"))
Nadina_sf_cal <- sf::st_as_sf(Nadina[,-c("dNBR","dNBRCAT")], coords = c("x", "y"))
write_sf(Nadina_sf_cat, "./Inputs/Vectors/Nadina_cat.gpkg")
write_sf(Nadina_sf_cal, "./Inputs/Vectors/Nadina_cal.gpkg")

# --------------------------------- test correlated variables -----------------------------------
#--- Assess correlated covariates
plot <- list()
corrT <- list()
task_names <- c("Chutlani","Nadina","Shovel","Island","Verdun","Tezzeron")
dat_names <- list(Chutanli,Nadina,Shovel,Island,Verdun,Tezzeron)

for(i in 1:length(dat_names)){
  dat <- as.data.table(dat_names[[i]])
  dat <- dat %>% select_if(is.numeric)
  
  corrMat <- round(cor(dat, method="spearman"), 2)
  
  plot[[i]] <- corrplot(corrMat, 
                        title = paste0(task_names[i]))
  # 0.7 cutoff
  corr <- as.data.frame(corrMat)
  corr[abs(corr) < .7 & abs(corr) < 1] <- ""
  write.csv(corr, paste0(datPath, task_names[i], "corr.csv"))
}


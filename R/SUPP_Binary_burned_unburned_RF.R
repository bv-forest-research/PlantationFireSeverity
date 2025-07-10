#binary (burned/unburned) RF




#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table", "magrittr") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.
ls <- append(ls,c("pdp","mlr3","mlr3spatiotempcv","mlr3verse","ranger"))
ls <- append(ls,c("iml","patchwork","DALEX","DALEXtra"))

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

# RF analysis
dat_names <- list.files(path="./Inputs/Vectors/", pattern = "bin.gpkg", full.names = TRUE)
dat_list <- purrr::map(dat_names, read_sf)

#Remove basal area outliers in Nadina:
dat_list[[3]] <- dat_list[[3]] %>%
  dplyr::filter(BASAL_AREA<70)


task_names <- c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun")

Test_Conf <- list()
TrainImp_dat <- list()
RF_scores <- list()
pd_scores <- list()
effects <- list()
NumCores <- 30

for(i in 1:length(task_names)){
  # Remove correlated covariates or those that are not included in the analysis
  dat <- dat_list[[i]]
  dat <- dat %>% 
    dplyr::select(., -dob, -bui, -dmc, -fwi, -DEMslope, -DEMaspect, 
                  -DFirCov, -FirCov, -PineCov, -SpruceCov)%>% #keep dc, isi for fire behaviour/spread
    mutate_if(sapply(., is.character), as.factor)
  
  task <- as_task_classif_st(dat, target = "dNBR_bin", 
                             id = task_names[i])
  #task <- as_task_classif_st(dat, target = "dNBRCAT", 
  #                          id = task_names[i])
  
  #--- Step 1: Define a learner
  # default ranger parameters: https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.ranger.html
  learner <- lrn("classif.ranger",
                 importance = "permutation",
                 respect.unordered.factors = "order",
                 predict_type = "response",
                 num.trees = 2000,
                 num.threads=NumCores)
  
  #--- Step 2: Split stratified task into test/train data
  set.seed(456)
  split <- partition(task, ratio=0.8, stratify=TRUE)
  task_train <- task$clone()$filter(rows=split$train)
  task_test <- task$clone()$filter(rows=split$test)
  
  #--- Step 3: Feature selection (using nested resampling)
  measure <- msr("classif.ce") #classification error
  fselector <- fs("random_search", batch_size = 30)
  terminator <- trm("evals", n_evals=100)
  inner_resampling <- rsmp("spcv_coords", folds=5)
  
  optFtlrn = AutoFSelector$new(
    learner = learner,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator,
    fselector = fselector
  )
  
  #--- Step 4: Train final model (applies tuning)
  set.seed(456)
  optFtlrn$train(task_train) 
  
  #--- Step 5: Predict on test data
  optFtlrnPred <- optFtlrn$predict(task_test) 
  optFtlrnPred$score(measure)
  
  #trained model predictions
  #optFtlrnPred$prob
  Test_Conf[[i]] <- optFtlrnPred$confusion
  RF_scores[[i]] <- optFtlrnPred$score()
  
  #--- Step 6: Model visualization
  # Feature importance
  TrainImp_dat[[i]] <- as.data.table(optFtlrn$learner$importance(), keep.rownames = TRUE)
  
  # Feature Imp
  #  Level of importance of the features
  dat_train <- task_train$data()
  x_train <- dat_train[,-c("dNBR_bin")]
  #x_train <- dat_train[,-c("dNBRCAT")]
  
  model <- Predictor$new(optFtlrn, data = x_train, y = dat_train$dNBR_cal)
  #model <- Predictor$new(optFtlrn, data = x_train, y = dat_train$dNBRCAT)
  effects[[i]] <- FeatureEffects$new(model)
  model_exp = explain_mlr3(optFtlrn,
                           data = x_train, # provide data without y 
                           y = dat_train$dNBR_cal,
                           #y = dat_train$dNBRCAT,
                           colorize = FALSE,
                           type="classification")
  
  pd_scores[[i]] <- model_profile(model_exp)$agr_profiles
  
  preds_all <- optFtlrn$predict(task_train)
  
} 

# Classification error: 
RF_dat <- data.table(do.call(rbind, Map(cbind, unname(RF_scores), FireID = task_names)))
setnames(RF_dat, "V1", "CE")
fwrite(RF_dat, "./Outputs/Tables/RF_ClassificationError_bin.csv")

# Partial scores
TrainImp_dt <- data.table(do.call(rbind, Map(cbind, unname(TrainImp_dat), FireID = task_names)))
setnames(TrainImp_dt, c("V1","V2"), c("Variable","PartialScore"))
fwrite(TrainImp_dt, "./Outputs/Tables/RF_PartialScores_bin.csv")


TrainImp_dt <- fread("./Outputs/Tables/RF_PartialScores_bin.csv")
unb <- unique(TrainImp_dt$Variable)

TrainImp_dt[,VarCat:=ifelse(Variable =="PlantAge","Stand structure",
                            ifelse(Variable =="BASAL_AREA","Stand structure",
                                   ifelse(Variable =="CROWN_CLOS","Stand structure",
                                          ifelse(Variable =="HistoricFires","Previous wildfire",
                                                 ifelse(Variable =="decidCov","Stand structure",
                                                        ifelse(Variable =="conifCov","Stand structure",
                                                               ifelse(Variable =="PCNM1","Spatial autocorrelation",
                                                                      ifelse(Variable =="PCNM2","Spatial autocorrelation",
                                                                             ifelse(Variable =="PCNM3","Spatial autocorrelation",
                                                                                    ifelse(Variable =="PCNM4","Spatial autocorrelation",
                                                                                           ifelse(Variable =="PCNM5","Spatial autocorrelation",
                                                                                                  ifelse(Variable =="PCNM6","Spatial autocorrelation",
                                                                                                         ifelse(Variable =="DEMhli","Topography",
                                                                                                                ifelse(Variable =="DEMtpi","Topography",
                                                                                                                       ifelse(Variable =="BEC","Climate",
                                                                                                                              ifelse(Variable =="dc","Fuel moisture",
                                                                                                                                     ifelse(Variable =="isi","Fire behaviour",
                                                                                                                                            ifelse(Variable =="maxT","Fire weather",
                                                                                                                                                   ifelse(Variable =="maxRH","Fire weather",
                                                                                                                                                          ifelse(Variable =="maxW","Fire weather",
                                                                                                                                                                 ifelse(Variable =="minRH","Fire weather",
                                                                                                                                                                        ifelse(Variable =="FireRun","Fire spread",
                                                                                                                                                                               "Forest Management"))))))))))))))))))))))]

TrainImp_dt[,VarNam:=ifelse(Variable =="PlantAge","Plantation Age",
                            ifelse(Variable =="BASAL_AREA","Basal area (m2/ha)",
                                   ifelse(Variable =="CROWN_CLOS","Crown closure (%)",
                                          ifelse(Variable =="HistoricFires","Time since wildfire",
                                                 ifelse(Variable =="Soil","Soil disturbance",
                                                        ifelse(Variable =="BroadBurn","Broadcast burn",
                                                               ifelse(Variable =="PileBurn","Pile burn",
                                                                      ifelse(Variable =="SpotBurn","Spot burn",
                                                                             ifelse(Variable =="DebrisMade","Debris made",
                                                                                    ifelse(Variable =="DebrisPiled","Debris piled",
                                                                                           ifelse(Variable =="decidCov","Deciduous cover (%)",
                                                                                                  ifelse(Variable =="Fertil","Fertilized",
                                                                                                         ifelse(Variable == "conifCov","Conifer cover(%)",
                                                                                                                ifelse(Variable =="DEMhli","HLI",
                                                                                                                       ifelse(Variable =="DEMtpi","TPI",
                                                                                                                              ifelse(Variable =="BEC","BEC",
                                                                                                                                     ifelse(Variable =="dc","drought code (dc)",
                                                                                                                                            ifelse(Variable =="isi","initial spread index (isi)",
                                                                                                                                                   ifelse(Variable =="maxT","max temperature",
                                                                                                                                                          ifelse(Variable =="minRH","min relative humidity",
                                                                                                                                                                 ifelse(Variable =="maxW","max wind",
                                                                                                                                                                        ifelse(Variable =="FireRun","Area burned (%/day)",
                                                                                                                                                                               Variable))))))))))))))))))))))]

ggplot(TrainImp_dt, aes(x = PartialScore, y = reorder(VarNam,PartialScore))) +  
  geom_point(aes(colour=FireID), size=3) +
  scale_color_viridis_d(name="Fire",labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
  xlab("Variable importance")+
  ylab("Variables")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(TrainImp_dt, aes(x = PartialScore, y = reorder(VarCat,PartialScore))) +  
  geom_point(aes(colour=FireID), size=3) +
  scale_color_viridis_d(name="Fire",labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
  xlab("Variable importance")+
  ylab("Variable groups")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(TrainImp_dt, aes(x = PartialScore, y = reorder(VarNam,PartialScore))) +  
  geom_point(aes(colour=FireID), size=3) +
  scale_color_viridis_d(name="Fire",labels=c("Chutanli","Island","Nadina","Shovel","Tezzeron","Verdun"))+
  xlab("Variable importance")+
  ylab("Variable groups")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=15))+
  facet_wrap("FireID")


#From the data csvs
csvs_names <- list.files(path="./Inputs/Datasets", pattern = "dat270.csv", full.names = TRUE)
dat_csvs <- purrr::map(csvs_names, fread)
for(i in 1:6){
  dat_csvs[[i]][,FireID := c("Chutanli","Tezzeron","Shovel","Verdun","Island","Nadina")[i]]
}
dat_csv_dt <- rbindlist(dat_csvs, fill=TRUE)
setkey(dat_csv_dt,FireID)

#remove outlier
dat_csv_dt <- dat_csv_dt[BASAL_AREA<70]

#update dNBR bins:
m <- c(-Inf,120, Inf)
dat_csv_dt[, dNBR_cal := cut(dNBR*1000, breaks = m, labels = c("1","2"),
                             right = TRUE, include.lowest = TRUE)]


###### Plantation Age
ggplot(dat_csv_dt, aes(x=PlantAge, y=dNBRReSamp))+
  geom_point(aes(colour = as.factor(dNBR_cal)), size=2, shape=1, alpha =0.9)+
  scale_color_manual(name="Severity", labels=c("Unburned","Low","Moderate","High"),
                     values=c("darkgreen","orange"))+
  guides(colour = guide_legend(reverse = TRUE))+
  geom_smooth(aes(x=PlantAge, y=dNBRReSamp), 
              method="gam", formula= y ~ s(x),
              linewidth=1.5, colour="grey")+
  geom_smooth(aes(x=PlantAge, y=dNBRReSamp), 
              method="lm",
              linewidth=1.5, colour="black")+
  xlab("Plantation Age")+
  ylab("Fire severity (dNBR)")+
  theme_minimal()+
  #facet_wrap("FireID")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),strip.text.x = element_text(face="bold"), 
        text=element_text(size=21))



ggplot(dat_csv_dt, aes(x=PlantAge, y=dNBRReSamp))+
  geom_jitter(aes(colour = as.factor(dNBR_cal)), size=2, shape=1, alpha =0.9)+
  scale_color_manual(name="Severity", labels=c("Unburned","Low","Moderate","High"),
                     values=c("darkgreen","orange"))+
  geom_smooth(aes(x=PlantAge, y=dNBRReSamp), method="gam", formula= y ~ s(x), 
              color="black", linewidth=1.5)+
  #geom_smooth(aes(x=PlantAge, y=dNBRReSamp), 
   #           method="lm",
    #          linewidth=1.5, colour="black")+
  xlab("Plantation Age")+
  ylab("Fire severity (dNBR)")+
  theme_minimal()+
  facet_wrap("FireID")+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=15))


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


dat_names <- list.files(path="./Inputs/Shapefiles/", pattern = "cat.gpkg", full.names = TRUE)
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


#---------------------- use benchmarking RF runs to test which fire weather to keep ----------------

task_names <- c("Chutlani","Nadina","Shovel","Island","Verdun","Tezzeron")
dat_list <- list(Chutanli_sf_cat, Nadina_sf_cat, Shovel_sf_cat, Island_sf_cat,
                 Verdun_sf_cat, Tezzeron_sf_cat)
tab <- list()

for(i in 1:length(task_names)){
  # Remove correlated covariates
  dat <- dat_list[[i]]
  dat.1 <- dplyr::select(dat, -dob, -bui, -dc, -isi) #keep dmc, fwi
  dat.2 <- dplyr::select(dat, -dob, -bui, -dmc, -isi) #keep dc, fwi
  dat.3 <- dplyr::select(dat, -dob, -dc, -dmc, -isi) #keep bui, fwi
  dat.4 <- dplyr::select(dat, -dob, -bui, -dc, -fwi) #keep dmc, isi
  dat.5 <- dplyr::select(dat, -dob, -bui, -dmc, -fwi) #keep dc, isi
  dat.6 <- dplyr::select(dat, -dob, -dc, -dmc, -fwi) #keep bui, isi
  
  
  task.1 <- TaskClassifST$new(task_names[i],
                              backend = dat.1, 
                              target = "dNBRCAT")
  
  task.2 <- TaskClassifST$new(task_names[i],
                              backend = dat.2, 
                              target = "dNBRCAT")
  task.3 <- TaskClassifST$new(task_names[i],
                              backend = dat.3, 
                              target = "dNBRCAT")
  task.4 <- TaskClassifST$new(task_names[i],
                              backend = dat.4, 
                              target = "dNBRCAT")
  task.5 <- TaskClassifST$new(task_names[i],
                              backend = dat.5, 
                              target = "dNBRCAT")
  task.6 <- TaskClassifST$new(task_names[i],
                              backend = dat.6, 
                              target = "dNBRCAT")
  
  tasks <- list(task.1, task.2, task.3, task.4, task.5, task.6)
  
  #--- Step 2: Define a learner
  # default ranger parameters: https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.ranger.html
  learner <- lrn("classif.ranger",
                 importance = "permutation",
                 respect.unordered.factors = "order",
                 predict_type = "response",
                 num.threads=NumCores)
  
  
  #--- Step 3: Feature selection & task benchmarking (using nested resampling)
  measure <- msr("classif.ce") #classification error
  fselector <- fs("random_search", batch_size=20) # using random_search b/c of large search space
  #level = 0.2 
  #terminator = trm("perf_reached", level=level)
  #terminator = trm("stagnation", iters=40, threshold=0)
  terminator <- trm("evals", n_evals=20)
  inner_resampling <- rsmp("spcv_coords", folds=5)
  outer_resampling <- rsmp("spcv_coords", folds=5)
  
  # Feature selection
  optFtlrn <- AutoFSelector$new(
    learner = learner,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator,
    fselector = fselector
  )
  
  # Task benchmarking
  grid <- benchmark_grid(
    tasks = tasks,
    learner = optFtlrn,
    resampling = outer_resampling
  )
  
  # Apply optimal learner and benchmarking
  set.seed(456)
  bmr <- benchmark(grid, store_models = TRUE)
  
  measure <- msr("classif.ce")
  
  #--- Compare all tasks 
  tab[[i]] <- bmr$aggregate(measure)
  
} 
for(i in 1:6){
  tab[[i]][,FireID := task_names[i]]
}
tab_dt <- rbindlist(tab)

#which nr is the min for each fire?
tab_dt[,.SD[which.min(classif.ce)], by=.(FireID)]

#which is the most common? 5 or 6.

#select 5?



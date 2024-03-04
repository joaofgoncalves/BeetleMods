

library(tidyverse)
library(terra)
library(biomod2)
library(biomod2plus)


setwd("C:/MyFiles/R-dev/BeetleMods")
#setwd("D:/JG/BeetleMods")


## ------------------------------------------------------------------------------ ##

#working_dir <- "E:/R-dev/BeetleMods/OUT/mods"
working_dir <- "C:/MyFiles/R-dev/BeetleMods/OUT/var_imp_tests"

data_filter <- "All"
# data_filter <- c("New","Published")      
# data_filter <- "CitizenScience"


##----------------------------------------------------------------------------------- ##


sp_idx <- 3


spNames <- c("Lucanus cervus",
             "Lucanus barbarossa",
             "Dorcus parallelipipedus",
             "Platycerus spinifer")

spAcronym <- c("LC","LB","DP","PS")

dataAcronym <- c("ALL","REF","REF","CTS")
names(dataAcronym) <- c("All","New","Published","CitizenScience")

working_dir <- paste0(working_dir,"/",dataAcronym[data_filter[1]],"_",spAcronym[sp_idx])

if(!dir.exists(working_dir))
  dir.create(working_dir)


##----------------------------------------------------------------------------------- ##


selVars <- readxl::read_excel("./DATA/TABLES/BMods_SelectedVariables-v1.xlsx",1)


ptb <- vect("./DATA/VECTOR/PT_caop2020_bounds_WGS84.shp")


fn <- list.files("./DATA/RASTER/_PT/VARS", 
                 pattern = ".tif$", full.names = TRUE)


modVars <- rast(fn)

names(modVars) <- gsub("_PT","",tools::file_path_sans_ext(basename(fn)))


modVars[["BIO_01"]] <- (modVars[["BIO_01"]] * 0.1) - 273.15
modVars[["BIO_02"]] <- (modVars[["BIO_02"]] * 0.1)
modVars[["BIO_03"]] <- (modVars[["BIO_03"]] * 0.1) 
modVars[["BIO_04"]] <- (modVars[["BIO_04"]] * 0.1)
modVars[["BIO_05"]] <- (modVars[["BIO_05"]] * 0.1) - 273.15
modVars[["BIO_06"]] <- (modVars[["BIO_06"]] * 0.1) - 273.15
modVars[["BIO_07"]] <- (modVars[["BIO_07"]] * 0.1)
modVars[["BIO_08"]] <- (modVars[["BIO_08"]] * 0.1) - 273.15
modVars[["BIO_09"]] <- (modVars[["BIO_09"]] * 0.1) - 273.15
modVars[["BIO_10"]] <- (modVars[["BIO_10"]] * 0.1) - 273.15
modVars[["BIO_11"]] <- (modVars[["BIO_11"]] * 0.1) - 273.15
modVars[["BIO_12"]] <- (modVars[["BIO_12"]] * 0.1) 
modVars[["BIO_13"]] <- (modVars[["BIO_13"]] * 0.1) 
modVars[["BIO_14"]] <- (modVars[["BIO_14"]] * 0.1)
modVars[["BIO_15"]] <- (modVars[["BIO_15"]] * 0.1) 
modVars[["BIO_16"]] <- (modVars[["BIO_16"]] * 0.1)
modVars[["BIO_17"]] <- (modVars[["BIO_17"]] * 0.1) 
modVars[["BIO_18"]] <- (modVars[["BIO_18"]] * 0.1)
modVars[["BIO_19"]] <- (modVars[["BIO_19"]] * 0.1)


##----------------------------------------------------------------------------------- ##


bd <- readxl::read_excel("./DATA/TABLES/BD_VL_ARTIGO2-2023_09_26-v2.xlsx", sheet = "TOTAL_v2") 

bd <- bd %>%  
  select(-Source) %>% 
  rename(Species = Especie) %>% 
  rename(Source = Origem) %>% 
  rename(Year = ANO) %>% 
  rename(Lat = gps_latitude) %>% 
  rename(Lon = gps_longitude) %>% 
  mutate(Lat = as.numeric(Lat)) %>% 
  mutate(Lon = as.numeric(Lon))

bdf <- bd %>% filter(#Geoprivacy == "free", 
                     
                     Lat != "", 
                     !is.na(Lat), 
                     
                     Lon != "", 
                     !is.na(Lon)) %>% 
  filter(Species %in% spNames)


bds <- vect(bdf  %>% 
              select(Species
                     ,Lon
                     ,Lat
                     ,Year
                     ,Source), 
            geom = c("Lon", "Lat"), 
            crs  = "EPSG:4326", keepgeom=FALSE)


bds <- terra::intersect(bds, ptb)


#writeVector(bds, "./TEMP/beetle_data_jgs_v2023_09_26_v2.shp")


# //////////////////////////////////////////////////////////

i = sp_idx

# //////////////////////////////////////////////////////////


projName <- "current"

spName <- spNames[i]
spRespName <- R.utils::toCamelCase(spName)

# selVarsTargetSpecies <- 
# selVars %>% filter(Species == spName, Selected == "x") %>% select(Variables) %>% pull

selVarsTargetSpecies <- paste0("BIO_",str_pad(1:19,2,"left",0))



modVarsDF <- extract(modVars, bds) %>% 
  bind_cols(bds %>% as.data.frame) %>% 
  bind_cols(crds(bds) %>% as.data.frame)



## Filter by species and source ----------------------------------------------------- ##


if(data_filter == "All"){
  modSpDF <- modVarsDF %>% filter(Species == spName)
}else{
  modSpDF <- modVarsDF %>% 
    filter(Species == spName, Source %in% data_filter)
}

##----------------------------------------------------------------------------------- ##


xyData <- modSpDF %>% select(x, y)


extData <- terra::extract(modVars[[1]], xyData, cells=TRUE) 

extData <- extData %>% 
  bind_cols(xyData) %>% 
  group_by(cell) %>% 
  summarize(x = first(x), y = first(y)) %>% 
  select(x, y)

extData <- as.data.frame(extData)

print(nrow(extData))


# //////////////////////////////////////////////////////////

# Change working directory to the one storing biomod2 data

setwd(working_dir)

# //////////////////////////////////////////////////////////


modData <-
  BIOMOD_FormatingData(
    resp.var = rep(1,nrow(extData)),
    resp.name = spRespName,
    expl.var = modVars[[selVarsTargetSpecies]],
    resp.xy = extData,
    PA.nb.rep = 3,
    PA.nb.absences = 10000,
    PA.strategy = "random")


# Define some modelling options for available algorithms
# Define some modelling options for available algorithms
modOptions <- BIOMOD_ModelingOptions(GAM = list(k = 2), ## Change this too!!!!!!!
                                     GBM = list(n.trees = 2000))

modObj <- BIOMOD_Modeling(bm.format       = modData,
                          bm.options      = modOptions,
                          models          = c("GLM", 
                                              "GAM",
                                              "CTA",
                                              "ANN",
                                              "FDA",
                                              "MARS",
                                              "RF", 
                                              "MAXNET", 
                                              "XGBOOST"),
                          CV.strategy        = "kfold",
                          #CV.strategy        = "random",
                          CV.perc            = 0,
                          CV.nb.rep          = 1,
                          CV.k               = 5,
                          CV.do.full.models  = FALSE,
                          prevalence         = 0.5, 
                          var.import         = 5,
                          metric.eval        = c("TSS", "ROC", "KAPPA", "ETS"),
                          scale.models       = FALSE,
                          do.progress        = TRUE)



# Get model evaluation values
myBiomodModelEval <- get_evaluations(modObj)

write.csv(myBiomodModelEval, file = paste(getwd(),"/",spRespName,"/",spRespName,"_evalDF_all.csv",sep=""))

TSS <- myBiomodModelEval %>% filter(metric.eval == "TSS") %>% select(validation) %>% pull
AUC <- myBiomodModelEval %>% filter(metric.eval == "ROC") %>% select(validation) %>% pull

cat("TSS validation average:", round(mean(TSS, na.rm=TRUE), 3))
cat("AUC validation average:", round(mean(AUC, na.rm=TRUE), 3))

cat("TSS validation median:", round(median(TSS, na.rm=TRUE), 3))
cat("AUC validation median:", round(median(AUC, na.rm=TRUE), 3))


TSS_P75 = quantile(TSS, probs=0.75, na.rm=TRUE)
AUC_P75 = quantile(AUC, probs=0.75, na.rm=TRUE)


##----------------------------------------------------------------------------------- ##


# Perform the ensemble of best models previously selected
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = modObj,
                                      #models.chosen = selMods,
                                      em.by = 'all',
                                      metric.select = "TSS",
                                      metric.select.thresh = TSS_P75,
                                      metric.select.dataset = "validation",
                                      em.algo = "EMmean")

# Get evaluation scores for the Ensemble Modeling stage
emEvalDF <- as.data.frame(get_evaluations(myBiomodEM))
write.csv(emEvalDF, file = paste(getwd(),"/",spRespName,"/",spRespName,
                                 "_EnsMod_evalDF_AllMetrics.csv",sep=""))


##----------------------------------------------------------------------------------- ##


selMods <- myBiomodEM@em.models_kept

myBiomodProj <- BIOMOD_Projection(bm.mod = modObj,
                                  new.env         = modVars[[selVarsTargetSpecies]],
                                  proj.name       = projName,
                                  models.chosen   = selMods,
                                  compress        = 'gzip',
                                  build.clamping.mask = TRUE,
                                  on_0_1000           = TRUE,
                                  output.format       = '.tif')


##----------------------------------------------------------------------------------- ##


# Perform the ensembling of projections
myBiomodEF <- BIOMOD_EnsembleForecasting(bm.em         = myBiomodEM,
                                         bm.proj       = myBiomodProj,
                                         metric.binary = 'TSS',
                                         output.format = '.tif',
                                         on_0_1000     = TRUE,
                                         do.stack      = FALSE)


save.image(file = paste(spRespName,"_sessionBkp_v1.RData",sep=""))



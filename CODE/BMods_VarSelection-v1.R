
library(caret)
# library(parallel)
# library(doParallel)
library(biomod2)
library(tidyverse)

setwd("C:/MyFiles/R-dev/BeetleMods")

source("./CODE/BMods_ExtractPointDataDF-v1.R")

# options(future.globals.onReference = "error")
# 
# if(!exists("cl")){
#   cl <- makeCluster(4)  
#   registerDoParallel(cl)
# }


# modVarsSelDF <-
# modVarsDF %>% 
#   select(-SOIL_AWC, -SOIL_BULKDENS, -SOIL_COARSFRAG, -FOR_TREEDENS) %>% 
#   select(Species, x, y, BIO_01:TOPO_TWI) %>% 
#   na.omit()
# 
# print(nrow(modVarsDF))
# print(nrow(modVarsSelDF))
# 
# cm <- cor(modVarsSelDF %>% select(-Species, -x, -y), method="spearman")
# #cor(modVarsSelDF, method="spearman") %>% round(2)
# 
# rmVars <- findCorrelation(cm, cutoff = 0.8, names = TRUE)
# 
# cn <- colnames(modVarsSelDF)
# 
# selVars <- cn[!(cn %in% rmVars)]
# print(selVars)
# 
# nm <- names(modVars)
# nm <- nm[!(nm %in% c("SOIL_AWC", "SOIL_BULKDENS", "SOIL_COARSFRAG", "FOR_TREEDENS"))]


spNames <- c("Dorcus parallelipipedus",
             "Lucanus barbarossa",
             "Lucanus cervus",
             "Platycerus spinifer")



setwd("./OUT/mods/_prelim")


nrecords <- list()

for(i in 1:length(spNames)){

  modSpDF <- modVarsDF %>% filter(Species == spNames[i])
  
  xyData <- modSpDF %>% select(x, y)
  
  extData <- terra::extract(modVars[[1]], xyData, cells=TRUE) 
  
  extData <- extData %>% 
    bind_cols(xyData) %>% 
    group_by(cell) %>% 
    summarize(x = first(x), y = first(y)) %>% 
    select(x, y)
  
  extData <- as.data.frame(extData)
  
  print(nrow(extData))
  
  nrecords[[spNames[i]]] <- nrow(extData)
}



# foreach(i = 1:length(spNames),
#         .packages = c("tidyverse", "biomod2", "terra"),
#         .export = c("modVarsDF", "modVars", "spNames")) %dopar% {
# 

#for(i in 1:length(spNames)){

i = 4
##################


modSpDF <- modVarsDF %>% filter(Species == spNames[i])

xyData <- modSpDF %>% select(x, y)

extData <- terra::extract(modVars[[1]], xyData, cells=TRUE) 

extData <- extData %>% 
  bind_cols(xyData) %>% 
  group_by(cell) %>% 
  summarize(x = first(x), y = first(y)) %>% 
  select(x, y)

extData <- as.data.frame(extData)

print(nrow(extData))

modData <-
  BIOMOD_FormatingData(
    resp.var = rep(1,nrow(extData)),
    resp.name = R.utils::toCamelCase(spNames[i]),
    expl.var = modVars, #[[1:19]],
    resp.xy = extData, #xyData,
    PA.nb.rep = 10,
    PA.nb.absences = 5000,
    PA.strategy = "random")

modObj <- BIOMOD_Modeling(bm.format = modData,
                          models = "RF",
                          nb.rep = 10,
                          data.split.perc = 70,
                          do.full.models = TRUE,
                          var.import = 20,
                          do.progress = TRUE)

save(modData, modObj, file = paste(spNames[i],"_v1.RData",sep=""))

list(spName = spNames[i],
     modData = modData,
     modObj = modObj)
  
#}


  
## ----------------------------------------------------------------------------------- ##
## Get variable importances
## ----------------------------------------------------------------------------------- ##
  
setwd("C:/MyFiles/R-dev/BeetleMods/OUT/mods/_prelim")


corThresh <- 0.8
  
fn <- list.files(pattern = ".RData$", full.names = TRUE)

modVarsDF <- readr::read_csv("../../../DATA/TABLES/modVarsDF_v1.csv")

spNames <- c("Dorcus parallelipipedus",
             "Lucanus barbarossa",
             "Lucanus cervus",
             "Platycerus spinifer")


selectedVars <- list()


for(i in 1:length(fn)){

  rm(modObj)
  
  spName <- spNames[i]
  
  load(fn[i])
  
  vimp <- get_variables_importance(modObj, as.data.frame = TRUE)
  
  modPerf <- get_evaluations(modObj)
  
  avgVarImp <- 
    vimp %>% 
    group_by(Expl.var) %>% 
    summarize(AvgImp = mean(Var.imp),
              StdImp = sd(Var.imp)) %>% 
    arrange(desc(AvgImp))
  
  
  for(j in 1:nrow(avgVarImp)){
    
    if(j==1){
      
      selVars <- levels(avgVarImp[,"Expl.var"] %>% pull)[as.numeric(avgVarImp[j ,"Expl.var"])]
      
    }else{
      
      testVar <- levels(avgVarImp[,"Expl.var"] %>% pull)[as.numeric(avgVarImp[j ,"Expl.var"])]
      varsToUse <- as.character(c(selVars, testVar))
      
      tmpDF <- modVarsDF %>% 
        filter(Species == spName) %>% 
        select(all_of(varsToUse)) %>% 
        na.omit
      
      cm <- cor(tmpDF)
      corValues <- cm[lower.tri(cm)] %>% as.numeric
      
      if(all(abs(corValues) <= corThresh)){
        
        selVars <- c(selVars, testVar)
        
      }
      
    }
    
  }
  
  selectedVars[[spName]] <- selVars
  
  cat("\n\n############################################################\n")
  cat("Sected variables for species:",spName,"\n\n")
  cat(paste(selVars,collapse="\n"),sep="")
  
  tmpSelVarsDF <- data.frame(spName = spName, varName = selVars, rank = 1:length(selVars)) %>% 
    left_join(avgVarImp, by=c("varName"="Expl.var"))
  
  if(i==1){
    outSelVarsDF <- tmpSelVarsDF
  }else{
    outSelVarsDF <- bind_rows(outSelVarsDF,tmpSelVarsDF)
  }
  
}


write.csv(outSelVarsDF, "../outSelVarsDF-v1.csv", row.names = FALSE)





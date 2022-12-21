

library(tidyverse)
library(terra)

setwd("C:/MyFiles/R-dev/BeetleMods")


ptb <- vect("./DATA/VECTOR/PT_caop2020_bounds_WGS84.shp")


fn <- list.files("./DATA/RASTER/_PT/VARS", 
                 pattern = ".tif$", full.names = TRUE)

# print(fn)
# 
# ref <- rast(fn[37])
# 
# 
# for(f in fn){
#   
#   r <- rast(f)
#   testGeom <- compareGeom(ref, r, stopOnError = FALSE)
#   
#   if(!testGeom){
#     
#     print(f)
#     resampRast <-resample(r, ref, method="near")
#     writeRaster(resampRast,paste(tools::file_path_sans_ext(f),"_v2.tif",sep=""), overwrite=TRUE)
#   }
# }


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

#plot(modVars[[1]])


spNames <- c("Lucanus cervus",
             "Lucanus barbarossa",
             "Dorcus parallelipipedus",
             "Platycerus spinifer")


bd <- read_csv("./DATA/TABLES/BD_VL_ARTIGO - TOTAL.csv") %>%  
  select(-Source) %>% 
  rename(Species = Especie) %>% 
  rename(Source = Origem) %>% 
  rename(Year = ANO) %>% 
  rename(Lat = gps_latitude) %>% 
  rename(Lon = gps_longitude)


bdf <- bd %>% filter(Geoprivacy == "free", 
                     
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
            geom=c("Lon", "Lat"), 
            crs="EPSG:4326", keepgeom=FALSE)


bds <- terra::intersect(bds, ptb)

#plot(bds)

modVarsDF <- extract(modVars, bds) %>% 
  bind_cols(bds %>% as.data.frame) %>% 
  bind_cols(crds(bds) %>% as.data.frame)

# 
# countNA <- function(x) sum(is.na(x))
# 
# apply(modVarsDF, 2, countNA) %>% sort
# 
# readr::write_csv(modVarsDF,"./DATA/TABLES/modVarsDF_v1.csv", na = "")
# 

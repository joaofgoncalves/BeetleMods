library(terra)
library(dplyr)

r <- rast("./DATA/RASTER/_PT/_MASK_PT_v5.tif")

fh <- rast("./DATA/RASTER/ForestHeight_2019_PT/ForestHeight_2019_v3_PT.tif")

fh[fh>60] <- NA
writeRaster(fh, "./DATA/RASTER/ForestHeight_2019_PT/ForestHeight_2019_v3_PT.tif")

z <- resample(r, fh, method="near")

# values(r) <- 1:ncell(r)
# 
# writeRaster(r, "C:/Users/JG/Desktop/RASTER_DATA/_PT/_MASK_PT_v2.tif")

#fh_stat <- zonal(fh, z, fun="median", na.rm = TRUE)
fh_stat <- zonal(fh, z, fun="mad", na.rm = TRUE)

fh_stat <- fh_stat %>% 
  rename(idx = `_MASK_PT_v5`)

DFV <- data.frame(idx=values(r)) %>% 
  rename(idx = X_MASK_PT_v5)

DFV <- DFV %>% left_join(fh_stat, by="idx")

fh_stat_rst <- r
values(fh_stat_rst) <- DFV$ForestHeight_2019_v2_PT
writeRaster(fh_stat_rst, "C:/MyFiles/R-dev/BeetleMods/DATA/RASTER/ForestHeight_2019_PT/FH_MAD_PT.tif")


fh_stat <- zonal(fh, z, fun="max", na.rm = TRUE)

fh_stat <- fh_stat %>% 
  rename(idx = `_MASK_PT_v5`)

DFV <- data.frame(idx=values(r)) %>% 
  rename(idx = X_MASK_PT_v5)

DFV <- DFV %>% left_join(fh_stat, by="idx")

fh_stat_rst <- r
values(fh_stat_rst) <- DFV$ForestHeight_2019_v2_PT
writeRaster(fh_stat_rst, "C:/MyFiles/R-dev/BeetleMods/DATA/RASTER/ForestHeight_2019_PT/FH_MAX_PT.tif")



library(terra)
library(readr)
library(sf)
library(dplyr)
library(rnaturalearth)
library(tidyterra)
library(tidyverse)

## ------------------------------------------------------------------------------ ##


spNames <- c("Lucanus cervus",
             "Lucanus barbarossa",
             "Dorcus parallelipipedus",
             "Platycerus spinifer")

spAcronym <- c("LC","LB","DP","PS")

names(spNames) <- spAcronym

dt_sources_names <- c("ALL" = "All sources",
                      "CTS" = "Citizen science",
                      "REF" = "Other sources")


## ------------------------------------------------------------------------------ ##


rst_mods <- list.files("G:/O meu disco/BeetleMods/OUT/BeetleMods_results", 
                       pattern=".tif$", full.names = TRUE, recursive = TRUE)

rst_mods <- rst_mods[!grepl("__old__",rst_mods)]


rst_mods_hbs <- rst_mods[grepl("_mergedAlgo.tif$",rst_mods)]
rst_mods_bin <- rst_mods[grepl("_TSSbin.tif$",rst_mods)]

ptb <- vect("./DATA/VECTOR/PT_caop2020_bounds_WGS84.shp")


##----------------------------------------------------------------------------------- ##


bd <- readxl::read_excel("./DATA/TABLES/BD_VL_ARTIGO2-2023_09_26-v2.xlsx", 
                         sheet = "TOTAL_v2") 

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

bd_sf <- st_as_sf(bds)

unique(bd_sf$Source)


##----------------------------------------------------------------------------------- ##


pb <- txtProgressBar(min = 1, max = length(rst_mods_bin), style = 3)



for(i in 1:length(rst_mods_bin)){
  
  fn <- rst_mods_bin[i]
  fn_hbs <- rst_mods_hbs[i]
  
  rst_mod <- rast(fn)
  rst_mod_hbs <- rast(fn_hbs)
  
  rst_mods_all <- c(rst_mod, rst_mod_hbs)
  
  mod_data <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[1]
  sp_code <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[2]
  sp_name <- spNames[sp_code]
  
  dt_source_name <- dt_sources_names[mod_data]
  
  ## Filter by species and source ---------------------------- ##
  
  if(mod_data == "ALL"){
    sp_data_by_source <- bd_sf %>% filter(Species == sp_name)
  }
  
  if(mod_data == "REF"){
    sp_data_by_source <- bd_sf %>% 
      filter(Species == sp_name, Source %in% c("New","Published") )
  }
  
  if(mod_data == "CTS"){
    sp_data_by_source <- bd_sf %>% 
      filter(Species == sp_name, Source %in% c("CitizenScience") )
  }
  
  ##---------------------------------------------------------- ##
  
  for(j in 1:nlyr(rst_mods_all)){
    
    g <- ggplot() +
      geom_spatraster(data = rst_mods_all[[j]]) + 
      # geom_sf(data = ptb, 
      #         fill = "transparent", 
      #         color = "blue", linewidth = 0.8) +
      geom_sf(data = sp_data_by_source, size = 2, shape = 3, color="white",
              mapping = aes(color=Species)) +
      labs(title = paste("Species: ",sp_name," (",
                         ifelse(j==1,"bin","hbs"),")\nData source: ", 
                         dt_source_name ,sep="")) +
      xlim(c(-9.5, -6)) +
      ylim(c(36.9, 42.2)) +
      theme_bw() + 
      theme(text = element_text(size = 16))
    
    #plot(g)
    if(j==1) out_fn <- paste("./OUT/maps/",sp_name,"_",dt_source_name,"_bin.png",sep="")
    else out_fn <- paste("./OUT/maps/",sp_name,"_",dt_source_name,"_hbs.png",sep="")
    
    ggsave(out_fn, g, width=8, height=11)
  }

  
  setTxtProgressBar(pb, i)
}



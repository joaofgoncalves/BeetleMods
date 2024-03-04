

library(tidyverse)
library(terra)
library(biomod2)
library(biomod2plus)
library(cli)

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

rnd_pts <- spatSample(modVars, size = 20000, as.points=TRUE)

rnd_pts <- terra::intersect(rnd_pts, ptb)

plot(rnd_pts)

rnd_df <- na.omit(as.data.frame(rnd_pts)) %>% select(-Id) %>% 
  mutate(Species="rnd")

sp_names <- unique(bds$Species)


#dp <- bds[bds$Species == "Dorcus parallelipipedus",]
# [1] "Dorcus parallelipipedus"
# [2] "Lucanus barbarossa"     
# [3] "Lucanus cervus"         
# [4] "Platycerus spinifer" 

env_df <- extract(modVars, bds)

sp_data <- bind_cols(as.data.frame(bds), env_df) %>% 
  select(-Year,-Source,-Id,-ID)

all_data <- bind_rows(rnd_df,sp_data)
mod_var_names <- colnames(all_data)[-38]

head(all_data)
i=1
j=1

bioclim_full_names <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "Isothermality (BIO2/BIO7) (×100)",
  "Temperature Seasonality (standard deviation ×100)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range (BIO5-BIO6)",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter"
)



cli_progress_bar("Making plots...",
                 total = length(sp_names)*length(mod_var_names))

for(i in 1:length(sp_names)){
  
  for(j in 1:length(mod_var_names)){
    
    
    sel_var <- mod_var_names[j]
    sel_sp <- sp_names[i]
    
    if(grepl("BIO_",sel_var)){
      vname <- paste(sel_var,"|",bioclim_full_names[j])
    }else{
      vname <- sel_var
    }
    
    filt_data <- all_data %>% 
      filter(Species %in% c("rnd",sel_sp))
    
    summ_data <- filt_data %>% 
      group_by(Species) %>% 
      summarise(med = median(!!sym(sel_var), na.rm=TRUE),
                mad = mad(!!sym(sel_var), na.rm=TRUE)) 
    
    g <- ggplot(filt_data,
                aes(y = !!sym(sel_var), x= Species, fill=Species)) + 
      geom_boxplot() + 
      theme_bw() + 
      scale_y_continuous(breaks = scales::extended_breaks(n = 30))+ 
      xlab("Species data distribution vs.\nrandomly distributed points") +
      ylab(vname) +
      guides(fill = 'none') + 
      labs(title = vname)
    
    #plot(g)
    out_path <- paste("./OUT/eda_boxplots_by_sp/",gsub("\\ +","_",sel_sp),"_boxplot_",
                      sel_var,".png",sep="")
    ggsave(filename = out_path,g, width=5, height = 8)
    
    
    
    g1 <- ggplot(filt_data,
                aes(x = !!sym(sel_var), fill=Species, color=Species)) + 
      #geom_boxplot() +
      geom_density(alpha=0.2, linewidth=1) +
      geom_vline(data = summ_data, aes(xintercept =med, color=Species), 
                  linetype = "dashed", linewidth=0.7) + 
      scale_x_continuous(breaks = scales::extended_breaks(n = 25))+ 
      xlab("Species data distribution vs.randomly distributed points") +
      ylab(vname) +
      labs(title = vname) + 
      theme_bw() + 
      theme(legend.position = "bottom") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      #guides(fill = 'none', color='none')
    
    #plot(g1)
    
    out_path <- paste("./OUT/eda_density-plots_by_sp/",gsub("\\ +","_",sel_sp),"_densplot_",
                      sel_var,".png",sep="")
    
    ggsave(filename = out_path,g1, width=8, height = 5)
    
    
    cli_progress_update()
  }
}






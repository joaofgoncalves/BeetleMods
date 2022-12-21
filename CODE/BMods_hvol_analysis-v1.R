
library(tidyverse)
library(terra)
library(hypervolume)
library(ggVennDiagram)

setwd("C:/MyFiles/R-dev/BeetleMods")


ptb <- vect("./DATA/VECTOR/PT_caop2020_bounds_WGS84.shp")


fn <- list.files("./DATA/RASTER/_PT/VARS/", 
           pattern = ".tif$", full.names = TRUE)

print(fn)


climData <- rast(fn[1:19])

names(climData) <- gsub("_PT","",tools::file_path_sans_ext(basename(fn[1:19])))


climData[["BIO_01"]] <- (climData[["BIO_01"]] * 0.1) - 273.15
climData[["BIO_02"]] <- (climData[["BIO_02"]] * 0.1)
climData[["BIO_03"]] <- (climData[["BIO_03"]] * 0.1) 
climData[["BIO_04"]] <- (climData[["BIO_04"]] * 0.1)
climData[["BIO_05"]] <- (climData[["BIO_05"]] * 0.1) - 273.15
climData[["BIO_06"]] <- (climData[["BIO_06"]] * 0.1) - 273.15
climData[["BIO_07"]] <- (climData[["BIO_07"]] * 0.1)
climData[["BIO_08"]] <- (climData[["BIO_08"]] * 0.1) - 273.15
climData[["BIO_09"]] <- (climData[["BIO_09"]] * 0.1) - 273.15
climData[["BIO_10"]] <- (climData[["BIO_10"]] * 0.1) - 273.15
climData[["BIO_11"]] <- (climData[["BIO_11"]] * 0.1) - 273.15
climData[["BIO_12"]] <- (climData[["BIO_12"]] * 0.1) 
climData[["BIO_13"]] <- (climData[["BIO_13"]] * 0.1) 
climData[["BIO_14"]] <- (climData[["BIO_14"]] * 0.1)
climData[["BIO_15"]] <- (climData[["BIO_15"]] * 0.1) 
climData[["BIO_16"]] <- (climData[["BIO_16"]] * 0.1)
climData[["BIO_17"]] <- (climData[["BIO_17"]] * 0.1) 
climData[["BIO_18"]] <- (climData[["BIO_18"]] * 0.1)
climData[["BIO_19"]] <- (climData[["BIO_19"]] * 0.1)




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
  filter(Species %in% c("Lucanus cervus",
                        "Lucanus barbarossa",
                        "Dorcus parallelipipedus",
                        "Platycerus spinifer"
  ))


bds <- vect(bdf  %>% 
                     select(Species
                           ,Lon
                           ,Lat
                           ,Year
                           ,Source), 
            geom=c("Lon", "Lat"), 
            crs="EPSG:4326", keepgeom=FALSE)


bds <- terra::intersect(bds, ptb)

plot(bds)

bdClim <- extract(climData, bds) %>% 
  bind_cols(bds %>% as.data.frame)


bdSumm <- bdClim %>% 
  group_by(Species, Year) %>% 
  summarize(Count = n())

vnames <- paste("BIO_",str_pad(c(3,4,7, 8,10,12,15,17,18), width = 2,pad = "0",side = "left"),sep="")

cm <- cor(na.omit(bdClim[,vnames], method="spearman"))

print(cm %>% round(2))

excVars <- caret::findCorrelation(cm,cutoff = 0.7)

vnames <- vnames[-excVars]


bdClim_scaled <- 
bind_cols(
  bdClim %>% select(ID, Species:Source),
  bdClim %>% select(all_of(vnames)) %>% scale
)

g <- ggplot(bdSumm,aes(x=Year, y=Count)) + 
  geom_bar(stat="identity") + 
  facet_wrap(Species~.) + 
  scale_y_log10()

plot(g)



## -------------------------------------------------------------------------------- ##



spNames <- c("Lucanus cervus",
             "Lucanus barbarossa",
             "Dorcus parallelipipedus",
             "Platycerus spinifer")


hvolsLists <- list()

i <- 0
for(sp in spNames){
  
  i<-i+1
  
  outDir <- paste(getwd(),"/OUT/hvol_v3/",sp,sep="")
  dir.create(outDir)
  
  bdSubsetRefs <- bdClim_scaled %>% 
    filter(Source != "CitizenScience", Species == sp) %>% 
    select(all_of(vnames)) %>% 
    na.omit
  
  bdSubsetCS <- bdClim_scaled %>% 
    filter(Source == "CitizenScience", Species == sp) %>% 
    select(all_of(vnames)) %>% 
    na.omit
  
  bdSubsetAll <- bdClim_scaled %>% 
    filter(Species == sp) %>% 
    select(all_of(vnames)) %>% 
    na.omit
  
  hvRefs <- hypervolume(bdSubsetRefs, method = "svm", verbose = FALSE, chunk.size = 10000)
  hvCS <- hypervolume(bdSubsetCS, method = "svm", verbose = FALSE, chunk.size = 10000)
  hvAll <- hypervolume(bdSubsetAll, method = "svm", verbose = FALSE, chunk.size = 10000)
  
  

  hvRefs@Name <- paste(sp,"(other sources)")
  hvCS@Name <- paste(sp,"(citizen science)")
  hvAll@Name <- paste(sp,"(all data)")
  
  
  hv_list <- hypervolume_join(hvRefs, hvCS, hvAll)
  
  
  
  saveRDS(hvRefs,file = paste(outDir,"/01_",sp,"_hvRefs_otherSources_v3.rds",sep=""))
  saveRDS(hvCS,file = paste(outDir,"/02_",sp,"_hvCS_citizenSci_v3.rds",sep=""))
  saveRDS(hvAll,file = paste(outDir,"/03_",sp,"_hvAll_allSourcesCombn_v3.rds",sep=""))
  
  tmpDF <- data.frame(spName = sp,
                      DataType = c("Refs","CS","All"),
                      svmHvol = c(hvRefs@Volume,
                               hvCS@Volume,
                               hvAll@Volume),
                      dirPath = outDir)
  
  
  #hvolsLists[[i]] <- to_hv_list(outDir)
  
  png(filename = paste("./OUT/",sp,"_hvols_v2.png",sep=""), width = 2800, height = 2300, res=300)
  plot(hv_list)
  dev.off()
  
  
  hv_set <- hypervolume_set(hvRefs, hvCS, check.memory=FALSE, 
                             num.points.max = 1E6)
  

  
  hv_set3 <-
  
  hypervolume_set_n_intersection(hv_list, num.points.max = 1E6, 
                                 verbose = TRUE, distance.factor = 1, check.hyperplane = FALSE)
  
  
  plot(hv_set)
  
  
  uniall    <- hv_set@HVList$Union@Volume
  int       <- hv_set@HVList$Intersection@Volume / uniall
  fracUniq1 <- hv_set@HVList$Unique_1@Volume / uniall
  fracUniq2 <- hv_set@HVList$Unique_2@Volume / uniall
  
  # hypervolume_overlap_statistics(hv_set)
  
  tl <- 10000
 
  fint <- 1:(round(int*tl))
  len_fint <- length(fint)
  
  f1 <- c(fint,(len_fint + 1):(len_fint + round(fracUniq1*tl)))
  len_f1 <- length(f1)
  
  f2 <- c(fint, (len_f1 + len_fint + 1) : (len_f1 + len_fint + round(fracUniq2*tl)))
  
  ggv <- ggVennDiagram(list(`Other sources` = f1,
                     `Citizen Science` = f2), label = "percent", label_percent_digit=2) + 
    scale_fill_distiller(palette = "Paired") +
    labs(title = "\nVenn diagram of niche hipervolume fractions\n") +
    theme(legend.position = "none") + 
    theme(panel.background = element_rect(fill = 'white', color=NA),
          plot.background = element_rect(fill = 'white', color=NA))
  
  
  ggsave(paste("./OUT/",sp,"_hvols_VennDiagram_v2.png",sep=""), ggv, width = 5, height= 4)
  
  
  if(i==1){
    outDF <- tmpDF
  }else{
    outDF <- rbind(outDF, tmpDF)
  }
  
  cat("Finished..",sp,"....\n\n")

}




## -----------------------------------------------------------------------------------------##

outDF$DataType <- c("Other sources","Citizen science")

outDF$DataType <- factor(outDF$DataType, 
                         levels = c("Other sources","Citizen science"))

g <- ggplot(outDF,
            aes(y=svmHvol, x=DataType, fill=DataType)) + 
  geom_bar(stat="identity") +
  xlab("Data sources") + 
  ylab("Niche hypervolume size") +
  facet_wrap(spName~.) + 
  scale_fill_discrete(name="Data sources") +
  labs(title = "Niche hypervolume size by data source") + 
  theme_bw()
plot(g)

ggsave("./OUT/HVolSize_BeforeAfterCSdatacoll_v2.png",plot = g,width = 8,height = 8)





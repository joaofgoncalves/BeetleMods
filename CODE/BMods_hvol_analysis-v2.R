

library(tidyverse)
library(terra)
library(hypervolume)
library(ggVennDiagram)
library(alphahull)


setwd("C:/MyFiles/R-dev/BeetleMods")


ggVennDiagHvols <- function(hvRefs, hvCS, sp, vs, width = 5, height= 4, tl = 10000){
  
  hv_set <- hypervolume_set(hvRefs, hvCS, check.memory=FALSE, 
                            num.points.max = 1E6)
  

  
  uniall    <- hv_set@HVList$Union@Volume
  int       <- hv_set@HVList$Intersection@Volume / uniall
  fracUniq1 <- hv_set@HVList$Unique_1@Volume / uniall
  fracUniq2 <- hv_set@HVList$Unique_2@Volume / uniall
  
  # hypervolume_overlap_statistics(hv_set)
  
  tl <- tl
  
  fint <- 1:(round(int*tl))
  len_fint <- length(fint)
  
  f1 <- c(fint,(len_fint + 1):(len_fint + round(fracUniq1*tl)))
  len_f1 <- length(f1)
  
  f2 <- c(fint, (len_f1 + len_fint + 1) : (len_f1 + len_fint + round(fracUniq2*tl)))
  
  ggv <- ggVennDiagram(list(`Other sources` = f1,
                            `Citizen Science` = f2), label = "percent", label_percent_digit=2) + 
    scale_fill_distiller(palette = "Paired") +
    labs(title = paste("\n",sp," | Venn diagram - hipervolume fractions\n"),sep="") +
    theme(legend.position = "none") + 
    theme(panel.background = element_rect(fill = 'white', color=NA),
          plot.background = element_rect(fill = 'white', color=NA))
  
  
  ggsave(paste("./OUT/VennDiagram_",sp,"_hvols_v",vs,".png",sep=""), 
         plot = ggv, width = width, height = height)
  
}


biplotHvol <- function(hv, sp, vs, colors = c('#e41a1c','#377eb8','#4daf4a'), 
                       width = 4000, height = 2700, res=300){
  
  png(filename = paste("./OUT/HvolBiplot_",sp,"_hvols_v",vs,".png",sep=""), 
      width = width, height = height, res = res)
  
  plot(hv,
       #colors = c("#EBE300", "#0058FA", "#49A32D"),
       #colors = c("yellow", "dark blue", "dark green"),
       colors = colors,
       point.alpha.min=0.3, 
       point.dark.factor=0.4,
       cex.names=1.5,
       cex.legend=1.5,
       cex.data = 1.5,
       cax.random = 1.1,
       num.points.max.data = 1000, 
       num.points.max.random = 2000)
  dev.off()
  
  
}


## ------------------------------------------------------------------------------ ##


spNames <- c("LC" = "Lucanus cervus",
             "LB" = "Lucanus barbarossa",
             "DP" = "Dorcus parallelipipedus",
             "PS" = "Platycerus spinifer")

dt_sources_names <- c("ALL" = "All sources",
                      "CTS" = "Citizen science",
                      "REF" = "Other sources")


## ------------------------------------------------------------------------------ ##


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


## ------------------------------------------------------------------------------ ##


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


plot(bds)

bdClim <- extract(climData, bds) %>% 
  bind_cols(bds %>% as.data.frame)


## ------------------------------------------------------------------------------ ##


vnames <- paste("BIO_",str_pad(c(3,4,7, 8,10,12,15,17,18), 
                               width = 2,pad = "0",side = "left"),sep="")

cm <- cor(na.omit(bdClim[,vnames], method="spearman"))

print(cm %>% round(2))

excVars <- caret::findCorrelation(cm,cutoff = 0.7)

vnames <- vnames[-excVars]


bdClim_scaled <- 
bind_cols(
  bdClim %>% select(ID, Species:Source),
  bdClim %>% select(all_of(vnames)) %>% scale
)


# bdSumm <- bdClim %>% 
#   group_by(Species, Year) %>% 
#   summarize(Count = n())
#
# g <- ggplot(bdSumm,aes(x=Year, y=Count)) + 
#   geom_bar(stat="identity") + 
#   facet_wrap(Species~.) + 
#   scale_y_log10()
# 
# plot(g)
# 


## -------------------------------------------------------------------------------- ##


hvolsLists <- list()

i <- 0
for(sp in spNames){
  
  i<-i+1
  
  outDir <- paste(getwd(),"/OUT/hvol_v4/",sp,sep="")
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
  
  png(filename = paste("./OUT/",sp,"_hvols_v4.png",sep=""), width = 2800, height = 2300, res=300)
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
  
  
  ggsave(paste("./OUT/",sp,"_hvols_VennDiagram_v4.png",sep=""), ggv, width = 5, height= 4)
  
  
  if(i==1){
    outDF <- tmpDF
  }else{
    outDF <- rbind(outDF, tmpDF)
  }
  
  cat("Finished..",sp,"....\n\n")

}


## -----------------------------------------------------------------------------------------##



sps <- c("Lucanus cervus",
         "Lucanus barbarossa",
         "Dorcus parallelipipedus",
         "Platycerus spinifer")

spNames <- rep(sps, each=3)

dataSources <- rep(c("REFS","CTSC", "ALL"),4)


hv_volumes <- data.frame(spName = spNames, dtSource = dataSources, hvol = NA)


hv_fn <- list.files("./OUT/hvol_v4/", pattern=".rds$", full.names = TRUE, recursive = TRUE)

hypervols <- list()

i <- 0
for(fn in hv_fn){
  i <- i + 1
  
  hv <- readr::read_rds(hv_fn[i])
  hypervols[[spNames[i]]][[dataSources[i]]] <- hv
  
  hv_volumes[i,3] <- hv@Volume
  
}


readr::write_csv(hv_volumes,"./OUT/hv_volumes_BySpecies&dataSource-v1.csv")


## -----------------------------------------------------------------------------------------##


hv_lc <- hypervolume_join(hypervols[[1]][[1]],
                          hypervols[[1]][[2]],
                          hypervols[[1]][[3]])

hv_lb <- hypervolume_join(hypervols[[2]][[1]],
                          hypervols[[2]][[2]],
                          hypervols[[2]][[3]])

hv_dp <- hypervolume_join(hypervols[[3]][[1]],
                          hypervols[[3]][[2]],
                          hypervols[[3]][[3]])

hv_ps <- hypervolume_join(hypervols[[4]][[1]],
                          hypervols[[4]][[2]],
                          hypervols[[4]][[3]])


biplotHvol(hv=hv_lc, sp=sps[1], vs=4, width = 4000, height = 2700, res=300)
biplotHvol(hv=hv_lb, sp=sps[2], vs=4, width = 4000, height = 2700, res=300)
biplotHvol(hv=hv_dp, sp=sps[3], vs=4, width = 4000, height = 2700, res=300)
biplotHvol(hv=hv_ps, sp=sps[4], vs=4, width = 4000, height = 2700, res=300)


ggVennDiagHvols(hypervols[[1]][[1]],
                hypervols[[1]][[2]], 
                sp = sps[1], vs=4, width = 6, height= 4, tl = 10000)
ggVennDiagHvols(hypervols[[2]][[1]],
                hypervols[[2]][[2]], 
                sp = sps[2], vs=4, width = 6, height= 4, tl = 10000)
ggVennDiagHvols(hypervols[[3]][[1]],
                hypervols[[3]][[2]], 
                sp = sps[3], vs=4, width = 6, height= 4, tl = 10000)
ggVennDiagHvols(hypervols[[4]][[1]],
                hypervols[[4]][[2]], 
                sp = sps[4], vs=4, width = 6, height= 4, tl = 10000)


## -----------------------------------------------------------------------------------------##


hv_volumes <- readr::read_csv("./OUT/hv_volumes_BySpecies&dataSource-v1.csv")

hv_all <- hv_volumes %>% filter(dtSource == "ALL") %>% pull(hvol) %>% rep(each=3)

hv_volumes <- hv_volumes %>% mutate(dt_source = dtSource) %>% 
  mutate(dt_source = forcats::fct_recode(dt_source, "All sources" = "ALL",
                                         "Citizen science" = "CTSC",
                                         "Other sources" = "REFS"))

hv_volumes <- hv_volumes %>% mutate(spName = forcats::fct_relevel(as.factor(spName), 
                                                c("Lucanus cervus", "Lucanus barbarossa", 
                                                  "Dorcus parallelipipedus", "Platycerus spinifer"))) %>% 
  mutate(spName = forcats::fct_recode(spName, 
                                         "L. cervus"           = "Lucanus cervus", 
                                         "L. barbarossa"       = "Lucanus barbarossa", 
                                         "D. parallelipipedus" = "Dorcus parallelipipedus", 
                                         "P. spinifer"         = "Platycerus spinifer")) %>% 
  mutate(hvol_perc = (hvol / hv_all) * 100)


## -----------------------------------------------------------------------------------------##


g <- ggplot(hv_volumes,
            aes(y=hvol, x=dt_source, fill=dt_source)) + 
  geom_bar(stat="identity", color="black", alpha=0.65, width=0.7) +
  xlab("Data sources") + 
  ylab("Niche hypervolume size") +
  facet_wrap(spName~.) + 
  #labs(title = "Niche hypervolume size by data source") + 
  scale_fill_manual(values = c('#4daf4a', '#377eb8','#e41a1c'), name="") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
plot(g)

ggsave("./OUT/HVolSize_BeforeAfterCSdatacoll_v4.png",plot = g,width = 8,height = 8)



g1 <- ggplot(hv_volumes,
            aes(y=hvol_perc, x=dt_source, fill=dt_source)) + 
  geom_bar(stat="identity", color="black", alpha=0.65, width=0.7) +
  xlab("Data sources") + 
  ylab("Niche hypervolume size (%)") +
  facet_wrap(spName~.) + 
  #labs(title = "Niche hypervolume size by data source") + 
  scale_fill_manual(values = c('#4daf4a', '#377eb8','#e41a1c'), name="") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
plot(g1)

ggsave("./OUT/HVolSize_percentage_BeforeAfterCSdatacoll_v4.png",plot = g1,width = 8,height = 8)



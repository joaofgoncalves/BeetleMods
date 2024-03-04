
library(terra)
library(tidyverse)



## ------------------------------------------------------------------------------ ##


spNames <- c("LC" = "Lucanus cervus",
             "LB" = "Lucanus barbarossa",
             "DP" = "Dorcus parallelipipedus",
             "PS" = "Platycerus spinifer")

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


pb <- txtProgressBar(min = 1, max = length(rst_mods_bin), style = 3)



for(i in 1:length(rst_mods_bin)){
  
  fn <- rst_mods_bin[i]

  rst_mod <- rast(fn) %>% project("epsg:32629", method="near", res=850)

  mod_data <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[1]
  sp_code <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[2]
  sp_name <- spNames[sp_code]
  
  dt_source_name <- dt_sources_names[mod_data]
  
  
  suit_hab <- values(rst_mod) %>% na.omit %>% sum
  suit_hab <- (suit_hab * (850^2)) / 1E6
  
  tmp <- data.frame(sp_name      = sp_name, 
             dt_source    = dt_source_name, 
             suit_hab_km2 = suit_hab)
  
  if(i==1){
    suit_hab_stats <- tmp
    
  }else{
    suit_hab_stats <- rbind(suit_hab_stats, tmp)
  }
  
  setTxtProgressBar(pb, i)
}


##----------------------------------------------------------------------------------- ##

g1 <- ggplot(suit_hab_stats, 
             aes(x=dt_source, y=suit_hab_km2, fill = dt_source)) + 
  geom_bar(stat = "identity", alpha=0.7, color="black", width=0.6) + 
  facet_wrap(sp_name ~ .) + 
  xlab("Data source") + 
  ylab("Potential suitable habitat (Km²)") +
  scale_fill_manual(values = c('#4daf4a', '#377eb8','#e41a1c'), name="") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

plot(g1)

ggsave(filename = "./OUT/SuitableHabitat-bySource-vs-Species-v3.png",
       plot = g1, width=7, height=7) 


write.csv(suit_hab_stats, "./OUT/BMods_suit_hab_stats-v1.csv", row.names = FALSE)



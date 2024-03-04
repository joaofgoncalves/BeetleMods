

library(tidyverse)
library(ggplot2)
library(dplyr)

## ------------------------------------------------------------------------------ ##

spNames <- c("Lucanus cervus",
             "Lucanus barbarossa",
             "Dorcus parallelipipedus",
             "Platycerus spinifer")

spAcronym <- c("LC","LB","DP","PS")

names(spNames) <- spAcronym

## ------------------------------------------------------------------------------ ##

fl <- list.files("G:/O meu disco/BeetleMods/OUT/BeetleMods_results", 
           pattern = "_EnsMod_evalDF_AllMetrics.csv$", recursive = TRUE, full.names = TRUE)

fl <- fl[!grepl("__old__", fl)]

## ------------------------------------------------------------------------------ ##

pb <- txtProgressBar(min = 1, max = length(fl), style = 3)

for(i in 1:length(fl)){
  
  fn <- fl[i]
  
  mod_data <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[1]
  sp_code <- unlist(strsplit(unlist(strsplit(fn,"/"))[6], "_"))[2]
  sp_name <- spNames[sp_code]
  
  tmp <- suppressMessages(read_csv(fn, progress = FALSE))

  tmp <- tmp %>% 
    mutate(mod_data = mod_data) %>% 
    mutate(sp_name = sp_name)

  
  if(i==1){
    mod_perf <- tmp
  }else{
    mod_perf <- rbind(mod_perf, tmp)
  }
  
  setTxtProgressBar(pb, i)
}


## ------------------------------------------------------------------------------ ##

mod_perf <- mod_perf %>% 
  mutate(dt_source = mod_data) %>% 
  mutate(dt_source = forcats::fct_recode(dt_source, 
                                         "All sources"     = "ALL",
                                         "Citizen science" = "CTS",
                                         "Other sources"   = "REF"))

mod_perf <- mod_perf %>% 
  mutate(sp_short_names = forcats::fct_recode(sp_name, 
                                              "L. cervus"           = "Lucanus cervus", 
                                              "L. barbarossa"       = "Lucanus barbarossa", 
                                              "D. parallelipipedus" = "Dorcus parallelipipedus", 
                                              "P. spinifer"         = "Platycerus spinifer"))

## ------------------------------------------------------------------------------ ##



write_csv(mod_perf,"./OUT/ensemble_model_performances_biomod2-v3.csv")

write_rds(mod_perf,"./OUT/ensemble_model_performances_biomod2-v3.rds")


## ------------------------------------------------------------------------------ ##


g1 <- ggplot(mod_perf %>% filter(metric.eval %in% c("TSS")), 
             aes(x=dt_source, y=calibration, fill = dt_source)) + 
  geom_bar(stat = "identity", alpha=0.7, color="black", width=0.6) + 
  facet_wrap(sp_short_names ~ .) + 
  xlab("Data source") + 
  ylab("TSS value") +
  #scale_fill_brewer(palette = "Set1", name="Data source") +
  scale_fill_manual(values = c('#4daf4a', '#377eb8','#e41a1c'), name="") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

plot(g1)


g2 <- ggplot(mod_perf %>% filter(metric.eval %in% c("ROC")), 
             aes(x=dt_source, y=calibration, fill=dt_source)) + 
  geom_bar(stat = "identity", alpha=0.7, color="black", width=0.6) + 
  facet_wrap(sp_short_names ~ .) +
  xlab("Data source") + 
  ylab("AUC value") +
  #scale_fill_brewer(palette = "Set1", name="Data source") +
  scale_fill_manual(values = c('#4daf4a', '#377eb8','#e41a1c'), name="") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

plot(g2)



ggsave(filename = "./OUT/ModelPerformance-TSS-bySource-vs-Species-v3.png",
       plot = g1, width=7, height=7) 
ggsave(filename = "./OUT/ModelPerformance-AUC-bySource-vs-Species-v3.png",
       plot = g2, width=7, height=7)  
























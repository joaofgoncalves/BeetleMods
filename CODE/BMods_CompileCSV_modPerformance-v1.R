

fl <- list.files("./OUT/mod_perf", pattern = ".csv$", recursive = TRUE, full.names = TRUE)

modDataSource <- rep(c("ALL","CTSC","REFS"), each=4)

colNames <- c("Metric", "value","cutoff","sensitivity","specificity")


spNames <- rep(c("D. parallelipipedus",
             "L. barbarossa",
             "L. cervus",
             "P. spinifer"), 3)

i<-0
for(fn in fl){
  i<-i+1
  csv <- readr::read_csv(fn)[,1:5]
  colnames(csv) <- colNames
 
  if(i==1)
    modPerfs <- data.frame(data_source = modDataSource[i], spNames = spNames[i], csv)
  else
    modPerfs <- rbind(modPerfs, data.frame(data_source = modDataSource[i], spNames = spNames[i], csv))
  
}


readr::write_csv(modPerfs,"./OUT/beetleMods_modelPerformanceByDataset-v1.csv")



library(ggplot2)
library(dplyr)

g1 <- ggplot(modPerfs %>% filter(Metric %in% c("TSS")), 
            aes(x=modDataSource, y=value, fill=modDataSource)) + 
  geom_bar(stat = "identity", alpha=0.65, color="black", width=0.6) + 
  facet_wrap(spNames ~ .) + 
  xlab("Data source") + 
  ylab("TSS value") +
  scale_fill_brewer(palette = "Set1", name="Data source") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16))

plot(g1)

g2 <- ggplot(modPerfs %>% filter(Metric %in% c("ROC")), 
             aes(x=modDataSource, y=value, fill=modDataSource)) + 
  geom_bar(stat = "identity", alpha=0.65, color="black", width=0.6) + 
  facet_wrap(spNames ~ .) +
  xlab("Data source") + 
  ylab("AUC value") +
  scale_fill_brewer(palette = "Set1", name="Data source") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 16))

plot(g2)

ggsave(filename = "./OUT/ModelPerformance-TSS-bySource-vs-Species.png",
       plot = g1, width=7, height=7) 
ggsave(filename = "./OUT/ModelPerformance-AUC-bySource-vs-Species.png",
       plot = g2, width=7, height=7)  



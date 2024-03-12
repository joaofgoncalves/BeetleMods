

library(terra)
library(biomod2)
library(cli)
library(tidyverse)
library(ggpubr)

source("C:/MyFiles/R-dev/BeetleMods/CODE/BMods_ResponseCurves-AuxFuns-v1.R")


bioclim_full_names <- c(
  "Annual Mean Temperature",
  "Mean Diurnal Range (monthly avg (mx_t - mn_t))",
  "Isothermality (BIO2/BIO7) (×100)",
  "Temperature Seasonality (std×100)",
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
  "Precipitation Seasonality (coef. variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter"
)

names(bioclim_full_names) <- paste0("BIO_",c(paste0("0",1:9), paste0(10:19)))


## ---------------------------------------------------------------------------- ##


setwd("Z:/R-dev/BeetleMods/OUT/mods_v2")


fl_eval <- list.files(pattern="_evalDF_all.csv", recursive = TRUE, full.names = TRUE)[1:4]

for(i in 1:4){
  
  tmp <- suppressMessages(read_csv(fl_eval[i]))
  if(i==1){
    evalDF <- tmp
  }else{
    evalDF <- bind_rows(evalDF,tmp)
  }
}

sum_eval_TSS <- evalDF %>% 
  mutate(sp_name = format_sp_name(get_sp_name(full.name))) %>% 
  filter(metric.eval == "TSS") %>% 
  group_by(sp_name, algo) %>% 
  summarize(avg = mean(validation),
            med = median(validation),
            std = sd(validation),
            mad = mad(validation)) %>% 
  arrange(sp_name,desc(avg)) %>% 
  group_by(sp_name) %>% 
  slice(1)

sum_eval_ROC <-evalDF %>% 
  mutate(sp_name = format_sp_name(get_sp_name(full.name))) %>% 
  filter(metric.eval == "ROC") %>% 
  group_by(sp_name, algo) %>% 
  summarize(avg = mean(validation),
            med = median(validation),
            std = sd(validation),
            mad = mad(validation)) %>% 
  arrange(sp_name,desc(avg)) %>% 
  group_by(sp_name) %>% 
  slice(1)

print(sum_eval_TSS)
print(sum_eval_ROC)

## ---------------------------------------------------------------------------- ##
## ---------------------------------------------------------------------------- ##


setwd("Z:/R-dev/BeetleMods/OUT/mods_v2")

fl <- list.files("Z:/R-dev/BeetleMods/OUT/mods_v2",
                 pattern="_sessionBkp_v1.RData", recursive = TRUE, full.names = TRUE)[1:4]

for(sel_mod in c("MAXNET","RF")){
  
  for(i in 1:4){
    
    #rm(list = ls())
    
    sp_name <- get_sp_name(basename(fl[i]))
    sp_name_ <- format_sp_name(sp_name)
    
    setwd(go_up_filepath(fl[i], 1))
    
    suppressMessages(load(fl[i]))
    
    mods <- get_built_models(modObj, algo = sel_mod)
    
    resp_plot_dt <- bm_PlotResponseCurves(bm.out = modObj, 
                                          models.chosen = mods,
                                          fixed.var = 'mean',
                                          do.bivariate = FALSE,
                                          do.plot = FALSE)
    
    
    rp <- resp_plot_dt$tab %>% 
      mutate(sp_name = get_sp_name(as.character(pred.name)),
              algo_name = get_algo_name(as.character(pred.name))) %>% 
      group_by(sp_name,
               algo_name,
               expl.name, id) %>% 
      summarize(x_val = first(expl.val), 
                y_avg = mean(pred.val),
                y_std = sd(pred.val))
    
    
    var_names <- unique(rp$expl.name)
    
    for(j in 1:length(var_names)){
      
      
      sel_var <- as.character(var_names[j])
      
      if(grepl("BIO_",sel_var)){
        vname <- paste(sel_var,"|",bioclim_full_names[sel_var])
      }else{
        vname <- sel_var
      }
      
      
      g <- ggplot(rp %>% filter(expl.name == sel_var), aes(x=x_val, y=y_avg)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = y_avg - 0.5*y_std, 
                        ymax =  y_avg + 0.5*y_std),alpha = 0.3) +
        xlab(vname) + 
        ylab("Response/Habitat suitability") + 
        # labs(title = sp_name_,
        #      subtitle = "Model: " + sel_mod + " | Predictor: " + var_names[j]) +
        theme_bw() + 
        theme(text = element_text(size=17))
      
        
      #plot(g)
      
      fname <- "C:/MyFiles/R-dev/BeetleMods/OUT/response_plots/" + 
        sp_name_ + "_" + as.character(var_names[j]) + "_" + sel_mod + ".png"
      
      ggsave(fname, g, width=6, height=6)
      
      
      cat("Finished", sel_mod, sp_name_,as.character(var_names[j]),"\n\n",sep=" | ")
    }
  }
}






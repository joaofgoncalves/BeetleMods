
library(dplyr)


##' Iterative variable selection based on Random Forests
##' 
##' A two-step variable selection algorithm based on RF importance and correlation
##' 
##' @param varImp A two column data frame with:
##'  -- "varName" - character/string column with the name of each variable
##'  -- "importance" - the importance values calculated from random forest importance function 
##'  or similar one (Note: use the same names for columns as these ones!)
##'  
##'  @param modData The modelling dataset with all predictive variables
##'  
##'  @param corThresh The threshold for correlation used to exclude variables 
##'  (in absolute value, default: 0.75)
##'  
##'  @param method Correlation method (default: "spearman")
##'  


iterativeRFvarSel <- function(varImp, modData, corThresh = 0.75, method = "spearman"){
  
  # Sort the importance data frame by decreasing order
  varImp <- varImp %>% 
    arrange(desc(importance)) %>% 
    as.data.frame
  
  # Iterate through all variables

  for(j in 1:nrow(varImp)){
    
    if(j==1){# Add the "most" important variable with maximum importance at round #1
      
      selVars <- varImp[j, "varName"]
      
    }else{# Test the remaining variables ...
      
      
      # Active variable to be tested at this iteration
      testVar <- varImp[j, "varName"]
      
      # Variables used to calculate the correlation
      varsToUse <- as.character(c(selVars, testVar))
      
      # Extract a temp dataset with previously selected variables plus the one being tested
      tmpDF <- modData %>% 
        select(all_of(varsToUse)) %>% 
        na.omit
      
      # Calculate the correlation matrix
      cm <- cor(tmpDF, method = method)
      
      # Extract the lower triangular matrix from the correlation matrix and
      # convert it to a numeric vector of correlation values to allow checking
      # against the threshold
      corValues <- cm[lower.tri(cm)] %>% as.numeric
      
      # If all variables have correlations below the absolute value of corThresh
      # then add the new variable to the pool
      
      if(all(abs(corValues) <= corThresh)){
        
        selVars <- c(selVars, testVar)
        
      }
    }
  }
  return(selVars)
}






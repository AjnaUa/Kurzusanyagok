sample_i <- function (var , n = 100 , i = 100 , method = "IID") {
  
  library (tidyverse) 
  
  if (method == "IID") {
    
    samples <- map_dfc (1:i , ~ sample (var , n , replace = T))    
    
  } else {
    
    samples <- map_dfc (1:i , ~ sample (var , n , replace = F))
    
  }

  return (samples)
  
}

sample_i <- function (var , n = 100 , i = 100 , method = "IID") {
  
  library (tidyverse) 
  
  if (method == "IID") {
    
    samples <- map_dfc (1:i , ~ sample (var , n , replace = T))    
    
  } else {
    
    samples <- map_dfc (1:i , ~ sample (var , n , replace = F))
    
  }

  samples <- samples %>% 
    mutate (rn = str_c ("element_" , 1:n)) %>% 
    add_rownames (rn)
  colnames (samples) <- str_c ("sample_" , 1:i)
  
  return (samples)
  
}

norm_function <- function (mu = 0 , sigma = 1 , z = mu) {
  
  # normal distribution DF and CDF
  x <- seq (mu - 3 * sigma , mu + 3 * sigma , length.out = 100)
  fx <- dnorm (x , mu , sigma)
  Fx <- pnorm (x , mu , sigma)
  
  dz <- data.frame (variable = x , 
                    DF = fx,
                    CDF = Fx)
  p <- round (pnorm (z , mean = mu , sd = sigma) , 3)
  
  library (ggplot2)
  g1 <- ggplot (dz , aes (x = variable , y = DF)) +
    geom_path () +
    geom_area (mapping = aes (x = ifelse (x < z , x, 0)), fill = "red") +
    geom_vline(xintercept = z , lty = 2) +
    annotate ("text" , label = str_c ("z = " , z) ,
              x = z , y = 0.1) +
    scale_y_continuous (limits = c (0 , 0.6)) +
    theme_classic()
  g2 <- ggplot (dz , aes (x = variable , y = CDF)) +
    geom_path ()+
    geom_vline(xintercept = z , lty = 2) +
    geom_hline(yintercept = p, col = "red") +
    annotate ("text" , label = str_c ("p = " , p) ,
              x = z , y = p + 0.15) +
    theme_classic()
  
  library (patchwork)
  g1 /g2
  
}

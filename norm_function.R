norm_function <- function (mu, sigma , z = mu) {
  
  x <- seq (mu - 3 * sigma , mu + 3 * sigma , length.out = 100)
  fx <- dnorm (x , mu , sigma)
  Fx <- pnorm (x , mu , sigma)
  
  dz <- data.frame (variable = x , 
                    DF = fx,
                    CDF = Fx)
  
  g1 <- ggplot (dz) +
    geom_path (aes (x = variable , y = DF)) +
    geom_vline(xintercept = z , lty = 2) +
    theme_classic()
  g2 <- ggplot (dz , aes (x = variable , y = CDF)) +
    geom_path ()+
    geom_vline(xintercept = z , lty = 2) +
    geom_hline(yintercept = pnorm (z , mean = mu , sd = sigma)) +
    theme_classic()
  
  library (patchwork)
  g1 /g2
  
}
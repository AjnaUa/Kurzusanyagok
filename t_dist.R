t_dist <- function (df) {
  
  t <- rt (n = 1000 , df = df)

  sk <- round (psych::describe (t)$skew, 1)
  ku <- round (psych::describe (t)$kurtosis, 1)
  
  data <- data.frame (x = seq (-3 , 3 , 0.01))
  
  ggplot (data , aes (x = x)) +
    stat_function (fun = dnorm , args = list (mean = 0 , sd = 1)) +
    stat_function (fun = dt , args = list (df = df), col = "blue") +
    annotate (geom = "text" , x = -0.15 , y = 0.2 , 
              label = paste ("df = " , df )) +
    annotate (geom = "text" , x = -0.15 , y = 0.15 , 
              label = paste ("skew = " , sk )) +
    annotate (geom = "text" , x = -0.15 , y = 0.1 , 
              label = paste ("kurt = " , ku )) +
    labs (y = "probability",
          title = "comparint DF of t distribution with standard normal distribution") +
    theme_classic()
  
}

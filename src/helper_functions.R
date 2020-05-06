generate_population <- function(n, prob){
  rand <- runif(n)
  population <- as.integer(rand <= prob)
  
  return(population)
}

# Function to transfrom dates to year.week 
yearweek <- function(x = Sys.Date()) {
  xday <- ISOdate(year(x), month(x), day(x), tz=tz(x))
  dn <- 1 + (wday(x) + 5)%%7
  nth <- xday + ddays(4 - dn)
  jan1 <- ISOdate(year(nth), 1, 1, tz=tz(x))
  return(sprintf("%s.%d", format(nth, "%Y"), 1 + (nth - jan1)%/%ddays(7)))
}

cost_surface <- function(p, price_ratio){
  
  # Cut FPR at 100x%
  x = 0.5
  
  # Set axis 
  tp.axis <- seq(0, (p-p/100), p/100) # TPs go from 0 to p
  fp.axis <- seq(0, (1-p)*x - ((1-p)*x)/100, ((1-p)*x)/100) # FPs need to be limited: they explode the cost (due to reinvites) approaching 1-p
  
  # Set grid
  cost <- matrix(, nrow = length(tp.axis), ncol = length(fp.axis))
  
  tpi <- 0
  for(Rtp in tp.axis){
    tpi <- tpi + 1
    fpi <- 0
    for(Rfp in fp.axis){
      fpi <- fpi + 1
      E <- ((1/(1 - p - Rfp)) - (1/(1 - p))) + price_ratio * (((p - Rtp)/(1 - p - Rfp)) - (p/(1 - p)))
      cost[tpi, fpi] <- E
    }
  }
  return(cost)
}

plot_surface <- function(surface, price_ratio, p){
  x <- seq(0, (50-0.1), 0.5)
  y <- seq(0, 99, 1)
  data <- expand.grid(FPR=x, TPR=y)
  data$Cost.effect <- as.vector(t(surface))
  
  ggplot(data, aes(FPR, TPR, fill = Cost.effect)) + 
    geom_tile() + 
    
    labs(title = "Cost Effect of Classifier in ROC Space",
         subtitle = paste0("Using a price ratio of ", price_ratio, " and positive prevalence of ", p, ".")) +
    geom_abline(intercept = 0, slope = 1, color="white") + 
    theme_minimal() 
}

plot_interactive_surface <- function(surface){
  p <- plot_ly(z = surface, type = "contour", contours=list(coloring = "heatmap"))
  p <- p %>% layout(xaxis = list(title = "FPR"), yaxis = list(title = "TPR"), title="Cost surface in ROC space")
  p
}
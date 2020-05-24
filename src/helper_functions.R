generate_pool_series <- function(num.levels, props, pool_size, days, exclusion.levels){
  if(length(props) != num.levels){
    stop(paste0("Length of proportions list does not match given number of levels: ", length(props)," != ", num.levels))
  }
  
  levels <- 1:num.levels
  elig.probs <- 1/exclusion.levels
  
  # Initialize population based on levels
  population <- sample(levels, pool_size, replace = TRUE, prob = props)
  
  # Initialize series
  init.days <- c()
  for(donor in population){
    days.until <- sample(0:(exclusion.levels[donor]-1), 1)
    init.days <- c(init.days, days.until)
  }
  
  # Initialize time series grid
  pool.series.matrix <- matrix(, nrow = pool_size, ncol = days)
  
  # Construct series
  i <- 0
  for(donor.time in init.days){
    i <- i + 1
    donor.init <- rep(0, donor.time)
    level.cycle <- c(rep(0, exclusion.levels[population[i]]), c(1))
    if(days <= length(level.cycle)){
      donor.series <- c(donor.init, level.cycle)[1:days]
    } else{
      donor.series <- c(donor.init, rep(level.cycle, ceiling(days/length(level.cycle))))[1:days]
    }
    pool.series.matrix[i, ] <- donor.series
  }
  return(pool.series.matrix)
}

# Function to transfrom dates to year.week 
yearweek <- function(x = Sys.Date()) {
  xday <- ISOdate(year(x), month(x), day(x), tz=tz(x))
  dn <- 1 + (wday(x) + 5)%%7
  nth <- xday + ddays(4 - dn)
  jan1 <- ISOdate(year(nth), 1, 1, tz=tz(x))
  return(sprintf("%s.%d", format(nth, "%Y"), 1 + (nth - jan1)%/%ddays(7)))
}

savings_function <- function(p, q, response.rate, invite.price, deferral.price){
  # This is for the current implementation: we are only aware of savings
  cost <- (1/(1 - q - response.rate + q*response.rate) - 
             1/(1 - p - response.rate + p*response.rate))*invite.price + 
          (q/(1 - q) - p/(1 - p))*deferral.price
  return(cost)
}

savings_effect <- function(p, response.rate, invite.price, deferral.price){
  
  TPR <- seq(0, 1, 0.01) # TRUE POSITIVE RATE
  q.axis <- p - TPR*p
  
  # Savings effect
  E <- savings_function(p, q.axis, response.rate, invite.price, deferral.price)
  
  return(E)
}

cost_surface <- function(p, price_ratio){
  # DEPRECATED!!! WORKS BUT IS NOT CORRECT. DO NOT USE.
  
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
  # DEPRECATED!!! SEE cost_surface()!!
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
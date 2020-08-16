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


filter_donations <- function(data, 
                             donor=NULL, donation13=NULL, site=NULL,
                             date=NULL, phleb_start=NULL, status=NULL,
                             donat_phleb=NULL, Hb=NULL, gender=NULL,
                             dob=NULL, aborh=NULL, age=NULL,
                             agegroup=NULL, Hb_deferral=NULL){
  temp <- data
  if(!is.null(donor)){
    temp <- temp[temp$donor==donor,]
  }
  if(!is.null(donation13)){
    temp <- temp[temp$donation13==donation13,]
  }
  if(!is.null(site)){
    temp <- temp[temp$site==site,]
  }
  if(!is.null(date)){
    temp <- temp[temp$date > date[1] & temp$date < date[1],]
  }
  if(!is.null(phleb_start)){
    temp <- temp[temp$phleb_start==phleb_start,]
  }
  if(!is.null(status)){
    temp <- temp[temp$status==status,]
  }
  if(!is.null(donat_phleb)){
    temp <- temp[temp$donat_phleb==donat_phleb,]
  }
  if(!is.null(Hb)){
    temp <- temp[temp$Hb > Hb[1] & temp$Hb < Hb[2],]
  }
  if(!is.null(gender)){
    temp <- temp[temp$gender==gender,]
  }
  if(!is.null(site)){
    temp <- temp[temp$site==site,]
  }
  if(!is.null(dob)){
    temp <- temp[temp$dob > dob[1] & temp$dob < dob[2],]
  }
  if(!is.null(aborh)){
    temp <- temp[temp$aborh==aborh,]
  }
  if(!is.null(age)){
    temp <- temp[temp$age > age[1] & temp$age < age[2],]
  }
  if(!is.null(agegroup)){
    temp <- temp[temp$agegroup==agegroup,]
  }
  if(!is.null(Hb_deferral)){
    temp <- temp[temp$Hb_deferral==Hb_deferral,]
  }
  
  return(temp)
}
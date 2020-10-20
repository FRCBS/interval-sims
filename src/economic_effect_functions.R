#!/usr/bin/env Rscript

library(tidyverse)

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

get_cost_constants <- function(data) {
  data <- data %>% mutate(date=dateonly, status="-")
  
  # All successful whole blood donations
  dons <- filter_donations(data, donat_phleb = "K", status = "-")
  # Order donations by date
  dons <- dons[with(dons, order(date)), ]
  # Find only returning donors
  rdons <- dons[duplicated(dons$donor),]
  
  # Successful male
  mdon <- filter_donations(data = rdons, donat_phleb = "K", status = "-", gender = "Men")
  # Successful female
  fdon <- filter_donations(data = rdons, donat_phleb = "K", status = "-", gender = "Women")
  # Deferrals
  def <- filter_donations(data = data, donat_phleb = "*", Hb_deferral = 1)
  mdef <- filter_donations(data = def, gender = "Men")
  fdef <- filter_donations(data = def, gender = "Women")
  
  # Windows
  walldons <- subset(dons, date >= "2018-01-01" & date <= "2019-12-30")
  wdons <- subset(rdons, date >= "2018-01-01" & date <= "2019-12-30")
  wmdon <- subset(mdon, date >= "2018-01-01" & date <= "2019-12-30")
  wfdon <- subset(fdon, date >= "2018-01-01" & date <= "2019-12-30")
  wdef <- subset(def, date >= "2018-01-01" & date <= "2019-12-30")
  wmdef <- subset(mdef, date >= "2018-01-01" & date <= "2019-12-30")
  wfdef <- subset(fdef, date >= "2018-01-01" & date <= "2019-12-30")
  
  # Male ratio
  mr <- nrow(wmdon)/nrow(wdons)
  # Female ratio
  fr <- nrow(wfdon)/nrow(wdons)
  # Male deferral ratio
  mdr <- nrow(wmdef) / nrow(wdef)
  # Female deferral ratio
  fdr <- nrow(wfdef) / nrow(wdef)
  # Total deferral prevalence
  d <- nrow(wdef)/(nrow(wdef) + nrow(walldons))
  return(list(
    mr=mr,
    fr=fr,
    mdr=mdr,
    fdr=fdr,
    d=d
  ))
}

# These are from the random forest
get_mikkos_cost_constants <- function() {
  return(list(
    mr=0.4732352,
    fr=0.5267648,
    mdr=0.2335795,
    fdr=0.7664205,
    d=0.03269718
  ))
  
}

get_cost <- function(TPR, FPR,d.=d, mr.=mr,fr.=fr,mdr.=mdr,fdr.=fdr,Pm.=Pm,Pd.=Pd,Fn.=Fn,rl.=rl) {
  q <- d. * TPR
  a6 <- 1 * ((1 - FPR) * (1 - d.)) + 
    3 * mr. * (FPR * (1 - d.)) + 2 * fr. * (FPR * (1 - d.)) + 
    3 * mdr. * d. + 2 * fdr. * d.
  
  a12 <- 1 * ((1 - FPR) * (1 - d.)) + 
    6 * mr. * (FPR * (1 - d.)) + 4 * fr. * (FPR * (1 - d.)) + 
    6 * mdr. * d. + 4 * fdr. * d.
  
  # a6
  Fratio <- a6 / (1 + Fn. * (a6 - 1))
  Em <- Pm. * (Fratio - 1 - q * rl.)
  Ed <- Pd. * q
  E6 <- Em - Ed
  
  # a12
  Fratio <- a12 / (1 + Fn. * (a12 - 1))
  Em <- Pm. * (Fratio - 1 - q * rl.)
  Ed <- Pd. * q
  E12 <- Em - Ed
  tibble(q=q, a6=a6, a12=a12, E6=E6, E12=E12, TPR=TPR, FPR=FPR)
}

get_linear_progesa_rates <- function() {
  dir="~/FRCBS/interval_prediction/data/raw_results/"
  mfile <- "raw_result_male_icp_date-2020-08-18-gender-male-sample_fraction-1.0-method-icp-fix-hlen-7-iterations-2400-extra_id-progesa-test.rdata"
  ffile <- "raw_result_female_icp_date-2020-08-12-gender-female-sample_fraction-1.0-method-icp-fix-hlen-7-extra_id-progesa-test.rdata"
  male <- load_single(paste0(dir, mfile))$comp_df
  female <- load_single(paste0(dir, ffile))$comp_df
  both <- bind_rows(male, female)
  positives <- both %>% filter(deferral==1) %>% nrow()
  negatives <- both %>% filter(deferral==0) %>% nrow()
  true_positives <- both %>% filter(predicted_labels==1 & deferral==1) %>% nrow()
  false_positives <- both %>% filter(predicted_labels==1 & deferral==0) %>% nrow()
  TPR <- true_positives / positives
  FPR <- false_positives / negatives
  return(list(TPR=TPR,FPR=FPR))
}

# Rates from random forest
get_rf_rates <- function() {
  TPR <- 1662/(1662 + 454) #78.5%
  FPR <- 9137 / (31719 + 9137) # 22.4%
  return(list(TPR=TPR,FPR=FPR))
}

# Rates and other parameters from random forest for different probability thresholds
get_rf_rates2 <- function(threshold) {
  params <- tibble::tribble(
    ~probability,  ~TPR,   ~FPR,     ~q,  ~a6, ~a12,    ~E6,   ~E12,
    0.1, 0.983,  0.672, 0.0322,    2, 3.68, -0.317,    1.8,
    0.2, 0.949,  0.508,  0.031, 1.76, 3.05, -0.599,   1.15,
    0.3, 0.909,  0.394, 0.0297,  1.6, 2.62, -0.771,  0.684,
    0.4, 0.855,    0.3,  0.028, 1.47, 2.26,  -0.88,  0.308,
    0.5, 0.785,  0.224, 0.0257, 1.36, 1.97, -0.923, 0.0256,
    0.6,  0.71,   0.16, 0.0232, 1.27, 1.73, -0.925, -0.188,
    0.7, 0.618,  0.114, 0.0202,  1.2, 1.55, -0.856, -0.284,
    0.8, 0.517,  0.079, 0.0169, 1.15, 1.41, -0.745, -0.304,
    0.9, 0.401, 0.0571, 0.0131, 1.12, 1.33, -0.571, -0.214
  )
  df <- params %>% filter(probability==threshold)
  return(list(TPR=df$TPR, FPR=df$FPR))
  
}

test_get_cost <- function(name, threshold=0.5) {
  
  if (name == "computed_consts_linear_progesa") {
    data <- load_single("~/FRCBS/interval_prediction/data/full_data_preprocessed-2020-05-25.rdata")
    consts <- get_cost_constants(data)
    ret <- get_linear_progesa_rates()
  } else if (name == "mikkos_consts_linear_progesa") {
    consts <- get_mikkos_cost_constants()
    ret <- get_linear_progesa_rates()
  } else if (name == "mikkos_consts_rf_progesa") {
    consts <- get_mikkos_cost_constants()
    ret <- get_rf_rates2(threshold)
    name <- sprintf("%s %.1f", name, threshold)
  }
  
  consts <- get_mikkos_cost_constants()
  #ret <- get_linear_progesa_rates()
  
  d <- consts$d
  mr <- consts$mr
  fr <- consts$fr
  mdr <- consts$mdr
  fdr <- consts$fdr
  
  TPR <- ret$TPR
  FPR <- ret$FPR
  #cat(TPR, "\n")
  #cat(FPR)
  q <- d * TPR
  rl <- 0
  Pm <- 2
  Pd <- 60
  Fn <- 0.1066
  cost <- get_cost(TPR, FPR, d, mr, fr, mdr, fdr, Pm, Pd, Fn, rl)
  cost <- cost %>% mutate(name = name)
  return(cost)
  
}

test_get_all_costs <- function() {
  costs <- bind_rows(test_get_cost("computed_consts_linear_progesa"), 
                     test_get_cost("mikkos_consts_linear_progesa"), 
                     test_get_cost("mikkos_consts_rf_progesa", 0.5),
                     test_get_cost("mikkos_consts_rf_progesa", 0.6),
                     test_get_cost("mikkos_consts_rf_progesa", 0.8),
  )
  return(costs)
}


cost_func <- function(q, atot, Pm, Pd, F, Fn, rloss) {Pm * ((F*atot) / (F + Fn*(atot-1)) - 1 - q*rloss) - Pd*q}

cost_func_simplified <- function(q, atot) {2 / ((1-0.1066)/atot + 0.1066)  - 2 - 60*q}

myoperator2 <- function(f) {
  function(L) { f(L[1], L[2])}  
}

myoperator7 <- function(f) {
  function(L) { f(L[1], L[2], L[3], L[4], L[5], L[6], L[7])}  
}


# Computes the gradient of the cost function at the given point.
# The result is a vector, whose components can be thought of as coefficients of a linear function
# that approximates the cost function at the given point.
compute_gradient_of_cost <- function(q=0.015, atot=1.5, Pm=2, Pd=60, F=1, Fn=0.107, rloss=0) {
  func <- myoperator7(myfunc2)
  point <- c(q=q, atot=atot, Pm=Pm, Pd=Pd, F=F, Fn=Fn, rloss=rloss)
  v <- numDeriv::grad(func, point)
  n <- c("q", "atot", "Pm", "Pd", "F", "Fn", "rloss")
  names(v) <- n
  cat("Gradient at point\n")
  print(point)
  cat("is\n")
  print(v)
  #cat(sprintf("Gradient at point %s is %s\n", paste(point, sep=",", collapse = ","),  paste(v, collapse=",")))
  return(invisible(v))
}

# Draw the cost surface on parameters q and atot
draw_surface <- function() {
  n <- 1000
  df <- tibble(atot=seq(1, 2, length.out = n), 
               q=seq(0, 0.032, length.out = n))
  df <- df %>% expand(atot, q)
  df <- df %>% mutate(E = cost_func_simplified(q, atot))
  
  g <- df %>% ggplot(aes(x=q, y=atot, fill=E)) +
    geom_raster()
  return(g)
}

# Draw the absolute value of the gradient of the cost function on parameters q and atot only
draw_gradient <- function() {
  mylength <- function(L) sqrt(L[1]^2 + L[2]^2)
  n <- 100
  df <- tibble(atot=seq(1, 2, length.out = n), 
               q=seq(0, 0.032, length.out = n))
  df <- df %>% expand(atot, q)
  func <- myoperator2(cost_func_simplified)
  df <- df %>% mutate(G = map2_dbl(q, atot, function(x, y) mylength(numDeriv::grad(func, c(x, y)))))
  
  g <- df %>% ggplot(aes(x=q, y=atot, fill=G)) +
    geom_raster()
  return(g)
}

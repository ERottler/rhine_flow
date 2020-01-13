###

#Function used in rhine_basin.R

###

#Local polynomial Regression Fitting with NA-restoring
loess_na <- function (data_in, sm_span = 0.2, NA_restore = TRUE, poly_degree = 2){
  
  NAs <- which(is.na(data_in))
  data_in_sm <- na.approx(data_in, na.rm = F)
  x <- 1:length(data_in_sm)
  smooth_mod <- loess(data_in_sm ~ x, span = sm_span, degree = poly_degree)
  data_in_sm[which(!is.na(data_in_sm))] <- predict(smooth_mod)
  if (NA_restore) {
    data_in_sm[NAs] <- NA
  }
  return(data_in_sm)
}


is.even <- function(x) {x %% 2 == 0}


#Berry gdp function for parametric quantiles
GPDquantile <- function(x, probs)
{
  NA_output <- rep(NA, length(probs))
  mom <- lmomco::lmoms(x, nmom=5)
  if(!lmomco::are.lmom.valid(mom)) return(NA_output)
  param <- lmomco::lmom2par(mom, type="gpa")
  if(is.null(param)) return(NA_output)
  if(all(is.na(x))) return(NA_output)
  lmomco::qlmomco(f=probs, para=param)
}

#trend statistics: basic function
dis_ana <- function(disc, date, start_year = 1950, end_year = 2010, method_analys, quant_in = .95,
                    method_quant = "gev", do_moving_average = T, window_width = 30, 
                    cover_thresh = 0.2, above_below_quant = "above", w_allign = "center", quant_annual = F,
                    break_day = 0, do_prob_beta_reg = F, beta_zero_one_thres = 0.2, rank_sel = 30,
                    weather_type = c(1, 2, 3, 4)){
  
  input_data_full <- data.frame(date = date, value = disc)
  
  #Clip selected time period
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) >= start_year, ]
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) <= end_year, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, value[match(as.Date(full_date), as.Date(date))])
  )
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[, 1] <- start_year:end_year
  
  if(break_day > 0){
    
    for(i in 0:(length(start_year:end_year) - 2)) {
      
      data_day[i+1, 2:366] <- input_data$values[(i*365 + 1 + break_day):((i+1)*365 + break_day)]
      
    }
    
    data_day <- data_day[-nrow(data_day),]
  }else{
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]
      
    }
  }

  #Function for trend calculations
  
  f_sens_slope <- function(data_in, cover_thresh = 0.9){
    
    if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
      sens_slo <-  NA
    }else{
      time_step <- 1:length(data_in)
      sens_slo <- as.numeric(zyp.sen(data_in~time_step)$coefficients[2])
      #sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
    }
    return(sens_slo)
  }

  #Select quantile method
  
  if(method_quant == "empirical"){
    
    f_quan <- function(data_in){
      if((length(which(is.na(data_in))) / length(data_in)) > cover_thresh){#check cover threshold
        quant_out <-rep(NA, length(quant_in))}else{
          if(length(unique(data_in)) <= 1){
            quant_out <- rep(unique(data_in), length(quant_in))
          }else{
            quant_out <- quantile(data_in, probs = quant_in, type = 8, na.rm = T)}}
      
      return(as.numeric(quant_out))
      
    }
    
  }
  
  if(method_quant == "gev"){
    f_quan <- function(data_in){
      if((length(which(is.na(data_in))) / length(data_in)) > cover_thresh){#check cover threshold
        quant_out <- NA}else{
          if(length(unique(data_in)) <= 1){
            quant_out <- unique(data_in)
          }else{
            params <- lmr2par(data_in, type = "gev")
            if(is.null(params)){#check if parameters were calculated
              quant_out <- median(data_in)}else{
                quant_out <- par2qua(para = params, f = quant_in)}}}
      return(quant_out)
    }
  }
  
  if(method_quant == "gpd"){
    
    f_quan <- function(data_in){
      
      quant_out <- GPDquantile(data_in, probs = quant_in)
      
      return(quant_out)
      
    }
    
  }
  

  #Selecte analytical method
  
  if(method_analys == "mean"){
    f_mea_na_thres <- function(data_in){mea_na_thres(x = data_in, na_thres = 1 - cover_thresh)}
    res <- apply(data_day[,-1], 2, f_mea_na_thres)
  }
  
  if(method_analys == "median"){
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
  }
  
  if(method_analys == "sum"){
    f_sum <- function(data_in){sum(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_sum)
  }
  
  if(method_analys == "sens_slope"){
    #Moving average filter
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = mea_na_thres, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Trends only calculated when at least 50 % of input not NA or 0
    res <- apply(data_day[,-1], 2, f_sens_slope)
    
    
  }
  
  if(method_analys == "window_rank_sens_slope"){
    
    f_rank_extr <- function(data_in){
      
      sort(data_in, na.last = F)[rank_sel]
  
    }
    
    #Window maximum
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_rank_extr, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Trends only calculated when at least 50 % of input not NA or 0
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    res <- apply(data_day[,-1], 2, f_sens_slope)
    
  }
  
  if(method_analys == "window_rank_mean"){
    
    f_rank_extr <- function(data_in){
      
      sort(data_in, na.last = F)[rank_sel]
      
    }
    
    #Window maximum
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_rank_extr, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Trends only calculated when at least 50 % of input not NA or 0
    f_mea_na_thres <- function(data_in){mea_na_thres(x = data_in, na_thres = 1 - cover_thresh)}
    res <- apply(data_day[,-1], 2, f_mea_na_thres)
    
  }
  
  if(method_analys == "window_rank_medi"){
    
    f_rank_extr <- function(data_in){
      
      sort(data_in, na.last = F)[rank_sel]
      
    }
    
    #Window maximum
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_rank_extr, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Trends only calculated when at least 50 % of input not NA or 0
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
    
  }
  
  if(method_analys == "window_rank_max"){
    
    f_rank_extr <- function(data_in){
      
      sort(data_in, na.last = F)[rank_sel]
      
    }
    
    #Window maximum
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_rank_extr, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    f_max <- function(data_in){max(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_max)
    
  }
  
  if(method_analys == "window_rank_min"){
    
    f_rank_extr <- function(data_in){
      
      sort(data_in, na.last = F)[rank_sel]
      
    }
    
    #Window maximum
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_rank_extr, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    f_min <- function(data_in){min(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_min)
    
  }
  
  if(method_analys == "sens_slope_snow"){
    #Moving average filter
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = mea_na_thres, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }

    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    res <- apply(data_day[,-1], 2, f_sens_slope) * 10 # cm/dec
    for (i in 1:365) {
      if ((length(which(is.na(data_day[, i + 1])))/nrow(data_day)) < 
          (cover_thresh)) {
        if (length(which(data_day[, i + 1] == 1)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, i + 1]))))) {
          res[i] <- 0
        }
        if (length(which(data_day[, i + 1] == 0)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, i + 1]))))) {
          res[i] <- 0
        }
      }
    }
    
  }
  
  if(method_analys == "quantile"){
    
    if(quant_annual){
      res <- apply(data_day[,-1], 1, f_quan)
    }else{
      res <- apply(data_day[,-1], 2, f_quan)
    }
  }
  
  if(method_analys == "quantile_prob"){
    
    quant_thres <- f_quan(data_day[, -1])
      
    f_quant_prob <- function(data_in){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        quant_lik <-  NA
      }else{
        if(above_below_quant == "above"){
          quant_lik <-sum(ifelse(data_in > quant_thres, 100, 0), na.rm=T) /
            (length(data_in)-length(which(is.na(data_in))))}
        if(above_below_quant == "below"){
          quant_lik <-sum(ifelse(data_in < quant_thres, 100, 0), na.rm=T) /
            (length(data_in)-length(which(is.na(data_in))))
        }
      }
      return(quant_lik)
    }
    
    res <- apply(data_day[,-1], 2, f_quant_prob)
    
  }
  
  if(method_analys == "weather_type_window_sens_slope"){
    #Selected weather type: Yes (1) or No (0)
    
    f_weatherYN <- function(data_in, gwts = weather_type){
      
      if(data_in %in% gwts){
        data_in <- 1
      }else{
        data_in <- 0
      }
      return(data_in)
    }
    
    input_data$values <- sapply(input_data$values, f_weatherYN)
    
    
    #Apply moving average
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = sum_na, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate trends of window likelihood
    mov_res <- apply(data_day[, -1], 2, f_sens_slope)
    
    #When likelihood always (or only one value different from) 0 / 1 no trend
    #calculated (NA), but trend is 0
    
    for(i in 1:365){
      
      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){
        
        if(length(which(data_day[, i+1] == 1)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }
        
        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }
      }
    }
    
    res <- mov_res
    
  }
  
  if(method_analys == "mov_quant_trend"){
    
    
    ns <- 1:(length(input_data$values) - 29) #interger to moving window over time series
    
    mov_quants <- function(int){
      
      if(length(which(is.na(input_data$values[int:(int+window_width-1)]))) > (window_width/2)){
        
        mov_quant_out <- rep(NA, length(quant_in))
        
      }else{
        mov_quant_out <- f_quan(input_data$values[int:(int+window_width-1)])
        
      }
      
      return(mov_quant_out)
      
    }
    
    mov_quants_vals <- pbsapply(ns, mov_quants, simplify = T)
    
    #add NA columns beginning and end to 'center' moving window
    
    na_cols_sta <- matrix(NA, nrow = nrow(mov_quants_vals), ncol = 14)
    na_cols_end <- matrix(NA, nrow = nrow(mov_quants_vals), ncol = 15)
    mov_quants_vals <- cbind(na_cols_sta, mov_quants_vals, na_cols_end)
    
    #Calculate trends of quantiles

    f_quant_wind_slo <- function(row_sel){
      
      data_quant_sel <- mov_quants_vals[row_sel, ]
      
      #Order data by day
      data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
      colnames(data_day) <- c("year", days)
      data_day[ ,1] <- start_year:end_year
      
      for(i in 0:(length(start_year:end_year)-1)) {
        
        data_day[i+1, 2:366] <- data_quant_sel[(i*365+1):((i+1)*365)]
        
      }
      
      res_quant <- apply(data_day[, -1], 2, f_sens_slope) * 10 # per decade
      
      return(res_quant)
      
    }
    
    quants_int <- 1:nrow(mov_quants_vals)
    
    res <- pbsapply(quants_int, f_quant_wind_slo, simplify = T)
    
    
    
    # #Moving quantiles
    # input_data$mq <- rollapply(data = input_data$values, width = window_width,
    #                            FUN = f_quan, align = "center", fill = NA)
    # 
    # #Order data by day
    # data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    # colnames(data_day) <- c("year", days)
    # data_day[ ,1] <- start_year:end_year
    # 
    # for(i in 0:(length(start_year:end_year)-1)) {
    #   
    #   data_day[i+1, 2:366] <- input_data$mq[(i*365+1):((i+1)*365)]
    #   
    # }
    # 
    # #Calculate trends of quantiles
    # f_sens_slope <- function(data_in, cover_thresh = 0.2){
    #   if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
    #     sens_slo <-  NA
    #   }else{
    #     sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
    #   }
    #   return(sens_slo)
    # }
    # res <- apply(data_day[, -1], 2, f_sens_slope)
    
  }
  
  if(method_analys == "mov_quant_trend_above_thres"){
    
    if(method_quant == "empirical"){
      f_quan <- function(data_in){
        above_zero <- which(data_in > 1)
        quantile(data_in[above_zero], probs = quant_in, na.rm = T)}
    }
    
    if(method_quant == "gev"){
      f_quan <- function(data_in){
        if((length(which(is.na(data_in))) / length(data_in)) < cover_thresh){
          above_zero <- which(data_in > 1)
          params <- lmr2par(data_in[above_zero], type = "gev")
          par2qua(para = params, f = quant_in)}else{
            return(NA)
          }
      }
    }
    
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    #Moving quantiles
    input_data$mq <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_quan, align = "center", fill = NA)
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$mq[(i*365+1):((i+1)*365)]
      
    }
    
    #Calculate trends of quantiles
    res <- apply(data_day[, -1], 2, f_sens_slope)
  }
  
  if(method_analys == "mov_quant_prob_trend"){
    
    # #Flood yes or no (above or below selected quantile)
    # if(method_quant == "empirical"){
    #   quant_thres <- quantile(input_data$values, probs = quant_in, na.rm = T) #selected quantile
    # }
    # 
    # if(method_quant == "gev"){
    #   params <- lmr2par(input_data$values, type = "gev")
    #   quant_thres <- par2qua(para = params, f = quant_in)
    # }
    
    quant_thres <- f_quan(data_day[, -1])
    
    if(above_below_quant == "above"){
      f_floodYN <- function(data_in){ifelse(data_in >= quant_thres, 100, 0)}
    }
    
    if(above_below_quant == "below"){
      f_floodYN <- function(data_in){ifelse(data_in <= quant_thres, 100, 0)}
    }
    
    input_data$values <- sapply(input_data$values, f_floodYN)
    
    #Calculate moving window probability
    # f_mean <- function(data_in){mean(data_in, na.rm = T)}
    f_prop <- function(data_in){
      sum_in <- sum(data_in, na.rm = T)
      length_non_na <- length(which(!is.na(data_in)))
      frac_out <- sum_in/length_non_na
      return(frac_out)
    }
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_prop, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate trends in window exceedance probabilites
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    f_beta_reg <- function(data_in, cover_thres = 0.2, zero_one_thres = beta_zero_one_thres){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        beta_coef <-  NA
      }else{
        
        #Only do regression when enough values different to 0 / 1
        
        not_zero_one <- 1 - (length(which(data_in == 0)) + length(which(data_in == 1))) / length(data_in)
        
        if(not_zero_one > zero_one_thres){
          
          #transforme extremes 0 and 1 
          # Smithson, M., & Verkuilen, J. (2006). A better lemon squeezer? Maximum-likelihood 
          # regression with beta-distributed dependent variables. Psychological Methods, 11(1), 54-71.
          # http://dx.doi.org/10.1037/1082-989X.11.1.54
          
          f_betaXtrans <- function(data_in){
            
            ndata <- length(data_in)
            
            f_trans <- function(value_in){
              
              value_out <- (value_in * (ndata -1) +0.5)/ndata
              
            }
            
            data_out <- sapply(data_in, f_trans)
            
            return(data_out)
            
          }
          
          data_trans <- f_betaXtrans(data_in) / 100 # divide by 100 for data 0 to 1; before data in [%]
          time_steps <- 1:length(data_in)
          
          beta_dat <- data.frame(time_steps = time_steps,
                                 data_trans = data_trans)
          
          beta_mod <- betareg(data_trans ~ time_steps, data = beta_dat)
          
          beta_coef <- as.numeric(beta_mod$coefficients$mean[2])
          
        }else{
          
          beta_coef <- NA
          
        }
        
      }
      
      return(beta_coef)
      
    }
    
    if(do_prob_beta_reg){
      
      res <- apply(data_day[, -1], 2, f_beta_reg)
      
    }else{
      res <- apply(data_day[, -1], 2, f_sens_slope) *10 #[%/dec]
      
      #When likelihood always (or only one value different from) 0 / 100 no trend
      #calculated (NA), but trend is 0
      
      for(i in 1:365){
        
        #if trend magnitude calculated
        if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){
          
          if(length(which(data_day[, i+1] == 100)) >=
             ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
            res[i] <- 0
          }
          
          if(length(which(data_day[, i+1] == 0)) >=
             ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
            res[i] <- 0
          }
        }
      }
      
    }
    
  }
  
  if(method_analys == "mov_quant_prob_trend_above_thres"){
    
    #Flood yes or no (above or below selected quantile)
    if(method_quant == "empirical"){
      data_above <- input_data$values[which(input_data$values > 1)]
      quant_thres <- quantile(data_above, probs = quant_in, na.rm = T) #selected quantile
    }
    
    if(method_quant == "gev"){
      data_above <- input_data$values[which(input_data$values > 1)]
      params <- lmr2par(data_above, type = "gev")
      quant_thres <- par2qua(para = params, f = quant_in)
    }
    
    
    if(above_below_quant == "above"){
      f_floodYN <- function(data_in){ifelse(data_in > quant_thres, 100, 0)}
    }
    
    if(above_below_quant == "below"){
      f_floodYN <- function(data_in){ifelse(data_in < quant_thres, 100, 0)}
    }
    
    input_data$values <- sapply(input_data$values, f_floodYN)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate trends of flood likelihood
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
    #When likelihood always (or only one value different from) 0 / 100 no trend
    #calculated (NA), but trend is 0
    
    for(i in 1:365){
      
      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){
        
        if(length(which(data_day[, i+1] == 100)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
        
        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
      }
    }
  }
  
  if(method_analys == "above_zero_prob"){
    
    #Value above zero
    f_valueYN <- function(data_in){ifelse(data_in > 0, 100, 0)}
    
    input_data$values <- sapply(input_data$values, f_valueYN)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate mean probability
    f_mean <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        medi_val <-  NA
      }else{
        medi_val <- mean(data_in, na.rm = T)
      }
      return(medi_val)
    }
    
    res <- apply(data_day[, -1], 2, f_mean)
    
  }
  
  if(method_analys == "above_thres_prob"){
    
    #Value above zero
    f_valueYN <- function(data_in){ifelse(data_in > 1, 100, 0)}
    
    input_data$values <- sapply(input_data$values, f_valueYN)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate mean probability
    f_mean <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        medi_val <-  NA
      }else{
        medi_val <- mean(data_in, na.rm = T)
      }
      return(medi_val)
    }
    
    res <- apply(data_day[, -1], 2, f_mean)
    
  }
  
  if(method_analys == "above_zero_prob_trend"){
    
    #Value above zero
    f_valueYN <- function(data_in){ifelse(data_in > 0, 100, 0)}
    
    input_data$values <- sapply(input_data$values, f_valueYN)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate trends of flood likelihood
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
    #When likelihood always (or only one value different from) 0 / 100 no trend
    #calculated (NA), but trend is 0
    
    for(i in 1:365){
      
      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){
        
        if(length(which(data_day[, i+1] == 100)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
        
        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
      }
    }
    
  }
  
  if(method_analys == "above_thres_prob_trend"){
    
    #Value above zero
    f_valueYN <- function(data_in){ifelse(data_in > 1, 100, 0)}
    
    input_data$values <- sapply(input_data$values, f_valueYN)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate trends of flood likelihood
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
    #When likelihood always (or only one value different from) 0 / 100 no trend
    #calculated (NA), but trend is 0
    
    for(i in 1:365){
      
      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){
        
        if(length(which(data_day[, i+1] == 100)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
        
        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          res[i] <- 0
        }
      }
    }
    
  }
  
  if(method_analys == "frac_yearly_disc"){
    
    #Get year of corresponding date
    # f_get_year <- function(date){
    #     year <- as.numeric(format(input_data$date,'%Y'))
    #     # year <- as.numeric(substring(as.character(input_data$date), 1, 4))
    # }
    # 
    # input_data$years <- sapply(input_data$dates, f_get_year)
    # 
    # #Calculate yearly sum of discharge (January-December)
    # f_year_sum <- function(date, values = input_data$values, dates = input_data$dates, 
    #                        years = input_data$years){
    #   
    #   year_sum <- sum(values[which(as.numeric(format(dates, '%Y')) == as.numeric(format(date, '%Y')))])
    #   return(year_sum)
    # 
    # }
    # 
    # input_data$year_sum <- sapply(input_data$dates, f_year_sum)  
    
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$values[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    f_sum <- function(data_in){sum(data_in, na.rm = T)}
    sums <- apply(data_day[, -1], 1, f_sum)
    
    year_sums <- data.frame(years = data_day[ ,1], sums = sums)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    # #Calculate fraction of the year
    # input_data$frac <- input_data$ma / input_data$year_sum
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate fraction of the year
    for(i in 1:nrow(data_day)){
      data_day[i, -1] <- data_day[i, -1] / year_sums$sums[i]
    }
    
    #Calculate median for each doy
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
  }
  
  if(method_analys == "frac_yearly_disc_trend"){
    
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$values[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    f_sum <- function(data_in){sum(data_in, na.rm = T)}
    sums <- apply(data_day[, -1], 1, f_sum)
    
    year_sums <- data.frame(years = data_day[ ,1], sums = sums)
    
    #Apply moving average
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mean, align = "center", fill = NA)
    
    # #Calculate fraction of the year
    # input_data$frac <- input_data$ma / input_data$year_sum
    
    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }
    
    #Calculate fraction of the year
    for(i in 1:nrow(data_day)){
      data_day[i, -1] <- data_day[i, -1] / year_sums$sums[i]
    }
    
    #Calculate trends of discharge fraction
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
  }
  
  if(method_analys == "mov_sum_sens_slope"){
    #Moving average filter
    sum_na <- function(x){sum(x, na.rm = T)}
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = sum_na, align = w_allign, fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    res <- apply(data_day[,-1], 2, f_sens_slope)
    
    
  }
  
  if(method_analys == "mov_peak_time"){
    #Moving average filter
    f_peak <- function(x){
      thresh <- quantile(x, probs = 0.9)
      thresh_index <- mean(which(x > thresh), na.rm = T)
      #Rescale to values 0 to 1
      #seq(0, 1, length.out = window_width)[thresh_index]
    }
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_peak, align = "right", fill = NA)
    
    max(dis$Diepoldsau, na.rm = T)
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Calculate median peak timing
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
    
  }

  if(method_analys == "mov_peak_time_trend"){
    #Moving average filter
    f_peak <- function(x){
      thresh <- quantile(x, probs = 0.9)
      thresh_index <- mean(which(x > thresh), na.rm = T)
      #Rescale to values 0 to 1
      #seq(0, 1, length.out = window_width)[thresh_index]
    }
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_peak, align = "right", fill = NA)
    
    max(dis$Diepoldsau, na.rm = T)
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Calculate trends of peak timing within window
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
  }
  
  if(method_analys == "rain_event_intens"){
    f_mea_rain_int <- function(x){
      rain_days <- which(x > 0)
      mea_int <- mean(x[rain_days])
      return(mea_int)
    }
    res <- apply(data_day[,-1], 2, f_mea_rain_int)
  }
  
  if(method_analys == "mov_rain_event_intens"){
    #Moving average filter
    f_mea_rain_int <- function(x){
      rain_days <- which(x > 0)
      mea_int <- mean(x[rain_days])
      return(mea_int)
    }
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_mea_rain_int, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Calculate trends of peak timing within window
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
  }
  
  if(method_analys == "frac_rain_high_intens"){
    f_fra_rain_int <- function(x){
      rain_days <- which(x > 0)
      params <- lmr2par(x[rain_days], type = "gev")
      my_quant <- par2qua(para = params, f = quant_in)
      high_int_days <- which(x > my_quant)
      rain_sum_high_int <- sum(x[high_int_days])
      frac_high <- rain_sum_high_int / sum(x)
      return(frac_high)
    }
    res <- apply(data_day[,-1], 2, f_fra_rain_int)
  }
  
  if(method_analys == "mov_frac_rain_high_intens"){
    #Moving average filter
    f_fra_rain_int <- function(x){
      rain_days <- which(x > 0)
      params <- lmr2par(x[rain_days], type = "gev")
      my_quant <- par2qua(para = params, f = quant_in)
      high_int_days <- which(x > my_quant)
      rain_sum_high_int <- sum(x[high_int_days])
      frac_high <- rain_sum_high_int / sum(x)
      return(frac_high)
    }
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = f_fra_rain_int, align = "center", fill = NA)
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day) <- c("year", days)
    data_day[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
      
    }
    
    #Calculate trends of peak timing within window
    f_sens_slope <- function(data_in, cover_thresh = 0.2){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    res <- apply(data_day[, -1], 2, f_sens_slope)
    
  }
  
  if (method_analys == "weather_type_window_likeli_sens_slope") {
    f_weatherYN <- function(data_in, gwts = weather_type) {
      if (data_in %in% gwts) {
        data_in <- 1
      }
      else {
        data_in <- 0
      }
      return(data_in)
    }
    input_data$values <- sapply(input_data$values, f_weatherYN)
    input_data$ma <- rollapply(data = input_data$values, 
                               width = window_width, FUN = mea_na_thres, align = "center", 
                               fill = NA)
    data_day = matrix(NA, nrow = length(start_year:end_year), 
                      ncol = 366)
    colnames(data_day) = c("year", days)
    data_day[, 1] <- start_year:end_year
    for (i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 
                                                 1):((i + 1) * 365)]
    }
    
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    mov_res <- apply(data_day[, -1], 2, f_sens_slope)
    for (i in 1:365) {
      if ((length(which(is.na(data_day[, i + 1])))/nrow(data_day)) < 
          (1 - cover_thresh)) {
        if (length(which(data_day[, i + 1] == 1)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                                i + 1]))))) {
          mov_res[i] <- 0
        }
        if (length(which(data_day[, i + 1] == 0)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                                i + 1]))))) {
          mov_res[i] <- 0
        }
      }
    }
  }
  
  if (method_analys == "snow_probability") {

    snow_thres <- 0
    f_snow_likelihood <- function(data_in) {
      if (length(which(is.na(data_in)))/length(data_in) > (1 - cover_thresh)) {
        snow_lik <- NA
      }
      else {
        snow_lik <- sum(ifelse(data_in > snow_thres,  100, 0), na.rm = T)/(length(data_in) - length(which(is.na(data_in))))
      }
      return(snow_lik)
    }
    res <- apply(data_day[, -1], 2, f_snow_likelihood)
  }
  
  if (method_analys == "snow_window_prob_slo") {
    snow_thres <- 0
    f_snowYN <- function(data_in) {
      ifelse(data_in > snow_thres, 100, 0)
    }
    input_data$values <- sapply(input_data$values, f_snowYN)
    input_data$ma <- rollapply(data = input_data$values, 
                               width = window_width, FUN = mea_na_thres, align = "center", 
                               fill = NA)
    data_day = matrix(NA, nrow = length(start_year:end_year), 
                      ncol = 366)
    colnames(data_day) = c("year", days)
    data_day[, 1] <- start_year:end_year
    for (i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 
                                                 1):((i + 1) * 365)]
    }
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    res <- apply(data_day[, -1], 2, f_sens_slope) * 10 # %/dec
    for (i in 1:365) {
      if ((length(which(is.na(data_day[, i + 1])))/nrow(data_day)) < 
          (cover_thresh)) {
        if (length(which(data_day[, i + 1] == 100)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, i + 1]))))) {
          res[i] <- 0
        }
        if (length(which(data_day[, i + 1] == 0)) >= 
            ((nrow(data_day) - 2) - length(which(is.na(data_day[, i + 1]))))) {
          res[i] <- 0
        }
      }
    }
  }
  
  return(res)
}

f_med <- function(data_in){dis_ana(disc = data_in,
                                   date = meteo_date,
                                   start_year = start_year,
                                   end_year = end_year,
                                   window_width = window_width,
                                   method_analys = "median",
                                   cover_thresh = cover_thres
)}

f_med_snow <- function(data_in){dis_ana(disc = data_in,
                                   date = data_snow$date,
                                   start_year = start_year,
                                   end_year = end_year,
                                   window_width = window_width,
                                   method_analys = "median",
                                   cover_thresh = cover_thres
)}

f_sum <- function(data_in){dis_ana(disc = data_in,
                                   date = meteo_date,
                                   start_year = start_year,
                                   end_year = end_year,
                                   window_width = window_width,
                                   method_analys = "sum",
                                   cover_thresh = cover_thres
)}

f_slo <- function(data_in){dis_ana(disc = data_in,
                                    date = meteo_date,
                                    start_year = start_year,
                                    end_year = end_year,
                                    window_width = window_width,
                                    method_analys = "sens_slope",
                                    cover_thresh = cover_thres
)}

f_slo_snow <- function(data_in){dis_ana(disc = data_in,
                                   date = data_snow$date,
                                   start_year = start_year,
                                   end_year = end_year,
                                   window_width = window_width,
                                   method_analys = "sens_slope_snow",
                                   cover_thresh = cover_thres
)}

f_q75_snow <- function(data_in){dis_ana(disc = data_in,
                                        date = data_snow$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        method_analys = "quantile",
                                        method_quant = "empirical",
                                        quant_in = 0.75
)}

f_mea <- function(data_in){dis_ana(disc = data_in,
                                   date = meteo_date,
                                   start_year = start_year,
                                   end_year = end_year,
                                   window_width = window_width,
                                   method_analys = "mean",
                                   cover_thresh = cover_thres
)}

f_mea_snow <- function(data_in){dis_ana(disc = data_in,
                                        date = data_snow$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        method_analys = "mean",
                                        cover_thresh = cover_thres
)}

f_pro_snow <- function(data_in){dis_ana(disc = data_in,
                                        date = data_snow$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        method_analys = "snow_probability",
                                        cover_thresh = cover_thres
)}

f_pro_snow_sim <- function(data_in){dis_ana(disc = data_in,
                                            date = meteo_date,
                                            start_year = start_year,
                                            end_year = end_year,
                                            window_width = window_width,
                                            method_analys = "snow_probability",
                                            cover_thresh = cover_thres
)}

f_psl_snow <- function(data_in){dis_ana(disc = data_in,
                                        date = data_snow$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        method_analys = "snow_window_prob_slo",
                                        cover_thresh = cover_thres
)}

f_psl_snow_sim <- function(data_in){dis_ana(disc = data_in,
                                            date = meteo_date,
                                            start_year = start_year,
                                            end_year = end_year,
                                            window_width = window_width,
                                            method_analys = "snow_window_prob_slo",
                                            cover_thresh = cover_thres
)}

f_tzer <- function(data_in){dis_ana(disc = data_in,
                                    date = meteo_date,
                                    start_year = start_year,
                                    end_year = end_year,
                                    method_analys = "above_zero_prob"
)}

f_tzes <- function(data_in){dis_ana(disc = data_in,
                                           date = meteo_date,
                                           start_year = 1950,
                                           end_year = 2010,
                                           method_analys = "above_zero_prob_trend"
)} 

f_sus <- function(data_in){dis_ana(disc = data_in,
                                   date = meteo_date,
                                   start_year = 1950,
                                   end_year = 2010,
                                   method_analys = "mov_sum_sens_slope",
                                   w_allign = "right"
)} 

f_quant <- function(quant_sel){dis_ana(disc = disc_sel,
                                      date = dis_new$date,
                                      start_year = start_year,
                                      end_year = end_year,
                                      quant_in = quant_sel,
                                      window_width = window_width,
                                      method_analys = "quantile",
                                      method_quant = "gev"
)}

f_quant_rain <- function(quant_sel){dis_ana(disc = prec_basin,
                                       date = meteo_date,
                                       start_year = start_year,
                                       end_year = end_year,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "quantile",
                                       method_quant = "gev"
)}

f_qtren <- function(quant_sel){dis_ana(disc = disc_sel,
                                      date = dis_new$date,
                                      start_year = start_year,
                                      end_year = end_year,
                                      quant_in = quant_sel,
                                      window_width = window_width,
                                      method_analys = "mov_quant_trend",
                                      method_quant = "gev"
)}

# f_qprob <- function(quant_sel){dis_ana(disc = disc_sel,
#                                       date = dis_new$date,
#                                       start_year = start_year,
#                                       end_year = end_year,
#                                       quant_in = quant_sel,
#                                       window_width = window_width,
#                                       method_analys = "quantile_prob",
#                                       method_quant = "gev"
# )}

f_qprob_rain <- function(quant_sel){dis_ana(disc = prec_basin,
                                            date = meteo_date,
                                            start_year = start_year,
                                            end_year = end_year,
                                            quant_in = quant_sel,
                                            window_width = window_width,
                                            method_analys = "quantile_prob",
                                            method_quant = "gev"
)}

# f_qmove <- function(quant_sel){dis_ana(disc = disc_sel,
#                                       date = dis_new$date,
#                                       start_year = start_year,
#                                       end_year = end_year,
#                                       quant_in = quant_sel,
#                                       window_width = window_width,
#                                       method_analys = "mov_quant_prob_trend",
#                                       method_quant = "gev")}

f_qmove_rain <- function(quant_sel){dis_ana(disc = prec_basin,
                                       date = meteo_date,
                                       start_year = start_year,
                                       end_year = end_year,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "mov_quant_prob_trend",
                                       method_quant = "gev")}

#Fraction solid/liquid precipitation
f_lfra <- function(dates, preci, tempe){
  
  input_data <- data.frame(dates = dates,
                           precs = preci,
                           temps = tempe)
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order precipitation data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$precs[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_precs <- data_day
  
  #Order temperature data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$temps[(i*365+1):((i+1)*365)]
    
  }
  data_day_temps <- data_day
  
  
  for (i in 2:ncol(data_day_precs)) {
    
    pos_days <- which(data_day_temps[, i] > 0)
    liqu_frac_day <- (sum(data_day_precs[pos_days, i]) / sum(data_day_precs[, i])) * 100 #[%]
    
    if(i == 2){
      liqu_frac <- liqu_frac_day
    }else
      liqu_frac <- c(liqu_frac, liqu_frac_day)
  }
  
  return(liqu_frac)
  
}

f_my_lfra <- function(k){
  f_lfra(dates = meteo_date,
         preci = precs[, k],
         tempe = temps[, k])
}

#Trends window fraction solid/liquid precipitation
f_lfrs<- function(dates, preci, tempe, window = window_width,
                  cover_thresh = 0.2){
  
  input_data <- data.frame(dates = dates,
                           precs = preci,
                           temps = tempe)
  
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Moving sum with all precipitation
  f_sum <- function(x){sum(x, na.rm = T)}
  input_data$prec_mov_sum_all <- rollapply(data = input_data$precs, width = window,
                                           FUN = f_sum, align = "center", fill = NA)
  
  #Moving sum only liquid precipitation
  input_data$prec_liqu <- input_data$precs
  input_data$prec_liqu[which(input_data$temps < 0)] <- 0
  input_data$prec_mov_sum_liq <- rollapply(data = input_data$prec_liqu, width = window,
                                           FUN = f_sum, align = "center", fill = NA)
  
  #Calculate liquid fraction
  input_data$liqu_frac <- (input_data$prec_mov_sum_liq / input_data$prec_mov_sum_all) * 100 #[%]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order  data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$liqu_frac[(i*365+1):((i+1)*365)]
    
  }
  
  f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
  res <- apply(data_day[,-1], 2, f_sens_slope)
  
  
  return(res)
  
}

f_my_lfrs <- function(k){
  f_lfrs(dates = meteo_date,
         preci = precs[ , k],
         tempe = temps[ , k])
}

#average value of climate variable for weather types
gwt_med <- function(dates, clim_data, gwt_data, numb_wt=26){
  
  input_data <- data.frame(dates = dates,
                           clim = clim_data,
                           gwt = gwt_data)
  
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$dates, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order climate data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$clim[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_clim <- data_day
  
  #Order gwt data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$gwt[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_gwt <- data_day
  
  
  for(k in 1:numb_wt){
    
    for (i in 2:ncol(data_day_clim)) {
      
      gwt_days <- which(data_day_gwt[, i] == k)
      gwt_days_med <- median(data_day_clim[gwt_days, i])
      
      if(i == 2){
        gwt_med <- gwt_days_med
      }else
        gwt_med <- c(gwt_med, gwt_days_med)
    }
    
    if(k ==1){
      gwt_out <- gwt_med
    }else{
      gwt_out <- cbind(gwt_out, gwt_med)
    }
    
  }
  
  colnames(gwt_out) <- 1:numb_wt
  return(gwt_out)
  
}
gwt_mea <- function(dates, clim_data, gwt_data, numb_wt=26){
  
  input_data <- data.frame(dates = dates,
                           clim = clim_data,
                           gwt = gwt_data)
  
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$dates, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order climate data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$clim[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_clim <- data_day
  
  #Order gwt data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$gwt[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_gwt <- data_day
  
  
  for(k in 1:numb_wt){
    
    for (i in 2:ncol(data_day_clim)) {
      
      gwt_days <- which(data_day_gwt[, i] == k)
      gwt_days_mea <- mea_na(data_day_clim[gwt_days, i])
      
      if(i == 2){
        gwt_mea <- gwt_days_mea
      }else
        gwt_mea <- c(gwt_mea, gwt_days_mea)
    }
    
    if(k ==1){
      gwt_out <- gwt_mea
    }else{
      gwt_out <- cbind(gwt_out, gwt_mea)
    }
    
  }
  
  colnames(gwt_out) <- 1:numb_wt
  return(gwt_out)
  
}








wt_flex_mov <- function(wt_data, wt_rank, date, hig_low = 1, start_year, end_year, 
                        window_width, cover_thres, wts_numb = length(wt_hig)){
  
  input_data_full <- data.frame(date = date, value = wt_data)
  
  #Clip selected time period
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) >= start_year, ]
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) <= end_year, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, value[match(as.Date(full_date), as.Date(date))])
  )
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Selected weather types for each day
  f_wt_sel <- function(day_in, wts_numb_in = wts_numb){
    if(hig_low == 1) {wt_out <- which(day_in == 1)}
    if(hig_low == -1) {wt_out <- which(day_in == -1)}
    if(length(wt_out) < wts_numb){
      num_nas <- wts_numb - length(wt_out)
      wt_out = c(wt_out, rep(NA,num_nas))
    }
    return(wt_out)
  }

  wts_sel <- apply(wt_rank, 1, f_wt_sel)
  
  wts_window <- function(wts_sel_in){
    wts_freq <- as.data.frame(table(c(wts_sel_in)))
    wts_freq$Var1 <- as.numeric(levels(wts_freq$Var1))
  
    wts_freq_ord <- wts_freq[order(wts_freq$Freq),]
  
    if(hig_low == 1){
      wts_sel_index <- (nrow(wts_freq_ord)-(wts_numb-1)) : nrow(wts_freq_ord)
    }
  
    if(hig_low == -1){
      wts_sel_index <- 1:wts_numb
    }
  
    wts_out <- wts_freq_ord$Var1[wts_sel_index]
    return(wts_out)
  }
  
  window_indices <- data.frame(start = c(rep(1,14), 1:336, rep(336,15)), end = c(rep(30,14), 30:365, rep(365,15)))
  
  for(i in 1:nrow(window_indices)){
    
    wt_sing <- wts_window(wts_sel[,window_indices[i,1]:window_indices[i,2]])
    
    if(i == 1){
      wts_mw <- wt_sing
    }else{
      wts_mw <- cbind(wts_mw, wt_sing)
    }
  }
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]
    
  }
  
  f_weatherYN <- function(data_in, gwts = weather_type) {
    if (data_in %in% gwts) {
      data_in <- 1
    }
    else {
      data_in <- 0
    }
    return(data_in)
  }
  
  for(i in 1:365){
    
    my_f_weatherYN <- function(data_in){f_weatherYN(data_in, gwts = wts_mw[, i])}
  
    out_sing <- sapply(data_day[,i], my_f_weatherYN)
  
    if(i == 1){
      wt_YN <- out_sing
    }else{
      wt_YN <- cbind(wt_YN, out_sing)
   }
  
  }
  
  #Reconstruct time series out of day-ordered data
  wt_YN_series <- as.vector(t(wt_YN))
  
  input_data$ma <- rollapply(data = wt_YN_series, 
                             width = window_width, FUN = mea_na_thres, align = "center", 
                             fill = NA)
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
    
  }
  
  f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thres)}
  mov_res <- apply(data_day[, -1], 2, f_sens_slope)
  for (i in 1:365) {
    if ((length(which(is.na(data_day[, i + 1])))/nrow(data_day)) < 
        (1 - cover_thres)) {
      if (length(which(data_day[, i + 1] == 1)) >= 
          ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                              i + 1]))))) {
        mov_res[i] <- 0
      }
      if (length(which(data_day[, i + 1] == 0)) >= 
          ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                              i + 1]))))) {
        mov_res[i] <- 0
      }
    }
  }
  
  return(mov_res)  

}


wt_flex_mov_2 <- function(wt_data, wt_rank, date, hig_low = 1, start_year, end_year, 
                        window_width, cover_thres, wts_numb = length(wt_hig)){
  
  input_data_full <- data.frame(date = date, value = wt_data)
  
  #Clip selected time period
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) >= start_year, ]
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) <= end_year, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, value[match(as.Date(full_date), as.Date(date))])
  )
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]
    
  }
  
  #Selected weather types for each day
  f_wt_sel <- function(day_in, wts_numb_in = wts_numb){
    if(hig_low == 1) {wt_out <- which(day_in == 1)}
    if(hig_low == -1) {wt_out <- which(day_in == -1)}
    if(length(wt_out) < wts_numb){
      num_nas <- wts_numb - length(wt_out)
      wt_out = c(wt_out, rep(NA,num_nas))
    }
    return(wt_out)
  }
  
  wts_sel <- apply(wt_rank, 1, f_wt_sel)
  
  f_weatherYN <- function(data_in, gwts = weather_type) {
    if (data_in %in% gwts) {
      data_in <- 1
    }
    else {
      data_in <- 0
    }
    return(data_in)
  }
  
  for(i in 1:365){
    
    my_f_weatherYN <- function(data_in){f_weatherYN(data_in, gwts = wts_sel[, i])}
    
    out_sing <- sapply(data_day[,i], my_f_weatherYN)
    
    if(i == 1){
      wt_YN <- out_sing
    }else{
      wt_YN <- cbind(wt_YN, out_sing)
    }
    
  }
  
  #Reconstruct time series out of day-ordered data
  wt_YN_series <- as.vector(t(wt_YN))
  
  input_data$ma <- rollapply(data = wt_YN_series, 
                             width = window_width, FUN = mea_na_thres, align = "center", 
                             fill = NA)
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]
    
  }
  
  f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thres)}
  mov_res <- apply(data_day[, -1], 2, f_sens_slope)
  for (i in 1:365) {
    if ((length(which(is.na(data_day[, i + 1])))/nrow(data_day)) < 
        (1 - cover_thres)) {
      if (length(which(data_day[, i + 1] == 1)) >= 
          ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                              i + 1]))))) {
        mov_res[i] <- 0
      }
      if (length(which(data_day[, i + 1] == 0)) >= 
          ((nrow(data_day) - 2) - length(which(is.na(data_day[, 
                                                              i + 1]))))) {
        mov_res[i] <- 0
      }
    }
  }
  
  return(mov_res)  
  
}



#Discharge coefficient for snow volume estimate
liq_dis_coef <- function(dates_meteo, date_disc, temp_in, preci_in, disc_in, start_year,
                         end_year, method_analys, mov_window=7, cover_thres=0.2){
  
  input_meteo_full <- data.frame(date = dates_meteo,
                                 prec = preci_in,
                                 temp = temp_in)
  
  input_disch_full <- data.frame(date = date_disc,
                                 disc = disc_in)
  
  
  #Clip selected time period
  input_meteo <- input_meteo_full[as.numeric(format(input_meteo_full$date,'%Y')) >= start_year, ]
  input_meteo <- input_meteo[as.numeric(format(input_meteo_full$date,'%Y')) <= end_year, ]
  input_disch <- input_disch_full[as.numeric(format(input_disch_full$date,'%Y')) >= start_year, ]
  input_disch <- input_disch[as.numeric(format(input_disch$date,'%Y')) <= end_year, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_temp <- data.frame(dates  = full_date,
                           values = with(input_meteo, temp[match(as.Date(full_date), as.Date(date))])
  )
  
  input_prec <- data.frame(dates  = full_date,
                           values = with(input_meteo, prec[match(as.Date(full_date), as.Date(date))])
  )
  
  input_disc <- data.frame(dates  = full_date,
                           values = with(input_disch, disc[match(as.Date(full_date), as.Date(date))])
  )

  #Remove 29th of February
  input_temp <- input_temp[-which(format(input_temp$date, "%m%d") == "0229"),]
  input_prec <- input_prec[-which(format(input_prec$date, "%m%d") == "0229"),]
  input_disc <- input_disc[-which(format(input_disc$date, "%m%d") == "0229"),]
  
  #Only liquid fraction precipitation (rain > ?C)
  
  input_prec$values[which(input_temp$values < 0)] <- 0
  
  #Moving sum precipitation
  input_prec$sum <- rollapply(data = input_prec$values, width = mov_window,
                             FUN = sum_na, align = "right", fill = NA)
  
  input_disc$coef <- input_disc$values / input_prec$sum
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_disc$coef[(i*365+1):((i+1)*365)]
    
  }
  
  if(method_analys == "mean"){
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_mean)
  }
  
  if(method_analys == "median"){
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
  }
  
  if(method_analys == "sens_slope"){
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thres)}
    res <- apply(data_day[,-1], 2, f_sens_slope)
    
    
  }
  
  return(res)
  
}


f_coef_med <- function(k){
  
  liq_dis_coef(dates_meteo = meteo_date, date_disc = dis_new$date, 
               temp_in = temps[,k], preci_in = precs[,k],
               disc_in = disc_sel,
               start_year = start_year,
               end_year = end_year,
               method_analys = "median",
               mov_window = 7)
}



#Liquid precipitation

liqu_prec <- function(dates, temp_in, preci_in, start_y=start_year, end_y=end_year, 
                      method_analys, temp_thres = 0, data_day_output=F, mov_window = 4){
  
  input_full <- data.frame(date = dates_meteo,
                           prec = preci_in,
                           temp = temp_in)
  
  #Clip selected time period
  input_meteo <- input_full[as.numeric(format(input_meteo_full$date,'%Y')) >= start_y, ]
  input_meteo <- input_meteo[as.numeric(format(input_meteo_full$date,'%Y')) <= end_y, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_y,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_y,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_temp <- data.frame(dates  = full_date,
                           values = with(input_meteo, temp[match(as.Date(full_date), as.Date(date))])
  )
  
  input_prec <- data.frame(dates  = full_date,
                           values = with(input_meteo, prec[match(as.Date(full_date), as.Date(date))])
  )
  
  #Remove 29th of February
  input_temp <- input_temp[-which(format(input_temp$date, "%m%d") == "0229"),]
  input_prec <- input_prec[-which(format(input_prec$date, "%m%d") == "0229"),]
  
  #Only liquid fraction precipitation (rain > ?C)
  input_prec$values[which(input_temp$values < temp_thres)] <- 0
  
  #Moving average precipitation
  input_prec$ma <- rollapply(data = input_prec$values, width = mov_window,
                             FUN = mea_na, align = "right", fill = NA)
  
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_prec$ma[(i*365+1):((i+1)*365)]
    
  }
  
  if(method_analys == "mean"){
    f_mean <- function(data_in){mean(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_mean)
  }
  
  if(method_analys == "median"){
    f_medi <- function(data_in){median(data_in, na.rm = T)}
    res <- apply(data_day[,-1], 2, f_medi)
  }
  
  if(method_analys == "sens_slope"){
    f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
    res <- apply(data_day[,-1], 2, f_sens_slope)
  }
  
  if(data_day_output){
    return(data_day)
  }else{
    return(res) 
  }
  
}

f_liqu_prec_mea <- function(k){
  liqu_prec(dates = meteo_date,
            temp_in = temps[ ,k],
            preci_in = precs[ ,k],
            method_analys = "mean")
}

f_tota_prec_mea <- function(k){
  liqu_prec(dates = meteo_date,
            temp_in = temps[ ,k],
            preci_in = precs[ ,k],
            method_analys = "mean",
            temp_thres = -100)
}

f_liqu_prec_slo <- function(k){
  liqu_prec(dates = meteo_date,
            temp_in = temps[ ,k],
            preci_in = precs[ ,k],
            method_analys = "sens_slope")
}

f_tota_prec_slo <- function(k){
  liqu_prec(dates = meteo_date,
            temp_in = temps[ ,k],
            preci_in = precs[ ,k],
            method_analys = "sens_slope",
            temp_thres = -100)
}

#Image plot quantile analysis discharge
image_flow <- function(data_in, colors, breaks,
                       main = "", ylab = "",
                       ytiks = seq(9, 90, by = 10),
                       ylabels = seq(0.1, 0.9, by =0.1),
                       margins_1 = c(1.6,2.5,1.6,0),
                       margins_2 = c(1.6,0.5,1.6,1.7)){
  
  
  #Positions ticks and labels for x-axis
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  
  y <- 1:ncol(data_in)
  x <- 1:365
  
  par(mar= margins_1)
  
  image(x, y, as.matrix(data_in), col = colors, breaks = breaks, ylab = "",
        xlab = "", axes = F)
  
  axis(2, at = ytiks, labels = ylabels, mgp = c(3, 0.3, 0))
  mtext(ylab, 2, 1.5, cex = 0.7)
  
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  mtext(main, 3, 0.1, adj = 0.5, cex = 1)
  box()
  
  par(mar= margins_2)
  
  image_scale(as.matrix(data_in), col = colors, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.3, 0))
  box()
  
}


#Get fraction of NAs in data
na_test <- function(data_in){
  nu_na <- length(which(is.na(data_in)))
  na_frac <- nu_na / length(data_in)
  return(na_frac)
}


stn_col <- function(stn_in, colnames_in=colnames(data_snow)){
  col_sel <- which(colnames_in == stn_in)
  return(col_sel)
}

#get column index from data frame with name as input
sel_dis <- function(stn_name, dis_data = dis){
  col_sel <- which(colnames(dis) == stn_name)
  return(col_sel)
}

#Average (median) discharge
f_qmedi <- function(disc_in){dis_ana(disc = disc_in,
                                     date = dis$date,
                                     start_year = start_year,
                                     end_year = end_year,
                                     window_width = window_width,
                                     method_analys = "median")
} 

#Average mean discharge
f_qmean <- function(disc_in){dis_ana(disc = disc_in,
                                     date = dis$date,
                                     start_year = start_year,
                                     end_year = end_year,
                                     window_width = window_width,
                                     method_analys = "mean")
} 

#Runoff coefficient moving window
f_disco <- function(rain_in, disc_in, rain_date, disc_date, start_year, end_year, window_width, method_analy){
  
  input_full_rain <- data.frame(date = rain_date,
                                valu = rain_in)
  
  input_full_disc <- data.frame(date = disc_date,
                                valu = disc_in)
  
  input_rain <- input_full_rain[as.numeric(format(input_full_rain$date,'%Y')) >= start_year, ]#Clip selected time period
  input_rain <- input_rain[as.numeric(format(input_rain$date,'%Y')) <= end_year, ]
  
  input_disc <- input_full_disc[as.numeric(format(input_full_disc$date,'%Y')) >= start_year, ]#Clip selected time period
  input_disc <- input_disc[as.numeric(format(input_disc$date,'%Y')) <= end_year, ]
  
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC")) #Fill possible gaps
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_rain <- data.frame(dates  = full_date,
                           values = with(input_rain, valu[match(as.Date(full_date), as.Date(date))])
  )
  
  input_disc <- data.frame(dates  = full_date,
                           values = with(input_disc, valu[match(as.Date(full_date), as.Date(date))])
  )
  
  input_rain$values <- input_rain$values / 1000 * area_m2 #rainfall [m?/day]
  input_disc$values <- input_disc$values * 3600 * 24 #discharge [m?/day]
  
  rain <- rollapply(data = input_rain$values, width = window_width,
                    FUN = sum_na, align = "center", fill = NA)
  
  disc <- rollapply(data = input_disc$values, width = window_width,
                    FUN = sum_na, align = "center", fill = NA)
  
  dis_coef <- disc / rain #runoff coefficient total precipitation
  
  dcoef_year <- ord_day(data_in = dis_coef, date = input_rain$dates)
  
  if(method_analy == "median"){
    analy_out <- apply(dcoef_year[,-1], 2, med_na)
  }
  
  if(method_analy == "slope"){
    analy_out <- apply(dcoef_year[,-1], 2, sens_slope)
  }
  
  return(analy_out)
  
}


#Logarithmic sequence
logseq <- function(from=1, to=100000, length.out=6, base_sel = 10) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  base_sel**seq(log(from, base = base_sel), log(to, base = base_sel), length.out = length.out)
}


#Analysis within-type changes weather types using climate station data
f_wtc_cli <- function(wtc_data_in, clim_data_in, annu_analy, 
                      wtc_sel = my_wtc_sel, method_analy){
  
  start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  data_wtc <- data.frame(date = full_date,
                         value = with(wtc_data_in, wtc_data_in$valu[match(full_date, date)]))
  
  data_cli <- data.frame(date = full_date,
                         value = with(clim_data_in, clim_data_in$valu[match(full_date, date)]))
  
  n_wtcs <- length(wtc_sel)
  n_year <- length(start_year:end_year)
  
  
  f_wtc_annu <- function(input_wtc, input_cli, gwt_sel, my_annu_analy = annu_analy){
    
    #Remove 29th of February
    input_wtc <- input_wtc[-which(format(input_wtc$date, "%m%d") == "0229"),]
    input_cli <- input_cli[-which(format(input_cli$date, "%m%d") == "0229"),]
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day_wtc <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    data_day_cli <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day_wtc) <- c("year", days)
    colnames(data_day_cli) <- c("year", days)
    data_day_wtc[ ,1] <- start_year:end_year
    data_day_cli[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day_wtc[i+1, 2:366] <- input_wtc$valu[(i*365+1):((i+1)*365)]
      data_day_cli[i+1, 2:366] <- input_cli$valu[(i*365+1):((i+1)*365)]
      
    }
    
    wtc_out <- rep(NA, nrow(data_day_cli))
    
    for(i in 1:nrow(data_day_cli)){
      
      if(my_annu_analy == "mean"){
        
        wtc_out[i] <- mea_na(data_day_cli[i, which(data_day_wtc[i, ] == gwt_sel)])
        
      }
      
      if(my_annu_analy == "sum"){
        
        wtc_out[i] <- sum_na(data_day_cli[i, which(data_day_wtc[i, ] == gwt_sel)])
        
      }
      
    }
    
    return(wtc_out)
    
  }
  
  wtc_out <- matrix(data=rep(NA, n_wtcs*n_year), ncol = n_wtcs)
  
  for(i in 1:n_wtcs){
    
    wtc_out[, i] <- f_wtc_annu(input_wtc = data_wtc,
                               input_cli = data_cli,
                               gwt_sel = i)
  }
  
  if(method_analy == "mean"){
    
    wtc_return <- apply(wtc_out, 2, mea_na)
    
  }
  
  if(method_analy == "sens_slope"){
    wtc_sens_slope <- function(wtc_data_in, wtc_cover = 0.01){
      sens_slope(data_in = wtc_data_in, cover_thresh = wtc_cover)
    }
    
    wtc_return <- apply(wtc_out, 2, wtc_sens_slope) * 10 # per decade
    
  }
  
  
  return(wtc_return)
  
} 

#Analysis frequency and  change in frequency weather types
f_wtc_fre <- function(wtc_data_in, wtc_sel = my_wtc_sel, method_analy){
  
  start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  data_wtc <- data.frame(date = full_date,
                         value = with(wtc_data_in, wtc_data_in$valu[match(full_date, date)]))
  
  n_wtcs <- length(wtc_sel)
  n_year <- length(start_year:end_year)
  
  f_annu_fre <- function(input_wtc, gwt_sel){
    
    #Remove 29th of February
    input_wtc <- input_wtc[-which(format(input_wtc$date, "%m%d") == "0229"),]
    
    #Vector with the 365 days of the year
    days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
    days <- format(days,"%m-%d")
    
    #Order data by day
    data_day_wtc <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day_wtc) <- c("year", days)
    data_day_wtc[ ,1] <- start_year:end_year
    
    for(i in 0:(length(start_year:end_year)-1)) {
      
      data_day_wtc[i+1, 2:366] <- input_wtc$valu[(i*365+1):((i+1)*365)]
      
    }
    
    fre_wtc <- rep(NA, nrow(data_day_wtc))
    
    for(i in 1:nrow(data_day_wtc)){
      
      fre_wtc[i] <-length(which(data_day_wtc[i, ] == gwt_sel))
      
    }
    
    return(fre_wtc)
    
  }
  
  wtc_fre <- matrix(data=rep(NA, n_wtcs*n_year), ncol = n_wtcs)
  
  k <- 1
  for(i in wtc_sel){
    
    wtc_fre[, k] <- f_annu_fre(input_wtc = data_wtc,
                               gwt_sel = i)
    k <- k+1
  }
  
  #plot(wtc_fre[, 24], type = "l")
  
  if(method_analy == "mean"){
    
    wtc_fre_out <- apply(wtc_fre, 2, mea_na)
    
  }
  
  if(method_analy == "sens_slope"){
    
    f_sens_slope <- function(data_in, cover_thresh = 0.9){
      
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        sens_slo <-  NA
      }else{
        time_step <- 1:length(data_in)
        sens_slo <- as.numeric(zyp.sen(data_in~time_step)$coefficients[2])
        # sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
      }
      return(sens_slo)
    }
    
    wtc_fre_out <- apply(wtc_fre, 2, f_sens_slope) * 10 # per decaade
    
  }
  
  return(wtc_fre_out)
  
}



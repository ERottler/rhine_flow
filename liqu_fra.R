###

#Proxi for snow: solid/Liqid fraction for rainfall
#Erwin Rottler, University of Potsdam

###

stat_sel <- "BER" #SAM, BER
window_width <- 90
sta_yea_emd <- 1869
end_yea_emd <- 2016
my_enseble_size <- 10000
my_noise_strength <- 0.5
do_na_fil_emd <- T
do_scale_emd <- T

if(stat_sel == "SAM"){# Samedan
  
  temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74874/order_74874_data.txt"), sep = ";", skip = 2, header = T,
                         na.strings = "-")
  temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
  temp_emd <- data.frame(date    = temp_emd$date,
                         values  = temp_emd$ths200d0)
  
  rain_emd <- read.table(paste0(base_dir, "data/idaweb/order74874/order_74874_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
  rain_emd$date <- as.Date(strptime(rain_emd$time, "%Y%m%d", tz="UTC"))
  rain_emd <- data.frame(date   = rain_emd$date,
                         values = rain_emd$rhs150d0)
  
}

if(stat_sel == "BER"){# Bern
  
  temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T,
                         na.strings = "-")
  temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
  temp_emd <- data.frame(date    = temp_emd$date,
                         values  = temp_emd$ths200d0)
  
  rain_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
  rain_emd$date <- as.Date(strptime(rain_emd$time, "%Y%m%d", tz="UTC"))
  rain_emd <- data.frame(date   = rain_emd$date,
                         values = rain_emd$rhs150d0)
  
}

#Fill possible gaps and cut time series: Temperature
sta_day_emd <- paste0(sta_yea_emd, "-01-01")
end_day_emd <- paste0(end_yea_emd, "-12-31")

start_date <- as.POSIXct(strptime(sta_day_emd, "%Y-%m-%d", tz="UTC"))
end_date   <- as.POSIXct(strptime(end_day_emd, "%Y-%m-%d", tz="UTC"))
full_date  <- as.Date(seq(start_date, end_date, by="day"))

temp_emd <- data.frame(dates  = full_date,
                       values = with(temp_emd, values[match(as.Date(full_date), as.Date(date))])
                       )

rain_emd <- data.frame(dates  = full_date,
                       values = with(rain_emd, values[match(as.Date(full_date), as.Date(date))])
                       )

#Moving sum total rain
rain_emd$ms_all <- rollapply(data = rain_emd$values, width = window_width,
                             FUN = sum_na, align = "center", fill = NA)

# #Moving sum rain at positive temperatures (> 0 °C)
# rain_emd$snow <- rain_emd$values
# rain_emd$snow[which(temp_emd$values <= 0)] <- 0
# 
# rain_emd$ms_sno <- rollapply(data = rain_emd$snow, width = window_width,
#                              FUN = sum_na, align = "center", fill = NA)

#Moving sum only solid precipitation
rain_emd$snow <- rain_emd$values
rain_emd$snow[which(temp_emd$values > 0)] <- 0

rain_emd$ms_sno <- rollapply(data = rain_emd$snow, width = window_width,
                             FUN = mea_na, align = "center", fill = NA)

# rain_emd$lf <- rain_emd$ms_sno / rain_emd$ms_all *100

#Remove 29th of February
rain_emd <- rain_emd[-which(format(rain_emd$dates, "%m%d") == "0229"), ]

emd_day <- ord_day(data_in = rain_emd$ms_sno,
                   date = rain_emd$dates,
                   start_y = 1869,
                   end_y = 2016)

#fill NA with mean of day
if(do_na_fil_emd){
  
  emd_day <- apply(emd_day, 2, na2mea)
  
}

#Do CEEMDAN
emd_resid <- foreach::foreach(i = 1:ncol(emd_day), .combine = 'cbind') %dopar% {
  
  f_emd_resid(emd_day[, i])
  
}  

#Center results
if(do_scale_emd){
  
  #center restuls by substract mean (omitting NAs)
  emd_resid_meas <- apply(emd_resid, 2, mea_na)
  
  for (i in 1:ncol(emd_resid)) {
    
    emd_resid[, i] <- emd_resid[, i] - emd_resid_meas[i]
    
  }
  
}


plot_emd_val(emd_resid, n_iso = 8)







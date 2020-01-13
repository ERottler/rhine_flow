###

#Rhine flow observations - Empirical mode decomposition
#Erwin Rottler, University of Potsdam

###

#parameter----
vari_sel <- "rain" # disc, tem0, snow, rain, pres, wpre, rhum, clou, grdc
stat_sel <- "SIO" # Basel_Rheinhalle_2 (1869), Diepoldsau_2 (1919), Rekingen_2 (1904), Koeln (1824), Cochem (1901)
#BER, BAS, SMA, CHM, GVE, LUG, NEU, SAM, SIO
sta_yea_emd <- 1869
end_yea_emd <- 2016
window_width <- 90 
do_ma_emd <- T # do moving average
do_na_fil_emd <- T
do_scale_emd <- T
do_emd <- T
my_enseble_size <- 10000
my_noise_strength <- 0.5
do_fft <- F
smooth_par <- 10
do_loess <- F
my_span <- 0.90
my_poly_degree <- 1
rain_thres <- -1 #negative values > no threshold applied
do_mk <- T #do Mann Kendal test

#CEEMDAN_calc----

if(vari_sel == "tem0"){
  
  if(stat_sel == "BER"){# Bern / Zollikofen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BER  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BAS  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "SMA"){# Zuerich / Fluntern
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           SMA  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "CHM"){# Chaumont
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74870/order_74870_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           CHM  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "GVE"){# Geneve-Cointrin
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74871/order_74871_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           GVE  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "LUG"){# Lugano
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74872/order_74872_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           LUG  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "NEU"){# Neuchatel
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74873/order_74873_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           NEU  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "SAM"){# Samedan
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74874/order_74874_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           SAM  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "SIO"){# Sion
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order74875/order_74875_data.txt"), sep = ";", skip = 2, header = T,
                           na.strings = "-")
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           SIO  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "Hohenpeissenberg"){
    
    temp_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-06_11_13/TMK_MN004.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    temp_emd$date <- as.Date(strptime(temp_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = temp_emd$date,
                           Hohenpeissenberg = temp_emd$WERT)
    
  }
  
}

if(vari_sel == "rain"){
  
  if(stat_sel == "BER"){# Bern
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           BER = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$BER[which(data_emd$BER <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           BAS = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$BAS[which(data_emd$BAS <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "SMA"){# Zuerich
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           SMA = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$SMA[which(data_emd$SMA <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "CHM"){# Chaumont
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74870/order_74870_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           CHM = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$CHM[which(data_emd$CHM <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "GVE"){# Geneve-Cointrin
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74871/order_74871_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           GVE = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$GVE[which(data_emd$GVE <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "LUG"){# Lugano
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74872/order_74872_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           LUG = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$LUG[which(data_emd$LUG <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "NEU"){# Neuchatel
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74873/order_74873_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           NEU = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$NEU[which(data_emd$NEU <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "SAM"){# Samedan
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74874/order_74874_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           SAM = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$SAM[which(data_emd$SAM <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "SIO"){# Sion
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order74875/order_74875_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           SIO = data_emd$rhs150d0)
    
    #Only rainy days
    data_emd$SIO[which(data_emd$SIO <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "Freudenstadt_Kniebis"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_17_38/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           Freudenstadt_Kniebis = data_emd$WERT)
    
    #Only rainy days
    data_emd$Freudenstadt_Kniebis[which(data_emd$Freudenstadt_Kniebis <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "Karlsruhe"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_18_04/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           Karlsruhe = data_emd$WERT)
    
    #Only rainy days
    data_emd$Karlsruhe[which(data_emd$Karlsruhe <= rain_thres)] <- NA
    
  }
  
  if(stat_sel == "Hohenpeissenberg"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_18_09/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           Hohenpeissenberg = data_emd$WERT)
    
    #Only rainy days
    data_emd$Hohenpeissenberg[which(data_emd$Hohenpeissenberg <= rain_thres)] <- NA
    
    #length(which(data_emd$Hohenpeissenberg[which(format(data_emd$date, '%Y') == 1879)] == 0))
    
  }
  
  if(stat_sel == "Frankfurt_AM"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-06_14_16/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date   = data_emd$date,
                           Frankfurt_AM = data_emd$WERT)
    
    #Only rainy days
    data_emd$Frankfurt_AM[which(data_emd$Frankfurt_AM <= rain_thres)] <- NA
    
  }
  
}

if(vari_sel == "disc"){
  
  load(paste0(base_dir, "data/bafu/dis_new.RData")); dis <- dis_new; rm(dis_new)
  data_emd <- dis
  
}

if(vari_sel == "snow"){
  
  if(stat_sel == "BER"){# Bern / Zollikofen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order66632/order_66632_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd <- temp_emd[which(temp_emd$stn == "BER"),]
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BER  = temp_emd$hto000d0)
    
    if(is.factor(data_emd$BER)){
      data_emd$BER <-  as.numeric(as.character(data_emd$BER))
    }
    
  }
  
  if(stat_sel == "DAV"){# Davos
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order66632/order_66632_data.txt"), sep = ";", 
                           skip = 2, header = T, na.strings = "-")
    temp_emd <- temp_emd[which(temp_emd$stn == "DAV"),]
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           DAV  = temp_emd$hto000d0)
    
    if(is.factor(data_emd$DAV)){
      data_emd$DAV <-  as.numeric(as.character(data_emd$DAV))
    }

  }
  
  if(stat_sel == "EIN"){# Einsiedeln
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order66635/order_66635_data.txt"), sep = ";", 
                           skip = 2, header = T, na.strings = "-")
    temp_emd <- temp_emd[which(temp_emd$stn == "EIN"),]
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           EIN  = temp_emd$hto000d0)
    
    if(is.factor(data_emd$EIN)){
      data_emd$EIN <-  as.numeric(as.character(data_emd$EIN))
    }
    
  }
  
  if(stat_sel == "SMA"){# Zuerich / Fluntern
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order66635/order_66635_data.txt"), sep = ";", 
                           skip = 2, header = T, na.strings = "-")
    temp_emd <- temp_emd[which(temp_emd$stn == "SMA"),]
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           SMA  = temp_emd$hto000d0)
    
    if(is.factor(data_emd$SMA)){
      data_emd$SMA <-  as.numeric(as.character(data_emd$SMA))
    }
    
  }
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order66635/order_66635_data.txt"), sep = ";", 
                           skip = 2, header = T, na.strings = "-")
    temp_emd <- temp_emd[which(temp_emd$stn == "BAS"),]
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BAS  = temp_emd$hto000d0)
    
    if(is.factor(data_emd$BAS)){
      data_emd$BAS <-  as.numeric(as.character(data_emd$BAS))
    }
    
  }
  
  if(stat_sel == "DZUG"){# Zugspitze
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order62735/order_62735_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           DZUG = snow_emd$hto000d0)
    
  }
  
}

if(vari_sel == "pres"){
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order63476/order_63476_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BAS = snow_emd$prestad0)
    
  }
  
  if(stat_sel == "BER"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order63477/order_63477_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BER = snow_emd$prestad0)
    
  }
}

if(vari_sel == "wpre"){
  
  if(stat_sel == "BER"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order63477/order_63477_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BER = snow_emd$pva200d0)
    
  }
}

if(vari_sel == "rhum"){
  
  if(stat_sel == "BER"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order63477/order_63477_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BER = snow_emd$ure200d0)
    
  }
}

if(vari_sel == "clou"){
  
  if(stat_sel == "BER"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order63477/order_63477_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BER = snow_emd$nto000d0)
    
  }
}

if(vari_sel == "grdc"){
  
  data_emd <- grdc_data
  emd_sel <- data.frame(dates = grdc_data$date,
                        values= grdc_data$value)
  sta_day_emd <- paste0(sta_yea_emd, "-01-01")
  end_day_emd <- paste0(end_yea_emd, "-12-31")
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(sta_day_emd, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day_emd, "%Y-%m-%d", tz="UTC"))
  full_date  <- as.Date(seq(start_date, end_date, by="day"))
  
  emd_sel <- data.frame(date  = full_date,
                        values = with(emd_sel, values[match(as.Date(full_date), as.Date(dates))])
  )
  
  emd_val <- emd_sel$values[which(emd_sel$date == sta_day_emd):which(emd_sel$date == end_day_emd)]
  emd_dat <- emd_sel$date[which(emd_sel$date == sta_day_emd):which(emd_sel$date == end_day_emd)]
  emd_sel <- data.frame(dates  = emd_dat,
                        values = emd_val)
  
  
}else{
  
  sta_day_emd <- paste0(sta_yea_emd, "-01-01")
  end_day_emd <- paste0(end_yea_emd, "-12-31")
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(sta_day_emd, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day_emd, "%Y-%m-%d", tz="UTC"))
  full_date  <- as.Date(seq(start_date, end_date, by="day"))
  
  data_emd$values <- data_emd[, which(colnames(data_emd) == stat_sel)]
  data_emd <- data.frame(dates  = full_date,
                         values = with(data_emd, values[match(as.Date(full_date), as.Date(date))])
  )
  
  sta_day_emd <- paste0(sta_yea_emd, "-01-01")
  end_day_emd <- paste0(end_yea_emd, "-12-31")
  start_row <- which(data_emd$date == sta_day_emd)
  emd_val <- data_emd$values[which(data_emd$date == sta_day_emd):which(data_emd$date == end_day_emd)]
  emd_dat <- data_emd$date[which(data_emd$date == sta_day_emd):which(data_emd$date == end_day_emd)]
  emd_sel <- data.frame(dates  = emd_dat,
                        values = emd_val)
  
}

# #Fill possible gaps
# start_date <- as.POSIXct(strptime(sta_day_emd, "%Y-%m-%d", tz="UTC"))
# end_date   <- as.POSIXct(strptime(end_day_emd, "%Y-%m-%d", tz="UTC"))
# full_date  <- as.Date(seq(start_date, end_date, by="day"))
# 
# emd_sel <- data.frame(dates  = full_date,
#                        values = with(emd_sel, values[match(as.Date(full_date), as.Date(dates))])
# )

#Remove 29th of February
emd_sel <- emd_sel[-which(format(emd_sel$dates, "%m%d") == "0229"),]

#Moving average filter
if(do_ma_emd){

  emd_sel$values <- rollapply(data = emd_sel$values, width = window_width, FUN = mea_na, align = "center", fill = "extend")
  
}

#Vector with the 365 days of the year
days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
days <- format(days,"%m-%d")

#Order data by day
emd_day <-  matrix(NA, nrow = length(sta_yea_emd:end_yea_emd), ncol = 365)
colnames(emd_day) <- c(days)
rownames(emd_day) <- sta_yea_emd:end_yea_emd

for(i in 0:(length(sta_yea_emd:end_yea_emd)-1)) {
  
  emd_day[i+1, 1:365] <- emd_sel$values[(i*365+1):((i+1)*365)]
  
}

#fill NA with mean of day
if(do_na_fil_emd){
  
  na2mea <- function(data_in){
    
    data_in[which(is.na(data_in))] <- mea_na(data_in)
    return(data_in)
    
  } 
  
  emd_day <- apply(emd_day, 2, na2mea)
  
}

if(do_emd){
  
  #Function to do CEEMDAN and return residual
  f_emd_resid <- function(data_in){
    
    my_emd <- Rlibeemd::ceemdan(input = data_in, ensemble_size = my_enseble_size, noise_strength = my_noise_strength)
    emd_out <- my_emd[, ncol(my_emd)]
    
    if(length(emd_out) < 1){
      emd_out <- rep(NA, length(data_in))
    }
    
    return(emd_out)
  }
  
  emd_resid <- foreach::foreach(i = 1:ncol(emd_day), .combine = 'cbind') %dopar% {
    
    f_emd_resid(emd_day[, i])
    
  }  
  
}

if(do_fft){
  
  #function to smooth data with FFT
  myFFTsmooth <- function(data_in){
    
    smoothFFT(data_in, sd = smooth_par)
    
  }
  
  emd_resid <- foreach::foreach(i = 1:ncol(emd_day), .combine = 'cbind') %dopar% {
    
    myFFTsmooth(emd_day[, i])
    
  }  
  
}

if(do_loess){
  
  #function to smooth data with FFT
  myLoess <- function(data_in){
    
    loess_na(data_in, sm_span = my_span, poly_degree = my_poly_degree)
    
  }
  
  emd_resid <- foreach::foreach(i = 1:ncol(emd_day), .combine = 'cbind') %dopar% {
    
    myLoess(emd_day[, i])
    
  }  
  
}

if(do_mk){
    
  f_mk <- function(data_in){
      
    cover_thresh = 0.9
    
    if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        mann_ken <-  NA
      }else{
        mann_ken <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[6])
      }
      return(mann_ken)
    }
  
  emd_mk <- foreach::foreach(i = 1:ncol(emd_day), .combine = 'cbind') %dopar% {
    
    f_mk(emd_day[, i])
    
  } 
  
  emd_mk <- as.vector(emd_mk)
  
  # plot(emd_mk)
  # abline(h = 0.05)
  
}

#Center results
if(do_scale_emd){
  
  # emd_resid <- scale(emd_resid, center = TRUE, scale = FALSE)
  
  # #substract the minimum of each day
  # emd_resid_mins <- apply(emd_resid, 2, min_na)
  # 
  # for (i in 1:ncol(emd_resid)) {
  #   
  #   emd_resid[, i] <- emd_resid[, i] - emd_resid_mins[i]
  #   
  # }
  
  # #divide by column mean
  # emd_resid_meas <- apply(emd_resid, 2, mea_na)
  # 
  # for (i in 1:ncol(emd_resid)) {
  #   
  #   emd_resid[, i] <- emd_resid[, i] / emd_resid_meas[i]
  #   
  # }
  
  #center restuls by substract mean (omitting NAs)
  emd_resid_meas <- apply(emd_resid, 2, mea_na)

  for (i in 1:ncol(emd_resid)) {

    emd_resid[, i] <- emd_resid[, i] - emd_resid_meas[i]

  }
  
}


#CEEMDAN_visu----

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

n_max <- round(abs(max_na(emd_resid[, ])) / (max_na(emd_resid[, ]) + abs(min_na(emd_resid[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_resid), max_na(emd_resid),length.out = length(cols_emd)+1))

# cols_emd <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white",
#                                           "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)
# max_break <- max_na(emd_resid)
# min_break <- min_na(emd_resid)
# qua_break <- quantile(emd_resid, probs = 0.5, type = 8, na.rm = T)
# 
# breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
# breaks_2 <- seq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
# breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + min_na(breaks_1)/100
# 
# brea_emd <- c(breaks_1, breaks_2)


image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_resid), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Year", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.01)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(emd_resid), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("CEEMDAN residual (scaled)", side = 4, line = 1.5, cex = 0.8)

box()




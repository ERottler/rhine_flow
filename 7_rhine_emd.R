###

#Rhine flow observations - Empirical mode decomposition
#Erwin Rottler, University of Potsdam

###

#parameter----
vari_sel <- "disc" # disc, tem0, snow, rain, pres, wpre, rhum, clou
stat_sel <- "Basel_Rheinhalle_2" # Basel_Rheinhalle_2 (1869), Diepoldsau_2 (1919), Rekingen_2 (1904), Koeln (1824), Cochem (1901), BER (1864), BAS (1864), DZUG (1901)
sta_yea_emd <- 1869
end_yea_emd <- 2017
window_width <- 30 
do_ma_emd <- T # do moving average
do_na_fil_emd <- T
do_center_emd <- T
my_enseble_size <- 2000
my_noise_strength <- 0.5

#CEEMDAN_calc----

if(vari_sel == "tem0"){
  
  if(stat_sel == "BER"){# Bern / Zollikofen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order62409/order_62409_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BER  = temp_emd$tre200d0)
    
  }
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order63459/order_63459_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = temp_emd$date,
                           BAS  = temp_emd$tre200d0)
    
  }
  
}

if(vari_sel == "disc"){
  
  load(paste0(base_dir, "data/bafu/dis_new.RData")); dis <- dis_new; rm(dis_new)
  data_emd <- dis
  
}

if(vari_sel == "snow"){
  
  if(stat_sel == "DZUG"){# Zugspitze
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order62735/order_62735_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           DZUG = snow_emd$hto000d0)
    
  }
  
}

if(vari_sel == "rain"){
  
  if(stat_sel == "BAS"){# Basel / Binningen
    
    snow_emd <- read.table(paste0(base_dir, "data/idaweb/order62545/order_62545_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    snow_emd$date <- as.Date(strptime(snow_emd$time, "%Y%m%d", tz="UTC"))
    data_emd <- data.frame(date = snow_emd$date,
                           BAS = snow_emd$rhs150d0)
    
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

sta_day_emd <- paste0(sta_yea_emd, "-01-01")
end_day_emd <- paste0(end_yea_emd, "-12-31")
start_row <- which(data_emd$date == sta_day_emd)
emd_val <- data_emd[which(data_emd$date == sta_day_emd):which(data_emd$date == end_day_emd), which(colnames(data_emd) == stat_sel)]
emd_dat <- data_emd$date[which(data_emd$date == sta_day_emd):which(data_emd$date == end_day_emd)]
emd_sel <- data.frame(dates  = emd_dat,
                      values = emd_val)

#Fill possible gaps
start_date <- as.POSIXct(strptime(sta_day_emd, "%Y-%m-%d", tz="UTC"))
end_date   <- as.POSIXct(strptime(end_day_emd, "%Y-%m-%d", tz="UTC"))
full_date  <- as.Date(seq(start_date, end_date, by="day"))

emd_sel <- data.frame(dates  = full_date,
                      values = with(emd_sel, values[match(as.Date(full_date), as.Date(dates))])
)

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

if(do_center_emd){
  
  emd_resid <- scale(emd_resid, center = T, scale = F)
  
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

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(min_na(emd_resid), max_na(emd_resid),length.out = length(cols_scale)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_resid), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Year", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.01)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(emd_resid), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("CEEMDAN residual (centered) [°C]", side = 4, line = 1.5, cex = 0.8)

box()




###

#Rhine flow observations - EMD meets GPD
#Erwin Rottler, University of Potsdam

###

#parameter----

vari_annu <- "rain" # disc, rain, tem0, grdc
stat_annu <- "SMA" # Basel_Rheinhalle_2 (1869), BAS (1864), BER (1864), Cochem (1901), Koeln (1824), Diepoldsau_2,
                                # Freudenstadt_Kniebis, Karlsruhe (1876), Hohenpeissenberg, Frankfurt_AM, SMA
sta_yea_ann <- 1869
end_yea_ann <- 2016
my_break_day <- 0  # 1-Oct: 274 (Switzerland), 1-Nov: 305 (Germany)
quants <- seq(0.01, 0.99, by = 0.01)
quant_method <- "gpd" #empirical, gev, gpd
rain_thres <- 2 #threshold rainy day (below or equal set to NA)
my_cover_threshold <- 0.9999
do_emd <- T
my_enseble_size <- 10000
my_noise_strength <- 0.5
do_fft <- F
smooth_par <- 10
do_loess <- F
my_span <- 0.90
my_poly_degree <- 1
do_na_fil_emd <-  T
do_seasons <- T
seas_sel <- "winter" #spring, summer, autumn, winter

#annual_cal----

if(vari_annu == "disc"){
  
  load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
  dis <- dis_new ; dis_new <- NULL
  
  cols_sel <- sapply(stat_annu, sel_dis) #columns with selected gauges
  dat_annu <- data.frame(date   = dis$date,
                         values = dis[, cols_sel]) #extract selected time series from data frame
  
}

if(vari_annu == "rain"){
  
  if(stat_annu == "BER"){# Bern
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$rhs150d0)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "BAS"){# Basel / Binningen
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$rhs150d0)

    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "SMA"){# Zuerich
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$rhs150d0)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "Freudenstadt_Kniebis"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_17_38/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$WERT)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "Karlsruhe"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_18_04/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$WERT)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "Hohenpeissenberg"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_18_09/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$WERT)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
  if(stat_annu == "Frankfurt_AM"){
    
    data_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-06_14_16/RS_MN006.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$WERT)
    
    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
}

if(vari_annu == "tem0"){
  
  if(stat_annu == "BER"){# Bern / Zollikofen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date    = temp_emd$date,
                           values  = temp_emd$ths200d0)
    
  }
  
  if(stat_annu == "BAS"){# Basel / Binningen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date    = temp_emd$date,
                           values  = temp_emd$ths200d0)
    
  }
  
  if(stat_annu == "SMA"){# Zürich /Fluntern
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date    = temp_emd$date,
                           values  = temp_emd$ths200d0)
    
  }
  
  if(stat_sel == "Hohenpeissenberg"){
    
    temp_emd <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-06_11_13/TMK_MN004.txt"), sep = ";", skip = 0, header = T, na.strings = c("-"))
    temp_emd$date <- as.Date(strptime(temp_emd$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = temp_emd$date,
                           values = temp_emd$WERT)
    
  }
  
}

if(vari_annu == "grdc"){
  
  dat_annu <- data.frame(date   = grdc_data$date,
                         values = grdc_data$value) #extract selected time series from data frame
  
}

#Seasonal analysis

if(do_seasons){
  
  if(seas_sel == "winter"){
    dat_annu$values[which(!format(dat_annu$date, '%m') %in% c("12","01","02"))] <- NA
  }
  
  if(seas_sel == "spring"){
    dat_annu$values[which(!format(dat_annu$date, '%m') %in% c("03","04","05"))] <- NA
  }
  if(seas_sel == "summer"){
    dat_annu$values[which(!format(dat_annu$date, '%m') %in% c("06","07","08"))] <- NA
  }
  if(seas_sel == "autumn"){
    dat_annu$values[which(!format(dat_annu$date, '%m') %in%c("09","10","11"))] <- NA
  }
  
  
}

         
#Calculate all quantiles for each year
f_annu_quant <- function(my_quant){
  
  dis_ana(disc = dat_annu$values, 
          date = dat_annu$date, 
          start_year = sta_yea_ann, 
          end_year = end_yea_ann,
          method_analys = "quantile",
          quant_in = my_quant,
          method_quant = quant_method, 
          quant_annual = T,
          break_day = my_break_day,
          cover_thresh = my_cover_threshold
  )
  
}

qannu <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_annu_quant(k)
}




#fill NA with mean of day
if(do_na_fil_emd){
  
  na2mea <- function(data_in){
    
    data_in[which(is.na(data_in))] <- mea_na(data_in)
    return(data_in)
    
  } 
  
  qannu <- apply(qannu, 2, na2mea)
  
}

if(do_fft){
  
  #function to smooth data with FFT
  myFFTsmooth <- function(data_in){
    
    smoothFFT(data_in, sd = smooth_par)
  
  }
  
  qannu_resid <- foreach::foreach(i = 1:ncol(qannu), .combine = 'cbind') %dopar% {
    
    myFFTsmooth(qannu[, i])
    
  }
  
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
  
  qannu_resid <- foreach::foreach(i = 1:ncol(qannu), .combine = 'cbind') %dopar% {
    
    f_emd_resid(qannu[, i])
    
  }
  
}
if(do_loess){
  
  #function to smooth data with FFT
  myLoess <- function(data_in){
    
    loess_na(data_in, sm_span = my_span, poly_degree = my_poly_degree )
    
  }
  
  qannu_resid <- foreach::foreach(i = 1:ncol(qannu), .combine = 'cbind') %dopar% {
    
    myLoess(qannu[, i])
    
  }
  
}

#Center data
# qannu_resid <- scale(qannu_resid, center = T, scale = F)

qannu_resid_meas <- apply(qannu_resid, 2, mea_na)

for (i in 1:ncol(qannu_resid)) {
  
  qannu_resid[, i] <- qannu_resid[, i] - qannu_resid_meas[i]
  
}


#annual_vis----

x_axis_tic <- seq(10, 90, by = 10)
par(mar = c(3, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

n_max <- round(abs(alptempr::max_na(qannu_resid[, ])) / (alptempr::max_na(qannu_resid[, ]) + abs(alptempr::min_na(qannu_resid[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_resid), alptempr::max_na(qannu_resid),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_resid), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.01, mgp=c(3, 0.15, 0))#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.01)
mtext("Year", side = 2, line = 1.5, cex = 0.8)
mtext("Quantile", side = 1, line = 1.5, cex = 0.8)
box()

par(mar = c(3, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(qannu_resid), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("CEEMDAN residual (centered) [°C]", side = 4, line = 1.5, cex = 0.8)

box()



#Berry quantiles----

# Usage of quantile estimation method largely depending on sample size
# Roughly when having more than 100 than empirical is fine
# for window try gdp, wak an weighted
# gdp usually only for tail of distribution (e.g. > 90%)
# wak used in US?

# Next:
# >adapt function, different parallelization (determine parameters of distribution only once and then extract all desired probs)



str(dat_annu)

round( extremeStat::distLquantile(dat_annu$values[47000 + 1:30], truncate=0, probs=c(0:9,9.5,9.9)/10, selection="gpa", gpd=FALSE, weighted=FALSE) )
library(extremeStat)
for(i in sample(30000:nrow(dat_annu), size = 50))
plotLquantile(distLquantile(dat_annu$values[59000 + 1:90], list=T, truncate=0, gpd=FALSE, weighted=FALSE), nbest=10)


( distLquantile(dat_annu$values[59000 + 1:90], truncate=0, gpd=FALSE) )

#get quantile estimations from different distributions (based on R package lmomco)
distLquantile(dat_annu$values[59000 + 1:90], truncate=0, gpd=FALSE)["weighted3", -4] # letzte Spalte RMSE
#truncate: how much of data used to fit distribution       
#weighted, wak, gpd, 
       
              
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

GPDquantile(dat_annu$values[47000 + 1:30], probs=c(0:9,9.5,9.9)/10 )


compare_gpd_empirical <- function(i)
{
 x <- dat_annu$values[i + 1:30]
 param <- GPDquantile(x, probs=quants)
 empir <- quantile(x, probs=quants, type=8)
 #plot(quants, param, type="l", log="y", ylim=range(param,empir), yaxt="n")
 #berryFunctions::logAxis(2)
 lines(quants, param)
 lines(quants, empir, col="red")
}

plot(0:1, 0:1, ylim=c(500,1500), type="n")
for(i in sample(30000:nrow(dat_annu), size = 20)) compare_gpd_empirical(i)

###

#Rhine flow observations - EMD meets GEV
#Erwin Rottler, University of Potsdam

###


#parameter----

vari_annu <- "disc" # disc, rain, tem0
stat_annu <- "Cochem" # Basel_Rheinhalle_2 (1869), BAS (1864), BER (1864), Cochem (1901), Koeln (1824)
sta_yea_ann <- 1901
end_yea_ann <- 2011
my_break_day <- 0  # 1-Oct: 274 (Switzerland), 1-Nov: 305 (Germany)
quants <- seq(0.01, 0.99, by = 0.01)
quant_method <- "empirical" #empirical, gev
rain_thres <- 5 #threshold rainy day (below or equal to NA)
my_cover_threshold <- 0.999

#annual_cal----

if(vari_annu == "disc"){
  
  load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
  dis <- dis_new ; dis_new <- NULL
  
  cols_sel <- sapply(stat_annu, sel_dis) #columns with selected gauges
  dat_annu <- data.frame(date   = dis$date,
                         values = dis[, cols_sel]) #extract selected time series from data frame
  
}

if(vari_annu == "rain"){
  
  if(stat_annu == "BAS"){# Basel / Binningen
    
    data_emd <- read.table(paste0(base_dir, "data/idaweb/order62545/order_62545_data.txt"), sep = ";", skip = 2, header = T, na.strings = c("-"))
    data_emd$date <- as.Date(strptime(data_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date   = data_emd$date,
                           values = data_emd$rhs150d0)

    #Only rainy days
    dat_annu$values[which(dat_annu$values <= rain_thres)] <- NA
    
  }
  
}

if(vari_annu == "tem0"){
  
  if(stat_annu == "BER"){# Bern / Zollikofen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order62409/order_62409_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date    = temp_emd$date,
                           values  = temp_emd$tre200d0)
    
  }
  
  if(stat_annu == "BAS"){# Basel / Binningen
    
    temp_emd <- read.table(paste0(base_dir, "data/idaweb/order63459/order_63459_data.txt"), sep = ";", skip = 2, header = T)
    temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
    dat_annu <- data.frame(date    = temp_emd$date,
                           values  = temp_emd$tre200d0)
    
  }
  
}

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

qannu_resid <- scale(qannu_resid, center = T, scale = F)


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


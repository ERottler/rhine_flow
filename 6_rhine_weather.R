###

#Analysis weather type data
#Erwin Rottler, University of Potsdam

###


start_year <- 1960
end_year <- 2017
cover_thres <- 0.9

  
#wtc_calc----  
  
gwt26geo_data <- read.table("U:/RhineFlow/rhine_obs/data/idaweb/order60448/order_60448_data.txt",
                            sep = ";", skip = 1, header = TRUE, na.strings = "-")

gwt26msl_data <- read.table("U:/RhineFlow/rhine_obs/data/idaweb/order60446/order_60446_data.txt",
                            sep = ";", skip = 1, header = TRUE, na.strings = "-")

cap27msl_data <- read.table("U:/RhineFlow/rhine_obs/data/idaweb/order60779/order_60779_data.txt",
                            sep = ";", skip = 1, header = TRUE, na.strings = "-")

ncl40ali_data <- read.table("U:/RhineFlow/rhine_obs/data/WT_Aline/ERA20C_set70_ncl40_slo326_sla4358_seq1.cat",
                            header = F, na.strings = "-")

gwt26geo_data$time <- as.POSIXct(strptime(gwt26geo_data$time, "%Y%m%d", tz="UTC"))
gwt26msl_data$time <- as.POSIXct(strptime(gwt26msl_data$time, "%Y%m%d", tz="UTC"))
cap27msl_data$time <- as.POSIXct(strptime(cap27msl_data$time, "%Y%m%d", tz="UTC"))
ncl40ali_data$time <- as.POSIXct(strptime(paste(ncl40ali_data$V1,ncl40ali_data$V2, 
                                                ncl40ali_data$V3),"%Y%m%d", tz="UTC"))

start_day <- paste0(start_year, "-01-01")
end_day   <- paste0(end_year,   "-12-31")

start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
full_date  <- seq(start_date, end_date, by="day")

data_gwt26geo <- data.frame(date = full_date,
                            value = with(gwt26geo_data, gwt26geo_data$wkwtg3d0[match(full_date, gwt26geo_data$time)]))
data_gwt26msl <- data.frame(date = full_date,
                            value = with(gwt26msl_data, gwt26msl_data$wkwtp3d0[match(full_date, gwt26msl_data$time)]))
data_cap27msl <- data.frame(date = full_date,
                            value = with(cap27msl_data, cap27msl_data$wkcap3d0[match(full_date, cap27msl_data$time)]))
data_ncl40ali <- data.frame(date = full_date,
                            value = with(ncl40ali_data, ncl40ali_data$V5[match(full_date, ncl40ali_data$time)]))

#sub-basin average values for rangking of weather types
temp_basin <- apply(temps, 1, med_na)
prec_basin <- apply(precs, 1, mea_na) 

if(wt_clim_data == "temperature"){
  clim_data <- temp_basin
}
if(wt_clim_data == "rainfall"){
  clim_data <- prec_basin
}

if(wt_wt_data == "gwt26geo"){
  wt_data <- data_gwt26geo
}
if(wt_wt_data == "gwt26msl"){
  wt_data <- data_gwt26msl
}
if(wt_wt_data == "cap27msl"){
  wt_data <- data_cap27msl
}
if(wt_wt_data == "ncl40ali"){
  wt_data <- data_ncl40ali
}

#average value of climate variable for weather types
wt_clim_med <- gwt_med(dates = full_date, clim_data = clim_data, gwt_data = wt_data$value, numb_wt=wt_num)

if(do_mean_wt_clim){
  wt_clim_med <- gwt_mea(dates = full_date, clim_data = clim_data, gwt_data = wt_data$value, numb_wt=wt_num)
}

#get rank out of mean values
wt_rank <- matrix(NA, ncol = wt_num, nrow = 365)
for (i in 1:365) {
  
  wt_clim_med_sort <- sort(wt_clim_med[i, ])
  
  if(length(wt_clim_med_sort) > length(c(wt_low, wt_hig))){
    gwt_low <- as.numeric(names(wt_clim_med_sort)[wt_low])
    gwt_hig <- as.numeric(names(wt_clim_med_sort)[(length(wt_clim_med_sort)-(length(wt_hig)-1)) : length(wt_clim_med_sort)])
  }else{
    if(is.even(length(wt_clim_med_sort))){
      gwt_low <- as.numeric(names(wt_clim_med_sort)[1:(length(wt_clim_med_sort) / 2)])
      gwt_hig <- as.numeric(names(wt_clim_med_sort)[((length(wt_clim_med_sort) / 2) + 1) : length(wt_clim_med_sort)])
    }else{
      gwt_low <- as.numeric(names(wt_clim_med_sort)[1:(floor(length(wt_clim_med_sort) / 2))])
      gwt_hig <- as.numeric(names(wt_clim_med_sort)[ceiling((length(wt_clim_med_sort) / 2)) : length(wt_clim_med_sort)])
    }
  }
  wt_rank[i, gwt_low] <-  -1
  wt_rank[i, gwt_hig] <-   1
  
}


#Calculate changes in frequencies
if(wt_slo == "fixed"){
  
  #Determine driving weather types
  wt_rank_sum <- apply(wt_rank[,], 2, sum_na)
  
  names(wt_rank_sum) <- 1:length(wt_rank_sum)
  
  wt_rank_sum_sort <- sort(wt_rank_sum)
  
  low_wts  <- as.numeric(names(wt_rank_sum_sort)[wt_low])
  hig_wts  <- as.numeric(names(wt_rank_sum_sort)[wt_hig])
  
  wt_hig_slo <- moving_analys(dates = wt_data$date, values = wt_data$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = hig_wts)*100*10# [%/dec]
  
  wt_low_slo <- moving_analys(dates = wt_data$date, values = wt_data$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = low_wts)*100*10 # [%/dec]
}

if(wt_slo == "flexi"){
  
  wt_hig_slo <- wt_flex_mov_2(wt_data = wt_data$value, wt_rank = wt_rank, date = wt_data$date , hig_low = 1, 
                              start_year = start_year, end_year = end_year, window_width = window_width, 
                              cover_thres = cover_thres, wts_numb = length(wt_hig))*100*10 # [%/dec]
  
  wt_low_slo <- wt_flex_mov_2(wt_data = wt_data$value, wt_rank = wt_rank, date = wt_data$date , hig_low = -1, 
                              start_year = start_year, end_year = end_year, window_width = window_width, 
                              cover_thres = cover_thres, wts_numb = length(wt_low))*100*10 # [%/dec]
  
}
  

#wtc_visu----

if(do_wtc_visu){
  
  pdf(paste0(base_dir, "figs/", basin_sel, "_", wt_wt_data, "_", wt_clim_data, "_", wt_slo, ".pdf"), width = 7.09, height = 2.5)
  
  par(oma = c(0,0,0,0))
  par(family = "serif")
  par(mfrow = c(1,2))
  
  y <- 1:wt_num
  x <- 1:365
  
  #Plot 1: Discharge - Weather type ranking high / low
  par(mar = c(1, 2.2, 1.5, 0.6))
  
  col_lows  <- "darkblue"
  col_highs <- "darkorange3" #firebrick
  col_hig_im <- "darkorange3"
  col_low_im <- "darkblue"
  col_net <- "black"
  
  wt_max <- max_na(c(loess_NA_restore(wt_low_slo),
                     loess_NA_restore(wt_hig_slo),
                     (loess_NA_restore(wt_hig_slo) - loess_NA_restore(wt_low_slo))))+2
  
  wt_min <- min_na(c(loess_NA_restore(wt_low_slo),
                     loess_NA_restore(wt_hig_slo),
                     (loess_NA_restore(wt_hig_slo) - loess_NA_restore(wt_low_slo))))-1
  
  image(x, y, as.matrix(wt_rank), col = c(col_low_im, col_hig_im), breaks = c(-2, 0, 2), ylab = "",
        xlab = "", main = "", axes = F)
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15
  
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.04, cex.axis = 0.7)
  mtext(paste0(wt_wt_data, " weather type"), side = 2, line = 1.5, padj = 1, cex = 0.8)
  box()
  mtext(paste0("Weather types: ", wt_clim_data), side = 3, line = 0.8, padj = 1, at = 385, cex = 1)
  
  if(wt_slo == "fixed"){
    #Add markers for selected weather types
    par(xpd = T)
    
    points_x_low <- rep(375, length(low_wts))
    points_y_hig <- rep(375, length(hig_wts))
    points_y_low  <- c(low_wts)
    points_y_high <- c(hig_wts)
    
    Arrows(points_x_low, points_y_low, points_x_low+5,  points_y_low, col = col_lows,
           arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)
    
    Arrows(points_y_hig, points_y_high, points_y_hig+5,  points_y_high, col = col_highs,
           arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)
    
    par(xpd = F)
  }
  
  #Plot 2: Temperature - Window trends frequencies
  par(mar = c(1, 0.2, 1.5, 2))
  
  plot(wt_low_slo, type = "n", main ="", ylim = c(wt_min, wt_max), ylab = "", xlab = "", axes = F)
  lines(loess_NA_restore(wt_low_slo), col = col_lows, lwd = 2)
  lines(loess_NA_restore(wt_hig_slo), col = col_highs, lwd = 2)
  lines(loess_NA_restore(wt_hig_slo) - loess_NA_restore(wt_low_slo), col = col_net, lwd = 2)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
  axis(4, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
  abline(h = 0, lty = "dashed", lwd = 0.9)
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  legend("topleft", c("            ","            "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
  mtext("hig GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = col_highs)
  mtext("low GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = col_lows)
  mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = col_net)
  box()
  
  mtext("Trend window prob. [%/dec]", side = 4, line = 0.3, padj = 1, cex = 0.8)
  
  dev.off()
  
  
  
}



#Preparation correlation test
if(T){
  gwt26geo_data <- read.table(paste0(base_dir, "/data/idaweb/order60448/order_60448_data.txt"),
                              sep = ";", skip = 1, header = TRUE, na.strings = "-")
  
  gwt26msl_data <- read.table(paste0(base_dir, "/data/idaweb/order60446/order_60446_data.txt"),
                              sep = ";", skip = 1, header = TRUE, na.strings = "-")
  
  cap27msl_data <- read.table(paste0(base_dir, "/data/idaweb/order60779/order_60779_data.txt"),
                              sep = ";", skip = 1, header = TRUE, na.strings = "-")
  
  ncl40ali_data <- read.table(paste0(base_dir, "/data/WT_Aline/ERA20C_set70_ncl40_slo326_sla4358_seq1.cat"),
                              header = F, na.strings = "-")
  
  gwt26geo_data$time <- as.POSIXct(strptime(gwt26geo_data$time, "%Y%m%d", tz="UTC"))
  gwt26msl_data$time <- as.POSIXct(strptime(gwt26msl_data$time, "%Y%m%d", tz="UTC"))
  cap27msl_data$time <- as.POSIXct(strptime(cap27msl_data$time, "%Y%m%d", tz="UTC"))
  ncl40ali_data$time <- as.POSIXct(strptime(paste(ncl40ali_data$V1,ncl40ali_data$V2, 
                                                  ncl40ali_data$V3),"%Y%m%d", tz="UTC"))
  
  start_day <- paste0(start_year, "-01-01")
  end_day   <- paste0(end_year,   "-12-31")
  
  start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  data_gwt26geo <- data.frame(date = full_date,
                              value = with(gwt26geo_data, gwt26geo_data$wkwtg3d0[match(full_date, gwt26geo_data$time)]))
  data_gwt26msl <- data.frame(date = full_date,
                              value = with(gwt26msl_data, gwt26msl_data$wkwtp3d0[match(full_date, gwt26msl_data$time)]))
  data_cap27msl <- data.frame(date = full_date,
                              value = with(cap27msl_data, cap27msl_data$wkcap3d0[match(full_date, cap27msl_data$time)]))
  data_ncl40ali <- data.frame(date = full_date,
                              value = with(ncl40ali_data, ncl40ali_data$V5[match(full_date, ncl40ali_data$time)]))
  
  #sub-basin average values for rangking of weather types
  temp_basin <- apply(temps, 1, med_na)
  prec_basin <- apply(precs, 1, mea_na) 
  
  if(wt_clim_data == "temperature"){
    clim_data <- temp_basin
  }
  if(wt_clim_data == "rainfall"){
    clim_data <- prec_basin
  }
  
  if(wt_wt_data == "gwt26geo"){
    wt_data <- data_gwt26geo
  }
  if(wt_wt_data == "gwt26msl"){
    wt_data <- data_gwt26msl
  }
  if(wt_wt_data == "cap27msl"){
    wt_data <- data_cap27msl
  }
  if(wt_wt_data == "ncl40ali"){
    wt_data <- data_ncl40ali
  }
  
  #average value of climate variable for weather types
  wt_clim_med <- gwt_med(dates = full_date, clim_data = clim_data, gwt_data = wt_data$value, numb_wt=wt_num)
  
  if(do_mean_wt_clim){
    wt_clim_med <- gwt_mea(dates = full_date, clim_data = clim_data, gwt_data = wt_data$value, numb_wt=wt_num)
  }
  
  #get rank out of mean values
  wt_rank <- matrix(NA, ncol = wt_num, nrow = 365)
  for (i in 1:365) {
    
    wt_clim_med_sort <- sort(wt_clim_med[i, ])
    
    if(length(wt_clim_med_sort) > length(c(wt_low, wt_hig))){
      gwt_low <- as.numeric(names(wt_clim_med_sort)[wt_low])
      gwt_hig <- as.numeric(names(wt_clim_med_sort)[(length(wt_clim_med_sort)-(length(wt_hig)-1)) : length(wt_clim_med_sort)])
    }else{
      if(is.even(length(wt_clim_med_sort))){
        gwt_low <- as.numeric(names(wt_clim_med_sort)[1:(length(wt_clim_med_sort) / 2)])
        gwt_hig <- as.numeric(names(wt_clim_med_sort)[((length(wt_clim_med_sort) / 2) + 1) : length(wt_clim_med_sort)])
      }else{
        gwt_low <- as.numeric(names(wt_clim_med_sort)[1:(floor(length(wt_clim_med_sort) / 2))])
        gwt_hig <- as.numeric(names(wt_clim_med_sort)[ceiling((length(wt_clim_med_sort) / 2)) : length(wt_clim_med_sort)])
      }
    }
    wt_rank[i, gwt_low] <-  -1
    wt_rank[i, gwt_hig] <-   1
    
  }
  
  wt_rank_sum <- apply(wt_rank[,], 2, sum_na)
  
  names(wt_rank_sum) <- 1:length(wt_rank_sum)
  
  wt_rank_sum_sort <- sort(wt_rank_sum)
  
}

slo_med <- apply(tslo_reuss, 1, med_na)
wt_test <- cbind(c(rep(1, 4),  rep(1, 4),  rep(1, 5),  rep(1, 5),  rep(1, 6),  rep(1, 6),  rep(1, 7),  rep(1, 7)), 
                 c(rep(4, 4),  1:4,        rep(5, 5),  1:5,        rep(6, 6),  1:6,        rep(7, 7),  1:7),
                 c(23:26,      rep(23, 4), 22:26,      rep(22, 5), 21:26,      rep(21, 6), 20:26,      rep(20, 7)),
                 c(rep(26, 4), rep(26, 4), rep(26, 5), rep(26, 5), rep(26, 6), rep(26, 6), rep(26, 7), rep(26, 7)))

for(i in 1:nrow(wt_test)){
  
  wt_low <- wt_test[i, 1] : wt_test[i, 2] #selected low weather types for trend analysis
  wt_hig <- wt_test[i, 3] : wt_test[i, 4] #selected high weather types for trend analysis
  
  low_wts  <- as.numeric(names(wt_rank_sum_sort)[wt_low])
  hig_wts  <- as.numeric(names(wt_rank_sum_sort)[wt_hig])
  
  wt_hig_slo <- moving_analys(dates = wt_data$date, values = wt_data$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = hig_wts)*100*10# [%/dec]
  
  wt_low_slo <- moving_analys(dates = wt_data$date, values = wt_data$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = low_wts)*100*10 # [%/dec]
  
  wte_index_sing <- wt_hig_slo - wt_low_slo
  
  cor_out_sing <- cor(slo_med, wte_index_sing, method = "spearman")
  
  if(i == 1){
    wte_index <- wte_index_sing
    cor_out <- cor_out_sing
  }else{
    wte_index <- cbind(wte_index, wte_index_sing)
    cor_out <- c(cor_out, cor_out_sing)
  }
  
}

wt_test[which(cor_out == max(cor_out)), ]

plot(slo_med, type = "l")
par(new = TRUE)
plot(wte_index[, which(cor_out == max(cor_out))], type = "l", col = "red3")


#Focus pluvial regime
#New stuff on precipitation

length(prec_basin)
length(which(prec_basin > 20))

quantile(prec_basin, quant_prec)

hist(prec_basin[prec_basin > 10])
boxplot(prec_basin[prec_basin > 1])

quant_prec <- c(0.750, 0.800, 0.850, 0.900, 0.950)

#Quantile probability

Sys.time()
f_qpre <- function(quant_sel){dis_ana(disc = prec_basin,
                                      date = meteo_date,
                                      start_year = start_year,
                                      end_year = end_year,
                                      quant_in = quant_sel,
                                      window_width = window_width,
                                      method_analys = "quantile_prob",
                                      method_quant = "gev"
)}

qprec <- mapply(f_qpre, quant_prec, SIMPLIFY = T)
Sys.time()


#Trend moving quantile probability
Sys.time()
f_mpre <- function(quant_sel){dis_ana(disc = prec_basin,
                                      date = meteo_date,
                                      start_year = start_year,
                                      end_year = end_year,
                                      quant_in = quant_sel,
                                      window_width = window_width,
                                      method_analys = "mov_quant_prob_trend",
                                      method_quant = "gev"
)}

qmpre <- mapply(f_mpre, quant_prec, SIMPLIFY = T)
Sys.time()


plot(qprec[,1], type = "n", ylim = c(0, max(qprec)))
for(i in 1:ncol(qprec)){
  lines(smoothFFT(qprec[, i], sd =2))
}

plot(qmpre[,1], type = "n", ylim = c(min(qmpre), max(qmpre)))
for(i in 1:ncol(qmpre)){
  lines(smoothFFT(qmpre[, i], sd =2))
}


#Extreme frequency Nov-Apr

quant_prec <- c(0.950)

f_extr_frequ <- function(quant_in){
  
  frequ_years <- start_year:end_year
  
  for(i in 1: length(frequ_years)){
    meteo_sel_year <- meteo_date[which(format(meteo_date, "%Y") == frequ_years[i])]
    prec_basin_year <-prec_basin[meteo_sel_year] 
    
    meteo_sel_months <- c(which(format(meteo_sel_year, "%m") == "11"),
                          which(format(meteo_sel_year, "%m") == "12"),
                          which(format(meteo_sel_year, "%m") == "01"),
                          which(format(meteo_sel_year, "%m") == "02"),
                          which(format(meteo_sel_year, "%m") == "03"),
                          which(format(meteo_sel_year, "%m") == "04"))
    
    prec_basin_months <-prec_basin_year[meteo_sel_months] 
    
    frequ_year <- length(which(prec_basin_months > quantile(prec_basin, quant_in)))
    
    if(i == 1){
      frequ_out <- frequ_year
    }else{
      frequ_out <- c(frequ_out, frequ_year)
    }
  }
  
  return(frequ_out)
}


frequ <- f_extr_frequ(0.8)
frequ_out <- mapply(f_extr_frequ, quant_prec, SIMPLIFY = T)

plot(frequ_out[, 1], type = "n", ylim = c(0, 25))
for (i in 1:ncol(frequ_out)) {
  lines(frequ_out[, i])
}


###

#Rhine flow observations - Empirical mode decomposition
#Erwin Rottler, University of Potsdam
#October 2018

###

#Basel temperature data
if(data_sel_emd == "basel_temp"){
  
  temp_emd <- read.table(paste0(base_dir, "data/idaweb/order62409/order_62409_data.txt"), sep = ";", skip = 2, header = T)#Basel
  temp_emd$date <- as.Date(strptime(temp_emd$time, "%Y%m%d", tz="UTC"))
  temp_emd$tem0 <- temp_emd$tre200d0
  data_emd  <- temp_emd
  stat_sel <- "tem0"
  
}

#Basel discharge data
if(data_sel_emd == "basel_disc"){
  
  load(paste0(base_dir, "data/bafu/dis_new.RData")); dis <- dis_new; rm(dis_new)
  data_emd <- dis
  stat_sel <- "Diepoldsau_2" # Diepoldsau_2, Basel_Rheinhalle_2, Rekingen_2, Koeln, Cochem, Gsteig
  
}


#emd_calc----

#get selected data
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

#Function to do emd and return residual
f_emd_resid <- function(data_in){
  
  #Calculate emd and return residual
  # dis_emd <- hht::CEEMD(sig = data_in, tt = 1:length(data_in), noise.type = "gaussian", noise.amp = noise_amp, trials = number_trials)
  # dis_emd <- hht::Sig2IMF(sig = data_in, tt = 1:length(data_in), boundary = "wave")
  # emd_out <- dis_emd$residue
  my_emd <- Rlibeemd::ceemdan(input = data_in, ensemble_size = number_trials, noise_strength = noise_str)
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


#emd_visu----

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

n_max <- round(abs(max_na(emd_resid[, ])) / (max_na(emd_resid[, ]) + abs(min_na(emd_resid[, ]))), digits = 2) * 200
n_min <- 200 - n_max
# cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
# cols_max <- colorRampPalette(c("grey90", "yellow2","gold", "orange2", "orangered2", "orangered4"))(n_max)
# cols_min <- colorRampPalette(c(viridis(9, direction = 1)[c(2,4)], "cadetblue3","grey90"))(n_min)
# cols_max <- colorRampPalette(c("grey90", "yellow2","gold", "orange", "orange2"))(n_max)
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
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(emd_resid), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("EMD residual (centered) [°C]", side = 4, line = 1.5, cex = 0.8)

box()



#exprob_plus----

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

gaug_emd <- "Diepoldsau_2" #Koeln, Basel_Rheinhalle_2, Cochem, Rekingen_2, Diepoldsau_2, Davos, Gsteig

cols_sel <- sapply(gaug_emd, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame
# dis_sel <- temp_emd$tem0

####
sta_yea_emd <- 1869
end_yea_emd <- 1943
sta_yea_emd <- 1944
end_yea_emd <- 2017

qprob_long_dip_1 <- qprob_long
qprob_long_dip_2 <- qprob_long

qprob_long_rek_1 <- qprob_long
qprob_long_rek_2 <- qprob_long

qprob_long_bas_1 <- qprob_long
qprob_long_bas_2 <- qprob_long

plot(smoothFFT(qprob_long_dip_1[, 90], sd = 5), type = "l")
lines(smoothFFT(qprob_long_dip_2[, 90], sd = 5), col = "red3")
abline(h = 0)

plot(smoothFFT(qprob_long_rek_1[, 90], sd = 5), type = "l")
lines(smoothFFT(qprob_long_rek_2[, 90], sd = 5), col = "red3")
abline(h = 0)

plot(smoothFFT(qprob_long_bas_1[, 80], sd = 5), type = "l")
lines(smoothFFT(qprob_long_bas_2[, 80], sd = 5), col = "red3")
abline(h = 0)

sum(qprob_long_bas_2[, 80])
####

#Exceedance probability
f_qprob <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dis$date,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "quantile_prob",
                                       method_quant = "gev"
)}
    
qprob_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qprob(k)
  }
    

#Trend exceedance probability
f_qmove <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dis$date,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "mov_quant_prob_trend",
                                       method_quant = "gev"
)}
    
qmove_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qmove(k)
}
    

    
#Plot: Exceedance probability

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks <-  seq(9, 90, by = 10)
ylabels <-  seq(0.1, 0.9, by =0.1)

cols_1 <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                             "yellow2","gold2", "orange2", "orangered3", "orangered4"))(200)

# cols_1 <- rev(colorRampPalette(c("orangered4", brewer.pal(11,"RdYlBu"), "midnightblue"))(200))

breaks <-  seq(min_na(qprob_long), max_na(qprob_long), length.out = length(cols_1) +1)

y <- 1:ncol(qprob_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qprob_long), col = cols_1, breaks = breaks, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabels, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(qprob_long), col = cols_1, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Exceedance prob. [%]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Trend exceedance probability

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks <-  seq(9, 90, by = 10)
ylabels <-  seq(0.1, 0.9, by =0.1)

n_max <- round(abs(max_na(qmove_long)) / (max_na(qmove_long) + abs(min_na(qmove_long))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_2 <- c(cols_min, cols_max)

breaks <-  seq(min_na(qmove_long), max_na(qmove_long), length.out = length(cols_1) +1)

y <- 1:ncol(qprob_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qmove_long), col = cols_2, breaks = breaks, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabels, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(data_in), col = cols_2, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Trend exceedance window prob. [%/dec]", side = 4, line = 1.5, cex = 0.8)

box()




  

















par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

n_max <- round(abs(max_na(qmove_long[, z])) / (max_na(qmove_long[, z]) + abs(min_na(qmove_long[, z]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
cols_max <- colorRampPalette(c("grey90", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_2 <- c(cols_min, cols_max)

image_flow(data_in = qmove_long,
           colors = cols_2,
           breaks = seq(min_na(qmove_long[, z]), max_na(qmove_long[, z]), length.out = length(cols_2) +1),
           ylab = "Quantile",
           margins_1 = c(1.6,2.5,1.6,0),
           margins_2 = c(1.6,0.5,1.6,1.7)
)



  






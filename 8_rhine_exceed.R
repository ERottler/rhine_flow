###

#Rhine flow observations - Quantile exceedance probabilites for selected gauge
#Erwin Rottler, University of Potsdam

###

#parameter----

gaug_exce <- "Basel_Rheinhalle_2" #Koeln, Basel_Rheinhalle_2, Cochem, Rekingen_2, Diepoldsau_2, Davos, Gsteig
is_grdc_data <- T #usa grdc data selected; gauge_exce ingored
sta_yea_emd <- 1869
end_yea_emd <- 2012
quants <- seq(0.99, 0.01, by = -0.01)
quant_method <- "empirical" #empirical, gev

#quan_cal----

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

cols_sel <- sapply(gaug_exce, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame
dat_sel <- dis$date

if(is_grdc_data){
  
  dis_sel <- grdc_data$value
  dat_sel <- grdc_data$date
  
}

#Quantile value
f_qvalu <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dat_sel,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "quantile",
                                       method_quant = quant_method
)}

qvalu_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qvalu(k)
}

#Quantile value trend
f_qvslo <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dat_sel,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "mov_quant_trend",
                                       method_quant = quant_method
)}

qvslo_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qvslo(k)
}

#quan_vis----

#Plot: Quantile value

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

max_break <- max_na(qvalu_long)
min_break <- min_na(qvalu_long)
qua_break <- quantile(qvalu_long, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Discharge [m³/sec]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Trend moving quantile

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

n_max <- round(abs(alptempr::max_na(qvslo_long)) / (alptempr::max_na(qvslo_long) + abs(alptempr::min_na(qvslo_long))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_long), alptempr::max_na(qvslo_long), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Trend window quantiles [m³/sec/dec]", side = 4, line = 1.5, cex = 0.8)

box()





#prob_cal----

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

cols_sel <- sapply(gaug_exce, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame
dat_sel <- dis$date
  
if(is_grdc_data){
  
  dis_sel <- grdc_data$value
  dat_sel <- grdc_data$date
  
}

#Exceedance probability
f_qprob <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dat_sel,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "quantile_prob",
                                       method_quant = "empirical"
)}

qprob_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qprob(k)
}


#Trend exceedance probability
f_qmove <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dat_sel,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "mov_quant_prob_trend",
                                       method_quant = "empirical"
)}

qmove_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qmove(k)
}


#prob_vis----

#Plot: Exceedance probability

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks      <- seq(10, 90, by = 10)

cols_1 <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                             "yellow2","gold2", "orange2", "orangered3", "orangered4"))(200)

# cols_1 <- rev(colorRampPalette(c("orangered4", brewer.pal(11,"RdYlBu"), "midnightblue"))(200))

breaks <-  seq(alptempr::min_na(qprob_long), alptempr::max_na(qprob_long), length.out = length(cols_1) +1)

y <- 1:ncol(qprob_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qprob_long), col = cols_1, breaks = breaks, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ytiks/100, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(qprob_long), col = cols_1, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Exceedance prob. [%]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Trend exceedance probability

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks <-  seq(10, 90, by = 10)

n_max <- round(abs(alptempr::max_na(qmove_long)) / (alptempr::max_na(qmove_long) + abs(alptempr::min_na(qmove_long))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_2 <- c(cols_min, cols_max)

breaks <-  seq(alptempr::min_na(qmove_long), alptempr::max_na(qmove_long), length.out = length(cols_1) +1)

y <- 1:ncol(qmove_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(qmove_long), col = cols_2, breaks = breaks, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ytiks/100, mgp = c(3, 0.3, 0))
mtext("Quantile", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(qmove_long), col = cols_2, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Trend window exceedance prob. [%/dec]", side = 4, line = 1.5, cex = 0.8)

box()


#rank_cal----

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

cols_sel <- sapply(gaug_exce, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame
dat_sel <- dis$date

if(is_grdc_data){
  
  dis_sel <- grdc_data$value
  dat_sel <- grdc_data$date
  
}

all_ranks <- 30:1

#Ranking mean
f_ramea <- function(my_rank_sel){dis_ana(disc = dis_sel,
                                         date = dat_sel,
                                         start_year = sta_yea_emd,
                                         end_year = end_yea_emd,
                                         window_width = window_width,
                                         method_analys = "window_rank_min",
                                         method_quant = quant_method,
                                         rank_sel = my_rank_sel
)}

ramea_long <- foreach(k = all_ranks, .combine = 'cbind') %dopar%{
  f_ramea(k)
}



#Ranking trend
f_raslo <- function(my_rank_sel){dis_ana(disc = dis_sel,
                                         date = dat_sel,
                                         start_year = sta_yea_emd,
                                         end_year = end_yea_emd,
                                         window_width = window_width,
                                         method_analys = "window_rank_sens_slope",
                                         method_quant = quant_method,
                                         rank_sel = my_rank_sel
)}

raslo_long <- foreach(k = all_ranks, .combine = 'cbind') %dopar%{
  f_raslo(k)
}


#rank_vis----

#Plot: Ranked values mead

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks      <- seq(1, 30, by =  5)
ylabs      <- seq(30, 1, by = -5)

cols_ramea <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

max_break <- max_na(ramea_long)
min_break <- min_na(ramea_long)
qua_break <- quantile(ramea_long, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_ramea <- c(breaks_1, breaks_2)

y <- 1:ncol(ramea_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(ramea_long), col = cols_ramea, breaks = breaks_ramea, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs, mgp = c(3, 0.3, 0))
mtext("Rank", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(ramea_long), col = cols_ramea, breaks = breaks_ramea, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Mean window rank [m³/sec]", side = 4, line = 1.5, cex = 0.8)

box()



#Plot: Ranked values trends

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
ytiks      <- seq(1, 30, by =  5)
ylabs      <- seq(30, 1, by = -5)

n_max <- round(abs(alptempr::max_na(raslo_long)) / (alptempr::max_na(raslo_long) + abs(alptempr::min_na(raslo_long))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_raslo <- c(cols_min, cols_max)

breaks_raslo <-  seq(alptempr::min_na(raslo_long), alptempr::max_na(raslo_long), length.out = length(cols_raslo) +1)

y <- 1:ncol(raslo_long)
x <- 1:365

par(mar = c(1.6, 3, 0.6, 0))

image(x, y, as.matrix(raslo_long), col = cols_raslo, breaks = breaks_raslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs, mgp = c(3, 0.3, 0))
mtext("Rank", 2, 1.5, cex = 0.8)

axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.03)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

alptempr::image_scale(as.matrix(raslo_long), col = cols_raslo, breaks = breaks_raslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Trend window rank [m³/sec/dec]", side = 4, line = 1.5, cex = 0.8)

box()






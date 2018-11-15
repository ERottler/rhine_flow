###

#Rhine flow observations - Quantile exceedance probabilites for selected gauge
#Erwin Rottler, University of Potsdam

###

#parameter----

gaug_exce <- "Basel_Rheinhalle_2" #Koeln, Basel_Rheinhalle_2, Cochem, Rekingen_2, Diepoldsau_2, Davos, Gsteig
sta_yea_emd <- 1950
end_yea_emd <- 2014
quants <- seq(0.01, 0.99, by = 0.01)

#prob_cal----

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

cols_sel <- sapply(gaug_exce, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame

#Exceedance probability
f_qprob <- function(quant_sel){dis_ana(disc = dis_sel,
                                       date = dis$date,
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
                                       date = dis$date,
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








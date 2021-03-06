###

#Quantile analysis for selected gauges
#Erwin Rottler, University of Potsdam

###

#parameter----

sta_yea_emd <- 1869
end_yea_emd <- 2016
quants <- seq(0.01, 0.99, by = 0.01)
quant_method_val <- "empirical" #empirical, gev, gpd
quant_method_slo <- "empirical" #empirical, gev, gpd
window_width <- 30 

#quan_cal----

#Quantile value
f_qvalu <- function(quant_sel){dis_ana(disc = grdc_data$value,
                                       date = grdc_data$date,
                                       start_year = sta_yea_emd,
                                       end_year = end_yea_emd,
                                       quant_in = quant_sel,
                                       window_width = window_width,
                                       method_analys = "quantile",
                                       method_quant = quant_method_val
)}

qvalu_long <- foreach(k = quants, .combine = 'cbind') %dopar%{
  f_qvalu(k)
}

#Quantile value trend
qvslo_long <- dis_ana(disc = grdc_data$value,
                      date = grdc_data$date,
                      start_year = sta_yea_emd,
                      end_year = end_yea_emd,
                      quant_in = quants,
                      window_width = window_width,
                      method_analys = "mov_quant_trend",
                      method_quant = quant_method_slo
)


#quan_vis----

#Plot: Quantile value

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
lab_months <- c("J","F","M","A","M","J","J","A","S","O","N","D")

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(10, 90, by =  10)

cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1, 1)]))(100)
cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2","gold2", "yellow2", "white"))(100)
cols_qvalu <- c(cols_min, cols_max)

probs_iso <- c(0.1, 0.5, 0.9)
break_quant <- 0.5
par(family = "serif")

max_break <- max_na(qvalu_long)
min_break <- min_na(qvalu_long)
qua_break <- quantile(qvalu_long, probs = break_quant, type = 8, na.rm = T)
iso_def <- quantile(qvalu_long, probs = probs_iso, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_long)
x <- 1:365

par(mar = c(2.0, 1.7, 0.5, 0))

image(x, y, as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = 1.7)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = 1.7)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, 0.40, 0), cex.axis = 1.7)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvalu_long),
        levels = round(iso_def, 0),
        add = T,
        lwd = 0.7,
        labcex = 0.7)

par(mar = c(2.0, 0.5, 0.5, 1.7))

alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.40, 0), tck = -0.08, cex.axis = 1.7)
box()


#Plot: Trend moving quantile

n_max <- round(abs(alptempr::max_na(qvslo_long)) / (alptempr::max_na(qvslo_long) + abs(alptempr::min_na(qvslo_long))), digits = 2) * 200
n_min <- 200 - n_max
cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1, 1)]))(n_max)
cols_min <- grDevices::colorRampPalette(c("orangered4", "orangered3", "orange2", "gold2", "yellow2", "white"))(n_min)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_long), alptempr::max_na(qvslo_long), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_long)
x <- 1:365

par(mar = c(2.0, 1.7, 0.5, 0))

image(x, y, as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = 1.7)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = 1.7)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, 0.40, 0), cex.axis = 1.7)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvslo_long),
        nlevels = 6,
        add = T,
        lwd = 0.7,
        labcex = 0.7)

par(mar = c(2.0, 0.5, 0.5, 1.7))

alptempr::image_scale(as.matrix(qvslo_wass), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.4, 0), tck = -0.08, cex.axis = 1.7)

box()



#prob_cal----

#Exceedance probability
f_qprob <- function(quant_sel){dis_ana(disc = grdc_data$value,
                                       date = grdc_data$date,
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
f_qmove <- function(quant_sel){dis_ana(disc = grdc_data$value,
                                       date = grdc_data$date,
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


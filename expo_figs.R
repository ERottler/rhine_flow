###

#Long-term changes in river runoff - Export figures
#Erwin Rottler, University of Potsdam

###

pacman::p_load(sp, alptempr, zoo, emdbook, viridis, Rlibeemd, zyp)

load("U:/RhineFlow/rhine_obs/R/figs_manus/riv_flow_new.Rdata")
# load("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/riv_flow_new.Rdata")

mar_1 <- c(2.0, 1.7, 0.5, 0)
mar_2 <- c(2.0, 0.5, 0.5, 1.7)
cex_header <- 1.60
cex_x_axis <- 1.7
cex_y_axis <- 1.7 #Size labels y-axis
x_lab_posi <- 0.40 #Position labels of x-axis
y_lab_scal <- 0.40 #Position labels of y-axis scale bar
lab_months <- c("J","F","M","A","M","J","J","A","S","O","N","D")
lwd_iso <- 0.7
cex_iso <- 0.7
n_iso <- 12
n_iso_2 <- 6 #number isolines for Discharge 'Changes in seasonality'
n_iso_3 <- 8 #number isolines for Discharge 'Onset and evolution'
n_iso_4 <- 8 #number isolines for Temperature
n_iso_5 <- 6 #number isolines for Rain 'Onset and evolution'
n_iso_6 <- 8 #number isolines for Rain 'Changes in intensity'
n_iso_7 <- 8 #number isolines for Seasonal plots
par(family = "serif")
lev_sig <- 0.05

#Fig_2----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig2.pdf", width = 8.3, height = 6.5)
pdf("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/Fig2.pdf", width = 8.3, height = 7.0)

layout(matrix(c(1,2,3,
                1,2,4), 
              3, 2), heights = c(1, 1, 1.3))

line_lwd <- 0.7

par(mar = c(2.5, 4.0, 2.3, 0.5))

#Observations Wasserburg
dat_ori <- emd_day_wass_ori[which(rownames(emd_day_wass_ori) %in% c(1955:1974)), ]

plot(c(t(dat_ori)), type = "n", ylim = c(min(dat_ori), max(dat_ori)),
     xlab = "", ylab = "", axes = F)
abline(v = c(1, 365*5, 365*10, 365*15, 365*20, 365*25), lty = "dashed", col = "grey55", lwd = 0.8)
abline(h = c(0, 500, 1000, 1500), lty = "dashed", col = "grey55", lwd = 0.8)
lines(c(t(dat_ori)), lwd = 1.2)
axis(1, at = c(1, 365*5, 365*10, 365*15, 365*20, 365*25), c("1955", "1960", "1965", "1970", "1975", "1980"),
     mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) Discharge Wasserburg", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
mtext("Discharge [m³/s]", side = 2, line = 2.0, cex = 1, adj = 0.5)
box()


#Observations Koeln
dat_ori <- emd_day_koel_ori[which(rownames(emd_day_koel_ori) %in% c(1955:1974)), ]

plot(c(t(dat_ori)), type = "n", ylim = c(min(dat_ori), max(dat_ori)),
     xlab = "", ylab = "", axes = F)
lines(c(t(dat_ori)), lwd = 1.2)
abline(v = c(1, 365*5, 365*10, 365*15, 365*20, 365*25), lty = "dashed", col = "grey55", lwd = 0.8)
abline(h = c(1000, 2000, 3000, 4000, 5000, 6000), lty = "dashed", col = "grey55", lwd = 0.8)
axis(1, at = c(1, 365*5, 365*10, 365*15, 365*20, 365*25), c("1955", "1960", "1965", "1970", "1975", "1980"),
     mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Discharge Cologne", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
mtext("Discharge [m³/s]", side = 2, line = 2.0, cex = 1, adj = 0.5)
box()

par(mar = c(2.0, 4.0, 3.5, 0.5))

#Wasserburg 1.Feb
day_sel <- 32
dis_data <- emd_day_wass[, day_sel] # discharge Wasserburg after 30DMA

emd_years <- sta_yea_emd:end_yea_emd
emd_out <- Rlibeemd::ceemdan(input = dis_data, ensemble_size = 10000, noise_strength = 0.5)

imf_1_3 <- emd_out[, 1] + emd_out[, 2] + emd_out[, 3]
imf_4_6 <- emd_out[, 4] + emd_out[, 5] + emd_out[, 6]
residu <-  emd_out[, 7]

dis_data_mk <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[6])
dis_data_sl <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[2])
dis_data_in <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[11])

plot(emd_years, dis_data, type = "n", ylim = c(min(dis_data), max(dis_data)),
     xlab = "", ylab = "", axes = F, lwd = line_lwd)
abline(h = c(100,150,200,250), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, dis_data, lwd = 1.2)
# abline(a = dis_data_in, b = dis_data_sl, col = "orangered3", lwd = 1.5)
segments(x0 = 1869, y0 = (1869*dis_data_sl+dis_data_in), x1 = 2016, y1 =(2016*dis_data_sl+dis_data_in), col = "orangered3", lwd = 1.8)
lines(emd_years, residu, lwd = 1.8, col = viridis::viridis(9, direction = 1)[3])
axis(1, at =c(1900, 1940, 1980),  mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.015, cex.axis = cex_y_axis)
mtext(paste0("c) Wasserburg 1.Feb"), side = 3, line = 1.8, cex = cex_header, adj = 0.0)
mtext("Discharge [m³/s]", side = 2, line = 2.0, cex = 1, adj = 0.5)
mtext(paste0("Linear trend: ", round(dis_data_sl*10, 2), " m³/s/dec     p-value: ", round(dis_data_mk, 4)), 
      side = 3, line = 0.1, cex = 1.0, adj = 0.0)
legend("topleft", c("CEEMDAN residual", "Linear trend"), pch = 19, col = c(viridis::viridis(9, direction = 1)[3], "orangered3"),
       bg ="white", cex = 1.0)
box()



#Koeln 1.June
day_sel <- 152
dis_data <- emd_day_koel[, day_sel] # discharge Wasserburg after 30DMA

emd_years <- sta_yea_emd:end_yea_emd
emd_out <- Rlibeemd::ceemdan(input = dis_data, ensemble_size = 10000, noise_strength = 0.5)

imf_1_3 <- emd_out[, 1] + emd_out[, 2] + emd_out[, 3]
imf_4_6 <- emd_out[, 4] + emd_out[, 5] + emd_out[, 6]
residu <-  emd_out[, 7]

dis_data_mk <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[6])
dis_data_sl <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[2])
dis_data_in <- as.numeric(zyp.trend.vector(dis_data, x = emd_years,  method = "zhang", conf.intervals = F)[11])

plot(emd_years, dis_data, type = "n", ylim = c(min(dis_data), max(dis_data)),
     xlab = "", ylab = "", axes = F, lwd = line_lwd)
abline(h = c(1000, 2000, 3000, 4000, 5000, 6000), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1900, 1940, 1980), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, dis_data, lwd = 1.2)
# abline(a = dis_data_in, b = dis_data_sl, col = "orangered3", lwd = 1.5)
segments(x0 = 1869, y0 = (1869*dis_data_sl+dis_data_in), x1 = 2016, y1 =(2016*dis_data_sl+dis_data_in), col = "orangered3", lwd = 1.8)
lines(emd_years, residu, lwd = 1.8, col = viridis::viridis(9, direction = 1)[3])
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext(paste0("d) Cologne 1.June"), side = 3, line = 1.8, cex = cex_header, adj = 0.0)
mtext("Discharge [m³/s]", side = 2, line = 2.0, cex = 1, adj = 0.5)
mtext(paste0("Linear trend: ", round(dis_data_sl*10, 2), " m³/s/dec     p-value: ", round(dis_data_mk, 4)), 
      side = 3, line = 0.1, cex = 1.0, adj = 0.0)
legend("topleft", c("CEEMDAN residual", "Linear trend"), pch = 19, col = c(viridis::viridis(9, direction = 1)[3], "orangered3"),
       bg ="white", cex = 1.0)
box()


dev.off()





#Fig_4----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig4.pdf", width = 16.6, height = 8)
# tiff("u:/RhineFlow/rhine_obs/manus/figures/Fig4.tif", width = 16.6, height = 8, units = 'in', res = 800)
pdf("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/Fig4.pdf", width = 16.6, height = 8.0)


layout(matrix(c(34,33,33,33,33,
                34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 
                34,2,4,6,8,
                34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 
                34,10,12,14,16,
                34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 
                34,18,20,22,24,
                34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 
                34,26,28,30,32),
              5, 33), widths=c(0.55,rep(1,32)), heights=c(0.3,1,1,1,1))

# layout.show(n = 34)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

# cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
#                                             "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(100)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(100)
cols_qvalu <- c(cols_min, cols_max)

probs_iso <- c(0.1, 0.5, 0.9)
break_quant <- 0.5
par(family = "serif")

#Figure 1.1: Waserburg quantiles

max_break <- max_na(qvalu_wass)
min_break <- min_na(qvalu_wass)
qua_break <- quantile(qvalu_wass, probs = break_quant, type = 8, na.rm = T)
iso_def <- quantile(qvalu_wass, probs = probs_iso, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_wass)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_wass), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("a) Wasserb. runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
# axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
#      col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvalu_wass),
        levels = round(iso_def, 0),
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_wass), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Basel quantiles

max_break <- max_na(qvalu_base)
min_break <- min_na(qvalu_base)
qua_break <- quantile(qvalu_base, probs = break_quant, type = 8, na.rm = T)
iso_def <- quantile(qvalu_base, probs = probs_iso, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_base)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_base), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("c) Basel runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvalu_base),
        levels = round(iso_def, 0),
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_base), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Koeln quantiles

max_break <- max_na(qvalu_koel)
min_break <- min_na(qvalu_koel)
qua_break <- quantile(qvalu_koel, probs = break_quant, type = 8, na.rm = T)
iso_def <- quantile(qvalu_koel, probs = probs_iso, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_koel)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_koel), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("e) Koeln runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvalu_koel),
        levels = round(iso_def, 0),
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_koel), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Wuerzburg quantiles

max_break <- max_na(qvalu_wuer)
min_break <- min_na(qvalu_wuer)
qua_break <- quantile(qvalu_wuer, probs = break_quant, type = 8, na.rm = T)
iso_def <- quantile(qvalu_wuer, probs = probs_iso, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_wuer)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_wuer), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("g) Wurzb. runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvalu_wuer),
        levels = round(iso_def, 0),
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_wuer), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Waserburg trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_wass)) / (alptempr::max_na(qvslo_wass) + abs(alptempr::min_na(qvslo_wass))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_wass), alptempr::max_na(qvslo_wass), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_wass)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_wass), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Wasserb. runoff [(m³/s)/dec)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvslo_wass),
        nlevels = n_iso_2,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_wass), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_base)) / (alptempr::max_na(qvslo_base) + abs(alptempr::min_na(qvslo_base))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_base), alptempr::max_na(qvslo_base), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_base)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_base), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("d) Basel runoff [(m³/s)/dec)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvslo_base),
        nlevels = n_iso_2,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_base), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Koeln trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_koel)) / (alptempr::max_na(qvslo_koel) + abs(alptempr::min_na(qvslo_koel))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_koel), alptempr::max_na(qvslo_koel), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_koel)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_koel), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("f) Koeln runoff [(m³/s)/dec)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvslo_koel),
        nlevels = n_iso_2,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_koel), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Wuerzburg trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_wuer)) / (alptempr::max_na(qvslo_wuer) + abs(alptempr::min_na(qvslo_wuer))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_wuer), alptempr::max_na(qvslo_wuer), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_wuer)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_wuer), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("h) Wuerzb. runoff [(m³/s)/dec)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
box()

contour(x = x,
        y = y,
        z = as.matrix(qvslo_wuer),
        nlevels = n_iso_2,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_wuer), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.3: Wasserburg CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_wass[, ])) / (max_na(emd_disc_wass[, ]) + abs(min_na(emd_disc_wass[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_wass), max_na(emd_disc_wass),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_wass), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) Wasserb. runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_ann:end_yea_ann,
        z = t(emd_disc_wass),
        nlevels = n_iso_3,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_disc_wass)), xlim = c(1, 365), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_disc_wass) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_wass > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)

par(mar = mar_2)

image_scale(as.matrix(emd_disc_wass), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3: Basel CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_base[, ])) / (max_na(emd_disc_base[, ]) + abs(min_na(emd_disc_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_base), max_na(emd_disc_base),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_base), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("c) Basel runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_ann:end_yea_ann,
        z = t(emd_disc_base),
        nlevels = n_iso_3,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_disc_base)), xlim = c(1, 365), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_disc_base) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_base > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)








par(mar = mar_2)

image_scale(as.matrix(emd_disc_base), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.3: Koeln CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_koel[, ])) / (max_na(emd_disc_koel[, ]) + abs(min_na(emd_disc_koel[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_koel), max_na(emd_disc_koel),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_koel), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) Koeln runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_ann:end_yea_ann,
        z = t(emd_disc_koel),
        nlevels = n_iso_3,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_disc_koel)), xlim = c(1, 365), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_disc_koel) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_koel > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)






par(mar = mar_2)

image_scale(as.matrix(emd_disc_koel), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.3: Wuerzburg CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_wuer[, ])) / (max_na(emd_disc_wuer[, ]) + abs(min_na(emd_disc_wuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_wuer), max_na(emd_disc_wuer),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_wuer), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("g) Wuerzb. runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_ann:end_yea_ann,
        z = t(emd_disc_wuer),
        nlevels = n_iso_3,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_disc_wuer)), xlim = c(1, 365), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_disc_wuer) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_wuer > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_disc_wuer), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.4: Wasserburg CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass[, ])) / (alptempr::max_na(qannu_wass[, ]) + abs(alptempr::min_na(qannu_wass[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass), alptempr::max_na(qannu_wass),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Wasserb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z = t(qannu_wass),
        nlevels = n_iso,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wass)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wass) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.4: Basel CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base[, ])) / (alptempr::max_na(qannu_base[, ]) + abs(alptempr::min_na(qannu_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base), alptempr::max_na(qannu_base),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("d) Basel runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z = t(qannu_base),
        nlevels = n_iso,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_base)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_base) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_base > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 3.4: Koeln CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel[, ])) / (alptempr::max_na(qannu_koel[, ]) + abs(alptempr::min_na(qannu_koel[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel), alptempr::max_na(qannu_koel),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("f) Koeln runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z = t(qannu_koel),
        nlevels = n_iso,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_koel)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_koel) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_koel > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.4: Wuerzwuer CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer[, ])) / (alptempr::max_na(qannu_wuer[, ]) + abs(alptempr::min_na(qannu_wuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer), alptempr::max_na(qannu_wuer),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("h) Wuerzb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z = t(qannu_wuer),
        nlevels = n_iso,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wuer)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wuer) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wuer > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Station names

par(mar = c(0,0,0,0))

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Wasserburg (a)", side = 2, line = -1.8, cex = cex_header, adj = 1.00)
mtext("Basel (b)",      side = 2, line = -1.8, cex = cex_header, adj = 0.66)
mtext("Cologne (c)",      side = 2, line = -1.8, cex = cex_header, adj = 0.37)
mtext("Wuerzburg (d)",  side = 2, line = -1.8, cex = cex_header, adj = 0.04, outer = T)
# mtext("A",  side = 3, line = -6.00, cex = 1.5, adj = 0.014, outer = T)
# mtext("B",  side = 3, line = -20.0, cex = 1.5, adj = 0.015, outer = T)
# mtext("C",  side = 3, line = -34.0, cex = 1.5, adj = 0.016, outer = T)
# mtext("D",  side = 3, line = -48.0, cex = 1.5, adj = 0.017, outer = T)

#Analytical method

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("1. Seasonality of runoff [m³/s]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.051-0.01)
mtext("2. Change in season. [m³/s/dec]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.3624-0.015)
mtext("3. Onset and evolution [m³/s]",   
      side = 3, line = -3.75, cex = cex_header, adj = 0.662-0.01)
mtext("4. Changes in quantiles [m³/s]",  
      side = 3, line = -3.75, cex = cex_header, adj = 0.9675-0.01)
mtext("Discharge",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.52)


dev.off()




#Fig_5----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig5.pdf", width = 16.6, height = 6.14)
# tiff("u:/RhineFlow/rhine_obs/manus/figures/Fig5.tif", width = 16.6, height = 6.14, units = 'in', res = 800)
pdf("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/Fig5.pdf", width = 16.6, height = 6.14)

layout(matrix(c(26,25,25,25,
                26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5,  
                26,2,4,6,
                26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11,  
                26,8,10,12,
                26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 
                26,14,16,18,
                26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 
                26,20,22,24),
              4, 33), widths=c(0.55,rep(1,22)), heights=c(0.3,1,1,1))

# layout.show(n = 26)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

par(family = "serif")


#Figure 1.1 : Bern CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_bern[, ])) / (max_na(emd_temp_bern[, ]) + abs(min_na(emd_temp_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_bern), max_na(emd_temp_bern),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_bern), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) Bern temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_bern)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_bern)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_bern) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_bern > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_bern), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Basel CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_base[, ])) / (max_na(emd_temp_base[, ]) + abs(min_na(emd_temp_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_base), max_na(emd_temp_base),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_base), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("c) Basel temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_base)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_base)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_base) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_base > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_base), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1 : Zuerich CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_zuer[, ])) / (max_na(emd_temp_zuer[, ]) + abs(min_na(emd_temp_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_zuer), max_na(emd_temp_zuer),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_zuer), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) Zuerich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_zuer)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_zuer)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_zuer) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_zuer > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_zuer), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Bern CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_bern[, ])) / (alptempr::max_na(qannu_temp_bern[, ]) + abs(alptempr::min_na(qannu_temp_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_bern), alptempr::max_na(qannu_temp_bern),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_bern), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_bern),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_bern)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_bern) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_bern > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_bern), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_base[, ])) / (alptempr::max_na(qannu_temp_base[, ]) + abs(alptempr::min_na(qannu_temp_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_base), alptempr::max_na(qannu_temp_base),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_base), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_base),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_base)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_base) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_base > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_base), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Zuerich CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_zuer[, ])) / (alptempr::max_na(qannu_temp_zuer[, ]) + abs(alptempr::min_na(qannu_temp_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_zuer), alptempr::max_na(qannu_temp_zuer),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_zuer), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) zuer precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_zuer),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_zuer)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_zuer) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_zuer > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_zuer), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.3 : Bern CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_bern[, ])) / (max_na(emd_rain_bern[, ]) + abs(min_na(emd_rain_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_bern), max_na(emd_rain_bern),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_bern), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) Bern rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_bern)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_bern)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_bern) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_bern > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_bern), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3 : Basel CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_base[, ])) / (max_na(emd_rain_base[, ]) + abs(min_na(emd_rain_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_base), max_na(emd_rain_base),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_base), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) base rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_base)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_base)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_base) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_base > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_base), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.3 : Zuerich CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_zuer[, ])) / (max_na(emd_rain_zuer[, ]) + abs(min_na(emd_rain_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_zuer), max_na(emd_rain_zuer),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_zuer), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) zuer rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_zuer)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_zuer)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_zuer) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_zuer > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_zuer), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.4: Bern CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern[, ])) / (alptempr::max_na(qannu_rain_bern[, ]) + abs(alptempr::min_na(qannu_rain_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern), alptempr::max_na(qannu_rain_bern),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_bern),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_bern)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_bern) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_bern > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.4: Basel CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base[, ])) / (alptempr::max_na(qannu_rain_base[, ]) + abs(alptempr::min_na(qannu_rain_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base), alptempr::max_na(qannu_rain_base),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_base),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_base)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_base) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_base > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.4: Zuerich CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer[, ])) / (alptempr::max_na(qannu_rain_zuer[, ]) + abs(alptempr::min_na(qannu_rain_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer), alptempr::max_na(qannu_rain_zuer),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) zuer precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_zuer),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_zuer)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_zuer) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_zuer > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

par(mar = c(0,0,0,0))

#Station names

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Bern (a)",    side = 2, line = -1.8, cex = cex_header, adj = 0.92)
mtext("Basel (b)",   side = 2, line = -1.8, cex = cex_header, adj = 0.53)
mtext("Zuerich (c)", side = 2, line = -1.8, cex = cex_header, adj = 0.12)

#Analytical method

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("1. Onset and evolution [°C]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.051)
mtext("2. Changes in quantiles [°C]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.352)
mtext("3. Onset and evolution [mm]",   
      side = 3, line = -3.75, cex = cex_header, adj = 0.658-0.005)
mtext("4. Changes in quantiles [mm]",  
      side = 3, line = -3.75, cex = cex_header, adj = 0.966-0.013)
mtext("Temperature",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.24)
mtext("Precipitation",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.77)

dev.off()


#Fig_6----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig6.pdf", width = 16.6, height = 8)
pdf("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/Fig6.pdf", width = 16.6, height = 8.0)

layout(matrix(c(34,33,33,33,33,
                34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 34,1,3,5,7, 
                34,2,4,6,8,
                34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 34,9,11,13,15, 
                34,10,12,14,16,
                34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 34,17,19,21,23, 
                34,18,20,22,24,
                34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 34,25,27,29,31, 
                34,26,28,30,32),
              5, 33), widths=c(0.55,rep(1,32)), heights=c(0.3,1,1,1,1))

# layout.show(n = 34)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

par(family = "serif")



#Figure 1.1: Wasserburg CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_spr[, ])) / (alptempr::max_na(qannu_wass_spr[, ]) + abs(alptempr::min_na(qannu_wass_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_spr), alptempr::max_na(qannu_wass_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wass_sprerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wass_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wass_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wass_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Basel CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_spr[, ])) / (alptempr::max_na(qannu_base_spr[, ]) + abs(alptempr::min_na(qannu_base_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_spr), alptempr::max_na(qannu_base_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base_sprerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_base_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_base_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_base_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_base_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Koeln CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_spr[, ])) / (alptempr::max_na(qannu_koel_spr[, ]) + abs(alptempr::min_na(qannu_koel_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_spr), alptempr::max_na(qannu_koel_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) koel_sprerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_koel_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_koel_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_koel_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_koel_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Wuerzburg CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_spr[, ])) / (alptempr::max_na(qannu_wuer_spr[, ]) + abs(alptempr::min_na(qannu_wuer_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_spr), alptempr::max_na(qannu_wuer_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wuer_sprerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wuer_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wuer_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wuer_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wuer_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Wasserburg CEEMDAN discharge quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_sum[, ])) / (alptempr::max_na(qannu_wass_sum[, ]) + abs(alptempr::min_na(qannu_wass_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_sum), alptempr::max_na(qannu_wass_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wass_sumerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wass_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wass_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wass_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel CEEMDAN discharge quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_sum[, ])) / (alptempr::max_na(qannu_base_sum[, ]) + abs(alptempr::min_na(qannu_base_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_sum), alptempr::max_na(qannu_base_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base_sumerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_base_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_base_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_base_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_base_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Koeln CEEMDAN discharge quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_sum[, ])) / (alptempr::max_na(qannu_koel_sum[, ]) + abs(alptempr::min_na(qannu_koel_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_sum), alptempr::max_na(qannu_koel_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) koel_sumerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_koel_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_koel_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_koel_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_koel_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Wuerzburg CEEMDAN discharge quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_sum[, ])) / (alptempr::max_na(qannu_wuer_sum[, ]) + abs(alptempr::min_na(qannu_wuer_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_sum), alptempr::max_na(qannu_wuer_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wuer_sumerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wuer_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wuer_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wuer_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wuer_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 1.3: Wasserburg CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_aut[, ])) / (alptempr::max_na(qannu_wass_aut[, ]) + abs(alptempr::min_na(qannu_wass_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_aut), alptempr::max_na(qannu_wass_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wass_auterb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wass_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wass_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wass_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3: Basel CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_aut[, ])) / (alptempr::max_na(qannu_base_aut[, ]) + abs(alptempr::min_na(qannu_base_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_aut), alptempr::max_na(qannu_base_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base_auterb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_base_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_base_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_base_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_base_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.3: Koeln CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_aut[, ])) / (alptempr::max_na(qannu_koel_aut[, ]) + abs(alptempr::min_na(qannu_koel_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_aut), alptempr::max_na(qannu_koel_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) koel_auterb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_koel_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_koel_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_koel_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_koel_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.3: Wuerzburg CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_aut[, ])) / (alptempr::max_na(qannu_wuer_aut[, ]) + abs(alptempr::min_na(qannu_wuer_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_aut), alptempr::max_na(qannu_wuer_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wuer_auterb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wuer_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wuer_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wuer_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wuer_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 1.4: Wasserburg CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_win[, ])) / (alptempr::max_na(qannu_wass_win[, ]) + abs(alptempr::min_na(qannu_wass_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_win), alptempr::max_na(qannu_wass_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wass_winerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wass_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wass_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wass_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.4: Basel CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_win[, ])) / (alptempr::max_na(qannu_base_win[, ]) + abs(alptempr::min_na(qannu_base_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_win), alptempr::max_na(qannu_base_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) base_winerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_base_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_base_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_base_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_base_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.4: Koeln CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_win[, ])) / (alptempr::max_na(qannu_koel_win[, ]) + abs(alptempr::min_na(qannu_koel_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_win), alptempr::max_na(qannu_koel_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) koel_winerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_koel_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_koel_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_koel_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_koel_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.4: Wuerzburg CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_win[, ])) / (alptempr::max_na(qannu_wuer_win[, ]) + abs(alptempr::min_na(qannu_wuer_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_win), alptempr::max_na(qannu_wuer_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) wuer_winerb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_wuer_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_wuer_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_wuer_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wuer_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


par(mar = c(0,0,0,0))

#Station names

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Wasserburg (a)", side = 2, line = -1.8, cex = cex_header, adj = 1.00)
mtext("Basel (b)",      side = 2, line = -1.8, cex = cex_header, adj = 0.66)
mtext("Cologne (c)",      side = 2, line = -1.8, cex = cex_header, adj = 0.37)
mtext("Wuerzburg (d)",  side = 2, line = -1.8, cex = cex_header, adj = 0.04, outer = T)
# mtext("A",  side = 3, line = -6.00, cex = 1.5, adj = 0.014, outer = T)
# mtext("B",  side = 3, line = -20.0, cex = 1.5, adj = 0.015, outer = T)
# mtext("C",  side = 3, line = -34.0, cex = 1.5, adj = 0.016, outer = T)
# mtext("D",  side = 3, line = -48.0, cex = 1.5, adj = 0.017, outer = T)

#Analytical method

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("1. Spring", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.045+0.064)
mtext("2. Summer", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.311+0.061)
mtext("3. Autumn",   
      side = 3, line = -3.75, cex = cex_header, adj = 0.576+0.055)
mtext("4. Winter",  
      side = 3, line = -3.75, cex = cex_header, adj = 0.835+0.055)
mtext("Discharge: Changes in quantiles [m³/s]",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.50)


dev.off()



#Fig_7----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig7.pdf", width = 16.6, height = 6.14)
pdf("/home/erwin/ownCloud/RhineFlow/rhine_obs/manus/figures/Fig7.pdf", width = 16.6, height = 6.14)

layout(matrix(c(26,25,25,25,
                26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5, 26,1,3,5,  
                26,2,4,6,
                26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11, 26,7,9,11,  
                26,8,10,12,
                26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 26,13,15,17, 
                26,14,16,18,
                26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 26,19,21,23, 
                26,20,22,24),
              4, 33), widths=c(0.55,rep(1,22)), heights=c(0.3,1,1,1))

# layout.show(n = 26)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

par(family = "serif")




#Figure 1.1: Bern CEEMDAN precipitation quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern_spr[, ])) / (alptempr::max_na(qannu_rain_bern_spr[, ]) + abs(alptempr::min_na(qannu_rain_bern_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern_spr), alptempr::max_na(qannu_rain_bern_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_bern_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_bern_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_bern_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_bern_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Basel CEEMDAN precipitation quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base_spr[, ])) / (alptempr::max_na(qannu_rain_base_spr[, ]) + abs(alptempr::min_na(qannu_rain_base_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base_spr), alptempr::max_na(qannu_rain_base_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_base_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_base_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_base_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_base_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.3: Zuerich CEEMDAN precipitation quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer_spr[, ])) / (alptempr::max_na(qannu_rain_zuer_spr[, ]) + abs(alptempr::min_na(qannu_rain_zuer_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer_spr), alptempr::max_na(qannu_rain_zuer_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_zuer_spr),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_zuer_spr)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_zuer_spr) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_zuer_spr > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Bern CEEMDAN precipitation quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern_sum[, ])) / (alptempr::max_na(qannu_rain_bern_sum[, ]) + abs(alptempr::min_na(qannu_rain_bern_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern_sum), alptempr::max_na(qannu_rain_bern_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_bern_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_bern_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_bern_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_bern_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel CEEMDAN precipitation quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base_sum[, ])) / (alptempr::max_na(qannu_rain_base_sum[, ]) + abs(alptempr::min_na(qannu_rain_base_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base_sum), alptempr::max_na(qannu_rain_base_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_base_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_base_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_base_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_base_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3: Zuerich CEEMDAN precipitation quantiles summer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer_sum[, ])) / (alptempr::max_na(qannu_rain_zuer_sum[, ]) + abs(alptempr::min_na(qannu_rain_zuer_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer_sum), alptempr::max_na(qannu_rain_zuer_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_zuer_sum),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_zuer_sum)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_zuer_sum) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_zuer_sum > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.3: Bern CEEMDAN precipitation quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern_aut[, ])) / (alptempr::max_na(qannu_rain_bern_aut[, ]) + abs(alptempr::min_na(qannu_rain_bern_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern_aut), alptempr::max_na(qannu_rain_bern_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_bern_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_bern_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_bern_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_bern_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3: Basel CEEMDAN precipitation quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base_aut[, ])) / (alptempr::max_na(qannu_rain_base_aut[, ]) + abs(alptempr::min_na(qannu_rain_base_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base_aut), alptempr::max_na(qannu_rain_base_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_base_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_base_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_base_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_base_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.3: Zuerich CEEMDAN precipitation quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer_aut[, ])) / (alptempr::max_na(qannu_rain_zuer_aut[, ]) + abs(alptempr::min_na(qannu_rain_zuer_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer_aut), alptempr::max_na(qannu_rain_zuer_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_zuer_aut),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_zuer_aut)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_zuer_aut) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_zuer_aut > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.4: Bern CEEMDAN precipitation quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern_win[, ])) / (alptempr::max_na(qannu_rain_bern_win[, ]) + abs(alptempr::min_na(qannu_rain_bern_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern_win), alptempr::max_na(qannu_rain_bern_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_bern_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_bern_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_bern_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_bern_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.4: Basel CEEMDAN precipitation quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base_win[, ])) / (alptempr::max_na(qannu_rain_base_win[, ]) + abs(alptempr::min_na(qannu_rain_base_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base_win), alptempr::max_na(qannu_rain_base_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_base_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_base_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_base_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_base_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.4: Zuerich CEEMDAN precipitation quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer_win[, ])) / (alptempr::max_na(qannu_rain_zuer_win[, ]) + abs(alptempr::min_na(qannu_rain_zuer_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer_win), alptempr::max_na(qannu_rain_zuer_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_zuer_win),
        nlevels = n_iso_7,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_zuer_win)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_zuer_win) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_zuer_win > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


par(mar = c(0,0,0,0))

#Station names

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Bern (a)",    side = 2, line = -1.8, cex = cex_header, adj = 0.92)
mtext("Basel (b)",   side = 2, line = -1.8, cex = cex_header, adj = 0.53)
mtext("Zuerich (c)", side = 2, line = -1.8, cex = cex_header, adj = 0.12)

#Analytical method

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("1. Spring", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.045+0.064)
mtext("2. Summer", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.311+0.061)
mtext("3. Autumn",   
      side = 3, line = -3.75, cex = cex_header, adj = 0.576+0.055)
mtext("4. Winter",  
      side = 3, line = -3.75, cex = cex_header, adj = 0.835+0.055)
mtext("Precipitation: Changes in quantiles [mm]",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.50)

dev.off()


#Fig_8----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig8.pdf", width = 16.6, height = 12.28)
# tiff("u:/RhineFlow/rhine_obs/manus/figures/Fig5.tif", width = 16.6, height = 6.14, units = 'in', res = 800)

layout(matrix(c(50,49,49,49,49,49,49,
                50,1,3,5,7,9,11, 50,1,3,5,7,9,11, 50,1,3,5,7,9,11,50,1,3,5,7,9,11, 50,1,3,5,7,9,11, 50,1,3,5,7,9,11, 50,1,3,5,7,9,11,  
                50,2,4,6,8,10,12,
                50,13,15,17,19,21,23, 50,13,15,17,19,21,23, 50,13,15,17,19,21,23, 50,13,15,17,19,21,23, 50,13,15,17,19,21,23, 50,13,15,17,19,21,23, 50,13,15,17,19,21,23,  
                50,14,16,18,20,22,24,
                50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 50,25,27,29,31,33,35, 
                50,26,28,30,32,34,36,
                50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 50,37,39,41,43,45,47, 
                50,38,40,42,44,46,48),
              7, 33), widths=c(0.55,rep(1,22)), heights=c(0.3,rep(1,6)))

# layout.show(n = 50)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

par(family = "serif")


#Figure 1.1 : Sion CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_sion[, ])) / (max_na(emd_temp_sion[, ]) + abs(min_na(emd_temp_sion[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_sion), max_na(emd_temp_sion),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_sion), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) sionich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_sion)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_sion)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_sion) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_sion > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_sion), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 5.1 : Samedan CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_same[, ])) / (max_na(emd_temp_same[, ]) + abs(min_na(emd_temp_same[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_same), max_na(emd_temp_same),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_same), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) sameich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_same)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_same)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_same) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_same > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_same), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1 : Neuchatel CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_neuc[, ])) / (max_na(emd_temp_neuc[, ]) + abs(min_na(emd_temp_neuc[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_neuc), max_na(emd_temp_neuc),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_neuc), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) neucich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_neuc)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_neuc)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_neuc) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_neuc > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_neuc), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1 : Lugano CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_luga[, ])) / (max_na(emd_temp_luga[, ]) + abs(min_na(emd_temp_luga[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_luga), max_na(emd_temp_luga),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_luga), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("e) lugaich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_luga)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_luga)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_luga) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_luga > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_luga), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 5.1: Geneve CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_gene[, ])) / (max_na(emd_temp_gene[, ]) + abs(min_na(emd_temp_gene[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_gene), max_na(emd_temp_gene),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_gene), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("c) genel temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_gene)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_gene)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_gene) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_gene > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_gene), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 6.1 : Chaumont CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_chau[, ])) / (max_na(emd_temp_chau[, ]) + abs(min_na(emd_temp_chau[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_chau), max_na(emd_temp_chau),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_chau), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) chau temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_temp_chau)),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_temp_chau)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_temp_chau) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_temp_chau > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

image_scale(as.matrix(emd_temp_chau), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()




#Figure 1.2: Sion CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_sion[, ])) / (alptempr::max_na(qannu_temp_sion[, ]) + abs(alptempr::min_na(qannu_temp_sion[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_sion), alptempr::max_na(qannu_temp_sion),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_sion), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) sion precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_sion),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_sion)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_sion) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_sion > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_sion), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Samedan CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_same[, ])) / (alptempr::max_na(qannu_temp_same[, ]) + abs(alptempr::min_na(qannu_temp_same[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_same), alptempr::max_na(qannu_temp_same),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_same), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) same precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_same),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_same)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_same) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_same > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_same), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Neuchatel CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_neuc[, ])) / (alptempr::max_na(qannu_temp_neuc[, ]) + abs(alptempr::min_na(qannu_temp_neuc[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_neuc), alptempr::max_na(qannu_temp_neuc),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_neuc), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) neuc precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_neuc),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_neuc)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_neuc) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_neuc > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_neuc), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Lugano CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_luga[, ])) / (alptempr::max_na(qannu_temp_luga[, ]) + abs(alptempr::min_na(qannu_temp_luga[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_luga), alptempr::max_na(qannu_temp_luga),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_luga), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) luga precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_luga),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_luga)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_luga) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_luga > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_luga), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 5.2: Geneve CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_gene[, ])) / (alptempr::max_na(qannu_temp_gene[, ]) + abs(alptempr::min_na(qannu_temp_gene[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_gene), alptempr::max_na(qannu_temp_gene),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_gene), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) gene precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_gene),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_gene)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_gene) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_gene > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_gene), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 6.2: Chaumont CEEMDAN temperature quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_temp_chau[, ])) / (alptempr::max_na(qannu_temp_chau[, ]) + abs(alptempr::min_na(qannu_temp_chau[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_temp_chau), alptempr::max_na(qannu_temp_chau),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_temp_chau), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) chau precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_temp_chau),
        nlevels = n_iso_4,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)


par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_temp_chau)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_temp_chau) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_temp_chau > 0.05, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_temp_chau), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 1.3 : Sion CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_sion[, ])) / (max_na(emd_rain_sion[, ]) + abs(min_na(emd_rain_sion[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_sion), max_na(emd_rain_sion),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_sion), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) Bern rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_sion)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_sion)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_sion) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_sion > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_sion), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.3 : Samedan CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_same[, ])) / (max_na(emd_rain_same[, ]) + abs(min_na(emd_rain_same[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_same), max_na(emd_rain_same),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_same), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) same rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_same)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_same)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_same) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_same > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_same), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.3 : Neuchatel CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_neuc[, ])) / (max_na(emd_rain_neuc[, ]) + abs(min_na(emd_rain_neuc[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_neuc), max_na(emd_rain_neuc),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_neuc), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) neuc rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_neuc)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_neuc)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_neuc) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_neuc > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_neuc), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.3 : Lugano CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_luga[, ])) / (max_na(emd_rain_luga[, ]) + abs(min_na(emd_rain_luga[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_luga), max_na(emd_rain_luga),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_luga), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) luga rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_luga)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_luga)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_luga) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_luga > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_luga), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 5.3 : Geneve CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_gene[, ])) / (max_na(emd_rain_gene[, ]) + abs(min_na(emd_rain_gene[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_gene), max_na(emd_rain_gene),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_gene), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) gene rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_gene)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_gene)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_gene) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_gene > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_gene), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 6.3 : Chaumont CEEMDAN precipitation

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_rain_chau[, ])) / (max_na(emd_rain_chau[, ]) + abs(min_na(emd_rain_chau[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_rain_chau), max_na(emd_rain_chau),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_rain_chau), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
for(i in 1:length(x_axis_lab)){
  axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black", 
       mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
}
# mtext("a) chau rainerature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

contour(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = as.matrix(t(emd_rain_chau)),
        nlevels = n_iso_5,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_rain_chau)), xlim = c(0.5, 365.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:365, rep((nrow(emd_rain_chau) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_rain_chau > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)



par(mar = mar_2)

image_scale(as.matrix(emd_rain_chau), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 1.4: Sion CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_sion[, ])) / (alptempr::max_na(qannu_rain_sion[, ]) + abs(alptempr::min_na(qannu_rain_sion[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_sion), alptempr::max_na(qannu_rain_sion),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_sion), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) sion precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_sion),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_sion)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_sion) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_sion > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_sion), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.4: Samedan CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_same[, ])) / (alptempr::max_na(qannu_rain_same[, ]) + abs(alptempr::min_na(qannu_rain_same[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_same), alptempr::max_na(qannu_rain_same),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_same), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) same precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_same),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_same)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_same) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_same > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_same), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.4: Neuchatel CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_neuc[, ])) / (alptempr::max_na(qannu_rain_neuc[, ]) + abs(alptempr::min_na(qannu_rain_neuc[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_neuc), alptempr::max_na(qannu_rain_neuc),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_neuc), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) neuc precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_neuc),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_neuc)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_neuc) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_neuc > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_neuc), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.4: Lugano CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_luga[, ])) / (alptempr::max_na(qannu_rain_luga[, ]) + abs(alptempr::min_na(qannu_rain_luga[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_luga), alptempr::max_na(qannu_rain_luga),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_luga), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) luga precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_luga),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_luga)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_luga) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_luga > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_luga), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 5.4: Geneve CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_gene[, ])) / (alptempr::max_na(qannu_rain_gene[, ]) + abs(alptempr::min_na(qannu_rain_gene[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_gene), alptempr::max_na(qannu_rain_gene),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_gene), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) gene precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_gene),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_gene)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_gene) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_gene > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_gene), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 6.4: Chaumont CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_chau[, ])) / (alptempr::max_na(qannu_rain_chau[, ]) + abs(alptempr::min_na(qannu_rain_chau[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_chau), alptempr::max_na(qannu_rain_chau),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_chau), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
# mtext("b) chau precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

contour(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z =t(qannu_rain_chau),
        nlevels = n_iso_6,
        add = T,
        lwd = lwd_iso,
        labcex = cex_iso)

par(new = T)

par(mar = mar_1)

par(xpd=NA)
plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_rain_chau)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
points(1:99, rep((nrow(qannu_rain_chau) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_rain_chau > lev_sig, "#FFFFFF00", "black"))
par(xpd=F)


par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_chau), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

par(mar = c(0,0,0,0))

#Station names

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Sion (a)",    side = 2, line = -1.8, cex = cex_header, adj = 0.96)
mtext("Samedan (b)",   side = 2, line = -1.8, cex = cex_header, adj = 0.79)
mtext("Neuchatel (c)", side = 2, line = -1.8, cex = cex_header, adj = 0.61)
mtext("Lugano (d)", side = 2, line = -1.8, cex = cex_header, adj = 0.42)
mtext("Geneve (e)", side = 2, line = -1.8, cex = cex_header, adj = 0.24)
mtext("Chaumont (f)", side = 2, line = -1.8, cex = cex_header, adj = 0.04)

#Analytical method

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("1. Onset and evolution [°C]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.051)
mtext("2. Changes in quantiles [°C]", 
      side = 3, line = -3.75, cex = cex_header, adj = 0.352)
mtext("3. Onset and evolution [mm]",   
      side = 3, line = -3.75, cex = cex_header, adj = 0.658-0.005)
mtext("4. Changes in quantiles [mm]",  
      side = 3, line = -3.75, cex = cex_header, adj = 0.966-0.013)
mtext("Temperature",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.24)
mtext("Precipitation",  
      side = 3, line = -1.65, cex = cex_header, adj = 0.77)

dev.off()





#Fig_9----

#High Rhine reservoirs
#Wildenhahn und Klaholz 1996: Gro?e Speicherseen im Einzugsgebiet des Rheins, Internationale Kommission fur die Hydrologie des Rheingebietes (KHR)
# https://www.chr-khr.org/de/veroffentlichung/grosse-speicherseen-im-einzugsgebiet-des-rheins?position=16&list=zjXE9vdtnkjt146rdN7VhvwPTUHgowCdFNWGTSyOXtU

#Vorderrhein
name_vr <- c("Cumera", "Nalps", "Santa Maria", "Runcahez", "Zervreila", "Egschi")
volu_vr <- c(40.80, 44.50, 67.00, 0.44, 100.00, 0.40)
year_vr <- c(1966, 1962, 1968, 1961, 1957, 1949)
rese_vr <- cbind(name_vr, volu_vr, year_vr)

#Hinterrehin
name_hr <- c("Surner", "Valle di Lei", "B?reburg", "Marmorera", "Davoser See", "Solis", "Isel")
volu_hr <- c(18.30, 197.00, 1.00, 60.00, 11.30, 1.46, 0.3) 
year_hr <- c(1962, 1961, 1960, 1954, 1925, 1985, 1969)
rese_hr <- cbind(name_hr, volu_hr, year_hr)

#Tamina
name_ta <- c("Gigerwald", "Mapprag")
volu_ta <- c(33.40, 5.10)
year_ta <- c(1976, 1976)
rese_ta <- cbind(name_ta, volu_ta, year_ta)

#Thur
name_tu <- c("Seealpsee")
volu_tu <- c(0.60)
year_tu <- c(1905)
rese_tu <- cbind(name_tu, volu_tu, year_tu)

#Aare
name_aa <- c("Sanetsch", "Amensee", "Rossiniere", "Lac d'Hogrin", "Lessoc", "Montsalvens", "Rossens",  "Lac de Perolles",
             "Schiffenen", "Oberaar", "Tr?btensee", "Totensee", "Grimsel", "Raetherichboden", "Gelmer", "Mattenalp", "Engstlensee",
             "Wohlensee", "Stausee Niederried")
volu_aa <- c(2.70, 10.30, 1.70, 52.10, 0.75, 11.00, 180.00, 0.30, 35.50, 56.00, 1.00, 2.50, 98.70, 25.00, 13.40, 2.00, 2.00, 1.60, 0.40)
year_aa <- c(1965, 1942, 1972, 1968, 1973, 1920, 1947, 1870, 1963, 1953, 1950, 1950, 1932, 1950, 1929, 1950, 1961, 1920, 1913)
rese_aa <- cbind(name_aa, volu_aa, year_aa)

#Reuss
name_re <- c("Lucendro", "Oberalpsee", "G?scheneralpsee", "Schl?ttli (Seglis)", "Bannalpsee", "Lungernsee", "Wichelsee")
volu_re <- c(25.00, 0.83, 75.00, 0.35, 1.63, 50.00, 0.38)
year_re <- c(1947, 1963, 1960, 1965, 1976, 1921, 1957)
rese_re <- cbind(name_re, volu_re, year_re)

#Limmat
name_li <- c("Limmern", "Garichte", "Kloental", "Chapfensee", "Murgtal", "W?gitall/Schr?h", "Rempen", "Sihlsee", "Wettingen")
volu_li <- c(92.00, 2.90, 39.80, 0.50, 1.20, 80.30, 0.36, 91.80, 6.00)
year_li <- c(1963, 1931, 1910, 1948, 1925, 1924, 1924, 1936, 1933)
rese_li <- cbind(name_li, volu_li, year_li)

#Ill
name_il <- c("Silvretta", "Vermunt", "Kops", "Spullersee S?d/Nord", "L?nersee", "Raggal")
volu_il <- c(38.60, 5.30, 43.50, 15.70, 78.30, 2.00)
year_il <- c(1959, 1931, 1967, 1925, 1958, 1968)
rese_il <- cbind(name_il, volu_il, year_il)

#Bregenzerach
name_br <- c("Bolgenach")
volu_br <- c(8.40)
year_br <- c(1978)
rese_br <- cbind(name_br, volu_br, year_br)

#Bodensee
name_bo <- c("Andelshofer Weiher")
volu_bo <- c(1.40)
year_bo <- c(1942)
rese_bo <- cbind(name_bo, volu_bo, year_bo)

#Hochrhein (Deutschland)
name_or <- c("Mettma", "Schluchsee", "Schwarza-H?usern", "Schwarza-Witznau", "Albsperre", "Wehrabecken", "Bergsee")
volu_or <- c(1.75, 108.00, 1.70, 1.40, 2.30, 4.46, 0.50)
year_or <- c(1943, 1932, 1931, 1943, 1941, 1974, 1906)
rese_or <- cbind(name_or, volu_or, year_or)

#All resevoirs
rese_all <- rbind(rese_aa, rese_bo, rese_br, rese_hr, rese_il, 
                  rese_li, rese_or, rese_re, rese_ta, rese_tu, rese_vr)
reses <- data.frame(name = rese_all[, 1],
                    volu = as.numeric(rese_all[, 2]),
                    year = as.numeric(rese_all[, 3]))

reses_agg <- aggregate(reses$volu, by = list(years = reses$year), FUN = sum)
reses_agg$cum <- cumsum(reses_agg$x)

perc_vol <- c(
  round(reses_agg$cum[which(reses_agg$years == 1929)] / max(cumsum(reses_agg$x)), 2)*100,
  round(reses_agg$cum[which(reses_agg$years == 1936)] / max(cumsum(reses_agg$x)), 2)*100,
  round(reses_agg$cum[which(reses_agg$years == 1950)] / max(cumsum(reses_agg$x)), 2)*100,
  round(reses_agg$cum[which(reses_agg$years == 1960)] / max(cumsum(reses_agg$x)), 2)*100,
  round(reses_agg$cum[which(reses_agg$years == 1969)] / max(cumsum(reses_agg$x)), 2)*100
)

tota_vol <- c(
  reses_agg$cum[which(reses_agg$years == 1929)],
  reses_agg$cum[which(reses_agg$years == 1936)],
  reses_agg$cum[which(reses_agg$years == 1950)],
  reses_agg$cum[which(reses_agg$years == 1960)],
  reses_agg$cum[which(reses_agg$years == 1969)]
)



pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig9.pdf", width = 8.3, height = 3.0)
# pdf("/home/erwin/ownCloud/Fig9.pdf", width = 8.3, height = 3.0)

my_blu_2 <- rgb(44, 114, 142, max = 255, alpha = 50)

par(mar = c(1.2, 2.5, 0.2, 0.2))

plot(reses_agg$years, reses_agg$cum, xlim = c(1900, 1985), ylim = c(0, max(cumsum(reses_agg$x))), 
     type = "n", xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "")
rect(xleft = 180, ybottom = -1000, xright = 1985, ytop = max(cumsum(reses_agg$x)),
     col = my_blu_2, border = NA, lwd = 1)
rect(xleft = 180, ybottom = -1000, xright = 1970, ytop = tota_vol[5],
     col = my_blu_2, border = NA, lwd = 1)
rect(xleft = 180, ybottom = -1000, xright = 1960, ytop = tota_vol[4],
     col = my_blu_2, border = NA, lwd = 1)
rect(xleft = 180, ybottom = -1000, xright = 1950, ytop = tota_vol[3],
     col = my_blu_2, border = NA, lwd = 1)
rect(xleft = 180, ybottom = -1000, xright = 1940, ytop = tota_vol[2],
     col = my_blu_2, border = NA, lwd = 1)
# rect(xleft = 1800, ybottom = -1000, xright = 1930, ytop = tota_vol[1],
#      col = my_blu_2, border = NA, lwd = 1)
lines(reses_agg$years, reses_agg$cum, lwd = 1.8)
axis(1, at = c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980), c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980),
     mgp=c(3, 0.10, 0), tck = -0.015, cex.axis = 1.0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = 1.0)
mtext("Cum. storage volume [hm?]", side = 2, line = 1.5, cex = 1.2, adj = 0.5)
mtext(paste(perc_vol[2], "%"), side = 1, line = -2.8, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[3], "%"), side = 1, line = -5.4, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[4], "%"), side = 1, line = -7.8, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[5], "%"), side = 1, line = -11.5, cex = 1.2, adj = 0.07)
box(lwd = 1)

dev.off()



#Fig_10----


#Raster hydrograph Wasserburg

library(meltimr)
library(alptempr)
library(emdbook)

grdc_dir <- "/home/erwin/ownCloud/"

grdc_data <- read_grdc(paste0(grdc_dir, "6343100_Q_Day.Cmd.txt"))

#Order data by day (including break day to set start hydrologica year)
data_day <- ord_day(data_in = grdc_data$value,
                    date = grdc_data$date,
                    start_y = 1869,
                    end_y = 2016,
                    break_day = 0,
                    do_ma = F,
                    window_width = 30)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15


lab_unit <- "[m³/s]"
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(100)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(100)
cols_hydro <- c(cols_min, cols_max)

max_break <- max_na(data_day)
min_break <- min_na(data_day)
qua_break <- quantile(data_day, probs = 0.6, type = 8, na.rm = T)

breaks_1 <- lseq(min_break, qua_break, length.out = length(cols_hydro)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_hydro)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_hydro <- c(breaks_1, breaks_2)

pdf("/home/erwin/ownCloud/Fig10.pdf", width = 8.3, height = 4.5)

par(mar = c(1.6, 2.5, 0.2, 0.2))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:ncol(data_day),
      y = 1869:2016,
      z = t(data_day),
      col = cols_hydro,
      breaks = breaks_hydro,
      ylab = "", xlab = "", axes = F)

axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O", "N", "D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.55, 0), cex.axis = 1.8)#plot labels

# mtext("Year", side = 2, line = 2.0, cex = 1.2)
axis(2, mgp=c(3, 0.25, 0), tck = -0.01, cex.axis = 1.8)
box()

par(mar = c(1.6, 0.8, 2.5, 2.0))

alptempr::image_scale(as.matrix(data_day), col = cols_hydro, breaks = breaks_hydro, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.75, 0), tck = -0.08, cex.axis = 1.6)
mtext(lab_unit, side = 3, line = 0.6, cex = 1)

box()

dev.off()




#Fig_11----

#Elevation distribution and Parde-Coefficients

library(raster)
library(meltimr)
library(alptempr)

#Elevation boxplots individual basins
load("/home/erwin/ownCloud/ele_parde.RData")
# base_dir <- "U:/RhineFlow/rhine_obs/manus/figures/map/"

dem = raster(paste0(base_dir, "/eu_dem_500_fil.tif"))

#Load basin boundaries (shapefile delineated beforehand using Q-GIS)
my_basin_base <- paste0(base_dir, "basin_", "base", ".shp")
my_basin_koel <- paste0(base_dir, "basin_", "koel", ".shp")
my_basin_wass <- paste0(base_dir, "basin_", "wass", ".shp")
my_basin_wuer <- paste0(base_dir, "basin_", "wuer", ".shp")

basin_base <- rgdal::readOGR(dsn = my_basin_base)
basin_koel <- rgdal::readOGR(dsn = my_basin_koel)
basin_wass <- rgdal::readOGR(dsn = my_basin_wass)
basin_wuer <- rgdal::readOGR(dsn = my_basin_wuer)

#corp DEM sub-basin area
dem_cro_base <- raster::crop(dem, extent(basin_base))
dem_cro_koel <- raster::crop(dem, extent(basin_koel))
dem_cro_wass <- raster::crop(dem, extent(basin_wass))
dem_cro_wuer <- raster::crop(dem, extent(basin_wuer))
dem_sub_base <- mask(dem_cro_base, basin_base)
dem_sub_koel <- mask(dem_cro_koel, basin_koel)
dem_sub_wass <- mask(dem_cro_wass, basin_wass)
dem_sub_wuer <- mask(dem_cro_wuer, basin_wuer)

#get elevations of cells cropped dem
dem_ele_NA_base <- dem_sub_base@data@values
dem_ele_NA_koel <- dem_sub_koel@data@values
dem_ele_NA_wass <- dem_sub_wass@data@values
dem_ele_NA_wuer <- dem_sub_wuer@data@values

dem_ele_base <- dem_ele_NA_base[!is.na(dem_ele_NA_base)]
dem_ele_koel <- dem_ele_NA_koel[!is.na(dem_ele_NA_koel)]
dem_ele_wass <- dem_ele_NA_wass[!is.na(dem_ele_NA_wass)]
dem_ele_wuer <- dem_ele_NA_wuer[!is.na(dem_ele_NA_wuer)]
ele_list <- list(dem_ele_wass, dem_ele_base, dem_ele_koel, dem_ele_wuer)


#Parde-Coefficients

dir_grdc <- "d:/nrc_user/rottler/GRDC_DAY/"

grdc_data_wass <- read_grdc(paste0(dir_grdc, "6343100_Q_Day.Cmd.txt"))
grdc_data_base <- read_grdc(paste0(dir_grdc, "6935051_Q_Day.Cmd.txt"))
grdc_data_koel <- read_grdc(paste0(dir_grdc, "6335060_Q_Day.Cmd.txt"))
grdc_data_wuer <- read_grdc(paste0(dir_grdc, "6335500_Q_Day.Cmd.txt"))

#Order data by day 
data_day_wass <- ord_day(data_in = grdc_data_wass$value,
                         date = grdc_data_wass$date,
                         start_y = 1869,
                         end_y = 2016,
                         break_day = 0,
                         do_ma = F,
                         window_width = 30)

data_day_base <- ord_day(data_in = grdc_data_base$value,
                         date = grdc_data_base$date,
                         start_y = 1869,
                         end_y = 2016,
                         break_day = 0,
                         do_ma = F,
                         window_width = 30)

data_day_koel <- ord_day(data_in = grdc_data_koel$value,
                         date = grdc_data_koel$date,
                         start_y = 1869,
                         end_y = 2016,
                         break_day = 0,
                         do_ma = F,
                         window_width = 30)

data_day_wuer <- ord_day(data_in = grdc_data_wuer$value,
                         date = grdc_data_wuer$date,
                         start_y = 1869,
                         end_y = 2016,
                         break_day = 0,
                         do_ma = F,
                         window_width = 30)


yea_mea_wass <- apply(data_day_wass, 2, mea_na)
yea_mea_base <- apply(data_day_base, 2, mea_na)
yea_mea_koel <- apply(data_day_koel, 2, mea_na)
yea_mea_wuer <- apply(data_day_wuer, 2, mea_na)

plot(yea_mea_wuer)

month_ind <- c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30),
               rep(7, 31), rep(8, 31), rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31))

mon_mea_wass <- aggregate(yea_mea_wass, by = list(years = month_ind), FUN = mea_na)
mon_mea_base <- aggregate(yea_mea_base, by = list(years = month_ind), FUN = mea_na)
mon_mea_koel <- aggregate(yea_mea_koel, by = list(years = month_ind), FUN = mea_na)
mon_mea_wuer <- aggregate(yea_mea_wuer, by = list(years = month_ind), FUN = mea_na)

parde_ind_wass <- mon_mea_wass$x / mea_na(yea_mea_wass)
parde_ind_base <- mon_mea_base$x / mea_na(yea_mea_base)
parde_ind_koel <- mon_mea_koel$x / mea_na(yea_mea_koel)
parde_ind_wuer <- mon_mea_wuer$x / mea_na(yea_mea_wuer)

#Plot: Elevation + Parde-Coefficients

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig11.pdf", width = 8.3, height = 5.5)
pdf("/home/erwin/ownCloud/Fig11.pdf", width = 8.3, height = 5.5)

x_axis_lab <- 1:12
x_axis_tic <- (1:13)-0.5
col_wass <- viridis::viridis(9, direction = 1)[2] 
col_base <- viridis::viridis(9, direction = 1)[4]
col_koel <- "orange2"
col_wuer <- "orangered3"

# cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(100)
# cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(100)

par(mfrow = c(2, 1))
par(mar = c(1.8, 2.6, 1.5, 0.2))

plot(1:10, 1:10, type = "n", ylim = range(ele_list), xlim = c(0.5, 4.5), ylab = "", xlab = "", axes = F)
abline(h = c(0, 1000, 2000, 3000), lty = "dashed", col = "grey55", lwd = 0.8)
par(new = T)
boxplot(ele_list, axes = F, col = c(col_wass, col_base, col_koel, col_wuer))
axis(2, at = c(0, 1000, 2000, 3000), labels = c(0, 1000, 2000, 3000),
     mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = 1.2)
axis(1, at = c(1, 2, 3, 4), labels = c("Wasserb.", "Basel", "Koeln", "Wuerzb."),
     mgp=c(3, 0.15, 0), tck = -0.00001, cex.axis = 1.2)
mtext("Elevation", side = 2, line = 1.3, cex = 1.2, adj = 0.5)
mtext("a) Elevation distribution", side = 3, line = 0.2, cex = 1.6, adj = 0.0)
box()

ylims <- range(c(parde_ind_wass, parde_ind_base, parde_ind_koel, parde_ind_wuer))

plot(parde_ind_wass, type = "n", axes = F, ylim = ylims, ylab = "", xlab = "")
lines(parde_ind_wass, col = col_wass, lwd = 2.2)
lines(parde_ind_base, col = col_base, lwd = 2.2)
lines(parde_ind_koel, col = col_koel, lwd = 2.2)
lines(parde_ind_wuer, col = col_wuer, lwd = 2.2)
points(parde_ind_wass, col = col_wass, pch = 19, cex = 0.8)
points(parde_ind_base, col = col_base, pch = 19, cex = 0.8)
points(parde_ind_koel, col = col_koel, pch = 19, cex = 0.8)
points(parde_ind_wuer, col = col_wuer, pch = 19, cex = 0.8)
abline(v = c(x_axis_tic), lty = "dashed", col = "grey55", lwd = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = 1.2)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.08)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O", "N", "D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.35, 0), cex.axis = 1.2)#plot labels
mtext("Pard?-coefficient", side = 2, line = 1.3, cex = 1.2, adj = 0.5)
mtext("b) Pard?-coefficients", side = 3, line = 0.3, cex = 1.6, adj = 0.0)
box()

dev.off()




#table----

read_grdc <- function(file_path){
  
  data_grdc <- read.table(file_path, sep = ";", header = T, stringsAsFactors = F, na.strings = "-999.000")
  
  #define date
  data_grdc_date <- as.Date(strptime(data_grdc$YYYY.MM.DD, "%Y-%m-%d", tz="UTC"))
  #covert to time series object
  data_grdc_value <- data_grdc$Value
  
  data_grdc_ts <- data.frame(date  = data_grdc_date,
                             value = data_grdc_value)
  
  return(data_grdc_ts)
  
}

ord_day <- function(data_in, date, start_y = start_year, end_y = end_year){
  
  input_data_full <- data.frame(date = date, value = data_in)
  
  #Clip selected time period
  input_data <- input_data_full[as.numeric(format(input_data_full$date,'%Y')) >= start_y, ]
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) <= end_y, ]
  
  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_y,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_y,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")
  
  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, value[match(as.Date(full_date), as.Date(date))])
  )
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_y:end_y), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_y:end_y
  
  for(i in 0:(length(start_y:end_y)-1)) {
    
    data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]
    
  }
  
  return(data_day)
  
}

# 6343100_Q_Day.Cmd.txt Wasserburg (Inn)
# 6935051_Q_Day.Cmd.txt Basel (Rhine)
# 6335060_Q_Day.Cmd.txt Koeln (Rhine)
# 6335500_Q_Day.Cmd.txt Würzburg (Main)

grdc_data <- read_grdc(paste0("E:/GRDC_DAY/", "6335500_Q_Day.Cmd.txt"))

grdc_data_day <- ord_day(grdc_data$value, grdc_data$date, start_y = 1869, end_y = 2012)

mean(grdc_data_day)

annu_mea <- apply(grdc_data_day, 1, mean)

mean(annu_mea)


total_vol <- 1.86*10^9
days <- 31+31+28+31+30

total_vol / (days*24*365) / 144


#dfsdfsd----











line_lwd <- 0.7
sta_yea_meas <- 1956
end_yea_meas <- 1970

days_sel <- c(1, 15, 32, 46, 60, 74, 91, 105, 121, 135, 152, 166, 182, 196, 213, 227, 244, 258, 274, 288, 305, 329, 335)

pdf("u:/RhineFlow/rhine_obs/manus/figures/test_wuer.pdf", width = 16.6, height = 8)

for(i in 1:length(days_sel)){
  
day_sel <- days_sel[i]
emd_data <- emd_day_wass[, day_sel] # discharg Jan Wasserburg after 30DMA

emd_data_ori <- emd_day_wass_ori[, day_sel] # discharg Wasserburg original
qua_data <- emd_day_wass_ori[which(rownames(emd_day_wass_ori) == 1955),] #discharge Wasserburg 1955
emd_years <- sta_yea_emd:end_yea_emd
emd_out <- Rlibeemd::ceemdan(input = emd_data, ensemble_size = 10000, noise_strength = 0.5)
my_quants <-  seq(0.01,0.99,0.01)

dis_qua_yea <- quantile(qua_data, my_quants, type = 8)
dis_qua_day <- quantile(emd_data_ori, my_quants, type = 8)

imf_1_3 <- emd_out[, 1] + emd_out[, 2] + emd_out[, 3]
imf_4_6 <- emd_out[, 4] + emd_out[, 5] + emd_out[, 6]
residu <-  emd_out[, 7]

emd_data_mk <- as.numeric(zyp.trend.vector(emd_data, x =emd_years,  method = "zhang", conf.intervals = F)[6])
emd_data_sl <- as.numeric(zyp.trend.vector(emd_data, x =emd_years,  method = "zhang", conf.intervals = F)[2])
emd_data_in <- as.numeric(zyp.trend.vector(emd_data, x =emd_years,  method = "zhang", conf.intervals = F)[11])

#Observations after 30DMA
plot(emd_years, emd_data, type = "n", ylim = c(min(emd_data), max(emd_data)),
     xlab = "", ylab = "", axes = F, lwd = line_lwd)
abline(h = c(100,150,200,250), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, emd_data, lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext(paste0("e) Observations (30DMA) Day: ", day_sel), side = 3, line = 1.5, cex = cex_header, adj = 0.0)
mtext(paste0("Slope: ", round(emd_data_sl, 4), " MK: ", round(emd_data_mk, 4)), side = 3, line = 0.5, cex = 0.7, adj = 0.0)
box()

abline(a = emd_data_in, b = emd_data_sl, col = "red3")
lines(emd_years, residu, lwd = line_lwd)

}

dev.off()

year_sel <- 1955

dat_ori <- emd_day_wass_ori[which(rownames(emd_day_wass_ori) %in% c(1966:1975)), ]
dat_ori <- emd_day_koel_ori[which(rownames(emd_day_koel_ori) %in% c(1966:1975)), ]

#Oberservations 1955
plot(c(t(dat_ori)), type = "n", ylim = c(min(dat_ori), max(dat_ori)),
     xlab = "", ylab = "", axes = F)
# abline(h = c(200,600,1000,1400), lty = "dashed", col = "grey55", lwd = 0.8)
# abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(c(t(dat_ori)), lwd = line_lwd)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) Observations 1955", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

dev.off()











#Fig_2_old2----

day_sel <- 15
emd_data <- emd_day_wass[, day_sel] # discharg Jan Wasserburg after 30DMA
emd_data_ori <- emd_day_wass_ori[, day_sel] # discharg Wasserburg original
qua_data <- emd_day_wass_ori[which(rownames(emd_day_wass_ori) == 1955),] #discharge Wasserburg 1955
emd_years <- sta_yea_emd:end_yea_emd
emd_out <- Rlibeemd::ceemdan(input = emd_data, ensemble_size = 10000, noise_strength = 0.5)
my_quants <-  seq(0.01,0.99,0.01)

dis_qua_yea <- quantile(qua_data, my_quants, type = 8)
dis_qua_day <- quantile(emd_data_ori, my_quants, type = 8)

imf_1_3 <- emd_out[, 1] + emd_out[, 2] + emd_out[, 3]
imf_4_6 <- emd_out[, 4] + emd_out[, 5] + emd_out[, 6]
residu <-  emd_out[, 7]

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig2.pdf", width = 8.3, height = 8.5)

layout(matrix(c(9,1,3,5,7,
                9,2,4,6,8), 
              5, 2), heights = c(0.25,1,1,1))

# layout.show(n = 5)

par(family = "serif")

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

par(mar = c(2.0, 2.0, 2.3, 0.5))
line_lwd <- 1.7

#Oberservations 1955
plot(qua_data, type = "n", ylim = c(min(qua_data), max(qua_data)),
     xlab = "", ylab = "", axes = F)
abline(h = c(200,600,1000,1400), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(qua_data, lwd = line_lwd)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) Observations 1955", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#Oberservations selected day
plot(emd_years, emd_data_ori, type = "n", ylim = c(min(emd_data_ori), max(emd_data_ori)),
     xlab = "", ylab = "", axes = F)
abline(h = c(100,200,300,400), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, emd_data_ori, lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Observations 15. Jan", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#Quantiles selected year
plot(my_quants, dis_qua_yea, type = "n", ylim = c(min(dis_qua_yea), max(dis_qua_yea)),
     xlab = "", ylab = "", axes = F)
abline(h = c(200,400,600,800, 1000,1200), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = seq(0,1,0.1), lty = "dashed", col = "grey55", lwd = 0.8)
lines(my_quants, dis_qua_yea, type = "h", lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("c) Quantile values 1955", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#Quantiles selected day
plot(my_quants, dis_qua_day, type = "n", ylim = c(min(dis_qua_day), max(dis_qua_day)),
     xlab = "", ylab = "", axes = F)
abline(h = c(100,200,300,400), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = seq(0,1,0.1), lty = "dashed", col = "grey55", lwd = 0.8)
lines(my_quants, dis_qua_day, type = "h", lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) Quantile values 15. Jan", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#Observations after 30DMA
plot(emd_years, emd_data, type = "n", ylim = c(min(emd_data), max(emd_data)),
     xlab = "", ylab = "", axes = F, lwd = line_lwd)
abline(h = c(100,150,200,250), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, emd_data, lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("e) Observations 15. Jan (30DMA)", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()


#IMF 1-3
plot(emd_years, imf_1_3, type = "n", xlab = "", ylab = "", axes = F, col = "black")
abline(h = c(-40,0,40,80), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, imf_1_3, lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) IMF 1-3", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#IMF 4-6
plot(emd_years, imf_4_6, type = "n", xlab = "", ylab = "", axes = F, col = "black")
abline(h = c(-40,-20,0,20,40,80), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, imf_4_6, lwd = line_lwd )
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("g) IMF 4-6", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#Residual
plot(emd_years, residu, type = "l", ylim = c(min(emd_data), max(emd_data)),
     xlab = "", ylab = "", axes = F, col = "black")
abline(h = c(100,150,200,250), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1880, 1900, 1920, 1940, 1960, 1980, 2000), lty = "dashed", col = "grey55", lwd = 0.8)
lines(emd_years, residu, lwd = line_lwd)
axis(1, mgp=c(3, x_lab_posi, 0), tck = -0.02, cex.axis = cex_y_axis)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) IMF 7 (Residual)", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Discharge Wasserburg [m³/s]",
      side = 3, line = -2.5, cex = cex_header, adj = 0.52)

dev.off()


#Fig_3_old2----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig3.pdf", width = 8.3, height = 5.0)

layout(matrix(c(5,1,3,
                5,2,4), 
              3, 2), heights = c(0.2,1,1))

# layout.show(n = 5)

par(family = "serif")

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

par(mar = c(2.0, 2.0, 2.3, 0.5))
line_lwd <- 1.7

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))
col_10 <- "red4"
col_50 <- "black"
col_90 <- viridis::viridis(9, direction = 1)[4]

quant_low <-  95
quant_mid <-  50
quant_high <- 5

#1.1 Wasserburg
plot(qvalu_wass[, quant_high], type = "n", ylim = c(min_na(qvalu_wass[,c(quant_high,quant_mid,quant_low)]), max_na(qvalu_wass[,c(quant_high,quant_mid,quant_low)])), axes = F, ylab = "", xlab = "")
abline(h = c(200,400,600,800,1000), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(qvalu_wass[, quant_high], lwd =line_lwd, col = col_90)
lines(qvalu_wass[, quant_mid], lwd =line_lwd, col = col_50)
lines(qvalu_wass[, quant_low], lwd =line_lwd, col = col_10)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) Wasserburg", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
legend("topleft", c("0.95", "0.50", "0.05"), col = c(col_90, col_50, col_10),
       pch = 19, cex = 1.7, bg = "white")
box()

#1.2 Basel
plot(qvalu_base[, quant_high], type = "n", ylim = c(min_na(qvalu_base[,c(quant_high,quant_mid,quant_low)]), max_na(qvalu_base[,c(quant_high,quant_mid,quant_low)])), axes = F, ylab = "", xlab = "")
abline(h = c(500,1000,1500,2000), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(qvalu_base[, quant_high], lwd =line_lwd, col = col_90)
lines(qvalu_base[, quant_mid], lwd =line_lwd, col = col_50)
lines(qvalu_base[, quant_low], lwd =line_lwd, col = col_10)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Basel", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#2.1 Koeln
plot(qvalu_koel[, quant_high], type = "n", ylim = c(min_na(qvalu_koel[,c(quant_high,quant_mid,quant_low)]), max_na(qvalu_koel[,c(quant_high,quant_mid,quant_low)])), axes = F, ylab = "", xlab = "")
abline(h = c(1000,2000,3000,4000,5000,6000), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(qvalu_koel[, quant_high], lwd =line_lwd, col = col_90)
lines(qvalu_koel[, quant_mid], lwd =line_lwd, col = col_50)
lines(qvalu_koel[, quant_low], lwd =line_lwd, col = col_10)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("c) Koeln", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

#2.2 Wuerzburg
plot(qvalu_wuer[, quant_high], type = "n", ylim = c(min_na(qvalu_wuer[,c(quant_high,quant_mid,quant_low)]), max_na(qvalu_wuer[,c(quant_high,quant_mid,quant_low)])), axes = F, ylab = "", xlab = "")
abline(h = c(100,200,300,400), lty = "dashed", col = "grey55", lwd = 0.8)
abline(v = c(1,x_axis_tic,365), lty = "dashed", col = "grey55", lwd = 0.8)
lines(qvalu_wuer[, quant_high], lwd =line_lwd, col = col_90)
lines(qvalu_wuer[, quant_mid], lwd =line_lwd, col = col_50)
lines(qvalu_wuer[, quant_low], lwd =line_lwd, col = col_10)
axis(1, at = c(1,x_axis_tic,365), c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) Wuerzburg", side = 3, line = 0.5, cex = cex_header, adj = 0.0)
box()

par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Seasonality of runoff [m³/s]",
      side = 3, line = -2.5, cex = cex_header, adj = 0.52)

dev.off()

#Fig_3_old----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig3.pdf", width = 8.3, height = 8)

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

par(family = "serif")

#Figure 1.1: Wasserburg CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_wass[, ])) / (max_na(emd_disc_wass[, ]) + abs(min_na(emd_disc_wass[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_wass), max_na(emd_disc_wass),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_wass), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("a) Wasserb. runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_disc_wass), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Wasserburg CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass[, ])) / (alptempr::max_na(qannu_wass[, ]) + abs(alptempr::min_na(qannu_wass[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass), alptempr::max_na(qannu_wass),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Wasserb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Basel CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_base[, ])) / (max_na(emd_disc_base[, ]) + abs(min_na(emd_disc_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_base), max_na(emd_disc_base),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_base), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("c) Basel runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_disc_base), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Basel CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base[, ])) / (alptempr::max_na(qannu_base[, ]) + abs(alptempr::min_na(qannu_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base), alptempr::max_na(qannu_base),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) Basel runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Koeln CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_koel[, ])) / (max_na(emd_disc_koel[, ]) + abs(min_na(emd_disc_koel[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_koel), max_na(emd_disc_koel),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_koel), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("e) Koeln runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_disc_koel), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Koeln CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel[, ])) / (alptempr::max_na(qannu_koel[, ]) + abs(alptempr::min_na(qannu_koel[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel), alptempr::max_na(qannu_koel),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) Koeln runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Wuerzburg CEEMDAN discharge

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_disc_wuer[, ])) / (max_na(emd_disc_wuer[, ]) + abs(min_na(emd_disc_wuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_disc_wuer), max_na(emd_disc_wuer),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_disc_wuer), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("g) Wuerzb. runoff val. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_disc_wuer), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Wuerzwuer CEEMDAN discharge quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer[, ])) / (alptempr::max_na(qannu_wuer[, ]) + abs(alptempr::min_na(qannu_wuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer), alptempr::max_na(qannu_wuer),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) Wuerzb. runoff qua. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



dev.off()





#Fig_2_old----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig2.pdf", width = 8.3, height = 8)

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

#layout.show(n = 32)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

ytiks      <- seq(10, 90, by =  10)
ylabs      <- seq(90, 10, by = -10) 

cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                            "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)

par(family = "serif")

#Figure 1.1: Waserburg quantiles

max_break <- max_na(qvalu_wass)
min_break <- min_na(qvalu_wass)
qua_break <- quantile(qvalu_wass, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_wass)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_wass), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) Wasserb. runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_wass), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 1.2: Waserburg trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_wass)) / (alptempr::max_na(qvslo_wass) + abs(alptempr::min_na(qvslo_wass))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_wass), alptempr::max_na(qvslo_wass), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_wass)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_wass), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Wasserb. runoff [(m³/s)/year)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_wass), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Basel quantiles

max_break <- max_na(qvalu_base)
min_break <- min_na(qvalu_base)
qua_break <- quantile(qvalu_base, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_base)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_base), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("c) Basel runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_base), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 2.2: Basel trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_base)) / (alptempr::max_na(qvslo_base) + abs(alptempr::min_na(qvslo_base))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_base), alptempr::max_na(qvslo_base), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_base)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_base), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) Basel runoff [(m³/s)/year)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_base), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Koeln quantiles

max_break <- max_na(qvalu_koel)
min_break <- min_na(qvalu_koel)
qua_break <- quantile(qvalu_koel, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_koel)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_koel), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("e) Koeln runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_koel), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 3.2: Koeln trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_koel)) / (alptempr::max_na(qvslo_koel) + abs(alptempr::min_na(qvslo_koel))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_koel), alptempr::max_na(qvslo_koel), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_koel)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_koel), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) Koeln runoff [(m³/s)/year)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_koel), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 4.1: Wuerzburg quantiles

max_break <- max_na(qvalu_wuer)
min_break <- min_na(qvalu_wuer)
qua_break <- quantile(qvalu_wuer, probs = 0.7, type = 8, na.rm = T)

breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_qvalu <- c(breaks_1, breaks_2)

y <- 1:ncol(qvalu_wuer)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvalu_wuer), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("g) Wurzb. runoff [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvalu_wuer), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 4.2: Wuerzwuer trend window quantiles

n_max <- round(abs(alptempr::max_na(qvslo_wuer)) / (alptempr::max_na(qvslo_wuer) + abs(alptempr::min_na(qvslo_wuer))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_qvslo <- c(cols_min, cols_max)

breaks_qvslo <-  seq(alptempr::min_na(qvslo_wuer), alptempr::max_na(qvslo_wuer), length.out = length(cols_qvslo) +1)

y <- 1:ncol(qvslo_wuer)
x <- 1:365

par(mar = mar_1)

image(x, y, as.matrix(qvslo_wuer), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
      xlab = "", axes = F)

axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) Wuerzb. runoff [(m³/s)/year)]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qvslo_wuer), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


dev.off()



#Fig_4_old----
  
pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig4.pdf", width = 8.3, height = 8)

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

#Figure 1.1 : Bern CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_bern[, ])) / (max_na(emd_temp_bern[, ]) + abs(min_na(emd_temp_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_bern), max_na(emd_temp_bern),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_bern), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06, cex.axis = cex_x_axis)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("a) Bern temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_temp_bern), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Bern CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_bern[, ])) / (alptempr::max_na(qannu_rain_bern[, ]) + abs(alptempr::min_na(qannu_rain_bern[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_bern), alptempr::max_na(qannu_rain_bern),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_bern), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) Bern precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_bern), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 2.1: Basel CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_base[, ])) / (max_na(emd_temp_base[, ]) + abs(min_na(emd_temp_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_base), max_na(emd_temp_base),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_base), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("c) Basel temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_temp_base), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_base[, ])) / (alptempr::max_na(qannu_rain_base[, ]) + abs(alptempr::min_na(qannu_rain_base[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_base), alptempr::max_na(qannu_rain_base),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_base), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) Basel precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_base), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1 : Zuerich CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_zuer[, ])) / (max_na(emd_temp_zuer[, ]) + abs(min_na(emd_temp_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_zuer), max_na(emd_temp_zuer),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_zuer), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("e) Zuerich temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()


par(mar = mar_2)

image_scale(as.matrix(emd_temp_zuer), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Zuerich: CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_zuer[, ])) / (alptempr::max_na(qannu_rain_zuer[, ]) + abs(alptempr::min_na(qannu_rain_zuer[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_zuer), alptempr::max_na(qannu_rain_zuer),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_zuer), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) Zuerich precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_zuer), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()



#Figure 4.1: Hohenpeissenberg CEEMDAN temperature

par(mar = mar_1)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
n_max <- round(abs(max_na(emd_temp_hohe[, ])) / (max_na(emd_temp_hohe[, ]) + abs(min_na(emd_temp_hohe[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_emd <- c(cols_min, cols_max)

brea_emd <- c(seq(min_na(emd_temp_hohe), max_na(emd_temp_hohe),length.out = length(cols_emd)+1))

image(x = 1:365,
      y = sta_yea_emd:end_yea_emd,
      z = t(emd_temp_hohe), 
      col    = cols_emd, 
      breaks = brea_emd,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot labels
mtext("g) Hohenp. temperature [°C]", side = 3, line = 0.3, cex = cex_header, adj = 0)
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
box()

par(mar = mar_2)

image_scale(as.matrix(emd_temp_hohe), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()

#Figure 4.2: Hohenpeissenberg: CEEMDAN precipitation quantiles

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_rain_hohe[, ])) / (alptempr::max_na(qannu_rain_hohe[, ]) + abs(alptempr::min_na(qannu_rain_hohe[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_rain_hohe), alptempr::max_na(qannu_rain_hohe),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_rain_hohe), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) Hohenp. precipitation [mm]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_rain_hohe), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


dev.off()




#Fig_5_old----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig5.pdf", width = 8.3, height = 8)

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

par(family = "serif")

#Figure 1.1: Wasserburg CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_spr[, ])) / (alptempr::max_na(qannu_wass_spr[, ]) + abs(alptempr::min_na(qannu_wass_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_spr), alptempr::max_na(qannu_wass_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) MAM Wasserb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Basel CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_spr[, ])) / (alptempr::max_na(qannu_base_spr[, ]) + abs(alptempr::min_na(qannu_base_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_spr), alptempr::max_na(qannu_base_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) MAM Basel [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Wasserburg CEEMDAN discharge quantiles summmer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_sum[, ])) / (alptempr::max_na(qannu_wass_sum[, ]) + abs(alptempr::min_na(qannu_wass_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_sum), alptempr::max_na(qannu_wass_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("c) JJA Wasserb [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Basel CEEMDAN discharge quantiles summmer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_sum[, ])) / (alptempr::max_na(qannu_base_sum[, ]) + abs(alptempr::min_na(qannu_base_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_sum), alptempr::max_na(qannu_base_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) JJA Basel [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Wasserburg CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_aut[, ])) / (alptempr::max_na(qannu_wass_aut[, ]) + abs(alptempr::min_na(qannu_wass_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_aut), alptempr::max_na(qannu_wass_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("e) SON Wasserb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Basel CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_aut[, ])) / (alptempr::max_na(qannu_base_aut[, ]) + abs(alptempr::min_na(qannu_base_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_aut), alptempr::max_na(qannu_base_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) SON Basel [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Wasserburg CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wass_win[, ])) / (alptempr::max_na(qannu_wass_win[, ]) + abs(alptempr::min_na(qannu_wass_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wass_win), alptempr::max_na(qannu_wass_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wass_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("g) DJF Wasserb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wass_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Basel CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_base_win[, ])) / (alptempr::max_na(qannu_base_win[, ]) + abs(alptempr::min_na(qannu_base_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_base_win), alptempr::max_na(qannu_base_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_base_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) DJF Basel [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_base_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


dev.off()





#Fig_6_old----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig6.pdf", width = 8.3, height = 8)

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

par(family = "serif")

#Figure 1.1: Koeln CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_spr[, ])) / (alptempr::max_na(qannu_koel_spr[, ]) + abs(alptempr::min_na(qannu_koel_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_spr), alptempr::max_na(qannu_koel_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("a) MAM Koeln [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 1.2: Wuerzburg CEEMDAN discharge quantiles spring

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_spr[, ])) / (alptempr::max_na(qannu_wuer_spr[, ]) + abs(alptempr::min_na(qannu_wuer_spr[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_spr), alptempr::max_na(qannu_wuer_spr),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_spr), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("b) MAM Wuerzb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_spr), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.1: Koeln CEEMDAN discharge quantiles summmer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_sum[, ])) / (alptempr::max_na(qannu_koel_sum[, ]) + abs(alptempr::min_na(qannu_koel_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_sum), alptempr::max_na(qannu_koel_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("c) JJA Koeln [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 2.2: Wuerzburg CEEMDAN discharge quantiles summmer

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_sum[, ])) / (alptempr::max_na(qannu_wuer_sum[, ]) + abs(alptempr::min_na(qannu_wuer_sum[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_sum), alptempr::max_na(qannu_wuer_sum),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_sum), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("d) JJA Wuerzb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_sum), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.1: Koeln CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_aut[, ])) / (alptempr::max_na(qannu_koel_aut[, ]) + abs(alptempr::min_na(qannu_koel_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_aut), alptempr::max_na(qannu_koel_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("e) SON Koeln [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 3.2: Wuerzburg CEEMDAN discharge quantiles autumn

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_aut[, ])) / (alptempr::max_na(qannu_wuer_aut[, ]) + abs(alptempr::min_na(qannu_wuer_aut[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_aut), alptempr::max_na(qannu_wuer_aut),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_aut), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("f) SON Wuerzb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_aut), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.1: Koeln CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_koel_win[, ])) / (alptempr::max_na(qannu_koel_win[, ]) + abs(alptempr::min_na(qannu_koel_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_koel_win), alptempr::max_na(qannu_koel_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_koel_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("g) DJF Koeln [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_koel_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


#Figure 4.2: Wuerzburg CEEMDAN discharge quantiles winter

par(mar = mar_1)

x_axis_tic <- seq(10, 90, by = 10)
n_max <- round(abs(alptempr::max_na(qannu_wuer_win[, ])) / (alptempr::max_na(qannu_wuer_win[, ]) + abs(alptempr::min_na(qannu_wuer_win[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)

cols_scale <- c(cols_min, cols_max)
brea_scale <- c(seq(alptempr::min_na(qannu_wuer_win), alptempr::max_na(qannu_wuer_win),length.out = length(cols_scale)+1))

image(x = 1:99,
      y = sta_yea_ann:end_yea_ann,
      z = t(qannu_wuer_win), 
      col    = cols_scale, 
      breaks = brea_scale,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
     col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
mtext("h) DJF Wuerzb. [m³/s]", side = 3, line = 0.3, cex = cex_header, adj = 0)
box()

par(mar = mar_2)

alptempr::image_scale(as.matrix(qannu_wuer_win), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

box()


dev.off()





#Fig_wtc----

pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig5.pdf", width = 12, height = 8)

layout(matrix(c(1,3,5, 2,4,6),
              3, 2), widths=c(), heights=c(1,1,1,1))
par(oma = c(0,0,0,0))

mar_1 <- c(2.2, 2.0, 2.0, 0.5)
mar_2 <- c(3.2, 2.2, 2.2, 0.5)

gap_lenght <- 2
lwd_bar <- 2.5
gaps_wtc_plot <- 0:25 * gap_lenght
size_y_labs <- 1.5
size_x_labs <- 1.5
size_main <- 1.5
line_main <- 0.3
x_lab_posi <- c(1:8, 10, 12, 14, 16, 18, 20, 22, 24, 26)

par(family = "serif")

#Plot a: Mean annual frequency of weather types

par(mar = mar_1)

my_ylim <- c(0, max_na(wtc_fre_mea) + 5)
my_xlim <- c(0.5, 26.5)

plot(wtc_fre_mea, pch = 19, type = "h", lwd = 8, lend = 1, axes = F, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i", ylim = my_ylim, xlim = my_xlim)
axis(1, at = (1:27)-0.5, labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_lab_posi, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed")
abline(v = c(8.5, 16.5, 24.5), lty = "dashed")
mtext("a) Annaul GWT frequency [ndays]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
pos_labs <- (1:26)
for (i in 1:8){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
}
for (i in 9:16){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
}
for (i in 17:24){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
}
mtext("cyclonic",                  side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
mtext("anticyclonic",              side = 3, line = -1.8, adj = 0.45, padj = 1, cex = 0.8)
mtext("indifferent",               side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
mtext("low pressure",              side = 4, line = -3.8, adj = 0.90, padj = 1, cex = 0.8)
mtext("high pressure",             side = 4, line = -2.1, adj = 0.925, padj = 1, cex = 0.8)

#Plot b: Trend annual frequency of weather types

my_ylim <- c(min_na(wtc_fre_slo) - 0.2, max_na(wtc_fre_slo) + 0.2)
my_xlim <- c(0.5, 26.5)

plot(wtc_fre_slo, pch = 19, type = "h", lwd = 8, lend = 1, axes = F, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i", ylim = my_ylim, xlim = my_xlim)
axis(1, at = (1:27)-0.5, labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_lab_posi, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed")
abline(v = c(8.5, 16.5, 24.5), lty = "dashed")
mtext("b) Trend annaul GWT frequency [ndays/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
pos_labs <- (1:26)
for (i in 1:8){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
}
for (i in 9:16){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 1, line = - 1.2)
}
for (i in 17:24){
  mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
}
mtext("cyclonic",                  side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
mtext("anticyclonic",              side = 1, line = -3.2, adj = 0.45, padj = 1, cex = 0.8)
mtext("indifferent",               side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
mtext("low pressure",              side = 4, line = -3.8, adj = 0.10, padj = 1, cex = 0.8)
mtext("high pressure",             side = 4, line = -2.1, adj = 0.10, padj = 1, cex = 0.8)


# Plot c: Mean precipitation per GWT

gap_lenght <- 2
lwd_bar <- 2.5
gaps_wtc_plot <- 0:25 * gap_lenght

my_ylim <- c(0, max_na(c(wtc_mea_mea_bas, wtc_mea_mea_ber, wtc_mea_mea_sma, wtc_mea_mea_hoh)) + 1.2)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_mea_mea_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_mea_mea_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_mea_mea_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_mea_hoh, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("c) Precipitation per occurence [mm/day]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

mtext("Basel",     side = 3, line = -3.0 + 2.5, adj = 0.005, padj = 1, cex = 0.6)
mtext("Bern",      side = 3, line = -3.6 + 2.5, adj = 0.016, padj = 1, cex = 0.6)
mtext("Zuerich",   side = 3, line = -4.2 + 2.5, adj = 0.022,  padj = 1, cex = 0.6)
mtext("Hohenp.",   side = 3, line = -4.8 + 2.5, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(9.9, 11.25), type = "l", lwd = 0.5)
lines(c(2,2), c(9.9, 10.80), type = "l", lwd = 0.5)
lines(c(3,3), c(9.9, 10.35), type = "l", lwd = 0.5)
lines(c(4,4), c(9.9, 10.00), type = "l", lwd = 0.5)


# Plot: d: Trend mean precipiation per day

my_ylim <- c(min_na(c(wtc_mea_slo_bas, wtc_mea_slo_ber, wtc_mea_slo_sma, wtc_mea_slo_hoh)) - 0.03, 
             max_na(c(wtc_mea_slo_bas, wtc_mea_slo_ber, wtc_mea_slo_sma, wtc_mea_slo_hoh)) + 0.03)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_mea_slo_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_mea_slo_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_mea_slo_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_slo_hoh, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("d) Trend precipiation per occur. [(mm/day)/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)


# Plot e: Mean annual sum

par(mar = mar_2)

my_ylim <- c(0, max_na(c(wtc_sum_mea_bas, wtc_sum_mea_ber, wtc_sum_mea_sma, wtc_sum_mea_hoh)) + 5)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_sum_mea_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_sum_mea_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_sum_mea_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_sum_mea_hoh, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("e) Annual total precipiation [mm/year]", side = 3, line = line_main, cex = size_main, adj = 0)
mtext("GWT26 weather type", side = 1, line = 2, cex = 1.2, adj = 0.5)
box(lwd = 1.2)

mtext("Basel",     side = 3, line = -3.0 - 0.8, adj = 0.005, padj = 1, cex = 0.6)
mtext("Bern",      side = 3, line = -3.6 - 0.8, adj = 0.016, padj = 1, cex = 0.6)
mtext("Zuerich",   side = 3, line = -4.2 - 0.8, adj = 0.022, padj = 1, cex = 0.6)
mtext("Hohenp.",   side = 3, line = -4.8 - 0.8, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(75+0, 129-20), type = "l", lwd = 0.5)
lines(c(2,2), c(75+0, 122-20), type = "l", lwd = 0.5)
lines(c(3,3), c(75+0, 116-20), type = "l", lwd = 0.5)
lines(c(4,4), c(75+0, 110-20), type = "l", lwd = 0.5)

# Plot f: Trend annual sum

my_ylim <- c(min_na(c(wtc_sum_slo_bas, wtc_sum_slo_ber, wtc_sum_slo_sma, wtc_sum_slo_hoh)) - 0.5,
             max_na(c(wtc_sum_slo_bas, wtc_sum_slo_ber, wtc_sum_slo_sma, wtc_sum_slo_hoh)) + 0.5)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_sum_slo_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_sum_slo_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_sum_slo_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_sum_slo_hoh, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("f) Trend annual total precipiation [(mm/year)/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
mtext("GWT26 weather type", side = 1, line = 2, cex = 1.2, adj = 0.5)
box(lwd = 1.2)


dev.off()




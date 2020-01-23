###

#Long-term changes in river runoff - Export figures
#Erwin Rottler, University of Potsdam

###

pacman::p_load(sp, alptempr, zoo, emdbook, viridis, Rlibeemd, zyp, meltimr, raster)

load("U:/RhineFlow/rhine_obs/R/figs_manus/riv_flow.Rdata")

#General plot parameter
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
par(family = "serif")
lev_sig <- 0.05

#plot_functs----

#Seasonality of river runoff
plot_quan_doy <- function(qvalu_in){
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  ytiks      <- seq(10, 90, by =  10)
  ylabs      <- seq(10, 90, by =  10)
  
  cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1)]))(100)
  cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2","gold2", "yellow2", "white"))(100)
  cols_qvalu <- c(cols_min, cols_max)
  
  probs_iso <- c(0.1, 0.5, 0.9)
  break_quant <- 0.5
  par(family = "serif")
  
  max_break <- max_na(qvalu_in)
  min_break <- min_na(qvalu_in)
  qua_break <- quantile(qvalu_in, probs = break_quant, type = 8, na.rm = T)
  iso_def <- quantile(qvalu_in, probs = probs_iso, type = 8, na.rm = T)
  
  breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
  breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
  breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1
  
  breaks_qvalu <- c(breaks_1, breaks_2)
  
  y <- 1:ncol(qvalu_in)
  x <- 1:365
  
  par(mar = mar_1)

  image(x, y, as.matrix(qvalu_in), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
        xlab = "", axes = F)
  
  axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.05, cex.axis = cex_x_axis)#plot ticks
  for(i in 1:length(x_axis_lab)){
    axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black",
         mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
  }
  box()

  contour(x = x,
          y = y,
          z = as.matrix(qvalu_in),
          levels = round(iso_def, 0),
          add = T,
          lwd = lwd_iso,
          labcex = cex_iso)

  par(mar = mar_2)

  alptempr::image_scale(as.matrix(qvalu_in), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

  box()

}

#Changes in seasonality of river runoff
plot_mov_quan <- function(qvslo_in, n_iso = 6){
  
  par(family = "serif")
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  ytiks      <- seq(10, 90, by =  10)
  ylabs      <- seq(10, 90, by =  10)
  
  n_max <- round(abs(alptempr::max_na(qvslo_in)) / (alptempr::max_na(qvslo_in) + abs(alptempr::min_na(qvslo_in))), digits = 2) * 200
  n_min <- 200 - n_max
  cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1)]))(n_max)
  cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2","gold2", "yellow2", "white"))(n_min)
  cols_qvslo <- c(cols_min, cols_max)
  
  breaks_qvslo <-  seq(alptempr::min_na(qvslo_in), alptempr::max_na(qvslo_in), length.out = length(cols_qvslo) +1)
  
  y <- 1:ncol(qvslo_in)
  x <- 1:365
  
  par(mar = mar_1)

  image(x, y, as.matrix(qvslo_in), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
        xlab = "", axes = F)
  axis(2, at = ytiks, labels = ylabs/100, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.05, cex.axis = cex_x_axis)#plot ticks
  for(i in 1:length(x_axis_lab)){
    axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black",
         mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
  }
  box()

  contour(x = x,
          y = y,
          z = as.matrix(qvslo_in),
          nlevels = n_iso,
          add = T,
          lwd = lwd_iso,
          labcex = cex_iso)

  par(mar = mar_2)

  alptempr::image_scale(as.matrix(qvslo_in), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

  box()
  
}

#Onset and evolution of changes
plot_emd_val <- function(emd_val_in, n_iso = 8, rev_cols = F){
  
  par(family = "serif")
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  ytiks      <- seq(10, 90, by =  10)
  ylabs      <- seq(10, 90, by =  10)
  
  par(mar = mar_1)
  
  n_max <- round(abs(max_na(emd_val_in[, ])) / (max_na(emd_val_in[, ]) + abs(min_na(emd_val_in[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1)]))(n_max)
  cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2", "gold2", "yellow2", "white"))(n_min)
  if(rev_cols){
    cols_max <- grDevices::colorRampPalette(c("white", "yellow2", "gold2", "orange2", "orangered4", "red4"))(n_max)
    cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[c(1:4)], "cadetblue3", "white"))(n_min)
  }
  
  cols_emd <- c(cols_min, cols_max)
  
  brea_emd <- c(seq(min_na(emd_val_in), max_na(emd_val_in),length.out = length(cols_emd)+1))

  image(x = 1:365,
        y = sta_yea_emd:end_yea_emd,
        z = t(emd_val_in), 
        col    = cols_emd, 
        breaks = brea_emd,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.06)#plot ticks
  for(i in 1:length(x_axis_lab)){
    axis(1, at = x_axis_lab[i], lab_months[i], tick = FALSE, col="black", col.axis="black",
         mgp=c(4, x_lab_posi, 0), cex.axis = cex_x_axis)
  }
  axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
  box()

  contour(x = 1:365,
          y = sta_yea_ann:end_yea_ann,
          z = t(emd_val_in),
          nlevels = n_iso,
          add = T,
          lwd = lwd_iso,
          labcex = cex_iso)

  par(new = T)

  par(mar = mar_1)

  par(xpd=NA)
  plot(1:365, rep(1, 365), ylim = c(1, nrow(emd_val_in)), xlim = c(1, 365), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
  points(1:365, rep((nrow(emd_val_in) + 5), 365), pch = 19, cex = 0.25, col = ifelse(emd_mk_wass > lev_sig, "#FFFFFF00", "black"))
  par(xpd=F)

  par(mar = mar_2)

  image_scale(as.matrix(emd_val_in), col = cols_emd, breaks = brea_emd, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

  box()
  
  
}

#Changes in intesity
plot_emd_quan <- function(qannu_in, n_iso = 12, rev_cols = F){
  
  par(family = "serif")
  
  par(mar = mar_1)
  
  x_axis_tic <- seq(10, 90, by = 10)
  n_max <- round(abs(alptempr::max_na(qannu_in[, ])) / (alptempr::max_na(qannu_in[, ]) + abs(alptempr::min_na(qannu_in[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1)]))(n_max)
  cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2","gold2", "yellow2", "white"))(n_min)
  if(rev_cols){
    cols_max <- grDevices::colorRampPalette(c("white", "yellow2", "gold2", "orange2", "orangered4", "red4"))(n_max)
    cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[c(1:4)], "cadetblue3", "white"))(n_min)
  }
  
  cols_scale <- c(cols_min, cols_max)
  brea_scale <- c(seq(alptempr::min_na(qannu_in), alptempr::max_na(qannu_in),length.out = length(cols_scale)+1))

  image(x = 1:99,
        y = sta_yea_ann:end_yea_ann,
        z = t(qannu_in), 
        col    = cols_scale, 
        breaks = brea_scale,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, x_axis_tic/100, tick = TRUE,
       col = "black", col.axis = "black", tck = -0.02, mgp=c(3, x_lab_posi, 0), cex.axis = cex_x_axis)#plot ticks
  axis(2, mgp=c(3, 0.15, 0), tck = -0.02, cex.axis = cex_y_axis)
  box()

  contour(x = 1:99,
          y = sta_yea_ann:end_yea_ann,
          z = t(qannu_in),
          nlevels = n_iso,
          add = T,
          lwd = lwd_iso,
          labcex = cex_iso)

  par(new = T)

  par(mar = mar_1)

  par(xpd=NA)
  plot(1:99, rep(1, 99), ylim = c(1, nrow(qannu_in)), xlim = c(0.5, 99.5), axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", type = "n")
  points(1:99, rep((nrow(qannu_in) + 5), 99), pch = 19, cex = 0.25, col = ifelse(qannu_mk_wass > 0.05, "#FFFFFF00", "black"))
  par(xpd=F)


  par(mar = mar_2)

  alptempr::image_scale(as.matrix(qannu_in), col = cols_scale, breaks = brea_scale, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, y_lab_scal, 0), tck = -0.08, cex.axis = cex_y_axis)

  box()
  
}


#emd_intro----

# pdf("u:/RhineFlow/rhine_obs/manus/figures/Fig2.pdf", width = 8.3, height = 6.5)
pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_emd_intro.pdf", width = 8.3, height = 7.0)

layout(matrix(c(1,2,3,
                1,2,4), 
              3, 2), heights = c(1, 1, 1.3))

line_lwd <- 0.7

par(family = "serif")
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





#ltc_disc----

# tiff("u:/RhineFlow/rhine_obs/manus/figures/Fig4.tif", width = 16.6, height = 8, units = 'in', res = 800)
pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_ltc_disc.pdf", width = 16.6, height = 8.0)


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

#Plot type 1: Seasonality of river runoff

plot_quan_doy(qvalu_wass)

plot_quan_doy(qvalu_base)

plot_quan_doy(qvalu_koel)

plot_quan_doy(qvalu_wuer)


#Plot type 2: Changes in seasonality of river runoff

plot_mov_quan(qvslo_wass, n_iso = 6)

plot_mov_quan(qvslo_base, n_iso = 6)

plot_mov_quan(qvslo_koel, n_iso = 6)

plot_mov_quan(qvslo_wuer, n_iso = 6)


#Plot type 3: Onset and evolution of changes

plot_emd_val(emd_disc_wass, n_iso = 8)

plot_emd_val(emd_disc_base, n_iso = 8)

plot_emd_val(emd_disc_koel, n_iso = 8)

plot_emd_val(emd_disc_wuer, n_iso = 8)


#Plot type 3: Onset and evolution of changes

plot_emd_quan(qannu_wass, n_iso = 12)

plot_emd_quan(qannu_base, n_iso = 12)

plot_emd_quan(qannu_koel, n_iso = 12)

plot_emd_quan(qannu_wuer, n_iso = 12)


#Station names

par(mar = c(0,0,0,0))

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Wasserburg (a)", side = 2, line = -1.8, cex = cex_header, adj = 1.00)
mtext("Basel (b)",      side = 2, line = -1.8, cex = cex_header, adj = 0.66)
mtext("Cologne (c)",      side = 2, line = -1.8, cex = cex_header, adj = 0.37)
mtext("Wuerzburg (d)",  side = 2, line = -1.8, cex = cex_header, adj = 0.04, outer = T)

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


#ltc_tp----

# tiff("u:/RhineFlow/rhine_obs/manus/figures/Fig5.tif", width = 16.6, height = 6.14, units = 'in', res = 800)
pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_ltc_tp.pdf", width = 16.6, height = 6.14)

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


#Plot: Emd values temperature

plot_emd_val(emd_temp_bern, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_base, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_zuer, rev_cols = T, n_iso = 8)


#Plot: EMD annual quantiles temperature

plot_emd_quan(qannu_temp_bern, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_base, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_zuer, rev_cols = T, n_iso = 8)


#Plot: Emd values precipitation

plot_emd_val(emd_rain_bern, n_iso = 6)

plot_emd_val(emd_rain_base, n_iso = 6)

plot_emd_val(emd_rain_zuer, n_iso = 6)


#Plot: EMD annual quantiles precipitation

plot_emd_quan(qannu_rain_bern, n_iso = 8)

plot_emd_quan(qannu_rain_base, n_iso = 8)

plot_emd_quan(qannu_rain_zuer, n_iso = 8)


#Station names

par(mar = c(0,0,0,0))

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


#ltc_disc_seas----

pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_ltc_disc_seas.pdf", width = 16.6, height = 8.0)

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

#Quantile change spring
plot_emd_quan(qannu_wass_spr, n_iso = 8)

plot_emd_quan(qannu_base_spr, n_iso = 8)

plot_emd_quan(qannu_koel_spr, n_iso = 8)

plot_emd_quan(qannu_wuer_spr, n_iso = 8)


#Quantile change summer
plot_emd_quan(qannu_wass_sum, n_iso = 8)

plot_emd_quan(qannu_base_sum, n_iso = 8)

plot_emd_quan(qannu_koel_sum, n_iso = 8)

plot_emd_quan(qannu_wuer_sum, n_iso = 8)


#Quantile change autumn
plot_emd_quan(qannu_wass_aut, n_iso = 8)

plot_emd_quan(qannu_base_aut, n_iso = 8)

plot_emd_quan(qannu_koel_aut, n_iso = 8)

plot_emd_quan(qannu_wuer_aut, n_iso = 8)


#Quantile change winter
plot_emd_quan(qannu_wass_win, n_iso = 8)

plot_emd_quan(qannu_base_win, n_iso = 8)

plot_emd_quan(qannu_koel_win, n_iso = 8)

plot_emd_quan(qannu_wuer_win, n_iso = 8)


#Station names

par(mar = c(0,0,0,0))

plot(1:100, 1:100, axes = F, type = "n", xlab = "", ylab = "")
mtext("Wasserburg (a)", side = 2, line = -1.8, cex = cex_header, adj = 1.00)
mtext("Basel (b)",      side = 2, line = -1.8, cex = cex_header, adj = 0.66)
mtext("Cologne (c)",      side = 2, line = -1.8, cex = cex_header, adj = 0.37)
mtext("Wuerzburg (d)",  side = 2, line = -1.8, cex = cex_header, adj = 0.04, outer = T)

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



#ltc_prec_seas----

pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_ltc_prec_seas.pdf", width = 16.6, height = 6.14)

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


#Quantile change spring
plot_emd_quan(qannu_rain_bern_spr, n_iso = 8)

plot_emd_quan(qannu_rain_base_spr, n_iso = 8)

plot_emd_quan(qannu_rain_zuer_spr, n_iso = 8)


#Quantile change summer
plot_emd_quan(qannu_rain_bern_sum, n_iso = 8)

plot_emd_quan(qannu_rain_base_sum, n_iso = 8)

plot_emd_quan(qannu_rain_zuer_sum, n_iso = 8)


#Quantile change autumn
plot_emd_quan(qannu_rain_bern_aut, n_iso = 8)

plot_emd_quan(qannu_rain_base_aut, n_iso = 8)

plot_emd_quan(qannu_rain_zuer_aut, n_iso = 8)


#Quantile change winter
plot_emd_quan(qannu_rain_bern_win, n_iso = 8)

plot_emd_quan(qannu_rain_base_win, n_iso = 8)

plot_emd_quan(qannu_rain_zuer_win, n_iso = 8)


#Station names

par(mar = c(0,0,0,0))

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


#ltc_tp_add----

pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_ltc_tp_add.pdf", width = 16.6, height = 12.28)

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

#Plot: Emd values temperature

plot_emd_val(emd_temp_sion, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_same, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_neuc, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_luga, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_gene, rev_cols = T, n_iso = 8)

plot_emd_val(emd_temp_chau, rev_cols = T, n_iso = 8)


#Plot: EMD annual quantiles temperature

plot_emd_quan(qannu_temp_sion, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_same, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_neuc, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_luga, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_gene, rev_cols = T, n_iso = 8)

plot_emd_quan(qannu_temp_chau, rev_cols = T, n_iso = 8)


#Plot: Emd values precipitation

plot_emd_val(emd_rain_sion, n_iso = 6)

plot_emd_val(emd_rain_same, n_iso = 6)

plot_emd_val(emd_rain_neuc, n_iso = 6)

plot_emd_val(emd_rain_luga, n_iso = 6)

plot_emd_val(emd_rain_gene, n_iso = 6)

plot_emd_val(emd_rain_chau, n_iso = 6)


#Plot: EMD annual quantiles precipitation

plot_emd_quan(qannu_rain_sion, n_iso = 8)

plot_emd_quan(qannu_rain_same, n_iso = 8)

plot_emd_quan(qannu_rain_neuc, n_iso = 8)

plot_emd_quan(qannu_rain_luga, n_iso = 8)

plot_emd_quan(qannu_rain_gene, n_iso = 8)

plot_emd_quan(qannu_rain_chau, n_iso = 8)


#Station names

par(mar = c(0,0,0,0))

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





#reservoirs----

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


pdf("U:/RhineFlow/rhine_obs/R/figs_manus/reservoirs.pdf", width = 8.3, height = 3.0)

my_blu_2 <- rgb(44, 114, 142, max = 255, alpha = 50)

par(family = "serif")
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
mtext("Cum. storage volume [hm³]", side = 2, line = 1.5, cex = 1.2, adj = 0.5)
mtext(paste(perc_vol[2], "%"), side = 1, line = -2.8, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[3], "%"), side = 1, line = -5.4, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[4], "%"), side = 1, line = -7.8, cex = 1.2, adj = 0.07)
mtext(paste(perc_vol[5], "%"), side = 1, line = -11.5, cex = 1.2, adj = 0.07)
box(lwd = 1)

dev.off()


#rast_hydro----

#Raster hydrograph Wasserburg
pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_rast_hydro.pdf", width = 8.3, height = 4.5)

par(family = "serif")

grdc_data <- read_grdc(paste0(dir_grdc, "6343100_Q_Day.Cmd.txt"))

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
# cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(100)
# cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(100)
cols_max <- grDevices::colorRampPalette(c("white", "cadetblue3", viridis::viridis(9, direction = 1)[c(4:1, 1)]))(100)
cols_min <- grDevices::colorRampPalette(c("red4","orangered4", "orange2","gold2", "yellow2", "white"))(100)
cols_hydro <- c(cols_min, cols_max)

max_break <- max_na(data_day)
min_break <- min_na(data_day)
qua_break <- quantile(data_day, probs = 0.6, type = 8, na.rm = T)

breaks_1 <- lseq(min_break, qua_break, length.out = length(cols_hydro)/2)
breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_hydro)/2 + 1)
breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1

breaks_hydro <- c(breaks_1, breaks_2)


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




#elevs_parde----

#Elevation distribution and Parde-Coefficients

#Elevation boxplots individual basins
# load("/home/erwin/ownCloud/ele_parde.RData")
base_dir <- "U:/RhineFlow/rhine_obs/data/map/"

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

pdf("U:/RhineFlow/rhine_obs/R/figs_manus/fig_elev_parde.pdf", width = 8.3, height = 5.5)

x_axis_lab <- 1:12
x_axis_tic <- (1:13)-0.5
col_wass <- viridis::viridis(9, direction = 1)[2] 
col_base <- viridis::viridis(9, direction = 1)[4]
col_koel <- "orange2"
col_wuer <- "orangered3"

par(family = "serif")
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
mtext("Pardé-coefficient", side = 2, line = 1.3, cex = 1.2, adj = 0.5)
mtext("b) Pardé-coefficients", side = 3, line = 0.3, cex = 1.6, adj = 0.0)
box()

dev.off()


#table----

grdc_data <- read_grdc(paste0(dir_grdc, "6335500_Q_Day.Cmd.txt"))

# 6343100_Q_Day.Cmd.txt Wasserburg (Inn)
# 6935051_Q_Day.Cmd.txt Basel (Rhine)
# 6335060_Q_Day.Cmd.txt Koeln (Rhine)
# 6335500_Q_Day.Cmd.txt Würzburg (Main)

grdc_data_day <- ord_day(grdc_data$value, grdc_data$date, start_y = 1869, end_y = 2016)

mea_na(grdc_data_day)


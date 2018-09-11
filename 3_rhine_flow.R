###

#Rhine flow observations - Analysis discharge data
#Erwin Rottler, University of Potsdam
#Summer 2018

###

load(paste0(base_dir, "data/bafu/dis_new.RData")) #load discharge data set
dis <- dis_new ; dis_new <- NULL

cols_sel <- sapply(gaug_sel, sel_dis) #columns with selected gauges
dis_sel <- dis[, cols_sel] #extract selected time series from data frame

col_basin <- sapply(basin_stn, sel_dis) #gauge of selected basin
dis_basin <- dis[, col_basin] #time series of basin gauge

#analy_prob----
if(do_prob){
  
#Quantile probability
for(i in 1:ncol(dis_sel)) {
  
  print(paste(colnames(dis_sel[i]), Sys.time()))

  f_qprob <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                         date = dis$date,
                                         start_year = start_year,
                                         end_year = end_year,
                                         quant_in = quant_sel,
                                         window_width = window_width,
                                         method_analys = "quantile_prob",
                                         method_quant = "gev"
  )}

  qprob_sing <- foreach(k = quants, .combine = 'cbind') %dopar%{
    f_qprob(k)
  }
    
  if(i == 1){
    qprob <- qprob_sing
  }else{
    qprob <- cbind(qprob, qprob_sing)
  }
}

#Trend moving quantile probability
for(i in 1:ncol(dis_sel)) {
  
  print(paste(colnames(dis_sel[i]), Sys.time()))
  
  f_qmove <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                         date = dis$date,
                                         start_year = start_year,
                                         end_year = end_year,
                                         quant_in = quant_sel,
                                         window_width = window_width,
                                         method_analys = "mov_quant_prob_trend",
                                         method_quant = "gev"
  )}
  
  qmove_sing <- foreach(k = quants, .combine = 'cbind') %dopar%{
    f_qmove(k)
  }
  
  
  if(i == 1){
    qmove <- qmove_sing
  }else{
    qmove <- cbind(qmove, qmove_sing)
  }
}

}
#visua_prob----  
if(do_prob){
  
pdf(paste0("u:/RhineFlow/rhine_obs/figs/flow_prob", ".pdf"), width = 5, height = 4)

par(oma=c(0,0,0,0))
par(family="serif")

for(i in 1:ncol(dis_sel)){

z <- 1:99 + (i-1)*99

layout(matrix(c(rep(c(1,3), 7),
                2,4),
                2, 8), widths=c(), heights=c(1,1))

# cols_1 <- colorRampPalette(c("darkblue", "blue", "deepskyblue", "grey90", "yellow2","orange", "red3"))(200)
cols_1 <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90", 
                             "yellow2","gold2", "orange2", "orangered3", "orangered4"))(200)

image_flow(data_in   = qprob[, z],
           colors    = cols_1,
           breaks    = seq(0, 100, length.out=201),
           main      = paste0(colnames(dis_sel)[i], ": Quantile probab. [%]"),
           ylab      = "Quantile",
           margins_1 = c(1.6,2.5,1.6,0),
           margins_2 = c(1.6,0.5,1.6,1.7)
)

n_max <- round(abs(max_na(qmove[, z])) / (max_na(qmove[, z]) + abs(min_na(qmove[, z]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c("darkblue", "blue", "deepskyblue", "grey90"))(n_min)
cols_max <- colorRampPalette(c("grey90", "yellow", "orange", "red3"))(n_max)
cols_2 <- c(cols_min, cols_max)

n_max <- round(abs(max_na(qmove[, z])) / (max_na(qmove[, z]) + abs(min_na(qmove[, z]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
cols_max <- colorRampPalette(c("grey90", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
cols_2 <- c(cols_min, cols_max)

image_flow(data_in <- qmove[, z],
           colors <- cols_2,
           breaks = seq(min_na(qmove[, z]), max_na(qmove[, z]), length.out = length(cols_2) +1),
           main <- paste0(colnames(dis_sel)[i], ": Trend moving prob. [%/dec]"),
           ylab <- "Quantile",
           margins_1 = c(1.6,2.5,1.6,0),
           margins_2 = c(1.6,0.5,1.6,1.7)
)


}

dev.off()

}
#analy_extr----
if(do_extr){
  
  #Quantile probability
  Sys.time()
  f_qext <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                        date = dis$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        quant_in = quant_sel,
                                        window_width = window_width,
                                        method_analys = "quantile_prob",
                                        method_quant = "gev"
  )}
  qpext <- foreach(i = 1:ncol(dis_sel), .combine = 'cbind') %dopar%{
    qpext <- mapply(f_qext, quants_ext, SIMPLIFY = T)
  }
  Sys.time()
  
  
  #Trend moving quantile probability
  Sys.time()
  f_mext <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                        date = dis$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        quant_in = quant_sel,
                                        window_width = window_width,
                                        method_analys = "mov_quant_prob_trend",
                                        method_quant = "gev"
  )}
  qmext <- foreach(i = 1:ncol(dis_sel), .combine = 'cbind') %dopar%{
    qmext <- mapply(f_mext, quants_ext, SIMPLIFY = T)
  }
  Sys.time()
  
}
#visua_extr----
if(do_extr){
  
  pdf(paste0("u:/RhineFlow/rhine_obs/figs/flow_extr", ".pdf"), width = 5.09, height = 3.8)
  
  par(oma=c(0,0,0,0))
  par(family="serif")
  par(mfrow = c(2,1))
  par(mar = c(1.4, 2.0, 1.5, 0.2))
  
  cols_ext <- c("grey25", viridis(9, direction = 1)[4], "gold2", "orangered4")
  
  for(i in 1:ncol(dis_sel)){
    z <- 1:length(quants_ext) + (i-1)*length(quants_ext)
    
    x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15
    
    plot(qpext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "")
    for (k in 1:length(z)){
      lines(smoothFFT(qpext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
    }
    abline(h = 0, lty = "dashed", lwd = 0.9)
    abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
    axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
    axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
         col="black", col.axis="black", tck=-0.04)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
    mtext(paste0(colnames(dis_sel)[i], ": Quantile probability "), side = 3, line = 0.2, cex = 1)
    mtext("Probab. [%]", side = 2, line = 1.5, padj = 1, cex = 0.8)
    legend(290, max_na(qpext[,z]), quants_ext, pch = 19, col = cols_ext, cex = 0.6, bg = "white")
    box()
    
    plot(qmext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "", 
         ylim = c(min_na(qmext[, z]), max_na(qmext[, z])))
    for (k in 1:length(z)){
      lines(smoothFFT(qmext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
    }
    abline(h = 0, lty = "dashed", lwd = 0.9)
    abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
    axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
    axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
         col="black", col.axis="black", tck=-0.04)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
    mtext(paste0(colnames(dis_sel)[i], ": Quantile prob. trend "), side = 3, line = 0.2, cex = 1)
    mtext("Probab. [%/dec]", side = 2, line = 1.5, padj = 1, cex = 0.8)
    legend(290, max_na(qmext[,z]), quants_ext, pch = 19, col = cols_ext, cex = 0.6, bg = "white")
    box()
    
  }
  
  dev.off()
  
}

if(FALSE){
  
  gaug_sel <- c("Diepoldsau_2", "Cochem")
  cols_sel <- sapply(gaug_sel, sel_dis) #columns with selected gauges
  dis_sel <- dis[, cols_sel] #extract selected time series from data frame
  
  #Quantile probability
  Sys.time()
  f_qext <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                        date = dis$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        quant_in = quant_sel,
                                        window_width = window_width,
                                        method_analys = "quantile_prob",
                                        method_quant = "gev"
  )}
  qpext <- foreach(i = 1:ncol(dis_sel), .combine = 'cbind') %dopar%{
    qpext <- mapply(f_qext, quants_ext, SIMPLIFY = T)
  }
  Sys.time()
  
  
  #Trend moving quantile probability
  Sys.time()
  f_mext <- function(quant_sel){dis_ana(disc = dis_sel[, i],
                                        date = dis$date,
                                        start_year = start_year,
                                        end_year = end_year,
                                        quant_in = quant_sel,
                                        window_width = window_width,
                                        method_analys = "mov_quant_prob_trend",
                                        method_quant = "gev"
  )}
  qmext <- foreach(i = 1:ncol(dis_sel), .combine = 'cbind') %dopar%{
    qmext <- mapply(f_mext, quants_ext, SIMPLIFY = T)
  }
  Sys.time()
  
  
  pdf(paste0("u:/RhineFlow/rhine_obs/figs/flow_extr_publ", ".pdf"), width = 5.09, height = 5)
  
  par(oma=c(0,0,0,0))
  par(family="serif")
  par(mfrow = c(4,1))
  par(mar = c(1.4, 2.2, 1.5, 0.2))
  
  cols_ext <- c("grey25", viridis(9, direction = 1)[4], "gold2", "orangered4")
  lengend_vals <- c("0.850", "0.900", "0.925", "0.950")
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15
  
  #Plot 1: Diepoldsau Quantile probability
  i <-1
  z <- 1:length(quants_ext) + (i-1)*length(quants_ext)
  
  plot(qpext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "",
       ylim = c(0, 85))
  for (k in 1:length(z)){
    lines(smoothFFT(qpext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
  }
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.2, 0))
  mtext(paste0("a) ", "Alpine Rhine", ": Quantile probability "), side = 3, line = 0.2, cex = 0.8)
  mtext("Probabil. [%]", side = 2, line = 1.7, padj = 1, cex = 0.7)
  legend("topright", lengend_vals, pch = 19, col = cols_ext, cex = 0.7, 
         bg = "white", box.col = "black", ncol = 4)
  box()
  
  #Plot 2: Diepoldsau: Quantile probability trend
  i <-1
  z <- 1:length(quants_ext) + (i-1)*length(quants_ext)
  
  plot(qmext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "", 
       ylim = c(-6.2, 3.8))
  for (k in 1:length(z)){
    lines(smoothFFT(qmext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
  }
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.2, 0))
  mtext(paste0("b) ", "Alpine Rhine", ": Quantile prob. trend "), side = 3, line = 0.2, cex = 0.8)
  mtext("Probabil. [%/dec]", side = 2, line = 1.7, padj = 1, cex = 0.7)
  legend("topright", lengend_vals, pch = 19, col = cols_ext, cex = 0.7, 
         bg = "white", box.col = "black", ncol = 4)
  box()
  
  #Plot 3: Cochem: Quantile probability trend
  i <-2
  z <- 1:length(quants_ext) + (i-1)*length(quants_ext)
  
  plot(qmext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "", 
       ylim = c(-0.5, 6))
  for (k in 1:length(z)){
    lines(smoothFFT(qmext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
  }
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.2, 0))
  mtext(paste0("c) ", "Moselle", ": Quantile prob. trend "), side = 3, line = 0.2, cex = 0.8)
  mtext("Probabil. [%/dec]", side = 2, line = 1.7, padj = 1, cex = 0.7)
  legend("topright", lengend_vals, pch = 19, col = cols_ext, cex = 0.7, 
         bg = "white", box.col = "black", ncol = 4)
  box()
  
  #Plot 4: Cochem Quantile probability
  i <-2
  z <- 1:length(quants_ext) + (i-1)*length(quants_ext)
  
  plot(qpext[, z[1]], type = "n", axes = F, main = "", ylab = "", xlab = "",
       ylim = c(0, 67))
  for (k in 1:length(z)){
    lines(smoothFFT(qpext[, z[k]], sd =3), col = cols_ext[k], lwd = 1.5)
  }
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.2, 0))
  mtext(paste0("d) ", "Moselle", ": Quantile probability "), side = 3, line = 0.2, cex = 0.8)
  mtext("Probabil. [%]", side = 2, line = 1.7, padj = 1, cex = 0.7)
  legend("topright", lengend_vals, pch = 19, col = cols_ext, cex = 0.7, 
         bg = "white", box.col = "black", ncol = 4)
  box()
    
  
  dev.off()
}
#analy_disco----
if(do_disco){
#Daily average (median) discharge
qmedi <- f_qmedi(dis_basin)
qmedi_mea <- mea_na(qmedi)

#Runoff coefficient moving window
qcoef_med <- f_disco(rain_in = prec_basin,
                     disc_in = dis_basin,
                     rain_date = meteo_date,
                     disc_date = dis$date,
                     start_year = start_year,
                     end_year = end_year,
                     window_width = window_width,
                     method_analy = "median")

qcoef_med_mea <- mea_na(qcoef_med)

qcoef_slo <- f_disco(rain_in = prec_basin,
                     disc_in = dis_basin,
                     rain_date = meteo_date,
                     disc_date = dis$date,
                     start_year = start_year,
                     end_year = end_year,
                     window_width = window_width,
                     method_analy = "slope")

qcoef_slo_mea <- mea_na(qcoef_slo)

}
#visua_disco----
if(do_disco){
  
  pdf(paste0("u:/RhineFlow/rhine_obs/figs/flow_", basin_sel, ".pdf"), width = 8, height = 7)
  
  par(mfrow = c(1, 2))
  par(mar = c(8, 2, 8, 1))
  
  #Plot 1: Basin outlines + DEM
  plot(dem_84, main = "", axes=F, col = viridis(100, direction = 1), legend = F)
  r_range <- c(minValue(dem_84), maxValue(dem_84))
  plot(dem_84, legend.only=TRUE, col=viridis(100, direction = 1),
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=seq(r_range[1], r_range[2], 500) - 26,
                      labels=round(seq(r_range[1], r_range[2], 500) - 26, digits = 0), 
                      mgp = c(3, 0.2, 0), tck= -0.2, cex.axis = 0.7),
       legend.args=list(text='', side=4, font=2, line=2, cex=0.8))
  plot(basin_84, lwd = 0.7, col = rgb(205, 0, 0, max = 255, alpha = 100), add = T)
  lines(basin_84, lwd = 0.7, col = "red3")
  axis(1, mgp = c(3, 0.0, 0), tck=-0.02, cex.axis = 0.7)
  axis(2, mgp = c(3, 0.0, 0), tck=-0.02, cex.axis = 0.7)
  mtext(paste0(basin_sel), side = 3, line = 0.5, cex = 1)
  
  #Plot 2: Histogramm elevation distribution dem
  graphics::hist(dem_ele,  main ="", breaks = 15, col = "grey50",
                 # yaxs =  "i", xaxs = "i",
                 xlab = "", ylab = "", axes=F)
  axis(1, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  mtext(paste0("Frequency"), side = 2, line = 1.5, padj = 1, cex = 0.8)
  mtext(paste0("Elevation"), side = 1, line = 0.5, padj = 1, cex = 0.8)
  box()
  
  
  par(mfrow = c(3,1))
  par(mar = c(2.2, 8, 2, 6))
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15
  
  #Plot 3: Average yearly cycle of discharge 
  plot(qmedi, type = "l", axes = F, lwd = 0.7, col = "grey50",
       main = "", ylab = "", xlab = "")
  lines(smoothFFT(qmedi, sd = 10), col = "blue3", lwd = 1.5)
  abline(h = qmedi_mea, col = "red3")
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
  mtext(paste0(basin_stn, ": Average flow "), side = 3, line = 0.5, cex = 1)
  mtext("Discharge [m?/s]", side = 2, line = 2, padj = 1, cex = 0.8)
  mtext(round(qmedi_mea, digits = 2), side = 3, line = 0.5, adj = 0, col = "red3")
  box()
  
  #Plot 4: Average runoff coefficient 
  plot(qcoef_med, type = "l", axes = F, lwd = 0.7, col = "grey50",
       main = "", ylab = "", xlab = "")
  lines(smoothFFT(qcoef_med, sd = 10), col = "blue3", lwd = 1.5)
  abline(h = qcoef_med_mea, col = "red3")
  abline(h = 1, lty = "dashed", lwd = 0.9)
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
  mtext(round(qcoef_med_mea, digits = 2), side = 3, line = 0.5, adj = 0, col = "red3")
  mtext(paste0(basin_stn, ": Average runoff coefficient "), side = 3, line = 0.5, cex = 1)
  mtext("Runoff coeff. [-]", side = 2, line = 2, padj = 1, cex = 0.8)
  box()
  
  #Plot 5: Trend runoff coefficient 
  plot(qcoef_slo, type = "l", axes = F, lwd = 0.7, col = "grey50",
       main = "", ylab = "", xlab = "")
  lines(smoothFFT(qcoef_slo, sd = 10), col = "blue3", lwd = 1.5)
  abline(h = qcoef_slo_mea, col = "red3")
  abline(h = 0, lty = "dashed", lwd = 0.9)
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
  mtext(round(qcoef_slo_mea, digits = 4), side = 3, line = 0.5, adj = 0, col = "red3")
  mtext(paste0(basin_stn, ": Trend runoff coefficient "), side = 3, line = 0.5, cex = 1)
  mtext("Runoff coeff. [-/a]", side = 2, line = 2, padj = 1, cex = 0.8)
  box()
  
  dev.off()

}
#analy_regim----
if(do_regime){
  
  regi_sel <- c("Basel_Rheinhalle", "Worms", "Kaub", "Koeln")
  
  cols_reg <- sapply(regi_sel, sel_dis) #columns with selected gauges
  dis_reg <- dis[, cols_reg] #extract selected time series from data frame
  
  #Daily average (median) discharge
  qmedi_regi <- foreach(i = 1:ncol(dis_reg), .combine = 'cbind') %dopar%{
    f_qmedi(dis_reg[, i])
  }
  
}
#visua_regim----
if(do_regime){
  
  pdf(paste0("u:/RhineFlow/rhine_obs/figs/flow_regi", ".pdf"), width = 5.09, height = 2.2)
  
  #Positions ticks and labels for x-axis
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15
  
  legend_names <- c("Basel", "Worms", "Kaub", "Koeln")
  legend_rivers <- c("Neckar", "Main", "Nahe", "Lahn", "Moselle")
  
  par(oma=c(0,0,0,0))
  par(family="serif")
  par(mar = c(1.5, 2.5, 0.5, 0.5))
  layout(matrix(c(rep(1, 14),rep(2,2)),
                1, 16), widths=c(), heights=c())
  
  colors_regi <- c("grey25", viridis(9, direction = 1)[4], "gold2", "orangered4")
  # cols_1 <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90", 
  #                              "yellow2","gold2", "orange2", "orangered3", "orangered4"))(200)
  
  plot(qmedi_regi[, 1], type = "n", axes = F, ylim = c(min_na(qmedi_regi), max_na(qmedi_regi)),
       ylab = "", xlab = "", yaxs="i", xaxs="i")
  for(i in 1:ncol(qmedi_regi)){
    lines(smoothFFT(qmedi_regi[, i], sd = 3), col = colors_regi[i], lwd = 1.5)
  }
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.15, 0), tck=-0.015, cex.axis = 0.9)
  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.3, 0))
  mtext("Discharge [m³/s]", side = 2, line = 2, padj = 1, cex = 0.7)
  box()
  
  #Plot: Legend to the left
  par(mar = c(1.5, 0.0, 0.5, 0.5))
  
  x_points <- rep(7.5, ncol(dis_reg))
  y_points <- c(2, 22, 43, 75)
  x_rivers <- rep(9.2, length(legend_rivers))
  y_rivers <- c(12, 30, 35, 56, 61)
  col_rivers <- "black"
  plot(1:100, 1:100, type = "n", axes = F, ylab = "", xlab = "", xlim = c(5, 25))
  points(x_points, y_points, col = colors_regi, pch = 15, cex = 1.5)
  points(x_rivers, y_rivers, col =col_rivers, pch = "<", cex = 0.8)
  for(i in 1:ncol(dis_reg)){
    text(x_points[i]+3, y_points[i], legend_names[i], col = "black", adj = 0)
  }
  for(i in 1:length(legend_rivers)){
    text(x_rivers[i]+1.7, y_rivers[i], legend_rivers[i], col = col_rivers, adj = 0, cex = 0.7)
  }
  
  box()
  
  dev.off()

}









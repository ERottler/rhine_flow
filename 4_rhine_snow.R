###

#Rhine flow observations - Analysis snow data
#Erwin Rottler, University of Potsdam
#Summer 2018

###

#snow_stat_ana----

# idaweb_data_prep(param_code = "hto000d0", order_dir = paste0(base_dir,"data/idaweb/snow/"),
#                  out_dir = paste0(base_dir,"data/idaweb/"), start_day = "1864-01-01", end_day = "2017-12-31")
load(paste0(base_dir, "data/idaweb/hto000d0.RData")) ; data_snow <- out_data

swiz_meta <- read.table(paste0(base_dir,"data/idaweb/stationMeta.csv"), sep=",", header=T)

#Snow cover changes with elevation
# stn_sel <-  c("MAS","PAY","GRA","THU","BER","KOP","LUZ","SMA","CHU","RAG",
#               "ELM", "DAV", "ARO","WFJ")
stn_sel <-  c("CHU", "ELM", "DAV", "ARO", "WFJ")
#stn_sel <-  c("WFJ", "ARO", "DAV", "ELM", "BER", "SMA", "CHU")
col_sel <- sapply(stn_sel, stn_col)
data_snow_sel <- data_snow[, col_sel]


#Snow median average
ssmd <- foreach(i = 1:ncol(data_snow_sel), .combine = 'cbind') %dopar% {
  
  f_med_snow(data_snow_sel[, i])
  
}
colnames(ssmd) <- colnames(data_snow_sel)
ssmd_med <- apply(ssmd, 2, med_na)
ssmd_mea <- apply(ssmd, 2, mea_na)

#Snow mean average
ssme <- foreach(i = 1:ncol(data_snow_sel), .combine = 'cbind') %dopar% {
  
  f_mea_snow(data_snow_sel[, i])
  
}
colnames(ssme) <- colnames(data_snow_sel)
ssme_med <- apply(ssme, 2, med_na)
ssme_mea <- apply(ssme, 2, mea_na)

#Snow moving average trend
sssl <- foreach(i = 1:ncol(data_snow_sel), .combine = 'cbind') %dopar% {
  
  f_slo_snow(data_snow_sel[, i])
  
}
colnames(sssl) <- colnames(data_snow_sel)
sssl_med <- apply(sssl, 2, med_na)
sssl_mea <- apply(sssl, 2, mea_na)

#Snow probability
spro <- foreach(i = 1:ncol(data_snow_sel), .combine = 'cbind') %dopar% {
  
  f_pro_snow(data_snow_sel[, i])
  
}
colnames(spro) <- colnames(data_snow_sel)
spro_med <- apply(spro, 2, med_na)
spro_mea <- apply(spro, 2, mea_na)

#Snow window probability trend
spsl <- foreach(i = 1:ncol(data_snow_sel), .combine = 'cbind') %dopar% {
  f_psl_snow(data_snow_sel[, i])
}
colnames(spsl) <- colnames(data_snow_sel)
spsl_med <- apply(spsl, 2, med_na)
spsl_mea <- apply(spsl, 2, mea_na)


#snow_stat_vis----

pdf(paste0("u:/RhineFlow/rhine_obs/figs/snow_swiss", ".pdf"), width = 5.09, height = 5)

par(oma=c(0,0,0,0))
par(family="serif")
# par(mfrow = c(5,1), heights = c(1,1,0.2,1,1))
layout(matrix(c(1:5), 5, 1), widths=c(1, 1), heights=c(1,1,0.3,1,1))
mar_1 <- c(1.6, 1.5, 1.4, 0.5)
mar_2 <- c(0.5, 6, 0.5, 3.5)

# my_cols <- c("deepskyblue4","blue3","black","yellow3","orange3","red3")
# c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90", "yellow2","gold2", "orange2", "orangered3", "orangered4")
my_cols <- c(rep("grey25", 3),
             # rep(viridis(5, direction = 1)[1], 10),
             viridis(4, direction = 1)[1:3], "gold2")
my_cols <- c(rep("grey10", 3), viridis(20, direction = 1)[4], viridis(20, direction = 1)[9], "gold2", "orangered4")
#my_cols <- c("orangered4", "gold2", viridis(20, direction = 1)[9], viridis(20, direction = 1)[4], rep("grey10", 3))
#plot(1:7, 1:7, cex = 3, pch = 19, col = my_cols)


col2rgb("lightsteelblue2")
col_1 <- rgb(1, 1, 1, alpha = 255, maxColorValue = 255)
col_2 <- rgb(125, 125, 125, alpha = 255, maxColorValue = 255)
col_3 <- rgb(188, 210, 238, alpha = 255, maxColorValue = 255)
col_3 <- rgb(202, 225, 255, alpha = 255, maxColorValue = 255)
col_4 <- rgb(135, 206, 235, alpha = 255, maxColorValue = 255)
col_5 <- rgb( 74, 112, 139, alpha = 255, maxColorValue = 255)

my_cols <- c(col_1, col_2, col_3, col_4, col_5)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

#Plot 1: Snow mean average
par(mar = mar_1)
plot(smoothFFT(ssmd[, 1], sd = 4), type = "n", ylim = c(min_na(ssmd)-5, max_na(ssmd)+5),
     ylab = "", xlab = "", axes = F)

for(i in length(col_sel):1){
  #lines(smoothFFT(smed[, i], sd = 4), type = "l", lwd = 1.8, col = my_cols[i])  
  polygon(c(1, 1:365, 365), c(1, smoothFFT(ssmd[, i], sd = 4), 0), 
          col = my_cols[i], border = NA)
}
# lines(smoothFFT(spro_sel_mea, sd = 5), col = "red2", lwd =2)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
axis(2, mgp = c(3, 0.4, 0), tck=-0.04, cex.axis = 1)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.05)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.2, 0), cex = 1.2)
#mtext("Snow depth [cm]", side = 2, line = 2.2, padj = 1, cex = 0.8)
mtext("a) Snow depth [cm]", side = 3, line = 0.1, cex = 0.8)
box()

#Plot 2: Snow depth trend
par(mar = mar_1)
plot(smoothFFT(sssl[, 1], sd = 4), type = "n", ylim = c(min_na(sssl), max_na(sssl)),
     ylab = "", xlab = "", axes = F)
for(i in 1:length(col_sel)){
  lines(smoothFFT(sssl[, i], sd = 4), type = "l", lwd = 1.8, col = my_cols[i])  
}
# lines(smoothFFT(spro_sel_mea, sd = 5), col = "red2", lwd =2)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
axis(2, mgp = c(3, 0.4, 0), tck=-0.04, cex.axis = 1)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.05)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.2, 0), cex = 1.2)
#mtext("Snow depth [cm/a]", side = 2, line = 2.2, padj = 1, cex = 0.8)
mtext("d) Snow depth trend [cm/dec]", side = 3, line = 0.1, cex = 0.8)
box()


#Legend-Plot
par(mar = mar_2)
plot(1:365, 1:365, type = "n", axes = F, ylab = "", xlab = "", xlim = c(45,345))
x_points <- c(70, 120, 170, 220, 270)+35
y_points <- rep(180, 5)
cols_legend <- c(col_1, col_2, col_3, col_4, col_5)
points(x_points, y_points, col = cols_legend, pch = 19, cex = 0.8)
text(x_points+5, y_points+2, c("556 m", "958 m", "1594 m", "1878 m", "2691 m"), 
     col = cols_legend, adj = 0, cex = 1)
text(70, y_points+2, "Elevation:", col = "black", cex = 1)
box(lwd = 0.5)

#Plot 3: Snow probability
par(mar = mar_1)
plot(smoothFFT(spro[, 1], sd = 4), type = "n", ylim = c(-5, 105),
     ylab = "", xlab = "", axes = F)
# for(i in length(col_sel):1){
#   lines(smoothFFT(spro[, i], sd = 4), type = "l", lwd = 1.8, col = my_cols[i])  
# }
for(i in length(col_sel):1){
  # lines(smoothFFT(spro[, i], sd = 4), type = "l", lwd = 1.8, col = my_cols[i])  
  polygon(c(1, 1:365, 365), c(1, smoothFFT(spro[, i], sd = 4), 0), 
          col = my_cols[i], border = NA)
}
# lines(smoothFFT(spro_sel_mea, sd = 5), col = "red2", lwd =2)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
axis(2, mgp = c(3, 0.4, 0), tck=-0.04, cex.axis = 1)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.05)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.2, 0), cex = 1.2)
#mtext("Snow probab. [%]", side = 2, line = 2.2, padj = 1, cex = 0.8)
mtext("c) Snow probability [%]", side = 3, line = 0.1, cex = 0.8)
box()


#Plot 4: Trend snow window probability
par(mar = mar_1)
plot(smoothFFT(spsl[, 1], sd = 4), type = "n", ylim = c(min_na(spsl), max_na(spsl)),
     ylab = "", xlab = "", axes = F)
for(i in 1:length(col_sel)){
  lines(smoothFFT(spsl[, i], sd = 4), type = "l", lwd = 1.8, col = my_cols[i])  
}
# lines(smoothFFT(spro_sel_mea, sd = 5), col = "red2", lwd =2)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
axis(2, mgp = c(3, 0.4, 0), tck=-0.04, cex.axis = 1)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.05)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.2, 0), cex = 1.2)
#mtext("Window prob. [%/a]", side = 2, line = 2.2, padj = 1, cex = 0.8)
mtext("d) Snow window prob. [%/dec]", side = 3, line = 0.1, cex = 0.8)
box()


dev.off()












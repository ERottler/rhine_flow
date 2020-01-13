###

#Rhine flow observations: Weather type classifications
#Erwin Rottler, University of Potsdam, 2019

###

start_year <- 1958
end_year <- 2017

start_day <- "1958-01-01"
end_day   <- "2017-12-31"

wtc_sel <- "gwt26_msl" #gwt26_msl, gwt26_500, cap27_msl
cli_sel <- "swiss_mix" # BAS, BER, SMA, Hohenpeissenberg, swiss_mix

my_wtc_sel <- 1:26

#sel_varies----

if(wtc_sel == "gwt26_msl"){
  
  #Weather type classification data: GWT26 based on mean sea level pressure
  wtc_data <- read.table(paste0(base_dir, "data/idaweb/order64866/order_64866_data.txt"),
                           sep = ";", skip = 1, header = TRUE, na.strings = "-")
  wtc_data$date <- as.POSIXct(strptime(wtc_data$time, "%Y%m%d", tz="UTC"))
  wtc_data$valu <- wtc_data$wkwtp3d0
  
}

if(wtc_sel == "gwt26_500"){
  
  #Weather type classification data: GWT26 based on mean sea level pressure
  wtc_data <- read.table(paste0(base_dir, "data/idaweb/order64970/order_64970_data.txt"),
                         sep = ";", skip = 1, header = TRUE, na.strings = "-")
  wtc_data$date <- as.POSIXct(strptime(wtc_data$time, "%Y%m%d", tz="UTC"))
  wtc_data$valu <- wtc_data$wkwtg3d0
  
}

if(wtc_sel == "cap27_msl"){
  
  #Weather type classification data: GWT26 based on mean sea level pressure
  wtc_data <- read.table(paste0(base_dir, "data/idaweb/order64973/order_64973_data.txt"),
                         sep = ";", skip = 1, header = TRUE, na.strings = "-")
  wtc_data$date <- as.POSIXct(strptime(wtc_data$time, "%Y%m%d", tz="UTC"))
  wtc_data$valu <- wtc_data$wkcap3d0
  
}

if(cli_sel == "BAS"){
  
  # Basel / Binningen
  data_cli <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli$date <- as.POSIXct(strptime(data_cli$time, "%Y%m%d", tz="UTC"))
  data_cli$valu <- data_cli$rhs150d0
  
}

if(cli_sel == "BER"){
  
  # Bern
  data_cli <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli$date <- as.POSIXct(strptime(data_cli$time, "%Y%m%d", tz="UTC"))
  data_cli$valu <- data_cli$rhs150d0
  
}

if(cli_sel == "SMA"){
  
  # Zuerich
  data_cli <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli$date <- as.POSIXct(strptime(data_cli$time, "%Y%m%d", tz="UTC"))
  data_cli$valu <- data_cli$rhs150d0
  
}

if(cli_sel == "Hohenpeissenberg"){
  
  # Hohenpeissenberg
  data_cli <- read.table(paste0(base_dir, "data/dwd_data/cdc_download_2018-12-05_18_09/RS_MN006.txt"), 
                         sep = ";", skip = 0, header = T, na.strings = c("-"))
  data_cli$date <- as.POSIXct(strptime(data_cli$ZEITSTEMPEL, "%Y%m%d", tz="UTC"))
  data_cli$valu <- data_cli$WERT
  
}

if(cli_sel == "swiss_mix"){
  
  # Basel / Binningen
  data_cli_bas <- read.table(paste0(base_dir, "data/idaweb/order64388/order_64388_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli_bas$date <- as.POSIXct(strptime(data_cli_bas$time, "%Y%m%d", tz="UTC"))
  data_cli_bas$valu <- data_cli_bas$rhs150d0
  
  # Bern
  data_cli_ber <- read.table(paste0(base_dir, "data/idaweb/order64387/order_64387_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli_ber$date <- as.POSIXct(strptime(data_cli_ber$time, "%Y%m%d", tz="UTC"))
  data_cli_ber$valu <- data_cli_ber$rhs150d0
  
  # Zuerich
  data_cli_sma <- read.table(paste0(base_dir, "data/idaweb/order64389/order_64389_data.txt"), 
                         sep = ";", skip = 2, header = T, na.strings = c("-"))
  data_cli_sma$date <- as.POSIXct(strptime(data_cli_sma$time, "%Y%m%d", tz="UTC"))
  data_cli_sma$valu <- data_cli_sma$rhs150d0
  
  #Swiss mix (mean precipitation from three swiss stations)
  data_cli <- data_cli_bas
  rain_swiss <- cbind(data_cli_bas$valu, data_cli_ber$valu, data_cli_sma$valu)
  data_cli$valu <- apply(rain_swiss, 1, mea_na)
  
}

#calc_wtc_cli----

wtc_sum_mea <- f_wtc_cli(wtc_data_in = wtc_data,
                         clim_data_in = data_cli,
                         annu_analy = "sum",
                         wtc_sel = my_wtc_sel,
                         method_analy = "mean")

wtc_sum_slo <- f_wtc_cli(wtc_data_in = wtc_data,
                         clim_data_in = data_cli,
                         annu_analy = "sum",
                         wtc_sel = my_wtc_sel,
                         method_analy = "sens_slope")

wtc_mea_mea <- f_wtc_cli(wtc_data_in = wtc_data,
                         clim_data_in = data_cli,
                         annu_analy = "mean",
                         wtc_sel = my_wtc_sel,
                         method_analy = "mean")

wtc_mea_slo <- f_wtc_cli(wtc_data_in = wtc_data,
                         clim_data_in = data_cli,
                         annu_analy = "mean",
                         wtc_sel = my_wtc_sel,
                         method_analy = "sens_slope")

if(cli_sel == "BAS"){
  
  wtc_sum_mea_bas <- wtc_sum_mea
  wtc_sum_slo_bas <- wtc_sum_slo
  wtc_mea_mea_bas <- wtc_mea_mea
  wtc_mea_slo_bas <- wtc_mea_slo
  
}

if(cli_sel == "BER"){
  
  wtc_sum_mea_ber <- wtc_sum_mea
  wtc_sum_slo_ber <- wtc_sum_slo
  wtc_mea_mea_ber <- wtc_mea_mea
  wtc_mea_slo_ber <- wtc_mea_slo
  
}

if(cli_sel == "SMA"){
  
  wtc_sum_mea_sma <- wtc_sum_mea
  wtc_sum_slo_sma <- wtc_sum_slo
  wtc_mea_mea_sma <- wtc_mea_mea
  wtc_mea_slo_sma <- wtc_mea_slo
  
}

if(cli_sel == "Hohenpeissenberg"){
  
  wtc_sum_mea_hoh <- wtc_sum_mea
  wtc_sum_slo_hoh <- wtc_sum_slo
  wtc_mea_mea_hoh <- wtc_mea_mea
  wtc_mea_slo_hoh <- wtc_mea_slo
  
}

#calc_wtc----

wtc_fre_mea <- f_wtc_fre(wtc_data_in = wtc_data, 
                         method_analy = "mean")

wtc_fre_slo <- f_wtc_fre(wtc_data_in = wtc_data, 
                         method_analy = "sens_slope")

#visu_with_hoh----

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













#visu_no_hoh----

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

my_ylim <- c(0, max_na(c(wtc_mea_mea_bas, wtc_mea_mea_ber, wtc_mea_mea_sma)) + 1.2)
my_xlim <- c(-0.5,(3 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_mea_mea_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_mea_mea_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_mea_mea_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
# par(new = T)
# plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_mea_hoh, type = "h", col = "black", lwd = 3, lend = 2,
#      xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 3 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 3) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 3 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("c) Precipitation per occurence [mm/day]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

mtext("Basel",     side = 3, line = -3.0 + 2.5, adj = 0.005, padj = 1, cex = 0.6)
mtext("Bern",      side = 3, line = -3.6 + 2.5, adj = 0.016, padj = 1, cex = 0.6)
mtext("Zuerich",   side = 3, line = -4.2 + 2.5, adj = 0.022,  padj = 1, cex = 0.6)
# mtext("Hohenp.",   side = 3, line = -4.8 + 2.5, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(9.9, 11.25), type = "l", lwd = 0.5)
lines(c(2,2), c(9.9, 10.80), type = "l", lwd = 0.5)
lines(c(3,3), c(9.9, 10.35), type = "l", lwd = 0.5)
# lines(c(4,4), c(9.9, 10.00), type = "l", lwd = 0.5)


# Plot: d: Trend mean precipiation per day

my_ylim <- c(min_na(c(wtc_mea_slo_bas, wtc_mea_slo_ber, wtc_mea_slo_sma)) - 0.03, 
             max_na(c(wtc_mea_slo_bas, wtc_mea_slo_ber, wtc_mea_slo_sma)) + 0.03)
my_xlim <- c(-0.5,(3 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_mea_slo_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_mea_slo_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_mea_slo_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
# par(new = T)
# plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_slo_hoh, type = "h", col = "black", lwd = 3, lend = 2,
#      xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 3 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 3) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 3 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("d) Trend precipiation per occur. [(mm/day)/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)


# Plot e: Mean annual sum

par(mar = mar_2)

my_ylim <- c(0, max_na(c(wtc_sum_mea_bas, wtc_sum_mea_ber, wtc_sum_mea_sma)) + 5)
my_xlim <- c(-0.5,(3 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_sum_mea_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_sum_mea_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_sum_mea_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)

axis(1, at = c(-0.5, ((1:26) * 3 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = (x_lab_posi * 3) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 3 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("e) Annual total precipiation [mm/year]", side = 3, line = line_main, cex = size_main, adj = 0)
mtext("GWT26 weather type", side = 1, line = 2, cex = 1.2, adj = 0.5)
box(lwd = 1.2)

mtext("Basel",     side = 3, line = -3.0 - 0.8, adj = 0.005, padj = 1, cex = 0.6)
mtext("Bern",      side = 3, line = -3.6 - 0.8, adj = 0.016, padj = 1, cex = 0.6)
mtext("Zuerich",   side = 3, line = -4.2 - 0.8, adj = 0.022, padj = 1, cex = 0.6)
# mtext("Hohenp.",   side = 3, line = -4.8 - 0.8, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(75+0, 129-20), type = "l", lwd = 0.5)
lines(c(2,2), c(75+0, 122-20), type = "l", lwd = 0.5)
lines(c(3,3), c(75+0, 116-20), type = "l", lwd = 0.5)
# lines(c(4,4), c(75+0, 110-20), type = "l", lwd = 0.5)

# Plot f: Trend annual sum

my_ylim <- c(min_na(c(wtc_sum_slo_bas, wtc_sum_slo_ber, wtc_sum_slo_sma)) - 0.5,
             max_na(c(wtc_sum_slo_bas, wtc_sum_slo_ber, wtc_sum_slo_sma)) + 0.5)
my_xlim <- c(-0.5,(3 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 3 - 3) + gaps_wtc_plot, wtc_sum_slo_bas, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_sum_slo_ber, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_sum_slo_sma, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
# par(new = T)
# plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_sum_slo_hoh, type = "h", col = "black", lwd = 3, lend = 2,
#      xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 3) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 3 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("f) Trend annual total precipiation [(mm/year)/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
mtext("GWT26 weather type", side = 1, line = 2, cex = 1.2, adj = 0.5)
box(lwd = 1.2)



#calc_season----

my_seasons <- c("winter", "spring", "summer", "autumn")

for(i in 1:length(my_seasons)){
  
  seas_sel <- my_seasons[i]
  print(paste(Sys.time(), seas_sel))
  
  if(seas_sel == "winter"){
    wtc_data_seas <- wtc_data
    wtc_data_seas$valu[which(!format(wtc_data$date, '%m') %in% c("12","01","02"))] <- NA
  }
  if(seas_sel == "spring"){
    wtc_data_seas <- wtc_data
    wtc_data_seas$valu[which(!format(wtc_data$date, '%m') %in% c("03","04","05"))] <- NA
  }
  if(seas_sel == "summer"){
    wtc_data_seas <- wtc_data
    wtc_data_seas$valu[which(!format(wtc_data$date, '%m') %in% c("06","07","08"))] <- NA
  }
  if(seas_sel == "autumn"){
    wtc_data_seas <- wtc_data
    wtc_data_seas$valu[which(!format(wtc_data$date, '%m') %in% c("09","10","11"))] <- NA
  }
  
  wtc_fre_mea <- f_wtc_fre(wtc_data_in = wtc_data_seas, 
                           method_analy = "mean")
  
  wtc_fre_slo <- f_wtc_fre(wtc_data_in = wtc_data_seas, 
                           method_analy = "sens_slope")
  
  wtc_sum_mea <- f_wtc_cli(wtc_data_in = wtc_data_seas,
                           clim_data_in = data_cli,
                           annu_analy = "sum",
                           wtc_sel = my_wtc_sel,
                           method_analy = "mean")
  
  wtc_sum_slo <- f_wtc_cli(wtc_data_in = wtc_data_seas,
                           clim_data_in = data_cli,
                           annu_analy = "sum",
                           wtc_sel = my_wtc_sel,
                           method_analy = "sens_slope")
  
  wtc_mea_mea <- f_wtc_cli(wtc_data_in = wtc_data_seas,
                           clim_data_in = data_cli,
                           annu_analy = "mean",
                           wtc_sel = my_wtc_sel,
                           method_analy = "mean")
  
  wtc_mea_slo <- f_wtc_cli(wtc_data_in = wtc_data_seas,
                           clim_data_in = data_cli,
                           annu_analy = "mean",
                           wtc_sel = my_wtc_sel,
                           method_analy = "sens_slope")
  
  if(seas_sel == "winter"){
    
    wtc_fre_mea_win <- wtc_fre_mea
    wtc_fre_slo_win <- wtc_fre_slo 
    
    wtc_sum_mea_win <- wtc_sum_mea
    wtc_sum_slo_win <- wtc_sum_slo
    wtc_mea_mea_win <- wtc_mea_mea
    wtc_mea_slo_win <- wtc_mea_slo
    
  }
  
  if(seas_sel == "spring"){
    
    wtc_fre_mea_spr <- wtc_fre_mea
    wtc_fre_slo_spr <- wtc_fre_slo 
    
    wtc_sum_mea_spr <- wtc_sum_mea
    wtc_sum_slo_spr <- wtc_sum_slo
    wtc_mea_mea_spr <- wtc_mea_mea
    wtc_mea_slo_spr <- wtc_mea_slo
    
  }
  
  if(seas_sel == "summer"){
    
    wtc_fre_mea_sum <- wtc_fre_mea
    wtc_fre_slo_sum <- wtc_fre_slo 
    
    wtc_sum_mea_sum <- wtc_sum_mea
    wtc_sum_slo_sum <- wtc_sum_slo
    wtc_mea_mea_sum <- wtc_mea_mea
    wtc_mea_slo_sum <- wtc_mea_slo
    
  }
  
  if(seas_sel == "autumn"){
    
    wtc_fre_mea_aut <- wtc_fre_mea
    wtc_fre_slo_aut <- wtc_fre_slo 
    
    wtc_sum_mea_aut <- wtc_sum_mea
    wtc_sum_slo_aut <- wtc_sum_slo
    wtc_mea_mea_aut <- wtc_mea_mea
    wtc_mea_slo_aut <- wtc_mea_slo
    
  }
  
}

#visu_season----

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

my_ylim <- c(min_na(c(wtc_fre_mea_win, wtc_fre_mea_spr, wtc_fre_mea_sum, wtc_fre_mea_aut)) - 0.03, 
             max_na(c(wtc_fre_mea_win, wtc_fre_mea_spr, wtc_fre_mea_sum, wtc_fre_mea_aut)) + 0.03)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_fre_mea_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_fre_mea_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_fre_mea_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_fre_mea_win, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("a) Annaul GWT frequency [ndays]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
pos_labs <- ((1:26) * 4 - 1.5) + gaps_wtc_plot
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

my_ylim <- c(min_na(c(wtc_fre_slo_win, wtc_fre_slo_spr, wtc_fre_slo_sum, wtc_fre_slo_aut)) - 0.03, 
             max_na(c(wtc_fre_slo_win, wtc_fre_slo_spr, wtc_fre_slo_sum, wtc_fre_slo_aut)) + 0.03)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_fre_slo_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_fre_slo_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_fre_slo_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_fre_slo_win, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks

axis(1, at = (x_lab_posi * 4) + gaps_wtc_plot[x_lab_posi] -1.5, labels = x_lab_posi, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.4, 0), cex.axis = size_x_labs)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = size_y_labs)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("b) Trend annaul GWT frequency [ndays/decade]", side = 3, line = line_main, cex = size_main, adj = 0)
box(lwd = 1.2)

directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
pos_labs <- ((1:26) * 4 - 1.5) + gaps_wtc_plot
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

my_ylim <- c(0, max_na(c(wtc_mea_mea_win, wtc_mea_mea_spr, wtc_mea_mea_sum, wtc_mea_mea_aut)) + 1.2)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_mea_mea_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_mea_mea_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_mea_mea_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_mea_win, type = "h", col = "black", lwd = 3, lend = 2,
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

mtext("spring",     side = 3, line = -3.0 + 2.5, adj = 0.005, padj = 1, cex = 0.6)
mtext("summer",      side = 3, line = -3.6 + 2.5, adj = 0.016, padj = 1, cex = 0.6)
mtext("autumn",   side = 3, line = -4.2 + 2.5, adj = 0.022,  padj = 1, cex = 0.6)
mtext("winter",   side = 3, line = -4.8 + 2.5, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(9.9, 11.25), type = "l", lwd = 0.5)
lines(c(2,2), c(9.9, 10.80), type = "l", lwd = 0.5)
lines(c(3,3), c(9.9, 10.35), type = "l", lwd = 0.5)
lines(c(4,4), c(9.9, 10.00), type = "l", lwd = 0.5)


# Plot: d: Trend mean precipiation per day

my_ylim <- c(min_na(c(wtc_mea_slo_win, wtc_mea_slo_spr, wtc_mea_slo_sum, wtc_mea_slo_aut)) - 0.03, 
             max_na(c(wtc_mea_slo_win, wtc_mea_slo_spr, wtc_mea_slo_sum, wtc_mea_slo_aut)) + 0.03)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_mea_slo_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_mea_slo_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_mea_slo_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_mea_slo_win, type = "h", col = "black", lwd = 3, lend = 2,
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

my_ylim <- c(0, max_na(c(wtc_sum_mea_win, wtc_sum_mea_spr, wtc_sum_mea_sum, wtc_sum_mea_aut)) + 5)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_sum_mea_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_sum_mea_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_sum_mea_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_sum_mea_win, type = "h", col = "black", lwd = 3, lend = 2,
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

mtext("spring",     side = 3, line = -3.0 - 0.8, adj = 0.005, padj = 1, cex = 0.6)
mtext("summer",      side = 3, line = -3.6 - 0.8, adj = 0.016, padj = 1, cex = 0.6)
mtext("autumn",   side = 3, line = -4.2 - 0.8, adj = 0.022, padj = 1, cex = 0.6)
mtext("winter",   side = 3, line = -4.8 - 0.8, adj = 0.029, padj = 1, cex = 0.6)

lines(c(1,1), c(75+0, 129-20), type = "l", lwd = 0.5)
lines(c(2,2), c(75+0, 122-20), type = "l", lwd = 0.5)
lines(c(3,3), c(75+0, 116-20), type = "l", lwd = 0.5)
lines(c(4,4), c(75+0, 110-20), type = "l", lwd = 0.5)

# Plot f: Trend annual sum

my_ylim <- c(min_na(c(wtc_sum_slo_win, wtc_sum_slo_spr, wtc_sum_slo_sum, wtc_sum_slo_aut)) - 0.5,
             max_na(c(wtc_sum_slo_win, wtc_sum_slo_spr, wtc_sum_slo_sum, wtc_sum_slo_aut)) + 0.5)
my_xlim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_sum_slo_spr, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_sum_slo_sum, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_sum_slo_aut, type = "h", col = "black", lwd = 3, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "", ylim = my_ylim, xlim = my_xlim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_sum_slo_win, type = "h", col = "black", lwd = 3, lend = 2,
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




#cpa27----
# my_wtc_sel <- c(21, 24, 19, 17, 14, 4, 15, 10, 22, 23, 27, 13,6, 11, 8, 18, 3, 16,
#                 1, 5, 7, 9, 20, 25, 12, 26, 2)
# my_wtc_sel[19]
#21 West, cyclonic
#24 West-SouthWest, cyclonic
#19 SouthWest, cyclonic
#17 South-SouthWest, cyclonic
#14 South-SouthEast, cyclonic
#4 West-SouthWest, cyclonic, flat pressure
#15 West-NorthWest, cyclonic
#10 NorthWest, cyclonic
#22 NorthEast, cyclonic
#23 East, cyclonic
#27 Westerly Flow over Southern Europe, cyclonic
#13 Westerly Flow over Northern Europe
#6 Trough over Central Europe

#11 West-NorthWest, anticyclonic
#8 South-SouthEast, anticyclonic
#18 East-SouthEast, anticyclonic
#3 NorthWest, anticyclonic, flat pressure
#16 East, anticyclonic / indifferent

#1 West, indifferent, flat pressure
#5 NorthWest, indifferent
#7 East-SouthEast, indifferent
#9 East, indifferent
#20 West, indifferent

#25 Low pressure over the Alps
#12 High pressure over the Alps
#26 High pressure over Central Europe
#2 High pressure over Eastern Europe



#stuff----

#Frequency of occurence
gwt_frequ <- rep(NA, 26)

for(i in 1:26){
  
  gwt_frequ[i] <- (length(which(data_gwt26$value == i)) / length(data_gwt26$value)) * 100 #%
  
}

names(gwt_frequ) <- 1:26
gwt_frequ_sort <- sort(gwt_frequ, decreasing = T)

plot(gwt_frequ, type ="h", lwd = 8, lend=1)
# plot(gwt_frequ_sort, type ="h", lwd = 8)


#Total amount of rainfall for each GWT
rain_sums <- rep(NA, 26)

for(i in 1:26){
  
  rain_sums[i] <- sum_na(data_clima$value[which(data_gwt26$value == i)])
  
}

names(rain_sums) <- 1:26
rain_sums_sort <- sort(rain_sums, decreasing = T)

plot(rain_sums, type ="h", lwd = 8, lend=1)
# plot(rain_sums_sort, type ="h", lwd = 8)

#Mean amount of rainfall for each GWT
rain_meas <- rep(NA, 26)

for(i in 1:26){
  
  rain_meas[i] <- mea_na(data_clima$value[which(data_gwt26$value == i)])
  
}

names(rain_meas) <- 1:26
rain_meas_sort <- sort(rain_meas, decreasing = T)

plot(rain_meas, type ="h", lwd = 8, lend=1)
# plot(rain_meas_sort, type ="h", lwd = 8)


#difficulty: even GWTs with low average rainfall amount (mostly dry) can substantially contribute to the
#total amount of rainfall when very frequent (e.g. GWT type 9 and 10)






















#Trend window mean amount
#change amount per occurence? Intensification?


#
#
#














gwt_sel <- 1

data_clima$value[which(data_gwt26$value == gwt_sel)]




gwt_med <- function(dates, clim_data, gwt_data){
  
  input_data <- data.frame(dates = dates,
                           clim = clim_data,
                           gwt = gwt_data)
  
  
  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$dates, "%m%d") == "0229"),]
  
  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")
  
  #Order climate data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$clim[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_clim <- data_day
  
  #Order gwt data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year
  
  for(i in 0:(length(start_year:end_year)-1)) {
    
    data_day[i+1, 2:366] <- input_data$gwt[(i*365+1):((i+1)*365)]
    
  }
  
  data_day_gwt <- data_day
  
  
  for(k in 1:26){
    
    for (i in 2:ncol(data_day_clim)) {
      
      gwt_days <- which(data_day_gwt[, i] == k)
      gwt_days_med <- median(data_day_clim[gwt_days, i])
      
      if(i == 2){
        gwt_med <- gwt_days_med
      }else
        gwt_med <- c(gwt_med, gwt_days_med)
    }
    
    if(k ==1){
      gwt_out <- gwt_med
    }else{
      gwt_out <- cbind(gwt_out, gwt_med)
    }
    
  }
  
  colnames(gwt_out) <- 1:26
  return(gwt_out)
  
}

  
gwt_clima <- gwt_med(dates = data_clima$date, clim_data = data_clima$value, gwt_data = data_gwt26$value)

#get rank out of mean values
num_hig_sel <- 15
num_low_sel <- 15
gwt_rank_clima <- matrix(NA, ncol = 26, nrow = 365)

for (i in 1:365) {
  
  gwt_clima_sort <- sort(gwt_clima[i, ])
  
  if(length(gwt_clima_sort) > sum(num_hig_sel, num_low_sel)){
    gwt_cold <- as.numeric(names(gwt_clima_sort)[1:num_low_sel])
    gwt_warm <- as.numeric(names(gwt_clima_sort)[(length(gwt_clima_sort) - num_hig_sel + 1) : length(gwt_clima_sort)])
  }else{
    is.even <- function(x) {x %% 2 == 0}
    if(is.even(length(gwt_clima_sort))){
      gwt_cold <- as.numeric(names(gwt_clima_sort)[1:(length(gwt_clima_sort) / 2)])
      gwt_warm <- as.numeric(names(gwt_clima_sort)[((length(gwt_clima_sort) / 2) + 1) : length(gwt_clima_sort)])
    }else{
      gwt_cold <- as.numeric(names(gwt_clima_sort)[1:(floor(length(gwt_clima_sort) / 2))])
      gwt_warm <- as.numeric(names(gwt_clima_sort)[(ceiling((length(gwt_clima_sort) / 2)) + 1) : length(gwt_clima_sort)])
    }
  }
  # gwt_rank_tem0[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
  # gwt_rank_tem0[i, gwt_warm] <-   1 : length(gwt_warm)
  gwt_rank_clima[i, gwt_cold] <-  -1
  gwt_rank_clima[i, gwt_warm] <-   1
  
}

#Determine driving weather types
f_sum_neg <- function(data_in){
  
  data_in[which(data_in > 0)] <- NA
  score_out <- sum_na(data_in)
  
}
f_sum_pos <- function(data_in){
  
  data_in[which(data_in < 0)] <- NA
  score_out <- sum_na(data_in)
  
}
gwt_sums_clima_low <- apply(gwt_rank_clima[ , ], 2, f_sum_neg)
gwt_sums_clima_hig <- apply(gwt_rank_clima[ , ], 2, f_sum_pos)
gwt_sums_clima     <- apply(gwt_rank_clima[ , ], 2, sum_na)

names(gwt_sums_clima_low) <- 1:26
names(gwt_sums_clima_hig) <- 1:26
names(gwt_sums_clima)     <- 1:26

gwt_sums_clima_low_sort <- sort(gwt_sums_clima_low)
gwt_sums_clima_hig_sort <- sort(gwt_sums_clima_hig)
gwt_sums_clima_sort     <- sort(gwt_sums_clima)

gwt_score_out <- cbind(gwt_sums_clima_low, gwt_sums_clima_hig, gwt_sums_clima)
colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"))

gwt_low_clima <- c(17, 1, 2, 18)
gwt_high_clima <- c(17, 1, 2, 18, 25, 3, 8)

#Calculate changes in frequencies
gwt_clima_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_clima)*100*10# [%/dec]

plot(smoothFFT(gwt_clima_high, sd = 4), type = "l")
abline(h = 0, lty = "dashed")

gwt_clima_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_clima)*100*10 # [%/dec]

plot(smoothFFT(gwt_clima_low, sd = 4), type = "l")
abline(h = 0, lty = "dashed")


#CumSums
gwt_sel <- c(17, 1, 2, 18)

test1 <- gwt26_data$wkwtp3d0

test1[which(!(test1 %in% gwt_sel))] <- 0
test1[which(test1 %in% gwt_sel)]    <- 1

plot(cumsum(test1), type = "l")
abline(lm())


test <- dis_ana(disc = data_gwt26$value, 
        date = data_gwt26$date, 
        start_year = start_year, 
        end_year = end_year,
        method_analys = "weather_type_window_sens_slope",
        weather_type = c(17, 1, 2, 18, 25, 3, 8)
)

plot(smoothFFT(test, sd = 4), type = "l")
abline(h = 0, lty = "dashed")


#visu_old----

gwt_max <- max_na(c(loess_NA_restore(gwt_clima_low),
                    loess_NA_restore(gwt_clima_high),
                    (loess_NA_restore(gwt_clima_high) - loess_NA_restore(gwt_clima_low))))+2

gwt_min <- min_na(c(loess_NA_restore(gwt_clima_low),
                    loess_NA_restore(gwt_clima_high),
                    (loess_NA_restore(gwt_clima_high) - loess_NA_restore(gwt_clima_low))))-1

#col2rgb("blue3")
my_blu     <- rgb(0, 0, 205, max=255, alpha = 255)
my_blu_bar <- rgb(0, 0, 205, max=255, alpha = 255)
my_blu_rec <- rgb(0, 0, 205, max=255, alpha = 40)
#col2rgb("red3")
my_red     <- rgb(205, 0, 0, max=255, alpha = 255)
my_red_bar <- rgb(205, 0, 0, max=255, alpha = 255)
my_red_rec <- rgb(205, 0, 0, max=255, alpha = 40)
#col2rgb("grey20")
my_bla     <- rgb(50, 50, 50,   max=255, alpha = 220)
my_bla_bar <- rgb(0, 0, 0, max=255, alpha = 255)


#WTE index trend

par(mar = c(1.5, 2.5, 2.1, 1.3))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(gwt_clima_high, type = "n", main ="",
     ylim = c(gwt_min, gwt_max),
     ylab = "", xlab = "", axes = F)
lines(loess_NA_restore(gwt_clima_low),  col = my_blu, lwd = 2)
lines(loess_NA_restore(gwt_clima_high), col = my_red, lwd = 2)
lines(loess_NA_restore(gwt_clima_high) - loess_NA_restore(gwt_clima_low), col = "black", lwd = 2)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.9)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
mtext("b) WTE index",               side = 3, line = 0.1, cex = 1.0)
mtext("Window prob. [%/dec]", side = 2, line = 2,                padj = 1, cex = 0.8)
legend("topleft", c("                    ","                    "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
mtext("warm GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = my_red)
mtext("cold GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = my_blu)
mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = "black")
box()







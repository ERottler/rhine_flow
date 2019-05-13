###

#Rhine river observations - Main file
#Erwin Rottler, University of Potsdam
#Summer 2018

###

#settings----

#load packages
# devtools::install_github('ERottler/alptempr')
pacman::p_load(ncdf4, ncdf4.helpers, PCICt, dplyr, readr, tidyr, rgeos, ggplot2, 
               sp, viridis, rgdal, leaflet, ggmap, zoo, zyp, alptempr, lmomco, 
               raster, foreach, rfs, dismo, XML, parallel, doParallel, Lmoments,
               shape, devtools, pbapply, profvis, RColorBrewer, viridis, Rcpp, rEchseSnow,
               Rlibeemd, xts, emdbook, rfs)

#set directories
base_dir <- "u:/RhineFlow/rhine_obs/"
# file_dir <- "e:/mhm_data/04_Daten/lobith_6435060/input/"
file_dir <- "d:/nrc_user/rottler/toErwin1/6435060/"
# file_dir <- "D:/nrc_data/01_Data localized (area specific)/Germany/E-OBS Daten Rhein Berry Boessenkool/RhineFloodSeasonality(data mHM)/04_Daten/lobith_6435060/input/" #location data files

#general parameter
start_year <- 1950
end_year <- 2014
window_width <- 30 
cover_thres <- 10/60 #minimum fraction of NA values at trend computing

#basin parameter
do_basin_prep <- T
do_basin_calc <- T
do_snow_sim <- T #do snow cover simulation with snowAlone
do_snow_sim_vis <- T #visualization basin output with snow simulation
# Rcpp::sourceCpp(paste0(base_dir, "R/rhine_flow/echse_snow.cpp"))
# Rcpp::Rcpp.package.skeleton(name = "rEchseSnow", cpp_files = paste0(base_dir, "R/rhine_flow/echse_snow.cpp"))
# install.packages("u:/RhineFlow/rhine_obs/R/rhine_flow/rEchseSnow", repos=NULL, type="source")
# library(rEchseSnow)
snow_params <- read.table(paste0(base_dir, "R/rhine_flow/snow_param.txt"), header = T, sep = ";")
snow_exe <- paste0(base_dir, "snow_sim/snowAlone/snowAlone/Debug/snowAlone.exe")
basin_sel <- "alp_rhine"        # alp_rhine,  reuss,     aare,  moselle, nahe,      neckar,   main,      lahn, basel
basin_stn <- "Diepoldsau"       # Diepoldsau, Mellingen, Brugg, Cochem,  Grolsheim, Rockenau, Frankfurt, Kalkofen, Basel_Rheinhalle
high_stat_thresh <- 1900
middle_stat_thresh <- 900

#flow parameter
do_flow <- F #do go into file 3_rhine_flow.R to to discharge calculations
do_prob <- F #do analysis/visualization (moving) probability all quantiles
do_extr <- F #do analysis/visualization (moving) probability high quantiles only
do_disco <- F #do analysis/visualization discharge coefficient
do_regime <- T #regime change along the Rhine river analysis/visualization
gaug_sel <- c("Diepoldsau", "Brugg", "Mellingen", "Basel_Rheinhalle", "Rockenau", "Worms", 
              "Frankfurt", "Grolsheim", "Kaub", "Kalkofen", "Cochem", "Koeln")
quants <- seq(0.01, 0.99, by = 0.01)
quants_ext <- c(0.850, 0.900, 0.925, 0.950)

#snow parameter
do_snow = F #do analysis snow height measurements selected stations

#weather parameter
#do basin meteo data preparation!!! (do_basin_prep <- T)
do_wtc_calc <- T
do_wtc_visu <- T
do_mean_wt_clim <- T
wt_wt_data <- "gwt26msl" #gwt26geo, gwt26msl, cap27msl, ncl40ali
wt_clim_data <- "rainfall" #temperature, rainfall
wt_num <- 26 #number of weather type classes in classification
wt_low <- 1:6 #selected low weather types for trend analysis
wt_hig <- 25:26 #selected high weather types for trend analysis
wt_slo <-  "fixed" #fixed, flexi

# #emd parameter
# data_sel_emd <- "basel_disc"  #basel_disc, basel_temp
# sta_yea_emd <- 1950
# end_yea_emd <- 2014
# do_ma_emd <- T
# do_na_fil_emd <- T
# do_center_emd <- T
# number_trials <- 1000
# noise_str <- 0.5

#load functions
source(paste0(base_dir, "R/rhine_flow/2_rhine_functions.R"))





# load("u:/RhineFlow/rhine_obs/manus/figures/riv_flow_new.Rdata")
# 
# qvalu_wuer <- qvalu_long
# qvslo_wuer <- qvslo_long
# 
# emd_disc_wuer <- emd_resid
# emd_temp_zuer <- emd_resid
# 
# qannu_wuer_win <- qannu_resid
# qannu_rain_zuer_win <- qannu_resid
# 
# save(qvalu_wass, qvslo_wass, 
#      qvalu_koel, qvslo_koel,
#      qvalu_base, qvslo_base,
#      qvalu_wuer, qvslo_wuer,
#      emd_disc_wass, emd_disc_base, emd_disc_koel, emd_disc_wuer,
#      qannu_wass, qannu_base, qannu_koel, qannu_wuer,
#      sta_yea_ann, end_yea_ann, sta_yea_emd, end_yea_emd,
#      qannu_wass_spr, qannu_wass_sum, qannu_wass_aut, qannu_wass_win,
#      qannu_base_spr, qannu_base_sum, qannu_base_aut, qannu_base_win,
#      qannu_koel_spr, qannu_koel_sum, qannu_koel_aut, qannu_koel_win,
#      qannu_wuer_spr, qannu_wuer_sum, qannu_wuer_aut, qannu_wuer_win,
#      emd_rain_base, emd_rain_bern, emd_rain_zuer,
#      emd_temp_base, emd_temp_bern, emd_temp_zuer,
#      qannu_rain_base, qannu_rain_bern, qannu_rain_zuer,
#      qannu_temp_base, qannu_temp_bern, qannu_temp_zuer,
#      qannu_rain_bern_spr, qannu_rain_bern_sum, qannu_rain_bern_aut, qannu_rain_bern_win,
#      qannu_rain_base_spr, qannu_rain_base_sum, qannu_rain_base_aut, qannu_rain_base_win,
#      qannu_rain_zuer_spr, qannu_rain_zuer_sum, qannu_rain_zuer_aut, qannu_rain_zuer_win,
#      emd_day_wass, emd_day_wass_ori,
#      qvalu_long_wass, qvalu_long_base, qvalu_long_koel, qvalu_long_wuer,
#      
#      file = "u:/RhineFlow/rhine_obs/manus/figures/riv_flow_new.Rdata")
# 


# load("u:/RhineFlow/rhine_obs/R/rhine_flow/RhineApp/data/rhine_flow_app.RData")
# 
# qvalu_long_koel <- qvalu_long
# qvslo_long_koel <- qvslo_long
# 
# save(qvalu_long_diep, qvslo_long_diep,
#      qvalu_long_mell, qvslo_long_mell,
#      qvalu_long_brug, qvslo_long_brug,
#      qvalu_long_base, qvslo_long_base,
#      qvalu_long_coch, qvslo_long_coch,
#      qvalu_long_grol, qvslo_long_grol,
#      qvalu_long_koel, qvslo_long_koel,
#      qvalu_long_kaub, qvslo_long_kaub,
#      qvalu_long_rock, qvslo_long_rock,
#      qvalu_long_kalk, qvslo_long_kalk,
#      qvalu_long_fran, qvslo_long_fran,
#      qvalu_long_worm, qvslo_long_worm,
# 
#      meta_grid_bands_diep, my_elev_bands_diep,
#      tmed_band_diep, tmed_band_med_diep,
#      tslo_band_diep, tslo_band_med_diep,
#      pmea_band_diep, pmea_band_med_diep,
#      pslo_band_diep, pslo_band_med_diep,
#      emed_band_diep, emed_band_med_diep,
#      eslo_band_diep, eslo_band_med_diep,
#      smea_band_diep, vmea_band_diep,
#      vdif_band_diep, vdis_band_diep,
# 
#      meta_grid_bands_mell, my_elev_bands_mell,
#      tmed_band_mell, tmed_band_med_mell,
#      tslo_band_mell, tslo_band_med_mell,
#      pmea_band_mell, pmea_band_med_mell,
#      pslo_band_mell, pslo_band_med_mell,
#      emed_band_mell, emed_band_med_mell,
#      eslo_band_mell, eslo_band_med_mell,
#      smea_band_mell, vmea_band_mell,
#      vdif_band_mell, vdis_band_mell,
# 
#      meta_grid_bands_brug, my_elev_bands_brug,
#      tmed_band_brug, tmed_band_med_brug,
#      tslo_band_brug, tslo_band_med_brug,
#      pmea_band_brug, pmea_band_med_brug,
#      pslo_band_brug, pslo_band_med_brug,
#      emed_band_brug, emed_band_med_brug,
#      eslo_band_brug, eslo_band_med_brug,
#      smea_band_brug, vmea_band_brug,
#      vdif_band_brug, vdis_band_brug,
# 
#      meta_grid_bands_neck, my_elev_bands_neck,
#      tmed_band_neck, tmed_band_med_neck,
#      tslo_band_neck, tslo_band_med_neck,
#      pmea_band_neck, pmea_band_med_neck,
#      pslo_band_neck, pslo_band_med_neck,
#      emed_band_neck, emed_band_med_neck,
#      eslo_band_neck, eslo_band_med_neck,
#      smea_band_neck, vmea_band_neck,
#      vdif_band_neck, vdis_band_neck,
# 
#      meta_grid_bands_mose, my_elev_bands_mose,
#      tmed_band_mose, tmed_band_med_mose,
#      tslo_band_mose, tslo_band_med_mose,
#      pmea_band_mose, pmea_band_med_mose,
#      pslo_band_mose, pslo_band_med_mose,
#      emed_band_mose, emed_band_med_mose,
#      eslo_band_mose, eslo_band_med_mose,
#      smea_band_mose, vmea_band_mose,
#      vdif_band_mose, vdis_band_mose,
# 
#      meta_grid_bands_nahe, my_elev_bands_nahe,
#      tmed_band_nahe, tmed_band_med_nahe,
#      tslo_band_nahe, tslo_band_med_nahe,
#      pmea_band_nahe, pmea_band_med_nahe,
#      pslo_band_nahe, pslo_band_med_nahe,
#      emed_band_nahe, emed_band_med_nahe,
#      eslo_band_nahe, eslo_band_med_nahe,
#      smea_band_nahe, vmea_band_nahe,
#      vdif_band_nahe, vdis_band_nahe,
# 
#      meta_grid_bands_lahn, my_elev_bands_lahn,
#      tmed_band_lahn, tmed_band_med_lahn,
#      tslo_band_lahn, tslo_band_med_lahn,
#      pmea_band_lahn, pmea_band_med_lahn,
#      pslo_band_lahn, pslo_band_med_lahn,
#      emed_band_lahn, emed_band_med_lahn,
#      eslo_band_lahn, eslo_band_med_lahn,
#      smea_band_lahn, vmea_band_lahn,
#      vdif_band_lahn, vdis_band_lahn,
# 
#      meta_grid_bands_main, my_elev_bands_main,
#      tmed_band_main, tmed_band_med_main,
#      tslo_band_main, tslo_band_med_main,
#      pmea_band_main, pmea_band_med_main,
#      pslo_band_main, pslo_band_med_main,
#      emed_band_main, emed_band_med_main,
#      eslo_band_main, eslo_band_med_main,
#      smea_band_main, vmea_band_main,
#      vdif_band_main, vdis_band_main,
# 
#      meta_grid_bands_base, my_elev_bands_base,
#      tmed_band_base, tmed_band_med_base,
#      tslo_band_base, tslo_band_med_base,
#      pmea_band_base, pmea_band_med_base,
#      pslo_band_base, pslo_band_med_base,
#      emed_band_base, emed_band_med_base,
#      eslo_band_base, eslo_band_med_base,
#      smea_band_base, vmea_band_base,
#      vdif_band_base, vdis_band_base,
# 
#      file = "u:/RhineFlow/rhine_obs/R/rhine_flow/RhineApp/data/rhine_flow_app.RData")
# 
# 
# 
# 
# meta_grid_bands_base <- meta_grid_bands
# my_elev_bands_base <- my_elev_bands
# tmed_band_base <- tmed_band
# tmed_band_med_base <- tmed_band_med
# tslo_band_base <- tslo_band
# tslo_band_med_base <- tslo_band_med
# pmea_band_base <- pmea_band
# pmea_band_med_base <- pmea_band_med
# pslo_band_base <- pslo_band
# pslo_band_med_base <- pslo_band_med
# emed_band_base <- emed_band
# emed_band_med_base <- emed_band_med
# eslo_band_base <- eslo_band
# eslo_band_med_base <- eslo_band_med
# 
# smea_band_base <- smea_band
# vmea_band_base <- vmea_band
# vdif_band_base <- vdif_band
# vdis_band_base <- vdis_band
# 
# 
# 
# 














load("u:/RhineFlow/rhine_obs/manus/figures/riv_flow.Rdata")

save(qvalu_wass, qvslo_wass, qvalu_burg, qvslo_burg, qvalu_base, qvslo_base,
     qvalu_koel, qvslo_koel, qvalu_rees, qvslo_rees, qvalu_wuer, qvslo_wuer,
     qvalu_dres, qvslo_dres,
     emd_disc_wass, emd_disc_burg, emd_disc_base, emd_disc_koel,
     emd_disc_rees, emd_disc_wuer, emd_disc_dres,
     emd_temp_bern, emd_temp_base, emd_temp_zuer, emd_temp_hohe,
     sta_yea_emd, end_yea_emd,
     qannu_wass, qannu_burg, qannu_base, qannu_koel, 
     qannu_rees, qannu_wuer, qannu_dres,
     qannu_rain_bern, qannu_rain_base, qannu_rain_zuer, qannu_rain_hohe,
     sta_yea_ann, end_yea_ann, 
     file = "u:/RhineFlow/rhine_obs/manus/figures/riv_flow.Rdata") 








# #Make cluster for parallel computing
# my_clust <- makeCluster(n_cores)
# clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, alptempr, lmomco, ncdf4, rEchseSnow, sp, raster))
# registerDoParallel(my_clust)

#analy_basin----
source(paste0(base_dir, "R/5_rhine_basin.R"))
#analy_flow----
if(do_flow){
  source(paste0(base_dir, "R/3_rhine_flow.R"))
}

#analy_snow----
if(do_snow){
  source(paste0(base_dir, "R/4_rhine_snow.R"))
}



#analysis_wtype----

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
wt_clim_mea <- gwt_mea(dates = full_date, clim_data = clim_data, gwt_data = wt_data$value, numb_wt=wt_num)

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
  
wt_hig_slo <- wt_flex_mov_2(wt_data = wt_data$value, wt_rank = wt_rank, date = my_date , hig_low = 1, 
                          start_year = start_year, end_year = end_year, window_width = window_width, 
                          cover_thres = cover_thres, wts_numb = length(wt_hig))*100*10 # [%/dec]

wt_low_slo <- wt_flex_mov_2(wt_data = wt_data$value, wt_rank = wt_rank, date = my_date , hig_low = -1, 
                          start_year = start_year, end_year = end_year, window_width = window_width, 
                          cover_thres = cover_thres, wts_numb = length(wt_low))*100*10 # [%/dec]

}

#visuali_weath----

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

#visuali_spati----

pdf(paste0(base_dir, "figs/",basin_sel, "_spatial", ".pdf"), width = 6, height = 4)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mfrow = c(2,2))

#Map of watershed and enclosed meteo grid points
par(mar = c(3, 3, 2, 1))

plot(dem_84, main = paste0(basin_sel), axes=F)
lines(basin_84, lwd = 0.7)
# points(grid_points, cex = 0.03, pch=19)
axis(1, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
axis(2, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)

plot(1,1, type = F)

#Histogramm elevation distribution dem
par(mar = c(3, 3, 2, 1))
graphics::hist(dem_ele,  breaks = 15, col = "grey50",
               # yaxs =  "i", xaxs = "i", 
               xlab = "Elevation [m]", axes=F,
               main = "dem")
axis(1, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
axis(2, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
mtext(paste0("Frequency"), side = 2, line = 1.5, padj = 1, cex = 0.8)
mtext(paste0("Elevation"), side = 1, line = 0.5, padj = 1, cex = 0.8)
box()

#Histogramm elevation distribution grid points
graphics::hist(elevs,  breaks = 15, col = "grey50",
               # yaxs =  "i", xaxs = "i", 
               xlab = "Elevation [m]", axes=F,
               main = paste0("grid (", length(grid_points), ")"))
axis(1, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
axis(2, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
mtext(paste0("Frequency"), side = 2, line = 1.5, padj = 1, cex = 0.8)
mtext(paste0("Elevation"), side = 1, line = 0.5, padj = 1, cex = 0.8)
box()

dev.off()




# #stop cluster----
# stopCluster(my_clust)


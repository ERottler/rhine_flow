###

#Rhine river observations - Main file
#Erwin Rottler, University of Potsdam
#Summer 2018

###

#settings----

#load packages
# devtools::install_github('ERottler/alptempr')
# devtools::install_github('ERottler/meltimr')
pacman::p_load(ncdf4, ncdf4.helpers, PCICt, dplyr, readr, tidyr, rgeos, ggplot2, 
               sp, viridis, rgdal, leaflet, ggmap, zoo, zyp, alptempr, lmomco, 
               raster, foreach, rfs, dismo, XML, parallel, doParallel, Lmoments,
               shape, devtools, pbapply, profvis, RColorBrewer, viridis, Rcpp, rEchseSnow,
               Rlibeemd, xts, emdbook, rfs, meltimr)

#set directories
base_dir <- "U:/RhineFlow/rhine_obs/"
# file_dir <- "e:/mhm_data/04_Daten/lobith_6435060/input/"
# file_dir <- "d:/nrc_user/rottler/toErwin1/6435060/"
# file_dir <- "D:/nrc_data/01_Data localized (area specific)/Germany/E-OBS Daten Rhein Berry Boessenkool/RhineFloodSeasonality(data mHM)/04_Daten/lobith_6435060/input/" #location data files

#general parameter
# start_year <- 1950
# end_year <- 2014
# window_width <- 30 
# cover_thres <- 10/60 #minimum fraction of NA values at trend computing

#flow parameter
# do_flow <- F #do go into file 3_rhine_flow.R to to discharge calculations
# do_prob <- F #do analysis/visualization (moving) probability all quantiles
# do_extr <- F #do analysis/visualization (moving) probability high quantiles only
# do_disco <- F #do analysis/visualization discharge coefficient
# do_regime <- T #regime change along the Rhine river analysis/visualization
# gaug_sel <- c("Diepoldsau", "Brugg", "Mellingen", "Basel_Rheinhalle", "Rockenau", "Worms", 
#               "Frankfurt", "Grolsheim", "Kaub", "Kalkofen", "Cochem", "Koeln")
# quants <- seq(0.01, 0.99, by = 0.01)
# quants_ext <- c(0.850, 0.900, 0.925, 0.950)

#snow parameter
# do_snow = F #do analysis snow height measurements selected stations

# #weather parameter
# #do basin meteo data preparation!!! (do_basin_prep <- T)
# do_wtc_calc <- T
# do_wtc_visu <- T
# do_mean_wt_clim <- T
# wt_wt_data <- "gwt26msl" #gwt26geo, gwt26msl, cap27msl, ncl40ali
# wt_clim_data <- "rainfall" #temperature, rainfall
# wt_num <- 26 #number of weather type classes in classification
# wt_low <- 1:6 #selected low weather types for trend analysis
# wt_hig <- 25:26 #selected high weather types for trend analysis
# wt_slo <-  "fixed" #fixed, flexi


#load functions
source(paste0(base_dir, "R/rhine_flow/2_rhine_functions.R"))


emd_rain_sion <- emd_resid
emd_mk_rain_sion <- emd_mk

qannu_rain_sion <- qannu_resid
qannu_mk_rain_sion <- qannu_mk

load("u:/RhineFlow/rhine_obs/manus/figures/riv_flow_new.Rdata")

save(qvalu_wass, qvslo_wass,
     qvalu_base, qvslo_base,
     qvalu_koel, qvslo_koel,
     qvalu_wuer, qvslo_wuer,
     emd_disc_wass, emd_disc_base, emd_disc_koel, emd_disc_wuer,
     qannu_wass, qannu_base, qannu_koel, qannu_wuer,
     emd_temp_bern, emd_temp_base, emd_temp_zuer,
     emd_temp_chau, emd_temp_gene, emd_temp_luga,
     emd_temp_neuc, emd_temp_same, emd_temp_sion,
     emd_mk_temp_chau, emd_mk_temp_gene, emd_mk_temp_luga,
     emd_mk_temp_neuc, emd_mk_temp_same, emd_mk_temp_sion,
     emd_rain_chau, emd_rain_gene, emd_rain_luga,
     emd_rain_neuc, emd_rain_same, emd_rain_sion,
     emd_mk_rain_chau, emd_mk_rain_gene, emd_mk_rain_luga,
     emd_mk_rain_neuc, emd_mk_rain_same, emd_mk_rain_sion,
     qannu_temp_chau, qannu_temp_gene, qannu_temp_luga,
     qannu_temp_neuc, qannu_temp_same, qannu_temp_sion,
     qannu_mk_temp_chau, qannu_mk_temp_gene, qannu_mk_temp_luga,
     qannu_mk_temp_neuc, qannu_mk_temp_same, qannu_mk_temp_sion,
     qannu_rain_chau, qannu_rain_gene, qannu_rain_luga,
     qannu_rain_neuc, qannu_rain_same, qannu_rain_sion,
     qannu_mk_rain_chau, qannu_mk_rain_gene, qannu_mk_rain_luga,
     qannu_mk_rain_neuc, qannu_mk_rain_same, qannu_mk_rain_sion,
     
     emd_rain_bern, emd_rain_base, emd_rain_zuer,
     qannu_temp_bern, qannu_temp_base, qannu_temp_zuer,
     qannu_rain_bern, qannu_rain_base, qannu_rain_zuer,
     qannu_wass_spr, qannu_wass_sum, qannu_wass_aut, qannu_wass_win,
     qannu_base_spr, qannu_base_sum, qannu_base_aut, qannu_base_win,
     qannu_koel_spr, qannu_koel_sum, qannu_koel_aut, qannu_koel_win,
     qannu_wuer_spr, qannu_wuer_sum, qannu_wuer_aut, qannu_wuer_win,
     qannu_rain_bern_spr, qannu_rain_bern_sum, qannu_rain_bern_aut, qannu_rain_bern_win,
     qannu_rain_base_spr, qannu_rain_base_sum, qannu_rain_base_aut, qannu_rain_base_win,
     qannu_rain_zuer_spr, qannu_rain_zuer_sum, qannu_rain_zuer_aut, qannu_rain_zuer_win,
     emd_day_wass, emd_day_base, emd_day_koel, emd_day_wuer,
     emd_day_wass_ori, emd_day_koel_ori, sta_yea_emd, end_yea_emd,
     sta_yea_ann, end_yea_ann,
     emd_mk_wass, emd_mk_base, emd_mk_koel, emd_mk_wuer, 
     qannu_mk_wass, qannu_mk_base, qannu_mk_koel, qannu_mk_wuer,
     emd_mk_temp_bern, emd_mk_temp_base, emd_mk_temp_zuer,
     emd_mk_rain_bern, emd_mk_rain_base, emd_mk_rain_zuer,
     qannu_mk_temp_bern, qannu_mk_temp_base, qannu_mk_temp_zuer,
     qannu_mk_rain_bern, qannu_mk_rain_base, qannu_mk_rain_zuer,
     qannu_mk_wass_spr, qannu_mk_wass_sum, qannu_mk_wass_aut, qannu_mk_wass_win,
     qannu_mk_base_spr, qannu_mk_base_sum, qannu_mk_base_aut, qannu_mk_base_win,
     qannu_mk_koel_spr, qannu_mk_koel_sum, qannu_mk_koel_aut, qannu_mk_koel_win,
     qannu_mk_wuer_spr, qannu_mk_wuer_sum, qannu_mk_wuer_aut, qannu_mk_wuer_win,
     qannu_mk_rain_bern_spr, qannu_mk_rain_bern_sum, qannu_mk_rain_bern_aut, qannu_mk_rain_bern_win,
     qannu_mk_rain_base_spr, qannu_mk_rain_base_sum, qannu_mk_rain_base_aut, qannu_mk_rain_base_win,
     qannu_mk_rain_zuer_spr, qannu_mk_rain_zuer_sum, qannu_mk_rain_zuer_aut, qannu_mk_rain_zuer_win,
     
     file = "u:/RhineFlow/rhine_obs/manus/figures/riv_flow_new.Rdata")

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







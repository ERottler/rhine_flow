###

#Rhine flow observations - Analysis meteo
#Erwin Rottler, University of Potsdam
#Summer 2018

###

#data preparation----
if(do_basin_prep){
  #Load ncdf E-OBS gridded datasets
  nc_temp_file <- paste0(file_dir, "meteo/tavg/tavg.nc")
  nc_prec_file <- paste0(file_dir, "meteo/pre/pre.nc")
  nc_petr_file <- paste0("D:/nrc_user/rottler/meteo_HS/pet.nc")
  
  nc_temp <- ncdf4::nc_open(nc_temp_file)
  nc_prec <- ncdf4::nc_open(nc_prec_file)
  nc_petr <- ncdf4::nc_open(nc_petr_file)
  
  #Projections used later
  crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  epsg3035 <- sp::CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 
                      +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  #Load DEM
  # dem = raster(paste0(file_dir, "morph/dem.asc"), crs = epsg3035)
  dem = raster(paste0(base_dir, "data/basins_shp/eu_dem_500_fil.tif"))
  
  #Load basin boundaries (shapefile delineated beforehand using Q-GIS)
  my_basin <- paste0(base_dir, "data/basins_shp/", basin_sel, ".shp")
  basin <- rgdal::readOGR(dsn = my_basin)
  
  #Transform projection WGS84
  basin_84 <- sp::spTransform(basin, CRS = crswgs84)
  dem_84    <- raster::projectRaster(dem, crs = crswgs84)
  
  #Area of basin in m2
  area_m2 <- area(basin_84)
  
  #corp DEM sub-basin area
  dem_84_cro <- raster::crop(dem_84, extent(basin_84))
  dem_84_sub <- mask(dem_84_cro, basin_84)
  
  #get elevations of cells cropped dem
  dem_ele_NA <- dem_84_sub@data@values
  dem_ele <- dem_ele_NA[!is.na(dem_ele_NA)]
  dem_ele_50 <- med_na(dem_ele)
  dem_ele_mea <- alptempr::mea_na(dem_ele)
  dem_ele_25 <- stats::quantile(dem_ele, probs = 0.25)
  dem_ele_75 <- stats::quantile(dem_ele, probs = 0.75)
  
  #get lat/lon/time of .nc meteo data
  lon <- ncdf4::ncvar_get(nc_temp, varid = "lon2D")
  lat <- ncdf4::ncvar_get(nc_temp, varid = "lat2D")
  date <- as.Date(as.character(ncdf4.helpers::nc.get.time.series(nc_temp, time.dim.name = "time")))
  
  #define date start index and count for extraction from .nc file
  date_min_index <- which(date == paste0(start_year, "-01-01"))
  date_max_index <- which(date == paste0(end_year, "-12-31"))
  date_count <- date_max_index - date_min_index +1
  meteo_date <- date[date_min_index:date_max_index]
  
  #extract coordinates from basin shapefile
  basin_coords <- extractCoords(basin_84)
  
  #get min/max of lat/lon for extraction from .nc file
  lon_min <- min(basin_coords[ ,1]) - 0.1
  lon_max <- max(basin_coords[ ,1]) + 0.1
  lat_min <- min(basin_coords[ ,2]) - 0.1
  lat_max <- max(basin_coords[ ,2]) + 0.1
  
  lon_min_index <- which(abs(lon - lon_min) == min(abs(lon - lon_min)), arr.ind = T)[1,1]
  lat_min_index <- which(abs(lat - lat_max) == min(abs(lat - lat_max)), arr.ind = T)[1,2]
  
  lon_max_index <- which(abs(lon - lon_max) == min(abs(lon - lon_max)), arr.ind = T)[1,1]
  lat_max_index <- which(abs(lat - lat_min) == min(abs(lat - lat_min)), arr.ind = T)[1,2]
  
  lon_count <- abs(lon_max_index - lon_min_index) 
  lat_count <- abs(lat_max_index - lat_min_index) 
  
  #extract meteo data from .nc file using previously defined course cube extensions
  #Temperature
  system.time(
    temps_cube <- ncdf4::ncvar_get(nc_temp, start =c(lon_min_index, lat_min_index, date_min_index), 
                                   count = c(lon_count, lat_count, date_count), varid = "tavg")
  )
  
  #Precipitation
  system.time(
    precs_cube <- ncdf4::ncvar_get(nc_prec, start =c(lon_min_index, lat_min_index, date_min_index), 
                                   count = c(lon_count, lat_count, date_count), varid = "pre")
  )
  
  #Evapotranspiration
  system.time(
    petr_cube <- ncdf4::ncvar_get(nc_petr, start =c(lon_min_index, lat_min_index, date_min_index),
                                  count = c(lon_count, lat_count, date_count), varid = "pet")
  )
  
  #get lat/lon values of extracted cube
  lon2D <- lon[lon_min_index : (lon_min_index+lon_count-1), lat_min_index : (lat_min_index+lat_count-1)]
  lat2D <- lat[lon_min_index : (lon_min_index+lon_count-1), lat_min_index : (lat_min_index+lat_count-1)]
  
  #spatial grip points from lat/lon info
  grid_points_cube <-  sp::SpatialPoints(data.frame(lon = c(lon2D), lat = c(lat2D)), proj4string =  crswgs84)
  
  #get grid points inside watershed
  inside <- !is.na(sp::over(grid_points_cube, as(basin_84, "SpatialPolygons")))
  grid_points <- grid_points_cube[which(inside == T)]
  
  #get index in cube from points inside sub-basin
  lon_cube_index <- sapply(grid_points@coords[,1], get_cube_index_lon)
  lat_cube_index <- sapply(grid_points@coords[,2], get_cube_index_lat)
  
  #get time series from grid points inside sub-basin
  #temperature
  for (i in 1:length(lon_cube_index)) {
    
    temp_sing <- temps_cube[lon_cube_index[i], lat_cube_index[i], ]
    
    if(i == 1){
      temps <- temp_sing
    }else{
      temps <- cbind(temps, temp_sing)
    }
  }
  #precipitation
  for (i in 1:length(lon_cube_index)) {
    
    precs_sing <- precs_cube[lon_cube_index[i], lat_cube_index[i], ]
    
    if(i == 1){
      precs <- precs_sing
    }else{
      precs <- cbind(precs, precs_sing)
    }
  }
  #evapotranspiration
  for (i in 1:length(lon_cube_index)) {

    petr_sing <- petr_cube[lon_cube_index[i], lat_cube_index[i], ]

    if(i == 1){
      petrs <- petr_sing
    }else{
      petrs <- cbind(petrs, petr_sing)
    }
  }
  
  #sub-basin average values for rangking of weather types
  temp_basin <- apply(temps, 1, med_na)
  prec_basin <- apply(precs, 1, mea_na) 
  petr_basin <- apply(petrs, 1, med_na) 
  
  #elevation of grip points from DEM
  elevs <- raster::extract(dem_84, grid_points)
  elevs_ord <- elevs[order(elevs)]
  
  #Syntetic meta data info for grid points to use alptempr functions
  HS_amount <- length(which(elevs_ord > high_stat_thresh))
  MS_amount <- length(which(elevs_ord > middle_stat_thresh)) - HS_amount
  LS_amount <- length(elevs_ord) - MS_amount - HS_amount
  
  meta_grid <- data.frame(stn = paste0("point",1:length(grid_points)),
                          alt = elevs_ord,
                          category = c(rep("low", LS_amount), rep("middle", MS_amount), rep("high", HS_amount)),
                          data_qual = rep("quality-checked", length(grid_points)),
                          clim_reg = rep("Jura", length(grid_points)))
}

#snow_simu----
if(do_snow_sim){
  
  for(i in 1:ncol(precs)){
    
    print(paste(Sys.time(), "Processing grid point", i, "out of", ncol(precs)))
    
    #Rainfall [mm]
    rain <- precs[, i]
    
    #Temperature [캜]
    temp = temps[, i]
    
    #Raidation [W/m2]
    radi=rep(100, nrow(precs))
    
    #Humidity [%]
    humi=rep(70, nrow(precs))
    
    #Wind [m/s]
    wind=rep(1, nrow(precs))
    
    #Could Coverage [-]
    cloud=rep(0.5, nrow(precs))
    
    #Air Pressure [hPa]
    pressAir = rep(1000, nrow(precs))
    
    ### Export table
    output = cbind(temp, rain, radi, humi, pressAir, wind, cloud)
    write.table(file = paste0(base_dir, "snow_sim/snowAlone/input/input_meteo.dat"), x=output, 
                na = "-9999", sep="\t", col.names=FALSE, row.names = FALSE, quote = FALSE)
    
    system(snow_exe)
    
    swe <- read.table(paste0(base_dir, "snow_sim/snowAlone/output/swe.out"), header = T)
    
    if(i == 1){
      snows <- swe$swe
    }else{
      snows <- cbind(snows, swe$swe)
    }
    
  }
  
  #Save data as .Rdata
  save(file = paste0(base_dir, "snow_sim/data_sim/", "snows_", basin_sel, ".RData"), list="snows")
  
}

#data analysis basin----
if(do_basin_calc){
  
  if(do_snow_sim){
  
    #Average (mean) 30DMA snow water equivalent
    print(paste(Sys.time(),"Average (mean) 30DMA swe"))
    smea <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_mea(snows[, i]) * 1000 #[mm]
      
    }
    smea <- smea[, order(elevs)] #order columns by elevation of grip points
    colnames(smea) <- paste0("point", 1:length(grid_points))
    smea_mea <- apply(smea, 2, mea_na)#annual average values
    
    #Average median snow water equivalent
    print(paste(Sys.time(),"Average (median) swe"))
    smed <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_med(snows[, i])* 1000 #[mm]
      
    }
    smed <- smed[, order(elevs)] #order columns by elevation of grip points
    colnames(smed) <- paste0("point", 1:length(grid_points))
    smed_mea <- apply(smea, 2, mea_na)#annual average values
    
    #Trends 30DMA snow water equivalent
    print(paste(Sys.time(),"Trends 30DMA swe"))
    sslo <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_slo(snows[, i])*1000*10 #[mm/dec]
      
    }
    sslo <- sslo[, order(elevs)]
    colnames(sslo) <- paste0("point", 1:length(grid_points))
    sslo_mea <- apply(sslo, 2, mea_na)#annual average values
    sslo_med <- apply(sslo, 2, med_na)#annual average values
    
    #Snow probability
    print(paste(Sys.time(),"Snow probability"))
    swep <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_pro_snow_sim(snows[, i])
      
    }
    swep <- swep[, order(elevs)] #order columns by elevation of grip points
    colnames(swep) <- paste0("point", 1:length(grid_points))
    swep_mea <- apply(swep, 2, mea_na)#annual average values
    swep_med <- apply(swep, 2, med_na)#annual average values
    
    #Snow window probability trend
    print(paste(Sys.time(),"Snow window probability trend"))
    swes <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_psl_snow_sim(snows[, i])
      
    }
    swes <- swes[, order(elevs)] #order columns by elevation of grip points
    colnames(swes) <- paste0("point", 1:length(grid_points))
    swes_mea <- apply(swes, 2, mea_na)#annual average values
    swes_med <- apply(swes, 2, med_na)#annual average values
  
  }
  
  #Average 30DMA temperature
  print(paste(Sys.time(),"Average (median) 30DMA temperature"))
  tmed <- foreach(i = 1:ncol(temps), .combine = 'cbind') %dopar% {
    
    f_med(temps[, i])
    
  }
  tmed <- tmed[, order(elevs)] #order columns by elevation of grip points
  colnames(tmed) <- paste0("point", 1:length(grid_points))
  tmed_med <- apply(tmed, 2, med_na)#annual average values
  
  #Trends 30DMA temperatures
  print(paste(Sys.time(),"Trends 30DMA temperatures"))
  tslo <- foreach(i = 1:ncol(temps), .combine = 'cbind') %dopar% {
    
    f_slo(temps[, i])*10 #[째C/dec]
    
  }
  tslo <- tslo[, order(elevs)]
  colnames(tslo) <- paste0("point", 1:length(grid_points))
  tslo_med <- apply(tslo, 2, med_na)#annual average values
  
  #Average (median) 30DMA precipitation
  print(paste(Sys.time(),"Average (median) 30DMA precipitation"))
  pmed <- foreach(i = 1:ncol(precs), .combine = 'cbind') %dopar% {
    
    f_med(precs[, i])
    
  }
  pmed <- pmed[, order(elevs)] #order columns by elevation of grip points
  colnames(pmed) <- paste0("point", 1:length(grid_points))
  pmed_med <- apply(pmed, 2, med_na)#annual average values
  
  #Average (mean) 30DMA precipitation
  print(paste(Sys.time(),"Average (mean) 30DMA precipitation"))
  pmea <- foreach(i = 1:ncol(precs), .combine = 'cbind') %dopar% {
    
    f_mea(precs[, i])
    
  }
  pmea <- pmea[, order(elevs)] #order columns by elevation of grip points
  colnames(pmea) <- paste0("point", 1:length(grid_points))
  pmea_med <- apply(pmea, 2, med_na)#annual average values
  
  #Trends 30DMA precipitation
  print(paste(Sys.time(),"Trends 30DMA precipitation"))
  pslo <- foreach(i = 1:ncol(precs), .combine = 'cbind') %dopar% {
    
    f_slo(precs[, i])*10 #[째C/dec]
    
  }
  pslo <- pslo[, order(elevs)] #order columns by elevation of grip points
  colnames(pslo) <- paste0("point", 1:length(grid_points))
  pslo_med <- apply(pslo, 2, med_na)#annual average values
  
  #Average (median) 30DMA evapotranspiration
  print(paste(Sys.time(),"Average (median) 30DMA evapotranspiration"))
  emed <- foreach(i = 1:ncol(petrs), .combine = 'cbind') %dopar% {
    
    f_med(petrs[, i])
    
  }
  emed <- emed[, order(elevs)] #order columns by elevation of grip points
  colnames(emed) <- paste0("point", 1:length(grid_points))
  emed_med <- apply(emed, 2, med_na)#annual average values
  
  #Trends 30DMA evapotranspiration
  print(paste(Sys.time(),"Trends 30DMA evapotranspiration"))
  eslo <- foreach(i = 1:ncol(petrs), .combine = 'cbind') %dopar% {
    
    f_slo(petrs[, i])*10 #[째C/dec]
    
  }
  eslo <- eslo[, order(elevs)]
  colnames(eslo) <- paste0("point", 1:length(grid_points))
  eslo_med <- apply(eslo, 2, med_na)#annual average values
  
  #Fraction solid/liquid precipitation
  print(paste(Sys.time(),"Fraction solid/liquid precipitation"))
  lfra <- foreach::foreach(k = 1:ncol(precs), .combine = 'cbind') %dopar%{
    f_my_lfra(k)
  }
  lfra <- lfra[, order(elevs)] 
  colnames(lfra) <- paste0("point", 1:length(grid_points))
  lfra_med <- apply(lfra, 2, med_na)#annual average values
  lfra_mea <- apply(lfra, 2, mea_na)#annual average values
  
  #Trends window fraction solid/liquid precipitation
  print(paste(Sys.time(),"Trends window fraction solid/liquid precipitation"))
  lfrs <- foreach::foreach(k = 1:ncol(precs), .combine = 'cbind') %dopar%{
    f_my_lfrs(k)*10 #[째C/dec]
  }
  lfrs <- lfrs[, order(elevs)] 
  colnames(lfrs) <- paste0("point", 1:length(grid_points))
  lfrs_med <- apply(lfrs, 2, med_na)#annual average values
  lfrs_mea <- apply(lfrs, 2, mea_na)#annual average values
  
  #Frequency of days below zero
  print(paste(Sys.time(),"Frequency of days below zero"))
  tzer <- foreach(i = 1:ncol(temps), .combine = 'cbind') %dopar% {
    
    f_tzer(temps[, i])
    
  }
  tzer <- tzer[, order(elevs)] 
  colnames(tzer) <- paste0("point", 1:length(grid_points))
  tzer_med <- apply(tzer, 2, med_na)#annual average values
  tzer_mea <- apply(tzer, 2, mea_na)#annual average values
  
  #Trends window frequency of days below zero
  print(paste(Sys.time(),"Trends window frequency of days below zero"))
  tzes <- foreach(i = 1:ncol(temps), .combine = 'cbind') %dopar% {
    
    f_tzes(temps[, i])*10 #[째C/dec]
    
  }
  tzes <- tzes[, order(elevs)] 
  colnames(tzes) <- paste0("point", 1:length(grid_points))
  tzes_med <- apply(tzes, 2, med_na)#annual average values
  tzes_mea <- apply(tzes, 2, mea_na)#annual average values
  
}

#data_visualization----

pdf(paste0(base_dir, "figs/basin_", basin_sel, ".pdf"), width = 6.7, height = 11)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,3,5,7,9,11,13,15,17,19,
                2,4,6,8,10,12,14,16,18,20), 10, 2), widths=c(1, 1), heights=rep(1, 10))  


plot_cycl_elev(data_in = tmed, data_mk = tmed, data_in_me = tmed_med,
               data_meta = meta_grid, main_text = paste0("Temperature [째C]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = tslo, data_mk = tslo, data_in_me = tslo_med,
               data_meta = meta_grid, main_text = paste0("Temperature [째C/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = pmea, data_mk = pmea, data_in_me = pmea_med,
               data_meta = meta_grid, main_text = paste0("Precipitation [mm]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = pslo, data_mk = pslo, data_in_me = pslo_med,
               data_meta = meta_grid, main_text = paste0("Precipitation [mm/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = emed, data_mk = emed, data_in_me = emed_med,
               data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = eslo, data_mk = eslo, data_in_me = eslo_med,
               data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = tzer, data_mk = tzer, data_in_me = tzer_mea,
               data_meta = meta_grid, main_text = paste0("Temp. days above zero [%]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = tzes, data_mk = tzes, data_in_me = tzes_mea,
               data_meta = meta_grid, main_text = paste0("Temp. days above zero [%/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = lfra, data_mk = lfra, data_in_me = lfra_mea,
               data_meta = meta_grid, main_text = paste0("Liquid Frac. Prec. [%]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = lfrs, data_mk = lfrs, data_in_me = lfrs_mea,
               data_meta = meta_grid, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

if(do_snow_sim_vis){

plot_cycl_elev(data_in = smea, data_mk = smea, data_in_me = smea_mea,
               data_meta = meta_grid, main_text = paste0("SWE [mm]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.01, mk_sig_level = 0.05, add_st_num = T)
  
plot_cycl_elev(data_in = sslo, data_mk = sslo, data_in_me = sslo_med,
               data_meta = meta_grid, main_text = paste0("SWE [mm/dec]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.01, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = swep, data_mk = swep, data_in_me = swep_mea,
               data_meta = meta_grid, main_text = paste0("Snow probability [%]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.08, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = swes, data_mk = swes, data_in_me = swes_mea,
               data_meta = meta_grid, main_text = paste0("Snow window prob. [%]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.01, mk_sig_level = 0.05, add_st_num = T)
}

dev.off()

# tmed_aare <- tmed
# tmed_med_aare <- tmed_med
# tslo_aare <- tslo
# tslo_med_aare <- tslo_med
# pmea_aare <- pmea
# pmea_med_aare <- pmea_med
# pslo_aare <- pslo
# pslo_med_aare <- pslo_med
# emed_aare <- emed
# emed_med_aare <- emed_med
# eslo_aare <- eslo
# eslo_med_aare <- eslo_med
# tzer_aare <- tzer
# tzer_mea_aare <- tzer_mea
# tzes_aare <- tzes
# tzes_mea_aare <- tzes_mea
# lfra_aare <- lfra
# lfra_mea_aare <- lfra_mea
# lfrs_aare <- lfrs
# lfrs_mea_aare <- lfrs_mea
# smea_aare <- smea
# smea_mea_aare <- smea_mea
# sslo_aare <- sslo
# sslo_mea_aare <- sslo_mea
# sslo_med_aare <- sslo_med
# swep_aare <- swep
# swep_mea_aare <- swep_mea
# swep_med_aare <- swep_med
# swes_aare <- swes
# swes_mea_aare <- swes_mea
# swes_med_aare <- swes_med
# meta_grid_aare <- meta_grid





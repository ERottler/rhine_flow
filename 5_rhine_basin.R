###

#Rhine flow observations - Analysis meteo
#Erwin Rottler, University of Potsdam
#Summer 2018

###

#data preparation----

if(do_basin_prep){
  #Load ncdf E-OBS gridded datasets
  nc_temp_file <- paste0(file_dir, "meteo/tavg.nc")
  nc_prec_file <- paste0(file_dir, "meteo/pre.nc")
  nc_petr_file <- paste0(file_dir, "meteo_HS/pet.nc")
  
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
  
  #corp DEM sub-basin area
  dem_cro <- raster::crop(dem, extent(basin))
  dem_sub <- mask(dem_cro, basin)
  
  #get elevations of cells cropped dem
  dem_ele_NA <- dem_sub@data@values
  dem_ele <- dem_ele_NA[!is.na(dem_ele_NA)]
  
  #Area of basin in m2
  area_m2 <- area(basin)
  
  # plot(dem_84_sub)
  # plot(basin_84, add =T)
  # hist(dem_ele, nclass = 100)
  
  #Transform projection WGS84
  basin_84 <- sp::spTransform(basin, CRS = crswgs84)
  # dem_84    <- raster::projectRaster(dem, crs = crswgs84)

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
  lon_min <- min(basin_coords[ ,1]) - 0.15
  lon_max <- max(basin_coords[ ,1]) + 0.15
  lat_min <- min(basin_coords[ ,2]) - 0.15
  lat_max <- max(basin_coords[ ,2]) + 0.15
  
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
  grid_points_cube_84 <-  sp::SpatialPoints(data.frame(lon = c(lon2D), lat = c(lat2D)), proj4string =  crswgs84)
  grid_points_cube     <- sp::spTransform(grid_points_cube_84, CRS = crs(basin, asText = T))

  #get grid points inside watershed
  inside <- !is.na(sp::over(grid_points_cube, as(basin, "SpatialPolygons")))
  grid_points <- grid_points_cube[which(inside == T)]
  grid_points_84 <- sp::spTransform(grid_points, CRS = crs(grid_points_cube_84, asText = T))

  #get index in cube from points inside sub-basin
  cube_index_col <- sapply(grid_points_84@coords[,1], get_cube_index_col)
  cube_index_row <- sapply(grid_points_84@coords[,1], get_cube_index_row)
  
  #get time series from grid points inside sub-basin
  #temperature
  for (i in 1:length(cube_index_col)) {
    
    temp_sing <- temps_cube[cube_index_col[i], cube_index_row[i], ]
    
    if(i == 1){
      temps <- temp_sing
    }else{
      temps <- cbind(temps, temp_sing)
    }
  }
  #precipitation
  for (i in 1:length(cube_index_col)) {
    
    precs_sing <- precs_cube[cube_index_col[i], cube_index_row[i], ]
    
    if(i == 1){
      precs <- precs_sing
    }else{
      precs <- cbind(precs, precs_sing)
    }
  }
  #evapotranspiration
  for (i in 1:length(cube_index_col)) {

    petr_sing <- petr_cube[cube_index_col[i], cube_index_row[i], ]

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

  #Average mean snow water equivalent
  elevs <- foreach(i = 1:length(grid_points), .combine = 'c') %dopar% {
    
    elev_buff(i) #[m]
    
  }
  elevs_ord <- elevs[order(elevs)]
  
  # #elevation of grip points from DEM
  # elevs <- raster::extract(dem, grid_points)
  
  #Syntetic meta data info for grid points to use alptempr functions
  HS_amount <- length(which(elevs_ord > high_stat_thresh))
  MS_amount <- length(which(elevs_ord > middle_stat_thresh)) - HS_amount
  LS_amount <- length(elevs_ord) - MS_amount - HS_amount
  
  meta_grid <- data.frame(stn = paste0("point",1:length(grid_points)),
                          alt = elevs_ord,
                          category = c(rep("low", LS_amount), rep("middle", MS_amount), rep("high", HS_amount)),
                          data_qual = rep("quality-checked", length(grid_points)),
                          clim_reg = rep("Jura", length(grid_points)))
  
  
  #Radiation data for snow simulations
  
  radi_sno <- read.table(paste0(base_dir, "data/idaweb/order62894/order_62894_data.txt"), sep = ";", skip = 2, header = T)#Basel
  radi_sno$date <- as.Date(strptime(radi_sno$time, "%Y%m%d", tz="UTC"))
  radi_sno$radi <- radi_sno$gre000d0
  
  radi_mea <- dis_ana(disc = radi_sno$radi,
                      date = radi_sno$date,
                      start_year = 1981,
                      end_year = 2017,
                      window_width = window_width,
                      method_analys = "mean",
                      cover_thresh = cover_thres)
  
  radi_mea_smo <- smoothFFT(radi_mea, sd = 7)
  
  #Syntetic radiation time series based on mean values
  radi_mea_seri <- rep( c(radi_mea_smo, radi_mea_smo[365], rep(radi_mea_smo, 3)), 50)[1:nrow(temps)]
  
}


#Downscale temperature grid
#using simple lapse-rate based approach
#lapse rate in basin on daily bases used to addapt

#Lapse rate on daily basis from all data points selected
f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = 0.9)}

temps_ord <- temps[, order(elevs)]

lapse_temp <- foreach(i = 1:nrow(temps_ord), .combine = 'c') %dopar% {
  
  f_sens_slope(temps_ord[i, ]) #[°C/m]
  
}

#New grid points at 1 km resolution

down_points <- function(lon_lat_point_in){
  
  res_new <- 1000 # new desirted resolution in [m]
  
  d_00 <- lon_lat_point_in
  d_01 <- d_00 + res_new * 1
  d_02 <- d_00 + res_new * 2
  d_03 <- d_00 - res_new * 1
  d_04 <- d_00 - res_new * 1
  d_05 <- d_00
  d_05[1] <- d_05[1] + res_new * 1
  d_05[2] <- d_05[2] - res_new * 1
  d_06 <- d_00
  d_06[1] <- d_06[1] + res_new * 2
  d_06[2] <- d_06[2] - res_new * 2
  d_07 <- d_00
  d_07[1] <- d_07[1] - res_new * 1
  d_07[2] <- d_07[2] + res_new * 1
  d_08 <- d_00
  d_08[1] <- d_08[1] - res_new * 2
  d_08[2] <- d_08[2] + res_new * 2
  d_09 <- d_00
  d_09[1] <- d_09[1] + res_new * 1
  d_09[2] <- d_07[2] + res_new * 0
  d_10 <- d_00
  d_10[1] <- d_10[1] + res_new * 2
  d_10[2] <- d_10[2] + res_new * 0
  d_11 <- d_00
  d_11[1] <- d_11[1] - res_new * 1
  d_11[2] <- d_11[2] + res_new * 0
  d_12 <- d_00
  d_12[1] <- d_12[1] - res_new * 2
  d_12[2] <- d_12[2] + res_new * 0
  d_13 <- d_00
  d_13[1] <- d_13[1] + res_new * 0
  d_13[2] <- d_13[2] - res_new * 1
  d_14 <- d_00
  d_14[1] <- d_14[1] + res_new * 0
  d_14[2] <- d_14[2] - res_new * 2
  d_15 <- d_00
  d_15[1] <- d_15[1] + res_new * 0
  d_15[2] <- d_15[2] + res_new * 1
  d_16 <- d_00
  d_16[1] <- d_16[1] + res_new * 0
  d_16[2] <- d_16[2] + res_new * 2
  d_17 <- d_00
  d_17[1] <- d_17[1] + res_new * 2
  d_17[2] <- d_17[2] + res_new * 1
  d_18 <- d_00
  d_18[1] <- d_18[1] + res_new * 2
  d_18[2] <- d_18[2] - res_new * 1
  d_19 <- d_00
  d_19[1] <- d_19[1] + res_new * 1
  d_19[2] <- d_19[2] - res_new * 2
  d_20 <- d_00
  d_20[1] <- d_20[1] - res_new * 1
  d_20[2] <- d_20[2] - res_new * 2
  d_21 <- d_00
  d_21[1] <- d_21[1] - res_new * 2
  d_21[2] <- d_21[2] - res_new * 1
  d_22 <- d_00
  d_22[1] <- d_22[1] - res_new * 2
  d_22[2] <- d_22[2] + res_new * 1
  d_23 <- d_00
  d_23[1] <- d_23[1] - res_new * 1
  d_23[2] <- d_23[2] + res_new * 2
  d_24 <- d_00
  d_24[1] <- d_24[1] + res_new * 1
  d_24[2] <- d_24[2] + res_new * 2
  
  d_points_raw <- rbind(d_00, d_01, d_02, d_03, d_04, d_05, d_06, d_07, d_08, d_09, d_10, d_11, d_12,
                        d_13, d_14, d_15, d_16, d_17, d_18, d_19, d_20, d_21, d_22, d_23, d_24)
  
  
  #spatial grip points from lat/lon info
  d_points <- sp::SpatialPoints(data.frame(lon = d_points_raw[, 1], lat = d_points_raw[, 2]), proj4string =  crs(basin))
  
  return(d_points)
  
}

d_points <- down_points(grid_points@coords[1, ])

d_points_elevs <- raster::extract(dem, d_points)

d_points_elevs_dif <- d_points_elevs - elevs[1]

f_temps_laps <- function(data_in){
  
  f_laps_mod <- function(index_in){
    
    data_laps <- data_in + lapse_temp* d_points_elevs_dif[index_in]
    return(data_laps)
    
  }
  
  test <- sapply(1:length(d_points_elevs_dif), f_laps_mod)
}

temps_d <- foreach(i = 1:5, .combine = 'cbind') %dopar% {
  
  f_temps_laps(temps[, 1]) #[°C]
  
}



#snow_simu----
if(F){
  
  # for(i in 1:ncol(precs)){
  for(i in 1:ncol(precs)){  
    print(paste(Sys.time(), "Processing grid point", i, "out of", ncol(precs)))
    
    #Rainfall [mm]
    rain <- precs[, i]
    
    #Temperature [?C]
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

if(do_snow_sim){
  
  print(paste(Sys.time(),"Start: Echse snow simulations"))
  snows <- foreach(k = 1:ncol(temps), .combine = 'cbind') %dopar% {
    
    swe_sim   <- rep(NA, nrow(temps))
    sec_sim   <- rep(NA, nrow(temps))
    alb_sim   <- rep(NA, nrow(temps))
    
    swe_init <- .0
    sec_init <- .0
    alb_init <- snow_params$albedoMax
    
    swe_sim[1] <- swe_init
    sec_sim[1] <- sec_init
    alb_sim[1] <- alb_init
    
    for(i in 2:nrow(temps)){
      
      sim_out <- snowModel_inter(
        #Forcings
        precipSumMM = precs[i, k],
        shortRad = radi_mea_seri[k],
        tempAir = temps[i, k],
        pressAir = 1000,
        relHumid = 70,
        windSpeed = 1,
        cloudCoverage = 0.5,
        #Parameters
        precipSeconds = snow_params$precipSeconds,
        a0 = snow_params$a0,
        a1 = snow_params$a1,
        kSatSnow = snow_params$kSatSnow,
        densDrySnow = snow_params$densDrySnow,
        specCapRet = snow_params$specCapRet,
        emissivitySnowMin = snow_params$emissivitySnowMin,
        emissivitySnowMax = snow_params$emissivitySnowMax,
        tempAir_crit = snow_params$tempAir_crit,
        albedoMin = snow_params$albedoMin,
        albedoMax = snow_params$albedoMax,
        agingRate_tAirPos = snow_params$agingRate_tAirPos,
        agingRate_tAirNeg = snow_params$agingRate_tAirNeg,
        soilDepth = snow_params$soilDepth,
        soilDens = snow_params$soilDens,
        soilSpecHeat = snow_params$soilSpecHeat,
        weightAirTemp = snow_params$weightAirTemp,
        tempMaxOff = snow_params$tempMaxOff,
        tempAmpli = snow_params$tempAmpli,
        #States
        snowEnergyCont = sec_sim[i-1],
        snowWaterEquiv = swe_sim[i-1],
        albedo = alb_sim[i-1],
        #Outputs
        TEMP_MEAN = NA,
        TEMP_SURF = NA,
        LIQU_FRAC = NA,
        flux_R_netS = NA,
        flux_R_netL = NA,
        flux_R_soil = NA,
        flux_R_sens = NA,
        stoi_f_prec = NA,
        stoi_f_subl = NA,
        stoi_f_flow = NA,
        flux_M_prec = NA,
        flux_M_subl = NA,
        flux_M_flow = NA,
        rate_G_alb = NA
      )
      
      sec_sim[i] <- sim_out[1]
      swe_sim[i] <- sim_out[2]
      alb_sim[i] <- sim_out[3]
      
    }
    
    swe_sim
    
  }
  print(paste(Sys.time(),"End: Echse snow simulations"))
  
}


#data analysis basin----
if(do_basin_calc){
  
  if(do_snow_sim){
  
    grid_m2 <- 5000*5000 #grid 5 km resolution
    
    #Average mean snow water equivalent
    print(paste(Sys.time(),"Average mean swe"))
    smea <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_mea(snows[, i]) #[m]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    smea[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    smea <- smea[, order(elevs)] #order columns by elevation of grip points
    colnames(smea) <- paste0("point", 1:length(grid_points))
    smea_mea <- apply(smea, 2, mea_na)#annual average values
    
    #Average median snow water equivalent
    print(paste(Sys.time(),"Average median swe"))
    smed <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_med(snows[, i]) #[m]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    smed[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    smed <- smed[, order(elevs)] #order columns by elevation of grip points
    colnames(smed) <- paste0("point", 1:length(grid_points))
    smed_mea <- apply(smea, 2, mea_na)#annual average values
    
    #Average mean snow volume
    print(paste(Sys.time(),"Average mean snow volume"))
    vmea <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_mea(snows[, i] * grid_m2) #[m³]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    vmea[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    vmea <- vmea[, order(elevs)] #order columns by elevation of grip points
    colnames(vmea) <- paste0("point", 1:length(grid_points))
    vmea_mea <- apply(vmea, 2, mea_na)#annual average values
    
    #Average median snow volume
    print(paste(Sys.time(),"Average median snow volume"))
    vmed <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_med(snows[, i] * grid_m2) #[m³]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    vmed[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    vmed <- vmed[, order(elevs)] #order columns by elevation of grip points
    colnames(vmed) <- paste0("point", 1:length(grid_points))
    vmed_mea <- apply(vmed, 2, mea_na)#annual average values
    
    #Trends 30DMA snow water equivalent
    print(paste(Sys.time(),"Trends 30DMA swe"))
    sslo <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_slo(snows[, i])*1000*10 #[mm/dec]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    sslo[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    sslo <- sslo[, order(elevs)]
    colnames(sslo) <- paste0("point", 1:length(grid_points))
    sslo_mea <- apply(sslo, 2, mea_na)#annual average values
    sslo_med <- apply(sslo, 2, med_na)#annual average values
    
    #Trends 30DMA snow volume
    print(paste(Sys.time(),"Trends 30DMA snow volume"))
    vslo <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_slo(snows[, i] * grid_m2) * 10 #[m³/dec]
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    vslo[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    vslo <- vslo[, order(elevs)]
    colnames(vslo) <- paste0("point", 1:length(grid_points))
    vslo_mea <- apply(vslo, 2, mea_na)#annual average values
    vslo_med <- apply(vslo, 2, med_na)#annual average values
    
    #Snow probability
    print(paste(Sys.time(),"Snow probability"))
    swep <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_pro_snow_sim(snows[, i])
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    swep[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    swep <- swep[, order(elevs)] #order columns by elevation of grip points
    colnames(swep) <- paste0("point", 1:length(grid_points))
    swep_mea <- apply(swep, 2, mea_na)#annual average values
    swep_med <- apply(swep, 2, med_na)#annual average values
    
    #Snow window probability trend
    print(paste(Sys.time(),"Snow window probability trend"))
    swes <- foreach(i = 1:ncol(snows), .combine = 'cbind') %dopar% {
      
      f_psl_snow_sim(snows[, i])
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    swes[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    swes <- swes[, order(elevs)] #order columns by elevation of grip points
    colnames(swes) <- paste0("point", 1:length(grid_points))
    swes_mea <- apply(swes, 2, mea_na)#annual average values
    swes_med <- apply(swes, 2, med_na)#annual average values
    
    #Snow accumulation and melt water outflow
    sv_diff <- function(snow_volume_in){
      
      sv_diff <- c(NA, diff(snow_volume_in))
      
      return(sv_diff)
      
    }
    snows_dif <- apply(snows, 2, sv_diff)
    
    #Average snow volume diff
    print(paste(Sys.time(),"Average (mean) snow volume diff"))
    vdi_mea <- foreach(i = 1:ncol(snows_dif), .combine = 'cbind') %dopar% {
      
      f_mea(snows_dif[, i] * grid_m2)
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    vdi_mea[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    vdi_mea <- vdi_mea[, order(elevs)] #order columns by elevation of grip points
    colnames(vdi_mea) <- paste0("point", 1:length(grid_points))
    vdi_mea_mea <- apply(vdi_mea, 2, mea_na)#annual average values
    
    #Trends 30DMA snow volume diff
    print(paste(Sys.time(),"Trends 30DMA  snow volume diff"))
    vdi_slo <- foreach(i = 1:ncol(snows_dif), .combine = 'cbind') %dopar% {
      
      f_slo(snows_dif[, i] * grid_m2)  *10
      
    }
    temps_mea <- apply(temps, 2, mea_na)
    vdi_slo[ ,which(temps_mea < 0)] <- NA #remove 'glacier' points
    vdi_slo <- vdi_slo[, order(elevs)] #order columns by elevation of grip points
    colnames(vdi_slo) <- paste0("point", 1:length(grid_points))
    vdi_slo_mea <- apply(vdi_slo, 2, mea_na)#annual average values
    
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
    
    f_slo(temps[, i])*10 #[°C/dec]
    
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
    
    f_slo(precs[, i])*10 #[°C/dec]
    
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
    
    f_slo(petrs[, i])*10 #[°C/dec]
    
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
    f_my_lfrs(k)*10 #[°C/dec]
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
    
    f_tzes(temps[, i])*10 #[°C/dec]
    
  }
  tzes <- tzes[, order(elevs)] 
  colnames(tzes) <- paste0("point", 1:length(grid_points))
  tzes_med <- apply(tzes, 2, med_na)#annual average values
  tzes_mea <- apply(tzes, 2, mea_na)#annual average values
  
}

#elev_ranges----

min_na(meta_grid$alt)
max_na(meta_grid$alt)

my_elev_bands <- seq(250, 3200, 50)

f_elev_bands <- function(data_in, elev_bands = my_elev_bands, 
                         func_aggr = "medi", meta_dat = meta_grid){
  
  for(i in 1:(length(elev_bands) - 1)){
    # print(i)
    data_points_range <- which(meta_dat$alt > elev_bands[i] & meta_dat$alt < elev_bands[i+1])
    
    if(length(data_points_range) == 1){
      data_range_sing <- data_in[, data_points_range]
    }else{
      if(func_aggr == "mean"){
        data_range_sing <- apply(data_in[, data_points_range], 1, mea_na)
      }
      if(func_aggr == "medi"){
        data_range_sing <- apply(data_in[, data_points_range], 1, med_na)
      }
      if(func_aggr == "sum"){
        data_range_sing <- apply(data_in[, data_points_range], 1, sum_na)
      }
    }
    
    
    if(i == 1){
      
      data_range <- data_range_sing
      
    }else{
      
      data_range <- cbind(data_range, data_range_sing)
      
    }
    
  }
  
  return(data_range)
  
}

tmed_band <- f_elev_bands(data_in = tmed, func_aggr = "mean")
smea_band <- f_elev_bands(data_in = smea, func_aggr = "mean")
smed_band <- f_elev_bands(data_in = smed, func_aggr = "mean")
vmea_band <- f_elev_bands(data_in = vmea, func_aggr = "sum")
vmed_band <- f_elev_bands(data_in = vmed, func_aggr = "sum")
sslo_band <- f_elev_bands(data_in = sslo, func_aggr = "mean")
vslo_band <- f_elev_bands(data_in = vslo, func_aggr = "sum")
vdi_mea_band <- f_elev_bands(data_in = vdi_mea, func_aggr = "mean")
vdi_slo_band <- f_elev_bands(data_in = vdi_slo, func_aggr = "sum")

#Melt compensation: Trend snow volume diff over elevation
melt_comp <- apply(vdi_slo_band, 1, sum_na)

plot_test <- smea_band

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                             "yellow2","gold", "orange2", "orangered2"))(200)

n_max <- round(abs(max_na(plot_test[, ])) / (max_na(plot_test[, ]) + abs(min_na(plot_test[, ]))), digits = 2) * 200
n_min <- 200 - n_max

cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
my_col <- c(cols_min, cols_max)

my_bre <- seq(min_na(plot_test), max_na(plot_test), length.out = length(my_col)+1)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      y = my_elev_bands[-length(my_elev_bands)],
      z = plot_test, col =my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(plot_test), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Snow water equ. [m]", side = 4, line = 1.5, cex = 0.8)

box()





hist(elevs, breaks = my_elev_bands)




#Plot: melt compensation
par(mfrow = c(1, 1))
par(mar = c(1.6, 3.0, 0.6, 0.6))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(smoothFFT(melt_comp, sd = 5), type = "l", col = "black", axes = F,
     ylab = "", xlab = "", lwd = 2, ylim = rev(range(smoothFFT(melt_comp, sd = 5))))
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.5)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Trend total snow melt volume [m³/dec]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)

box(lwd = 0.7)





#data_visualization----

pdf(paste0(base_dir, "figs/basin_", basin_sel, ".pdf"), width = 6.7, height = 11)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,3,5,7,9,11,13,15,17,19,
                2,4,6,8,10,12,14,16,18,20), 10, 2), widths=c(1, 1), heights=rep(1, 10))  


plot_cycl_elev(data_in = tmed, data_mk = tmed, data_in_me = tmed_med,
               data_meta = meta_grid, main_text = paste0("Temperature [°C]"),
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = tslo, data_mk = tslo, data_in_me = tslo_med,
               data_meta = meta_grid, main_text = paste0("Temperature [°C/dec]"),
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

plot_cycl_elev(data_in = sslo, data_mk = sslo, data_in_me = sslo_mea,
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

# tmed_lahn <- tmed
# tmed_med_lahn <- tmed_med
# tslo_lahn <- tslo
# tslo_med_lahn <- tslo_med
# pmea_lahn <- pmea
# pmea_med_lahn <- pmea_med
# pslo_lahn <- pslo
# pslo_med_lahn <- pslo_med
# emed_lahn <- emed
# emed_med_lahn <- emed_med
# eslo_lahn <- eslo
# eslo_med_lahn <- eslo_med
# tzer_lahn <- tzer
# tzer_mea_lahn <- tzer_mea
# tzes_lahn <- tzes
# tzes_mea_lahn <- tzes_mea
# lfra_lahn <- lfra
# lfra_mea_lahn <- lfra_mea
# lfrs_lahn <- lfrs
# lfrs_mea_lahn <- lfrs_mea
# smea_lahn <- smea
# smea_mea_lahn <- smea_mea
# sslo_lahn <- sslo
# sslo_mea_lahn <- sslo_mea
# sslo_med_lahn <- sslo_med
# swep_lahn <- swep
# swep_mea_lahn <- swep_mea
# swep_med_lahn <- swep_med
# swes_lahn <- swes
# swes_mea_lahn <- swes_mea
# swes_med_lahn <- swes_med
# meta_grid_lahn <- meta_grid

# rm(basin, basin_84, basin_coords, crswgs84, dem, dem_84, dem_84_cro, dem_84_sub, dis,
#    emed, epsg3035, eslo, grid_points, grid_points_cube, lat, lat2D, lfra, lfrs, lon, lon2D,
#    meta_grid, my_clust, nc_petr, nc_prec, nc_temp, output, petrs, pmea, pmed, precs, pslo,
#    qmove_sing, qprob_sing, smea, smed, snows, sslo, swe, swes, temps, tmed, tslo, tzer, tzes, 
#    petr_cube, precs_cube, temps_cube)





#melt_compens_calc----

#calculate snow volume

area_m2 <- 5000*5000 #grid 5 km resolution
snovu <- snows * area_m2

#Snow simulation only points avearge temperature > 0°C
temps_mea <- apply(temps, 2, mea_na)
glacier_points <- which(temps_mea < 0)
length(glacier_points)
snovu[ , glacier_points] <- NA

snovu <- snovu[, order(elevs)] #order columns by elevation of grip points

min_na(meta_grid$alt)
max_na(meta_grid$alt)

snow_bands <- c(seq(250, 2750, 50), 3800)

#Snow volume per elevation band (sum)
for(i in 1:(length(snow_bands) - 1)){
  print(i)
  snovu_points_range <- which(meta_grid$alt > snow_bands[i] & meta_grid$alt < snow_bands[i+1])
  if(length(snovu_points_range) == 1){
    snovu_range_sing <- snovu[, snovu_points_range]
  }else{
    snovu_range_sing <- apply(snovu[, snovu_points_range], 1, sum_na)
  }

  
  if(i == 1){
    
    snovu_range <- snovu_range_sing
    
  }else{
    
    snovu_range <- cbind(snovu_range, snovu_range_sing)
    
  }
  
}

#Snow volume per elevation band (mea)
for(i in 1:(length(snow_bands) - 1)){
  print(i)
  snovu_points_range <- which(meta_grid$alt > snow_bands[i] & meta_grid$alt < snow_bands[i+1])
  if(length(snovu_points_range) == 1){
    snovu_range_sing <- snovu[, snovu_points_range]
  }else{
    snovu_range_sing <- apply(snovu[, snovu_points_range], 1, mea_na)
  }
  
  
  if(i == 1){
    
    snovu_range_mea <- snovu_range_sing
    
  }else{
    
    snovu_range_mea <- cbind(snovu_range_mea, snovu_range_sing)
    
  }
  
}

#Snow accumulation and melt water outflow
sv_diff <- function(snow_volume_in){
  
  sv_diff <- c(NA, diff(snow_volume_in))

  return(sv_diff)
  
  }

snovu_range_dif <- apply(snovu_range, 2, sv_diff)

if(FALSE){
  snovu_dif <- apply(snovu, 2, sv_diff)
  
  #Average snow volume
  print(paste(Sys.time(),"Average (median) snow volume"))
  snovu_med <- foreach(i = 1:ncol(snovu), .combine = 'cbind') %dopar% {
    
    f_med(snovu[, i])
    
  }
  
  #Trends 30DMA snow volume
  print(paste(Sys.time(),"Trends 30DMA  snow volume"))
  snovu_slo <- foreach(i = 1:ncol(snovu), .combine = 'cbind') %dopar% {
    
    f_slo(snovu[, i])*10 
    
  }
  
  #Average snow volume diff
  print(paste(Sys.time(),"Average (mean) snow volume diff"))
  snovu_dif_mea <- foreach(i = 1:ncol(snovu_dif), .combine = 'cbind') %dopar% {
    
    f_mea(snovu_dif[, i])
    
  }
  
  #Sum snow volume diff
  print(paste(Sys.time(),"Sum snow volume diff"))
  snovu_dif_sum <- foreach(i = 1:ncol(snovu_dif), .combine = 'cbind') %dopar% {
    
    f_sum(snovu_dif[, i])
    
  }
  
  #Trends 30DMA snow volume diff
  print(paste(Sys.time(),"Trends 30DMA snow volume diff"))
  snovu_dif_slo <- foreach(i = 1:ncol(snovu_dif), .combine = 'cbind') %dopar% {
    
    f_slo(snovu_dif[, i])*10 
    
  }
  
  
}

# Total snow volume for different time frames
# start_year <- 1961 ; end_year <- 2010 #total time frame
# start_year <- 1961 ; end_year <- 1985 #total time frame
# start_year <- 1986 ; end_year <- 2010 #total time frame

#Average snow volume (total sum per elevation band)
print(paste(Sys.time(),"Average (median) snow volume"))
snovu_med <- foreach(i = 1:ncol(snovu_range), .combine = 'cbind') %dopar% {

  f_med(snovu_range[, i])

}
colnames(snovu_med) <- snow_bands[-length(snow_bands)]

#Average snow volume (mean per elevation band)
print(paste(Sys.time(),"Average (mean) snow volume"))
snovu_mea_med <- foreach(i = 1:ncol(snovu_range_mea), .combine = 'cbind') %dopar% {
  
  f_med(snovu_range_mea[, i])
  
}
colnames(snovu_mea_med) <- snow_bands[-length(snow_bands)]

#Trends 30DMA snow volume
print(paste(Sys.time(),"Trends 30DMA  snow volume"))
snovu_slo <- foreach(i = 1:ncol(snovu_range), .combine = 'cbind') %dopar% {

  f_slo(snovu_range[, i])*10

}
colnames(snovu_slo) <- snow_bands[-length(snow_bands)]

#Average snow volume diff
print(paste(Sys.time(),"Average (mean) snow volume diff"))
snovu_dif_mea <- foreach(i = 1:ncol(snovu_range_dif), .combine = 'cbind') %dopar% {

  f_mea(snovu_range_dif[, i])

}
colnames(snovu_dif_mea) <- snow_bands[-length(snow_bands)]

#Sum snow volume diff
print(paste(Sys.time(),"Sum snow volume diff"))
snovu_dif_sum <- foreach(i = 1:ncol(snovu_range_dif), .combine = 'cbind') %dopar% {

  f_sum(snovu_range_dif[, i])

}
colnames(snovu_dif_sum) <- snow_bands[-length(snow_bands)]

#Trends 30DMA snow volume diff
print(paste(Sys.time(),"Trends 30DMA  snow volume diff"))
snovu_dif_slo <- foreach(i = 1:ncol(snovu_range_dif), .combine = 'cbind') %dopar% {

  f_slo(snovu_range_dif[, i])*10

}
colnames(snovu_dif_slo) <- snow_bands[-length(snow_bands)]

# #reverse trend sing accumulation phase
# snovu_dif_slo[which(snovu_dif_sum > 0)] <- snovu_dif_slo[which(snovu_dif_sum > 0)] * -1

#reverse trend sing to show effect on discharge
# snovu_dif_slo <- snovu_dif_slo * -1

# #Trends during accumulation phase to NA
# snovu_dif_slo[which(snovu_dif_sum < 0)] <- NA

#Upward melt compensation: Trend snow volume diff over elevation
umc <- apply(snovu_dif_slo, 1, sum_na)

#Annual cycle average snow volume in catchment
snovu_mea_total <- apply(snovu_mea_med, 1, sum_na)

plot(snovu_mea_total, type = "l")

#Annual cycle total snow volume in catchment
snovu_total <- apply(snovu_med, 1, sum_na)
# snovu_total_all <- snovu_total ; snovu_total_ear <- snovu_total ; snovu_total_lat <- snovu_total

#Annual cycle average total build up/melt
snovu_dif_mea_sum <- apply(snovu_dif_mea, 1, sum_na)


#melt_compens_vis----

#Plot: Average snow depth in elevations bands

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

# my_col <- colorRampPalette(c("grey80", viridis(9, direction = 1)[c(1,2,3,4)], "cadetblue3",
#                              "yellow2","gold", "orange2", "orangered2", "orangered4"))(200)
# 
# my_col <- colorRampPalette(c("white", "yellow", "orange2", "orangered2", "orangered4"))(200)
# my_col <- colorRampPalette(c("white", "cadetblue3", viridis(9, direction = 1)[c(4,2,1)]))(200)
my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                             "yellow2","gold", "orange2", "orangered2"))(200)

my_bre <- seq(min_na(snovu_mea_med/area_m2), max_na(snovu_mea_med/area_m2), length.out = length(my_col)+1)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      # y = 1:ncol(snovu_med),
      y = snow_bands[-length(snow_bands)],
      z = snovu_mea_med/area_m2, col =my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(snovu_mea_med/area_m2), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Snow water equ. [m]", side = 4, line = 1.5, cex = 0.8)

box()




#Plot: Total snow volume elevations bands

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

my_col <- colorRampPalette(c("grey80", viridis(9, direction = 1)[c(1,2,3,4)], "cadetblue3",
                             "yellow2","gold", "orange2", "orangered2", "orangered4"))(200)

my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                             "yellow2","gold", "orange2", "orangered2"))(200)

my_bre <- seq(min_na(snovu_med), max_na(snovu_med), length.out = length(my_col)+1)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      # y = 1:ncol(snovu_med),
      y = snow_bands[-length(snow_bands)],
      z = snovu_med, col =my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(snovu_med), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Total volume swe [m³]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Trend total Snow volume in elevations bands

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

n_max <- round(abs(max_na(snovu_slo[, ])) / (max_na(snovu_slo[, ]) + abs(min_na(snovu_slo[, ]))), digits = 2) * 200
n_min <- 200 - n_max

cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
cols_max <- colorRampPalette(c("grey90", "yellow2", "gold"))(n_max)
cols_0 <- c(cols_min, cols_max)

my_bre <- seq(min_na(snovu_slo), max_na(snovu_slo), length.out = length(cols_0)+1)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      y = snow_bands[-length(snow_bands)],
      z = snovu_slo, col = cols_0, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(snovu_slo), col = cols_0, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Total volume swe [m³]", side = 4, line = 1.5, cex = 0.8)

box()




#Plot: Average total snow build up/melt between consecutive days

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

# my_col <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90",
#                              "yellow2","gold", "orange2", "orangered2", "orangered4"))(200)
n_max <- round(abs(max_na(snovu_dif_mea[, ])) / (max_na(snovu_dif_mea[, ]) + abs(min_na(snovu_dif_mea[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
my_col <- c(cols_min, cols_max)

my_bre <- seq(min_na(snovu_dif_mea), max_na(snovu_dif_mea), length.out = 201)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      # y = 1:length(elevs_ord),
      y = snow_bands[-length(snow_bands)],
      z = snovu_dif_mea, col =my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(snovu_dif_mea), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Snow volume diff. [m³]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Temporary storage, redistribution

par(mfrow = c(1, 1))
par(mar = c(1.8, 3.5, 1.6, 0.5))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(smoothFFT(snovu_dif_mea_sum, sd = 5), type = "l", col = "red3", axes = F,
     ylab = "", xlab = "", main = "Temporal redistribution water in catchment", ylim = rev(range(snovu_dif_mea_sum)))
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.5)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
axis(2)
mtext("Water volume [m³]", side = 2, line = 2.5)
box(lwd = 0.7)



#Plot: Snow volume diff elevations bands (trend)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

# my_col <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90",
#                              "yellow2","gold", "orange2", "orangered2", "orangered4"))(200)
n_max <- round(abs(max_na(snovu_dif_slo[, ])) / (max_na(snovu_dif_slo[, ]) + abs(min_na(snovu_dif_slo[, ]))), digits = 2) * 200
n_min <- 200 - n_max
cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
cols_max <- colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
my_col <- c(cols_min, cols_max)

my_bre <- seq(min_na(snovu_dif_slo), max_na(snovu_dif_slo), length.out = length(my_col)+1)

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      y = snow_bands[-length(snow_bands)],
      z = snovu_dif_slo, col = my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(snovu_dif_slo), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Trend snow volume diff. [m³/dec]", side = 4, line = 1.5, cex = 0.8)

box()


#Plot: Upward melt compensation

par(mfrow = c(1, 1))
par(mar = c(1.6, 3.0, 0.6, 0.6))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(smoothFFT(umc, sd = 5), type = "l", col = "black", axes = F,
     ylab = "", xlab = "", lwd = 2, ylim = rev(range(smoothFFT(umc, sd = 5))))
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.5)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Trend total snow melt volume [m³/dec]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)

box(lwd = 0.7)



#Plot: Elevation distribution in basin

par(mfrow = c(1, 1))
par(mar = c(2.5, 2.5, 0.6, 0.6))

hist(dem_ele, nclass = 50, col = "grey50", main = "", axes = F, xlab = "", ylab = "")
axis(1, mgp=c(3, 0.15, 0), tck = -0.01)
axis(2, mgp=c(3, 0.15, 0), tck = -0.01)
mtext("Frequency", side = 2, line = 1.4)
mtext("Elevation", side = 1, line = 1.4)
box(lwd = 0.7)


#u2mc_data----


start_year   <- 1981
end_year     <- 2017
window_width <- 30
cover_thres  <- 32/37

load("u:/RhineFlow/Elevation/Data/hto000d0.RData") ; snow_data <- out_data ; rm(out_data)

stat_meta <- read.table("u:/RhineFlow/Elevation/Data/rawData/IDAweb/stationMeta.csv", sep=",", header=T)

#Average (mean) snow depth
f_mea_sd <- function(data_in){dis_ana(disc = data_in,
                                      date = snow_data$date,
                                      start_year = start_year,
                                      end_year = end_year,
                                      window_width = window_width,
                                      method_analys = "mean",
                                      cover_thresh = cover_thres
)}
print(paste(Sys.time(),"Average (mean) snow depth"))
sdata_mea <- foreach(i = 2:ncol(snow_data), .combine = 'cbind') %dopar% {
  
  f_mea_sd(snow_data[, i])
  
}
colnames(sdata_mea) <- colnames(snow_data)[-1]
sdata_mea <- as.data.frame(stat_coverage(sdata_mea)) #remove stations which had no sufficient data coverage
sdata_mea_an <- apply(sdata_mea[,], 2, mea_na) #mean average instead of median

plot_cycl_elev(data_in = sdata_mea, data_mk = sdata_mea, data_in_me = sdata_mea_an,
               data_meta = stat_meta, main_text = "f) Snow depth [cm] ",
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               mk_sig_level = 0.05, add_st_num = T)

stat_meta_snow <- stat_meta[which(stat_meta$stn %in% colnames(sdata_mea)), ]
stat_meta_snow <- stat_meta_snow[order(stat_meta_snow$alt), ]

#Order data by altitude; add stations that are not in data file
sdata_mea <- order_add_stat(sdata_mea, meta_stat = stat_meta_snow)

#Only keep station with sufficient data coverage in data
snow_data_sel <- snow_data[, which(colnames(snow_data) %in% colnames(sdata_mea))]
snow_data_sel <- order_add_stat(snow_data_sel, meta_stat = stat_meta_snow)

min_na(stat_meta_snow$alt)
max_na(stat_meta_snow$alt)

sdata_bands <- c(seq(250, 2050, 200), 3000)

#Snow volume per elevation band
for(i in 1:(length(sdata_bands) - 1)){
  print(i)
  sdata_stats_range <- which(stat_meta_snow$alt > sdata_bands[i] & stat_meta_snow$alt < sdata_bands[i+1])
  if(length(sdata_stats_range) == 1){
    sdata_range_sing <- snow_data_sel[, sdata_stats_range]
  }else{
    sdata_range_sing <- apply(snow_data_sel[, sdata_stats_range], 1, mea_na)
  }
  
  
  if(i == 1){
    
    sdata_range <- sdata_range_sing
    
  }else{
    
    sdata_range <- cbind(sdata_range, sdata_range_sing)
    
  }
  
}


print(paste(Sys.time(),"Average (mean) snow depth elevation band"))
sdata_range_mea <- foreach(i = 1:ncol(sdata_range), .combine = 'cbind') %dopar% {
  
  f_mea_sd(sdata_range[, i])
  
}
colnames(sdata_range_mea) <- sdata_bands[-length(sdata_bands)]


#Plot: Snow volume elevations bands (median)

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

my_col <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90",
                             "yellow2","gold", "orange2", "orangered2", "orangered4"))(200)
# my_col <- c(my_col, rep(my_col[length(my_col)], 125))
my_bre <- seq(min_na(sdata_range_mea), max_na(sdata_range_mea), length.out = 201)
my_bre <- seq(min_na(sdata_range_mea), 50, length.out = 201)
par(mar = c(1.6, 3.5, 1.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      y = sdata_bands[-length(sdata_bands)],
      z = sdata_range_mea, col = my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F, main = paste("Snow volume (median)"))
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
axis(2)
box()

par(mar = c(1.6,0.5,1.6,1.7))

image_scale(as.matrix(snovu_slo), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)

box()










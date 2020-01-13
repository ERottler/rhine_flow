###

#GRDC discharge data - Map
#Erwin Rottler, University of Potsdam

###

pacman::p_load(tidyverse, data.table, leaflet, htmlwidgets)

file_names <- list.files(path = dir_grdc, pattern = "*.Cmd", full.names = F)
file_paths <- list.files(path = dir_grdc, pattern = "*.Cmd", full.names = T)

#read_meta----

for(i in 1:length(file_paths)){
  
  print(i)
  
  #get rows with meta information
  meta_rows <- read_lines(file_paths[i], n_max = 32)
  meta_rows <- iconv(meta_rows, "UTF-8", "ASCII", "")
  #Name
  row_name <- meta_rows[11]
  sta_name <- substr(row_name, 26, nchar(row_name))
  #Longitude
  row_long <- meta_rows[14]
  sta_long <- substr(row_long, 24, nchar(row_long))
  #Latitude
  row_lati <- meta_rows[13]
  sta_lati <- substr(row_lati, 24, nchar(row_lati))
  #Altitude
  row_alti <- meta_rows[16]
  sta_alti <- substr(row_alti, 28, nchar(row_alti))
  #Start/End time series 
  row_seri <- meta_rows[24]
  sta_seri <- substr(row_seri, 26, nchar(row_seri)-13)
  end_seri <- substr(row_seri, 36, nchar(row_seri)-3)
  #Catchment area
  row_catc <- meta_rows[15]
  sta_catc <- substr(row_catc, 30, nchar(row_catc))
  #Number of years
  row_year <- meta_rows[25]
  sta_year <- substr(row_year, 26, nchar(row_year))
  
  meta_sing <- c(sta_name, sta_lati, sta_long, sta_alti, sta_catc, sta_seri, end_seri, sta_year, file_names[i])
  
  
  if(i == 1){
    
    grdc_meta <- meta_sing
    
  }else{
    
    grdc_meta <- rbind(grdc_meta, meta_sing)
    
  }
  
  
}

colnames(grdc_meta) <- c("name", "latitude", "longitude", "altitude", "cath_area", "start_series", "end_series", "n_years", "file")
rownames(grdc_meta) <- NULL
grdc_meta <- as.data.frame(grdc_meta)
grdc_meta$latitude   <- as.numeric(levels(grdc_meta$latitude))[grdc_meta$latitude]
grdc_meta$longitude  <- as.numeric(levels(grdc_meta$longitude))[grdc_meta$longitude]
grdc_meta$altitude   <- as.numeric(levels(grdc_meta$altitude))[grdc_meta$altitude]
grdc_meta$cath_area  <- as.numeric(levels(grdc_meta$cath_area))[grdc_meta$cath_area]
grdc_meta$start_series  <- as.numeric(levels(grdc_meta$start_series))[grdc_meta$start_series]
grdc_meta$end_series  <- as.numeric(levels(grdc_meta$end_series))[grdc_meta$end_series]
grdc_meta$n_years  <- as.numeric(levels(grdc_meta$n_years))[grdc_meta$n_years]

#map----


pal <- colorNumeric(palette = "Reds", domain = grdc_meta$n_years)

pal <- "darkblue"

map <- leaflet(grdc_meta) %>%
  # Base groups
  #addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.TerrainBackground, group = "TerrainBackground") %>%
  addProviderTiles(providers$Esri.WorldImagery,        group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap,      group = "NatGeoWorldMap") %>%
  addProviderTiles(providers$Esri.WorldTopoMap,      group = "Esri.WorldTopoMap") %>%
  # Overlay groups
  addCircleMarkers(lng = grdc_meta$longitude, lat = grdc_meta$latitude, popup = ~paste0(grdc_meta$name, 
                                                                                        # " (altitude: ", grdc_meta$altitude,
                                                                                        ", start: ", grdc_meta$start_series,
                                                                                        ", end: ", grdc_meta$end_series,
                                                                                        # ", n_years: ", grdc_meta$n_years,
                                                                                        # ", area: ", grdc_meta$cath_area, " km2",
                                                                                        ", file: ", grdc_meta$file),
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "bottom"), stroke = F, group = "Stations", 
                   color = pal, fillOpacity = 0.7) %>%
  # color = ~pal(n_years), fillOpacity = 0.7) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("TerrainBackground", "WorldImagery", "NatGeoWorldMap","Esri.WorldTopoMap"),
    overlayGroups = c("Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

saveWidget(map, file=paste0("u:/RhineFlow/rhine_obs/grdc_map.html"))







#read_data----

grdc_data <- read_grdc(paste0(dir_grdc, "6935051_Q_Day.Cmd.txt"))

# 6343100_Q_Day.Cmd.txt Wasserburg (Inn)
# 6935051_Q_Day.Cmd.txt Basel Rheinhalle (Rhine)
# 6335060_Q_Day.Cmd.txt Koeln (Rhine)
# 6335500_Q_Day.Cmd.txt Würzburg


###

#Rhine river basin flow changes: Shiny app
#Erwin Rottler, Universtiy of Potsdam

###

#settings####

# devtools::install_github('ERottler/alptempr')
library(shiny)
library(leaflet)
library(sp)
library(raster)
library(viridis)
library(shinythemes)
library(alptempr)
library(zoo)
library(emdbook)

base_dir <- "u:/RhineFlow/rhine_obs/R/rhine_flow/RhineApp/"
# base_dir  <- "/srv/shiny-server/RhineApp/"
bg_color <- "gray90"

#Load basin boundaries (shapefile delineated beforehand using GIS)
sh_alpr_path <- paste0(base_dir, "data/", "alp_rhine", ".shp")
sh_reus_path <- paste0(base_dir, "data/", "reuss", ".shp")
sh_aare_path <- paste0(base_dir, "data/", "aare", ".shp")
sh_main_path <- paste0(base_dir, "data/", "main", ".shp")
sh_lobi_path <- paste0(base_dir, "data/", "lobith", ".shp")
sh_lahn_path <- paste0(base_dir, "data/", "lahn", ".shp")
sh_nahe_path <- paste0(base_dir, "data/", "nahe", ".shp")
sh_mose_path <- paste0(base_dir, "data/", "moselle", ".shp")
sh_neck_path <- paste0(base_dir, "data/", "neckar", ".shp")
sh_griv_path <- paste0(base_dir, "data/", "gauges_river", ".shp")

sh_alpr <- rgdal::readOGR(dsn = sh_alpr_path)
sh_reus <- rgdal::readOGR(dsn = sh_reus_path)
sh_aare <- rgdal::readOGR(dsn = sh_aare_path)
sh_main <- rgdal::readOGR(dsn = sh_main_path)
sh_lobi <- rgdal::readOGR(dsn = sh_lobi_path)
sh_lahn <- rgdal::readOGR(dsn = sh_lahn_path)
sh_nahe <- rgdal::readOGR(dsn = sh_nahe_path)
sh_mose <- rgdal::readOGR(dsn = sh_mose_path)
sh_neck <- rgdal::readOGR(dsn = sh_neck_path)
sh_griv <- rgdal::readOGR(dsn = sh_griv_path)

crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

sh_alpr <- sp::spTransform(sh_alpr, CRS = crswgs84)
sh_reus <- sp::spTransform(sh_reus, CRS = crswgs84)
sh_aare <- sp::spTransform(sh_aare, CRS = crswgs84)
sh_main <- sp::spTransform(sh_main, CRS = crswgs84)
sh_lobi <- sp::spTransform(sh_lobi, CRS = crswgs84)
sh_lahn <- sp::spTransform(sh_lahn, CRS = crswgs84)
sh_nahe <- sp::spTransform(sh_nahe, CRS = crswgs84)
sh_mose <- sp::spTransform(sh_mose, CRS = crswgs84)
sh_neck <- sp::spTransform(sh_neck, CRS = crswgs84)
sh_griv <- sp::spTransform(sh_griv, CRS = crswgs84)

# load(paste0(base_dir, "data/rhine_flow_app_old.RData"))
# base_dir <- "u:/RhineFlow/rhine_obs/R/rhine_flow/RhineApp/"
load(paste0(base_dir, "data/rhine_flow_app.RData"))

#user_interface####
ui <- fluidPage(theme = shinytheme("superhero"),
  # place the contents inside a box
  fluidPage(
    width = 12,
    hr(),
    tags$h3("Hydro-meteorologische Datenanalyse der Veränderungen im Abfluss des Rheins und seiner Nebenflüsse"),
    tags$h5("Erwin Rottler, Till Francke, Gerd Bürger and Axel Bronstert"),
    tags$h6("Institut für Umweltwissenschaften und Geographie, Universität Potsdam"),
    hr(),
    
    column(
      width = 6,
      wellPanel(
        checkboxInput(inputId = "condi_map", label = strong("Interaktive Karte: Auswahl Pegelstation/Teileinzugsgebiet"), value = T),
        conditionalPanel(
          condition = "input.condi_map == true",
          leaflet::leafletOutput(outputId = "map", height = 350)
        )
      ),
      
      wellPanel(
      checkboxInput(inputId = "condi_clim", label = strong("Klima")),
      conditionalPanel(
          condition = "input.condi_clim == true",
          textOutput(outputId = "basin_select_clim"),
          plotOutput(outputId = "plot_clim", height = 350)
        )#conditional Panel
      ),#wellPanel
      
      wellPanel(
        checkboxInput(inputId = "condi_smea", label = strong("Schnee")),
        conditionalPanel(
          condition = "input.condi_smea == true",
          #textOutput(outputId = "basin_select_clim")
          plotOutput(outputId = "plot_smea", height = 350)
        )#conditional Panel
      )#wellPanel
      
    ),#column left
    
    # separate the box by a column
    column(
      width = 6,
      wellPanel(
        checkboxInput(inputId = "condi_disc", label = strong("Abfluss"), value = T),
        conditionalPanel(
          condition = "input.condi_disc == true",
          plotOutput(outputId = "plot_disc", height = 350)
        )#conditional Panel
      ),
      
      wellPanel(
        checkboxInput(inputId = "condi_clic", label = strong("Klimaänderung")),
        conditionalPanel(
          condition = "input.condi_clic == true",
          textOutput(outputId = "basin_select_clic"),
          plotOutput(outputId = "plot_clic", height = 350)
        )#conditional Panel
      ),#wellPanel
      
      wellPanel(
        checkboxInput(inputId = "condi_sslo", label = strong("Schneeänderung")),
        conditionalPanel(
          condition = "input.condi_sslo == true",
          #textOutput(outputId = "basin_select_clim")
          plotOutput(outputId = "plot_sslo", height = 350)
        )#conditional Panel
      )#wellPanel
      
    ),
    column(
      width = 12,
    wellPanel(
      tags$p("Sollten Sie Fragen oder Anregungen haben, zögern Sie bitte nicht uns zu kontaktieren: rottler(at)uni-potsdam.de"),
      tags$p("Gefördert durch die Deutsche Forschungsgemeinschaft (DFG) im Rahmen des Graduiertenkollegs 'NatRiskChange' (www.natriskchange.de).")
      )
    )
  ) # end of the box
  

) # end of fluid page


#server####
server <- function( input, output, session ){

rhine_map <- shiny::reactive({
    leaflet() %>%
    # Base groups
    #addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
    addProviderTiles(providers$Esri.WorldImagery,        group = "World Imagery") %>%
    # addProviderTiles(providers$Esri.WorldTerrain,        group = "World Terrain") %>%
    # addProviderTiles(providers$Esri.WorldShadedRelief,   group = "World Shaded") %>%
    # Overlay groups
    addPolygons(data = sh_lobi, layerId = "lobith", fillOpacity = 0)%>%
    addPolygons(data = sh_alpr, layerId = "alp_rhine", fillOpacity = 0.4, group = "click.list",
                label = "Alpenrhein bis Pegel Diepoldsau", color = "red",
                # popup = "Alpine Rhine measured at gauge Diepoldsau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_reus, layerId = "reuss", fillOpacity = 0.4, group = "click.list",
                label = "Reuss bis Pegel Mellingen", color = "red",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_aare, layerId = "aare", fillOpacity = 0.4, group = "click.list",
                label = "Aare bis Pegel Brugg", color = "red",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_mose, layerId = "moselle",fillOpacity = 0.4, group = "click.list",
                label = "Mosel bis Pegel Cochem",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_neck, layerId = "neckar", fillOpacity = 0.4, group = "click.list",
                label = "Neckar bis Pegel Rockenau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_nahe, layerId = "nahe", fillOpacity = 0.4, group = "click.list",
                label = "Nahe bis Pegel Grolsheim",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_lahn, layerId = "lahn", fillOpacity = 0.4, group = "click.list",
                label = "Lahn bis Pegel Kalkofen",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_main, layerId = "main", fillOpacity = 0.4, group = "click.list",
                label = "Main bis Pegel Frankfurt",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addCircleMarkers(data = sh_griv, label = ~name, color = "black", stroke = F, fillOpacity = 0.4,
                     labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20))) %>%
    addCircleMarkers(lng = 10.50, lat = 47.00, label = "nival", color = "red", stroke = F, fillOpacity = 0.0,
                     labelOptions = labelOptions(noHide = T, direction = "right", offset = c(0, 0),
                                                 style = list("color" = "red",
                                                              "font-size" = "14px"))) %>%
    addCircleMarkers(lng = 5.20, lat = 49.80, label = "pluvial", color = "red", stroke = F, fillOpacity = 0.0,
                     labelOptions = labelOptions(noHide = T, direction = "left", offset = c(0, 0),
                                                 style = list("color" = "blue",
                                                              "font-size" = "14px"))) %>%
    # addCircleMarkers(data = gauges_basins, label = ~name,
    #                  stroke = F, fillOpacity = 0.4) %>%
    # addPolylines(data = river, fillOpacity = 0.9, stroke = T, fillColor = "blue3") %>%
    # Layers control
    addLayersControl(
      baseGroups = c("Terrain Background", "World Imagery"),
      options = layersControlOptions(collapsed = T)
    )
})

output$map <- renderLeaflet({
  
  rhine_map()
  # plot(1:10, 1:10)
  
}) # end of leaflet::renderLeaflet({})

#Start plots
output$basin_select_clim <- renderText({"Alpenrhein bis Pegel Diepoldsau"})
output$basin_select_clic <- renderText({"Alpenrhein bis Pegel Diepoldsau"})
qvalu_long <- qvalu_long_diep
qvslo_long <- qvslo_long_diep

my_no_col <- F
meta_grid_bands <- meta_grid_bands_diep
my_elev_bands <- my_elev_bands_diep
tmed_band <- tmed_band_diep
tmed_band_med <- tmed_band_med_diep
tslo_band <- tslo_band_diep
tslo_band_med <- tslo_band_med_diep
pmea_band <- pmea_band_diep
pmea_band_med <- pmea_band_med_diep
pslo_band <- pslo_band_diep
pslo_band_med <- pslo_band_med_diep
emed_band <- emed_band_diep
emed_band_med <- emed_band_med_diep
eslo_band <- eslo_band_diep
eslo_band_med <- eslo_band_med_diep

smea_band <- smea_band_diep
vmea_band <- vmea_band_diep
vdif_band <- vdif_band_diep
vdis_band <- vdis_band_diep

f_plot_disc <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(rep(c(1,3), 7),
                  2,4),
                2, 8), widths=c(), heights=c(1,1))
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  ytiks      <- seq(10, 90, by =  10)
  ylabs      <- seq(90, 10, by = -10) 
  
  cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                              "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)
  
  max_break <- max_na(qvalu_long)
  min_break <- min_na(qvalu_long)
  qua_break <- quantile(qvalu_long, probs = 0.7, type = 8, na.rm = T)
  
  breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
  breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
  breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1
  
  breaks_qvalu <- c(breaks_1, breaks_2)
  
  y <- 1:ncol(qvalu_long)
  x <- 1:365
  
  par(mar = c(1.6, 3, 0.6, 0))
  
  image(x, y, as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
        xlab = "", axes = F)
  
  axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
  mtext("Quantil", 2, 1.5, cex = 0.8)
  
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Abfluss [m³/sec]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
  
  #Plot: Trend moving quantile
  
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  ytiks      <- seq(10, 90, by =  10)
  ylabs      <- seq(90, 10, by = -10) 
  
  n_max <- round(abs(alptempr::max_na(qvslo_long)) / (alptempr::max_na(qvslo_long) + abs(alptempr::min_na(qvslo_long))), digits = 2) * 200
  n_min <- 200 - n_max
  cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
  cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
  cols_qvslo <- c(cols_min, cols_max)
  
  breaks_qvslo <-  seq(alptempr::min_na(qvslo_long), alptempr::max_na(qvslo_long), length.out = length(cols_qvslo) +1)
  
  y <- 1:ncol(qvslo_long)
  x <- 1:365
  
  par(mar = c(1.6, 3, 0.6, 0))
  
  image(x, y, as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
        xlab = "", axes = F)
  
  axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
  mtext("Quantil", 2, 1.5, cex = 0.8)
  
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  alptempr::image_scale(as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Trend Fenster-Quantil [m³/sec/10a]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
}
f_plot_clim <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(1,3,5,
                  2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
  
  plot_cycl_elev(data_in = tmed_band, data_mk = tmed_band, data_in_me = tmed_band_med,
                 data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  
  plot_cycl_elev(data_in = pmea_band, data_mk = pmea_band, data_in_me = pmea_band_med,
                 data_meta = meta_grid_bands, main_text= paste0("Niederschlag [mm]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  
  plot_cycl_elev(data_in = emed_band, data_mk = emed_band, data_in_me = emed_band_med,
                 data_meta = meta_grid_bands, main_text= paste0("Evapotranspiration [mm]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  
}
f_plot_clic <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(1,3,5,
                  2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
  
  plot_cycl_elev(data_in = tslo_band, data_mk = tslo_band, data_in_me = tslo_band_med,
                 data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C/10a]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  
  plot_cycl_elev(data_in = pslo_band, data_mk = pslo_band, data_in_me = pslo_band_med,
                 data_meta = meta_grid_bands, main_text = paste0("Niederschlag [mm/10a]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  
  plot_cycl_elev(data_in = eslo_band, data_mk = eslo_band, data_in_me = eslo_band_med,
                 data_meta = meta_grid_bands, main_text = paste0("Evapotranspiration [mm/10a]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
}
f_plot_smea <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(rep(c(1,3), 7),
                  2,4),
                2, 8), widths=c(), heights=c(1,1))
  
  #Snow depth elevation band
  par(mar = c(1.6, 3, 0.6, 0))
  
  x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  
  my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                               "yellow2","gold", "orange2", "orangered2"))(200)
  
  n_max <- round(abs(max_na(smea_band[, ])) / (max_na(smea_band[, ]) + abs(min_na(smea_band[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  
  cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
  cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
  # my_col <- c(cols_min, cols_max)
  
  my_bre <- seq(min_na(smea_band), max_na(smea_band), length.out = length(my_col)+1)
  
  image(x = 1:365,
        y = my_elev_bands[-length(my_elev_bands)],
        z = smea_band, col =my_col, breaks = my_bre,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.06)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
  mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
  axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  image_scale(as.matrix(smea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Schnee-Wasser-Equ. [m]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
  #Snow volume elevation band
  par(mar = c(1.6, 3, 0.6, 0))
  
  x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  
  my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                               "yellow2","gold", "orange2", "orangered2"))(200)
  
  n_max <- round(abs(max_na(vmea_band[, ])) / (max_na(vmea_band[, ]) + abs(min_na(vmea_band[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  
  cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
  cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
  # my_col <- c(cols_min, cols_max)
  
  my_bre <- seq(min_na(vmea_band), max_na(vmea_band), length.out = length(my_col)+1)
  
  image(x = 1:365,
        y = my_elev_bands[-length(my_elev_bands)],
        z = vmea_band, col =my_col, breaks = my_bre,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.06)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
  mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
  axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  image_scale(as.matrix(vmea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Schnee volumen [m³]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
  
}
f_plot_sslo <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(rep(c(1,3), 7),
                  2,4),
                2, 8), widths=c(), heights=c(1,1))
  
  
  #Snow difference mean
  par(mar = c(1.6, 3, 0.6, 0))
  
  x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  
  my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                               "yellow2","gold", "orange2", "orangered2"))(200)
  
  n_max <- round(abs(max_na(vdif_band[, ])) / (max_na(vdif_band[, ]) + abs(min_na(vdif_band[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  
  cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
  cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
  my_col <- c(cols_min, cols_max)
  
  my_bre <- seq(min_na(vdif_band), max_na(vdif_band), length.out = length(my_col)+1)
  
  image(x = 1:365,
        y = my_elev_bands[-length(my_elev_bands)],
        z = vdif_band, col =my_col, breaks = my_bre,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.06)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
  mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
  axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  image_scale(as.matrix(vdif_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Diff. Schnee-Volumen [m³]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
  #Snow difference trend
  par(mar = c(1.6, 3, 0.6, 0))
  
  x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
  
  my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                               "yellow2","gold", "orange2", "orangered2"))(200)
  
  n_max <- round(abs(max_na(vdis_band[, ])) / (max_na(vdis_band[, ]) + abs(min_na(vdis_band[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  
  cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
  cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
  my_col <- c(cols_min, cols_max)
  
  my_bre <- seq(min_na(vdis_band), max_na(vdis_band), length.out = length(my_col)+1)
  
  image(x = 1:365,
        y = my_elev_bands[-length(my_elev_bands)],
        z = vdis_band, col =my_col, breaks = my_bre,
        ylab = "", xlab = "", axes = F)
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.06)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
  mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
  axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  image_scale(as.matrix(vdis_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Diff. Schnee-Volumen [m³/10a]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
}

output$plot_disc <-  renderPlot({f_plot_disc()})
output$plot_clim <-  renderPlot({f_plot_clim()})
output$plot_clic <-  renderPlot({f_plot_clic()})
output$plot_smea <-  renderPlot({f_plot_smea()})
output$plot_sslo <-  renderPlot({f_plot_sslo()})

#Selection gauge via click in map
gauge_sel <-  shiny::reactiveValues(clicked_gauge = "Basel")

observeEvent(input$map_marker_click,{
  
  gauge_sel$clicked_gauge <- input$map_marker_click
  # print(gauge_sel$clicked_gauge$lat)
  
  if(paste(gauge_sel$clicked_gauge$lat) == "49.6318"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    qvalu_long <- qvalu_long_worm
    qvslo_long <- qvslo_long_worm
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.08561"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    qvalu_long <- qvalu_long_kaub
    qvslo_long <- qvslo_long_kaub
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.937"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    qvalu_long <- qvalu_long_koel
    qvslo_long <- qvslo_long_koel
    
  }
  
  f_plot_disc <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(rep(c(1,3), 7),
                    2,4),
                  2, 8), widths=c(), heights=c(1,1))
    
    x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
    ytiks      <- seq(10, 90, by =  10)
    ylabs      <- seq(90, 10, by = -10) 
    
    cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                                "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)
    
    max_break <- max_na(qvalu_long)
    min_break <- min_na(qvalu_long)
    qua_break <- quantile(qvalu_long, probs = 0.7, type = 8, na.rm = T)
    
    breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
    breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
    breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1
    
    breaks_qvalu <- c(breaks_1, breaks_2)
    
    y <- 1:ncol(qvalu_long)
    x <- 1:365
    
    par(mar = c(1.6, 3, 0.6, 0))
    
    image(x, y, as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
          xlab = "", axes = F)
    
    axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
    mtext("Quantil", 2, 1.5, cex = 0.8)
    
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.03)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
    box()
    
    par(mar = c(1.6, 0.5, 0.6, 2.7))
    
    alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Abfluss [m³/sec]", side = 4, line = 1.5, cex = 0.8)
    
    box()
    
    
    #Plot: Trend moving quantile
    
    x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
    ytiks      <- seq(10, 90, by =  10)
    ylabs      <- seq(90, 10, by = -10) 
    
    n_max <- round(abs(alptempr::max_na(qvslo_long)) / (alptempr::max_na(qvslo_long) + abs(alptempr::min_na(qvslo_long))), digits = 2) * 200
    n_min <- 200 - n_max
    cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
    cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
    cols_qvslo <- c(cols_min, cols_max)
    
    breaks_qvslo <-  seq(alptempr::min_na(qvslo_long), alptempr::max_na(qvslo_long), length.out = length(cols_qvslo) +1)
    
    y <- 1:ncol(qvslo_long)
    x <- 1:365
    
    par(mar = c(1.6, 3, 0.6, 0))
    
    image(x, y, as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
          xlab = "", axes = F)
    
    axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
    mtext("Quantile", 2, 1.5, cex = 0.8)
    
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.03)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
    box()
    
    par(mar = c(1.6, 0.5, 0.6, 2.7))
    
    alptempr::image_scale(as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Trend Fenster-Quantil [m³/sec/10a]", side = 4, line = 1.5, cex = 0.8)
    
    box()
    
  }
  f_plot_clim <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    # mtext("Plot not available for current selection.", line = -1)
    mtext("Abbildung für aktuelle Auswahl nicht verfügbar.", line = -1)
  }
  f_plot_clic <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    # mtext("Plot not available for current selection.", line = -1)
    mtext("Abbildung für aktuelle Auswahl nicht verfügbar.", line = -1)
  }
  f_plot_smea <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    # mtext("Plot not available for current selection.", line = -1)
    mtext("Abbildung für aktuelle Auswahl nicht verfügbar.", line = -1)
  }
  f_plot_sslo <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    #mtext("Plot not available for current selection.", line = -1)
    mtext("Abbildung für aktuelle Auswahl nicht verfügbar.", line = -1)
  }
  
  #Basel with snow and climate analysis
  if(paste(gauge_sel$clicked_gauge$lat) == "47.55927"){
    
    output$basin_select_clim <- renderText({"Hochrhein bis Pegel Basel"})
    output$basin_select_clic <- renderText({"Hochrhein bis Pegel Basel"})
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    qvalu_long <- qvalu_long_base
    qvslo_long <- qvslo_long_base
    
    my_no_col <- F
    meta_grid_bands <- meta_grid_bands_base
    my_elev_bands <- my_elev_bands_base
    tmed_band <- tmed_band_base
    tmed_band_med <- tmed_band_med_base
    tslo_band <- tslo_band_base
    tslo_band_med <- tslo_band_med_base
    pmea_band <- pmea_band_base
    pmea_band_med <- pmea_band_med_base
    pslo_band <- pslo_band_base
    pslo_band_med <- pslo_band_med_base
    emed_band <- emed_band_base
    emed_band_med <- emed_band_med_base
    eslo_band <- eslo_band_base
    eslo_band_med <- eslo_band_med_base
    
    smea_band <- smea_band_base
    vmea_band <- vmea_band_base
    vdif_band <- vdif_band_base
    vdis_band <- vdis_band_base
    
    f_plot_clim <- function(){
      
      par(bg = "grey92")
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,
                      2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
      
      plot_cycl_elev(data_in = tmed_band, data_mk = tmed_band, data_in_me = tmed_band_med,
                     data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
      
      plot_cycl_elev(data_in = pmea_band, data_mk = pmea_band, data_in_me = pmea_band_med,
                     data_meta = meta_grid_bands, main_text= paste0("Niederschlag [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
      
      plot_cycl_elev(data_in = emed_band, data_mk = emed_band, data_in_me = emed_band_med,
                     data_meta = meta_grid_bands, main_text= paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
      
    }
    f_plot_clic <- function(){
      
      par(bg = "grey92")
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,
                      2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
      
      plot_cycl_elev(data_in = tslo_band, data_mk = tslo_band, data_in_me = tslo_band_med,
                     data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C/10a]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
      
      plot_cycl_elev(data_in = pslo_band, data_mk = pslo_band, data_in_me = pslo_band_med,
                     data_meta = meta_grid_bands, main_text = paste0("Niederschlag [mm/10a]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
      
      plot_cycl_elev(data_in = eslo_band, data_mk = eslo_band, data_in_me = eslo_band_med,
                     data_meta = meta_grid_bands, main_text = paste0("Evapotranspiration [mm/10a]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    }
    f_plot_smea <- function(){
      
      par(bg = "grey92")
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(rep(c(1,3), 7),
                      2,4),
                    2, 8), widths=c(), heights=c(1,1))
      
      #Snow depth elevation band
      par(mar = c(1.6, 3, 0.6, 0))
      
      x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
      x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
      
      my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                   "yellow2","gold", "orange2", "orangered2"))(200)
      
      n_max <- round(abs(max_na(smea_band[, ])) / (max_na(smea_band[, ]) + abs(min_na(smea_band[, ]))), digits = 2) * 200
      n_min <- 200 - n_max
      
      cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
      cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
      # my_col <- c(cols_min, cols_max)
      
      my_bre <- seq(min_na(smea_band), max_na(smea_band), length.out = length(my_col)+1)
      
      image(x = 1:365,
            y = my_elev_bands[-length(my_elev_bands)],
            z = smea_band, col =my_col, breaks = my_bre,
            ylab = "", xlab = "", axes = F)
      axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
           col = "black", col.axis = "black", tck = -0.06)#plot ticks
      axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
           col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
      mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
      axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
      box()
      
      par(mar = c(1.6, 0.5, 0.6, 2.7))
      
      image_scale(as.matrix(smea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
      axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
      mtext("Schnee-Wasser-Equ. [m]", side = 4, line = 1.5, cex = 0.8)
      
      box()
      
      #Snow volume elevation band
      par(mar = c(1.6, 3, 0.6, 0))
      
      x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
      x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
      
      my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                   "yellow2","gold", "orange2", "orangered2"))(200)
      
      n_max <- round(abs(max_na(vmea_band[, ])) / (max_na(vmea_band[, ]) + abs(min_na(vmea_band[, ]))), digits = 2) * 200
      n_min <- 200 - n_max
      
      cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
      cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
      # my_col <- c(cols_min, cols_max)
      
      my_bre <- seq(min_na(vmea_band), max_na(vmea_band), length.out = length(my_col)+1)
      
      image(x = 1:365,
            y = my_elev_bands[-length(my_elev_bands)],
            z = vmea_band, col =my_col, breaks = my_bre,
            ylab = "", xlab = "", axes = F)
      axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
           col = "black", col.axis = "black", tck = -0.06)#plot ticks
      axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
           col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
      mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
      axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
      box()
      
      par(mar = c(1.6, 0.5, 0.6, 2.7))
      
      image_scale(as.matrix(vmea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
      axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
      mtext("Schnee volumen [m³]", side = 4, line = 1.5, cex = 0.8)
      
      box()
      
      
    }
    f_plot_sslo <- function(){
      
      par(bg = "grey92")
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(rep(c(1,3), 7),
                      2,4),
                    2, 8), widths=c(), heights=c(1,1))
      
      
      #Snow difference mean
      par(mar = c(1.6, 3, 0.6, 0))
      
      x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
      x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
      
      my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                   "yellow2","gold", "orange2", "orangered2"))(200)
      
      n_max <- round(abs(max_na(vdif_band[, ])) / (max_na(vdif_band[, ]) + abs(min_na(vdif_band[, ]))), digits = 2) * 200
      n_min <- 200 - n_max
      
      cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
      cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
      my_col <- c(cols_min, cols_max)
      
      my_bre <- seq(min_na(vdif_band), max_na(vdif_band), length.out = length(my_col)+1)
      
      image(x = 1:365,
            y = my_elev_bands[-length(my_elev_bands)],
            z = vdif_band, col =my_col, breaks = my_bre,
            ylab = "", xlab = "", axes = F)
      axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
           col = "black", col.axis = "black", tck = -0.06)#plot ticks
      axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
           col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
      mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
      axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
      box()
      
      par(mar = c(1.6, 0.5, 0.6, 2.7))
      
      image_scale(as.matrix(vdif_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
      axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
      mtext("Diff. Schnee-Volumen [m³]", side = 4, line = 1.5, cex = 0.8)
      
      box()
      
      #Snow difference trend
      par(mar = c(1.6, 3, 0.6, 0))
      
      x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
      x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
      
      my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                   "yellow2","gold", "orange2", "orangered2"))(200)
      
      n_max <- round(abs(max_na(vdis_band[, ])) / (max_na(vdis_band[, ]) + abs(min_na(vdis_band[, ]))), digits = 2) * 200
      n_min <- 200 - n_max
      
      cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
      cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
      my_col <- c(cols_min, cols_max)
      
      my_bre <- seq(min_na(vdis_band), max_na(vdis_band), length.out = length(my_col)+1)
      
      image(x = 1:365,
            y = my_elev_bands[-length(my_elev_bands)],
            z = vdis_band, col =my_col, breaks = my_bre,
            ylab = "", xlab = "", axes = F)
      axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
           col = "black", col.axis = "black", tck = -0.06)#plot ticks
      axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
           col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
      mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
      axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
      box()
      
      par(mar = c(1.6, 0.5, 0.6, 2.7))
      
      image_scale(as.matrix(vdis_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
      axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
      mtext("Diff. Schnee-Volumen [m³/10a]", side = 4, line = 1.5, cex = 0.8)
      
      box()
      
    }
    
  }
  
  output$plot_disc <-  renderPlot({f_plot_disc()})
  output$plot_clim <-  renderPlot({f_plot_clim()})
  output$plot_clic <-  renderPlot({f_plot_clic()})
  output$plot_smea <-  renderPlot({f_plot_smea()})
  output$plot_sslo <-  renderPlot({f_plot_sslo()})
  
})

#Selection basin via click in map
basin_sel <- shiny::reactiveValues(clicked_basin = "Reuss")

observeEvent(input$map_shape_click,{
  
  basin_sel$clicked_basin <- input$map_shape_click
  # print(basin_sel$clicked_basin$id)
  
  if(paste(basin_sel$clicked_basin$id) == "alp_rhine"){
    
    output$basin_select_clim <- renderText({"Alpenrhein bis Pegel Diepoldsau"})
    output$basin_select_clic <- renderText({"Alpenrhein bis Pegel Diepoldsau"})
    qvalu_long <- qvalu_long_diep
    qvslo_long <- qvslo_long_diep
    
    my_no_col <- F
    meta_grid_bands <- meta_grid_bands_diep
    my_elev_bands <- my_elev_bands_diep
    tmed_band <- tmed_band_diep
    tmed_band_med <- tmed_band_med_diep
    tslo_band <- tslo_band_diep
    tslo_band_med <- tslo_band_med_diep
    pmea_band <- pmea_band_diep
    pmea_band_med <- pmea_band_med_diep
    pslo_band <- pslo_band_diep
    pslo_band_med <- pslo_band_med_diep
    emed_band <- emed_band_diep
    emed_band_med <- emed_band_med_diep
    eslo_band <- eslo_band_diep
    eslo_band_med <- eslo_band_med_diep
    
    smea_band <- smea_band_diep
    vmea_band <- vmea_band_diep
    vdif_band <- vdif_band_diep
    vdis_band <- vdis_band_diep
    
  }
  if(paste(basin_sel$clicked_basin$id) == "reuss"){
    
    output$basin_select_clim <- renderText({"Reuss bis Pegel Mellingen"})
    output$basin_select_clic <- renderText({"Reuss bis Pegel Mellingen"})
    qvalu_long <- qvalu_long_mell
    qvslo_long <- qvslo_long_mell
    
    my_no_col <- F
    meta_grid_bands <- meta_grid_bands_mell
    my_elev_bands <- my_elev_bands_mell
    tmed_band <- tmed_band_mell
    tmed_band_med <- tmed_band_med_mell
    tslo_band <- tslo_band_mell
    tslo_band_med <- tslo_band_med_mell
    pmea_band <- pmea_band_mell
    pmea_band_med <- pmea_band_med_mell
    pslo_band <- pslo_band_mell
    pslo_band_med <- pslo_band_med_mell
    emed_band <- emed_band_mell
    emed_band_med <- emed_band_med_mell
    eslo_band <- eslo_band_mell
    eslo_band_med <- eslo_band_med_mell
    
    smea_band <- smea_band_mell
    vmea_band <- vmea_band_mell
    vdif_band <- vdif_band_mell
    vdis_band <- vdis_band_mell
    
  }
  if(paste(basin_sel$clicked_basin$id) == "aare"){
    
    output$basin_select_clim <- renderText({"Aare bis Pegel Brugg"})
    output$basin_select_clic <- renderText({"Aare bis Pegel Brugg"})
    qvalu_long <- qvalu_long_brug
    qvslo_long <- qvslo_long_brug
    
    my_no_col <- F
    meta_grid_bands <- meta_grid_bands_brug
    my_elev_bands <- my_elev_bands_brug
    tmed_band <- tmed_band_brug
    tmed_band_med <- tmed_band_med_brug
    tslo_band <- tslo_band_brug
    tslo_band_med <- tslo_band_med_brug
    pmea_band <- pmea_band_brug
    pmea_band_med <- pmea_band_med_brug
    pslo_band <- pslo_band_brug
    pslo_band_med <- pslo_band_med_brug
    emed_band <- emed_band_brug
    emed_band_med <- emed_band_med_brug
    eslo_band <- eslo_band_brug
    eslo_band_med <- eslo_band_med_brug
    
    smea_band <- smea_band_brug
    vmea_band <- vmea_band_brug
    vdif_band <- vdif_band_brug
    vdis_band <- vdis_band_brug
    
  }
  if(paste(basin_sel$clicked_basin$id) == "moselle"){
    
    output$basin_select_clim <- renderText({"Mosel bis Pegel Cochem"})
    output$basin_select_clic <- renderText({"Mosel bis Pegel Cochem"})
    qvalu_long <- qvalu_long_coch
    qvslo_long <- qvslo_long_coch
    
    my_no_col <- T
    meta_grid_bands <- meta_grid_bands_mose
    my_elev_bands <- my_elev_bands_mose
    tmed_band <- tmed_band_mose
    tmed_band_med <- tmed_band_med_mose
    tslo_band <- tslo_band_mose
    tslo_band_med <- tslo_band_med_mose
    pmea_band <- pmea_band_mose
    pmea_band_med <- pmea_band_med_mose
    pslo_band <- pslo_band_mose
    pslo_band_med <- pslo_band_med_mose
    emed_band <- emed_band_mose
    emed_band_med <- emed_band_med_mose
    eslo_band <- eslo_band_mose
    eslo_band_med <- eslo_band_med_mose
    
    smea_band <- smea_band_mose
    vmea_band <- vmea_band_mose
    vdif_band <- vdif_band_mose
    vdis_band <- vdis_band_mose
    
  }
  if(paste(basin_sel$clicked_basin$id) == "nahe"){
    
    output$basin_select_clim <- renderText({"Nahe bis Pegel Grolsheim"})
    output$basin_select_clic <- renderText({"Nahe bis Pegel Grolsheim"})
    qvalu_long <- qvalu_long_grol
    qvslo_long <- qvslo_long_grol
    
    my_no_col <- T
    meta_grid_bands <- meta_grid_bands_nahe
    my_elev_bands <- my_elev_bands_nahe
    tmed_band <- tmed_band_nahe
    tmed_band_med <- tmed_band_med_nahe
    tslo_band <- tslo_band_nahe
    tslo_band_med <- tslo_band_med_nahe
    pmea_band <- pmea_band_nahe
    pmea_band_med <- pmea_band_med_nahe
    pslo_band <- pslo_band_nahe
    pslo_band_med <- pslo_band_med_nahe
    emed_band <- emed_band_nahe
    emed_band_med <- emed_band_med_nahe
    eslo_band <- eslo_band_nahe
    eslo_band_med <- eslo_band_med_nahe
    
    smea_band <- smea_band_nahe
    vmea_band <- vmea_band_nahe
    vdif_band <- vdif_band_nahe
    vdis_band <- vdis_band_nahe
    
  }
  if(paste(basin_sel$clicked_basin$id) == "lahn"){
    
    output$basin_select_clim <- renderText({"Lahn bis Pegel Kalkofen"})
    output$basin_select_clic <- renderText({"Lahn bis Pegel Kalkofen"})
    qvalu_long <- qvalu_long_kalk
    qvslo_long <- qvslo_long_kalk
    
    my_no_col <- T
    meta_grid_bands <- meta_grid_bands_lahn
    my_elev_bands <- my_elev_bands_lahn
    tmed_band <- tmed_band_lahn
    tmed_band_med <- tmed_band_med_lahn
    tslo_band <- tslo_band_lahn
    tslo_band_med <- tslo_band_med_lahn
    pmea_band <- pmea_band_lahn
    pmea_band_med <- pmea_band_med_lahn
    pslo_band <- pslo_band_lahn
    pslo_band_med <- pslo_band_med_lahn
    emed_band <- emed_band_lahn
    emed_band_med <- emed_band_med_lahn
    eslo_band <- eslo_band_lahn
    eslo_band_med <- eslo_band_med_lahn
    
    smea_band <- smea_band_lahn
    vmea_band <- vmea_band_lahn
    vdif_band <- vdif_band_lahn
    vdis_band <- vdis_band_lahn
    
  }
  if(paste(basin_sel$clicked_basin$id) == "main"){
    
    output$basin_select_clim <- renderText({"Main bis Pegel Frankfurt"})
    output$basin_select_clic <- renderText({"Main bis Pegel Frankfurt"})
    qvalu_long <- qvalu_long_fran
    qvslo_long <- qvslo_long_fran
    
    my_no_col <- T
    meta_grid_bands <- meta_grid_bands_main
    my_elev_bands <- my_elev_bands_main
    tmed_band <- tmed_band_main
    tmed_band_med <- tmed_band_med_main
    tslo_band <- tslo_band_main
    tslo_band_med <- tslo_band_med_main
    pmea_band <- pmea_band_main
    pmea_band_med <- pmea_band_med_main
    pslo_band <- pslo_band_main
    pslo_band_med <- pslo_band_med_main
    emed_band <- emed_band_main
    emed_band_med <- emed_band_med_main
    eslo_band <- eslo_band_main
    eslo_band_med <- eslo_band_med_main
    
    smea_band <- smea_band_main
    vmea_band <- vmea_band_main
    vdif_band <- vdif_band_main
    vdis_band <- vdis_band_main
    
  }
  if(paste(basin_sel$clicked_basin$id) == "neckar"){
    
    output$basin_select_clim <- renderText({"Neckar bis Pegel Rockenau"})
    output$basin_select_clic <- renderText({"Neckar bis Pegel Rockenau"})
    qvalu_long <- qvalu_long_rock
    qvslo_long <- qvslo_long_rock
    
    my_no_col <- T
    meta_grid_bands <- meta_grid_bands_neck
    my_elev_bands <- my_elev_bands_neck
    tmed_band <- tmed_band_neck
    tmed_band_med <- tmed_band_med_neck
    tslo_band <- tslo_band_neck
    tslo_band_med <- tslo_band_med_neck
    pmea_band <- pmea_band_neck
    pmea_band_med <- pmea_band_med_neck
    pslo_band <- pslo_band_neck
    pslo_band_med <- pslo_band_med_neck
    emed_band <- emed_band_neck
    emed_band_med <- emed_band_med_neck
    eslo_band <- eslo_band_neck
    eslo_band_med <- eslo_band_med_neck
    
    smea_band <- smea_band_neck
    vmea_band <- vmea_band_neck
    vdif_band <- vdif_band_neck
    vdis_band <- vdis_band_neck
    
  }
  
  f_plot_disc <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(rep(c(1,3), 7),
                    2,4),
                  2, 8), widths=c(), heights=c(1,1))
    
    x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
    ytiks      <- seq(10, 90, by =  10)
    ylabs      <- seq(90, 10, by = -10) 
    
    cols_qvalu <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white", 
                                                "yellow2","gold2", "orange2", "orangered3", "orangered4", "red4"))(200)
    
    max_break <- max_na(qvalu_long)
    min_break <- min_na(qvalu_long)
    qua_break <- quantile(qvalu_long, probs = 0.7, type = 8, na.rm = T)
    
    breaks_1 <- seq(min_break, qua_break, length.out = length(cols_qvalu)/2)
    breaks_2 <- lseq(qua_break+0.01, max_break, length.out = length(cols_qvalu)/2 + 1)
    breaks_2[length(breaks_2)] <- breaks_2[length(breaks_2)] + 0.1
    
    breaks_qvalu <- c(breaks_1, breaks_2)
    
    y <- 1:ncol(qvalu_long)
    x <- 1:365
    
    par(mar = c(1.6, 3, 0.6, 0))
    
    image(x, y, as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, ylab = "",
          xlab = "", axes = F)
    
    axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
    mtext("Quantil", 2, 1.5, cex = 0.8)
    
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.03)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
    box()
    
    par(mar = c(1.6, 0.5, 0.6, 2.7))
    
    alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Abfluss [m³/sec]", side = 4, line = 1.5, cex = 0.8)
    
    box()
    
    
    #Plot: Trend moving quantile
    
    x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
    ytiks      <- seq(10, 90, by =  10)
    ylabs      <- seq(90, 10, by = -10) 
    
    n_max <- round(abs(alptempr::max_na(qvslo_long)) / (alptempr::max_na(qvslo_long) + abs(alptempr::min_na(qvslo_long))), digits = 2) * 200
    n_min <- 200 - n_max
    cols_min <- grDevices::colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "white"))(n_min)
    cols_max <- grDevices::colorRampPalette(c("white", "yellow2","gold2", "orange2", "orangered3", "orangered4"))(n_max)
    cols_qvslo <- c(cols_min, cols_max)
    
    breaks_qvslo <-  seq(alptempr::min_na(qvslo_long), alptempr::max_na(qvslo_long), length.out = length(cols_qvslo) +1)
    
    y <- 1:ncol(qvslo_long)
    x <- 1:365
    
    par(mar = c(1.6, 3, 0.6, 0))
    
    image(x, y, as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, ylab = "",
          xlab = "", axes = F)
    
    axis(2, at = ytiks, labels = ylabs/100, mgp = c(3, 0.3, 0))
    mtext("Quantil", 2, 1.5, cex = 0.8)
    
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.03)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
    box()
    
    par(mar = c(1.6, 0.5, 0.6, 2.7))
    
    alptempr::image_scale(as.matrix(qvslo_long), col = cols_qvslo, breaks = breaks_qvslo, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Trend Fenster-Quantil [m³/sec/10a]", side = 4, line = 1.5, cex = 0.8)
    
    box()
    
  }
  f_plot_clim <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(1,3,5,
                    2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
    
    plot_cycl_elev(data_in = tmed_band, data_mk = tmed_band, data_in_me = tmed_band_med,
                   data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    
    plot_cycl_elev(data_in = pmea_band, data_mk = pmea_band, data_in_me = pmea_band_med,
                   data_meta = meta_grid_bands, main_text= paste0("Niederschlag [mm]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    
    plot_cycl_elev(data_in = emed_band, data_mk = emed_band, data_in_me = emed_band_med,
                   data_meta = meta_grid_bands, main_text= paste0("Evapotranspiration [mm]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    
  }
  f_plot_clic <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(1,3,5,
                    2,4,6), 3, 2), widths=c(1, 1), heights=rep(1, 3))
    
    plot_cycl_elev(data_in = tslo_band, data_mk = tslo_band, data_in_me = tslo_band_med,
                   data_meta = meta_grid_bands, main_text = paste0("Temperatur [°C/10a]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    
    plot_cycl_elev(data_in = pslo_band, data_mk = pslo_band, data_in_me = pslo_band_med,
                   data_meta = meta_grid_bands, main_text = paste0("Niederschlag [mm/10a]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
    
    plot_cycl_elev(data_in = eslo_band, data_mk = eslo_band, data_in_me = eslo_band_med,
                   data_meta = meta_grid_bands, main_text = paste0("Evapotranspiration [mm/10a]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = F)
  }
  f_plot_smea <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(rep(c(1,3), 7),
                    2,4),
                  2, 8), widths=c(), heights=c(1,1))
    
    #Snow depth elevation band
    par(mar = c(1.6, 3, 0.6, 0))
    
    x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

    my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                 "yellow2","gold", "orange2", "orangered2"))(200)

    n_max <- round(abs(max_na(smea_band[, ])) / (max_na(smea_band[, ]) + abs(min_na(smea_band[, ]))), digits = 2) * 200
    n_min <- 200 - n_max

    cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
    cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
    # my_col <- c(cols_min, cols_max)

    my_bre <- seq(min_na(smea_band), max_na(smea_band), length.out = length(my_col)+1)

    image(x = 1:365,
          y = my_elev_bands[-length(my_elev_bands)],
          z = smea_band, col =my_col, breaks = my_bre,
          ylab = "", xlab = "", axes = F)
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.06)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
    mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
    axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
    box()

    par(mar = c(1.6, 0.5, 0.6, 2.7))

    image_scale(as.matrix(smea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Schnee-Wasser-Equ. [m]", side = 4, line = 1.5, cex = 0.8)

    box()

    #Snow volume elevation band
    par(mar = c(1.6, 3, 0.6, 0))

    x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

    my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                 "yellow2","gold", "orange2", "orangered2"))(200)

    n_max <- round(abs(max_na(vmea_band[, ])) / (max_na(vmea_band[, ]) + abs(min_na(vmea_band[, ]))), digits = 2) * 200
    n_min <- 200 - n_max

    cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
    cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
    # my_col <- c(cols_min, cols_max)

    my_bre <- seq(min_na(vmea_band), max_na(vmea_band), length.out = length(my_col)+1)

    image(x = 1:365,
          y = my_elev_bands[-length(my_elev_bands)],
          z = vmea_band, col =my_col, breaks = my_bre,
          ylab = "", xlab = "", axes = F)
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.06)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
    mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
    axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
    box()

    par(mar = c(1.6, 0.5, 0.6, 2.7))

    image_scale(as.matrix(vmea_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Schnee volumen [m³]", side = 4, line = 1.5, cex = 0.8)

    box()
    
    
  }
  f_plot_sslo <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(rep(c(1,3), 7),
                    2,4),
                  2, 8), widths=c(), heights=c(1,1))
    

    #Snow difference mean
    par(mar = c(1.6, 3, 0.6, 0))

    x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

    my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                 "yellow2","gold", "orange2", "orangered2"))(200)

    n_max <- round(abs(max_na(vdif_band[, ])) / (max_na(vdif_band[, ]) + abs(min_na(vdif_band[, ]))), digits = 2) * 200
    n_min <- 200 - n_max

    cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
    cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
    my_col <- c(cols_min, cols_max)

    my_bre <- seq(min_na(vdif_band), max_na(vdif_band), length.out = length(my_col)+1)

    image(x = 1:365,
          y = my_elev_bands[-length(my_elev_bands)],
          z = vdif_band, col =my_col, breaks = my_bre,
          ylab = "", xlab = "", axes = F)
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.06)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
    mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
    axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
    box()

    par(mar = c(1.6, 0.5, 0.6, 2.7))

    image_scale(as.matrix(vdif_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Diff. Schnee-Volumen [m³]", side = 4, line = 1.5, cex = 0.8)

    box()
    
    #Snow difference trend
    par(mar = c(1.6, 3, 0.6, 0))

    x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
    x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

    my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                                 "yellow2","gold", "orange2", "orangered2"))(200)

    n_max <- round(abs(max_na(vdis_band[, ])) / (max_na(vdis_band[, ]) + abs(min_na(vdis_band[, ]))), digits = 2) * 200
    n_min <- 200 - n_max

    cols_min <- colorRampPalette(c(viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(n_min)
    cols_max <- colorRampPalette(c("grey90", "yellow2", "gold", "orange2", "orangered2"))(n_max)
    my_col <- c(cols_min, cols_max)

    my_bre <- seq(min_na(vdis_band), max_na(vdis_band), length.out = length(my_col)+1)

    image(x = 1:365,
          y = my_elev_bands[-length(my_elev_bands)],
          z = vdis_band, col =my_col, breaks = my_bre,
          ylab = "", xlab = "", axes = F)
    axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
         col = "black", col.axis = "black", tck = -0.06)#plot ticks
    axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
         col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
    mtext("Höhe ü.d.M. [m]", side = 2, line = 1.5, cex = 0.8)
    axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
    box()

    par(mar = c(1.6, 0.5, 0.6, 2.7))

    image_scale(as.matrix(vdis_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
    axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
    mtext("Diff. Schnee-Volumen [m³/10a]", side = 4, line = 1.5, cex = 0.8)

    box()
    
  }
  
  output$plot_disc <-  renderPlot({f_plot_disc()})
  output$plot_clim <-  renderPlot({f_plot_clim()})
  output$plot_clic <-  renderPlot({f_plot_clic()})
  output$plot_smea <-  renderPlot({f_plot_smea()})
  output$plot_sslo <-  renderPlot({f_plot_sslo()})
  
})

}

#run_app####
shiny::shinyApp( ui = ui, server = server)
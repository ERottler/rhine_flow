###

#Rhine river basin flow changes: Shiny app
#Erwin Rottler, Universtiy of Potsdam
#September 2019

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

load(paste0(base_dir, "data/rhine_flow_app_old.RData"))

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
          plotOutput(outputId = "plot_clim", height = 500)
        )#conditional Panel
      )#wellPanel
    ),
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
          plotOutput(outputId = "plot_clic", height = 500)
        )#conditional Panel
      )#wellPanel
    ),
    column(
      width = 12,
    wellPanel(
      tags$p("Sollten Sie Fragen oder Anregungen haben, bitte zögern Sie nicht uns zu kontaktieren: rottler(at)uni-potsdam.de"),
      tags$p("Diese Analysen werden durch die Deutsche Forschungsgemeinschaft (DFG) im Rahmen des Graduiertenkollegs 'NatRiskChange' gefördert (www.natriskchange.de).")
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
                label = "Alpine Rhine measured at gauge Diepoldsau", color = "red",
                # popup = "Alpine Rhine measured at gauge Diepoldsau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_reus, layerId = "reuss", fillOpacity = 0.4, group = "click.list",
                label = "Reuss catchment measured at gauge Mellingen", color = "red",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_aare, layerId = "aare", fillOpacity = 0.4, group = "click.list",
                label = "Aare catchment measured at gauge Brugg", color = "red",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_mose, layerId = "moselle",fillOpacity = 0.4, group = "click.list",
                label = "Moselle catchment measured at gauge Cochem",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_neck, layerId = "neckar", fillOpacity = 0.4, group = "click.list",
                label = "Neckar catchment measured at gauge Rockenau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_nahe, layerId = "nahe", fillOpacity = 0.4, group = "click.list",
                label = "Nahe catchment measured at gauge Grolsheim",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_lahn, layerId = "lahn", fillOpacity = 0.4, group = "click.list",
                label = "Lahn catchment measured at gauge Kalkofen",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "right", offset = c(10, -20)))%>%
    addPolygons(data = sh_main, layerId = "main", fillOpacity = 0.4, group = "click.list",
                label = "Main catchment measured at gauge Frankfurt",
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
  
}) # end of leaflet::renderLeaflet({})

#Start plots (Nahe-Grolsheim)
i = which(colnames(dis_sel) == "Grolsheim")
output$basin_select_clim <- renderText({"Nahe river catchment until gauge Grolsheim"})
output$basin_select_clic <- renderText({"Nahe river catchment until gauge Grolsheim"})

meta_grid <- meta_grid_nahe
my_no_col <- T
tmed <- tmed_nahe
tmed_med <- tmed_med_nahe
pmea <- pmea_nahe
pmea_med <- pmea_med_nahe
emed <- emed_nahe
emed_med <- emed_med_nahe
smea <- smea_nahe
smea_mea <- smea_mea_nahe
tslo <- tslo_nahe
tslo_med <- tslo_med_nahe
pslo <- pslo_nahe
pslo_med <- pslo_med_nahe
eslo <- eslo_nahe
eslo_med <- eslo_med_nahe
sslo <- sslo_nahe
sslo_mea <- sslo_mea_nahe

f_plot_disc <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
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
             main      = paste0(colnames(dis_sel)[i], ": Quantile exceedance probab. [%]"),
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
             main <- paste0(colnames(dis_sel)[i], ": Trend moving quantile exceedance prob. [%/dec]"),
             ylab <- "Quantile",
             margins_1 = c(1.6,2.5,1.6,0),
             margins_2 = c(1.6,0.5,1.6,1.7)
  )
  
}
f_plot_disc <- function(){
  
  layout(matrix(c(1,1,1,1,1,1,1,2),
                1, 8), widths=c(), heights=c())
  
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
  mtext("Quantile", 2, 1.5, cex = 0.8)
  
  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  box()
  
  par(mar = c(1.6, 0.5, 0.6, 2.7))
  
  alptempr::image_scale(as.matrix(qvalu_long), col = cols_qvalu, breaks = breaks_qvalu, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
  mtext("Discharge [m³/sec]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
  
  #Plot: Trend moving quantile
  
  layout(matrix(c(1,1,1,1,1,1,1,2),
                1, 8), widths=c(), heights=c())
  
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
  mtext("Trend window quantiles [m³/sec/dec]", side = 4, line = 1.5, cex = 0.8)
  
  box()
  
}
f_plot_clim <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(1,3,5,7,
                  2,4,6,8), 4, 2), widths=c(1, 1), heights=rep(1, 4))
  
  plot_cycl_elev(data_in = tmed, data_mk = tmed, data_in_me = tmed_med,
                 data_meta = meta_grid, main_text = paste0("Temperature [°C]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = pmea, data_mk = pmea, data_in_me = pmea_med,
                 data_meta = meta_grid, main_text = paste0("Precipitation [mm]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = emed, data_mk = emed, data_in_me = emed_med,
                 data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = smea, data_mk = smea, data_in_me = smea_mea,
                 data_meta = meta_grid, main_text = paste0("Snow water equiv. mod. [mm]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
}
f_plot_clic <- function(){
  
  par(bg = "grey92")
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  layout(matrix(c(1,3,5,7,
                  2,4,6,8), 4, 2), widths=c(1, 1), heights=rep(1, 4))
  
  plot_cycl_elev(data_in = tslo, data_mk = tslo, data_in_me = tslo_med,
                 data_meta = meta_grid, main_text = paste0("Temperature [°C/dec]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = pslo, data_mk = pslo, data_in_me = pslo_med,
                 data_meta = meta_grid, main_text = paste0("Precipitation [mm/dec]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = eslo, data_mk = eslo, data_in_me = eslo_med,
                 data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm/dec]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  
  plot_cycl_elev(data_in = sslo, data_mk = sslo, data_in_me = sslo_mea,
                 data_meta = meta_grid, main_text = paste0("Snow water equiv. mod. [mm/dec]"),
                 margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                 no_col = my_no_col, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                 smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
}

output$plot_disc <-  renderPlot({f_plot_disc()})
output$plot_clim <-  renderPlot({f_plot_clim()})
output$plot_clic <-  renderPlot({f_plot_clic()})

#Selection gauge via click in map
gauge_sel <-  shiny::reactiveValues(clicked_gauge = "Basel")

observeEvent(input$map_marker_click,{
  
  gauge_sel$clicked_gauge <- input$map_marker_click
  # print(gauge_sel$clicked_gauge$lat)
  
  if(paste(gauge_sel$clicked_gauge$lat) == "47.55927"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    i = which(colnames(dis_sel) == "Basel_Rheinhalle")
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "49.6318"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    i = which(colnames(dis_sel) == "Worms")
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.08561"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    i = which(colnames(dis_sel) == "Kaub")
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.937"){
    
    output$basin_select_clim <- renderText({"Select sub-basin"})
    output$basin_select_clic <- renderText({"Select sub-basin"})
    i = which(colnames(dis_sel) == "Koeln")
    
  }
  
  f_plot_disc <- function(){
    
    par(oma=c(0,0,0,0))
    par(family="serif")
    
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
               main      = paste0(colnames(dis_sel)[i], ": Quantile exceedance probab. [%]"),
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
               main <- paste0(colnames(dis_sel)[i], ": Trend moving quantile exceedance prob. [%/dec]"),
               ylab <- "Quantile",
               margins_1 = c(1.6,2.5,1.6,0),
               margins_2 = c(1.6,0.5,1.6,1.7)
    )
    
  }
  f_plot_clim <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Plot not available for current selection.", line = -1)
  }
  f_plot_clic <- function(){
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Plot not available for current selection.", line = -1)
  }
  
  output$plot_disc <-  renderPlot({f_plot_disc()})
  output$plot_clim <-  renderPlot({f_plot_clim()})
  output$plot_clic <-  renderPlot({f_plot_clic()})
  
})

#Selection basin via click in map
basin_sel <- shiny::reactiveValues(clicked_basin = "Reuss")

observeEvent(input$map_shape_click,{
  
  basin_sel$clicked_basin <- input$map_shape_click
  # print(basin_sel$clicked_basin$id)
  
  if(paste(basin_sel$clicked_basin$id) == "alp_rhine"){
    
    output$basin_select_clim <- renderText({"Alpine Rhine until gauge Diepoldsau"})
    output$basin_select_clic <- renderText({"Alpine Rhine until gauge Diepoldsau"})
    i = which(colnames(dis_sel) == "Diepoldsau")
    
    meta_grid <- meta_grid_diep
    my_no_col <- F
    tmed <- tmed_diep
    tmed_med <- tmed_med_diep
    pmea <- pmea_diep
    pmea_med <- pmea_med_diep
    emed <- emed_diep
    emed_med <- emed_med_diep
    smea <- smea_diep
    smea_mea <- smea_mea_diep
    tslo <- tslo_diep
    tslo_med <- tslo_med_diep
    pslo <- pslo_diep
    pslo_med <- pslo_med_diep
    eslo <- eslo_diep
    eslo_med <- eslo_med_diep
    sslo <- sslo_diep
    sslo_mea <- sslo_mea_diep
    
  }
  if(paste(basin_sel$clicked_basin$id) == "reuss"){
    
    output$basin_select_clim <- renderText({"Reuss river catchment until gauge Mellingen"})
    output$basin_select_clic <- renderText({"Reuss river catchment until gauge Mellingen"})
    i = which(colnames(dis_sel) == "Mellingen")
    
    meta_grid <- meta_grid_reus
    my_no_col <- F
    tmed <- tmed_reus
    tmed_med <- tmed_med_reus
    pmea <- pmea_reus
    pmea_med <- pmea_med_reus
    emed <- emed_reus
    emed_med <- emed_med_reus
    smea <- smea_reus
    smea_mea <- smea_mea_reus
    tslo <- tslo_reus
    tslo_med <- tslo_med_reus
    pslo <- pslo_reus
    pslo_med <- pslo_med_reus
    eslo <- eslo_reus
    eslo_med <- eslo_med_reus
    sslo <- sslo_reus
    sslo_mea <- sslo_mea_reus
    
  }
  if(paste(basin_sel$clicked_basin$id) == "aare"){
    
    output$basin_select_clim <- renderText({"Aare river catchment until gauge Brugg"})
    output$basin_select_clic <- renderText({"Aare river catchment until gauge Brugg"})
    i = which(colnames(dis_sel) == "Brugg")
    
    meta_grid <- meta_grid_aare
    my_no_col <- F
    tmed <- tmed_aare
    tmed_med <- tmed_med_aare
    pmea <- pmea_aare
    pmea_med <- pmea_med_aare
    emed <- emed_aare
    emed_med <- emed_med_aare
    smea <- smea_aare
    smea_mea <- smea_mea_aare
    tslo <- tslo_aare
    tslo_med <- tslo_med_aare
    pslo <- pslo_aare
    pslo_med <- pslo_med_aare
    eslo <- eslo_aare
    eslo_med <- eslo_med_aare
    sslo <- sslo_aare
    sslo_mea <- sslo_mea_aare
    
  }
  if(paste(basin_sel$clicked_basin$id) == "moselle"){
    
    output$basin_select_clim <- renderText({"Moselle river catchment until gauge Cochem"})
    output$basin_select_clic <- renderText({"Moselle river catchment until gauge Cochem"})
    i = which(colnames(dis_sel) == "Cochem")
    
    meta_grid <- meta_grid_mose
    my_no_col <- T
    tmed <- tmed_mose
    tmed_med <- tmed_med_mose
    pmea <- pmea_mose
    pmea_med <- pmea_med_mose
    emed <- emed_mose
    emed_med <- emed_med_mose
    smea <- smea_mose
    smea_mea <- smea_mea_mose
    tslo <- tslo_mose
    tslo_med <- tslo_med_mose
    pslo <- pslo_mose
    pslo_med <- pslo_med_mose
    eslo <- eslo_mose
    eslo_med <- eslo_med_mose
    sslo <- sslo_mose
    sslo_mea <- sslo_mea_mose
    
  }
  if(paste(basin_sel$clicked_basin$id) == "nahe"){
    
    output$basin_select_clim <- renderText({"Nahe river catchment until gauge Grolsheim"})
    output$basin_select_clic <- renderText({"Nahe river catchment until gauge Grolsheim"})
    i = which(colnames(dis_sel) == "Grolsheim")
    
    meta_grid <- meta_grid_nahe
    my_no_col <- T
    tmed <- tmed_nahe
    tmed_med <- tmed_med_nahe
    pmea <- pmea_nahe
    pmea_med <- pmea_med_nahe
    emed <- emed_nahe
    emed_med <- emed_med_nahe
    smea <- smea_nahe
    smea_mea <- smea_mea_nahe
    tslo <- tslo_nahe
    tslo_med <- tslo_med_nahe
    pslo <- pslo_nahe
    pslo_med <- pslo_med_nahe
    eslo <- eslo_nahe
    eslo_med <- eslo_med_nahe
    sslo <- sslo_nahe
    sslo_mea <- sslo_mea_nahe
    
  }
  if(paste(basin_sel$clicked_basin$id) == "lahn"){
    
    output$basin_select_clim <- renderText({"Lahn river catchment until gauge Kalkofen"})
    output$basin_select_clic <- renderText({"Lahn river catchment until gauge Kalkofen"})
    i = which(colnames(dis_sel) == "Kalkofen")
    
    meta_grid <- meta_grid_lahn
    my_no_col <- T
    tmed <- tmed_lahn
    tmed_med <- tmed_med_lahn
    pmea <- pmea_lahn
    pmea_med <- pmea_med_lahn
    emed <- emed_lahn
    emed_med <- emed_med_lahn
    smea <- smea_lahn
    smea_mea <- smea_mea_lahn
    tslo <- tslo_lahn
    tslo_med <- tslo_med_lahn
    pslo <- pslo_lahn
    pslo_med <- pslo_med_lahn
    eslo <- eslo_lahn
    eslo_med <- eslo_med_lahn
    sslo <- sslo_lahn
    sslo_mea <- sslo_mea_lahn
    
  }
  if(paste(basin_sel$clicked_basin$id) == "main"){
    
    output$basin_select_clim <- renderText({"Main river catchment until gauge Frankfurt"})
    output$basin_select_clic <- renderText({"Main river catchment until gauge Frankfurt"})
    i = which(colnames(dis_sel) == "Frankfurt")
    
    meta_grid <- meta_grid_main
    my_no_col <- T
    tmed <- tmed_main
    tmed_med <- tmed_med_main
    pmea <- pmea_main
    pmea_med <- pmea_med_main
    emed <- emed_main
    emed_med <- emed_med_main
    smea <- smea_main
    smea_mea <- smea_mea_main
    tslo <- tslo_main
    tslo_med <- tslo_med_main
    pslo <- pslo_main
    pslo_med <- pslo_med_main
    eslo <- eslo_main
    eslo_med <- eslo_med_main
    sslo <- sslo_main
    sslo_mea <- sslo_mea_main
    
  }
  if(paste(basin_sel$clicked_basin$id) == "neckar"){
    
    output$basin_select_clim <- renderText({"Neckar river catchment until gauge Rockenau"})
    output$basin_select_clic <- renderText({"Neckar river catchment until gauge Rockenau"})
    i = which(colnames(dis_sel) == "Rockenau")
    
    meta_grid <- meta_grid_neck
    my_no_col <- T
    tmed <- tmed_neck
    tmed_med <- tmed_med_neck
    pmea <- pmea_neck
    pmea_med <- pmea_med_neck
    emed <- emed_neck
    emed_med <- emed_med_neck
    smea <- smea_neck
    smea_mea <- smea_mea_neck
    tslo <- tslo_neck
    tslo_med <- tslo_med_neck
    pslo <- pslo_neck
    pslo_med <- pslo_med_neck
    eslo <- eslo_neck
    eslo_med <- eslo_med_neck
    sslo <- sslo_neck
    sslo_mea <- sslo_mea_neck
    
  }
  
  f_plot_disc <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
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
               main      = paste0(colnames(dis_sel)[i], ": Quantile exceedance probab. [%]"),
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
               main <- paste0(colnames(dis_sel)[i], ": Trend moving quantile exceedance prob. [%/dec]"),
               ylab <- "Quantile",
               margins_1 = c(1.6,2.5,1.6,0),
               margins_2 = c(1.6,0.5,1.6,1.7)
    )
    
  }
  f_plot_clim <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(1,3,5,7,
                    2,4,6,8), 4, 2), widths=c(1, 1), heights=rep(1, 4))
    
    plot_cycl_elev(data_in = tmed, data_mk = tmed, data_in_me = tmed_med,
                   data_meta = meta_grid, main_text = paste0("Temperature [°C]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = pmea, data_mk = pmea, data_in_me = pmea_med,
                   data_meta = meta_grid, main_text = paste0("Precipitation [mm]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = emed, data_mk = emed, data_in_me = emed_med,
                   data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = smea, data_mk = smea, data_in_me = smea_mea,
                   data_meta = meta_grid, main_text = paste0("Snow water equiv. mod. [mm]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  }
  f_plot_clic <- function(){
    
    par(bg = "grey92")
    par(oma=c(0,0,0,0))
    par(family="serif")
    
    layout(matrix(c(1,3,5,7,
                    2,4,6,8), 4, 2), widths=c(1, 1), heights=rep(1, 4))
    
    plot_cycl_elev(data_in = tslo, data_mk = tslo, data_in_me = tslo_med,
                   data_meta = meta_grid, main_text = paste0("Temperature [°C/dec]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = pslo, data_mk = pslo, data_in_me = pslo_med,
                   data_meta = meta_grid, main_text = paste0("Precipitation [mm/dec]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = eslo, data_mk = eslo, data_in_me = eslo_med,
                   data_meta = meta_grid, main_text = paste0("Evapotranspiration [mm/dec]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    
    plot_cycl_elev(data_in = sslo, data_mk = sslo, data_in_me = sslo_mea,
                   data_meta = meta_grid, main_text = paste0("Snow water equiv. mod. [mm/dec]"),
                   margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                   no_col = my_no_col, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                   smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
  }
  
  output$plot_disc <-  renderPlot({f_plot_disc()})
  output$plot_clim <-  renderPlot({f_plot_clim()})
  output$plot_clic <-  renderPlot({f_plot_clic()})
  
})

}

#run_app####
shiny::shinyApp( ui = ui, server = server)
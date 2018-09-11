###

#Rhine river basin flow changes: Shiny app
#Erwin Rottler, Universtiy of Potsdam
#September 2019

###

#settings####

# devtools::install_github("alptempr", "ERottler")
pacman::p_load(shiny, leaflet, sp, raster, viridis, shinythemes, 
               alptempr, htmltools, zoo)


base_dir <- "u:/RhineFlow/rhine_obs/R/rhine_flow/rhine_obs_app/"
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

load(paste0(base_dir, "data/rhine_flow_app.RData"))

#user_interface####
ui <- fluidPage(theme = shinytheme("superhero"),
  # place the contents inside a box
  fluidPage(
    width = 12,
    hr(),
    tags$h3("Rhine flood seasonality"),
    tags$h5("Erwin Rottler, Berry Boessenkool, Till Franke, Gerd Bürder and Axel Bronstert"),
    hr(),
    column(
      width = 6,
      wellPanel(
        tags$h5("Click to select sub-basin:"),
        leaflet::leafletOutput(outputId = "map", height = 350)
      ),
      wellPanel(
      checkboxInput(inputId = "condi_clim", label = strong("Climate")),
      conditionalPanel(
        condition = "input.condi_clim == true",
          plotOutput(outputId = "plot_clim", height = 500)
        )#conditional Panel
      )#wellPanel
    ),
    # separate the box by a column
    column(
      width = 6,
      wellPanel(
        tags$h5("Discharge:"),
        plotOutput(outputId = "plot_disc", height = 350)
      ),
      wellPanel(
        checkboxInput(inputId = "condi_clic", label = strong("Climatic changes")),
        conditionalPanel(
          condition = "input.condi_clic == true",
          plotOutput(outputId = "plot_clic", height = 500)
        )#conditional Panel
      )#wellPanel
    ),
    column(
      width = 12,
    wellPanel(
      tags$p("If you have questions, suggestions or ideas you want to share,
                   please do not hesitate to contact us: rottler(at)uni-potsdam.de"),
      tags$p("This researchwas funded by Deutsche Forschungsgemeinschaft (DFG) within the
           graduate research training group NatRiskChange (GRK 2043/1) at 
           the University of Potsdam.")
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
                label = "Alpine Rhine measured at gauge Diepoldsau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_reus, layerId = "reuss", fillOpacity = 0.4, group = "click.list",
                label = "Reuss catchment measured at gauge Mellingen",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_aare, layerId = "aare", fillOpacity = 0.4, group = "click.list",
                label = "Aare catchment measured at gauge Brugg",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_mose, layerId = "moselle",fillOpacity = 0.4, group = "click.list",
                label = "Moselle catchment measured at gauge Cochem",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_neck, layerId = "neckar", fillOpacity = 0.4, group = "click.list",
                label = "Neckar catchment measured at gauge Rockenau",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_nahe, layerId = "nahe", fillOpacity = 0.4, group = "click.list",
                label = "Nahe catchment measured at gauge Grolsheim",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_lahn, layerId = "lahn", fillOpacity = 0.4, group = "click.list",
                label = "Lahn catchment measured at gauge Kalkofen",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addPolygons(data = sh_main, layerId = "main", fillOpacity = 0.4, group = "click.list",
                label = "Main catchment measured at gauge Frankfurt",
                highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE),
                labelOptions = labelOptions(noHide = F, direction = "bottom"))%>%
    addCircleMarkers(data = sh_griv, label = ~name, color = "black", stroke = F, fillOpacity = 0.4,
                     labelOptions = labelOptions(noHide = F, direction = "bottom")) %>%
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

#Start plots
f_plot_disc <- function(){
  par(bg = bg_color)
  par(oma=c(0,0,0,0))
  par(family="serif")
  
  i = which(colnames(dis_sel) == "Basel_Rheinhalle")
  
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

#Selection gauge via click in map
gauge_sel <-  shiny::reactiveValues(clicked_gauge = "Basel")

observeEvent(input$map_marker_click,{
  
  gauge_sel$clicked_gauge <- input$map_marker_click
  # print(gauge_sel$clicked_gauge$lat)
  
  if(paste(gauge_sel$clicked_gauge$lat) == "47.55927"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Basel_Rheinhalle")
      
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
    f_plot_clim <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    f_plot_clic <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "49.6318"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Worms")
      
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
    f_plot_clim <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    f_plot_clic <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.08561"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Kaub")
      
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
    f_plot_clim <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    f_plot_clic <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    
  }
  if(paste(gauge_sel$clicked_gauge$lat) == "50.937"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Koeln")
      
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
    f_plot_clim <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    f_plot_clic <- function(){
      plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
      mtext("Plot not available for current selection.", line = -1)
    }
    
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
    
    f_plot_disc <- function(){
      par(bg = "white")
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Diepoldsau")
      
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
    f_plot_clim <- function(){
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_diep, data_mk = tmed_diep, data_in_me = tmed_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_diep, data_mk = pmea_diep, data_in_me = pmea_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_diep, data_mk = emed_diep, data_in_me = emed_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_diep, data_mk = tzer_diep, data_in_me = tzer_mea_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_diep, data_mk = lfra_diep, data_in_me = lfra_mea_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    f_plot_clic <- function(){
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_diep, data_mk = tslo_diep, data_in_me = tslo_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_diep, data_mk = pslo_diep, data_in_me = pslo_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_diep, data_mk = eslo_diep, data_in_me = eslo_med_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_diep, data_mk = tzes_diep, data_in_me = tzes_mea_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_diep, data_mk = lfrs_diep, data_in_me = lfrs_mea_diep,
                     data_meta = meta_grid_diep, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "reuss"){
    
    f_plot_disc <- function(){
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Mellingen")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_reuss, data_mk = tmed_reuss, data_in_me = tmed_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_reuss, data_mk = pmea_reuss, data_in_me = pmea_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_reuss, data_mk = emed_reuss, data_in_me = emed_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_reuss, data_mk = tzer_reuss, data_in_me = tzer_mea_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_reuss, data_mk = lfra_reuss, data_in_me = lfra_mea_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_reuss, data_mk = tslo_reuss, data_in_me = tslo_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_reuss, data_mk = pslo_reuss, data_in_me = pslo_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_reuss, data_mk = eslo_reuss, data_in_me = eslo_med_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_reuss, data_mk = tzes_reuss, data_in_me = tzes_mea_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_reuss, data_mk = lfrs_reuss, data_in_me = lfrs_mea_reuss,
                     data_meta = meta_grid_reuss, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "aare"){
    
    f_plot_disc <- function(){
     
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Brugg")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_aare, data_mk = tmed_aare, data_in_me = tmed_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_aare, data_mk = pmea_aare, data_in_me = pmea_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_aare, data_mk = emed_aare, data_in_me = emed_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_aare, data_mk = tzer_aare, data_in_me = tzer_mea_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_aare, data_mk = lfra_aare, data_in_me = lfra_mea_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_aare, data_mk = tslo_aare, data_in_me = tslo_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_aare, data_mk = pslo_aare, data_in_me = pslo_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_aare, data_mk = eslo_aare, data_in_me = eslo_med_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Evapotranspiration [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_aare, data_mk = tzes_aare, data_in_me = tzes_mea_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_aare, data_mk = lfrs_aare, data_in_me = lfrs_mea_aare,
                     data_meta = meta_grid_aare, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "moselle"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Cochem")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_mose, data_mk = tmed_mose, data_in_me = tmed_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_mose, data_mk = pmea_mose, data_in_me = pmea_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_mose, data_mk = emed_mose, data_in_me = emed_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_mose, data_mk = tzer_mose, data_in_me = tzer_mea_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_mose, data_mk = lfra_mose, data_in_me = lfra_mea_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_mose, data_mk = tslo_mose, data_in_me = tslo_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_mose, data_mk = pslo_mose, data_in_me = pslo_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_mose, data_mk = eslo_mose, data_in_me = eslo_med_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_mose, data_mk = tzes_mose, data_in_me = tzes_mea_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_mose, data_mk = lfrs_mose, data_in_me = lfrs_mea_mose,
                     data_meta = meta_grid_mose, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "nahe"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Grolsheim")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_nahe, data_mk = tmed_nahe, data_in_me = tmed_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_nahe, data_mk = pmea_nahe, data_in_me = pmea_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_nahe, data_mk = emed_nahe, data_in_me = emed_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_nahe, data_mk = tzer_nahe, data_in_me = tzer_mea_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_nahe, data_mk = lfra_nahe, data_in_me = lfra_mea_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_nahe, data_mk = tslo_nahe, data_in_me = tslo_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_nahe, data_mk = pslo_nahe, data_in_me = pslo_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_nahe, data_mk = eslo_nahe, data_in_me = eslo_med_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_nahe, data_mk = tzes_nahe, data_in_me = tzes_mea_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_nahe, data_mk = lfrs_nahe, data_in_me = lfrs_mea_nahe,
                     data_meta = meta_grid_nahe, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "lahn"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Kalkofen")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_lahn, data_mk = tmed_lahn, data_in_me = tmed_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_lahn, data_mk = pmea_lahn, data_in_me = pmea_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_lahn, data_mk = emed_lahn, data_in_me = emed_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_lahn, data_mk = tzer_lahn, data_in_me = tzer_mea_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_lahn, data_mk = lfra_lahn, data_in_me = lfra_mea_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_lahn, data_mk = tslo_lahn, data_in_me = tslo_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_lahn, data_mk = pslo_lahn, data_in_me = pslo_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_lahn, data_mk = eslo_lahn, data_in_me = eslo_med_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_lahn, data_mk = tzes_lahn, data_in_me = tzes_mea_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_lahn, data_mk = lfrs_lahn, data_in_me = lfrs_mea_lahn,
                     data_meta = meta_grid_lahn, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "main"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Frankfurt")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_main, data_mk = tmed_main, data_in_me = tmed_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_main, data_mk = pmea_main, data_in_me = pmea_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_main, data_mk = emed_main, data_in_me = emed_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Evapotranspiration [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_main, data_mk = tzer_main, data_in_me = tzer_mea_main,
                     data_meta = meta_grid_main, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_main, data_mk = lfra_main, data_in_me = lfra_mea_main,
                     data_meta = meta_grid_main, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_main, data_mk = tslo_main, data_in_me = tslo_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_main, data_mk = pslo_main, data_in_me = pslo_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_main, data_mk = eslo_main, data_in_me = eslo_med_main,
                     data_meta = meta_grid_main, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_main, data_mk = tzes_main, data_in_me = tzes_mea_main,
                     data_meta = meta_grid_main, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_main, data_mk = lfrs_main, data_in_me = lfrs_mea_main,
                     data_meta = meta_grid_main, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  if(paste(basin_sel$clicked_basin$id) == "neckar"){
    
    f_plot_disc <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      i = which(colnames(dis_sel) == "Rockenau")
      
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
    f_plot_clim <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tmed_neck, data_mk = tmed_neck, data_in_me = tmed_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Temperature [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pmea_neck, data_mk = pmea_neck, data_in_me = pmea_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Precipitation [mm]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = emed_neck, data_mk = emed_neck, data_in_me = emed_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Evapotranspiration [°C]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzer_neck, data_mk = tzer_neck, data_in_me = tzer_mea_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Temp. days above zero [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfra_neck, data_mk = lfra_neck, data_in_me = lfra_mea_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Liquid Frac. Prec. [%]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      
    }
    f_plot_clic <- function(){
      
      par(oma=c(0,0,0,0))
      par(family="serif")
      
      layout(matrix(c(1,3,5,7,9,
                      2,4,6,8,10), 5, 2), widths=c(1, 1), heights=rep(1, 5))
      
      plot_cycl_elev(data_in = tslo_neck, data_mk = tslo_neck, data_in_me = tslo_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Temperature [°C/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = pslo_neck, data_mk = pslo_neck, data_in_me = pslo_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Precipitation [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = eslo_neck, data_mk = eslo_neck, data_in_me = eslo_med_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Evapotranspiration [mm/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = tzes_neck, data_mk = tzes_neck, data_in_me = tzes_mea_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Temp. days above zero [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
      plot_cycl_elev(data_in = lfrs_neck, data_mk = lfrs_neck, data_in_me = lfrs_mea_neck,
                     data_meta = meta_grid_neck, main_text = paste0("Liquid Frac. Prec. [%/dec]"),
                     margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                     no_col = T, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                     smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
      
    }
    
  }
  
  output$plot_disc <-  renderPlot({f_plot_disc()})
  output$plot_clim <-  renderPlot({f_plot_clim()})
  output$plot_clic <-  renderPlot({f_plot_clic()})
  
})


}



#run_app####

shiny::shinyApp( ui = ui, server = server)





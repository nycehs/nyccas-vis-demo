##Script to create leaflet maps of NYCCAS surfaces for Y13 annual report
##Created by: Masha Pitiranggon on 1/27/23

library(dplyr)
library(leaflet)
library(raster)
library(htmlwidgets) ## for exporting leaflet maps as html files

dir_raster <- "X:/EODEShare/NYCCAS/Public and Stakeholder information/13-yr Report/Rasters/2021_rasters/"

# BC ----------------------------------------------------------------------
bc_raster <- raster(paste0(dir_raster, "bc2021.tif"))

# cellStats(bc_raster, range)

bc_vals <- data.frame(rasterToPoints(bc_raster))

palRaster_bc <- colorNumeric(c("Yellow", "Red"), domain = bc_vals$bc2021, na.color = "transparent", 
                             reverse = FALSE)

bc_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(bc_raster,
                 colors = palRaster_bc,
                 opacity = 1) %>%
  addLegend(pal = palRaster_bc,
            values = bc_vals$bc2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Black Carbon (BC)<br/>",
                                           "<em>(in µg/m<sup>3</sup>)</em>")),
            opacity = 1)

saveWidget(bc_leaflet, file = paste0(dir_raster, "Leaflets/bc_2021_org.html"))

## with min/max breaks 
bc_raster2 <- raster(paste0(dir_raster, "bc2021.tif"))

bc_raster2[bc_raster2 < 0.5] <- 0.5
bc_raster2[bc_raster2 > 1.0] <- 1.0

bc_vals2 <- data.frame(rasterToPoints(bc_raster2))

palRaster_bc_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(0.4,1.1), na.color = "transparent", 
                                  reverse = FALSE)

bclabelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = ">1"
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = "<0.5" 
  paste0(cuts[-n], cuts[-1])
}


bc_leaflet2 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(bc_raster2,
                 colors = palRaster_bc_cuts,
                 opacity = 1) %>%
  addLegend(pal = palRaster_bc_cuts,
            values = bc_vals2$bc2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Black Carbon (BC)<br/>",
                                           "<em>(in µg/m<sup>3</sup>)</em>")),
            opacity = 1,
            labFormat = bclabelfunction)

saveWidget(bc_leaflet2, file = paste0(dir_raster, "Leaflets/bc_2021.html"))

# NO ----------------------------------------------------------------------
no_raster <- raster(paste0(dir_raster, "no2021.tif"))

no_vals <- data.frame(rasterToPoints(no_raster))

palRaster_no <- colorNumeric(c("Yellow", "Red"), domain = no_vals$no2021, na.color = "transparent", 
                             reverse = FALSE)

no_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(no_raster,
                 colors = palRaster_no,
                 opacity = 1) %>%
  addLegend(pal = palRaster_no,
            values = no_vals$no2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitric Oxide (NO)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1)

saveWidget(no_leaflet, file = paste0(dir_raster, "Leaflets/no_2021_org.html"))

## with min/max breaks 
no_raster2 <- raster(paste0(dir_raster, "no2021.tif"))

no_raster2[no_raster2 < 10] <- 10
no_raster2[no_raster2 > 22] <- 22

no_vals2 <- data.frame(rasterToPoints(no_raster2))

palRaster_no_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(9.9,22.1), na.color = "transparent", 
                              reverse = FALSE)

mylabelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = ">22"
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = "<10" 
  paste0(cuts[-n], cuts[-1])
}


no_leaflet2 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(no_raster2,
                 colors = palRaster_no_cuts,
                 opacity = 1) %>%
  addLegend(pal = palRaster_no_cuts,
            values = no_vals2$no2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitric Oxide (NO)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1,
            labFormat = mylabelfunction)

saveWidget(no_leaflet2, file = paste0(dir_raster, "Leaflets/no_2021.html"))

# NO2 ----------------------------------------------------------------------
no2_raster <- raster(paste0(dir_raster, "no22021.tif"))

no2_vals <- data.frame(rasterToPoints(no2_raster))

palRaster_no2 <- colorNumeric(c("Yellow", "Red"), domain = no2_vals$no22021, na.color = "transparent", 
                              reverse = FALSE)

no2_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(no2_raster,
                 colors = palRaster_no2,
                 opacity = 1) %>%
  addLegend(pal = palRaster_no2,
            values = no2_vals$no22021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitrogen Dioxide (NO<sub>2</sub>)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1)

saveWidget(no2_leaflet, file = paste0(dir_raster, "Leaflets/no2_2021_org.html"))

## with min/max breaks 
no2_raster2 <- raster(paste0(dir_raster, "no22021.tif"))

no2_raster2[no2_raster2 < 11.1] <- 11.1
no2_raster2[no2_raster2 > 21.6] <- 21.6

no2_vals2 <- data.frame(rasterToPoints(no2_raster2))

palRaster_no2_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(11.0,21.7), na.color = "transparent", 
                                  reverse = FALSE)

no2labelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = ">21.6"
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = "<11.1" 
  paste0(cuts[-n], cuts[-1])
}


no2_leaflet2 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(no2_raster2,
                 colors = palRaster_no2_cuts,
                 opacity = 1) %>%
  addLegend(pal = palRaster_no2_cuts,
            values = no2_vals2$no22021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitrogen Dioxide (NO<sub>2</sub>)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1,
            labFormat = no2labelfunction)

saveWidget(no2_leaflet2, file = paste0(dir_raster, "Leaflets/no2_2021.html"))

# O3 ----------------------------------------------------------------------
o3_raster <- raster(paste0(dir_raster, "o32021.tif"))

o3_vals <- data.frame(rasterToPoints(o3_raster))

palRaster_o3 <- colorNumeric(c("Yellow", "Red"), domain = o3_vals$o32021, na.color = "transparent", 
                             reverse = FALSE)

o3_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(o3_raster,
                 colors = palRaster_o3,
                 opacity = 1) %>%
  addLegend(pal = palRaster_o3,
            values = o3_vals$o32021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Ozone (O<sub>3</sub>)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1)

saveWidget(o3_leaflet, file = paste0(dir_raster, "Leaflets/o3_2021_org.html"))

## with min/max breaks 
o3_raster2 <- raster(paste0(dir_raster, "o32021.tif"))

o3_raster2[o3_raster2 < 27] <- 27
o3_raster2[o3_raster2 > 32] <- 32

o3_vals2 <- data.frame(rasterToPoints(o3_raster2))

palRaster_o3_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(26,33), na.color = "transparent", 
                                   reverse = FALSE)

o3labelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = ">32"
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = "<27" 
  paste0(cuts[-n], cuts[-1])
}


o3_leaflet2 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(o3_raster2,
                 colors = palRaster_o3_cuts,
                 opacity = 1) %>%
  addLegend(pal = palRaster_o3_cuts,
            values = o3_vals2$o32021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Ozone (O<sub>3</sub>)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1,
            labFormat = o3labelfunction)

saveWidget(o3_leaflet2, file = paste0(dir_raster, "Leaflets/o3_2021.html"))

# PM2.5 ----------------------------------------------------------------------
pm_raster <- raster(paste0(dir_raster, "pm2021.tif"))

pm_vals <- data.frame(rasterToPoints(pm_raster))

palRaster_pm <- colorNumeric(c("Yellow", "Red"), domain = pm_vals$pm2021, na.color = "transparent", 
                             reverse = FALSE)

pm_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(pm_raster,
                 colors = palRaster_pm,
                 opacity = 1) %>%
  addLegend(pal = palRaster_pm,
            values = pm_vals$pm2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Fine Particles (PM<sub>2.5</sub>)<br/>",
                                           "<em>(in µg/m<sup>3</sup>)</em>")),
            opacity = 1)

saveWidget(pm_leaflet, file = paste0(dir_raster, "Leaflets/pm25_2021_org.html"))

## with min/max breaks 
pm_raster2 <- raster(paste0(dir_raster, "pm2021.tif"))

pm_raster2[pm_raster2 < 6] <- 6
pm_raster2[pm_raster2 > 8] <- 8

pm_vals2 <- data.frame(rasterToPoints(pm_raster2))

palRaster_pm_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(5.9,8.1), na.color = "transparent", 
                                  reverse = FALSE)

pmlabelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = ">8"
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = "<6" 
  paste0(cuts[-n], cuts[-1])
}


pm_leaflet2 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(pm_raster2,
                 colors = palRaster_pm_cuts,
                 opacity = 1) %>%
  addLegend(pal = palRaster_pm_cuts,
            values = pm_vals2$pm2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Fine Particles (PM<sub>2.5</sub>)<br/>",
                                           "<em>(in µg/m<sup>3</sup>)</em>")),
            opacity = 1,
            labFormat = pmlabelfunction)

saveWidget(pm_leaflet2, file = paste0(dir_raster, "Leaflets/pm25_2021.html"))

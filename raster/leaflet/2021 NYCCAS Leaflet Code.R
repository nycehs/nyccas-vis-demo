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

palRaster_bc <- colorNumeric("YlOrRd", domain = bc_vals$bc2021, na.color = "transparent", 
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

saveWidget(bc_leaflet, file = paste0(dir_raster, "Leaflets/bc_2021.html"))

# NO ----------------------------------------------------------------------
no_raster <- raster(paste0(dir_raster, "no2021.tif"))

no_vals <- data.frame(rasterToPoints(no_raster))

palRaster_no <- colorNumeric("YlOrRd", domain = no_vals$no2021, na.color = "transparent", 
                             reverse = FALSE)
binpalRaster_no <- colorBin("YlOrRd", domain = no_vals$no2021, na.color = "transparent", 
                             reverse = FALSE, bins = 5, pretty = FALSE)
qpalRaster_no <- colorQuantile("YlOrRd", domain = no_vals$no2021, na.color = "transparent", 
                               reverse = FALSE, n = 9)

no_leaflet <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addRasterImage(no_raster,
                 # colors = palRaster_no,
                 # colors = binpalRaster_no,
                 colors = qpalRaster_no,
                 opacity = 1) %>%
  addLegend(#pal = palRaster_no,
            # pal = binpalRaster_no,
            pal = qpalRaster_no ,
            values = no_vals$no2021,
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitric Oxide (NO)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1)

saveWidget(no_leaflet, file = paste0(dir_raster, "Leaflets/no_2021.html"))

library(tmap)

no_map <- tm_shape(no_raster) +
  tm_raster(style = "cont", 
            n = 5,
            breaks = c(9.9, 22.1),
            labels = c("<10", ">22"),
            palette = "YlOrRd"
            # ,
            # title = legend.title,
            # legend.format = legend.format
            ) +
#   tm_legend(legend.outside = F)
# +
  tm_layout(#legend.text.size = 1,
            # main.title = title,
            # main.title.size = 1,
            legend.show = F,
            # legend.width = 0.9,
            bg.color = "white")
# +
#   tm_shape(nycbound) +
#   tm_borders()

no_tmap_ll <- tmap_leaflet(no_map) %>%
  addProviderTiles(provider = "CartoDB") %>%
  addLegend(pal = palRaster_no,
            values = c(9.9, 22.1),
            labels = c("<10", ">22"),
            title = htmltools::HTML(paste0("Annual Average<br/>",
                                           "Nitric Oxide (NO)<br/>",
                                           "<em>(in ppb)</em>")),
            opacity = 1)


# NO2 ----------------------------------------------------------------------
no2_raster <- raster(paste0(dir_raster, "no22021.tif"))

no2_vals <- data.frame(rasterToPoints(no2_raster))

palRaster_no2 <- colorNumeric("YlOrRd", domain = no2_vals$no22021, na.color = "transparent", 
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

saveWidget(no2_leaflet, file = paste0(dir_raster, "Leaflets/no2_2021.html"))

# O3 ----------------------------------------------------------------------
o3_raster <- raster(paste0(dir_raster, "o32021.tif"))

o3_vals <- data.frame(rasterToPoints(o3_raster))

palRaster_o3 <- colorNumeric("YlOrRd", domain = o3_vals$o32021, na.color = "transparent", 
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

saveWidget(o3_leaflet, file = paste0(dir_raster, "Leaflets/o3_2021.html"))


# PM2.5 ----------------------------------------------------------------------
pm_raster <- raster(paste0(dir_raster, "pm2021.tif"))

pm_vals <- data.frame(rasterToPoints(pm_raster))

palRaster_pm <- colorNumeric("YlOrRd", domain = pm_vals$pm2021, na.color = "transparent", 
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

saveWidget(pm_leaflet, file = paste0(dir_raster, "Leaflets/pm25_2021.html"))


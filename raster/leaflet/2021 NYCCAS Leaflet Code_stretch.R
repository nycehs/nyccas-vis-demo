##Script to create leaflet maps of NYCCAS surfaces for Y13 annual report
##Created by: Masha Pitiranggon on 1/27/23

library(dplyr)
library(sf)
library(leaflet)
library(raster)
library(htmlwidgets) ## for exporting leaflet maps as html files
library(stringr)

dir_raster <- "X:/EODEShare/NYCCAS/Public and Stakeholder information/13-yr Report/Rasters/2021_rasters/"

airport_mask <- st_read("X:/EODEShare/NYCCAS/Public and Stakeholder information/13-yr Report/Rasters/GIS/Airport.shp")
airport_mask_lon_lat <- st_transform(airport_mask, 4326)

# For loop to create leaflet maps of each pollutant -----------------------
## create vector of file names
to_process <- list.files(dir_raster, pattern = ".tif$")

#helper function to customize leaflet legend labeling
labelfunction <- function(type, cuts, p){
  n = length(cuts) 
  cuts[n] = paste0(">", round(ul, 1))
  for (i in 2:(n-1)){cuts[i] = ""} 
  cuts[1] = paste0("<", round(ll, 1)) 
  paste0(cuts[-n], cuts[-1])
}


for(i in 1:length(to_process)) {
  
  raster <- raster(paste0(dir_raster, to_process[i]))
  
  center <- cellStats(raster, stat = 'mean')
  stdev <- cellStats(raster, stat = 'sd')
  ll <- center - (2*stdev)
  ul <- center + (2*stdev)
  
  ## with min/max breaks (based on 2X SD)
  raster[raster > ul] <- ul
  
  if(ll > 0) {
  raster[raster < ll] <- ll
  } else {
    ## there are no negative values in 2021 data, but ll can be < 0, need to adjust ll in this case
    ll <- center - (stdev)
    raster[raster < ll] <- ll
  }
  
  raster_vals <- data.frame(rasterToPoints(raster)) %>%
    dplyr::select(ends_with("2021"))
  
  palRaster_cuts <- colorNumeric(c("Yellow", "Red"), domain = c(ll-0.1,ul+0.1), na.color = "transparent", 
                                    reverse = FALSE)
  
  
  map <- leaflet() %>%
    addProviderTiles(provider = "CartoDB") %>%
    addRasterImage(raster,
                   colors = palRaster_cuts,
                   opacity = 1) %>%
    addPolygons(data = airport_mask_lon_lat,
                fillColor = "gray",
                weight = 0.5,
                opacity = 1,
                color = "gray",
                fillOpacity = 1) %>%
    { if(to_process[i] == "bc2021.tif")
      addLegend(., pal = palRaster_cuts,
                values = raster_vals[,1],
                title = htmltools::HTML(paste0("Annual Average<br/>",
                                               "Black Carbon (BC)<br/>",
                                               "<em>(in µg/m<sup>3</sup>)</em>")),
                opacity = 1,
                labFormat = labelfunction)
      else if(to_process[i] == "no2021.tif")
        addLegend(., pal = palRaster_cuts,
                  values = raster_vals[,1],
                  title = htmltools::HTML(paste0("Annual Average<br/>",
                                                 "Nitric Oxide (NO)<br/>",
                                                 "<em>(in ppb)</em>")),
                  opacity = 1,
                  labFormat = labelfunction)
      else if(to_process[i] == "no22021.tif")
        addLegend(., pal = palRaster_cuts,
                  values = raster_vals[,1],
                  title = htmltools::HTML(paste0("Annual Average<br/>",
                                                 "Nitrogen Dioxide (NO<sub>2</sub>)<br/>",
                                                 "<em>(in ppb)</em>")),
                  opacity = 1,
                  labFormat = labelfunction)
      else if(to_process[i] == "o32021.tif")
        addLegend(., pal = palRaster_cuts,
                  values = raster_vals[,1],
                  title = htmltools::HTML(paste0("Summer Average<br/>",
                                                 "Ozone (O<sub>3</sub>)<br/>",
                                                 "<em>(in ppb)</em>")),
                  opacity = 1,
                  labFormat = labelfunction)
      else if(to_process[i] == "pm2021.tif")
        addLegend(., pal = palRaster_cuts,
                  values = raster_vals[,1],
                  title = htmltools::HTML(paste0("Annual Average<br/>",
                                                 "Fine Particles (PM<sub>2.5</sub>)<br/>",
                                                 "<em>(in µg/m<sup>3</sup>)</em>")),
                  opacity = 1,
                  labFormat = labelfunction)
    } 
  
  saveWidget(map, file = paste0(dir_raster, "Leaflets/", str_remove(to_process[i], "\\.tif"), ".html"))
  
}




        /*
        georaster for leaflet: https://github.com/GeoTIFF/georaster-layer-for-leaflet
        requires `npm install georaster-layer-for-leaflet` and/or `npm install georaster`

        */
        var parse_georaster = require("georaster");
        var GeoRasterLayer = require("georaster-layer-for-leaflet");
        

        var map = L.map('map').setView([40.715554,-74.0026642],11); // [Lat,Long],Zoom
        L.tileLayer('https://api.maptiler.com/maps/basic-v2/{z}/{x}/{y}.png?key=dwIJ8hO2KsTMegUfEpYE',{
            attribution: '<a href="https://www.maptiler.com/copyright/" target="_blank">&copy; MapTiler</a> <a href="https://www.openstreetmap.org/copyright" target="_blank">&copy; OpenStreetMap contributors</a>',
        }).addTo(map);

        var url_to_geotiff_file = "RasterFiles/Geotiff/noAA12final.tif";

        fetch(url_to_geotiff_file)
          .then(response => response.arrayBuffer())
          .then(arrayBuffer => {
            parse_georaster(arrayBuffer).then(georaster => {
              console.log("georaster:", georaster);
        
              /*
                  GeoRasterLayer is an extension of GridLayer,
                  which means can use GridLayer options like opacity.
        
                  Just make sure to include the georaster option!
        
                  Optionally set the pixelValuesToColorFn function option to customize
                  how values for a pixel are translated to a color.
        
                  http://leafletjs.com/reference-1.2.0.html#gridlayer
              */
              var layer = new GeoRasterLayer({
                  georaster: georaster,
                  opacity: 0.7,
                  pixelValuesToColorFn: values => values[0] === 42 ? '#ffffff' : '#000000',
                  resolution: 64 // optional parameter for adjusting display resolution
              });
              layer.addTo(map);
        
              map.fitBounds(layer.getBounds());
        
          });
        });
        
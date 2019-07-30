library(raster)
library(tiler)

tiler_options(python = "python3")
tile_dir <- paste(getwd(),"/tiles/",sep="")

lsoa_sp = shapefile(x = "./raw/lsoa_sp")
event_sp = shapefile(x = "./raw/event_sp")


raster_grid <- raster(ncol=500, nrow=500)
extent(raster_grid) <- extent(lsoa_sp)
rasterized_lsoa <- rasterize(lsoa_sp, r, "mn_dstn")
plot(rp)

writeRaster(rp, filename="test.tif", format="GTiff", overwrite=TRUE)
map <- "./test.tif"
r = raster(map)
crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
pal <- colorRampPalette(c("darkblue", "lightblue"))(20)
tile(map, tile_dir, "3-8", crs = crs,col = pal, colNA = nodata)

library(leaflet)
tiles <- "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 7), width = "100%") %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addTiles(tiles, options = tileOptions(opacity = 1)) %>%
  setView(0, 52, zoom = 6) 



# pal <- colorRampPalette(c("darkblue", "lightblue"))(20)
# nodata <- "tomato"
# tile(map, tile_dir, "0-3", col = pal, colNA = nodata)



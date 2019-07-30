library(raster)
library(tiler)

tiler_options(python = "python3")
tile_dir <- paste(getwd(),"/tiles/",sep="")

lsoa_sp = shapefile(x = "./raw/lsoa_sp")
event_sp = shapefile(x = "./raw/event_sp")


raster_grid <- raster(ncol=500, nrow=500)
extent(raster_grid) <- extent(lsoa_sp)
rasterized_lsoa <- rasterize(lsoa_sp, raster_grid, "mn_dstn")
# plot(rasterized_lsoa)

writeRaster(rasterized_lsoa, filename="./raw/lsoa_mn_dstn.tif", format="GTiff", overwrite=TRUE)
mn_dsnt_tif_path <- "./raw/lsoa_mn_dstn.tif"
# rasterized_lsoa = raster(mn_dsnt_tif_path)
# pal <- colorRampPalette(c("darkblue", "lightblue"))(20)

crs <- "+proj=longlat +ellps=WGS84 +no_defs"
tile(mn_dsnt_tif_path, tile_dir, "7-8", crs = crs)

library(leaflet)
tiles <- "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 7), width = "100%") %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addTiles(tiles, options = tileOptions(opacity = 1)) %>%
  setView(0, 52, zoom = 6) 



# pal <- colorRampPalette(c("darkblue", "lightblue"))(20)
# nodata <- "tomato"
# tile(map, tile_dir, "0-3", col = pal, colNA = nodata)



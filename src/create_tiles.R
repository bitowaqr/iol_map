library(raster)
library(tiler)
library(leaflet)

tiler_options(python = "python3")
tile_dir <- paste(getwd(),"/tiles/",sep="")

lsoa_sp = shapefile(x = "./raw/lsoa_sp")
event_sp = shapefile(x = "./raw/event_sp")


raster_grid <- raster(ncol=1000, nrow=1000)
extent(raster_grid) <- extent(lsoa_sp)
lsoa_sp$mn_dstn_quantile = cut(lsoa_sp$mn_dstn,breaks = quantile(lsoa_sp$mn_dstn,probs = seq(0,1,by=0.1)))
rasterized_lsoa <- rasterize(lsoa_sp, raster_grid, "mn_dstn_quantile")
# plot(rasterized_lsoa)

writeRaster(rasterized_lsoa, filename="./raw/lsoa_mn_dstn.tif", format="GTiff", overwrite=TRUE)
mn_dsnt_tif_path <- "./raw/lsoa_mn_dstn.tif"
# rasterized_lsoa = raster(mn_dsnt_tif_path)
# pal <- colorRampPalette(c("darkblue", "lightblue"))(20)

crs <- as.character(crs(rasterized_lsoa))
# tile(mn_dsnt_tif_path, tile_dir, "5-8", crs = crs)

 
pal <- colorRampPalette(c("darkgreen","yellow","greenyellow","gold","orange", "red","darkred"))(10)
pal <- colorRampPalette(c("darkgreen","yellow","greenyellow","gold","orange", "red","darkred"))(10)
#nodata <- "white"
tile(mn_dsnt_tif_path, tile_dir, "4-11", col = pal,crs = crs)
     


 # x <- RGB(rasterized_lsoa,col=colorRampPalette(c("darkgreen","yellow","greenyellow","gold","orange", "red","darkred"))(10))
# plotRGB(x)


tiles <- "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11), width = "100%") %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addTiles(tiles, options = tileOptions(opacity = 1)) %>%
  setView(0, 52, zoom = 5) 






# load libraries
library(raster)
library(tiler)
library(leaflet)

# set tiler options
tiler_options(python = "python3")
tile_dir_mn_dstn <- paste(getwd(),"/tiles_distance/",sep="")
tile_dir_participation <- paste(getwd(),"/tiles_participation/",sep="")

# load data
lsoa_sp = shapefile(x = "./raw/lsoa_sp")

# define raster grid
raster_grid <- raster(ncol=5000, nrow=5000)
extent(raster_grid) <- extent(lsoa_sp)

# mn distance tile
lsoa_sp$mn_dstn_cuts = cut(lsoa_sp$mn_dstn,breaks = c(min(lsoa_sp$mn_dstn),1,2.5,5,10,20,max(lsoa_sp$mn_dstn)))
rasterized_lsoa_mn_dstn5 <- rasterize(lsoa_sp, raster_grid, "mn_dstn_cuts")
writeRaster(rasterized_lsoa_mn_dstn5, filename="./raw/lsoa_mn_dstn.tif", format="GTiff", overwrite=TRUE)
mn_dsnt_tif_path <- "./raw/lsoa_mn_dstn.tif"
crs <- as.character(crs(rasterized_lsoa_mn_dstn5))
pal <- colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
nodata <- "white"
tile(mn_dsnt_tif_path, tile_dir_mn_dstn, "1-11", col = pal,crs = crs)
rm("rasterized_lsoa_mn_dstn5")     

# mn distance tile
lsoa_sp$mn_dstn_cuts = cut(lsoa_sp$mn_dstn,breaks = c(min(lsoa_sp$mn_dstn),1,2.5,5,10,20,max(lsoa_sp$mn_dstn)))
rasterized_lsoa_mn_dstn5 <- rasterize(lsoa_sp, raster_grid, "mn_dstn_cuts")
writeRaster(rasterized_lsoa_mn_dstn5, filename="./raw/lsoa_mn_dstn.tif", format="GTiff", overwrite=TRUE)
mn_dsnt_tif_path <- "./raw/lsoa_mn_dstn.tif"
crs <- as.character(crs(rasterized_lsoa_mn_dstn5))
pal <- colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
nodata <- "white"
tile(mn_dsnt_tif_path, tile_dir_mn_dstn, "1-11", col = pal,crs = crs)



 # x <- RGB(rasterized_lsoa_mn_dstn5,col=colorRampPalette(c("darkgreen","yellow","greenyellow","gold","orange", "red","darkred"))(10))
# plotRGB(x)


# tiles <- "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
# leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11), width = "100%") %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addTiles(tiles, options = tileOptions(opacity = 1)) %>%
#   setView(0, 52, zoom = 5) 
# 
# 
# 
# 
# 

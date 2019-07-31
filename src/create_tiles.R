# load libraries
library(raster)
library(tiler)
library(leaflet)
library(fasterize)
library(sf)


# set tiler options
tiler_options(python = "python3")
tile_dir_mn_dstn <- paste(getwd(),"/tiles_distance/",sep="")
tile_dir_participation <- paste(getwd(),"/tiles_participation/",sep="")

# load data
lsoa_sp = shapefile(x = "./raw/lsoa_sf")
lsoa_sf = st_as_sf(lsoa_sp)

# define raster grid
raster_grid <- raster(ncol=5000, nrow=5000)
extent(raster_grid) <- extent(lsoa_sf)

# mn distance tile
lsoa_sf$mn_dstn_cuts = cut(lsoa_sf$mn_dstn,breaks = c(0,1,2.5,5,10,max(lsoa_sf$mn_dstn)))
rasterized_lsoa_mn_dstn5 <- fasterize(lsoa_sf, raster_grid, "mn_dstn_cuts")
writeRaster(rasterized_lsoa_mn_dstn5, filename="./raw/lsoa_mn_dstn.tif", format="GTiff", overwrite=TRUE)
mn_dsnt_tif_path <- "./raw/lsoa_mn_dstn.tif"
crs_dist <- as.character(crs(rasterized_lsoa_mn_dstn5))
pal <- colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
nodata <- "white"
tile(mn_dsnt_tif_path, tile_dir_mn_dstn, "4-12", col = pal,crs = crs_dist)
rm("rasterized_lsoa_mn_dstn5")     

# participation tile
# lsoa_sf$part_cuts = base::cut(lsoa_sf$rns_pm_,include.lowest = T,breaks = c(min(lsoa_sf$rns_pm_),0.1^5,0.5,1,2,max(lsoa_sf$rns_pm_)+1))
lsoa_sf$part_cuts = base::cut(lsoa_sf$rns_pm_,include.lowest = T,breaks = c(-1,0,0.5,1,2,20))
rasterized_lsoa_part5 <- rasterize(lsoa_sf, raster_grid, "part_cuts")
writeRaster(rasterized_lsoa_part5, filename="./raw/lsoa_participation_5.tif", format="GTiff", overwrite=TRUE)
part_tif_path <- "./raw/lsoa_participation_5.tif"
crs <- as.character(crs(rasterized_lsoa_part5))
pal_part <- colorRampPalette(c("darkred","red","orange","green","darkgreen"))(5)
nodata <- "white"
tile(part_tif_path, tile_dir_participation, "4-12", col = pal_part,crs = crs)
rm("rasterized_lsoa_part5")     


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

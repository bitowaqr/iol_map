# load libraries
library(raster)
library(leaflet)
library(fasterize)
library(sf)
library(tiler)

# load data
lsoa_sf = st_as_sf(shapefile(x = "./raw/lsoa_sp"))

# set tiler and grid options
  tiler_options(python = "python3")
  raster_grid = raster(ncol=5000, nrow=5000)
  extent(raster_grid) = extent(lsoa_sf)

  
#### RASTERIZE AND TILE
  
# mn distance tile
  tile_dir_mn_dstn = paste(getwd(),"/tiles_distance/",sep="")
  lsoa_sf$mn_dstn_cuts = cut(lsoa_sf$mn_dstn,breaks = c(0,1,2.5,5,10,max(lsoa_sf$mn_dstn)))
  rasterized_lsoa_mn_dstn5 = fasterize(lsoa_sf, raster_grid, "mn_dstn_cuts")
  mn_dsnt_tif_path = "./raw/lsoa_mn_dstn.tif"
  writeRaster(rasterized_lsoa_mn_dstn5, filename= mn_dsnt_tif_path, format="GTiff", overwrite=TRUE)
  crs_dist = as.character(crs(rasterized_lsoa_mn_dstn5))
  pal = colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
  nodata = "white"
  tile(mn_dsnt_tif_path, tile_dir_mn_dstn, "4-12", col = pal,crs = crs_dist)
  rm("rasterized_lsoa_mn_dstn5")     

# participation tile
  tile_dir_participation = paste(getwd(),"/tiles_participation/",sep="")
  lsoa_sf$part_cuts = base::cut(lsoa_sf$rns_pm_,include.lowest = T,breaks = c(-1,0,0.5,1,2,20))
  rasterized_lsoa_part5 = fasterize(lsoa_sf, raster_grid, "part_cuts")
  part_tif_path = "./raw/lsoa_participation_5.tif"
  writeRaster(rasterized_lsoa_part5, filename=part_tif_path, format="GTiff", overwrite=TRUE)
  crs = as.character(crs(rasterized_lsoa_part5))
  pal_part = colorRampPalette(c("darkred","red","orange","green","darkgreen"))(5)
  nodata = "white"
  tile(part_tif_path, tile_dir_participation, "4-12", col = pal_part,crs = crs)
  rm("rasterized_lsoa_part5")     
  
  
# IMD tile
  tile_dir_imd = paste(getwd(),"/tiles_imd/",sep="")
  lsoa_sf$imd_cuts = base::cut(lsoa_sf$imd_sc,include.lowest = T,breaks = quantile(lsoa_sf$imd_sc,probs=seq(0,1,by=0.2)))
  rasterized_lsoa_imd5 = fasterize(lsoa_sf, raster_grid, "imd_cuts")
  imd_tif_path = "./raw/lsoa_imd_5.tif"
  writeRaster(rasterized_lsoa_imd5, filename=imd_tif_path, format="GTiff", overwrite=TRUE)
  crs = as.character(crs(rasterized_lsoa_imd5))
  pal_imd = colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
  nodata = "white"
  tile(imd_tif_path, tile_dir_imd, "4-12", col = pal_imd,crs = crs)
  rm("rasterized_lsoa_imd5") 
  
  
# pop density tiles
  tile_dir_pop_km2 = paste(getwd(),"/tiles_pop_km2/",sep="")
  lsoa_sf$pop_km2_cuts = base::cut(lsoa_sf$pop_km2,include.lowest = T,breaks = quantile(lsoa_sf$pop_km2,probs=seq(0,1,by=0.2)))
  rasterized_lsoa_pop_km2 = fasterize(lsoa_sf, raster_grid, "pop_km2_cuts")
  pop_km2_tif_path = "./raw/lsoa_pop_km2_5.tif"
  writeRaster(rasterized_lsoa_pop_km2, filename=pop_km2_tif_path, format="GTiff", overwrite=TRUE)
  crs_pop_km2 = as.character(crs(rasterized_lsoa_pop_km2))
  pal_pop_km2 = colorRampPalette(c("darkgreen","green","orange","red","darkred"))(5)
  nodata = "white"
  tile(pop_km2_tif_path, tile_dir_pop_km2, "4-12", col = pal_pop_km2,crs = crs_pop_km2)
  rm("rasterized_lsoa_pop_km2") 


 # x = RGB(rasterized_lsoa_mn_dstn5,col=colorRampPalette(c("darkgreen","yellow","greenyellow","gold","orange", "red","darkred"))(10))
# plotRGB(x)


# tiles = "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
# leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11), width = "100%") %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addTiles(tiles, options = tileOptions(opacity = 1)) %>%
#   setView(0, 52, zoom = 5) 
# 
# 
# 
# 
# 

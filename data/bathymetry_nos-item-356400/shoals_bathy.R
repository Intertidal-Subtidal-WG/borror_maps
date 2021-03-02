#'-----------------------------
#' Quick script to use bathymetry_nos-item-356400
#' sidescan data from 2005 to get bathymetry around
#' SML
#' 
#' note, setwd to the script location to run
#'-----------------------------

library(raster)
library(mapview)

# Load the raster ####
bathy_2005 <- raster("./coast/H10001-H12000/H11296/BAG/H11296_LI_5m_MLLW_1of1.bag")
bathy_2005_wgs84 <- projectRaster(bathy_2005, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# check it out with mapview ####
mapview(bathy_2005, native.crs = TRUE)

# crop to just appledore ####
appledore_bathy <- crop(bathy_2005, e)

# write out the raster ####
writeRaster(appledore_bathy, "appledore_bathy.tiff")

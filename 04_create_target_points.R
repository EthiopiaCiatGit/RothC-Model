# ------------------------------------------------------------------------------
# Create points for crops, grassland and trees
# ------------------------------------------------------------------------------

library(raster)
library(rgdal)
library(dplyr)

rm(list = ls())

workspace <- "D:/RothC/workspace"
final <- "D:/RothC/final"

setwd(workspace)

LU_AOI <- raster("Land_Cover_10class_AOI.tif")
point <-
  rasterToPoints(
    x = LU_AOI,
    fun = function(x) {
      x == 2 | x == 3 | x == 4  #2 = crops, 3 = grassland, 4 = tree crops
    },
    spatial = TRUE
  )

setwd(final)
writeOGR(
  point,
  dsn = ".",
  layer = 'target_points',
  driver = "ESRI Shapefile",
  overwrite_layer = T
)

#-------------------------------------------------------------------------------
#Prepare the layers for the SPIN UP, warmup & forward process of the Roth C Model. 
#-------------------------------------------------------------------------------

#Spin-up

rm(list = ls())

library(raster)
library(rgdal)
library(sp)
library(dplyr)

input <- "D:/RothC/input"
workspace <- "D:/RothC/workspace"
final <- "D:/RothC/final"

# Open the shapefile and soc
setwd(input)
AOI <- readOGR("Abay_basin_bound.shp")
SOC_MAP_AOI <- raster("SOC_MAP_AOI.tif")

# Open Clay layer
setwd(workspace)
Clay_WA_AOI <- raster("Clay_WA_AOI.tif")

Clay_WA_AOI_res <-
  resample(Clay_WA_AOI, SOC_MAP_AOI, method = 'bilinear')

#Open Precipitation layer
PREC <- stack("Prec_Stack_81-00.tif")
PREC_AOI <- PREC %>% crop(AOI) %>% resample(SOC_MAP_AOI, method = 'bilinear') %>%
            mask(AOI) %>%
PREC_AOI <- stack(PREC_AOI)

#Open Temperatures layer
TEMP <- stack("Temp_Stack_81-00.tif")
TEMP_AOI <- TEMP %>% crop(AOI) %>%
            resample(SOC_MAP_AOI, method = 'bilinear') %>%
            mask(AOI)
TEMP_AOI <- stack(TEMP_AOI)

#Open Potential Evapotranspiration layer
PET <- stack("PET_Stack_81-00_CRU.tif")
PET_AOI <- PET %>% crop(AOI) %>%
           resample(SOC_MAP_AOI, method = 'bilinear') %>%
           mask(AOI)
PET_AOI <- stack(PET_AOI)

# OPen Land Use layer reclassify to FAO classes

# 0	No Data
# 1 Artificial
# 2 Croplands
# 3 Grassland
# 4 Tree Covered
# 5 Shrubs Covered
# 6 Herbaceous vegetation flooded
# 7 Mangroves
# 8 Sparse Vegetation
# 9 Baresoil
# 10 Snow and Glaciers
# 11 Waterbodies
# 12 TreeCrops

LU_AOI <- raster("Land_Cover_10class_AOI.tif")

# Open Vegetation Cover layer
Cov_AOI <- stack('NDVI_Cov_stack_AOI.tif')

# Use Land use layer to convert it to DR layer

#DPM/RPM (decomplosable vs resistant plant material)
#(1) Most agricultural crops and improved grassland and tree crops 1.44
#(2) Unimproved grassland and schrub 0.67
#(3) Deciduous and tropical woodland 0.25
# my lu -  foa lu
#   0 = 0	  No Data
#	10 = 1 Artificial
#	 4 = 2 Croplands
#	 5 = 3 Grassland
#	1  = 4 Tree Covered
#	2,3  = 5 Shrubs Covered
#	 7 = 6 Herbaceous vegetation flooded
#	7 = 7 Mangroves
#	3 = 8 Sparse Vegetation
#	 6 = 9 Baresoil
#	8 = 10 Snow and Glaciers
#	 8 = 11 Waterbodies
#	 4  = 12 Treecrops
# 4 = 13 Paddy fields(rice/ flooded crops)
DR1 <-
  (LU_AOI == 4 |
     LU_AOI == 4 |
     LU_AOI == 4) * 1.44 + (LU_AOI == 1) * 0.25 + (LU_AOI == 3 |
                                                     LU_AOI == 2 | LU_AOI == 7 | LU_AOI == 2) * 0.67

# STACK all layers
Stack_Set_AOI <-
  stack(SOC_MAP_AOI,
        Clay_WA_AOI_res,
        TEMP_AOI,
        PREC_AOI,
        PET_AOI,
        DR1,
        LU_AOI,
        Cov_AOI)

setwd(final)
writeRaster(
  Stack_Set_AOI,
  filename = ("Stack_Set_SPIN_UP_AOI.tif"),
  format = "GTiff",
  overwrite = TRUE
)

#-------------------------------------------------------------------------------
#Warmup no landuse change
nWUP <- 18
LU_Stack <- stack(replicate(nWUP, LU_AOI))
DR2 <-
  (LU_AOI == 4) * 1.44 + (LU_AOI == 1) * 0.25 + (LU_AOI == 5 |
                                                   LU_AOI == 7 | LU_AOI == 2) * 0.67
DR_Stack <- LU_Stack

for (i in 1:nlayers(LU_Stack)) {
  DR_Stack[[i]] <-
    (LU_Stack[[i]] == 4) * 1.44 + (LU_Stack[[i]] == 1) * 0.25 + (LU_Stack[[i]] ==
                                                                   5 | LU_Stack[[i]] == 3 | LU_Stack[[i]] == 2) * 0.67
}

# STACK the layers
Stack_Set_AOI <-
  stack(SOC_MAP_AOI, Clay_WA_AOI_res, Cov_AOI, LU_Stack, DR_Stack)

setwd(final)
writeRaster(
  Stack_Set_AOI,
  filename = ("Stack_Set_WARM_UP_AOI.tif"),
  format = "GTiff",
  overwrite = TRUE
)

#-------------------------------------------------------------------------------
#forward
setwd(workspace)
PREC2 <- stack("Prec_Stack_01-18.tif")

PREC_AOI2 <- PREC2 %>% crop(AOI) %>%
            resample(SOC_MAP_AOI, method = 'bilinear')
            mask(AOI)
PREC_AOI2 <- stack(PREC_AOI2)

#Open Temperatures layer 
TEMP2 <- stack("Temp_Stack_01-18.tif")
TEMP_AOI2 <- TEMP %>% crop(AOI) %>%
           resample(SOC_MAP_AOI, method = 'bilinear') %>%
           mask(AOI)
TEMP_AOI2 <- stack(TEMP_AOI2)

#Open Potential Evapotranspiration layer 
PET2 <- stack("PET_Stack_01-18.tif") 
PET_AOI2 <- PET %>% crop(AOI) %>%
           resample(SOC_MAP_AOI, method = 'bilinear') %>%
           mask(AOI)
PET_AOI2 <- stack(PET_AOI2)

DR3 <- (LU_AOI==4)*1.44+ (LU_AOI==1)*0.25 + (LU_AOI==5 | LU_AOI==7 | LU_AOI==2)*0.67

# STACK the layers
Stack_Set_AR <-
  stack(SOC_MAP_AOI,
        Clay_WA_AOI_res,
        TEMP_AOI2,
        PREC_AOI2,
        PET_AOI2,
        DR3,
        LU_AOI,
        Cov_AOI)
setwd(final)
writeRaster(
  Stack_Set_AR,
  filename = ("Stack_Set_FOWARD.tif"),
  format = "GTiff",
  overwrite = TRUE
)
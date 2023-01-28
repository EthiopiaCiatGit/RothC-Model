#-------------------------------------------------------------------------------
# Calculate npp mean
#-------------------------------------------------------------------------------

rm(list = ls())

library(raster)
library(rgdal)
library(dplyr)

#Set input directories
input <- "D:/RothC/input"
workspace <- "D:/RothC/workspace"

# Open the shapefile
setwd(input)
AOI <- readOGR("Abay_basin_bound.shp")
SOC_MAP <- raster("soc.tif") 
# Open Anual Precipitation (mm) and Mean Anual Temperature (grades C) stacks

setwd(workspace)
SOC_MAP_AOI <- raster("SOC_MAP_AOI.tif")
Temp <- stack("Temp_Stack_240_81-00.tif")
Prec <- stack("Prec_Stack_240_81-00.tif")

# Temperature Annual Mean
k <- 1
TempList <- list()
#######loop for starts#########
for (i in 1:20) {
  name <- paste0('Temp_Annual_average', i, '.tif')#added
  Temp1 <- mean(Temp[[k:(k + 11)]])
  names(Temp1) <- name # added
  TempList[i] <- Temp1
  
  k <- k + 12
}
#######loop for ends##########
TempStack <- stack(TempList)

#Annual Precipitation

k <- 1
PrecList <- list()
########loop for starts#######
for (i in 1:20) {
  name <- paste0('Precp_Annual_average', i, '.tif')#added
  Prec1 <- sum(Prec[[k:(k + 11)]])
  names(Prec1) <- name # added
  PrecList[i] <- Prec1
  
  k <- k + 12
}
########loop for ends#######
PrecStack <- stack(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)
NPP_Prec <- 3000 * (1 - exp(-0.000664 * PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)
NPP_temp <- 3000 / (1 + exp(1.315 - 0.119 * TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)
NPP_MIAMI_List <- list()

########loop for starts#######
for (i in 1:20) {
  NPP_MIAMI_List[i] <- min(NPP_Prec[[i]], NPP_temp[[i]])
}
########loop for ends#######
NPP_MIAMI <- stack(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year
NPP_MIAMI_tnDM_Ha_Year <- NPP_MIAMI * (1 / 100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year
NPP_MIAMI_tnC_Ha_Year <- NPP_MIAMI_tnDM_Ha_Year * 0.5
# Save WORLD NPP MIAMI MODEL tnC/ha/year

#setwd(WD_NPP)
setwd(workspace)
writeRaster(
  NPP_MIAMI_tnC_Ha_Year,
  filename = "NPP_MIAMI_tnC_Ha_Year_STACK_81-00.tif",
  format = "GTiff",
  overwrite = TRUE
)

#NPP_MIAMI_tnC_Ha_Year<-stack("NPP_MIAMI_tnC_Ha_Year_STACK_81-00.tif")

# NPP MEAN

NPP_MIAMI_MEAN_81_00 <- mean(NPP_MIAMI_tnC_Ha_Year)


# Crop & mask
NPP_MIAMI_MEAN_81_00_AOI <- NPP_MIAMI_MEAN_81_00 %>% crop(AOI) %>%
                            resample(SOC_MAP_AOI, method = 'bilinear') %>%
                            mask(AOI)

setwd(workspace)
writeRaster(
  NPP_MIAMI_MEAN_81_00_AOI,
  filename = "NPP_MIAMI_MEAN_81-00_AOI.tif",
  format = "GTiff",
  overwrite = TRUE)
writeRaster(
  NPP_MIAMI_MEAN_81_00,
  filename = "NPP_MIAMI_MEAN_81-00.tif",
  format = "GTiff",
  overwrite = TRUE)

#UNCERTAINTIES MINIMUM TEMP , PREC

Temp_min <- Temp * 1.02
Prec_min <- Prec * 0.95

# Temperature Annual Mean

k <- 1
TempList <- list()
########loop for starts#######
for (i in 1:20) {
  Temp1 <- mean(Temp_min[[k:(k + 11)]])
  TempList[i] <- Temp1
  
  k <- k + 12
}
########loop for ends#######

TempStack <- stack(TempList)

#Annual Precipitation

k <- 1
PrecList <- list()

########loop for starts#######
for (i in 1:20) {
  Prec1 <- sum(Prec_min[[k:(k + 11)]])
  PrecList[i] <- Prec1
  
  k <- k + 12
}
########loop for ends#######

PrecStack <- stack(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)

NPP_Prec <- 3000 * (1 - exp(-0.000664 * PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)

NPP_temp <- 3000 / (1 + exp(1.315 - 0.119 * TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)

NPP_MIAMI_List <- list()

########loop for starts#######
for (i in 1:20) {
  NPP_MIAMI_List[i] <- min(NPP_Prec[[i]], NPP_temp[[i]])
}
########loop for ends#######

NPP_MIAMI <- stack(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year
NPP_MIAMI_tnDM_Ha_Year <- NPP_MIAMI * (1 / 100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year
NPP_MIAMI_tnC_Ha_Year <- NPP_MIAMI_tnDM_Ha_Year * 0.5

# Save WORLD NPP MIAMI MODEL tnC/ha/year
setwd(workspace)

writeRaster(
  NPP_MIAMI_tnC_Ha_Year,
  filename = "NPP_MIAMI_tnC_Ha_Year_STACK_81-00_MIN.tif",
  format = "GTiff",
  overwrite = TRUE)

# NPP MEAN

NPP_MIAMI_MEAN_81_00 <- mean(NPP_MIAMI_tnC_Ha_Year)

# Crop & and mask

#setwd(WD_NPP)

NPP_MIAMI_MEAN_81_00_AOI <- NPP_MIAMI_MEAN_81_00 %>% crop(AOI) %>%
                            resample(SOC_MAP_AOI, method = 'bilinear') %>%
                            mask(AOI)

setwd(workspace)
writeRaster(
  NPP_MIAMI_MEAN_81_00_AOI,
  filename = "NPP_MIAMI_MEAN_81-00_AOI_MIN.tif",
  format = "GTiff",
  overwrite = TRUE).
writeRaster(
  NPP_MIAMI_MEAN_81_00,
  filename = "NPP_MIAMI_MEAN_81-00_MIN.tif",
  format = "GTiff",
  overwrite = TRUE)


#UNCERTAINTIES MAXIMUM TEMP , PREC

# Open Anual Precipitation (mm) and Mean Anual Temperature (grades C) stacks

Temp_max <- Temp * 0.98
Prec_max <- Prec * 1.05

# Temperature Annual Mean

k <- 1
TempList <- list()

########loop for starts#######
for (i in 1:20) {
  Temp1 <- mean(Temp_max[[k:(k + 11)]])
  TempList[i] <- Temp1
  
  k <- k + 12
}
########loop for ends#######

TempStack <- stack(TempList)

#Annual Precipitation

k <- 1
PrecList <- list()

########loop for starts#######
for (i in 1:20) {
  Prec1 <- sum(Prec_max[[k:(k + 11)]])
  PrecList[i] <- Prec1
  
  k <- k + 12
}
########loop for ends#######

PrecStack <- stack(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)

NPP_Prec <- 3000 * (1 - exp(-0.000664 * PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)

NPP_temp <- 3000 / (1 + exp(1.315 - 0.119 * TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)

NPP_MIAMI_List <- list()

########loop for starts#######
for (i in 1:20) {
  NPP_MIAMI_List[i] <- min(NPP_Prec[[i]], NPP_temp[[i]])
}
########loop for ends#######


NPP_MIAMI <- stack(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year
NPP_MIAMI_tnDM_Ha_Year <- NPP_MIAMI * (1 / 100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year
NPP_MIAMI_tnC_Ha_Year <- NPP_MIAMI_tnDM_Ha_Year * 0.5

setwd(workspace)
writeRaster(
  NPP_MIAMI_tnC_Ha_Year,
  filename = "NPP_MIAMI_tnC_Ha_Year_STACK_81-00_MAX.tif",
  format = "GTiff",
  overwrite = TRUE)

# NPP MEAN
NPP_MIAMI_MEAN_81_00 <- mean(NPP_MIAMI_tnC_Ha_Year)

# Crop & and mask
NPP_MIAMI_MEAN_81_00_AOI <- NPP_MIAMI_MEAN_81_00 %>% crop(AOI) %>%
                            resample(SOC_MAP_AOI, method = 'bilinear') %>%
                            mask(AOI)

setwd(workspace)
writeRaster(
  NPP_MIAMI_MEAN_81_00_AOI,
  filename = "NPP_MIAMI_MEAN_81-00_AOI_MAX.tif",
  format = "GTiff",
  overwrite = TRUE)
writeRaster(
  NPP_MIAMI_MEAN_81_00,
  filename = "NPP_MIAMI_MEAN_81-00_MAX.tif",
  format = "GTiff",
  overwrite = TRUE)

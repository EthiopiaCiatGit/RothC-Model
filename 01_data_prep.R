#-------------------------------------------------------------------------------
#preparation of input data
#-------------------------------------------------------------------------------

rm(list = ls())

#Load packages
library(raster)
library(rgdal)
library(dplyr)
library(ncdf4)
library(abind)

#create directories
root <- "D:/RothC"
setwd(root)

dirName1 <- "workspace"
dirName2 <- "final"

if (!dir.exists(dirName1)){
  dir.create(dirName1)
}

if (!dir.exists(dirName2)){
  dir.create(dirName2)
}

input <- "D:/RothC/input"
workspace <- "D:/RothC/workspace"

# Open the shapefile
setwd(input)
AOI <- readOGR("Abay_basin_bound.shp")

#Open SOC MAP , crop it and masked by the aoi. Then save it. 
SOC_MAP <- raster("soc.tif") 
SOC_MAP_AOI <- SOC_MAP %>% crop(AOI) %>% mask(AOI)

setwd(workspace)
writeRaster(SOC_MAP_AOI,filename = "SOC_MAP_AOI.tif", format = "GTiff", overwrite = TRUE)

#-------------------------------------------------------------------------------
# climate layer preparation

#spinup
# temperature 
setwd(input)
nc_temp_81_90 <- nc_open("temp_81_90.nc")

lon <- ncvar_get(nc_temp_81_90, "lon")
lat <- ncvar_get(nc_temp_81_90, "lat", verbose = F)
t_81_90 <- ncvar_get(nc_temp_81_90, "time")

tmp_81_90 <- ncvar_get(nc_temp_81_90, "tmp")

#close de nc temperature file

nc_close(nc_temp_81_90)

# Open nc temperature file 1991-2000

nc_temp_91_00 <- nc_open("temp_91_00.nc")

lon <- ncvar_get(nc_temp_91_00, "lon")
lat <- ncvar_get(nc_temp_91_00, "lat", verbose = F)
t_91_00 <- ncvar_get(nc_temp_91_00, "time")

tmp_91_00 <- ncvar_get(nc_temp_91_00, "tmp")

#close de nc temperature file

nc_close(nc_temp_91_00)

# Merge 1981-1990 and 1991-2000 data

tmp <- abind(tmp_81_90, tmp_91_00)

# Get one month temperature ( January)

tmp_Jan_1 <- tmp[, , 1]

dim(tmp_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)

# Average of 20 years (j)  and 12 months (i)

######for loop starts#######
for (i in 1:12) {
  var_sum <- tmp_Jan_1 * 0
  k <- i
  
  for (j in 1:20) {
    print(k)
    var_sum <- (var_sum + tmp[, , k])
    
    k <- k + 12
  }
  
  #Save each month average.
  
  var_avg <- var_sum / 20
  name <- paste0('Temp_1981_2000_years_avg_', i, '.tif')
  
  # Make a raster r from each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist[[i]] <- ra
}
#######for loop ends########

#save a stack of months averages
Temp_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(Temp_Stack, filename = 'Temp_Stack_81-00.tif', "GTiff")

#PRECIPITATION

# Open nc precipitation file 1981-1990

nc_pre_81_90 <- nc_open("prcp_81_90.nc")

lon <- ncvar_get(nc_pre_81_90, "lon")
lat <- ncvar_get(nc_pre_81_90, "lat", verbose = F)
t <- ncvar_get(nc_pre_81_90, "time")

pre_81_90 <- ncvar_get(nc_pre_81_90, "pre")

#close de nc temperature file

nc_close(nc_pre_81_90)

# Open nc precipitation file 1991-2000

nc_pre_91_00 <- nc_open("prcp_91_00.nc")

lon <- ncvar_get(nc_pre_91_00, "lon")
lat <- ncvar_get(nc_pre_91_00, "lat", verbose = F)
t <- ncvar_get(nc_pre_91_00, "time")

pre_91_00 <- ncvar_get(nc_pre_91_00, "pre")

#close de nc temperature file

nc_close(nc_pre_91_00)

# Merge 1981-1990 and 1991-2000 data

pre_81_00 <- abind(pre_81_90, pre_91_00)

# Get one month Precipitation ( January)

pre_Jan_1 <- pre_81_00[, , 1]

dim(pre_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)

# Average of 20 years (j)  and 12 months (i)

######for loop starts#######
for (i in 1:12) {
  var_sum <- pre_Jan_1 * 0
  k <- i
  
  for (j in 1:20) {
    print(k)
    var_sum <- (var_sum + pre_81_00[, , k])
    k <- k + 12
  }
  #Save each month average.
  
  var_avg <- var_sum / 20
  name <- paste0('Prec_1981_2000_years_avg_', i, '.tif')
  
  # Make a raster r from the each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  names(ra) <- name
  #writeRaster(ra,filename=name, format="GTiff")
  Rlist[[i]] <- ra
}

#save a stack of months averages

Prec_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(Prec_Stack, filename = 'Prec_Stack_81-00.tif', "GTiff")

# POTENTIAL EVAPOTRANSPIRATION
setwd(input)
# Open nc temperature file 81 - 90

nc_pet_81_90 <- nc_open("pet_81_90.pet.nc")

lon <- ncvar_get(nc_pet_81_90, "lon")
lat <- ncvar_get(nc_pet_81_90, "lat", verbose = F)
t <- ncvar_get(nc_pet_81_90, "time")

pet_81_90 <- ncvar_get(nc_pet_81_90, "pet")

#close de nc temperature file

nc_close(nc_pet_81_90)

# Open nc temperature file 91 - 00

nc_pet_91_00 <- nc_open("pet_91_00.nc")

lon <- ncvar_get(nc_pet_91_00, "lon")
lat <- ncvar_get(nc_pet_91_00, "lat", verbose = F)
t <- ncvar_get(nc_pet_91_00, "time")

pet_91_00 <- ncvar_get(nc_pet_91_00, "pet")

#close de nc temperature file

nc_close(nc_pet_91_00)

# Merge 1981-1990 and 1991-2000 data

pet_81_00 <- abind(pet_81_90, pet_91_00)

# Get one month PET ( January)

pet_Jan_1 <- pet_81_90[, , 1]

dim(pet_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)

# Average of 20 years (j)  and 12 months (i)

######for loop starts#######
for (i in 1:12) {
  var_sum <- pet_Jan_1 * 0
  k <- i
  
  for (j in 1:20) {
    print(k)
    var_sum <- (var_sum + pet_81_00[, , k])
    
    k <- k + 12
    
  }
  #Save each month average.
  
  var_avg <- var_sum * 30 / 20
  name <- paste0('PET_1981_2000_years_avg_', i, '.tif')
  
  # Make a raster r from the each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist[[i]] <- ra
}
######for loop ends#######

#save a stack of months averages
PET_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(PET_Stack, filename = 'PET_Stack_81-00.tif', "GTiff")

####################### For warmup stack ############################
# Open nc temperature file 2001-2010
setwd(input)
nc_temp_01_10 <- nc_open("temp_01_10.nc")

lon <- ncvar_get(nc_temp_01_10, "lon")
lat <- ncvar_get(nc_temp_01_10, "lat", verbose = F)
t_01_10 <- ncvar_get(nc_temp_01_10, "time")

tmp_01_10 <- ncvar_get(nc_temp_01_10, "tmp")

#close de nc temperature file

nc_close(nc_temp_01_10) 

# Open nc temperature file 2010-2018

nc_temp_11_18 <- nc_open("temp_01_18.nc")

lon <- ncvar_get(nc_temp_11_18, "lon")
lat <- ncvar_get(nc_temp_11_18, "lat", verbose = F)
t_11_18 <- ncvar_get(nc_temp_11_18, "time")

tmp_11_18 <- ncvar_get(nc_temp_11_18, "tmp")

#close de nc temperature file

nc_close(nc_temp_11_18)

# Merge 2001-2010 and 2011-2018 data

tmp <- abind(tmp_01_10, tmp_11_18)

# Get one month temperature ( January)

tmp_Jan_1 <- tmp[, , 1]

dim(tmp_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)

# Average of 20 years (j)  and 12 months (i)
##########for loop starts###############
for (i in 1:12) {
  var_sum <- tmp_Jan_1 * 0
  k <- i
  
  for (j in 1:(dim(tmp)[3] / 12)) {
    print(k)
    var_sum <- (var_sum + tmp[, , k])
    
    k <- k + 12
    
  }
  #Save each month average.
  
  var_avg <- var_sum / (dim(tmp)[3] / 12)
  name <- paste0('Temp_2001_2018_years_avg_', i, '.tif')
  
  # Make a raster r from each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist[[i]] <- ra
}
##########for loop ends#############
#save a stack of months averages

Temp_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(Temp_Stack,
            filename = 'Temp_Stack_01-18.tif',
            "GTiff",
            overwrite = TRUE)

#Calculate annual average for MIAMI MODEL
Temp_Mean <-
  (
    Rlist[[1]] + Rlist[[2]] + Rlist[[3]] + Rlist[[4]] + Rlist[[5]] + Rlist[[6]] +
      Rlist[[7]] + Rlist[[8]] + Rlist[[9]] + Rlist[[10]] + Rlist[[11]] + Rlist[[12]]
  ) / 12
writeRaster(Temp_Mean, filename = 'Temp_mean_01-18.tif', "GTiff", overwrite =
              TRUE)

# SAVE 1 layer per month per year

Rlist2 <- Rlist

# Create a raster image for each month
###########for loop starts################
for (q in 1:(dim(tmp)[3])) {
  var <- (tmp[, , q])
  
  #Save each month average.
  
  name <- paste0('Temp_2001-2018', q, '.tif')
  
  # Make a raster r from each average
  ra <-
    raster(
      t(var),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist2[[q]] <- ra
}
############for loop ends#################

Temp_Stack_2 <- stack(Rlist2)
setwd(workspace)
writeRaster(Temp_Stack_2,
            filename = 'Temp_Stack_216_01-18.tif',
            "GTiff",
            overwrite = TRUE)

#PRECIPITATION
setwd(input)
# Open nc precipitation file 2001-2010

nc_pre_01_10 <- nc_open("prcp_01_10.nc")

lon <- ncvar_get(nc_pre_01_10, "lon")
lat <- ncvar_get(nc_pre_01_10, "lat", verbose = F)
t <- ncvar_get(nc_pre_01_10, "time")

pre_01_10 <- ncvar_get(nc_pre_01_10, "pre")

#close de nc temperature file

nc_close(nc_pre_01_10)

# Open nc precipitation file 2011-2018

nc_pre_11_18 <- nc_open("prcp_11_18.nc")

lon <- ncvar_get(nc_pre_11_18, "lon")
lat <- ncvar_get(nc_pre_11_18, "lat", verbose = F)
t <- ncvar_get(nc_pre_11_18, "time")

pre_11_18 <- ncvar_get(nc_pre_11_18, "pre")

#close de nc temperature file

nc_close(nc_pre_11_18)

# Merge 2001-2010 and 2011-2018 data

pre_01_18 <- abind(pre_01_10, pre_11_18)

# Have one month Precipitation ( January)

pre_Jan_1 <- pre_01_18[, , 1]

dim(pre_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)


# Average of 20 years (j)  and 12 months (i)

#########for loop starts############
for (i in 1:12) {
  var_sum <- pre_Jan_1 * 0
  k <- i
  
  for (j in 1:(dim(pre_01_18)[3] / 12)) {
    print(k)
    var_sum <- (var_sum + pre_01_18[, , k])
    
    k <- k + 12
    
  }
  #Save each month average.
  
  var_avg <- var_sum / (dim(pre_01_18)[3] / 12)
  
  name <- paste0('Prec_2001_2018_years_avg_', i, '.tif')
  
  # Make a raster r from the each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff",overwrite=TRUE)
  names(ra) <- name
  Rlist[[i]] <- ra
}
##########for loop ends##########

#save a stack of months averages

Prec_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(Prec_Stack,
            filename = 'Prec_Stack_01-18.tif',
            "GTiff",
            overwrite = TRUE)

# SAVE 1 layer per month per year
Rlist2 <- Rlist
# Make a raster r from each month

##########for loop starts#########
for (q in 1:(dim(pre_01_18)[3])) {
  var <- (pre_01_18[, , q])
  
  #Save each month average.
  
  name <- paste0('Prec_2001-2018', q, '.tif')
  
  ra <-
    raster(
      t(var),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist2[[q]] <- ra
}
###########for loop ends##########

Prec_Stack_2 <- stack(Rlist2)
setwd(workspace)
writeRaster(Prec_Stack_2,
            filename = 'Prec_Stack_216_01-18.tif',
            "GTiff",
            overwrite = TRUE)

# POTENTIAL EVAPOTRANSPIRATION
setwd(input)
# Open nc temperature file 01 - 10

nc_pet_01_10 <- nc_open("pet_01_10.nc")

lon <- ncvar_get(nc_pet_01_10, "lon")
lat <- ncvar_get(nc_pet_01_10, "lat", verbose = F)
t <- ncvar_get(nc_pet_01_10, "time")

pet_01_10 <- ncvar_get(nc_pet_01_10, "pet")

#close de nc temperature file

nc_close(nc_pet_01_10)

# Open nc temperature file 11 - 18

nc_pet_11_18 <- nc_open("pet_11_18.nc")

lon <- ncvar_get(nc_pet_11_18, "lon")
lat <- ncvar_get(nc_pet_11_18, "lat", verbose = F)
t <- ncvar_get(nc_pet_11_18, "time")

pet_11_18 <- ncvar_get(nc_pet_11_18, "pet")

#close de nc temperature file

nc_close(nc_pet_11_18)

# Merge 2001-2010 and 2011-2018 data

pet_01_18 <- abind(pet_01_10, pet_11_18)

# Have one month ETP ( January)

pet_Jan_1 <- pet_01_18[, , 1]

dim(pet_Jan_1)

# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)
Rlist2 <- Rlist

# Average of 18 years (j)  and 12 months (i)
############for loop starts##############
for (i in 1:12) {
  var_sum <- pet_Jan_1 * 0
  k <- i
  
  for (j in 1:(dim(pet_01_18)[3] / 12)) {
    print(k)
    var_sum <- (var_sum + pet_01_18[, , k])
    
    k <- k + 12
    
  }
  #Save each month average.
  
  var_avg <- var_sum * 30 / (dim(pet_01_18)[3] / 12)
  name <- paste0('PET_2001_2018_years_avg_', i, '.tif')
  
  # Make a raster r from the each average
  ra <-
    raster(
      t(var_avg),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff",overwrite=TRUE)
  names(ra) <- name
  Rlist[[i]] <- ra
}
#########for loop ends############

#save a stack of months averages

PET_Stack <- stack(Rlist)
setwd(workspace)
writeRaster(PET_Stack, filename = 'PET_Stack_01-18.tif', "GTiff", overwrite = TRUE)

# SAVE 1 layer per month
########for loop starts##########
for (q in 1:(dim(pet_01_18)[3])) {
  var <- (pet_01_18[, , q]) * 30
  
  #Save each month average.
  
  name <- paste0('PET_2001-2018', q, '.tif')
  
  # Make a raster r from each average
  ra <-
    raster(
      t(var),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  names(ra) <- name
  Rlist2[[q]] <- ra
}
#########for loop ends###########

PET_Stack_2 <- stack(Rlist2)
setwd(workspace)
writeRaster(PET_Stack_2,
            filename = 'PET_Stack_216_01-18.tif',
            "GTiff",
            overwrite = TRUE)

# estimate NPP 1981-2000

#temperature
setwd(input)
# Open nc temperature file 1981-1990
nc_temp_81_90 <- nc_open("temp_81_90.nc")
lon <- ncvar_get(nc_temp_81_90, "lon")
lat <- ncvar_get(nc_temp_81_90, "lat", verbose = F)
t_81_90 <- ncvar_get(nc_temp_81_90, "time")
tmp_81_90 <- ncvar_get(nc_temp_81_90, "tmp")
#close de nc temperature file
nc_close(nc_temp_81_90)
# Open nc temperature file 1991-2000
nc_temp_91_00 <- nc_open("temp_91_00.nc")
lon <- ncvar_get(nc_temp_91_00, "lon")
lat <- ncvar_get(nc_temp_91_00, "lat", verbose = F)
t_91_00 <- ncvar_get(nc_temp_91_00, "time")
tmp_91_00 <- ncvar_get(nc_temp_91_00, "tmp")
#close de nc temperature file
nc_close(nc_temp_91_00)
# Merge 1981-1990 and 1991-2000 data
tmp <- abind(tmp_81_90, tmp_91_00)
# Get one month temperature ( January)

tmp_Jan_1 <- tmp[, , 1]
dim(tmp_Jan_1)
# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)
# SAVE 1 layer per month per year
Rlist2 <- Rlist
############for loop starts###########
for (q in 1:(dim(tmp)[3])) {
  var <- (tmp[, , q])
  #Save each month average.
  name <- paste0('Temp_1981-2000', q, '.tif')
  # Make a raster r from each average
  ra <-
    raster(
      t(var),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  Rlist2[[q]] <- ra
}
############for loop ends###########
Temp_Stack_2 <- stack(Rlist2)
setwd(workspace)
writeRaster(Temp_Stack_2, filename = 'Temp_Stack_240_81-00.tif', "GTiff")

#precipitation
# Open nc precipitation file 1981-1990
nc_pre_81_90 <- nc_open("prcp_81_90.nc")
lon <- ncvar_get(nc_pre_81_90, "lon")
lat <- ncvar_get(nc_pre_81_90, "lat", verbose = F)
t <- ncvar_get(nc_pre_81_90, "time")
pre_81_90 <- ncvar_get(nc_pre_81_90, "pre")
#close de nc temperature file
nc_close(nc_pre_81_90)
# Open nc precipitation file 1991-2000
nc_pre_91_00 <- nc_open("prcp_91_00.nc")
lon <- ncvar_get(nc_pre_91_00, "lon")
lat <- ncvar_get(nc_pre_91_00, "lat", verbose = F)
t <- ncvar_get(nc_pre_91_00, "time")
pre_91_00 <- ncvar_get(nc_pre_91_00, "pre")
#close de nc temperature file
nc_close(nc_pre_91_00)
# Merge 1981-1990 and 1991-2000 data
pre_81_00 <- abind(pre_81_90, pre_91_00)
# Create empty list
r <- raster(ncol = 3, nrow = 3)
Rlist <- list(r, r, r, r, r, r, r, r, r, r, r, r)
Rlist2 <- Rlist
# SAVE 1 layer per month per year
##############for loop starts############
for (q in 1:(dim(pre_81_00)[3])) {
  var <- (pre_81_00[, , q])
  #Save each month average.
  #name<-paste0('Prec_2001-2018',q,'.tif')
  # Make a raster r from each average
  ra <-
    raster(
      t(var),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat),
      crs = CRS(
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
      )
    )
  ra <- flip(ra, direction = 'y')
  #writeRaster(ra,filename=name, format="GTiff")
  Rlist2[[q]] <- ra
}
##############for loop ends############
Prec_Stack_2 <- stack(Rlist2)
setwd(workspace)
writeRaster(Prec_Stack_2, filename = 'Prec_Stack_240_81-00.tif', "GTiff")

#-------------------------------------------------------------------------------
#preparation of ndvi layers processed at  google earth engine

setwd(paste(input, "gee", sep = "/"), )
cov <- list.files(path = ".",
                  pattern = ".tif$",
                  all.files = T) %>%
                  lapply(rast) %>% stack()

Stack_Cov <- stack()
for (i in 1:nlayers(cov)) {
  ras1 <- cov[[i]]
  ras1[is.na(ras1[])] <- 0
  ras2 <-  ras1 %>% crop(AOI) %>% mask(AOI)  %>%
    resample(SOC_MAP_AOI, method = 'bilinear')
  addLayer(Stack_Cov) <- ras2
}

# rescale values to 1 if it is bare soil and 0.6 if it is vegetated
Cov <- ((Stack_Cov) * (-0.4)) + 1
setwd(workspace)
writeRaster(Cov,
            filename = 'NDVI_Cov_stack_AOI.tif',
            format = 'GTiff',
            overwrite = TRUE)

#-------------------------------------------------------------------------------
#preparation of clay layers

#open isric clay layers
setwd(input)
clay1 <- raster("clay0-5.tif")
clay2 <- raster("clay5-15.tif")
clay3 <- raster("clay15-30.tif")

# Weighted Average of four depths remark(only used three depths)

WeightedAverage <-
  function(r1, r2, r3, r4) {
    return(r1 * (5 / 30) + r2 * (10 / 30) + r3 * (15 / 30))
  }

Clay_WA <- overlay(clay1, clay2, clay3, fun = WeightedAverage)

Clay_WA_AOI <-  Clay_WA %>% crop(AOI) %>% mask(AOI)  %>%
  resample(SOC_MAP_AOI, method = 'bilinear')

setwd(workspace)

writeRaster(
  Clay_WA_AOI,
  filename = "Clay_WA_AOI.tif",
  format = 'GTiff',
  overwrite = TRUE
)

#-------------------------------------------------------------------------------
#preparation of landuse layers

setwd(input)
ESA_LU_AOI <- raster("2016_LULC_Level_I.tif") %>% crop(AOI) %>% mask(AOI)

#Reclassify ESA LAND USE to FAO LAND USE classes
#ours  FAO
#0 = 0	No Data
#10 = 1 Artificial
#4 = 2 Croplands
#5 = 3 Grassland
#1,2,9 = 4 Tree Covered
#3  = 5 Shrubs Covered
#6 = 9 Baresoil
#8 = 11 Waterbodies
#7 = 13 Paddy fields(rice/ flooded crops)

# Create a reclassification matrix. "Is" to "become"
is <- c(0, 10, 4, 5, 1, 2, 9, 3, 6, 8, 7)
become <- c(0, 1, 2, 3, 4, 4, 4, 5, 9, 11, 13)
recMat <- matrix(c(is, become), ncol = 2, nrow = 11)
#
# Reclassify
ESA_FAO <- reclassify(ESA_LU_AOI, recMat)

# Resample to SOC map layer extent and resolution
ESA_FAO_res <- resample(ESA_LU_AOI, SOC_MAP_AOI, method = 'ngb')

# Save Land Use raster
setwd(workspace)
writeRaster(ESA_FAO_res,
            filename = "Land_Cover_10class_AOI.tif",
            format = 'GTiff',
            overwrite = TRUE)

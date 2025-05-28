
# Información -------------------------------------------------------------
# Este codigo busca visualizar de manera preliminar los raster descargados de GEE, 
# observando la primera y ultima banda en cada uno respectivamente.


# Código ------------------------------------------------------------------
## Librerias
library(terra)

## Cargar los raster
modis <- rast("raster/m_ndvi_16_original.tif")
landsat <- rast("raster/l_ndvi_16.tif")
sentinel <- rast("raster/s_ndvi_16.tif")

## Mostrar número y nombres de bandas MODIS
modis
nlyr(modis)
names(modis)

## Mostrar número y nombres de bandas Landsat 8
landsat
nlyr(landsat)
names(landsat)

## Mostrar número y nombres de bandas Sentinel 2
sentinel
nlyr(sentinel)
names(sentinel)

## Visualizar MODIS
plot(modis[[1]], main = "MODIS NDVI (2016)")
plot(modis[[nlyr(modis)]], main = "MODIS NDVI (2024)")

## Visualizar Landsat 8
plot(landsat[[1]], main = "Landsat8 NDVI (2016)")
plot(landsat[[nlyr(landsat)]], main = "Landsat8 NDVI (2024)")

## Visualizar sentinel 2
plot(sentinel[[1]], main = "Sentinel NDVI (2019)")
plot(sentinel[[nlyr(sentinel)]], main = "Sentinel NDVI (2024)")



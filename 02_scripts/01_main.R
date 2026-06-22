
# Limpieza landsat --------------------------------------------------------

source("scripts/visualización_raster.R")

# 3. Cargar los raster (última banda, que corresponde a 2024)
# -------------------------------------------------------------
modis <- rast("01_data/raster/m_ndvi_16.tif")
landsat <- rast("01_data/raster/l_ndvi_16_test.tif")
sentinel <- rast("01_data/raster/s_ndvi_16.tif")

nlyr(modis)
nlyr(landsat)
nlyr(sentinel)


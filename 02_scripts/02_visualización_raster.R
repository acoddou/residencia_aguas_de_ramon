# ==========================================================================
# Script: 02_visualizacion_raster.R
# Descripción: Visualización preliminar comparativa de la última banda (2024) 
# de los rasters NDVI descargados (MODIS, Landsat, Sentinel).
# ==========================================================================

library(terra)
library(viridisLite)

# 1. Configurar la salida de imagen
# -------------------------------------------------------------
png("plots_test/resoluciones_sensores.png", width = 1800, height = 600, res = 150)

# 2. Cargar rasters y extraer la última capa (2024)
# -------------------------------------------------------------
modis <- rast("01_data/raster/m_ndvi_16.tif")[[nlyr(rast("01_data/raster/m_ndvi_16.tif"))]]
landsat <- rast("01_data/raster/l_ndvi_16_test.tif")[[nlyr(rast("01_data/raster/l_ndvi_16_test.tif"))]]
sentinel <- rast("01_data/raster/s_ndvi_16.tif")[[nlyr(rast("01_data/raster/s_ndvi_16.tif"))]]

# 3. Homogeneizar para visualización (reproyectar y remuestrear a Landsat)
# -------------------------------------------------------------
modis_resample <- resample(modis, landsat, method = "bilinear")
sentinel_resample <- resample(sentinel, landsat, method = "bilinear")

# 4. Configurar escala de colores común
# -------------------------------------------------------------
rasters_unidos <- c(modis_resample, landsat, sentinel_resample)
rango_min <- min(global(rasters_unidos, "min", na.rm = TRUE))
rango_max <- max(global(rasters_unidos, "max", na.rm = TRUE))
rango_comun <- c(rango_min, rango_max)
paleta_viridis <- viridis(100)

# 5. Generar gráficas
# -------------------------------------------------------------
par(mfrow = c(1, 3), 
    mar = c(2, 1, 2, 5), 
    oma = c(0, 0, 0, 0))

# Extraer el total de píxeles (originales)
pixeles_modis <- ncell(modis)
pixeles_landsat <- ncell(landsat)
pixeles_sentinel <- ncell(sentinel)

# Plot MODIS
plot(modis_resample,
     main = "MODIS NDVI (2024)",
     col = paleta_viridis,
     range = rango_comun)
north(xy = "topright", type = 2, cex = 1)
mtext(paste("Píxeles:", pixeles_modis), side = 1, line = 1)

# Plot Landsat
plot(landsat,
     main = "Landsat NDVI (2024)",
     col = paleta_viridis,
     range = rango_comun)
north(xy = "topright", type = 2, cex = 1)
mtext(paste("Píxeles:", pixeles_landsat), side = 1, line = 1)

# Plot Sentinel
plot(sentinel_resample,
     main = "Sentinel NDVI (2024)",
     col = paleta_viridis,
     range = rango_comun)
north(xy = "topright", type = 2, cex = 1)
mtext(paste("Píxeles:", pixeles_sentinel), side = 1, line = 1)

dev.off()

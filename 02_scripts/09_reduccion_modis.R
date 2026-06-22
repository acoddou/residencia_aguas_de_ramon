# ==========================================================================
# Script: 09_reduccion_modis.R
# Descripción: Convierte el dataframe de métricas fenológicas de MODIS 
# (formato largo) en rasters multicapa (una capa por año/temporada) para 
# cada métrica, y genera un raster final con los promedios multianuales.
# ==========================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(terra)
library(tidyverse)

# 1. Cargar datos fenológicos MODIS ----------------------------------------
feno_modis <- read_csv("03_results/m_fenologia.csv", show_col_types = FALSE)

feno_modis <- feno_modis %>%
  mutate(Seas = as.character(Seas))

# 2. Función generadora de Rasters Multicapa -------------------------------
# Toma una variable, la pivota a formato ancho (una columna por temporada)
# y la convierte en un SpatRaster guardando el archivo .tif resultante.
generar_raster_metrica <- function(data, variable_origen, nombre_salida) {
  
  feno_wide <- data %>%
    select(x, y, Seas, all_of(variable_origen)) %>%
    rename(Valor = all_of(variable_origen)) %>%
    pivot_wider(names_from = Seas,
                values_from = Valor,
                names_prefix = "Seas_",
                values_fn = mean)
  
  # digits=6 es necesario para forzar la regularidad de la grilla espacial
  raster_out <- rast(feno_wide, type="xyz", crs="EPSG:4326", digits=6)
  
  ruta_salida <- paste0("03_results/reducciones_modis/m_fenologia_", nombre_salida, ".tif")
  writeRaster(raster_out, ruta_salida, overwrite = TRUE)
  
  cat("Raster guardado:", ruta_salida, "\n")
}

# 3. Ejecutar generación para cada métrica ---------------------------------
cat("Generando rasters por temporada...\n")

generar_raster_metrica(feno_modis, "sos", "SOS")
generar_raster_metrica(feno_modis, "eos", "EOS")
generar_raster_metrica(feno_modis, "los", "LOS")
generar_raster_metrica(feno_modis, "peak_t", "PeakT")
generar_raster_metrica(feno_modis, "Ampl", "Ampl")
generar_raster_metrica(feno_modis, "Linteg", "Linteg")

# 4. Reducción a Promedios Multianuales ------------------------------------
cat("\nCalculando promedios multianuales...\n")

feno_prom <- feno_modis %>%
  select(id_row, x, y, sos, eos, los, Baseval, peak_t, Maxval, Ampl, Lder, Rder, Linteg, Sinteg, Startval, Endval) %>%
  group_by(id_row, x, y) %>%
  summarise(
    SOS = mean(sos, na.rm = TRUE),
    EOS = mean(eos, na.rm = TRUE),
    LOS = mean(los, na.rm = TRUE),
    Baseval = mean(Baseval, na.rm = TRUE),
    PeakT = mean(peak_t, na.rm = TRUE),
    Maxval = mean(Maxval, na.rm = TRUE),
    Ampl = mean(Ampl, na.rm = TRUE),
    Lder = mean(Lder, na.rm = TRUE),
    Rder = mean(Rder, na.rm = TRUE),
    Linteg = mean(Linteg, na.rm = TRUE),
    Sinteg = mean(Sinteg, na.rm = TRUE),
    Startval = mean(Startval, na.rm = TRUE),
    Endval = mean(Endval, na.rm = TRUE),
    .groups = 'drop'
  )

fenologia_prom <- rast(feno_prom, type="xyz", crs="EPSG:4326", digits=6) 
writeRaster(fenologia_prom, "03_results/reducciones_modis/m_fenologia_promedios.tif", overwrite = TRUE)

cat("Raster de promedios guardado en: 03_results/reducciones_modis/m_fenologia_promedios.tif\n")

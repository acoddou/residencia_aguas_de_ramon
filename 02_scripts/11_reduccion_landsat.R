# ==========================================================================
# Script: 11_reduccion_landsat.R
# Descripción: Convierte el dataframe de métricas fenológicas de Landsat 
# (formato largo) en rasters multicapa (una capa por año/temporada) para 
# cada métrica, y genera un raster final con los promedios multianuales.
# ==========================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(terra)
library(tidyverse)

# 1. Cargar datos fenológicos Landsat --------------------------------------
l_fenologia <- read_csv("03_results/l_fenologia_test.csv", show_col_types = FALSE)

l_fenologia <- l_fenologia %>%
  mutate(Seas = as.character(Seas))

# 2. Función generadora de Rasters Multicapa -------------------------------
generar_raster_metrica_landsat <- function(data, variable_origen, nombre_salida) {
  
  l_feno_wide <- data %>%
    select(x, y, Seas, all_of(variable_origen)) %>%
    rename(Valor = all_of(variable_origen)) %>%
    pivot_wider(names_from = Seas,
                values_from = Valor,
                names_prefix = "Seas_",
                values_fn = mean)
  
  raster_out <- rast(l_feno_wide, type="xyz", crs="EPSG:4326", digits=6)
  
  ruta_salida <- paste0("03_results/reducciones_landsat_test/l_fenologia_", nombre_salida, "_test.tif")
  writeRaster(raster_out, ruta_salida, overwrite = TRUE)
  
  cat("Raster guardado:", ruta_salida, "\n")
}

# 3. Ejecutar generación para cada métrica ---------------------------------
cat("Generando rasters por temporada para Landsat...\n")

generar_raster_metrica_landsat(l_fenologia, "sos", "SOS")
generar_raster_metrica_landsat(l_fenologia, "eos", "EOS")
generar_raster_metrica_landsat(l_fenologia, "los", "LOS")
generar_raster_metrica_landsat(l_fenologia, "peak_t", "PeakT")
generar_raster_metrica_landsat(l_fenologia, "Ampl", "Ampl")
generar_raster_metrica_landsat(l_fenologia, "Linteg", "Linteg")

# 4. Reducción a Promedios Multianuales ------------------------------------
cat("\nCalculando promedios multianuales...\n")

l_feno_prom <- l_fenologia %>%
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

l_fenologia_prom <- rast(l_feno_prom, type="xyz", crs="EPSG:4326", digits=6) 
writeRaster(l_fenologia_prom, "03_results/reducciones_landsat_test/l_fenologia_promedios_test.tif", overwrite = TRUE)

cat("Raster de promedios guardado en: 03_results/reducciones_landsat_test/l_fenologia_promedios_test.tif\n")

# ==========================================================================
# Script: 13_reduccion_sentinel.R
# Descripción: Convierte el dataframe de métricas fenológicas de Sentinel 
# (formato largo) en rasters multicapa. Incluye un ajuste temporal crítico 
# para alinear las temporadas de Sentinel con la serie base (MODIS/Landsat).
# ==========================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(terra)
library(tidyverse)
library(lubridate)

# 1. Cargar datos fenológicos Sentinel -------------------------------------
s_fenologia <- read_csv("03_results/s_fenologia.csv", show_col_types = FALSE)

s_fenologia <- s_fenologia %>%
  mutate(Seas = as.character(Seas))

# 2. Ajuste Temporal de Temporadas (Alineación Multi-sensor) ---------------
# NOTA METODOLÓGICA:
# La serie de tiempo de Sentinel inicia en 2019, mientras que MODIS y Landsat 
# inician en 2016. Para permitir comparaciones inter-sensores coherentes 
# (ej. comparar el año 2020 en los tres satélites), se suma un offset de +3 
# a la variable 'Seas'. Así, la Temporada 1 de Sentinel pasa a ser la Temporada 4.
# --------------------------------------------------------------------------
s_fenologia <- s_fenologia %>%
  mutate(Seas_original = Seas, 
         Seas = as.numeric(Seas), 
         Seas = Seas + 3,         
         Seas = as.character(Seas)) 

# 3. Función generadora de Rasters Multicapa -------------------------------
generar_raster_metrica_sentinel <- function(data, variable_origen, nombre_salida) {
  
  s_feno_wide <- data %>%
    select(x, y, Seas, all_of(variable_origen)) %>%
    rename(Valor = all_of(variable_origen)) %>%
    pivot_wider(names_from = Seas,
                values_from = Valor,
                names_prefix = "Seas_",
                values_fn = mean)
  
  raster_out <- rast(s_feno_wide, type="xyz", crs="EPSG:4326", digits=6)
  
  ruta_salida <- paste0("03_results/reducciones_sentinel/s_fenologia_", nombre_salida, ".tif")
  writeRaster(raster_out, ruta_salida, overwrite = TRUE)
  
  cat("Raster guardado:", ruta_salida, "\n")
}

# 4. Ejecutar generación para cada métrica ---------------------------------
cat("Generando rasters por temporada para Sentinel (Temporadas 4-8)...\n")

generar_raster_metrica_sentinel(s_fenologia, "sos", "SOS")
generar_raster_metrica_sentinel(s_fenologia, "eos", "EOS")
generar_raster_metrica_sentinel(s_fenologia, "los", "LOS")
generar_raster_metrica_sentinel(s_fenologia, "peak_t", "PeakT")
generar_raster_metrica_sentinel(s_fenologia, "Ampl", "Ampl")
generar_raster_metrica_sentinel(s_fenologia, "Linteg", "Linteg")

# 5. Reducción a Promedios Multianuales ------------------------------------
cat("\nCalculando promedios multianuales...\n")

s_feno_prom <- s_fenologia %>%
  select(id_row, x, y, sos, eos, los, Baseval, peak_t, Maxval, Ampl, Lder, Rder, Linteg, Sinteg, Startval, Endval, year_sos, year_eos, year_peak_t) %>%
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
    Year_SOS = mean(year_sos, na.rm = TRUE),
    Year_EOS = mean(year_eos, na.rm = TRUE),
    Year_PeakT = mean(year_peak_t, na.rm = TRUE),
    .groups = 'drop'
  )

s_fenologia_prom <- rast(s_feno_prom, type="xyz", crs="EPSG:4326", digits=6)
writeRaster(s_fenologia_prom, "03_results/reducciones_sentinel/s_fenologia_promedios.tif", overwrite = TRUE)

cat("Raster de promedios guardado en: 03_results/reducciones_sentinel/s_fenologia_promedios.tif\n")

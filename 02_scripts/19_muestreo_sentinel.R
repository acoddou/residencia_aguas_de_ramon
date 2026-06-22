# ==========================================================================
# Script: 19_cruce_sentinel_modis.R
# Descripción: Reproyecta los datos fenológicos de Sentinel a UTM, realiza 
# una intersección espacial con la grilla base de MODIS y extrae los 
# píxeles de Sentinel que caen dentro de las áreas muestreadas.
# ==========================================================================

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Configuración de Rutas y Parámetros -----------------------------------
ruta_csv_sentinel_original <- "03_results/metricas_crudas/s_fenologia.csv"
ruta_csv_sentinel_utm_temporal <- "03_results/muestreo_espacial/s_fenologia_utm_temp.csv"
ruta_shp_grilla_modis <- "01_data/vectorial/modis_grid.shp"
ruta_csv_sentinel_muestreado_salida <- "03_results/muestreo_espacial/muestreo_sentinel_coberturas.csv"

crs_puntos_sentinel_original <- "EPSG:4326"
crs_destino_utm <- "EPSG:32719" 

# 2. Preprocesamiento y Reproyección de Sentinel ---------------------------
cat("Cargando y preprocesando datos Sentinel...\n")
datos_sentinel_original <- read_csv(ruta_csv_sentinel_original, show_col_types = FALSE)

# Generar ID único por coordenada para trazabilidad
datos_sentinel_preprocesado <- datos_sentinel_original %>%
  distinct(.keep_all = TRUE) %>%
  group_by(x, y) %>%
  mutate(id_pixel_sentinel_unico = cur_group_id()) %>%
  ungroup() %>%
  select(id_pixel_sentinel_unico, everything())

puntos_sentinel_espacial_wgs84 <- vect(datos_sentinel_preprocesado, geom = c("x", "y"), crs = crs_puntos_sentinel_original)
puntos_sentinel_reproyectados_utm <- project(puntos_sentinel_espacial_wgs84, crs_destino_utm)

datos_sentinel_utm <- as.data.frame(puntos_sentinel_reproyectados_utm, geom = "XY") %>% as_tibble()
write_csv(datos_sentinel_utm, ruta_csv_sentinel_utm_temporal)

# 3. Intersección Espacial con Grilla MODIS --------------------------------
cat("Realizando intersección espacial con la grilla MODIS...\n")
poligonos_grilla_modis <- vect(ruta_shp_grilla_modis)
puntos_sentinel_espacial_utm <- vect(datos_sentinel_utm, geom = c("x", "y"), crs = crs_destino_utm)

sentinel_con_info_modis_raw <- terra::intersect(puntos_sentinel_espacial_utm, poligonos_grilla_modis)
sentinel_interseccion_df <- as_tibble(as.data.frame(sentinel_con_info_modis_raw, geom = "XY"))

# 4. Resolución de Duplicados y Extracción Final ---------------------------
# Resuelve asignaciones múltiples tomando solo la primera coincidencia
sentinel_con_info_modis_filtrado <- sentinel_interseccion_df %>%
  group_by(id_pixel_sentinel_unico, Seas) %>%
  slice_head(n = 1) %>% 
  ungroup()

datos_sentinel_muestreados <- sentinel_con_info_modis_filtrado %>%
  select(
    id_pixel_sentinel_unico, x, y, Seas,
    sos, eos, los, peak_t, Ampl, Linteg, Sinteg,
    year_sos, year_eos, year_peak_t,
    MODIS_ID, Cobertura
  ) %>%
  distinct(id_pixel_sentinel_unico, Seas, MODIS_ID, .keep_all = TRUE)

write_csv(datos_sentinel_muestreados, ruta_csv_sentinel_muestreado_salida)

cat("Proceso completado. Datos Sentinel muestreados guardados en:", ruta_csv_sentinel_muestreado_salida, "\n")

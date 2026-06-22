# ==========================================================================
# Script: 18_cruce_landsat_modis.R
# Descripción: Reproyecta los datos fenológicos de Landsat a UTM, realiza 
# una intersección espacial con la grilla base de MODIS y extrae los 
# píxeles de Landsat que caen dentro de las áreas muestreadas.
# ==========================================================================

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Configuración de Rutas y Parámetros -----------------------------------
ruta_csv_landsat_original <- "03_results/metricas_crudas/l_fenologia_test.csv"
ruta_csv_landsat_utm_temporal <- "03_results/muestreo_espacial/l_fenologia_utm_temp_test.csv"
ruta_shp_grilla_modis <- "01_data/vectorial/modis_grid.shp"
ruta_csv_landsat_muestreado_salida <- "03_results/muestreo_espacial/muestreo_landsat_coberturas_test.csv"

crs_puntos_landsat_original <- "EPSG:4326"
crs_destino_utm <- "EPSG:32719" 

# 2. Preprocesamiento y Reproyección de Landsat ----------------------------
cat("Cargando y preprocesando datos Landsat...\n")
datos_landsat_original <- read_csv(ruta_csv_landsat_original, show_col_types = FALSE)

# Generar ID único por coordenada para trazabilidad
datos_landsat_preprocesado <- datos_landsat_original %>%
  distinct(.keep_all = TRUE) %>%
  group_by(x, y) %>%
  mutate(id_pixel_landsat_unico = cur_group_id()) %>%
  ungroup() %>%
  select(id_pixel_landsat_unico, everything())

puntos_landsat_espacial_wgs84 <- vect(datos_landsat_preprocesado, geom = c("x", "y"), crs = crs_puntos_landsat_original)
puntos_landsat_reproyectados_utm <- project(puntos_landsat_espacial_wgs84, crs_destino_utm)

datos_landsat_utm <- as.data.frame(puntos_landsat_reproyectados_utm, geom = "XY") %>% as_tibble()
write_csv(datos_landsat_utm, ruta_csv_landsat_utm_temporal)

# 3. Intersección Espacial con Grilla MODIS --------------------------------
cat("Realizando intersección espacial con la grilla MODIS...\n")
poligonos_grilla_modis <- vect(ruta_shp_grilla_modis)
puntos_landsat_espacial_utm <- vect(datos_landsat_utm, geom = c("x", "y"), crs = crs_destino_utm)

landsat_con_info_modis_raw <- terra::intersect(puntos_landsat_espacial_utm, poligonos_grilla_modis)
landsat_interseccion_df <- as_tibble(as.data.frame(landsat_con_info_modis_raw, geom = "XY"))

# 4. Resolución de Duplicados y Extracción Final ---------------------------
# Resuelve asignaciones múltiples tomando solo la primera coincidencia
landsat_con_info_modis_filtrado <- landsat_interseccion_df %>%
  group_by(id_pixel_landsat_unico, Seas) %>%
  slice_head(n = 1) %>% 
  ungroup()

datos_landsat_muestreados <- landsat_con_info_modis_filtrado %>%
  select(
    id_pixel_landsat_unico, x, y, Seas,
    sos, eos, los, peak_t, Ampl, Linteg, Sinteg,
    year_sos, year_eos, year_peak_t,
    MODIS_ID, Cobertura
  ) %>%
  distinct(id_pixel_landsat_unico, Seas, MODIS_ID, .keep_all = TRUE)

write_csv(datos_landsat_muestreados, ruta_csv_landsat_muestreado_salida)

cat("Proceso completado. Datos Landsat muestreados guardados en:", ruta_csv_landsat_muestreado_salida, "\n")

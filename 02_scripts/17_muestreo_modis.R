# ==========================================================================
# Script: 17_muestreo_estratificado.R
# Descripción: Estandariza la grilla de análisis a la resolución de MODIS 
# (250m). Cruza los píxeles de MODIS con la cobertura de Landsat que fue utilizada meramenta para clasificar,
# aplicando un umbral de dominancia (70%) para clasificar el píxel, y realiza un 
# muestreo aleatorio estratificado unificado.
# ==========================================================================

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Configuración de Rutas y Parámetros -----------------------------------
ruta_csv_fenologia_modis_entrada <- "03_results/metricas_crudas/m_fenologia.csv" 
ruta_csv_muestreo_salida <- "03_results/muestreo_espacial/muestreo_modis_coberturas.csv" 
ruta_shp_coberturas <- "01_data/vectorial/landsatt_vector.shp" 
ruta_shp_modis_grid <- "01_data/vectorial/modis_grid.shp" 
ruta_raster_modis_base <- "01_data/raster/m_ndvi_16.tif"

n_muestras_por_cobertura <- 60
set.seed(123)

crs_modis_points <- "EPSG:4326" 
crs_utm_chile <- "EPSG:32719" 
umbral_porcentaje_cobertura <- 0.70 

# 2. Cargar y Preprocesar Datos --------------------------------------------
feno_modis_df <- read_csv(ruta_csv_fenologia_modis_entrada, show_col_types = FALSE)

# Obtener píxeles MODIS únicos
modis_unique_pixels <- feno_modis_df %>%
  distinct(id_row, x, y)

modis_points_spatvector_unique <- vect(modis_unique_pixels, geom = c("x", "y"), crs = crs_modis_points)
coberturas_shp <- vect(ruta_shp_coberturas)

# 3. Asignación de Cobertura Basada en Área (Umbral) -----------------------
# Proyección a UTM para cálculos de área correctos
modis_points_for_area_calc <- project(modis_points_spatvector_unique, crs_utm_chile)
coberturas_shp_for_area_calc <- project(coberturas_shp, crs_utm_chile)

# Polígonos base MODIS (250x250m)
modis_grid_polygons_for_intersect <- terra::buffer(modis_points_for_area_calc, width = 125, quadsegs = 1, capstyle = "SQUARE")

interseccion <- terra::intersect(modis_grid_polygons_for_intersect, coberturas_shp_for_area_calc)
interseccion$area_interseccion <- expanse(interseccion)
area_pixel_modis <- 250 * 250

cobertura_asignada_por_pixel_unico <- as.data.frame(interseccion) %>%
  as_tibble() %>%
  select(id_row, VALUE, area_interseccion) %>%
  group_by(id_row, VALUE) %>%
  summarise(total_area_value = sum(area_interseccion, na.rm = TRUE), .groups = "drop") %>%
  mutate(porcentaje_superposicion = total_area_value / area_pixel_modis) %>%
  filter(porcentaje_superposicion >= umbral_porcentaje_cobertura) %>%
  group_by(id_row) %>%
  slice_max(porcentaje_superposicion, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(Cobertura_raw_value = VALUE) %>%
  select(id_row, Cobertura_raw_value)

feno_modis_con_cobertura <- feno_modis_df %>%
  inner_join(cobertura_asignada_por_pixel_unico, by = "id_row") %>%
  mutate(Cobertura = as.factor(case_when(
    Cobertura_raw_value == 0 ~ "Suelo desprovisto",
    Cobertura_raw_value == 2 ~ "Bosque",
    Cobertura_raw_value == 3 ~ "Matorral y pradera",
    TRUE ~ NA_character_
  ))) %>%
  select(-Cobertura_raw_value) %>%
  filter(!is.na(Cobertura))

valid_pixels_for_sampling <- feno_modis_con_cobertura %>%
  group_by(x, y, Cobertura, id_row) %>%
  filter(if_any(c(sos, eos, los, peak_t, Ampl, Linteg, Sinteg), ~!is.na(.))) %>%
  ungroup() %>%
  distinct(x, y, Cobertura, id_row)

# 4. Muestreo Aleatorio Estratificado --------------------------------------
muestreo_modis <- valid_pixels_for_sampling %>%
  group_by(Cobertura) %>%
  slice_sample(n = n_muestras_por_cobertura, replace = FALSE) %>%
  ungroup()

datos_completos_muestreo_01_data/vectorial/ <- feno_modis_con_cobertura %>%
  inner_join(muestreo_modis %>% select(id_row), by = "id_row") %>%
  distinct(id_row, Seas, .keep_all = TRUE)

write_csv(datos_completos_muestreo_01_data/vectorial/, ruta_csv_muestreo_salida)

# 5. Generación de Grilla Base (Shapefile) ---------------------------------
modis_base_raster <- rast(ruta_raster_modis_base)
modis_base_raster_utm <- project(modis_base_raster[[1]], crs_utm_chile)

modis_unique_pixels_for_grid <- muestreo_modis %>%
  mutate(MODIS_ID = id_row) %>%
  select(x, y, MODIS_ID, Cobertura)

modis_points_utm_for_grid <- project(vect(modis_unique_pixels_for_grid, geom = c("x", "y"), crs = crs_modis_points), crs_utm_chile)

r_template <- rast(modis_base_raster_utm)
r_modis_id <- rasterize(modis_points_utm_for_grid, r_template, field = "MODIS_ID", fun = "first")

modis_points_utm_for_grid$Cobertura_num <- as.numeric(as.factor(modis_points_utm_for_grid$Cobertura))
r_cobertura_num <- rasterize(modis_points_utm_for_grid, r_template, field = "Cobertura_num", fun = "first")

s_out <- c(r_modis_id, r_cobertura_num)
names(s_out) <- c("MODIS_ID", "Cobertura_num")

modis_grid_polygons_utm <- as.polygons(s_out, dissolve = FALSE, values = TRUE)

niveles_cobertura <- levels(as.factor(muestreo_modis$Cobertura))
modis_grid_polygons_utm$Cobertura <- factor(modis_grid_polygons_utm$Cobertura_num,
                                            levels = as.numeric(as.factor(niveles_cobertura)),
                                            labels = niveles_cobertura)

modis_grid_polygons_utm <- modis_grid_polygons_utm %>%
  filter(!is.na(MODIS_ID)) %>%
  select(MODIS_ID, Cobertura) 

writeVector(modis_grid_polygons_utm, ruta_shp_modis_grid, overwrite = TRUE)

cat("Proceso completado. Grilla base generada.\n")

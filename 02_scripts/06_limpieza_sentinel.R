# ==========================================================================
# Script: 06_limpieza_sentinel.R
# Descripción: Limpieza y regularización de series de tiempo NDVI de Sentinel.
# Extrae fechas, agrupa por periodos y aplica interpolación temporal (na.approx).
# 
# ⚠️ ADVERTENCIA DE HARDWARE / RAM ⚠️
# Debido a la alta resolución espacial de Sentinel-2 (10m), el volumen de 
# datos procesados en este script es abrumadoramente mayor que MODIS o Landsat. 
# Las funciones de reestructuración (pivot_longer, complete) consumen mucha 
# memoria RAM. Se recomienda ejecutar este proceso en un servidor o en un 
# equipo con alta capacidad de memoria; de lo contrario, la sesión de R 
# podría abortarse por falta de recursos.
# ==========================================================================

library(terra)
library(tidyverse)
library(lubridate)
library(zoo) 
library(readr) 

# 1. Configuración ---------------------------------------------------------
s_path <- "01_data/raster/s_ndvi_16.tif" 
s_csv_path <- "01_data/raw/sentinel_ndvi.csv" 

target_years <- 2019:2024 
periods_per_year <- 24

# 2. Cargar Raster y extraer información de bandas -------------------------
if (!exists("sentinel_rast_obj") || !inherits(sentinel_rast_obj, "SpatRaster")) {
  if (file.exists(s_path)) {
    sentinel_rast_obj <- rast(s_path)
    cat("Raster cargado desde:", s_path, "\n")
  } else {
    stop("El archivo raster ", s_path, " no se encontró. Deteniendo.")
  }
}

sentinel_df_raw <- as.data.frame(sentinel_rast_obj, xy = TRUE, cells = TRUE, na.rm = FALSE)

band_info_original <- tibble(
  original_band_name = names(sentinel_rast_obj), 
  date_str = str_extract(original_band_name, "(?<=_)\\d{8}$") 
) %>%
  mutate(
    date = ymd(date_str),
    year = year(date),
    doy = yday(date),
    assigned_period_in_year = ceiling(doy / (365.25 / periods_per_year)),
    assigned_period_in_year = pmin(assigned_period_in_year, periods_per_year) 
  ) %>%
  filter(year %in% target_years)

# 3. Procesamiento y regularización de la serie de tiempo ------------------
cat("Iniciando procesamiento principal...\n")

processed_data <- sentinel_df_raw %>%
  pivot_longer(cols = -(cell:y),
               names_to = "original_band_name",
               values_to = "ndvi_value") %>%
  filter(!is.na(ndvi_value)) %>%
  inner_join(band_info_original, by = "original_band_name") %>%
  group_by(cell, x, y, year, assigned_period_in_year) %>%
  summarise(ndvi_observed_agg = max(ndvi_value, na.rm = TRUE), .groups = "drop") %>%
  rename(target_period_idx = assigned_period_in_year) %>%
  tidyr::complete(nesting(cell, x, y),
                  year = target_years,
                  target_period_idx = 1:periods_per_year) %>%
  arrange(cell, year, target_period_idx) %>%
  group_by(cell) %>% 
  mutate(
    ndvi_interpolated = zoo::na.approx(ndvi_observed_agg, na.rm = FALSE, rule = 2)
  ) %>%
  mutate(ndvi_interpolated = ifelse(is.na(ndvi_interpolated) & !is.na(cell), 0, ndvi_interpolated)) %>%
  ungroup()

# 4. Formatear y exportar a CSV --------------------------------------------
cat("Pivotando a formato ancho y exportando...\n")

final_wide_df <- processed_data %>%
  mutate(period_name = sprintf("NDVI_%d_P%02d", year, target_period_idx)) %>%
  select(x, y, period_name, ndvi_interpolated) %>%
  pivot_wider(names_from = period_name,
              values_from = ndvi_interpolated)

final_wide_df <- final_wide_df %>%
  mutate(id_row = 1:n(), .before = 1)

period_column_names_sorted <- processed_data %>%
  distinct(year, target_period_idx) %>%
  arrange(year, target_period_idx) %>%
  mutate(period_name = sprintf("NDVI_%d_P%02d", year, target_period_idx)) %>%
  pull(period_name)

final_wide_df <- final_wide_df %>% 
  select(id_row, x, y, all_of(period_column_names_sorted))

readr::write_csv(final_wide_df, s_csv_path)

cat("Proceso completado. Archivo CSV guardado en:", s_csv_path, "\n")

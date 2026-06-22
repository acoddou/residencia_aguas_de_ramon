# ==========================================================================
# Script: 04_limpieza_landsat.R
# Descripción: Limpieza y regularización de series de tiempo NDVI de Landsat.
# Extrae fechas, agrupa por periodos y aplica interpolación temporal (na.approx).
# ==========================================================================

library(terra)
library(tidyverse)
library(lubridate)
library(zoo) 
library(readr) 

# 1. Configuración ---------------------------------------------------------
raster_path <- "01_data/raster/l_ndvi_16_test.tif" 
output_csv_path <- "01_data/raw/landsat_ndvi_test.csv"

target_years <- 2016:2024
periods_per_year <- 24

# 2. Cargar Raster y extraer información de bandas -------------------------
if (!exists("landsat") || !inherits(landsat, "SpatRaster")) {
  if (file.exists(raster_path)) {
    landsat <- rast(raster_path)
    cat("Raster cargado desde:", raster_path, "\n")
  } else {
    stop("El archivo raster ", raster_path, " no se encontró. Deteniendo.")
  }
}

band_info_original <- tibble(
  original_band_name = names(landsat),
  date_str = str_extract(original_band_name, "(?<=_)\\d{8}(?=_)")
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

processed_data <- as.data.frame(landsat, xy = TRUE, cells = TRUE, na.rm = FALSE) %>%
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

readr::write_csv(final_wide_df, output_csv_path)

cat("Proceso completado. Archivo CSV guardado en:", output_csv_path, "\n")

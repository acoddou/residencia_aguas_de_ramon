# ==========================================================================
# Script: 21_series_tiempo_ndvi.R
# Descripción: Extrae las series de tiempo crudas de NDVI de MODIS, 
# Landsat y Sentinel, las alinea temporalmente y genera gráficos de 
# línea comparativos por tipo de cobertura.
# ==========================================================================

library(tidyverse)
library(sf)
library(readr)
library(lubridate)

# 1. Configuración de Rutas y Parámetros -----------------------------------
MODIS_NDVI_WIDE_FILE <- "01_data/raw/modis_ndvi.csv"
LANDSAT_NDVI_WIDE_FILE <- "01_data/raw/landsat_ndvi_test.csv"
SENTINEL_NDVI_WIDE_FILE <- "01_data/raw/sentinel_ndvi.csv"

MODIS_GRID_SHP_FILE <- "01_data/vectorial/modis_grid.shp"
LANDSAT_SAMPLE_COBERTURAS_FILE <- "03_results/muestreo_espacial/muestreo_landsat_coberturas_test.csv"
SENTINEL_SAMPLE_COBERTURAS_FILE <- "03_results/muestreo_espacial/muestreo_sentinel_coberturas.csv"

RESULTS_DIR <- "03_results"
BASE_PLOTS_DIR <- "04_plots"
TS_PLOTS_DIR <- file.path(BASE_PLOTS_DIR, "series_tiempo")
OUTPUT_ALL_SAT_AVG_TS_CSV <- file.path(RESULTS_DIR, "series_tiempo/all_satellites_avg_ndvi_ts.csv")

sat_colors_line <- c("MODIS" = "blue4", "Landsat" = "darkgreen", "Sentinel" = "darkorange")

if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR)
if (!dir.exists(BASE_PLOTS_DIR)) dir.create(BASE_PLOTS_DIR)
if (!dir.exists(TS_PLOTS_DIR)) dir.create(TS_PLOTS_DIR)

# 2. Función de Extracción y Estandarización -------------------------------
yday_to_date <- function(year, doy) {
  as.Date(paste0(year, "-01-01")) + days(doy - 1)
}

process_satellite_ndvi <- function(ndvi_wide_file, sample_info_file, satellite_name,
                                   pixel_id_col_in_sample_info, date_regex_pattern) {
  
  if (satellite_name == "MODIS") {
    pixel_coberturas_df <- st_read(sample_info_file, quiet = TRUE) %>%
      as_tibble() %>% select(pixel_id = MODIS_ID, Cobertura) %>% distinct() %>% filter(!is.na(Cobertura) & Cobertura != "")
  } else {
    pixel_coberturas_df <- read_csv(sample_info_file, show_col_types = FALSE) %>%
      select(pixel_id = !!sym(pixel_id_col_in_sample_info), Cobertura) %>% distinct() %>% filter(!is.na(Cobertura) & Cobertura != "")
  }
  
  ndvi_wide_data <- read_csv(ndvi_wide_file, show_col_types = FALSE) %>% rename(pixel_id = 1)
  
  cols_to_pivot <- setdiff(names(ndvi_wide_data), c("pixel_id", "x", "y"))
  
  long_data <- left_join(ndvi_wide_data, pixel_coberturas_df, by = "pixel_id") %>%
    filter(!is.na(Cobertura)) %>%
    pivot_longer(cols = all_of(cols_to_pivot), names_to = "date_col_name", values_to = "NDVI") %>%
    mutate(Satellite = satellite_name, date_extracted_str = str_extract(date_col_name, date_regex_pattern))
  
  if (satellite_name == "MODIS") {
    long_data <- long_data %>% mutate(date = as.Date(gsub("_", "-", date_extracted_str)))
  } else if (satellite_name %in% c("Landsat", "Sentinel")) {
    long_data <- long_data %>%
      mutate(
        year = as.numeric(str_extract(date_extracted_str, "\\d{4}")),
        period_num = as.numeric(str_extract(date_extracted_str, "(?<=P)\\d{2}$")),
        doy = round((period_num - 1) * (365 / 23) + (365 / 23) / 2), 
        date = yday_to_date(year, doy) 
      )
  }
  
  long_data %>% filter(!is.na(NDVI) & !is.na(date)) %>% select(Satellite, Cobertura, date, NDVI)
}

# 3. Procesamiento y Combinación -------------------------------------------
cat("Procesando series temporales por satélite...\n")

modis_data <- process_satellite_ndvi(MODIS_NDVI_WIDE_FILE, MODIS_GRID_SHP_FILE, "MODIS", "MODIS_ID", "^\\d{4}_\\d{2}_\\d{2}")
landsat_data <- process_satellite_ndvi(LANDSAT_NDVI_WIDE_FILE, LANDSAT_SAMPLE_COBERTURAS_FILE, "Landsat", "id_pixel_landsat_unico", "NDVI_\\d{4}_P\\d{2}")
sentinel_data <- process_satellite_ndvi(SENTINEL_NDVI_WIDE_FILE, SENTINEL_SAMPLE_COBERTURAS_FILE, "Sentinel", "id_pixel_sentinel_unico", "NDVI_\\d{4}_P\\d{2}")

all_sat_ts_long <- bind_rows(modis_data, landsat_data, sentinel_data) %>% 
  filter(date <= as.Date("2024-12-31"))

# 4. Cálculo de Promedios y Visualización ----------------------------------
cat("Calculando promedios y generando gráficos...\n")

avg_all_sat_ndvi_ts <- all_sat_ts_long %>%
  group_by(Cobertura, Satellite, date) %>%
  summarise(Mean_NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop") %>%
  arrange(Cobertura, Satellite, date)

write_csv(avg_all_sat_ndvi_ts, OUTPUT_ALL_SAT_AVG_TS_CSV)

unique_coverages <- unique(avg_all_sat_ndvi_ts$Cobertura)

for (cov_name in unique_coverages) {
  plot_data_for_cov <- avg_all_sat_ndvi_ts %>%
    filter(Cobertura == cov_name) %>%
    filter(n_distinct(Satellite) >= 2)
  
  if (nrow(plot_data_for_cov) == 0) next
  
  p_ndvi_ts <- ggplot(plot_data_for_cov, aes(x = date, y = Mean_NDVI, color = Satellite)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    labs(
      title = paste("Serie de Tiempo de NDVI Promedio:", cov_name),
      subtitle = "Comparación Inter-Sensor",
      x = "Fecha", y = "NDVI", color = "Satélite"
    ) +
    scale_color_manual(values = sat_colors_line) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  file_name_clean <- tolower(gsub("[^[:alnum:]_]", "", gsub(" ", "_", paste0("ts_ndvi_", cov_name))))
  ggsave(file.path(TS_PLOTS_DIR, paste0(file_name_clean, ".png")), plot = p_ndvi_ts, width = 12, height = 7, bg = "white")
}

cat("Proceso completado. Gráficos de series de tiempo guardados en '04_plots/series_tiempo/'.\n")

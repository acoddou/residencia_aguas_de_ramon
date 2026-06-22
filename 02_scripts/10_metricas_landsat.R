# ==========================================================================
# Script: 10_metricas_landsat.R
# Descripción: Asocia las métricas extraídas de TIMESAT a los píxeles 
# originales (id_row) de Landsat y transforma los índices temporales a 
# Días del Año (DOY) y años calendario.
# ==========================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(terra)
library(tidyverse)
library(lubridate)

# 1. Configuración de Parámetros -------------------------------------------
METRICAS_FILE <- "01_data/metricas_landsat_test" 
COORD_FILE <- "01_data/raw/landsat_ndvi_test.csv" 

EPOCH_LENGTH_DAYS <- 16 
TOTAL_YEARS_IN_SERIES <- 9 
START_YEAR_DATA <- 2016 

# 2. Cargar datos y limpieza inicial ---------------------------------------
l_seasonality <- read_table(METRICAS_FILE, show_col_types = FALSE)

if ("X17" %in% names(l_seasonality)) {
  l_seas_copia <- l_seasonality %>% select(-X17)
} else {
  l_seas_copia <- l_seasonality
}

# 3. Transformación de Índices a DOY y Años Calendario ---------------------
# NOTA METODOLÓGICA:
# TIMESAT exporta el tiempo de los eventos fenológicos como un índice 
# continuo acumulado sobre toda la serie temporal. Para analizar la 
# variación interanual, es obligatorio desglosar este índice continuo 
# y transformarlo en Días Julianos (DOY) y su respectivo año calendario.
# --------------------------------------------------------------------------
l_seas_copia <- l_seas_copia %>%
  mutate(
    year_in_series = as.integer(Begt / (365.25 / EPOCH_LENGTH_DAYS)), 
    
    sos = as.integer((Begt %% (365.25 / EPOCH_LENGTH_DAYS)) * EPOCH_LENGTH_DAYS + 1),
    eos = as.integer((Endt %% (365.25 / EPOCH_LENGTH_DAYS)) * EPOCH_LENGTH_DAYS + 1),
    peak_t = as.integer((Maxt %% (365.25 / EPOCH_LENGTH_DAYS)) * EPOCH_LENGTH_DAYS + 1),
    
    los = as.integer(Length * EPOCH_LENGTH_DAYS),
    
    year_sos = START_YEAR_DATA + year_in_series,
    year_eos = START_YEAR_DATA + as.integer(Endt / (365.25 / EPOCH_LENGTH_DAYS)),
    year_peak_t = START_YEAR_DATA + as.integer(Maxt / (365.25 / EPOCH_LENGTH_DAYS))
  ) %>%
  mutate(
    sos = pmax(1, pmin(366, sos)),
    eos = pmax(1, pmin(366, eos)),
    peak_t = pmax(1, pmin(366, peak_t))
  ) %>%
  select(Row, Col, Seas, 
         sos, eos, los, peak_t, 
         year_sos, year_eos, year_peak_t, 
         Baseval, Maxval, Ampl, Lder, Rder, Linteg, Sinteg, Startval, Endval)

# 4. Unión con coordenadas originales y exportación ------------------------
l_dataset <- read_csv(COORD_FILE, show_col_types = FALSE)

l_dataset_coor <- l_dataset %>%
  select(id_row, x, y)

l_fenologia <- left_join(l_dataset_coor, l_seas_copia, by = c("id_row" = "Row"))

write_csv(l_fenologia, "03_results/l_fenologia_test.csv")

cat("Proceso completado. Métricas Landsat guardadas en: 03_results/l_fenologia_test.csv\n")

# ==========================================================================
# Script: 08_metricas_modis.R
# Descripción: Asocia las métricas extraídas de TIMESAT a los píxeles 
# originales (id_row) de MODIS y transforma los índices temporales a 
# Días del Año (DOY) y años calendario.
# ==========================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(terra)
library(tidyverse)

# 1. Configuración de Parámetros -------------------------------------------
METRICAS_FILE <- "01_data/metricas_modis" 
COORD_FILE <- "01_data/raw/modis_ndvi_01.csv" 

EPOCH_LENGTH_DAYS <- 16 
TOTAL_YEARS_IN_SERIES <- 9 
START_YEAR_DATA <- 2016 

# 2. Cargar datos y limpieza inicial ---------------------------------------
m_seasonality <- read_table(METRICAS_FILE, show_col_types = FALSE)

if ("X17" %in% names(m_seasonality)) {
  m_seas_copia <- m_seasonality %>% select(-X17)
} else {
  m_seas_copia <- m_seasonality
}

# 3. Transformación de Índices a DOY y Años Calendario ---------------------
# NOTA METODOLÓGICA:
# TIMESAT exporta el tiempo de los eventos fenológicos como un índice 
# continuo acumulado sobre toda la serie temporal (por ejemplo, reportando 
# el periodo 45 del total de periodos analizados). Para analizar la 
# variación interanual, es obligatorio desglosar este índice continuo 
# y transformarlo en Días Julianos (DOY) y su respectivo año calendario.
# --------------------------------------------------------------------------
m_seas_copia <- m_seas_copia %>%
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
m_dataset <- read_csv(COORD_FILE, show_col_types = FALSE)

m_dataset_coor <- m_dataset %>%
  select(id_row, x, y) 

m_fenologia <- left_join(m_dataset_coor, m_seas_copia, by = c("id_row" = "Row"))

write_csv(m_fenologia, "03_results/m_fenologia.csv")

cat("Proceso completado. Métricas MODIS guardadas en: 03_results/m_fenologia.csv\n")

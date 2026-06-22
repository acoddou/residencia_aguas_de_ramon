# ==========================================================================
# Script: 07_extraccion_series_sentinel.R
# Descripción: Convierte el CSV regularizado de Sentinel en un archivo .txt 
# con el formato estricto requerido por TIMESAT (encabezado + separador simple).
# ==========================================================================

library(dplyr)
library(readr)

# 1. Configuración ---------------------------------------------------------
input_csv_path <- "01_data/raw/sentinel_ndvi.csv" 
output_txt_timesat_path <- "01_data/sentinel_ndvi_timesat.txt" 
nyear <- 6 
nptperyear <- 24 

# 2. Carga y verificación de dimensiones -----------------------------------
if (!file.exists(input_csv_path)) {
  stop("El archivo de entrada CSV no se encontró: ", input_csv_path)
}

datos_csv_sentinel <- readr::read_csv(input_csv_path, show_col_types = FALSE)
nts <- nrow(datos_csv_sentinel)

if (nts == 0) {
  stop("El archivo CSV está vacío o no se pudo leer correctamente.")
}

expected_total_cols_with_id <- 3 + nyear * nptperyear

if (ncol(datos_csv_sentinel) != expected_total_cols_with_id) {
  warning(paste("El CSV no tiene el número de columnas esperado. Tiene:", ncol(datos_csv_sentinel), 
                "Esperado:", expected_total_cols_with_id))
}

# 3. Limpieza de columnas (ID y Coordenadas) -------------------------------
sentinel_ndvi_values_only <- datos_csv_sentinel %>%
  select(-(1:3)) 

# 4. Escritura en formato TIMESAT ------------------------------------------
cat("Generando archivo TXT para TIMESAT en:", output_txt_timesat_path, "\n")

con_timesat <- file(output_txt_timesat_path, open = "wt")

writeLines(paste(nyear, nptperyear, nts), con_timesat)

apply(sentinel_ndvi_values_only, 1, function(fila_de_valores) {
  fila_formateada <- sprintf("%.6f", as.numeric(fila_de_valores))
  linea_a_escribir <- paste(fila_formateada, collapse = " ")
  writeLines(linea_a_escribir, con_timesat)
})

close(con_timesat)

cat("Proceso completado. Píxeles procesados:", nts, "\n")

# ==========================================================================
# ¡ALTO AQUÍ! FLUJO EXTERNO EN TIMESAT 3.3
# ==========================================================================
# El archivo .txt ha sido generado exitosamente.
# Antes de ejecutar el siguiente script en R, debes:
# 1. Abrir el software TIMESAT 3.3.
# 2. Cargar el archivo .txt recién generado.
# 3. Ejecutar el análisis y exportar las métricas fenológicas resultantes.
# (Revisar el README del repositorio para los parámetros exactos a utilizar).

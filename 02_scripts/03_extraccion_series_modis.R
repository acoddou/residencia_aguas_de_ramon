# ==========================================================================
# Script: 03_extraccion_series_modis.R
# Descripción: Extrae series de tiempo NDVI de MODIS y las formatea para 
# TIMESAT 3.3, eliminando NAs y generando un archivo .txt compatible.
# ==========================================================================

library(terra)
library(dplyr)
library(readr)

# 1. Cargar raster MODIS y limpiar NAs -------------------------------------
ruta_modis <- "01_data/raster/m_ndvi_16_original.tif"
modis_raster <- rast(ruta_modis)

modis_df_sin_na <- as.data.frame(modis_raster, xy = TRUE) %>% na.omit()
modis_df_limpio <- as.data.frame(modis_raster) %>% na.omit()

# 2. Guardar CSV completo con coordenadas e ID -----------------------------
csv_modis <- "01_data/raw/modis_ndvi_01.csv"

modis_df_sin_na <- modis_df_sin_na %>%
  mutate(id_row = 1:n(), .before = 1)

write_csv(modis_df_sin_na, csv_modis)

# 3. Preparación del archivo base para TIMESAT -----------------------------
datos_csv_m <- read_csv(csv_modis)
nts <- nrow(datos_csv_m)

nombre_txt_limpio_temp <- "01_data/modis_ndvi_limpio_temp.txt"
write.table(modis_df_limpio, file = nombre_txt_limpio_temp,
            sep = " ", row.names = FALSE, col.names = FALSE, na = "")

# 4. Añadir encabezado con parámetros requeridos por TIMESAT ---------------
nyear <- 9    
nptperyear <- 23  

valores <- readLines(nombre_txt_limpio_temp)
nombre_txt_limpio_final <- "01_data/modis_ndvi_limpio.txt"

writeLines(
  c(paste(nyear, nptperyear, nts), valores),
  con = nombre_txt_limpio_final
)

# 5. Reescribir archivo para asegurar separador de un solo espacio ---------
datos_timesat <- read.table(nombre_txt_limpio_final, skip = 1)
con <- file(nombre_txt_limpio_final, open = "wt")

writeLines(paste(nyear, nptperyear, nts), con)

apply(datos_timesat, 1, function(fila) {
  writeLines(paste(fila, collapse = " "), con)
})

close(con)

# 6. Limpiar archivos temporales y reportar --------------------------------
file.remove(nombre_txt_limpio_temp)

cat("\nArchivo CSV completo guardado en:", csv_modis, "\n")
cat("Archivo TIMESAT generado en:", nombre_txt_limpio_final, "\n")
cat("Número de series temporales (filas):", nts, "\n")
cat("Parámetros TIMESAT (nyear, nptperyear, nts):", nyear, nptperyear, nts, "\n")
cat("Filas eliminadas (NA):", nrow(as.data.frame(modis_raster, xy = TRUE)) - nrow(modis_df_sin_na), "\n")


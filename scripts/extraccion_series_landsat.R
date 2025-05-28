
# Información -------------------------------------------------------------

# Este código tiene como objetivo poder extraer las series de tiempo del sensor
#MODIS a un formato legible para el software TIMESAT 3.3, extrayendo los NA's y 
# dejando un archivo .txt compatible con el software.

# Código ------------------------------------------------------------------
# Librerias
library(terra)
library(dplyr)
library(readr)

## 1. Cargar raster MODIS 
ruta_modis <- "raster/m_ndvi_16_original.tif"  # Cambia por tu ruta
modis_raster <- rast(ruta_modis)

## 2. Convertir a dataframe con coordenadas y eliminar filas con NA 
modis_df_sin_na <- as.data.frame(modis_raster, xy = TRUE) %>% na.omit()

## 3. Crear dataframe limpio sin coordenadas ni NA 
modis_df_limpio <- as.data.frame(modis_raster) %>% na.omit()

## 4. Guardar CSV completo (con coordenadas) 
csv_modis <- "data/modis_ndvi.csv"
write_csv(modis_df_sin_na, csv_modis)

## 5. Leer el CSV para contar filas (número de pixeles del área de estudio) 
datos_csv_m <- read_csv(csv_modis)
nts <- nrow(datos_csv_m)

## 6. Guardar TXT limpio temporal (sin encabezados, sin coordenadas, sin NA) 
nombre_txt_limpio_temp <- "data/modis_ndvi_limpio_temp.txt"
write.table(modis_df_limpio, file = nombre_txt_limpio_temp,
            sep = " ", row.names = FALSE, col.names = FALSE, na = "")

## 7. Añadir la primera línea con parámetros para TIMESAT 
# Número de años en la serie temporal (ajusta según tus datos)
nyear <- 9      
# Número de puntos por año (MODIS 16 días son 23 series po año)
nptperyear <- 23    

valores <- readLines(nombre_txt_limpio_temp)

# Se le agrega una linea con nyear, nptperyear, nts (numero de filas)
nombre_txt_limpio_final <- "data/modis_ndvi_limpio.txt"
writeLines(
  c(paste(nyear, nptperyear, nts), valores),
  con = nombre_txt_limpio_final
)
## 7b. Reescribir archivo con separador válido (1 espacio), línea por línea
# Cargar nuevamente el archivo con todos los valores
datos_timesat <- read.table(nombre_txt_limpio_final, skip = 1)

# Abrir conexión de escritura
con <- file(nombre_txt_limpio_final, open = "wt")

# Escribir encabezado TIMESAT (nyear, nptperyear, nts)
writeLines(paste(nyear, nptperyear, nts), con)

# Escribir datos con un solo espacio como separador
apply(datos_timesat, 1, function(fila) {
  writeLines(paste(fila, collapse = " "), con)
})

# Cerrar archivo
close(con)

## 8. Borrar archivo temporal 
file.remove(nombre_txt_limpio_temp)

## 9. Mensajes informativos 
cat("\nArchivo CSV completo con coordenadas guardado en:", nombre_csv_salida, "\n")
cat("Archivo limpio para TIMESAT generado en:", nombre_txt_limpio_final, "\n")
cat("Número de series temporales (filas):", nts, "\n")
cat("Parámetros añadidos en la primera línea:", nyear, nptperyear, nts, "\n")
cat("Filas eliminadas por contener NA:", nrow(as.data.frame(modis_raster, xy=TRUE)) - nrow(modis_df_sin_na), "\n")

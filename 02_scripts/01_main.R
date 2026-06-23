# ==========================================================================
# Script: 01_main.R
# Descripción: Script principal de configuración. Establece el entorno de 
# trabajo, instala dependencias, crea la estructura de directorios e 
# inicializa la sesión de Google Earth Engine (rgee).
# ==========================================================================

# 1. Instalación y Carga de Paquetes ---------------------------------------
cat("Cargando paquetes principales...\n")

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra,
  sf,
  rgee,
  ggspatial,
  rayshader,
  gt,
  ggdist
)

cat("✔️ Paquetes cargados correctamente.\n")

# 2. Configuración de la Estructura de Directorios -------------------------
cat("\nVerificando estructura de directorios del proyecto...\n")

directorios_base <- c(
  "01_data/raw",
  "01_data/raster",
  "01_data/timesat_txt",
  "01_data/vectorial",
  "02_scripts",
  "03_results/metricas_crudas",
  "03_results/muestreo_espacial",
  "03_results/reducciones_modis",
  "03_results/reducciones_landsat_test",
  "03_results/reducciones_sentinel",
  "03_results/series_tiempo",
  "04_plots/3d_renders",
  "04_plots/boxplots",
  "04_plots/densidades",
  "04_plots/rainclouds",
  "04_plots/relaciones",
  "04_plots/series_tiempo",
  "04_plots/tablas",
  "04_plots/violines"
)

for (dir in directorios_base) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("  [Creado] %s\n", dir))
  }
}
cat("✔️ Arquitectura de directorios validada.\n")

# 3. Inicialización de Google Earth Engine ---------------------------------
cat("\nInicializando sesión de Google Earth Engine...\n")

tryCatch({
  # REEMPLAZA "TU_USUARIO_AQUI" CON TU CUENTA DE GOOGLE EARTH ENGINE
  rgee::ee_Initialize(user = "TU_USUARIO_AQUI", drive = TRUE)
  cat("✔️ GEE inicializado correctamente.\n")
}, error = function(e) {
  cat("⚠️ Error al inicializar GEE. Ejecuta ee_Authenticate() en la consola primero.\n")
})

cat("\n========================================================================\n")
cat("¡Entorno configurado con éxito! Listo para el procesamiento espacial.\n")
cat("========================================================================\n")


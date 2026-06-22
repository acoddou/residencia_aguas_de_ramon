# ==========================================================================
# Script: 15_l_visualizacion_metricas.R
# Descripción: Genera animaciones GIF (.gif) para cada métrica fenológica 
# de Landsat a lo largo de las temporadas utilizando gganimate.
# ==========================================================================

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski)
library(stringr)

# 1. Configuración de Parámetros -------------------------------------------
raster_dir <- "03_results/reducciones_landsat_test" 

metric_files_to_process <- c(
  "l_fenologia_Ampl_test.tif",
  "l_fenologia_EOS_test.tif",
  "l_fenologia_Linteg_test.tif",
  "l_fenologia_LOS_test.tif",
  "l_fenologia_PeakT_test.tif",
  "l_fenologia_SOS_test.tif"
)

full_raster_paths <- file.path(raster_dir, metric_files_to_process)
cat("Iniciando generación de GIFs para métricas Landsat...\n")

# 2. Bucle de Procesamiento y Animación ------------------------------------
for (file_path in full_raster_paths) {
  
  # Limpieza del nombre para usarlo en los títulos del gráfico
  metric_name_raw <- tools::file_path_sans_ext(basename(file_path)) 
  metric_name_clean <- str_remove(metric_name_raw, "l_fenologia_") 
  metric_name_clean <- str_remove(metric_name_clean, "_test") 
  
  cat(paste0("\nProcesando: ", metric_name_clean, "...\n"))
  
  r_metric_full <- rast(file_path)
  
  # Restricción a las primeras 8 temporadas
  if (nlyr(r_metric_full) > 8) {
    r_metric <- r_metric_full[[1:8]] 
  } else if (nlyr(r_metric_full) == 8) {
    r_metric <- r_metric_full 
  } else {
    cat(paste0("Saltando ", metric_name_clean, ": menos de 8 capas.\n"))
    next 
  }
  
  season_names <- paste0("Temporada_", sprintf("%02d", 1:8))
  names(r_metric) <- season_names
  
  # Transformación a formato largo para ggplot
  raster_df_wide <- as.data.frame(r_metric, xy = TRUE)
  
  raster_df_long <- raster_df_wide %>%
    pivot_longer(
      cols = starts_with("Temporada_"), 
      names_to = "temporada_id",        
      values_to = "value"               
    ) %>%
    mutate(temporada_id = factor(temporada_id, levels = season_names))
  
  # Creación del Plot Base (Paleta Viridis para Landsat)
  p_base <- ggplot(data = raster_df_long, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", direction = -1, option = "viridis") +
    labs(
      title = paste0("Métrica Landsat: ", metric_name_clean, " - {closest_state}"), 
      fill = "Valor"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_equal()
  
  # Configuración de Animación
  p_animated <- p_base +
    transition_states(temporada_id, transition_length = 0.5, state_length = 1.5) +
    ease_aes('linear')
  
  # Nombre de salida forzando el ecosistema test para evitar mezclas
  gif_output_file <- file.path(raster_dir, paste0("gif_fenologia_", metric_name_clean, "_test.gif"))
  
  num_seasons <- 8 
  total_animation_duration_seconds <- (num_seasons * 1.5) + ((num_seasons - 1) * 0.5)
  
  animate(p_animated,
          nframes = total_animation_duration_seconds * 10,
          fps = 10,
          width = 900,
          height = 700,
          duration = total_animation_duration_seconds,
          renderer = gifski_renderer(gif_output_file),
          start_pause = 10,
          end_pause = 10
  )
  
  cat("GIF guardado (seguro):", gif_output_file, "\n")
}

cat("\nGeneración de animaciones Landsat completada.\n")

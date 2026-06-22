# ==========================================================================
# Script: 16_s_visualizacion_metricas.R
# Descripción: Genera animaciones GIF (.gif) para cada métrica fenológica 
# de Sentinel a lo largo de las temporadas utilizando gganimate.
# ==========================================================================

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski)
library(stringr)

# 1. Configuración de Parámetros -------------------------------------------
raster_dir <- "03_results/reducciones_sentinel" 

metric_files_to_process <- c(
  "s_fenologia_Ampl.tif",
  "s_fenologia_EOS.tif",
  "s_fenologia_Linteg.tif",
  "s_fenologia_LOS.tif",
  "s_fenologia_PeakT.tif",
  "s_fenologia_SOS.tif"
)

full_raster_paths <- file.path(raster_dir, metric_files_to_process)
cat("Iniciando generación de GIFs para métricas Sentinel...\n")

# 2. Bucle de Procesamiento y Animación ------------------------------------
for (file_path in full_raster_paths) {
  
  # Limpieza del nombre para usarlo en los títulos del gráfico
  metric_name_raw <- tools::file_path_sans_ext(basename(file_path)) 
  metric_name_clean <- str_remove(metric_name_raw, "s_fenologia_") 
  
  cat(paste0("\nProcesando: ", metric_name_clean, "...\n"))
  
  r_metric_full <- rast(file_path)
  
  # Restricción a las 5 temporadas válidas de Sentinel (excluyendo la capa NA final)
  expected_layers <- 5 
  total_layers_in_file <- nlyr(r_metric_full)
  
  if (total_layers_in_file > expected_layers) {
    r_metric <- r_metric_full[[1:expected_layers]] 
  } else if (total_layers_in_file == expected_layers) {
    r_metric <- r_metric_full 
  } else {
    cat(paste0("Saltando ", metric_name_clean, ": menos de ", expected_layers, " capas.\n"))
    next 
  }
  
  # Alineación de temporadas: Sentinel empieza en la temporada 4 de la serie base
  season_names <- paste0("Temporada_", sprintf("%02d", 4:(3 + expected_layers)))
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
  
  # Creación del Plot Base (Paleta Viridis)
  p_base <- ggplot(data = raster_df_long, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", direction = -1, option = "viridis") +
    labs(
      title = paste0("Métrica Sentinel: ", metric_name_clean, " - {closest_state}"), 
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
  
  # Nombre de salida forzando el identificador sentinel
  gif_output_file <- file.path(raster_dir, paste0("gif_fenologia_", metric_name_clean, "_sentinel.gif"))
  
  total_animation_duration_seconds <- (expected_layers * 1.5) + ((expected_layers - 1) * 0.5)
  
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
  
  cat("GIF guardado:", gif_output_file, "\n")
}

cat("\nGeneración de animaciones Sentinel completada.\n")

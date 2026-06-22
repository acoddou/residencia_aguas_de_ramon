# ==========================================================================
# Script: 23_raincloud_plots.R
# Descripción: Genera Raincloud Plots (nube de densidad + boxplot + jitter) 
# utilizando 'ggdist' para una visualización profesional sin choque de ejes.
# ==========================================================================

library(tidyverse)
# Instalación silenciosa de ggdist si no lo tienes en tu entorno
if (!requireNamespace("ggdist", quietly = TRUE)) install.packages("ggdist")
library(ggdist)

# 1. Configuración de Directorios -------------------------------------------
BASE_PLOTS_DIR <- "plots"
RAINCLOUD_DIR <- file.path(BASE_PLOTS_DIR, "rainclouds")
if (!dir.exists(RAINCLOUD_DIR)) dir.create(RAINCLOUD_DIR)

sat_colors <- c("MODIS" = "#FF8C00", "Landsat" = "#A034F0", "Sentinel" = "#159090")
metric_labels <- c(sos="Start of Season (SOS)", eos="End of Season (EOS)", peak_t="Peak of Season (PET)",
                   los="Length of Season (LOS)", Ampl="Amplitude (Ampl)", Linteg="Linear Integral (LIN)", Sinteg="Smoothed Integral (Sinteg)")

all_metrics_names <- c("sos", "eos", "peak_t", "los", "Ampl", "Linteg", "Sinteg")

# 2. Generación de Raincloud Plots -----------------------------------------
cat("Generando Raincloud Plots con ggdist...\n")

# NOTA: Se asume que 'sat_list_processed' y la función 'prepare_plot_data_final' 
# ya están cargadas en el ambiente desde el Script 22.

for (metric in all_metrics_names) {
  plot_data <- prepare_plot_data_final(metric, sat_list_processed)
  
  if (!is.null(plot_data) && n_distinct(plot_data$Satellite) >= 2 && nrow(plot_data) > 0) {
    
    p <- ggplot(plot_data, aes(x = fct_rev(Cobertura), y = Value, fill = Satellite, color = Satellite)) +
      
      # 1. La "Media Nube" (Densidad Asimétrica)
      stat_halfeye(
        adjust = 1.2,
        width = 0.5,
        .width = 0,             # Oculta la barra de intervalos interna
        justification = -0.3,   # Desplaza la nube hacia arriba/derecha para hacer espacio
        point_colour = NA,      # Oculta el punto de la media
        alpha = 0.5,
        position = position_dodge(width = 0.8)
      ) +
      
      # 2. El Boxplot clásico
      geom_boxplot(
        width = 0.15,
        outlier.shape = NA,     # Ocultamos los outliers del boxplot porque ya los veremos en la lluvia
        color = "black",
        alpha = 0.8,
        position = position_dodge(width = 0.8)
      ) +
      
      # 3. La "Lluvia" (Puntos crudos)
      geom_point(
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
        size = 1,
        alpha = 0.3
      ) +
      
      coord_flip() +
      scale_fill_manual(values = sat_colors) +
      scale_color_manual(values = sat_colors) +
      labs(title = paste("Raincloud Plot:", metric_labels[[metric]]),
           x = "Cobertura", y = metric_labels[[metric]], fill = "Satélite", color = "Satélite") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    file_name <- sprintf("raincloud_cobertura_%s.png", tolower(metric))
    ggsave(file.path(RAINCLOUD_DIR, file_name), p, width = 12, height = 7, bg = "white")
  }
}

cat("Proceso completado. Raincloud plots guardados en '04_plots/rainclouds/'.\n")

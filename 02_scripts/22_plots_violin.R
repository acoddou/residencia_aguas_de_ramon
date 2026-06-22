# ==========================================================================
# Script: 22_analisis_estadistico_violines_correlacion.R
# Descripción: Complementa el análisis estadístico generando diagramas de 
# violín para visualizar la distribución de las métricas fenológicas y 
# compila una tabla resumen cruzada con todas las correlaciones (R).
# ==========================================================================

library(tidyverse)
library(gt)
library(webshot2)

# 1. Configuración y Directorios -------------------------------------------
metric_labels <- c(
  sos = "Start of Season (SOS)",
  eos = "End of Season (EOS)",
  peak_t = "Peak of Season (PET)",
  los = "Length of Season (LOS)",
  Ampl = "Amplitude (AMP)",
  Linteg = "Linear Integral (LIN)",
  Sinteg = "Smoothed Integral (SIN)"
)

sat_colors <- c("MODIS" = "skyblue", "Landsat" = "forestgreen", "Sentinel" = "orange")

BASE_PLOTS_DIR <- "plots"
subdirs <- c("violines", "tablas")

if (!dir.exists(BASE_PLOTS_DIR)) dir.create(BASE_PLOTS_DIR)
for (d in subdirs) {
  if (!dir.exists(file.path(BASE_PLOTS_DIR, d))) dir.create(file.path(BASE_PLOTS_DIR, d))
}

# 2. Carga y Procesamiento de Datos ----------------------------------------
modis_data <- read_csv("03_results/muestreo_espacial/muestreo_modis_coberturas.csv", show_col_types = FALSE) %>%
  mutate(Satellite = "MODIS", MODIS_ID = as.character(id_row))

landsat_data <- read_csv("03_results/muestreo_espacial/muestreo_landsat_coberturas_test.csv", show_col_types = FALSE) %>%
  mutate(Satellite = "Landsat", MODIS_ID = as.character(MODIS_ID))

sentinel_data <- read_csv("03_results/muestreo_espacial/muestreo_sentinel_coberturas.csv", show_col_types = FALSE) %>%
  mutate(Satellite = "Sentinel", MODIS_ID = as.character(MODIS_ID))

process_data_for_comparison <- function(df, metrics, satellite_type) {
  df_processed <- df %>%
    mutate(
      year_sos = as.numeric(if("year_sos" %in% names(.)) .data$year_sos else NA_real_),
      year_peak_t = as.numeric(if("year_peak_t" %in% names(.)) .data$year_peak_t else NA_real_),
      year_eos = as.numeric(if("year_eos" %in% names(.)) .data$year_eos else NA_real_)
    ) %>%
    mutate(
      eos = ifelse(!is.na(eos) & !is.na(year_eos) & !is.na(year_sos) & year_eos > year_sos, as.numeric(eos) + 365, as.numeric(eos)),
      peak_t = ifelse(!is.na(peak_t) & !is.na(year_peak_t) & !is.na(year_sos) & year_peak_t > year_sos, as.numeric(peak_t) + 365, as.numeric(peak_t))
    ) %>%
    mutate(across(any_of(metrics), as.numeric))
  
  if (satellite_type == "MODIS") {
    return(df_processed %>% select(MODIS_ID, Cobertura, Satellite, any_of(metrics)))
  } else {
    return(
      df_processed %>%
        group_by(MODIS_ID, Cobertura, Satellite) %>%
        summarise(across(any_of(metrics), ~mean(., na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
        mutate(across(any_of(metrics), ~ifelse(is.nan(.), NA_real_, .)))
    )
  }
}

all_metrics_names <- c("sos", "eos", "peak_t", "los", "Ampl", "Linteg", "Sinteg")

sat_list_processed <- list(
  MODIS = process_data_for_comparison(modis_data, all_metrics_names, "MODIS"),
  Landsat = process_data_for_comparison(landsat_data, all_metrics_names, "Landsat"),
  Sentinel = process_data_for_comparison(sentinel_data, all_metrics_names, "Sentinel")
)

prepare_plot_data_final <- function(metric, sat_data_list) {
  map_dfr(sat_data_list, ~ {
    if (!(metric %in% names(.x))) return(NULL)
    .x %>% select(MODIS_ID, Cobertura, Satellite, Value = !!sym(metric)) %>% filter(!is.na(Value))
  }, .id = "Source")
}

# 3. Generación de Diagramas de Violín -------------------------------------
cat("Generando Diagramas de Violín...\n")

for (metric in all_metrics_names) {
  plot_data <- prepare_plot_data_final(metric, sat_list_processed)
  
  if (!is.null(plot_data) && n_distinct(plot_data$Satellite) >= 2 && nrow(plot_data) > 0) {
    p_violin <- ggplot(plot_data, aes(x = Cobertura, y = Value, fill = Satellite)) +
      geom_violin(trim = FALSE, alpha = 0.6, position = position_dodge(width = 0.8), scale = "width") +
      geom_boxplot(width = 0.1, color = "black", position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
      scale_fill_manual(values = sat_colors) +
      labs(title = paste("Distribución de", metric_labels[[metric]], "por Cobertura y Satélite"),
           x = "Cobertura",
           y = paste(metric_labels[[metric]], ifelse(metric %in% c("sos", "eos", "peak_t", "los"), "(Días del Año)", "(Unidades)")),
           fill = "Satélite") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "bottom", 
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            strip.text = element_text(size = 12, face = "bold"))
    
    file_name <- sprintf("violin_cobertura_%s.png", tolower(metric))
    ggsave(file.path(BASE_PLOTS_DIR, "violines", file_name), p_violin, width = 12, height = 7, bg = "white")
  }
}

# 4. Generación de Tabla de Correlaciones (R) ------------------------------
cat("Calculando correlaciones y generando tabla...\n")

relationship_metrics <- c("sos", "eos", "peak_t", "los", "Ampl", "Linteg", "Sinteg")

data_for_correlations <- inner_join(
  sat_list_processed$MODIS %>% select(MODIS_ID, Cobertura, any_of(relationship_metrics)) %>% rename_with(~ paste0(., "_MODIS"), -c(MODIS_ID, Cobertura)),
  sat_list_processed$Landsat %>% select(MODIS_ID, Cobertura, any_of(relationship_metrics)) %>% rename_with(~ paste0(., "_Landsat"), -c(MODIS_ID, Cobertura)),
  by = c("MODIS_ID", "Cobertura")
) %>%
  inner_join(
    sat_list_processed$Sentinel %>% select(MODIS_ID, Cobertura, any_of(relationship_metrics)) %>% rename_with(~ paste0(., "_Sentinel"), -c(MODIS_ID, Cobertura)),
    by = c("MODIS_ID", "Cobertura")
  ) %>%
  mutate(Cobertura = as.factor(Cobertura))

correlations_df <- data.frame(Metric = character(), MODIS_vs_Landsat = numeric(), MODIS_vs_Sentinel = numeric(), Landsat_vs_Sentinel = numeric(), stringsAsFactors = FALSE)

for (metric in relationship_metrics) {
  cor_modis_landsat <- cor(data_for_correlations[[paste0(metric, "_MODIS")]], data_for_correlations[[paste0(metric, "_Landsat")]], use = "complete.obs")
  cor_modis_sentinel <- cor(data_for_correlations[[paste0(metric, "_MODIS")]], data_for_correlations[[paste0(metric, "_Sentinel")]], use = "complete.obs")
  cor_landsat_sentinel <- cor(data_for_correlations[[paste0(metric, "_Landsat")]], data_for_correlations[[paste0(metric, "_Sentinel")]], use = "complete.obs")
  
  correlations_df <- bind_rows(correlations_df, data.frame(Metric = metric_labels[[metric]], MODIS_vs_Landsat = cor_modis_landsat, MODIS_vs_Sentinel = cor_modis_sentinel, Landsat_vs_Sentinel = cor_landsat_sentinel))
}

gt_cor_table <- correlations_df %>%
  gt() %>%
  tab_header(
    title = md("**Coeficientes de Correlación (R) de Métricas Fenológicas**"),
    subtitle = "Comparación entre Pares de Satélites"
  ) %>%
  cols_label(Metric = "Métrica", MODIS_vs_Landsat = "MODIS vs Landsat", MODIS_vs_Sentinel = "MODIS vs Sentinel", Landsat_vs_Sentinel = "Landsat vs Sentinel") %>%
  cols_align(align = "center", columns = c(MODIS_vs_Landsat, MODIS_vs_Sentinel, Landsat_vs_Sentinel)) %>%
  fmt_number(columns = c(MODIS_vs_Landsat, MODIS_vs_Sentinel, Landsat_vs_Sentinel), decimals = 2)

gtsave(gt_cor_table, file.path(BASE_PLOTS_DIR, "tablas", "tabla_resumen_correlaciones.png"), zoom = 2)

cat("Proceso completado. Violines guardados en '04_plots/violines/' y tabla guardada en '04_plots/tablas/'.\n")

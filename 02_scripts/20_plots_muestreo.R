# ==========================================================================
# Script: 20_analisis_estadistico_comparativo.R
# Descripción: Genera análisis estadísticos y visuales cruzados entre 
# MODIS, Landsat y Sentinel. Produce boxplots, densidades, correlaciones 
# y tablas resumen, organizándolos automáticamente en subcarpetas.
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
  Ampl = "Amplitude (Ampl)",
  Linteg = "Linear Integral (LIN)", 
  Sinteg = "Smoothed Integral (SIN)"
)

sat_colors <- c("MODIS" = "skyblue", "Landsat" = "forestgreen", "Sentinel" = "orange")
sat_colors_line <- c("MODIS" = "blue4", "Landsat" = "darkgreen", "Sentinel" = "darkorange")

# Estructura de carpetas ordenada
BASE_PLOTS_DIR <- "plots"
subdirs <- c("boxplots", "densidades", "relaciones", "tablas")

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
        summarise(across(.cols = any_of(metrics), .fns = ~mean(., na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
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

# 3. Generación de Gráficos ------------------------------------------------
cat("Generando gráficos y tablas en subcarpetas de '04_plots/'...\n")

# Boxplots
for (metric in all_metrics_names) {
  plot_data <- prepare_plot_data_final(metric, sat_list_processed)
  if (!is.null(plot_data) && n_distinct(plot_data$Satellite) >= 2 && nrow(plot_data) > 0) {
    p_boxplot <- ggplot(plot_data, aes(x = Cobertura, y = Value, fill = Satellite)) +
      geom_boxplot(outlier.alpha = 0.4, position = position_dodge(width = 0.8)) +
      labs(title = paste("Boxplot de", metric_labels[[metric]], "por Cobertura y Satélite"), 
           x = "Cobertura", y = paste(metric_labels[[metric]], ifelse(metric %in% c("sos","eos","peak_t","los"), "(Días del Año)", "(Unidades)")), fill = "Satélite") +
      scale_fill_manual(values = sat_colors) + theme_minimal(base_size = 15) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(file.path(BASE_PLOTS_DIR, "boxplots", sprintf("boxplot_cobertura_%s.png", tolower(metric))), p_boxplot, width = 12, height = 7, dpi = 600, bg = "white")
  }
}

# Densidades
density_metrics <- c("sos", "eos", "peak_t", "Linteg", "Sinteg") 
for (metric in density_metrics) {
  plot_data <- prepare_plot_data_final(metric, sat_list_processed)
  if (!is.null(plot_data) && n_distinct(plot_data$Satellite) >= 2 && nrow(plot_data) > 0) {
    p_density <- ggplot(plot_data, aes(x = Value, fill = Satellite, color = Satellite)) +
      geom_density(alpha = 0.6, adjust = 1.5, linewidth = 1) +
      facet_wrap(~ Cobertura, scales = "free_y") +
      labs(title = paste("Distribución de", metric_labels[[metric]], "por Cobertura y Satélite"), 
           x = paste(metric_labels[[metric]], ifelse(metric %in% c("sos","eos","peak_t","los"), "(Días del Año)", "(Unidades)")), y = "Densidad") +
      scale_fill_manual(values = sat_colors) + scale_color_manual(values = sat_colors_line) +
      theme_minimal(base_size = 15) + theme(legend.position = "bottom")
    
    ggsave(file.path(BASE_PLOTS_DIR, "densidades", sprintf("density_cobertura_%s.png", tolower(metric))), p_density, width = 12, height = 7, dpi = 600, bg = "white")
  }
}

# Relaciones
relationship_metrics <- c("sos", "eos", "peak_t", "Linteg", "Sinteg")
generate_relation_plot <- function(data, metric, sat1_name, sat2_name) {
  sat1_col <- paste0(metric, "_", sat1_name)
  sat2_col <- paste0(metric, "_", sat2_name)
  plot_data_relation <- data %>% filter(!is.na(.data[[sat1_col]]) & !is.na(.data[[sat2_col]]))
  if (nrow(plot_data_relation) == 0) return(NULL)
  
  r_value <- cor(plot_data_relation[[sat1_col]], plot_data_relation[[sat2_col]], use = "complete.obs")
  r_label <- paste0("R = ", round(r_value, 2))
  
  p_relation <- ggplot(plot_data_relation, aes(x = .data[[sat1_col]], y = .data[[sat2_col]], color = Cobertura)) +
    geom_point(alpha = 0.6, size = 2) + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    annotate("text", x = min(plot_data_relation[[sat1_col]], na.rm = TRUE), y = max(plot_data_relation[[sat2_col]], na.rm = TRUE),
             label = r_label, hjust = 0, vjust = 1, size = 5, fontface = "bold") +
    labs(title = paste("Relación entre", metric_labels[[metric]], "de", sat1_name, "y", sat2_name),
         x = paste(metric_labels[[metric]], sat1_name), y = paste(metric_labels[[metric]], sat2_name)) +
    theme_minimal(base_size = 15) + theme(legend.position = "bottom")
  
  ggsave(file.path(BASE_PLOTS_DIR, "relaciones", sprintf("relacion_%s_vs_%s_%s.png", tolower(sat1_name), tolower(sat2_name), tolower(gsub(" ", "_", metric)))), p_relation, width = 12, height = 7, dpi = 600, bg = "white")
}

for (metric in relationship_metrics) {
  data_ml <- inner_join(sat_list_processed$MODIS %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_MODIS"), -c(MODIS_ID, Cobertura)), sat_list_processed$Landsat %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_Landsat"), -c(MODIS_ID, Cobertura)), by = c("MODIS_ID", "Cobertura"))
  generate_relation_plot(data_ml, metric, "MODIS", "Landsat")
  
  data_ms <- inner_join(sat_list_processed$MODIS %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_MODIS"), -c(MODIS_ID, Cobertura)), sat_list_processed$Sentinel %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_Sentinel"), -c(MODIS_ID, Cobertura)), by = c("MODIS_ID", "Cobertura"))
  generate_relation_plot(data_ms, metric, "MODIS", "Sentinel")
  
  data_ls <- inner_join(sat_list_processed$Landsat %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_Landsat"), -c(MODIS_ID, Cobertura)), sat_list_processed$Sentinel %>% select(MODIS_ID, Cobertura, all_of(metric)) %>% rename_with(~ paste0(., "_Sentinel"), -c(MODIS_ID, Cobertura)), by = c("MODIS_ID", "Cobertura"))
  generate_relation_plot(data_ls, metric, "Landsat", "Sentinel")
}

# 4. Tablas ----------------------------------------------------------------
ordered_summary_metrics <- c("sos", "peak_t", "eos", "los", "Linteg", "Sinteg")
all_summary_data <- map_dfr(sat_list_processed, function(df_sat) {
  current_satellite <- unique(df_sat$Satellite)
  metrics_to_summarize <- intersect(ordered_summary_metrics, names(df_sat))
  if (length(metrics_to_summarize) == 0) return(NULL)
  
  if (current_satellite == "MODIS") {
    df_sat %>% pivot_longer(cols = all_of(metrics_to_summarize), names_to = "Metric", values_to = "Value") %>%
      group_by(Satellite, Cobertura, Metric) %>% summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE), N = n_distinct(MODIS_ID[!is.na(Value)]), .groups = "drop")
  } else {
    df_raw <- switch(current_satellite, "Landsat" = landsat_data, "Sentinel" = sentinel_data)
    summary_stats <- df_sat %>% pivot_longer(cols = all_of(metrics_to_summarize), names_to = "Metric", values_to = "Value") %>%
      group_by(Satellite, Cobertura, Metric) %>% summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE), .groups = "drop")
    
    pixel_id_col <- ifelse(current_satellite == "Landsat", "id_pixel_landsat_unico", "id_pixel_sentinel_unico")
    n_counts <- df_raw %>% pivot_longer(cols = all_of(metrics_to_summarize), names_to = "Metric", values_to = "Value") %>%
      filter(!is.na(Value)) %>% group_by(Cobertura, Metric) %>% summarise(N = n_distinct(.data[[pixel_id_col]]), .groups = "drop") %>% mutate(Satellite = current_satellite)
    
    left_join(summary_stats, n_counts, by = c("Satellite", "Cobertura", "Metric")) %>% mutate(N = replace_na(N, 0L))
  }
}) %>%
  mutate(Metric = recode(Metric, !!!metric_labels)) %>% select(Satellite, Cobertura, Metric, Mean, SD, N) %>%
  mutate(Satellite = factor(Satellite, levels = c("MODIS", "Landsat", "Sentinel")))

for (cov_name in unique(all_summary_data$Cobertura)) {
  table_data_cov <- all_summary_data %>% filter(Cobertura == cov_name)
  if (nrow(table_data_cov) > 0) {
    gt_table <- table_data_cov %>% gt(groupname_col = "Satellite", rowname_col = "Metric") %>%
      tab_header(title = md(paste0("**Resumen de Métricas Fenológicas: ", cov_name, "**"))) %>%
      cols_label(Mean = "Promedio", SD = "Desv. Est.", N = "N") %>% fmt_number(columns = c(Mean, SD), decimals = 2)
    gtsave(gt_table, file.path(BASE_PLOTS_DIR, "tablas", sprintf("tabla_resumen_%s.png", tolower(gsub(" ", "_", cov_name)))), zoom = 3)
  }
}

cat("Proceso completado. Gráficos y tablas guardados organizados en '04_plots/'.\n")

# ==========================================================================
# Script: 24_visualizacion_3d_cuenca.R
# Descripción: Genera un modelo 3D de la cobertura arbórea de la cuenca 
# Aguas de Ramón utilizando Google Earth Engine (Dynamic World) y rayshader.
# ==========================================================================

pacman::p_load(
  osmdata, rgee, tidyverse,
  terra, sf, ggspatial, rayshader, stars, geojsonio
)

# 1. Configuración de Directorios ------------------------------------------
BASE_PLOTS_DIR <- "plots"
RENDER_DIR <- file.path(BASE_PLOTS_DIR, "3d_renders")
if (!dir.exists(RENDER_DIR)) dir.create(RENDER_DIR, recursive = TRUE)

# 2. Descarga de Límites de la Cuenca (OSM) --------------------------------
cat("Obteniendo límites de la cuenca desde OpenStreetMap...\n")
osm_query <- osmdata::opq("Parque Natural Aguas de Ramón") %>%
  osmdata::add_osm_feature(key = "name", value = "Parque Natural Aguas de Ramón")

osm_poly <- osmdata::osmdata_sf(osm_query)
cuenca_poly <- if(nrow(osm_poly$osm_multipolygons) > 0) osm_poly$osm_multipolygons else osm_poly$osm_polygons

# 3. Descarga de Cobertura Arbórea (Dynamic World via GEE) -----------------
cat("Inicializando Google Earth Engine...\n")
rgee::ee_clean_user_credentials("agustincoddoudiaz")
rgee::ee_Initialize(user = "agustincoddoudiaz", drive = TRUE)

tree_cover_data <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01", "2024-12-31")$
  select("trees")$
  max()

cuenca_bbox <- sf::st_bbox(cuenca_poly)
cuenca_limit <- ee$Geometry$Rectangle(
  c(west = cuenca_bbox[["xmin"]], south = cuenca_bbox[["ymin"]], 
    east = cuenca_bbox[["xmax"]], north = cuenca_bbox[["ymax"]]),
  geodetic = TRUE, proj = "EPSG:4326"
)

cat("Descargando raster de cobertura arbórea...\n")
tree_cover_cuenca <- rgee::ee_as_rast(tree_cover_data, region = cuenca_limit, scale = 10)

# 4. Procesamiento Espacial ------------------------------------------------
tree_cover_mask <- terra::mask(tree_cover_cuenca, terra::vect(cuenca_poly))
tree_cover_proj <- terra::project(tree_cover_mask, "EPSG:32719")

# 5. Renderizado 2D con ggplot2 --------------------------------------------
cat("Generando mapa base 2D...\n")
tree_cover_df <- as.data.frame(tree_cover_proj, xy = TRUE, na.rm = TRUE) %>%
  rename(percent_cover = 3)

cols <- rev(c("#003724", "#405f35", "#606C38", "#97A664", "#D4A373", "#D0C78D", "#F3EFDE"))
min_val <- 0
max_val <- round(max(tree_cover_df$percent_cover), 1)
pal <- colorRampPalette(cols)(8)
breaks <- seq(min_val, max_val, by = 0.1)

theme_map <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(), axis.title = element_blank(),
      axis.text = element_blank(), panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, hjust = 0),
      legend.position = c(.1, .2),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

p <- ggplot(tree_cover_df) +
  geom_raster(aes(x = x, y = y, fill = percent_cover)) +
  scale_fill_gradientn(
    colors = pal, breaks = breaks, limits = c(min_val, max_val),
    name = "Probability of full\ntree cover"
  ) +
  annotation_scale(location = "bl", width_hint = .25, plot_unit = "m", pad_y = unit(2, "mm")) +
  annotation_north_arrow(
    location = "tr", style = north_arrow_orienteering,
    pad_y = unit(8, "mm"), pad_x = unit(2, "mm"),
    height = unit(10, "mm"), width = unit(10, "mm")
  ) +
  coord_sf(crs = 32719) +
  labs(title = "Cobertura de árboles - Cuenca Aguas de Ramón", caption = "Google Dynamic World V1") +
  theme_map()

ggsave(file.path(RENDER_DIR, "tree_cover_cuenca_2d_max.png"), width = 7.5, height = 7, dpi = 600, bg = "white", plot = p)

# 6. Renderizado 3D con Rayshader ------------------------------------------
cat("Iniciando renderizado 3D con rayshader...\n")

rayshader::plot_gg(
  ggobj = p, width = 7.5, height = 7, windowsize = c(750, 700),
  scale = 75, shadow = FALSE, shadow_intensity = 1, zoom = .68,
  phi = 89, theta = 0
)

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/snow_field_4k.hdr"
hdri_file <- file.path(RENDER_DIR, basename(url))

if(!file.exists(hdri_file)) {
  download.file(url = url, destfile = hdri_file, mode = "wb")
}

filename <- file.path(RENDER_DIR, "cuenca_3D_completa.png")
rayshader::render_highquality(
  filename = filename, preview = TRUE, light = FALSE,
  environment_light = hdri_file, intensity_env = 1.75,
  interactive = FALSE, parallel = TRUE,
  width = 750 * 3, height = 700 * 3
)

cat("¡Renderizado completo! Imágenes guardadas en '04_plots/3d_renders/'.\n")

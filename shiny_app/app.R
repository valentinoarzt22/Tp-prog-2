# ==== 1. Cargar Librerías ====
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets) # Para controles más bonitos
  library(bslib)        # Para la interfaz moderna
  library(data.table)   # Para manejo rápido de datos
  library(dplyr)        # Para manipulación de datos
  library(lubridate)    # Para manejo de fechas
  library(plotly)       # Para gráficos interactivos
  library(leaflet)      # Para los mapas
  library(scales)       # Para escalas en gráficos
  library(janitor)      # Para limpieza de nombres
  library(fs)           # Para listar los gráficos estáticos
  library(ggplot2)      # Para gráficos (aunque usaremos plotly)
  library(sf)           # GeoJSON
})

# ==== 2. Función de Lectura Robusta ====
read_obj <- function(name, dirs = c("data", "../data", "..")) {
  cands <- if (grepl("\\.(qs|rds|RDS)$", name)) {
    name
  } else {
    c(paste0(name, ".qs"), paste0(name, ".rds"), paste0(name, ".RDS"))
  }
  paths <- unique(unlist(lapply(dirs, function(d) file.path(d, cands)), use.names = FALSE))
  hits  <- paths[file.exists(paths)]
  if (!length(hits)) {
    stop(sprintf("No encontré ninguno de:\n%s\nWD: %s",
                 paste0(" - ", paths, collapse = "\n"), getwd()), call. = FALSE)
  }
  picked <- hits[1]
  message("Leyendo: ", normalizePath(picked, mustWork = FALSE))
  if (grepl("\\.qs$", picked)) {
    if (!requireNamespace("qs", quietly = TRUE))
      stop("El archivo es .qs pero falta {qs}. install.packages('qs')", call. = FALSE)
    return(qs::qread(picked))
  }
  if (grepl("\\.(rds|RDS)$", picked)) {
    return(readRDS(picked))
  }
  stop("Tipo de archivo no reconocido: ", picked, call. = FALSE)
}

## Helper simple para encontrar archivos (como provincias.geojson)
find_data_file <- function(name, dirs = c("data", "../data", "..")) {
  paths <- unique(unlist(lapply(dirs, function(d) file.path(d, name)), use.names = FALSE))
  hits  <- paths[file.exists(paths)]
  if (!length(hits)) {
    return(NULL)
  }
  return(normalizePath(hits[1], mustWork = FALSE))
}

# ==== 3. Definir Variables de UI ====
var_choices <- c(
  "Temperatura Media" = "temp_mean",
  "Humedad Media" = "hum_mean",
  "Presión Media" = "pnm_mean",
  "Precipitación Acumulada" = "precip_sum"
)

meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
month_choices <- setNames(1:12, meses_es)

season_names <- c("Verano", "Otoño", "Invierno", "Primavera")
season_choices <- setNames(1:4, season_names)

# ==== 3.5 FUNCIONES HELPER ====
auto_iframe <- function(file_name) {
  tags$iframe(
    src = paste0("/figs/", file_name, ".html"),
    style = "width: 100%; height: 100%; border: none; min-height: 450px;"
  )
}
auto_iframe_modal <- function(file_name) {
  tags$iframe(
    src = paste0("/figs/", file_name, ".html"),
    style = "width: 100%; height: 90vh; border: none;"
  )
}
auto_img <- function(file_name) {
  tags$img(
    src = paste0("/figs/", file_name, ".png"),
    style = "width: 100%; height: 100%; border: none; object-fit: contain; min-height: 450px;"
  )
}
auto_img_modal <- function(file_name) {
  tags$img(
    src = paste0("/figs/", file_name, ".png"),
    style = "width: 100%; height: 90vh; border: none; object-fit: contain;"
  )
}

## Temas
daylight_theme <- bs_theme(
  version = 5,
  bg = "#ECF6FF",
  fg = "#1B1F3B",
  primary = "#79BBEA",
  secondary = "#FAD36F",
  base_font = font_google("Inter"),
  "card-bg" = "#FFFFFF",
  "card-border-color" = "#BADDF6"
)

night_sky_theme <- bs_theme(
  version = 5,
  bg = "#1B1F3B",
  fg = "#ECF6FF",
  primary = "#5C5EA3",
  secondary = "#3B3D72",
  base_font = font_google("Inter"),
  "card-bg" = "#2B2D54",
  "card-border-color" = "#3B3D72"
)

## Cargar GeoJSON
json_path <- find_data_file("provincias.geojson")
provincias_sf <- tryCatch({
  if (is.null(json_path)) stop("No se pudo encontrar 'provincias.geojson' en data/ o ../data/.")
  message("Leyendo GeoJSON desde: ", json_path)
  sf_data <- sf::st_read(json_path, quiet = TRUE)
  sf_data$provincia_join_key <- toupper(iconv(sf_data$nombre, from = "UTF-8", to = "ASCII//TRANSLIT"))
  sf_data$provincia_join_key <- dplyr::case_when(
    sf_data$provincia_join_key == "CIUDAD AUTONOMA DE BUENOS AIRES" ~ "CABA",
    sf_data$provincia_join_key == "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR" ~ "TIERRA DEL FUEGO",
    TRUE ~ sf_data$provincia_join_key
  )
  sf_data
}, error = function(e) {
  message("Error al LEER GeoJSON local: ", e$message)
  NULL
})

# ==== 4. User Interface (UI) ====
ui <- page_navbar(
  title = "Análisis Meteorológico Exploratorio",
  theme = daylight_theme,
  tags$head(
    tags$style(HTML("
      .static-chart-container {
        display: flex;
        flex-direction: column;
        height: 550px;
        padding: 1rem;
        margin-bottom: 1rem;
        border: 1px solid var(--bs-card-border-color);
        background-color: var(--bs-card-bg);
        border-radius: 0.375rem;
      }
      .story-text {
        font-size: 1.1rem;
        font-style: italic;
        color: var(--bs-secondary-color);
        text-align: center;
        margin-top: 1.5rem;
        margin-bottom: 1.5rem;
        max-width: 80%;
        margin-left: auto;
        margin-right: auto;
      }
      .static-chart-container h4 {
        flex-shrink: 0;
        margin-bottom: 0.25rem;
      }
      .static-chart-container p.text-muted {
        flex-shrink: 0;
        margin-bottom: 0.5rem;
      }
      .static-chart-container iframe,
      .static-chart-container img {
        flex-grow: 1;
        min-height: 400px;
      }
    "))
  ),
  
  nav_spacer(),
  nav_item(
    shinyWidgets::switchInput(
      inputId = "theme_toggle",
      offLabel = "Día",
      onLabel = "Noche",
      offStatus = "primary",
      onStatus = "primary",
      value = FALSE,
      inline = TRUE,
      size = "small"
    )
  ),
  
  # --- Pestaña 1: Introducción ---
  nav_panel(
    title = "Introducción",
    h2("Introducción y Análisis General"),
    p("Esta sección explica el dataset y muestra los patrones visuales exploratorios iniciales, basados en los Bloques 1-4 del EDA."),
    
    p(class="story-text", "Un mapa interactivo nos permite explorar la ubicación de las estaciones, coloreadas por provincia."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Mapa de Estaciones", actionLink("expand_m_intro", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Ubicación de las estaciones meteorológicas en el análisis."),
                                        auto_iframe("m_intro")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Un gráfico de barras cuantifica esto, mostrando el número de estaciones por provincia."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Estaciones por Provincia", actionLink("expand_p_bars", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Conteo de estaciones activas por jurisdicción."),
                                        auto_iframe("p_bars")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Finalmente, una serie temporal muestra el número de estaciones activas (que reportan datos) por año. Se observa una notable caída en los años más recientes."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Estaciones Activas por Año", actionLink("expand_p_timeline_zoom", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Caída reciente en el conteo de estaciones activas."),
                                        auto_iframe("p_timeline_zoom")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Luego, generamos histogramas para visualizar la distribución de las principales variables diarias: Temperatura, Humedad y Presión."),
    tags$div(class = "container-fluid",
             tags$div(class = "row",
                      tags$div(class = "col-md-4",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Dist. Temperatura", actionLink("expand_dist_temp", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        auto_iframe("dist_temp")
                               )
                      ),
                      tags$div(class = "col-md-4",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Dist. Humedad", actionLink("expand_dist_hum", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        auto_iframe("dist_hum")
                               )
                      ),
                      tags$div(class = "col-md-4",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Dist. Presión", actionLink("expand_dist_pnm", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        auto_iframe("dist_pnm")
                               )
                      )
             )
    ),
    
    p(class="story-text", "¿Cómo interactúan estas variables? Este gráfico muestra el ciclo estacional promedio de temperatura."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Ciclo Estacional Promedio", actionLink("expand_p_month", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Meses fríos (azul) y cálidos (rojo)."),
                                        auto_iframe("p_month")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Usamos un gráfico de hexágonos para visualizar la densidad de la relación entre temperatura y humedad."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Densidad Temp vs Humedad (Hex)", actionLink("expand_p_sc_hex", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Correlación negativa: a más calor, menos humedad relativa."),
                                        auto_iframe("p_sc_hex")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Un gráfico de contorno 2D confirma la relación inversa entre temperatura y humedad."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Relación Temperatura vs Humedad", actionLink("expand_p_th_contour", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Densidad 2D: A más temperatura, tiende a bajar la humedad relativa."),
                                        auto_iframe("p_th_contour")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Este histograma 2D muestra que las temperaturas más altas tienden a coincidir con presiones ligeramente menores."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Relación Temperatura vs Presión", actionLink("expand_p_tp", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Densidad 2D de 300k observaciones."),
                                        auto_iframe("p_tp")
                               )
                      )
             )
    ),
    
    
    p(class="story-text", "Mapa interactivo multicapa: Temp. Media y Amplitud, por Estación/Provincia."),
    tags$div(class = "container-fluid",
             tags$div(class = "row",
                      tags$div(class = "col-12",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Mapa: Temp. Media vs Amplitud Térmica", actionLink("expand_m_map", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Mapa interactivo de estaciones (puedes cambiar de capa)."),
                                        auto_iframe("m_map")
                               )
                      )
             )
    )
  ),
  
  # --- Pestaña 2: Análisis Profundos ---
  nav_panel(
    title = "Análisis Profundos",
    h2("Análisis Profundo de Variables"),
    p("Análisis detallado de series temporales, precipitación, viento y demanda energética."),
    
    p(class="story-text", "Histograma de temperaturas horarias (muestra)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Distribución de Temperaturas", actionLink("expand_p_hist", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Muestra de 200k registros; líneas punteadas = media y mediana"),
                                        auto_iframe("p_hist")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Serie nacional 'justa' con tendencia lineal (2018-2024)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Tendencia Lineal (2018-2024)", actionLink("expand_p_trend", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Análisis de la pendiente de temperatura en el período."),
                                        auto_iframe("p_trend")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Anomalías mensuales vs. climatología 2018–2023."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Anomalías Mensuales", actionLink("expand_p_anom", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "2023 cálido; evento frío en 2024."),
                                        auto_iframe("p_anom")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Ciclo diurno verano vs invierno."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Ciclo Diurno (Verano vs Invierno)", actionLink("expand_p_diurnal", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Promedio horario nacional para DJF vs JJA."),
                                        auto_iframe("p_diurnal")
                               )
                      )
             )
    ),
    
    hr(),
    p(class="story-text", "Lluvia fuerte (≥ 50 mm) 1991-2024."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Contribución Lluvia Fuerte (1991-2024)", actionLink("expand_p_rain_full", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Visión histórica de medianas mensuales."),
                                        auto_iframe("p_rain_full")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Mapa de eventos extremos (≥ 50 mm/día)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row",
                      tags$div(class = "col-12",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Mapa: Eventos de Precipitación Extrema", actionLink("expand_m_ext", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Mayor concentración en NEA/Litoral."),
                                        auto_iframe("m_ext")
                               )
                      )
             )
    ),
    
    hr(),
    p(class="story-text", "Grados-Día (HDD/CDD)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Demanda Energética Potencial (Grados-Día)", actionLink("expand_p_dd", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Mediana de HDD/CDD."),
                                        auto_iframe("p_dd")
                               )
                      )
             )
    ),
    
    hr(),
    p(class="story-text", "Rosa de Vientos (estación principal)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Rosa de Vientos (Estación Principal)", actionLink("expand_wind_plot", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Frecuencia e intensidad del viento por dirección."),
                                        auto_img("wind_plot")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Mapa interactivo de Rosas de Viento."),
    tags$div(class = "container-fluid",
             tags$div(class = "row",
                      tags$div(class = "col-12",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Mapa: Rosas de Viento Detalladas", actionLink("expand_m", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Clustering; zoom para ver rosas individuales."),
                                        auto_iframe("m")
                               )
                      )
             )
    ),
    
    hr(),
    p(class="story-text", "Gráfico de cinta (P50 y P10-P90)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Dispersión de Temperatura Nacional", actionLink("expand_p_ribbon", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Mediana (P50) y rango 80% (P10-P90)."),
                                        auto_iframe("p_ribbon")
                               )
                      )
             )
    )
  ),
  
  # --- Pestaña 3: Modelado (Prophet) ---
  nav_panel(
    title = "Modelado (Prophet)",
    h2("Modelado y Pronóstico con Prophet"),
    p("Resultados de ajustar Prophet a precipitación (1991-2024) y temperatura diaria."),
    
    p(class="story-text", "Pronóstico de precipitación mensual."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Pronóstico de Precipitación (Prophet)", actionLink("expand_plot_forecast", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Predicción (azul) vs. datos reales (negro)."),
                                        auto_iframe("plot_forecast")
                               )
                      )
             )
    ),
    
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Componente Precipitación: Tendencia", actionLink("expand_plot_forecast_comp1", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Análisis de la tendencia a largo plazo."),
                                        auto_img("plot_forecast_comp1")
                               )
                      )
             )
    ),
    
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Componente Precipitación: Estacionalidad", actionLink("expand_plot_forecast_comp2", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Ciclo anual de la precipitación."),
                                        auto_img("plot_forecast_comp2")
                               )
                      )
             )
    ),
    
    hr(),
    p(class="story-text", "Pronóstico de temperatura diaria."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Pronóstico de Temperatura Diaria (Prophet)", actionLink("expand_p_main", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Ajuste a datos de entrenamiento y prueba."),
                                        auto_iframe("p_main")
                               )
                      )
             )
    ),
    
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Componente Temperatura: Tendencia", actionLink("expand_p_comp1", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Tendencia a largo plazo."),
                                        auto_img("p_comp1")
                               )
                      )
             )
    ),
    
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Componente Temperatura: Estacionalidad", actionLink("expand_p_comp2", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Fuerte ciclo anual."),
                                        auto_img("p_comp2")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Comparación real vs predicción (test)."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Comparación: Real vs. Predicción (Test Set)", actionLink("expand_pr_comp", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "La predicción sigue bien la tendencia y la estacionalidad."),
                                        auto_iframe("pr_comp")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Residuos a lo largo del tiempo."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Residuos (Errores) a lo largo del tiempo", actionLink("expand_p_res", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "Idealmente ruido aleatorio centrado en 0."),
                                        auto_iframe("p_res")
                               )
                      )
             )
    ),
    
    p(class="story-text", "Distribución de errores de predicción."),
    tags$div(class = "container-fluid",
             tags$div(class = "row justify-content-center",
                      tags$div(class = "col-lg-10 col-xl-8",
                               tags$div(class = "static-chart-container",
                                        h4(tagList("Distribución de Errores de Predicción", actionLink("expand_hist_res", "Expandir ↗", style = "float: right; font-weight: bold;"))),
                                        p(class = "text-muted", "La mayoría de los errores se centran en 0."),
                                        auto_iframe("hist_res")
                               )
                      )
             )
    )
  ),
  
  # --- Pestaña 4: Gráficos Interactivos ---
  nav_panel(
    title = "Gráficos Interactivos",
    layout_sidebar(
      sidebar = sidebar(
        title = "Controles Interactivos",
        p("Usa estos controles para filtrar los datos."),
        selectInput("station_select", "Seleccionar Estación:",
                    choices = c("Cargando..." = ""),
                    selectize = TRUE),
        selectInput("variable_select", "Seleccionar Variable (Eje Izq.):",
                    choices = var_choices,
                    selected = "temp_mean"),
        selectInput("year_select", "Seleccionar Año:",
                    choices = c("Cargando..." = ""),
                    selected = 2024),
        selectInput("month_select", "Seleccionar Mes:",
                    choices = month_choices,
                    selected = 1),
        hr(),
        checkboxInput("compare_toggle", "Superponer períodos/variables?", FALSE),
        conditionalPanel(
          condition = "input.compare_toggle == true",
          p(class="text-muted", "Superponer otros períodos de tiempo:"),
          selectizeInput("compare_years", "Años a comparar:", choices = NULL, multiple = TRUE),
          selectizeInput("compare_months", "Meses a comparar:", choices = month_choices, multiple = TRUE),
          hr(),
          p(class="text-muted", "Superponer variable secundaria (eje derecho):"),
          selectInput("variable_select_secondary", "Variable (Eje Der.):",
                      choices = c("Ninguna" = "none", var_choices),
                      selected = "none")
        ),
        hr(),
        downloadButton("download_data_csv", "Descargar Datos (CSV)")
      ),
      h2("Constructor de Gráficos"),
      p("El gráfico se actualizará basado en los controles del panel lateral."),
      card(
        card_header("Visualización Interactiva"),
        plotlyOutput("interactivePlot", height = "600px")
      )
    )
  ),
  
  # --- Pestaña 5: Agricultura ---
  nav_panel(
    title = "Agricultura",
    layout_sidebar(
      sidebar = sidebar(
        title = "Índice de Idoneidad Climática",
        p(class="text-muted", "Calcula un índice de 0 a 100 basado en el clima estacional."),
        sliderInput(
          "season_select",
          "Estación: Verano",    # o NULL si preferís setearlo sólo desde el server
          min = 1, max = 4, value = 1, step = 1,
          animate = animationOptions(interval = 1200, loop = FALSE)
        ),
        selectInput("crop_preset", "Preajuste de Cultivo:",
                    choices = c("Soja (Cálido/Húmedo)" = "soja",
                                "Trigo (Fresco/Seco)" = "trigo",
                                "Maíz (Cálido/Mucha Lluvia)" = "maiz")),
        hr(),
        h5("Climatología Utilizada"),
        p(class="text-muted", "El mapa muestra el promedio de las variables de la estación seleccionada.")
      ),
      h2("Mapa de Idoneidad Climática"),
      p("El mapa muestra qué provincias tienen el clima más favorable (cercano a 100) para el cultivo y la estación seleccionada. El color de las estaciones individuales muestra la cobertura de datos."),
      accordion(
        open = FALSE,
        accordion_panel(
          "¿Cómo se calcula el Índice de Idoneidad Climática?",
          tags$ul(
            tags$li("1. Se calcula la climatología de la estación seleccionada (promedio de Temperatura, Humedad y Precipitación acumulada)."),
            tags$li("2. El 'Preajuste de Cultivo' define el clima 'ideal'."),
            tags$li(HTML("3. Para cada variable, se calcula un puntaje (0-100) con una <b>curva de Gauss</b> alrededor del óptimo.")),
            tags$li("4. El índice final es un promedio ponderado de Temp., Precip. y Humedad."),
            tags$li("5. El color de la provincia es el promedio de los índices de sus estaciones.")
          )
        )
      ),
      card(
        card_header(
          tagList("Mapa de Idoneidad Climática",
                  actionLink("expand_agri_map", "Expandir ↗", style = "float: right; font-weight: bold;"))
        ),
        card_body(
          leafletOutput("agri_map", height = "100%"),
          style = "height: 800px; padding: 0;"
        )
      )
    )
  )
)

# ==== 5. Server Logic ====
server <- function(input, output, session) {
  
  # Tema día/noche
  observe({
    if (isTRUE(input$theme_toggle)) {
      session$setCurrentTheme(night_sky_theme)
    } else {
      session$setCurrentTheme(daylight_theme)
    }
  }) %>% bindEvent(input$theme_toggle, ignoreNULL = TRUE)
  
  # ----- Carga de Datos -----
  data_r <- reactiveValues()
  
  observe({
    data_r$daily_station <- data.table::copy(read_obj("daily_station"))
    data_r$estaciones <- read_obj("estaciones")
    data_r$anios <- read_obj("anios")
    
    data_r$daily_station[, season_id := fcase(
      month %in% c(12, 1, 2), 1,
      month %in% c(3, 4, 5), 2,
      month %in% c(6, 7, 8), 3,
      month %in% c(9, 10, 11), 4
    )]
    
    if (!inherits(data_r$daily_station$ymd, "Date")) {
      data_r$daily_station[, ymd := as.Date(ymd)]
    }
  })
  
  # ----- Actualizar Controles de UI -----
  observe({
    req(data_r$estaciones)
    updateSelectInput(session, "station_select",
                      choices = sort(data_r$estaciones))
  })
  
  observe({
    req(data_r$anios)
    anios_sorted <- sort(data_r$anios, decreasing = TRUE)
    updateSelectInput(session, "year_select",
                      choices = anios_sorted,
                      selected = anios_sorted[1])
    updateSelectizeInput(session, "compare_years",
                         choices = anios_sorted)
  })
  
  observeEvent(input$season_select, {
    lbl <- paste("Estación:", season_names[as.integer(input$season_select)])
    updateSliderInput(session, "season_select", label = lbl)
  }, ignoreInit = TRUE)
  
  # ----- Datos Reactivos para el Gráfico Interactivo -----
  data_to_plot <- reactive({
    req(data_r$daily_station, input$station_select, input$variable_select,
        input$year_select, input$month_select)
    
    d_main <- data_r$daily_station[
      nombre == input$station_select &
        year == as.integer(input$year_select) &
        month == as.integer(input$month_select)
    ]
    
    if (nrow(d_main) > 0) {
      d_main[, group := paste(input$year_select, meses_es[as.integer(input$month_select)], "(Principal)")]
      d_main[, x_axis_val := mday(ymd)]
    }
    
    if (input$compare_toggle &&
        (!is.null(input$compare_years) || !is.null(input$compare_months)) ) {
      
      compare_years_list <- if (is.null(input$compare_years)) as.integer(input$year_select) else as.integer(input$compare_years)
      compare_months_list <- if (is.null(input$compare_months)) as.integer(input$month_select) else as.integer(input$compare_months)
      
      d_compare <- data_r$daily_station[
        nombre == input$station_select &
          year %in% compare_years_list &
          month %in% compare_months_list &
          !(year == as.integer(input$year_select) & month == as.integer(input$month_select))
      ]
      
      if (nrow(d_compare) > 0) {
        d_compare[, group := paste(year, meses_es[month])]
        d_compare[, x_axis_val := mday(ymd)]
      }
      
      d_plot <- rbind(d_main, d_compare, fill = TRUE)
    } else {
      d_plot <- d_main
    }
    
    if (nrow(d_plot) > 0) d_plot <- d_plot[order(group, x_axis_val)]
    d_plot
  })
  
  # ----- Renderizar el Gráfico Interactivo -----
  output$interactivePlot <- renderPlotly({
    d <- data_to_plot()
    pri_var_col  <- input$variable_select
    pri_var_name <- names(var_choices)[var_choices == pri_var_col]
    
    sec_var_col  <- req(input$variable_select_secondary)
    sec_var_name <- names(var_choices)[var_choices == sec_var_col]
    
    if (nrow(d) == 0) {
      return(plotly_empty() %>%
               layout(title = "No hay datos para la selección actual."))
    }
    
    hover_base <- paste(
      "<b>Fecha</b>: %{customdata|%Y-%m-%d}<br>",
      "<b>Día del mes</b>: %{x}<br>",
      "<b>Grupo</b>: %{data.name}<extra></extra>"
    )
    
    p <- plot_ly(
      data = d,
      x = ~x_axis_val,
      y = as.formula(paste0("~", pri_var_col)),
      color = ~group,
      type = 'scatter',
      mode = 'lines+markers',
      customdata = ~ymd,
      hovertemplate = paste(
        "<b>", pri_var_name, "</b>: %{y:.1f}<br>",
        hover_base
      )
    )
    
    plot_layout <- list(
      title = paste("Evolución para", input$station_select),
      xaxis = list(title = "Día del Mes"),
      yaxis = list(title = pri_var_name),
      legend = list(title = list(text = '<b>Comparativa</b>'))
    )
    
    if (input$compare_toggle && sec_var_col != "none") {
      p <- p %>% add_trace(
        y = as.formula(paste0("~", sec_var_col)),
        name = sec_var_name,
        yaxis = "y2",
        mode = "lines+markers",
        line = list(dash = 'dash'),
        color = ~group,
        showlegend = FALSE,
        customdata = ~ymd,
        hovertemplate = paste(
          "<b>", sec_var_name, "</b>: %{y:.1f}<br>",
          hover_base
        )
      )
      
      plot_layout$yaxis2 <- list(
        overlaying = "y",
        side = "right",
        title = sec_var_name,
        showgrid = FALSE
      )
    }
    
    p %>% layout(
      title = plot_layout$title,
      xaxis = plot_layout$xaxis,
      yaxis = plot_layout$yaxis,
      yaxis2 = plot_layout$yaxis2,
      legend = plot_layout$legend
    )
  })
  
  # ----- Download Handler (CSV) -----
  output$download_data_csv <- downloadHandler(
    filename = function() {
      paste0("datos_filtrados_", input$station_select, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(data_to_plot())
      fwrite(data_to_plot(), file)
    }
  )
  
  # ----- Modales helper -----
  create_modal_observer <- function(input_id, title, content_ui) {
    observeEvent(input[[input_id]], {
      showModal(modalDialog(
        title = title,
        content_ui,
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })
  }
  
  # --- Modales Pestaña 1
  create_modal_observer("expand_m_intro", "Mapa de Estaciones", auto_iframe_modal("m_intro"))
  create_modal_observer("expand_p_bars", "Estaciones por Provincia", auto_iframe_modal("p_bars"))
  create_modal_observer("expand_p_timeline_zoom", "Estaciones Activas por Año", auto_iframe_modal("p_timeline_zoom"))
  create_modal_observer("expand_dist_temp", "Distribución de Temperatura", auto_iframe_modal("dist_temp"))
  create_modal_observer("expand_dist_hum", "Distribución de Humedad", auto_iframe_modal("dist_hum"))
  create_modal_observer("expand_dist_pnm", "Distribución de Presión", auto_iframe_modal("dist_pnm"))
  create_modal_observer("expand_p_month", "Ciclo Estacional Promedio", auto_iframe_modal("p_month"))
  create_modal_observer("expand_p_sc_hex", "Densidad Temp vs Humedad (Hex)", auto_iframe_modal("p_sc_hex"))
  create_modal_observer("expand_p_th_contour", "Relación Temperatura vs Humedad", auto_iframe_modal("p_th_contour"))
  create_modal_observer("expand_p_tp", "Relación Temperatura vs Presión", auto_iframe_modal("p_tp"))
  create_modal_observer("expand_m_choro", "Temperatura Media por Provincia", auto_iframe_modal("m_choro"))
  create_modal_observer("expand_m_map", "Mapa: Temp. Media vs Amplitud Térmica", auto_iframe_modal("m_map"))
  
  # --- Modales Pestaña 2
  create_modal_observer("expand_p_hist", "Distribución de Temperaturas", auto_iframe_modal("p_hist"))
  create_modal_observer("expand_p_trend", "Tendencia Lineal (2018-2024)", auto_iframe_modal("p_trend"))
  create_modal_observer("expand_p_anom", "Anomalías Mensuales", auto_iframe_modal("p_anom"))
  create_modal_observer("expand_p_diurnal", "Ciclo Diurno (Verano vs Invierno)", auto_iframe_modal("p_diurnal"))
  create_modal_observer("expand_p_rain_full", "Contribución Lluvia Fuerte (1991-2024)", auto_iframe_modal("p_rain_full"))
  create_modal_observer("expand_m_ext", "Mapa: Eventos de Precipitación Extrema", auto_iframe_modal("m_ext"))
  create_modal_observer("expand_p_dd", "Demanda Energética Potencial (Grados-Día)", auto_iframe_modal("p_dd"))
  create_modal_observer("expand_wind_plot", "Rosa de Vientos (Estación Principal)", auto_img_modal("wind_plot"))
  create_modal_observer("expand_m", "Mapa: Rosas de Viento Detalladas", auto_iframe_modal("m"))
  create_modal_observer("expand_p_ribbon", "Dispersión de Temperatura Nacional", auto_iframe_modal("p_ribbon"))
  
  # --- Modales Pestaña 3
  create_modal_observer("expand_plot_forecast", "Pronóstico de Precipitación (Prophet)", auto_iframe_modal("plot_forecast"))
  create_modal_observer("expand_plot_forecast_comp1", "Componente Precipitación: Tendencia", auto_img_modal("plot_forecast_comp1"))
  create_modal_observer("expand_plot_forecast_comp2", "Componente Precipitación: Estacionalidad", auto_img_modal("plot_forecast_comp2"))
  create_modal_observer("expand_p_main", "Pronóstico de Temperatura Diaria (Prophet)", auto_iframe_modal("p_main"))
  create_modal_observer("expand_p_comp1", "Componente Temperatura: Tendencia", auto_img_modal("p_comp1"))
  create_modal_observer("expand_p_comp2", "Componente Temperatura: Estacionalidad", auto_img_modal("p_comp2"))
  create_modal_observer("expand_pr_comp", "Comparación: Real vs. Predicción (Test Set)", auto_iframe_modal("pr_comp"))
  create_modal_observer("expand_p_res", "Residuos (Errores) a lo largo del tiempo", auto_iframe_modal("p_res"))
  create_modal_observer("expand_hist_res", "Distribución de Errores de Predicción", auto_iframe_modal("hist_res"))
  
  # ==== Server Lógica para Pestaña Agricultura ====
  
  # 1. Datos base: Promedios ESTACIONALES por estación.
  seasonal_station_summary <- reactive({
    req(data_r$daily_station, cancelOutput = TRUE)
    
    # 1. Agrupar por estación (nombre) y por season_id (1=Verano, 2=Otoño, etc.)
    # La asignación de season_id ya está en el observe de carga de datos
    
    seasonal_data <- data_r$daily_station[
      , .(
        # Promedio de la media diaria de la temperatura para la estación
        temp_base = mean(temp_mean, na.rm = TRUE),
        # Promedio de la media diaria de la humedad
        hum_base = mean(hum_mean, na.rm = TRUE),
        # SUMA de la precipitación diaria (para tener un acumulado de la estación)
        precip_base = sum(precip_sum, na.rm = TRUE)
      ), by = .(nro, nombre, provincia, latitud, longitud, season_id)
    ]
    
    # 2. Quitar estaciones sin datos completos y estandarizar nombre de provincia
    climatology <- seasonal_data[complete.cases(temp_base, hum_base, precip_base, latitud, longitud, provincia)]
    
    climatology[, provincia_join_key := toupper(iconv(provincia, from = "UTF-8", to = "ASCII//TRANSLIT"))]
    climatology[provincia_join_key == "CAPITAL FEDERAL", provincia_join_key := "CABA"]
    climatology[provincia_join_key == "TIERRA DEL FUEGO", provincia_join_key := "TIERRA DEL FUEGO"]
    
    return(climatology)
  })
  
  # 2. Funciones de Puntuación (Helper - Sin cambios)
  score_curve <- function(x, min, opt, max, floor = 5) {
    # Gauss asimétrica: sigma izq/der a partir del rango
    sig_left  <- (opt - min) / 2
    sig_right <- (max - opt) / 2
    # Evitar sigmas 0
    sig_left  <- ifelse(sig_left  <= 0, 1e-6, sig_left)
    sig_right <- ifelse(sig_right <= 0, 1e-6, sig_right)
    
    sigma <- ifelse(x < opt, sig_left, sig_right)
    score <- 100 * exp(-0.5 * ((x - opt) / sigma)^2)
    
    # PISO suave para que nunca quede en 0 “letal”
    pmax(floor, pmin(100, score))
  }
  
  # 3. Cálculo del Índice (Reactivo a la estación seleccionada)
  index_data_sf <- reactive({
    req(seasonal_station_summary(), provincias_sf, input$crop_preset, input$season_select, cancelOutput = TRUE)
    
    data <- copy(seasonal_station_summary())
    
    # 3.1. Filtrar por la estación seleccionada (1=Verano, 2=Otoño, 3=Invierno, 4=Primavera)
    current_season_id <- as.integer(input$season_select)
    data <- data[season_id == current_season_id]
    
    # 3.2. Definir preajustes y pesos (CORREGIDO LÓGICA DE SOJA Y MAÍZ)
    if (input$crop_preset == "soja") {
      # Soja (primavera–verano): templado–cálido, agua moderada-alta
      opt_t <- 23;  min_t <- 18; max_t <- 32;  w_t <- 0.50
      opt_p <- 380; min_p <- 180; max_p <- 650; w_p <- 0.35  # mm en la estación (≈3 meses)
      opt_h <- 70;  min_h <- 50;  max_h <- 88;  w_h <- 0.15
    } else if (input$crop_preset == "trigo") {
      # Trigo (otoño–invierno–primavera): fresco, sensible a agua en macollaje/llenado
      opt_t <- 14;  min_t <- 4;  max_t <- 22;  w_t <- 0.45
      opt_p <- 260; min_p <- 120; max_p <- 450; w_p <- 0.40  # más peso a precipitación
      opt_h <- 60;  min_h <- 40; max_h <- 85;  w_h <- 0.15
    } else { # maíz
      # Maíz (primavera–verano): cálido, demanda hídrica alta
      opt_t <- 27;  min_t <- 19; max_t <- 35;  w_t <- 0.50
      opt_p <- 450; min_p <- 240; max_p <- 750; w_p <- 0.35
      opt_h <- 70;  min_h <- 50;  max_h <- 90;  w_h <- 0.15
    }
    
    if (input$crop_preset == "soja") {
      # Soja: óptimo templado-cálido y P/H razonables para Pampa Húmeda–Semiárida
      opt_t = 21.5;  min_t = 16; max_t = 28; w_t = 0.5
      opt_p = 350; min_p = 150; max_p = 650; w_p = 0.35  
      opt_h = 70;  min_h = 45;  max_h = 90;  w_h = 0.15
    } else if (input$crop_preset == "trigo") {
      opt_t = 12;  min_t = 0;  max_t = 25;  w_t = 0.5   # Optimo de 17°C
      opt_p = 300; min_p = 100; max_p = 550; w_p = 0.3
      opt_h = 60;  min_h = 35; max_h = 85;  w_h = 0.2
    } else { # maíz
      opt_t = 26;  min_t = 17; max_t = 36;  w_t = 0.5
      opt_p = 420; min_p = 200; max_p = 700; w_p = 0.35
      opt_h = 70;  min_h = 50;  max_h = 92;  w_h = 0.15
    }
    
    # 3.3. Calcular scores por ESTACIÓN (Sin ajustes de simulación)
    data[, score_t := score_curve(temp_base, min_t, opt_t, max_t)]
    data[, score_p := score_curve(precip_base, min_p, opt_p, max_p)]
    data[, score_h := score_curve(hum_base, min_h, opt_h, max_h)]
    
    # 3.4. Calcular índice final por ESTACIÓN
    total_weight <- w_t + w_p + w_h
    data[, index := (score_t * (w_t / total_weight) + 
                       score_p * (w_p / total_weight) + 
                       score_h * (w_h / total_weight))]
    
    # 3.5. PROMEDIAR POR PROVINCIA
    prov_index_data <- data[, .(
      index = mean(index, na.rm = TRUE),
      score_t = mean(score_t, na.rm = TRUE),
      score_p = mean(score_p, na.rm = TRUE),
      score_h = mean(score_h, na.rm = TRUE),
      temp_base = mean(temp_base, na.rm = TRUE),
      n_estaciones = .N
    ), by = .(provincia_join_key)]
    
    # 3.6. Unir con el GeoJSON
    map_data_sf <- merge(provincias_sf, prov_index_data, by = "provincia_join_key", all.x = TRUE)
    
    return(list(
      prov_data = map_data_sf,  # Los polígonos
      est_data = data           # Los puntos de las estaciones
    ))
  })
  
  # 4) Mapa base
  output$agri_map <- renderLeaflet({
    req(provincias_sf, cancelOutput = TRUE)
    leaflet(provincias_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -64.0, lat = -38.4, zoom = 4)
  })
  
  # 5) Actualizar mapa según índice
  observe({
    req(index_data_sf(), cancelOutput = TRUE)
    map_data_list <- index_data_sf()
    data_sf_prov  <- map_data_list$prov_data
    data_sf_est   <- map_data_list$est_data
    
    # Asegurar columnas en PROVINCIAS
    ensure_col <- function(df, col, fill = NA_real_) {
      if (!(col %in% names(df))) df[[col]] <- fill
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      df
    }
    data_sf_prov <- ensure_col(data_sf_prov, "index")
    data_sf_prov <- ensure_col(data_sf_prov, "temp_base")
    data_sf_prov <- ensure_col(data_sf_prov, "score_t")
    data_sf_prov <- ensure_col(data_sf_prov, "score_p")
    data_sf_prov <- ensure_col(data_sf_prov, "score_h")
    if (!("n_estaciones" %in% names(data_sf_prov))) data_sf_prov$n_estaciones <- NA_integer_
    
    # Asegurar columnas en ESTACIONES
    if (nrow(data_sf_est) > 0) {
      if (!("index" %in% names(data_sf_est)))      data_sf_est[, index := NA_real_]
      if (!("temp_base" %in% names(data_sf_est)))  data_sf_est[, temp_base := NA_real_]
      if (!("precip_base" %in% names(data_sf_est)))data_sf_est[, precip_base := NA_real_]
      if (!("score_t" %in% names(data_sf_est)))    data_sf_est[, score_t := NA_real_]
      if (!("score_p" %in% names(data_sf_est)))    data_sf_est[, score_p := NA_real_]
      if (!("score_h" %in% names(data_sf_est)))    data_sf_est[, score_h := NA_real_]
    }
    
    pal_index <- colorNumeric("RdYlGn", domain = c(0, 100), reverse = FALSE, na.color = "#808080")
    
    valid_temp <- data_sf_est[!is.na(temp_base) & is.finite(temp_base), temp_base]
    temp_domain <- if (length(valid_temp)) range(valid_temp, na.rm = TRUE, finite = TRUE) else c(0, 40)
    pal_temp_est <- colorNumeric("RdBu", domain = temp_domain, reverse = TRUE, na.color = "#808080")
    
    labels_prov <- sprintf(
      "<strong>%s</strong><br/>Índice Promedio: <strong>%.1f</strong><br/>(%s estaciones)<hr>T media: %.1f&nbsp;°C<br/>Puntaje Temp: %.0f<br/>Puntaje Precip: %.0f<br/>Puntaje Hum: %.0f",
      data_sf_prov$nombre,
      data_sf_prov$index,
      ifelse(is.na(data_sf_prov$n_estaciones), "s/d", as.character(data_sf_prov$n_estaciones)),
      data_sf_prov$temp_base,
      data_sf_prov$score_t, data_sf_prov$score_p, data_sf_prov$score_h
    ) |> lapply(htmltools::HTML)
    
    labels_est <- sprintf(
      "<strong>%s</strong><br/>T media: <strong>%.1f&nbsp;°C</strong><br/>Precip: <strong>%.0f&nbsp;mm</strong><hr>Índice: <strong>%.1f</strong><br/>Puntaje Temp: %.0f<br/>Puntaje Precip: %.0f<br/>Puntaje Hum: %.0f",
      data_sf_est$nombre, data_sf_est$temp_base, data_sf_est$precip_base,
      data_sf_est$index, data_sf_est$score_t, data_sf_est$score_p, data_sf_est$score_h
    ) |> lapply(htmltools::HTML)
    
    idx_vals <- data_sf_prov$index
    idx_vals <- idx_vals[is.finite(idx_vals)]
    
    leafletProxy("agri_map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addPolygons(
        data = data_sf_prov,
        fillColor = ~pal_index(index),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.6,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = labels_prov,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
        group = "Índice Provincial"
      ) %>%
      addCircleMarkers(
        data = data_sf_est,
        lng = ~longitud, lat = ~latitud,
        radius = 5,
        fillColor = ~pal_index(index),  # << ahora por índice
        color = "#000000", weight = 1, fillOpacity = 0.9,
        popup = labels_est,
        group = "Estaciones Individuales (Índice)"
      ) %>%
      addLegend("bottomright",
                pal = pal_index,
                values = idx_vals,
                title = "Índice (0-100)",
                opacity = 1,
                labFormat = labelFormat(digits = 0),
                na.label = NULL) %>%
      addLayersControl(
        # Nada en baseGroups para que todo sea overlay tildable
        overlayGroups = c("Índice Provincial", "Estaciones Individuales (Índice)"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      )
  })
  
  # 6) Mapa modal expandido
  observeEvent(input$expand_agri_map, {
    req(index_data_sf(), cancelOutput = TRUE)
    map_data_list <- index_data_sf()
    data_sf_prov <- map_data_list$prov_data
    data_sf_est  <- map_data_list$est_data
    
    # === 1. Lógica de Aseguramiento de Columnas (Replicando ensure_col) ===
    
    # Helper local para garantizar la columna numérica en el SF (Provincias)
    ensure_col_sf <- function(df, col, fill = NA_real_) {
      if (!(col %in% names(df))) df[[col]] <- fill
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      df
    }
    
    data_sf_prov <- ensure_col_sf(data_sf_prov, "index")
    data_sf_prov <- ensure_col_sf(data_sf_prov, "temp_base")
    data_sf_prov <- ensure_col_sf(data_sf_prov, "score_t")
    data_sf_prov <- ensure_col_sf(data_sf_prov, "score_p")
    data_sf_prov <- ensure_col_sf(data_sf_prov, "score_h")
    if (!("n_estaciones" %in% names(data_sf_prov))) data_sf_prov$n_estaciones <- NA_integer_
    
    # Asegurar columnas en ESTACIONES (data.table)
    has_est_data <- nrow(data_sf_est) > 0
    if (has_est_data) {
      if (!("index" %in% names(data_sf_est)))      data_sf_est[, index := NA_real_]
      if (!("temp_base" %in% names(data_sf_est)))  data_sf_est[, temp_base := NA_real_]
      if (!("precip_base" %in% names(data_sf_est)))data_sf_est[, precip_base := NA_real_]
      if (!("score_t" %in% names(data_sf_est)))    data_sf_est[, score_t := NA_real_]
      if (!("score_p" %in% names(data_sf_est)))    data_sf_est[, score_p := NA_real_]
      if (!("score_h" %in% names(data_sf_est)))    data_sf_est[, score_h := NA_real_]
    }
    
    # === 2. Paletas y Dominios (Deben ser consistentes) ===
    pal_index <- colorNumeric("RdYlGn", domain = c(0, 100), reverse = FALSE, na.color = "#808080")
    
    valid_temp <- if (has_est_data) data_sf_est[!is.na(temp_base) & is.finite(temp_base), temp_base] else numeric(0)
    temp_domain <- if (length(valid_temp)) range(valid_temp, na.rm = TRUE, finite = TRUE) else c(0, 40)
    pal_temp_est <- colorNumeric("RdBu", domain = temp_domain, reverse = TRUE, na.color = "#808080")
    
    # === 3. Creación Segura de Etiquetas (Usando ifelse para NAs) ===
    
    # Etiquetas de Provincias (labels_prov) - Usamos la sintaxis $ para que coincida con sf
    labels_prov <- sprintf(
      "<strong>%s</strong><br/>Índice Promedio: <strong>%.1f</strong><br/>(%s estaciones)<hr>T media: %.1f&nbsp;°C<br/>Puntaje Temp: %.0f<br/>Puntaje Precip: %.0f<br/>Puntaje Hum: %.0f",
      data_sf_prov$nombre,
      data_sf_prov$index,
      ifelse(is.na(data_sf_prov$n_estaciones), "s/d", as.character(data_sf_prov$n_estaciones)),
      data_sf_prov$temp_base,
      data_sf_prov$score_t, data_sf_prov$score_p, data_sf_prov$score_h
    ) |> lapply(htmltools::HTML)
    
    # Etiquetas de Estaciones (labels_est) - Protegemos el código si no hay estaciones
    if (has_est_data) {
      labels_est <- sprintf(
        "<strong>%s</strong><br/>T media: <strong>%.1f&nbsp;°C</strong><br/>Precip: <strong>%.0f&nbsp;mm</strong><hr>Índice: <strong>%.1f</strong><br/>Puntaje Temp: %.0f<br/>Puntaje Precip: %.0f<br/>Puntaje Hum: %.0f",
        data_sf_est$nombre, data_sf_est$temp_base, data_sf_est$precip_base,
        data_sf_est$index, data_sf_est$score_t, data_sf_est$score_p, data_sf_est$score_h
      ) |> lapply(htmltools::HTML)
    } else {
      labels_est <- character(0)
    }
    
    # === 4. Renderizar el Mapa Modal ===
    output$agri_map_modal_output <- renderLeaflet({
      leaflet(provincias_sf) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -64.0, lat = -38.4, zoom = 4) %>%
        addPolygons(
          data = data_sf_prov,
          fillColor = ~pal_index(index), weight = 1, opacity = 1, color = "white",
          dashArray = "3", fillOpacity = 0.6,
          highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
          label = labels_prov,
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
          group = "Índice Provincial"
        ) %>%
        addCircleMarkers(
          data = data_sf_est,
          lng = ~longitud, lat = ~latitud, radius = 5,
          fillColor = ~pal_temp_est(temp_base), color = "#000000", weight = 1, fillOpacity = 0.9,
          popup = labels_est,
          group = "Estaciones Individuales (Temp Base)"
        ) %>%
        addLegend("bottomright", pal = pal_index, values = data_sf_prov$index, title = "Índice (0-100)", opacity = 1) %>%
        addLegend("bottomleft", pal = pal_temp_est, values = valid_temp, title = "Temp. Base (°C)", opacity = 1) %>%
        addLayersControl(
          # Nada en baseGroups para que todo sea overlay tildable
          overlayGroups = c("Índice Provincial", "Estaciones Individuales (Temp Base)"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topleft"
        )
    })
    
    showModal(modalDialog(
      title = "Mapa de Idoneidad Climática (Expandido)",
      leafletOutput("agri_map_modal_output", width = "100%", height = "90vh"),
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })
  
}

# ==== 6. Ejecutar la App ====
shinyApp(ui = ui, server = server)
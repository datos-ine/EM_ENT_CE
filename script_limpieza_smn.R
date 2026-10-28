## Exceso de mortalidad por enfermedades no transmisibles (ENT) y causas externas (CE) en
## adultos de Argentina durante la pandemia COVID-19
## Limpieza y procesamiento de datos
## Fuentes de datos:
## - Percentil 10 temperaturas: Herrera et al. (2022). Sistema de Alerta Temprana
#  por Temperaturas Extremas Frío (SAT-TE Frío)
## - Percentil 90 temperaturas:
### Autora: Tamara Ricardo
# Última modificación: 2026-10-28

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  mgcv,
  tabulapdf,
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Percentil 10 temperaturas (1961-2010) -----
smn_pct10_raw <- extract_areas(
  file = "EM_ENT_CE/extra/Nota_Tecnica_SMN_2022-125.pdf",
  pages = 14:15,
  col_names = FALSE
)

## Percentil 90 temperaturas (1961-2010) -----
smn_pct90_raw <- extract_areas(
  file = "EM_ENT_CE/extra/Nota_Tecnica_SMN_2021-111.pdf",
  pages = 16:17,
  col_names = FALSE
)

## Metadatos estaciones SMN -----
smn_meta_raw <- import("raw/estaciones smn PAIS.xlsx")

## Temperaturas diarias por provincia (2010-2022) ------
smn_temp_raw <- read_delim("raw/Exp. 211810.txt", na = "\\N")


# Limpiar datos ----------------------------------------------------------
## Metadatos estaciones -----
smn_meta <- smn_meta_raw |>
  # --- Quitar columnas innecesarias ---
  select(-2) |>

  # --- Renombrar columnas ---
  rename_with(
    ~ c("estacion_id", "estacion_nombre", "provincia", "lat", "lon", "alt_m")
  ) |>

  # --- Modificar niveles provincia ---
  mutate(
    provincia = str_replace_all(
      str_to_title(provincia),
      c(
        "Capital Federal" = "CABA",
        "Cordoba" = "Córdoba",
        "Entre Rios" = "Entre Ríos",
        "Neuquen" = "Neuquén",
        "Rio Negro" = "Río Negro",
        "Santiago Del Estero" = "Santiago del Estero",
        "Tierra Del Fuego" = "Tierra del Fuego",
        "Tucuman" = "Tucumán"
      )
    )
  ) |>

  # --- Crear región y jurisdicción DEIS ---
  mutate(
    region_deis = case_when(
      provincia %in%
        c(
          "Buenos Aires",
          "CABA",
          "Córdoba",
          "Entre Ríos",
          "Santa Fe"
        ) ~ "Centro",
      provincia %in% c("Mendoza", "La Rioja", "San Juan", "San Luis") ~ "Cuyo",
      provincia %in% c("Chaco", "Corrientes", "Formosa", "Misiones") ~ "NEA",
      provincia %in%
        c(
          "Catamarca",
          "Jujuy",
          "Salta",
          "Santiago del Estero",
          "Tucumán"
        ) ~ "NOA",
      .default = "Patagonia"
    ),

    jurisd_deis = case_when(
      region_deis %in%
        c("Centro", "NEA") |
        provincia %in% c("Mendoza", "Tucumán") ~ provincia,
      region_deis == "Cuyo" & provincia != "Mendoza" ~ "Cuyo2",
      provincia %in% c("Jujuy", "Salta") ~ "NOA1",
      provincia %in% c("Catamarca", "Santiago del Estero") ~ "NOA2",
      provincia %in% c("La Pampa", "Neuquén", "Río Negro") ~ "Patagonia Norte",
      .default = "Patagonia Sur"
    )
  )


## Percentil 90 temperaturas (1961-2010) -----
smn_pct90 <- smn_pct90_raw |>
  # --- Unir elementos de lista ---
  list_rbind() |>

  # --- Seleccionar columnas ---
  select(-X2, -X3) |>

  # --- Renombrar columnas ---
  rename_with(
    ~ c(
      "estacion_id",
      "lon",
      "lat",
      "tmin_p90",
      "tmax_p90",
      "tmax_p95",
      "tmax_p99"
    )
  ) |>

  # --- Eliminar NAs ---
  drop_na()


## Percentil 10 temperaturas (1961-2010) -----
smn_pct10 <- smn_pct10_raw |>
  # --- Unir elementos de lista ---
  list_rbind() |>

  # --- Seleccionar columnas ---
  select(-X2, -X3) |>

  # --- Renombrar columnas ---
  rename_with(
    ~ c(
      "estacion_id",
      "lon",
      "lat",
      "tmin_p10",
      "tmin_p5",
      "tmin_p1",
      "tmax_p10"
    )
  ) |>

  # --- Eliminar NAs ---
  drop_na()


## Temperaturas diarias -----
smn_temp <- smn_temp_raw |>
  # --- Renombrar columnas ---
  rename(
    estacion_id = 1,
    fecha = 2,
    tmax = 3,
    tmin = 4
  ) |>

  # --- Crear año y mes ---
  mutate(
    anio = year(fecha),
    mes = month(fecha)
  ) |>

  # --- Añadir percentil 90 ---
  left_join(smn_pct90) |>

  # --- Añadir percentil 10 ---
  left_join(smn_pct10) |>

  # --- Quitar NAs ---
  drop_na() |>

  # --- Añadir metadatos ---
  left_join(smn_meta |> select(-lat, -lon))

# # Crear dataset olas -----------------------------------------------------
# smn_join <- smn_temp_raw |>
#   # Renombrar columnas
#   rename_with(
#     ~ c(
#       "estacion_id",
#       "fecha",
#       "tmax_c",
#       "tmin_c"
#     )
#   ) |>

#   # Crear columnas para el año
#   mutate(
#     anio = year(fecha),
#     mes = month(fecha),
#     # dia = yday(fecha),
#     .after = fecha
#   ) |>

#   drop_na() |>

#   # Añadir metadatos estaciones
#   left_join(smn_meta) |>

#   # Añadir percentiles históricos
#   left_join(
#     smn_pct |>
#       select(estacion_id, contains("tm"))
#   ) |>

#   # Imputar percentiles faltantes mediante predicción
#   mutate(
#     p90_tmax_pred = predict(m_p90_tmax, newdata = pick(lon, lat, alt_m)),
#     p90_tmin_pred = predict(m_p90_tmin, newdata = pick(lon, lat, alt_m)),
#     p10_tmax_pred = predict(m_p10_tmax, newdata = pick(lon, lat, alt_m)),
#     p10_tmin_pred = predict(m_p10_tmin, newdata = pick(lon, lat, alt_m)),

#     p90_tmax = coalesce(p90_tmax, p90_tmax_pred),
#     p90_tmin = coalesce(p90_tmin, p90_tmin_pred),
#     p10_tmax = coalesce(p10_tmax, p10_tmax_pred),
#     p10_tmin = coalesce(p10_tmin, p10_tmin_pred),

#     # Asegurar tipo numérico en percentiles
#     across(.cols = c(p90_tmax, p90_tmin, p10_tmax, p10_tmin), .fns = as.numeric)
#   ) |>

#   # Clasificar la temperatura del día
#   mutate(
#     temp_cat = case_when(
#       !between(mes, 4, 9) &
#         tmax_c >= p90_tmax &
#         tmin_c >= p90_tmin ~ "Calor",

#       between(mes, 4, 9) &
#         tmax_c <= p10_tmax &
#         tmin_c <= p10_tmin ~ "Frío",

#       .default = "Normal"
#     )
#   ) |>

#   # Ordenar datos e identificar rachas
#   arrange(estacion_id, fecha) |>
#   mutate(
#     racha_id = consecutive_id(temp_cat),
#     .by = estacion_id
#   ) |>

#   # Definir métricas de la racha
#   mutate(
#     racha_dias = if_else(temp_cat != "Normal" & n() >= 3, n(), 0),

#     ola_tipo = case_when(
#       temp_cat == "Calor" & racha_dias > 0 ~ "Calor",
#       temp_cat == "Frío" & racha_dias > 0 ~ "Frío",
#       .default = NA
#     ),

#     ola_ini = !is.na(ola_tipo) & row_number() == 1,

#     # fecha_ini = min(fecha),

#     .by = c(estacion_id, racha_id)
#   ) |>

#   # Resumir por estación meteorológica
#   group_by(region_deis, jurisd_deis, estacion_id, anio, mes) |>
#   summarise(
#     oc_n = sum(ola_ini & ola_tipo == "Calor"),
#     of_n = sum(ola_ini & ola_tipo == "Frío"),
#     max_dias_oc = max(
#       c(0, racha_dias[any(ola_ini) & ola_tipo == "Calor"]),
#       na.rm = TRUE
#     ),
#     max_dias_of = max(
#       c(0, racha_dias[any(ola_ini) & ola_tipo == "Frío"]),
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) |>

#   # Agrupar a nivel jurisdicción
#   group_by(region_deis, jurisd_deis, anio, mes) |>
#   summarise(
#     # Estaciones meteorológicas por jurisdicción
#     n_estaciones = n_distinct(estacion_id),

#     # Olas de calor
#     oc_mean = sum(oc_n, na.rm = TRUE) / n_estaciones,
#     max_dias_oc = max(max_dias_oc, na.rm = TRUE),

#     # Olas de frío
#     of_mean = sum(of_n, na.rm = TRUE) / n_estaciones,
#     max_dias_of = max(max_dias_of, na.rm = TRUE),

#     .groups = "drop"
#   )

# # Guardar datos ----------------------------------------------------------
# ## Olas de frío y calor por jurisdicción
# export(smn_join, "clean/arg_olas_frio_calor.rds")

# ## Población estándar 2010
# export(arg_pob_est, "clean/arg_pob_est_2010.rds")

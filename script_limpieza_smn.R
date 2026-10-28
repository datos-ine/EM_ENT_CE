## Exceso de mortalidad por enfermedades no transmisibles (ENT) y causas externas (CE) en
## adultos de Argentina durante la pandemia COVID-19
## Limpieza y procesamiento de datos
## Fuentes de datos:
## - Percentil 10 temperaturas: Herrera N, Chesini F, Saucedo M, et al Sistema de Alerta
# Temprana por Temperaturas Extremas Frío (SAT-TE Frío) [Nota Técnica].
# Servicio Meteorológico Nacional; 2022. Nota Técnica SMN 2022-125.
## - Percentil 90 temperaturas: Herrera N, Chesini F, Saucedo MA, et al. Sistema de Alerta
#  Temprana por Temperaturas Extremas Calor (SAT-TE Calor): la evolución del
#  SAT-OCS [Nota Técnica]. Servicio Meteorológico Nacional; 2021.
# Nota Técnica SMN 2021-111.

### Autora: Tamara Ricardo
# Última modificación: 2026-10-28 12:08

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  mgcv,
  tabulapdf,
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Metadatos estaciones SMN -----
smn_meta_raw <- import("raw/estaciones smn PAIS.xlsx")

## Percentil 10 temperaturas (1961-2010) -----
smn_pct10_raw <- extract_areas(
  file = "extra/Nota_Tecnica_SMN_2022-125.pdf",
  pages = 14:15,
  col_names = FALSE
)

## Percentil 90 temperaturas (1961-2010) -----
smn_pct90_raw <- extract_areas(
  file = "extra/Nota_Tecnica_SMN_2021-111.pdf",
  pages = 16:17,
  col_names = FALSE
)

## Temperaturas diarias por provincia (2010-2022) ------
smn_temp_raw <- read_delim("raw/Exp. 211810.txt", na = "\\N")


# Limpiar datos ----------------------------------------------------------
## Metadatos estaciones -----
smn_meta <- smn_meta_raw |>
  # --- Quitar columnas innecesarias ---
  select(!`NRO CLIM`) |>

  # --- Renombrar columnas ---
  clean_names() |>
  rename(estacion_id = nro_int) |>

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
  select(X1, X6:X9) |>

  # --- Renombrar columnas ---
  rename_with(
    ~ c(
      "estacion_id",
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
  select(X1, X6:X9) |>

  # --- Renombrar columnas ---
  rename_with(
    ~ c(
      "estacion_id",
      "tmin_p10",
      "tmin_p05",
      "tmin_p01",
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

  # --- Añadir metadatos ----
  left_join(smn_meta)


# Crear dataset olas -----------------------------------------------------
smn_olas <- smn_temp |>
  # --- Añadir percentil 90 ---
  left_join(smn_pct90) |>

  # --- Añadir percentil 10 ---
  left_join(smn_pct10) |>

  # --- Quitar NAs ---
  drop_na() |>

  # --- Identificar días cálidos y fríos ---
  mutate(
    temp_cat = case_when(
      # Día cálido
      !between(mes, 4, 9) &
        tmax >= tmax_p90 &
        tmin >= tmin_p90 ~ "Calor",

      # Día frío
      between(mes, 4, 9) &
        tmax <= tmax_p10 &
        tmin <= tmin_p10 ~ "Frío",

      # Valor por defecto
      .default = "Normal"
    )
  ) |>

  # --- Ordenar datos ---
  arrange(estacion_id, fecha) |>

  # --- Identificar rachas ---
  mutate(
    racha_id = consecutive_id(temp_cat),
    .by = estacion_id
  ) |>

  # --- Métricas de la racha ---
  mutate(
    # Días de racha
    racha_n = if_else(temp_cat != "Normal" & n() >= 3, n(), 0),

    # Tipo de ola
    ola_tipo = if_else(racha_n > 0, temp_cat, NA),

    # Inicio
    ola_ini = !is.na(ola_tipo) & row_number() == 1,

    # Fecha de inicio
    ola_ini_fecha = min(fecha),

    .by = c(estacion_id, racha_id)
  )


# Agregar por jurisdicción -----------------------------------------------
smn_olas_jur <- smn_olas |>
  # --- Número de olas de frío/calor ---
  group_by(region_deis, jurisd_deis, estacion_id, anio, mes) |>
  summarise(
    ola_calor_n = sum(ola_ini & ola_tipo == "Calor"),

    ola_frio_n = sum(ola_ini & ola_tipo == "Frío"),

    ola_calor_max = max(
      c(0, racha_n[any(ola_ini) & ola_tipo == "Calor"]),
      na.rm = TRUE
    ),

    ola_frio_max = max(
      c(0, racha_n[any(ola_ini) & ola_tipo == "Frío"]),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>

  # ---- Datos x jurisdicción ----
  group_by(region_deis, jurisd_deis, anio, mes) |>
  summarise(
    # --- Total estaciones SMN ---
    estaciones = n_distinct(estacion_id),

    # --- Estaciones en ola calor ---
    ola_calor_estaciones = n_distinct(estacion_id[ola_calor_n > 0]),

    # --- Olas de calor ---
    ola_calor_mean = if_else(
      ola_calor_estaciones > 0,
      sum(ola_calor_n, na.rm = TRUE) / ola_calor_estaciones,
      0
    ),

    # --- Máximo días ola calor ---
    ola_calor_max = max(ola_calor_max, na.rm = TRUE),

    # --- Estaciones en ola frío ---
    ola_frio_estaciones = n_distinct(estacion_id[ola_frio_n > 0]),

    # --- Olas de frío ---
    ola_frio_mean = if_else(
      ola_frio_estaciones > 0,
      sum(ola_frio_n, na.rm = TRUE) / ola_frio_estaciones,
      0
    ),

    # --- Máximo días ola frío ---
    ola_frio_max = max(ola_frio_max, na.rm = TRUE),

    .groups = "drop"
  )


# Guardar datos ----------------------------------------------------------
export(smn_olas_jur, "clean/arg_olas_frio_calor_jurisd.rds")

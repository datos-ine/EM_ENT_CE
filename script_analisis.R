## Exceso de mortalidad por enfermedades no transmisibles (ENT) y causas externas (CE) en
## adultos de Argentina durante la pandemia COVID-19
## Autora: Tamara Ricardo
## Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
    easystats,
    glmmTMB,
    mgcv,
    gratia,
    gtsummary,
    scales,
    rio,
    janitor,
    tidyverse
)


# Cargar datos -----------------------------------------------------------
# ---- Datos baseline ----
datos_em_train <- import("clean/arg_datos_em.rds") |> 
    filter_out(anio > 2019)


# ---- Datos predicciones ----
datos_em_test <- import("clean/arg_datos_em.rds") |> 
    filter(anio > 2019)


# ---- Población mensual 2010-2022 ----
proy_pob <- import("clean/arg_proy_mensual_2010_2022.rds")


# ---- Población estándar 2010 ----
pob_est_2010 <- import("clean/arg_proy_2010_2023.rds") |> 
    filter(anio == "2010" & sexo == "Total") |> 
    count(grupo_edad, wt = proy, name = "proy")


# ENTs -------------------------------------------------------------------
datos_ent <- datos_em_train |>
    filter(grupo_causa == "ENT") |>
    group_by(anio_mes, anio, mes, region_deis, jurisd_deis, sexo, grupo_edad) |>
    summarise(
        n = sum(n),
        proy_pob = mean(proy_pob),
        oc_mean = mean(oc_mean),
        of_mean = mean(of_mean),
        max_dias_oc = mean(max_dias_oc),
        max_dias_of = mean(max_dias_of),
        .groups = "drop"
    )


## Explorar datos ------
# Sexo y jurisdicción
datos_ent |>
    count(anio_mes, jurisd_deis, sexo, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = sexo)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


# Edad y jurisdicción
datos_ent |>
    count(anio_mes, jurisd_deis, grupo_edad, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = grupo_edad)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


## Modelos GAM -----
###  Nro de olas (sin interacciones) ----
mod_ent <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, k = 4) +
        s(of_mean, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ent
)

### Nro de olas (interacción con edad) ----
mod_ent2 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, by = jurisd_deis, k = 4) +
        s(of_mean, by = jurisd_deis, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ent
)


###  Días en olas (sin interacciones) ----
mod_ent3 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, k = 4) +
        s(max_dias_of, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ent
)

### Nro de olas (interacción con edad) ----
mod_ent4 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, by = jurisd_deis, k = 4) +
        s(max_dias_of, by = jurisd_deis, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ent
)

compare_performance(
    mod_ent,
    mod_ent2,
    mod_ent3,
    mod_ent4,
    metrics = "AIC",
    rank = TRUE
)


# Diabetes mellitus ------------------------------------------------------
datos_dm <- datos_em_train |>
    filter(causa == "DM") |>
    droplevels()


## Explorar datos ------
# Sexo y jurisdicción
datos_dm |>
    count(anio_mes, jurisd_deis, sexo, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = sexo)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


# Edad y jurisdicción
datos_dm |>
    count(anio_mes, jurisd_deis, grupo_edad, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = grupo_edad)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


## Modelos GAM -----
###  Nro de olas (sin interacciones) ----
mod_dm <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, k = 4) +
        s(of_mean, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_dm
)

### Nro de olas (interacción con edad) ----
mod_dm2 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, by = grupo_edad, k = 4) +
        s(of_mean, by = grupo_edad, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_dm
)


###  Días en olas (sin interacciones) ----
mod_dm3 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, k = 4) +
        s(max_dias_of, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_dm
)

### Nro de olas (interacción con edad) ----
mod_dm4 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, by = grupo_edad, k = 4) +
        s(max_dias_of, by = grupo_edad, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_dm
)

compare_performance(
    mod_dm,
    mod_dm2,
    mod_dm3,
    mod_dm4,
    metrics = "AIC",
    rank = TRUE
)


gam.check(mod_dm2)

draw(mod_dm2, select = "s(oc_mean):grupo_edad", partial_match = TRUE) +
    theme_minimal()

summary(mod_dm2)


# ECV --------------------------------------------------------------------
datos_ecv <- datos_em_train |>
    filter(causa == "ECV") |>
    droplevels()


## Explorar datos ------
# Sexo y jurisdicción
datos_ecv |>
    count(anio_mes, jurisd_deis, sexo, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = sexo)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


# Edad y jurisdicción
datos_ecv |>
    count(anio_mes, jurisd_deis, grupo_edad, wt = n) |>
    ggplot(aes(x = anio_mes, y = n, color = grupo_edad)) +
    facet_wrap(~jurisd_deis, ncol = 4, scales = "free_y") +
    geom_point() +
    geom_smooth()


## Modelos GAM -----
###  Nro de olas (sin interacciones) ----
mod_ecv <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, k = 4) +
        s(of_mean, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ecv
)

### Nro de olas (interacción con edad) ----
mod_ecv2 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(oc_mean, by = grupo_edad, k = 4) +
        s(of_mean, by = grupo_edad, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ecv
)


###  Días en olas (sin interacciones) ----
mod_ecv3 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, k = 4) +
        s(max_dias_of, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ecv
)

### Nro de olas (interacción con edad) ----
mod_ecv4 <- gam(
    n ~ s(anio, k = 4) +
        s(mes, bs = "cc", k = 12) +
        sexo +
        grupo_edad +
        s(max_dias_oc, by = grupo_edad, k = 4) +
        s(max_dias_of, by = grupo_edad, k = 4) +
        s(jurisd_deis, bs = "re"),
    offset = log(proy_pob),
    knots = list(mes = c(0.5, 12.5)),
    family = nb(),
    data = datos_ecv
)

compare_performance(
    mod_ecv,
    mod_ecv2,
    mod_ecv3,
    mod_ecv4,
    metrics = "AIC",
    rank = TRUE
)

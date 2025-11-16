########################################################################
# Punto_3.R
# Código R para:
#  - estimación con SE agrupados por anuncio
#  - ajustes por comparaciones múltiples
#  - simulaciones de poder para efecto principal y para interacción
#
########################################################################

rm(list = ls())

#Cargamos los paquetes
require(pacman)

# Usamos p_load() para cargar de forma eficiente varios paquetes necesarios
p_load( 
  tidyverse, # Conjunto de paquetes para manipulaci?n y visualizaci?n de datos (dplyr, ggplot2, etc.)
  sandwich,  # errores estándar robustos, tests, etc 
  lmtest,         
  multiwaycov,
  clubSandwich,
  dplyr,          
  tidyr,  
  ggplot2,        
  purrr, 
  broom     # pasar resultados de modelos a data frames.
)


setwd("C:\Users\USUARIO\Documents\GitHub\T2_EU_SMSTCH_EJ3")

#########################################
# PARÁMETROS
#########################################

SEED <- 20251114
set.seed(SEED)

# Diseño: número de anuncios (N_ann), solicitantes por anuncio (k), número de tratamientos T

# Hay 500 anuncios (vacantes).
# En cada anuncio se mandan 3 aplicaciones (cada una con un “tratamiento”: control, eu, non_eu).
# La probabilidad de respuesta para control es 0.30.
# Los efectos verdaderos (diferencias de probabilidad) que se van a explorar van de 0.01 a 0.10.
# Para cada valor de efecto se corren 500 simulaciones.
# Se considera significativo si p-value < 0.05.



N_ann <- 500        # número de anuncios a muestrear
k <- 3               # solicitantes por anuncio 
T <- 3               # treatments (control + 2)
# Baseline response rate (control)
p0 <- 0.30           # tasa de respuesta favorable base en control
# Efecto verdadero (en diferencia de probabilidad) a explorar (vector)
deltas <- seq(0.01, 0.10, by = 0.01)
# Número de simulaciones
nsim <- 500
# Nivel de significancia
alpha <- 0.05

##################################################
# Estimación de efecto por origen del solicitante
##################################################


# Generar dataset simulado según parámetros
generate_data <- function(N_ann, k, p0, delta_treat) {
  # create anuncio-level ids and strata (4 ciudades x tipo de landlord)
  anuncio_id <- 1:N_ann # Crea un ID para cada anuncio
  # Asigna aleatoriamente ciudad y tipo de landlord para cada anuncio
  city <- sample(c("Stockholm","Gothenburg","Malmo","Other"), N_ann, replace = TRUE)
  landlord_type <- sample(
    c("native", "foreign", "agency"),
    N_ann,
    replace = TRUE,
    prob = c(0.5, 0.3, 0.2)
  )
  # df_ann es un data frame con una fila por anuncio.
  df_ann <- data.frame(anuncio_id, city, landlord_type, stringsAsFactors = FALSE)
  
  # Repite cada anuncio k veces para tener k solicitantes por anuncio.
  df <- df_ann %>%
    slice(rep(1:n(), each = k)) %>%
    group_by(anuncio_id) %>%
    mutate(applicant_idx = row_number()-1) %>% ungroup()
  
  # Asignar tratamientos al interior de cada anuncio
  # applicant_idx 0 -> control, 1 -> T_eu, 2 -> T_non_eu (funciona si k >= T)
  df <- df %>%
    mutate(treatment = case_when(
      applicant_idx == 0 ~ "control",
      applicant_idx == 1 ~ "eu",
      applicant_idx == 2 ~ "non_eu",
      TRUE ~ sample(c("control","eu","non_eu"), n(), replace = TRUE)
    ))
  
  # Asignar probabilidades 
  df <- df %>%
    mutate(p = case_when(
      treatment == "control" ~ p0,
      # eu tiene una probabilidad de respuesta más baja que el caso base
      treatment == "eu" ~ p0 - delta_treat[1],
      # non_eu tiene una probabilidad de respuesta aún más baja
      treatment == "non_eu" ~ p0 - delta_treat[2],
      TRUE ~ p0
    ))
  
  # Genera una respuesta binaria (0/1) para cada solicitante con probabilidad p.
  # Se genera un dataframe con una fila por solicitante
  df <- df %>% mutate(Y = rbinom(n(), 1, p))
  return(df)
}

# Una Simulación y un test
# función que ejecuta una simulación y devuelve si rechaza H0 (p-value < alpha) para efecto non_eu vs control
# Versión extendida: devuelve coeficiente, p-valor y CI para d_non
run_one_sim <- function(N_ann, k, p0, deltas, alpha = 0.05) {
  # deltas = c(delta_eu, delta_non_eu)
  df <- generate_data(N_ann, k, p0, deltas)
  
  df <- df %>%
    mutate(
      d_eu  = ifelse(treatment == "eu", 1, 0),
      d_non = ifelse(treatment == "non_eu", 1, 0),
      stratum = factor(paste(city, landlord_type, sep = "_"))
    )
  
  if (length(unique(df$anuncio_id)) < 2) {
    return(list(
      coef = NA_real_,
      se = NA_real_,
      pval = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    ))
  }
  
  model <- lm(Y ~ d_eu + d_non, data = df)
  
  cluster_se <- coef_test(model, cluster = df$anuncio_id, vcov = "CR2")
  df_se <- as.data.frame(cluster_se)
  
  idx <- which(rownames(df_se) == "d_non")
  if (length(idx) == 0) {
    return(list(
      coef = NA_real_,
      se = NA_real_,
      pval = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    ))
  }
  
  # columnas de estimate, se y p-value de forma robusta
  col_est <- grep("coef|estimate|beta", colnames(df_se), value = TRUE, ignore.case = TRUE)
  col_se  <- grep("se|std\\.error|std_error", colnames(df_se), value = TRUE, ignore.case = TRUE)
  col_p   <- grep("^p$|p_val|p.value|pval|Pr\\(|pr\\(", colnames(df_se), value = TRUE, ignore.case = TRUE)
  
  # fallback: si no encuentra, usar columnas numéricas
  if (length(col_est) == 0 || length(col_se) == 0 || length(col_p) == 0) {
    numeric_cols <- which(sapply(df_se, is.numeric))
    if (length(numeric_cols) >= 3) {
      col_est <- colnames(df_se)[numeric_cols[1]]
      col_se  <- colnames(df_se)[numeric_cols[2]]
      col_p   <- colnames(df_se)[numeric_cols[length(numeric_cols)]]
    } else {
      return(list(
        coef = NA_real_,
        se = NA_real_,
        pval = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_
      ))
    }
  }
  
  est <- as.numeric(df_se[idx, col_est][[1]])
  se  <- as.numeric(df_se[idx, col_se][[1]])
  pval <- as.numeric(df_se[idx, col_p][[1]])
  
  # CI al 1 - alpha
  z <- qnorm(1 - alpha / 2)
  ci_lower <- est - z * se
  ci_upper <- est + z * se
  
  return(list(
    coef = est,
    se = se,
    pval = pval,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}


# Simulación de prueba
test <- run_one_sim(N_ann, k, p0, c(0.01, 0.02))
str(test)


# Simulación de poder para el efecto principal
# Simulación con sesgo y cobertura para el efecto principal de non_eu
power_results <- map_df(deltas, function(d) {
  # eu effect = d/2, non_eu effect = d (definición del diseño)
  deltas_vec <- c(d / 2, d)
  
  res <- replicate(
    nsim,
    run_one_sim(N_ann, k, p0, deltas_vec, alpha = alpha),
    simplify = FALSE
  )
  
  coefs  <- sapply(res, `[[`, "coef")
  pvals  <- sapply(res, `[[`, "pval")
  lower  <- sapply(res, `[[`, "ci_lower")
  upper  <- sapply(res, `[[`, "ci_upper")
  
  power     <- mean(pvals < alpha, na.rm = TRUE)
  bias      <- mean(coefs, na.rm = TRUE) - d          # sesgo de d_non
  coverage  <- mean(lower <= d & upper >= d, na.rm = TRUE)
  
  tibble(
    MDE       = d,
    power     = power,
    mean_coef = mean(coefs, na.rm = TRUE),
    bias      = bias,
    coverage  = coverage
  )
})

print(power_results)
 
# Plot power curve
ggplot(power_results, aes(x = MDE, y = power)) +
  geom_line() + geom_point() + ylim(0,1) +
  labs(title = "Power vs MDE (effect on non_eu vs control)",
       x = "Absolute difference in probability (MDE)",
       y = "Power")

############################
# Cálculo de MDE aproximado
############################

# Grilla de tamaños de muestra
N_grid <- c(200, 300, 400, 500, 700, 1000)

delta_target <- 0.05
deltas_target <- c(delta_target / 2, delta_target)

power_vs_N <- map_df(N_grid, function(NN) {
  res <- replicate(
    nsim,
    run_one_sim(NN, k, p0, deltas_target),
    simplify = FALSE
  )
  
  pvals <- sapply(res, `[[`, "pval")
  power <- mean(pvals < alpha, na.rm = TRUE)
  
  tibble(
    N_ann = NN,
    power = power
  )
})

print(power_vs_N)

# Power en función de N (anuncios) 
# Non-eu sufre penalización de 5 p.p., y eu sufre penalización de 2.5 p.p.

ggplot(power_vs_N, aes(x = N_ann, y = power)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(
    title = "Power vs número de anuncios",
    subtitle = paste0("Delta_non_eu = ", delta_target, 
                      ", Delta_eu = ", delta_target / 2),
    x = "Número de anuncios (N_ann)",
    y = "Power"
  ) +
  theme_minimal(base_size = 14)


# Busca la primera fila donde el poder es al menos 0.80.
# Esto da una aproximación al Mínimo Efecto Detectable con 80% de poder y los parámetros especificados.

desired_power <- 0.80
mde_est <- power_results %>% filter(power >= desired_power) %>% slice(1)
if(nrow(mde_est)==0) message("Con los parámetros actuales no alcanzas 80% de poder en el rango de deltas probado") else print(mde_est)

#############################################
# Poder para detectar interacción (heterogeneidad)
#############################################

# Simulamos que la proporción de landlords 'native' afecta el efecto: delta_non_eu_native vs delta_non_eu_non_native

run_one_sim_interact <- function(N_ann, k, p0, deltas, debug = FALSE) {
  # deltas
  #   deltas[1] = efecto (diferencia en probabilidad) de non_eu cuando el landlord es nativo
  #   deltas[2] = efecto de non_eu cuando el landlord es extranjero
  #
  # Nota: se mantiene generate_data con deltas originales
  
  # generar datos base sin efectos (para no duplicar)
  df <- generate_data(N_ann, k, p0, delta_treat = c(0, 0))
  
  if (debug) message("clusters iniciales: ", length(unique(df$anuncio_id)))
  
  # dummies
  df <- df %>%
    mutate(
      d_eu  = ifelse(treatment == "eu", 1, 0),
      d_non = ifelse(treatment == "non_eu", 1, 0),
      landlord_foreign = ifelse(landlord_type == "foreign", 1, 0)
    )
  
  # Sobreescribir p para incorporar heterogeneidad por landlord_type en el tratamiento non_eu
  df <- df %>%
    mutate(
      p = case_when(
        treatment == "control" ~ p0,
        # Mantenemos el efecto para eu en el baseline para esta interacción (sin efecto)
        treatment == "eu" ~ p0, 
        # non_eu con landlord nativo: probabilidad más baja por deltas[1]
        treatment == "non_eu" & landlord_type %in% c("native") ~ p0 - deltas[1],
        # non_eu con landlord extranjero: probabilidad más baja por deltas[2]
        treatment == "non_eu" & landlord_type %in% c("foreign") ~ p0 - deltas[2],
        TRUE ~ p0
      ),
      # asegurar límites [0,1]
      p = pmin(pmax(p, 0), 1)
    )
  
  # volver a muestrear outcomes (aseguramos que Y refleje la heterogeneidad)
  df <- df %>% mutate(Y = rbinom(n(), 1, p))
  
  # comprobación clusters suficientes
  if (length(unique(df$anuncio_id)) < 2) {
    if (debug) message("solo 1 cluster -> NA")
    return(list(pval = NA_real_, coef = NA_real_))
  }
  
  # estimar modelo con interacción explícita
  # Fórmula: Y ~ d_eu + d_non + d_non:landlord_foreign
  # Usamos variable de interacción explícita para nombres estables
  df <- df %>% mutate(d_non_landlord = d_non * landlord_foreign)
  
  model <- tryCatch(
    lm(Y ~ d_eu + d_non + d_non_landlord, data = df),
    error = function(e) return(NULL)
  )
  if (is.null(model)) {
    if (debug) message("lm falló")
    return(list(pval = NA_real_, coef = NA_real_))
  }
  
  # cluster-robust SE (clubSandwich::coef_test). manejar errores
  cluster_se <- tryCatch(
    coef_test(model, cluster = df$anuncio_id, vcov = "CR2"),
    error = function(e) return(NULL)
  )
  if (is.null(cluster_se)) {
    if (debug) message("coef_test falló")
    return(list(pval = NA_real_, coef = NA_real_))
  }
  
  # convertir a data.frame y buscar fila del coeficiente de interacción
  df_se <- as.data.frame(cluster_se)
  # coeficiente de interés
  coef_name_candidates <- c("d_non_landlord", "d_non:landlord_foreign", "d_non:landlord", "d_non_landlord_foreign")
  # buscar coincidencias parciales
  idx <- NULL
  for (nm in coef_name_candidates) {
    idx <- which(rownames(df_se) == nm)
    if (length(idx) > 0) break
  }
  # si no coincide exactamente, buscar por patrón que contenga "d_non" y "landlord" en el rowname
  if (length(idx) == 0) {
    idx <- grep("d[_\\.]?non.*landlord|landlord.*d[_\\.]?non", rownames(df_se), ignore.case = TRUE)
  }
  if (length(idx) == 0) {
    if (debug) {
      message("No se encontró la fila del coeficiente de interacción en rownames(df_se).")
      message("Row names disponibles: ", paste(head(rownames(df_se), 10), collapse = ", "))
    }
    return(list(pval = NA_real_, coef = NA_real_))
  }
  # si hay más de una coincidencia, usar la primera
  idx <- idx[1]
  
  # identificar columnas de estimate y p-value de forma robusta
  col_est <- grep("coef|estimate|beta", colnames(df_se), value = TRUE, ignore.case = TRUE)
  col_p   <- grep("^p$|p_val|p.value|pval|Pr\\(|pr\\(", colnames(df_se), value = TRUE, ignore.case = TRUE)
  # si no las encuentra por patron, tomar columnas numéricas: asumimos primera numérica = estimate, última numérica = p-value
  if (length(col_est) == 0 || length(col_p) == 0) {
    numeric_cols <- which(sapply(df_se, is.numeric))
    if (length(numeric_cols) >= 2) {
      col_est <- colnames(df_se)[numeric_cols[1]]
      col_p   <- colnames(df_se)[numeric_cols[length(numeric_cols)]]
    } else {
      if (debug) message("No se pudieron identificar columnas numéricas en df_se.")
      return(list(pval = NA_real_, coef = NA_real_))
    }
  }
  
  # extraer numéricos robustamente
  coef_val <- suppressWarnings(as.numeric(df_se[idx, col_est][[1]]))
  pval_val  <- suppressWarnings(as.numeric(df_se[idx, col_p][[1]]))
  
  # verificar que sean numéricos válidos
  if (is.na(coef_val) && is.na(pval_val)) {
    if (debug) message("coef o pval son NA tras extracción.")
    return(list(pval = NA_real_, coef = NA_real_))
  }
  
  return(list(pval = pval_val, coef = coef_val))
}


one_test <- run_one_sim_interact(N_ann, k, p0, c(0.02, 0.05), debug = TRUE)
print(one_test)

# Grilla para deltas de interacción
# De modo que siempre: d_native > d_non_native

d_native_seq      <- seq(0.02, 0.12, by = 0.02)  # discriminación fuerte
d_non_native_seq  <- seq(0.00, 0.06, by = 0.02)  # discriminación más débil


# Reducimos el número de simulaciones
nsim_int <- 400

interaction_power_grid <- expand.grid(
  d_native      = d_native_seq,
  d_non_native  = d_non_native_seq
) %>%
  mutate(
    deltas_local = purrr::map2(d_native, d_non_native, ~ c(.x, .y)),
    res = purrr::map(deltas_local, ~ replicate(
      nsim_int,
      run_one_sim_interact(N_ann, k, p0, .x),
      simplify = FALSE
    )),
    pvals = purrr::map(res, ~ sapply(.x, function(z) z$pval)),
    power = purrr::map_dbl(pvals, ~ mean(.x < alpha, na.rm = TRUE))
  ) %>%
  select(-deltas_local, -res, -pvals)

head(interaction_power_grid, 10)
summary(interaction_power_grid$power)
unique(interaction_power_grid$power)

# Graficar power heatmap
ggplot(interaction_power_grid, 
       aes(x = d_native, y = d_non_native, fill = power)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Poder estadístico para detectar interacción",
    subtitle = "Delta (nativos) vs Delta (no nativos)",
    x = expression(delta[native]),
    y = expression(delta[non~native]),
    fill = "Poder"
  ) +
  theme_minimal(base_size = 14)


# Guardar resultados
write.csv(power_results, "power_curve_results.csv", row.names = FALSE)
write.csv(interaction_power_grid, "interaction_power_grid.csv", row.names = FALSE)




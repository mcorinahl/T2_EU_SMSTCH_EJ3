########################################################################
# Punto_3_DeclareDesign.R
# Código R para:
#  - simulaciones de poder (efecto principal y heterogeneidad)
#  - diseño explícito con DeclareDesign
#  - LPM con errores estándar agrupados por anuncio
########################################################################

rm(list = ls())

# Cargar paquetes
require(pacman)
p_load(
  DeclareDesign,   # núcleo de diseño (inquiries, estimators, diagnose_design)
  fabricatr,       # generación de datos anidados
  estimatr,        # lm_robust con clusters
  tidyverse        # manipulación y gráficos
)

setwd("C:/Users/USUARIO/Documents/GitHub/T2_EU_SMSTCH_EJ3")

#########################################
# PARÁMETROS GLOBALES
#########################################

SEED  <- 20251114
set.seed(SEED)

N_ann   <- 500      # anuncios (vacantes)
k       <- 3        # solicitantes por anuncio (control, eu, non_eu)
p0      <- 0.46     # tasa de respuesta base para control
alpha   <- 0.05
nsim    <- 500      # número de simulaciones para poder
d_grid  <- seq(0.01, 0.30, by = 0.02)  # grilla para efectos verdaderos

#########################################
# DISEÑO 1: EFECTO PRINCIPAL non_eu vs control
#########################################
# Interpreta:
#   - control  = nombre sueco (Erik)
#   - eu       = inmigrante europeo (penalización moderada)
#   - non_eu   = inmigrante no europeo / árabe (penalización fuerte)
# Potenciales efectos verdaderos: 
#   delta_eu  = d/2
#   delta_non = d
# Parámetro de interés: ATE_non = p(non_eu) - p(control) = -delta_non

make_design_main <- function(N_ann, k, p0, delta_eu, delta_non, alpha = 0.05) {
  
  # 1. Población: anuncios y solicitantes
  population_main <- declare_population(
    anuncios = add_level(
      N              = N_ann,
      anuncio_id     = 1:N,
      city           = sample(c("Stockholm", "Gothenburg", "Malmo", "Other"),
                              size = N, replace = TRUE),
      landlord_type  = sample(c("native", "foreign", "agency"),
                              size = N, replace = TRUE,
                              prob = c(0.5, 0.3, 0.2))
    ),
    applicants = add_level(
      N              = k,
      applicant_idx  = 0:(k - 1),
      treatment      = case_when(
        applicant_idx == 0 ~ "control",
        applicant_idx == 1 ~ "eu",
        TRUE               ~ "non_eu"
      )
    )
  )
  
  # 2. Modelo de resultado: fija probabilidad verdadera y genera Y
  outcome_model_main <- declare_step(
    handler = function(data) {
      data %>%
        mutate(
          p = case_when(
            treatment == "control" ~ p0,
            treatment == "eu"      ~ p0 - delta_eu,
            treatment == "non_eu"  ~ p0 - delta_non
          ),
          p = pmin(pmax(p, 0), 1),
          Y = rbinom(n(), 1, p),
          d_eu  = as.numeric(treatment == "eu"),
          d_non = as.numeric(treatment == "non_eu")
        )
    },
    label = "outcome_model_main"
  )
  
  # 3. Inquiry (parámetro de interés): ATE non_eu vs control
  inquiry_main <- declare_inquiry(
    ATE_non = -delta_non,
    label   = "ATE_non"
  )
  
  # 4. Estimador: LPM con clusters en anuncio_id, coeficiente de d_non
  estimator_main <- declare_estimator(
    Y ~ d_eu + d_non,
    model      = lm_robust,
    clusters   = anuncio_id,
    term       = "d_non",
    inquiry    = "ATE_non",
    label      = "LPM_non_main"
  )
  
  # 5. poder, sesgo, cobertura, etc.
  diagnosands_main <- declare_diagnosands(
    mean_est   = mean(estimate),
    se_mean    = mean(std.error),
    bias       = mean(estimate - estimand),
    power      = mean(p.value < alpha),
    coverage   = mean(conf.low <= estimand & conf.high >= estimand)
  )
  
  design_main <- population_main +
    outcome_model_main +
    inquiry_main +
    estimator_main
  
  list(
    design      = design_main,
    diagnosands = diagnosands_main
  )
}

#########################################
# SIMULACIÓN DE PODER: EFECTO PRINCIPAL
#########################################

power_results_main <- map_dfr(d_grid, function(d) {
  delta_eu  <- d / 2
  delta_non <- d
  
  dm <- make_design_main(
    N_ann     = N_ann,
    k         = k,
    p0        = p0,
    delta_eu  = delta_eu,
    delta_non = delta_non,
    alpha     = alpha
  )
  
  diag <- diagnose_design(
    dm$design,
    sims          = nsim,
    diagnosands   = dm$diagnosands,
    bootstrap_sims = 0
  )
  
  # Filtramos explícitamente por inquiry y estimator para obtener un escalar
  dd <- diag$diagnosands_df %>%
    filter(inquiry == "ATE_non",
           estimator == "LPM_non_main")
  
  tibble(
    MDE          = d,
    delta_eu     = delta_eu,
    delta_non    = delta_non,
    power        = dd$power[1],
    mean_est     = dd$mean_est[1],
    bias         = dd$bias[1],
    coverage     = dd$coverage[1]
  )
})

print(power_results_main)

# Curva de poder vs MDE (efecto en non_eu)
ggplot(power_results_main, aes(x = MDE, y = power)) +
  geom_line() + 
  geom_point() +
  ylim(0, 1) +
  labs(
    title = "Power vs MDE (efecto non_eu vs control)",
    x     = "Diferencia verdadera en probabilidad (MDE)",
    y     = "Power"
  ) +
  theme_minimal(base_size = 14)

#########################################
# POWER vs N: tamaño de muestra (anuncios)
#########################################

N_grid <- c(200, 300, 400, 500, 700, 1000)
delta_target      <- 0.10
delta_eu_target   <- delta_target / 2
delta_non_target  <- delta_target

power_vs_N <- map_dfr(N_grid, function(NN) {
  dm <- make_design_main(
    N_ann     = NN,
    k         = k,
    p0        = p0,
    delta_eu  = delta_eu_target,
    delta_non = delta_non_target,
    alpha     = alpha
  )
  
  diag <- diagnose_design(
    dm$design,
    sims          = nsim,
    diagnosands   = dm$diagnosands,
    bootstrap_sims = 0
  )
  
  dd <- diag$diagnosands_df %>%
    filter(inquiry == "ATE_non",
           estimator == "LPM_non_main")
  
  tibble(
    N_ann = NN,
    power = dd$power[1]
  )
})

print(power_vs_N)

ggplot(power_vs_N, aes(x = N_ann, y = power)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(
    title = "Power vs número de anuncios (efecto principal)",
    subtitle = paste0("Delta_non_eu = ", delta_non_target,
                      ", Delta_eu = ", delta_eu_target),
    x = "Número de anuncios (N_ann)",
    y = "Power"
  ) +
  theme_minimal(base_size = 14)

#########################################
# DISEÑO 2: HETEROGENEIDAD POR ORIGEN DEL LANDLORD
#########################################
# Ahora: el efecto non_eu depende de si el landlord es nativo o extranjero.
#   - Landlord nativo: p = p0 - delta_native   (discriminación fuerte)
#   - Landlord extranjero: p = p0 - delta_foreign   (discriminación más débil)
# El coeficiente de interacción d_non: landlord_foreign = delta_native - delta_foreign

make_design_interact <- function(N_ann, k, p0,
                                 delta_native, delta_foreign,
                                 alpha = 0.05) {
  
  population_int <- declare_population(
    anuncios = add_level(
      N               = N_ann,
      anuncio_id      = 1:N,
      landlord_native = rbinom(N, 1, 0.8),
      landlord_type   = ifelse(landlord_native == 1, "native", "foreign"),
      city            = sample(c("Stockholm", "Gothenburg", "Malmo", "Other"),
                               size = N, replace = TRUE)
    ),
    applicants = add_level(
      N              = k,
      applicant_idx  = 0:(k - 1),
      treatment      = case_when(
        applicant_idx == 0 ~ "control",
        applicant_idx == 1 ~ "eu",
        TRUE               ~ "non_eu"
      )
    )
  )
  
  outcome_model_int <- declare_step(
    handler = function(data) {
      data %>%
        mutate(
          landlord_foreign = as.numeric(landlord_type == "foreign"),
          p = case_when(
            treatment == "control" ~ p0,
            treatment == "eu"      ~ p0,  # sin efecto en este bloque de heterogeneidad
            treatment == "non_eu" & landlord_type == "native"  ~ p0 - delta_native,
            treatment == "non_eu" & landlord_type == "foreign" ~ p0 - delta_foreign,
            TRUE ~ p0
          ),
          p = pmin(pmax(p, 0), 1),
          Y = rbinom(n(), 1, p),
          d_non = as.numeric(treatment == "non_eu")
        )
    },
    label = "outcome_model_int"
  )
  
  # Inquiry (parámetro de interacción):
  #   (efecto non_eu con foreign) - (efecto non_eu con native)
  # = (-delta_foreign) - (-delta_native) = delta_native - delta_foreign
  inquiry_int <- declare_inquiry(
    INT = delta_native - delta_foreign,
    label = "INT_true"
  )
  
  estimator_int <- declare_estimator(
    Y ~ d_non * landlord_foreign,
    model      = lm_robust,
    clusters   = anuncio_id,
    term       = "d_non:landlord_foreign",
    inquiry    = "INT_true",
    label      = "LPM_interaction"
  )
  
  diagnosands_int <- declare_diagnosands(
    mean_est = mean(estimate, na.rm = TRUE),
    se_mean  = mean(std.error, na.rm = TRUE),
    bias     = mean(estimate - estimand, na.rm = TRUE),
    power    = mean(p.value < alpha, na.rm = TRUE),
    coverage = mean(conf.low <= estimand & conf.high >= estimand, na.rm = TRUE)
  )
  
  design_int <- population_int +
    outcome_model_int +
    inquiry_int +
    estimator_int
  
  list(
    design      = design_int,
    diagnosands = diagnosands_int
  )
}

#########################################
# SIMULACIÓN DE PODER: INTERACCIÓN
#########################################

d_native_seq  <- seq(0.02, 0.35, by = 0.05)  # discriminación fuerte (nativos)
d_foreign_seq <- seq(0.00, 0.20, by = 0.05)  # discriminación más débil (foreign)

nsim_int <- 400

interaction_power_grid <- expand.grid(
  d_native  = d_native_seq,
  d_foreign = d_foreign_seq
) %>%
  as_tibble() %>%
  mutate(
    diag = map2(d_native, d_foreign, function(dn, df) {
      di <- make_design_interact(
        N_ann        = N_ann,
        k            = k,
        p0           = p0,
        delta_native = dn,
        delta_foreign= df,
        alpha        = alpha
      )
      diagnose_design(
        di$design,
        sims          = nsim_int,
        diagnosands   = di$diagnosands,
        bootstrap_sims = 0
      )
    }),
    # extraemos siempre una sola fila usando filtro por inquiry y estimator
    power    = map_dbl(diag, ~ {
      .x$diagnosands_df %>%
        filter(inquiry == "INT_true",
               estimator == "LPM_interaction") %>%
        pull(power) %>%
        .[1]
    }),
    mean_est = map_dbl(diag, ~ {
      .x$diagnosands_df %>%
        filter(inquiry == "INT_true",
               estimator == "LPM_interaction") %>%
        pull(mean_est) %>%
        .[1]
    }),
    bias     = map_dbl(diag, ~ {
      .x$diagnosands_df %>%
        filter(inquiry == "INT_true",
               estimator == "LPM_interaction") %>%
        pull(bias) %>%
        .[1]
    })
  ) %>%
  select(d_native, d_foreign, power, mean_est, bias)

print(head(interaction_power_grid, 10))
summary(interaction_power_grid$power)

# Heatmap de poder para la interacción
ggplot(interaction_power_grid,
       aes(x = d_native, y = d_foreign, fill = power)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title    = "Poder estadístico para detectar interacción",
    subtitle = "Discriminación fuerte - Nativo vs Discriminación más débil - Extranjero",
    x        = expression(delta[native]),
    y        = expression(delta[foreign]),
    fill     = "Power"
  ) +
  theme_minimal(base_size = 14)

# Guardar resultados

if (!dir.exists("outcomes")) dir.create("outcomes")

write_csv(power_results_main,      "outcomes/power_curve_results_main_dd.csv")
write_csv(power_vs_N,              "outcomes/power_vsN_main_dd.csv")
write_csv(interaction_power_grid,  "outcomes/interaction_power_grid_dd.csv")

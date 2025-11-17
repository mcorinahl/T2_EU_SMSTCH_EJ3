########################################################################
# Punto_3.R
# Código R para:
#  - Tablas de estadísticas descriptivas para simulaciones llevadas a cabo
#
########################################################################



############################################################
# MODELO PRINCIPAL (SIN HETEROGENEIDAD)
############################################################

# Elegimos un valor representativo de d para la tabla descriptiva
d_desc      <- 0.10                 
deltas_desc <- c(d_desc / 2, d_desc) # eu cae 5 p.p., non_eu 10 p.p.

# Generamos una sola réplica de datos simulados con ese d
df_desc_main <- generate_data(
  N_ann      = N_ann,
  k          = k,
  p0         = p0,
  delta_treat = deltas_desc
)

# Tabla 1: descriptivas generales
tab_overall_main <- df_desc_main %>%
  summarise(
    N_obs        = n(),
    N_ads        = n_distinct(anuncio_id),
    share_control = mean(treatment == "control"),
    share_eu      = mean(treatment == "eu"),
    share_non_eu  = mean(treatment == "non_eu"),
    mean_Y        = mean(Y),
    sd_Y          = sd(Y)
  )

tab_overall_main

# Tabla 2: descriptivas por tratamiento
tab_by_treat_main <- df_desc_main %>%
  group_by(treatment) %>%
  summarise(
    N_obs   = n(),
    mean_Y  = mean(Y),
    sd_Y    = sd(Y),
    mean_p  = mean(p)    # probabilidad asignada en el DGP
  ) %>%
  arrange(treatment)

tab_by_treat_main

############################################################
# MODELO CON HETEROGENEIDAD
############################################################

# Elegimos un par de deltas representativos para la interacción
# deltas_int[1] = penalización para non_eu con landlord nativo
# deltas_int[2] = penalización para non_eu con landlord extranjero
d_native_desc     <- 0.20   # p.ej. -20 p.p. frente a control (fuerte discriminación)
d_non_native_desc <- 0.05   # p.ej. -5 p.p. frente a control (discriminación más débil)

# Construimos una réplica del DGP con heterogeneidad  
df_desc_hetero <- generate_data(
  N_ann      = N_ann,
  k          = k,
  p0         = p0,
  delta_treat = c(0, 0)
) %>%
  mutate(
    landlord_foreign = ifelse(landlord_type == "foreign", 1, 0),
    p = case_when(
      treatment == "control" ~ p0,
      treatment == "eu" ~ p0 + 0.10,
      treatment == "non_eu" & landlord_type == "native"   ~ p0 - d_native_desc,
      treatment == "non_eu" & landlord_type == "foreign"  ~ p0 - d_non_native_desc,
      TRUE ~ p0
    ),
    p = pmin(pmax(p, 0), 1),
    Y = rbinom(n(), 1, p)
  )

# Tabla 3: descriptivas generales para el escenario con heterogeneidad
tab_overall_hetero <- df_desc_hetero %>%
  summarise(
    N_obs        = n(),
    N_ads        = n_distinct(anuncio_id),
    share_control = mean(treatment == "control"),
    share_eu      = mean(treatment == "eu"),
    share_non_eu  = mean(treatment == "non_eu"),
    share_native_landlord   = mean(landlord_type == "native"),
    share_foreign_landlord  = mean(landlord_type == "foreign"),
    share_agency_landlord   = mean(landlord_type == "agency"),
    mean_Y        = mean(Y),
    sd_Y          = sd(Y)
  )

tab_overall_hetero

# Tabla 4: descriptivas por tratamiento y tipo de landlord
tab_by_treat_landlord <- df_desc_hetero %>%
  group_by(landlord_type, treatment) %>%
  summarise(
    N_obs  = n(),
    mean_Y = mean(Y),
    sd_Y   = sd(Y),
    mean_p = mean(p)
  ) %>%
  arrange(landlord_type, treatment)

tab_by_treat_landlord

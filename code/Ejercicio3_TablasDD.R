########################################################################
# Punto_3_Tablas.R
# Código R para:
#  - Tablas de estadísticas descriptivas
#  - Usando los diseños de DeclareDesign
########################################################################

############################################################
# MODELO PRINCIPAL (SIN HETEROGENEIDAD)
############################################################

# Valor representativo de d para la tabla descriptiva
d_desc         <- 0.10
delta_eu_desc  <- d_desc / 2   # penalización eu (coherente con diseño principal)
delta_non_desc <- d_desc       # penalización non_eu

# Construir diseño DeclareDesign para esos parámetros
dm_desc <- make_design_main(
  N_ann     = N_ann,
  k         = k,
  p0        = p0,
  delta_eu  = delta_eu_desc,
  delta_non = delta_non_desc,
  alpha     = alpha
)

# Generar una sola réplica de datos del diseño (incluye Y, treatment, etc.)
df_desc_main <- draw_data(dm_desc$design)

# Tabla 1: descriptivas generales (modelo principal)
tab_overall_main <- df_desc_main %>%
  summarise(
    N_obs         = n(),
    N_ads         = n_distinct(anuncio_id),
    share_control = mean(treatment == "control"),
    share_eu      = mean(treatment == "eu"),
    share_non_eu  = mean(treatment == "non_eu"),
    mean_Y        = mean(Y),
    sd_Y          = sd(Y)
  )

tab_overall_main

# Tabla 2: descriptivas por tratamiento (modelo principal)
tab_by_treat_main <- df_desc_main %>%
  group_by(treatment) %>%
  summarise(
    N_obs  = n(),
    mean_Y = mean(Y),
    sd_Y   = sd(Y)
  ) %>%
  arrange(treatment)

tab_by_treat_main


############################################################
# MODELO CON HETEROGENEIDAD (INTERACCIÓN)
############################################################

# Valores representativos para la heterogeneidad:
#   d_native_desc  = penalización para non_eu con landlord nativo
#   d_foreign_desc = penalización para non_eu con landlord extranjero
d_native_desc   <- 0.20   # fuerte discriminación de landlords nativos
d_foreign_desc  <- 0.05   # discriminación más débil de landlords extranjeros

# Construir diseño DeclareDesign con heterogeneidad
di_desc <- make_design_interact(
  N_ann        = N_ann,
  k            = k,
  p0           = p0,
  delta_native = d_native_desc,
  delta_foreign= d_foreign_desc,
  alpha        = alpha
)

# Generar una réplica de datos del diseño heterogéneo
df_desc_hetero <- draw_data(di_desc$design)

# Tabla 3: descriptivas generales para el escenario con heterogeneidad
tab_overall_hetero <- df_desc_hetero %>%
  summarise(
    N_obs         = n(),
    N_ads         = n_distinct(anuncio_id),
    share_control = mean(treatment == "control"),
    share_eu      = mean(treatment == "eu"),
    share_non_eu  = mean(treatment == "non_eu"),
    share_native_landlord  = mean(landlord_type == "native"),
    share_foreign_landlord = mean(landlord_type == "foreign"),
    mean_Y        = mean(Y),
    sd_Y          = sd(Y)
  )

tab_overall_hetero

# Tabla 4: descriptivas por tratamiento y tipo de landlord (heterogeneidad)
tab_by_treat_landlord <- df_desc_hetero %>%
  group_by(landlord_type, treatment) %>%
  summarise(
    N_obs  = n(),
    mean_Y = mean(Y),
    sd_Y   = sd(Y)
  ) %>%
  arrange(landlord_type, treatment)

tab_by_treat_landlord

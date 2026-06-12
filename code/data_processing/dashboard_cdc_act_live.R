# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Load data -----------------------------------------------------------------
vigi_banasa_token <- Sys.getenv("vigi_banasa_token")
vigi_panta_token <- Sys.getenv("vigi_panta_token")
vigicasa_token <- Sys.getenv("vigicasa_token")

uri <- "https://redcap.ucdenver.edu/api/"

banasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigi_banasa_token
  )$data

panta <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigi_panta_token
  )$data

vigicasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigicasa_token
  )$data



# ==========================================================
# HOUSEHOLD DATA
# ==========================================================

data <- vigicasa %>%
  mutate(
    comunidad = case_when(
      startsWith(record_id, "C1") ~ "Los Encuentros",
      startsWith(record_id, "C2") ~ "Chiquirines",
      startsWith(record_id, "C3") ~ "Colonia Díaz",
      startsWith(record_id, "C4") ~ "Colonia Barillas",
      startsWith(record_id, "C5") ~ "El Troje",
      TRUE ~ NA_character_
    )
  )%>%
  mutate(f_visita = as.Date(f_visita, format="%Y-%m-%d"))%>%
  filter(redcap_event_name == "event_1_arm_1") 
 
data_2 <- data %>%
  filter(
    acepta_vigicasa == "1"
  )

conteo_por_comunidad <- data_2 %>%
  count(comunidad)
data$fecha_enrolamiento <- as.Date(data$fecha_enrolamiento, format = "%d/%m/%Y")
data_3 <- data %>%
  filter(!is.na(fecha_enrolamiento)) %>%
  mutate(fecha_enrolamiento = as.Date(fecha_enrolamiento, format="%Y-%m-%d"))

conteo_ind_por_comunidad <- data_3 %>%
  count(comunidad)

data_2 <- data_2 %>% 
  mutate(
    criterio = case_when(
      embarazada_m == "1" &
        ninos_menores == 0
      ~ "Embarazada",
      
      embarazada_m == "1" &
        ninos_menores > 0
      ~ "Embarazada y niño <2 años",
      
      embarazada_m == "0" &
        ninos_menores > 0
      ~ "Niño <2 años",
      
      TRUE ~ NA_character_
    )
  )

conteo_por_criterio <- data_2 %>%
  count(criterio) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1)
  )

# ==========================================================
# RESPIRATORY FORMS
# ==========================================================

ficha_casa <- vigicasa

ficha_banasa <- banasa

ficha_pantaleon <- panta

ficha_casa <- ficha_casa %>%
  dplyr::select(
    record_id,
    f_visita_f,
    muestra,
    c(
      virus_detectado___1:
        virus_detectado___5
    )
  )

ficha_banasa <- ficha_banasa %>%
  select(
    record_id,
    f_visita_f,
    muestra,
    c(
      virus_detectado___1:
        virus_detectado___5
    )
  )

ficha_pantaleon <- ficha_pantaleon %>%
  select(
    record_id,
    f_visita_f,
    muestra,
    c(
      virus_detectado___1:
        virus_detectado___5
    )
  )


todas_las_fichas <- bind_rows(
  ficha_casa,
  ficha_banasa,
  ficha_pantaleon
)

todas_las_fichas$f_visita_f <- as.Date(
  todas_las_fichas$f_visita_f,
  format = "%Y-%m-%d"
)

todas_las_fichas <- todas_las_fichas %>%
  filter(!is.na(f_visita_f)) %>%
  mutate(
    sitio = case_when(
      startsWith(record_id, "C") ~ "Vigicasa",
      startsWith(record_id, "B") ~ "Banasa",
      startsWith(record_id, "P") ~ "Pantaleon",
      TRUE ~ NA_character_
    )
  )


write.csv(todas_las_fichas, file = "docs/todas_las_fichas.csv", row.names = FALSE)
write.csv(conteo_por_criterio, file = "docs/conteo_por_criterio.csv", row.names = FALSE)
write.csv(conteo_por_comunidad, file = "docs/conteo_por_individuo.csv", row.names = FALSE)
write.csv(conteo_ind_por_comunidad, file = "docs/conteo_por_comunidad.csv", row.names = FALSE)

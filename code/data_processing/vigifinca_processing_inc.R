# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Load data -----------------------------------------------------------------
vigi_banasa_token <- Sys.getenv("vigi_banasa_token")
vigi_panta_token <- Sys.getenv("vigi_panta_token")

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

# Processing -----------------------------------------------------------------
# dates in the right format
banasa$f_muestra <- ymd(banasa$f_muestra)
banasa$fech_tom <- ymd(banasa$fech_tom)

panta$f_muestra <- ymd(panta$f_muestra)
panta$fech_tom <- ymd(panta$fech_tom)

# get rid of any blanks (non-filled out fichas)
banasa <- banasa %>%
  filter(!(is.na(f_muestra) & is.na(fech_tom)))

panta <- panta %>%
  filter(!(is.na(f_muestra) & is.na(fech_tom)))

##### fix the mismatch of districts and municipalities between the two databases
# ---- BANASA ----
#banasa <- banasa %>%
#  mutate(
 #   municipio_force = case_when(
      # RESP (personal/household)
 #     !is.na(direccion_3) & direccion_3 == 1 ~ municipio_p,
  #    !is.na(direccion_3) & direccion_3 == 0 ~ municipio_p_2,
      
      # DENG (household/dengue)
   #   !is.na(departamento_d_3) & departamento_d_3 == 1 ~ municipio_d,
   #   !is.na(departamento_d_3) & departamento_d_3 == 0 ~ municipio_d_2
 #   ),
  #  municipio_force = case_when(
  #    municipio_force == 1 ~ "Coatepeque",
  #    municipio_force == 2 ~ "Colomba",
  #    municipio_force == 3 ~ "El Asintal",
  #    municipio_force == 4 ~ "La Blanca",
   #   municipio_force == 5 ~ "La Reforma",
  #    municipio_force == 6 ~ "Pajapita",
  #    municipio_force == 7 ~ "Nuevo San Carlos",
  #    municipio_force == 8 ~ "San Sebastián",
  #    municipio_force == 9 ~ "El Quetzal",
  #    municipio_force == 10 ~ "Retalhuleu",
  #    municipio_force == 11 ~ "Malacatán",
  #    municipio_force == 12 ~ "Génova",
   #   municipio_force == 13 ~ "Flores"
 #   )
#  )

# ---- PANTA ----
#panta <- panta %>%
#  mutate(
#    municipio_force = case_when(
 #     # RESP
  #    !is.na(municipio_p) ~ municipio_p,
      
      # DENG
##      !is.na(municipio_d) ~ municipio_d
  #  ),
   # municipio_force = case_when(
    #  municipio_force == 1  ~ "Santa Lucía Cotzumalguapa",
     # municipio_force == 2  ~ "Ciudad de Guatemala",
      #municipio_force == 3  ~ "El Rodeo",
      #municipio_force == 4  ~ "Escuintla",
#      municipio_force == 5  ~ "La Democracia",
#      municipio_force == 6  ~ "La Gomera",
#      municipio_force == 7  ~ "Puerto San José",
 #     municipio_force == 8  ~ "San Andrés Osuna",
 #     municipio_force == 9  ~ "San Cristóbal",
  #    municipio_force == 10 ~ "San Pedro Yepocapa",
  #    municipio_force == 11 ~ "Santa Bárbara",
  #    municipio_force == 12 ~ "Siquinala",
  #    municipio_force == 13 ~ "Mixco",
  #    municipio_force == 14 ~ "San Miguel Chicaj",
  #    municipio_force == 15 ~ "San Pedro Sacatepéquez",
  #    municipio_force == 16 ~ "San Pedro Sacatepéquez"  # for completeness
 #   )
#  )


##### bind banasa and panta together with their respective farm label
# Find common columns
common_cols <- intersect(names(panta), names(banasa))

# Function to coerce column to class of reference
coerce_to_class <- function(column, ref_column) {
  target_class <- class(ref_column)[1]  # get main class
  switch(target_class,
         "Date" = as.Date(column),
         "numeric" = as.numeric(column),
         "integer" = as.integer(column),
         "logical" = as.logical(column),
         "factor" = as.factor(column),
         "character" = as.character(column),
         column)  # fallback: leave unchanged
}

# Align panta column types to banasa's
panta <- panta %>%
  select(all_of(common_cols)) %>%
  mutate(across(all_of(common_cols),
                ~ coerce_to_class(., banasa[[cur_column()]]))) %>%
  mutate(lugar = "Pantaleon")

# Align banasa and add label
banasa <- banasa %>%
  select(all_of(common_cols)) %>%
  mutate(lugar = "Banasa")

# Combine them
vigifinca <- bind_rows(panta, banasa)

############################ RESPIRATORY RESULTS
# Set this to TRUE to exclude post-June 23 samples without flu or RSV testing
exclude_flu_rsv_in_range <- TRUE
cutoff_start <- as.Date("2025-06-23")
cutoff_end   <- as.Date("2025-07-16")

resp_results <- vigifinca %>%
  filter(
    !is.na(f_muestra) &
      if_any(starts_with("virus_detectado___"), ~ .x %in% 1)
  ) %>%
  mutate(
    epiweek = epiweek(f_muestra),
    year = year(f_muestra),
    age = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    #municipio = toupper(municipio_force),
    sex = sexo_paciente,
    fecha_muestra = f_muestra
  ) %>%
  group_by(record_id, epiweek, year, age, sex, fecha_muestra, lugar) %>%
  summarize(
    total_tested = n_distinct(record_id),
    total_pos = n_distinct(record_id[virus_detectado___1 == 0], na.rm = TRUE),
    total_neg = n_distinct(record_id[virus_detectado___1 == 1], na.rm = TRUE),
    sars_cov2_pos = sum(virus_detectado___4 == 1, na.rm = TRUE),
    sars_cov2_neg = sum(virus_detectado___4 == 0, na.rm = TRUE),
    inf_a_pos = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_a_neg = sum(virus_detectado___2 == 0, na.rm = TRUE),
    inf_b_pos = sum(virus_detectado___3 == 1, na.rm = TRUE),
    inf_b_neg = sum(virus_detectado___3 == 0, na.rm = TRUE),
    vsr_pos = sum(virus_detectado___5 == 1, na.rm = TRUE),
    vsr_neg = sum(virus_detectado___5 == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    source = "Resp",
    
    #Zero out flu and RSV results if within the cutoff range and enabled
    inf_a_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_pos),
    inf_a_neg = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_neg),
    inf_b_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_b_pos),
    sars_cov2_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, sars_cov2_pos),
    inf_b_neg = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_b_neg),
    vsr_pos   = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, vsr_pos),
    vsr_neg   = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, vsr_neg)
  )

############################ DENGUE RESULTS
dengue_results <- vigifinca %>%
  filter(
    is.na(f_muestra) & 
      !is.na(fech_tom) & 
      if_any(c(p_ns1, p_igm, p_igg), ~ !is.na(.x))
  ) %>%  # Only dengue, exclude anyone with f_muestra
  mutate(
    epiweek = epiweek(fech_tom),
    year = year(fech_tom),
    age = floor(interval(start = fech_nacim, end = fech_tom) / years(1)),
    #municipio = toupper(municipio_force),
    sex = sexo_2,
    fecha_muestra = fech_tom
  ) %>%
  group_by(record_id, epiweek, year, age, sex, fecha_muestra, lugar) %>%
  summarize(
    total_tested = n_distinct(record_id),
    ns1_pos = sum(p_ns1 == 1, na.rm = TRUE),
    ns1_neg = sum(p_ns1 == 2, na.rm = TRUE),
    igm_pos = sum(p_igm == 1, na.rm = TRUE),
    igm_neg = sum(p_igm == 2, na.rm = TRUE),
    igg_pos = sum(p_igg == 1, na.rm = TRUE),
    igg_neg = sum(p_igg == 2, na.rm = TRUE),
    igg_neg = sum(p_igg == 2, na.rm = TRUE),
    pcr_pos = sum(p_pcr == 1, na.rm = TRUE),
    deng_pos = sum(ns1_pos == 1 |igm_pos == 1  |pcr_pos == 1 , na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(source = "Deng")

# Combine both into one unified results table 
vigifinca_results <- bind_rows(resp_results, dengue_results) %>%
  arrange(year, epiweek)

# convert epiweek/year to date
vigifinca_results <- vigifinca_results %>%
  mutate(epiweek_date = as.Date(paste(year, epiweek, 0), format = "%Y %U %w"))


vigifinca_results$week_start <- floor_date(vigifinca_results$fecha_muestra, unit = "week", week_start = 7)  # changed to fecha_vigilancia

vigifinca_results_week <- vigifinca_results %>% group_by(week_start, lugar, source) %>% 
  summarise(total_tested = sum(total_tested), inf_a_pos = sum(inf_a_pos), 
            inf_b_pos = sum(inf_b_pos), vsr_pos = sum(vsr_pos),
            ns1_pos = sum(ns1_pos), igm_pos = sum(igm_pos), igg_pos = sum(igg_pos), deng_pos = sum(deng_pos), scv2_pos = sum(sars_cov2_pos)
           )


vigifinca_results_week$month <- month(vigifinca_results_week$week_start)
vigifinca_results_week$denom <- ifelse(vigifinca_results_week$lugar == "Pantaleon" & (vigifinca_results_week$month <6 | vigifinca_results_week$month >10), 7000,
                                       ifelse(vigifinca_results_week$lugar == "Pantaleon" ,5000, ifelse(vigifinca_results_week$lugar == "Banasa" & (vigifinca_results_week$month <4 | vigifinca_results_week$month >9),
                                              2000, 3000  )))


library(dplyr)
library(zoo)


vigifinca_results_roll <- vigifinca_results_week %>%
  arrange(lugar, source, week_start) %>%   # critical: rollsum has no date awareness
  group_by(lugar, source) %>%
  mutate(
    # rolling sums (3-week centered window)
    total_tested_roll = rollsum(total_tested, k = 3, align = "center", fill = NA),
    inf_a_pos_roll     = rollsum(inf_a_pos,    k = 3, align = "center", fill = NA),
    inf_b_pos_roll     = rollsum(inf_b_pos,    k = 3, align = "center", fill = NA),
    vsr_pos_roll       = rollsum(vsr_pos,      k = 3, align = "center", fill = NA),
    ns1_pos_roll       = rollsum(ns1_pos,      k = 3, align = "center", fill = NA),
    igm_pos_roll       = rollsum(igm_pos,      k = 3, align = "center", fill = NA),
    igg_pos_roll       = rollsum(igg_pos,      k = 3, align = "center", fill = NA),
    deng_pos_roll       = rollsum(deng_pos,      k = 3, align = "center", fill = NA),
    scv2_pos_roll       = rollsum(scv2_pos,      k = 3, align = "center", fill = NA),
    denom_roll       = rollsum(denom,          k = 3, align = "center", fill = NA),
    
    # raw (weekly) test positivity
    inf_a_pos_rate = inf_a_pos / total_tested,
    inf_b_pos_rate = inf_b_pos / total_tested,
    vsr_pos_rate   = vsr_pos   / total_tested,
    ns1_pos_rate   = ns1_pos   / total_tested,
    igm_pos_rate   = igm_pos   / total_tested,
    igg_pos_rate   = igg_pos   / total_tested,
    deng_pos_rate   = deng_pos   / total_tested,
    scv2_pos_rate   = scv2_pos   / total_tested,
    
    
    # raw (weekly) incidence
    inf_a_pos_inc = inf_a_pos / denom,
    inf_b_pos_inc = inf_b_pos / denom,
    vsr_pos_inc   = vsr_pos   / denom,
    ns1_pos_inc   = ns1_pos   / denom,
    igm_pos_inc   = igm_pos   / denom,
    igg_pos_inc   = igg_pos   / denom,
    deng_pos_inc   = deng_pos   / denom,
    scv2_pos_inc   = scv2_pos   / denom,
    tested_inc  = total_tested / denom,
    
    # rolling test positivity (rolling positives / rolling tested)
    inf_a_pos_rate_roll = inf_a_pos_roll / total_tested_roll,
    inf_b_pos_rate_roll = inf_b_pos_roll / total_tested_roll,
    vsr_pos_rate_roll   = vsr_pos_roll   / total_tested_roll,
    ns1_pos_rate_roll   = ns1_pos_roll   / total_tested_roll,
    igm_pos_rate_roll   = igm_pos_roll   / total_tested_roll,
    igg_pos_rate_roll   = igg_pos_roll   / total_tested_roll,
    deng_pos_rate_roll   = deng_pos_roll   / total_tested_roll,
    scv2_pos_rate_roll   = scv2_pos_roll   / total_tested_roll,
    
    inf_a_pos_inc_roll = inf_a_pos_roll / denom_roll,
    inf_b_pos_inc_roll = inf_b_pos_roll / denom_roll,
    vsr_pos_inc_roll   = vsr_pos_roll   / denom_roll,
    ns1_pos_inc_roll   = ns1_pos_roll   / denom_roll,
    igm_pos_inc_roll   = igm_pos_roll   / denom_roll,
    igg_pos_inc_roll   = igg_pos_roll   / denom_roll,
    scv2_pos_inc_roll   = scv2_pos_roll   / denom_roll,
    deng_pos_inc_roll   = deng_pos_roll   / denom_roll,
    tested_inc_roll   = total_tested_roll   / denom_roll
    
  ) %>%
  ungroup()


vigifinca_results_week_overall <- vigifinca_results %>% group_by(week_start, source) %>% 
  summarise(total_tested = sum(total_tested), inf_a_pos = sum(inf_a_pos), 
            inf_b_pos = sum(inf_b_pos), vsr_pos = sum(vsr_pos),
            ns1_pos = sum(ns1_pos), igm_pos = sum(igm_pos), igg_pos = sum(igg_pos),deng_pos = sum(deng_pos), scv2_pos =sum(sars_cov2_pos))

vigifinca_results_week_overall$month <- month(vigifinca_results_week_overall$week_start)
vigifinca_results_week_overall$denom <- ifelse(vigifinca_results_week_overall$month <4 | vigifinca_results_week_overall$month >10, 9000,
                                               ifelse(vigifinca_results_week_overall$month <6 ,10000, 
                                                      ifelse(vigifinca_results_week_overall$month %in% c(6,7,8,9), 8000,  7000)))

vigifinca_results_roll_overall <- vigifinca_results_week_overall %>%
  arrange( source, week_start) %>%   # critical: rollsum has no date awareness
  group_by( source) %>%
  mutate(
    # rolling sums (3-week centered window)
    total_tested_roll = rollsum(total_tested, k = 3, align = "center", fill = NA),
    inf_a_pos_roll     = rollsum(inf_a_pos,    k = 3, align = "center", fill = NA),
    inf_b_pos_roll     = rollsum(inf_b_pos,    k = 3, align = "center", fill = NA),
    vsr_pos_roll       = rollsum(vsr_pos,      k = 3, align = "center", fill = NA),
    ns1_pos_roll       = rollsum(ns1_pos,      k = 3, align = "center", fill = NA),
    igm_pos_roll       = rollsum(igm_pos,      k = 3, align = "center", fill = NA),
    igg_pos_roll       = rollsum(igg_pos,      k = 3, align = "center", fill = NA),
    deng_pos_roll       = rollsum(deng_pos,      k = 3, align = "center", fill = NA),
    scv2_pos_roll       = rollsum(scv2_pos,      k = 3, align = "center", fill = NA),
    denom_roll       = rollsum(denom,          k = 3, align = "center", fill = NA),
    
    # raw (weekly) test positivity
    inf_a_pos_rate = inf_a_pos / total_tested,
    inf_b_pos_rate = inf_b_pos / total_tested,
    vsr_pos_rate   = vsr_pos   / total_tested,
    ns1_pos_rate   = ns1_pos   / total_tested,
    igm_pos_rate   = igm_pos   / total_tested,
    igg_pos_rate   = igg_pos   / total_tested,
    deng_pos_rate   = deng_pos   / total_tested,
    scv2_pos_rate   = scv2_pos   / total_tested,
    
    
    # raw (weekly) incidence
    inf_a_pos_inc = inf_a_pos / denom,
    inf_b_pos_inc = inf_b_pos / denom,
    vsr_pos_inc   = vsr_pos   / denom,
    ns1_pos_inc   = ns1_pos   / denom,
    igm_pos_inc   = igm_pos   / denom,
    igg_pos_inc   = igg_pos   / denom,
    deng_pos_inc   = deng_pos   / denom,
    scv2_pos_inc   = scv2_pos   / denom,
    tested_inc  = total_tested / denom,
    
    # rolling test positivity (rolling positives / rolling tested)
    inf_a_pos_rate_roll = inf_a_pos_roll / total_tested_roll,
    inf_b_pos_rate_roll = inf_b_pos_roll / total_tested_roll,
    vsr_pos_rate_roll   = vsr_pos_roll   / total_tested_roll,
    ns1_pos_rate_roll   = ns1_pos_roll   / total_tested_roll,
    igm_pos_rate_roll   = igm_pos_roll   / total_tested_roll,
    igg_pos_rate_roll   = igg_pos_roll   / total_tested_roll,
    deng_pos_rate_roll   = deng_pos_roll   / total_tested_roll,
    scv2_pos_rate_roll   = scv2_pos_roll   / total_tested_roll,
    
    inf_a_pos_inc_roll = inf_a_pos_roll / denom_roll,
    inf_b_pos_inc_roll = inf_b_pos_roll / denom_roll,
    vsr_pos_inc_roll   = vsr_pos_roll   / denom_roll,
    ns1_pos_inc_roll   = ns1_pos_roll   / denom_roll,
    igm_pos_inc_roll   = igm_pos_roll   / denom_roll,
    igg_pos_inc_roll   = igg_pos_roll   / denom_roll,
    scv2_pos_inc_roll   = scv2_pos_roll   / denom_roll,
    deng_pos_inc_roll   = deng_pos_roll   / denom_roll,
    tested_inc_roll   = total_tested_roll   / denom_roll
    
  ) %>%
  ungroup()


vigifinca_results_roll_overall$lugar <- "Overall"

vigifinca_results_roll <- rbind(vigifinca_results_roll, vigifinca_results_roll_overall)

write.csv(vigifinca_results_roll, file = "docs/vigifinca_incidence.csv", row.names = FALSE)


vigifinca$sign_sintom___3 <- ifelse(vigifinca$sign_sintom___3 == 1 |
                                      vigifinca$sign_sintom___10 == 1, 1,vigifinca$sign_sintom___3  )


##clean up symptoms
vigifinca <- vigifinca %>% 
  rename( anorexia_d = sign_sintom___1,
          dolor_articular_d = sign_sintom___2,
          articulares_hinchados_d = sign_sintom___3,
          fatiga_d = sign_sintom___4,
          dolor_cabeza_d = sign_sintom___5,
          conjuntivitis_d = sign_sintom___6,
          diarrea_d = sign_sintom___7,
          dolor_abdominal_d = sign_sintom___8,
          dolor_ojos_d = sign_sintom___9,
          enterorragia_d = sign_sintom___11,
          epistaxis_d = sign_sintom___12,
          sarpullido_d = sign_sintom___13, 
          fiebre_d = sign_sintom___14, 
          hemorragia_encías_d = sign_sintom___15, 
          hemorragia_urinaria_d = sign_sintom___16, 
          hemorragia_vaginal_d = sign_sintom___17, 
          melena_d = sign_sintom___18, 
          dolor_cuerpo_d = sign_sintom___19, 
          petequias_d = sign_sintom___20, 
          piel_fria_d = sign_sintom___21, 
          sudoracion_d = sign_sintom___22, 
          tos_d = sign_sintom___23, 
          vomito_d = sign_sintom___24, 
          vomito_sangre_d = sign_sintom___25, 
          manifestaciones_neurologicas_d = sign_sintom___26)
          
symptom_vars_r <- c(
  "fiebre_38", "ante_fiebre", "tos_p", "malestar", "dolor_decabeza",
  "dolor_muscular_articulaciones", "odinofagia", "rinorrea", "conjuntivitis",
  "adenopatia", "disnea", "p_gusto", "perdida_olfato", "nausea_vomitos",
  "diarrea_r", "alt_conciencia", "estridor", "tiraje", "aleteo_nasal",
  "vomitos_diarrea")

symptom_vars_d <- c(
  "anorexia_d", "dolor_articular_d", "articulares_hinchados_d",
  "fatiga_d" ,"dolor_cabeza_d","conjuntivitis_d","diarrea_d",
  "dolor_abdominal_d", "enterorragia_d",
  "epistaxis_d", "sarpullido_d",  "fiebre_d" , "hemorragia_encías_d" , 
  "hemorragia_urinaria_d",  "hemorragia_vaginal_d",  "melena_d", 
  "dolor_cuerpo_d", "petequias_d", "piel_fria_d", "sudoracion_d", 
  "tos_d" ,  "vomito_d" , "vomito_sangre_d" , "manifestaciones_neurologicas_d" )



symptom_results_resp <- vigifinca %>%
  filter(!is.na(f_visita_f)) %>%
  mutate(week_start = floor_date(f_visita_f, unit = "week", week_start = 7)) %>%
  group_by(record_id, lugar, week_start) %>%
  summarise(
    across(all_of(symptom_vars_r), ~ as.integer(any(.x == 1, na.rm = TRUE))),
    .groups = "drop"
  )

symptom_results_deng <- vigifinca %>%
  filter(!is.na(fecha_visita)) %>%
  mutate(week_start = floor_date(fecha_visita, unit = "week", week_start = 7)) %>%
  group_by(record_id,lugar, week_start) %>%
  summarise(
    across(all_of(symptom_vars_d), ~ as.integer(any(.x == 1, na.rm = TRUE))),
    .groups = "drop"
  )



symptom_resp_weekly <- symptom_results_resp %>%
  group_by(week_start, lugar) %>%
  summarise(
    across(all_of(symptom_vars_r), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(symptom_vars_r)), symptom_vars_r)))


symptom_deng_weekly <- symptom_results_deng %>%
  group_by(week_start, lugar) %>%
  summarise(
    across(all_of(symptom_vars_d), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(symptom_vars_d)), symptom_vars_d)))






# --- Rolling window size (in weeks) — adjust as needed ---
# Use an ODD number for a clean, symmetric centered window (e.g. 3 = the
# week itself + 1 week before + 1 week after).
roll_window <- 3

# rollsum(..., align = "center") = centered rolling sum. fill = NA means the
# first and last floor(roll_window/2) rows of each group/series will be NA,
# since there aren't enough weeks on one side yet to fill the window.

# =========================================================
# LUGAR-LEVEL COUNTS
# =========================================================
counts <- vigifinca_results_week %>%
  group_by(week_start, lugar, source, denom, total_tested) %>%
  tally() %>%
  ungroup()

# =========================================================
# OVERALL COUNTS (no lugar, but keeps source) — built by summing
# the lugar-level counts up to week level within each source, so
# it is guaranteed to have exactly one row per week_start/source
# even if denom/total_tested vary by lugar.
# =========================================================
counts_overall <- counts %>%
  group_by(week_start, source) %>%
  summarise(
    n            = sum(n),
    denom        = sum(denom),
    total_tested = sum(total_tested),
    .groups = "drop"
  ) %>%
  arrange(source, week_start) %>%
  group_by(source) %>%
  mutate(
    n_roll            = rollsum(n,            k = roll_window, fill = NA, align = "center"),
    denom_roll        = rollsum(denom,        k = roll_window, fill = NA, align = "center"),
    total_tested_roll = rollsum(total_tested, k = roll_window, fill = NA, align = "center")
  ) %>%
  ungroup()

# Now add the rolling columns to the lugar-level counts (rolled within
# each lugar + source combination)
counts <- counts %>%
  arrange(lugar, source, week_start) %>%
  group_by(lugar, source) %>%
  mutate(
    n_roll            = rollsum(n,            k = roll_window, fill = NA, align = "center"),
    denom_roll        = rollsum(denom,        k = roll_window, fill = NA, align = "center"),
    total_tested_roll = rollsum(total_tested, k = roll_window, fill = NA, align = "center")
  ) %>%
  ungroup()

# =========================================================
# RESP — by lugar
# =========================================================
symptom_resp_weekly$source <- "Resp"

symptom_incidence_resp <- symptom_resp_weekly %>%
  left_join(counts, by = c("week_start", "lugar", "source")) %>%
  mutate(across(all_of(symptom_vars_r), ~ replace_na(.x, 0))) %>%
  arrange(lugar, week_start) %>%
  group_by(lugar) %>%
  mutate(across(
    all_of(symptom_vars_r),
    ~ 1000 * rollsum(.x, k = roll_window, fill = NA, align = "center") / denom_roll,
    .names = "{.col}_roll"
  )) %>%
  ungroup()

symptom_incidence_resp <- symptom_incidence_resp %>%
  mutate(across(all_of(symptom_vars_r), ~ 1000 * .x / denom, .names = "{.col}_inc"))

# =========================================================
# DENGUE — by lugar
# =========================================================
symptom_deng_weekly$source <- "Deng"

symptom_incidence_deng <- symptom_deng_weekly %>%
  left_join(counts, by = c("week_start", "lugar", "source")) %>%
  mutate(across(all_of(symptom_vars_d), ~ replace_na(.x, 0))) %>%
  arrange(lugar, week_start) %>%
  group_by(lugar) %>%
  mutate(across(
    all_of(symptom_vars_d),
    ~ 1000 * rollsum(.x, k = roll_window, fill = NA, align = "center") / denom_roll,
    .names = "{.col}_roll"
  )) %>%
  ungroup()

symptom_incidence_deng <- symptom_incidence_deng %>%
  mutate(across(all_of(symptom_vars_d), ~ 1000 * .x / denom, .names = "{.col}_inc"))


# =========================================================
# RESP — overall
# =========================================================
symptom_resp_weekly_o <- symptom_results_resp %>%
  group_by(week_start) %>%
  summarise(
    across(all_of(symptom_vars_r), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(symptom_vars_r)), symptom_vars_r)))
symptom_resp_weekly_o$source <- "Resp"

symptom_deng_weekly_o <- symptom_results_deng %>%
  group_by(week_start) %>%
  summarise(
    across(all_of(symptom_vars_d), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(symptom_vars_d)), symptom_vars_d)))
symptom_deng_weekly_o$source <- "Deng"

symptom_incidence_resp_o <- symptom_resp_weekly_o %>%
  left_join(counts_overall, by = c("week_start", "source")) %>%
  mutate(across(all_of(symptom_vars_r), ~ replace_na(.x, 0))) %>%
  arrange(week_start) %>%
  mutate(across(
    all_of(symptom_vars_r),
    ~ 1000 * rollsum(.x, k = roll_window, fill = NA, align = "center") / denom_roll,
    .names = "{.col}_roll"
  ))

symptom_incidence_resp_o <- symptom_incidence_resp_o %>%
  mutate(across(all_of(symptom_vars_r), ~ 1000 * .x / denom, .names = "{.col}_inc"))

symptom_incidence_deng_o <- symptom_deng_weekly_o %>%
  left_join(counts_overall, by = c("week_start", "source")) %>%
  mutate(across(all_of(symptom_vars_d), ~ replace_na(.x, 0))) %>%
  arrange(week_start) %>%
  mutate(across(
    all_of(symptom_vars_d),
    ~ 1000 * rollsum(.x, k = roll_window, fill = NA, align = "center") / denom_roll,
    .names = "{.col}_roll"
  ))

symptom_incidence_deng_o <- symptom_incidence_deng_o %>%
  mutate(across(all_of(symptom_vars_d), ~ 1000 * .x / denom, .names = "{.col}_inc"))

# =========================================================
# COMBINE lugar-level + overall into one table per source
# =========================================================
symptom_incidence_resp_combined <- bind_rows(
  symptom_incidence_resp,
  symptom_incidence_resp_o %>% mutate(lugar = "Overall")
)

symptom_incidence_deng_combined <- bind_rows(
  symptom_incidence_deng,
  symptom_incidence_deng_o %>% mutate(lugar = "Overall")
)

write.csv(symptom_incidence_resp_combined, "docs/finca_symptom_incidence_resp.csv", row.names = FALSE)
write.csv(symptom_incidence_deng_combined, "docs/finca_symptom_incidence_deng.csv", row.names = FALSE)

vigifinca_results_deng <- vigifinca_results %>% filter(source == "Deng")
vigifinca_results_resp <- vigifinca_results %>% filter(source == "Resp")

results_symptoms_deng <- left_join(symptom_results_deng,vigifinca_results_deng )
results_symptoms_resp <- left_join(symptom_results_resp,vigifinca_results_resp )

results_symptoms_deng$month <- month(results_symptoms_deng$fecha_muestra)
results_symptoms_deng$year <- year(results_symptoms_deng$fecha_muestra)
results_symptoms_resp$month <- month(results_symptoms_resp$fecha_muestra)
results_symptoms_resp$year <- year(results_symptoms_resp$fecha_muestra)

###NOT ENOUGH POSITIVES TO REPORT 
results_symptoms_deng_month <- results_symptoms_deng %>%
  filter(deng_pos == 1) %>% 
  group_by(month) %>% 
  group_by(month, year) %>%
  summarise(
    n_positive = n(),
    across(all_of(symptom_vars_d), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(all_of(symptom_vars_d), ~ 100 * .x / n_positive, .names = "{.col}_pct"),
    pos_pathogen = "Dengue"  )

path_vars <-  c("inf_a_pos", "inf_b_pos", "sars_cov2_pos", "vsr_pos")
  
results_symptoms_resp_month <- purrr::map_dfr(path_vars, function(path) {
  results_symptoms_resp %>%
    filter(.data[[path]] == 1) %>%
    group_by(month, year) %>%
    summarise(
      n_positive = n(),
      across(all_of(symptom_vars_r), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      across(all_of(symptom_vars_r), ~ 100 * .x / n_positive, .names = "{.col}_pct"),
      pos_pathogen = path
    )
})

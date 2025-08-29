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
banasa <- banasa %>%
  mutate(
    municipio_force = case_when(
      # RESP (personal/household)
      !is.na(direccion_3) & direccion_3 == 1 ~ municipio_p,
      !is.na(direccion_3) & direccion_3 == 0 ~ municipio_p_2,
      
      # DENG (household/dengue)
      !is.na(departamento_d_3) & departamento_d_3 == 1 ~ municipio_d,
      !is.na(departamento_d_3) & departamento_d_3 == 0 ~ municipio_d_2
    ),
    municipio_force = case_when(
      municipio_force == 1 ~ "Coatepeque",
      municipio_force == 2 ~ "Colomba",
      municipio_force == 3 ~ "El Asintal",
      municipio_force == 4 ~ "La Blanca",
      municipio_force == 5 ~ "La Reforma",
      municipio_force == 6 ~ "Pajapita",
      municipio_force == 7 ~ "Nuevo San Carlos",
      municipio_force == 8 ~ "San Sebastián",
      municipio_force == 9 ~ "El Quetzal",
      municipio_force == 10 ~ "Retalhuleu",
      municipio_force == 11 ~ "Malacatán",
      municipio_force == 12 ~ "Génova",
      municipio_force == 13 ~ "Flores"
    )
  )

# ---- PANTA ----
panta <- panta %>%
  mutate(
    municipio_force = case_when(
      # RESP
      !is.na(municipio_p) ~ municipio_p,
      
      # DENG
      !is.na(municipio_d) ~ municipio_d
    ),
    municipio_force = case_when(
      municipio_force == 1  ~ "Santa Lucía Cotzumalguapa",
      municipio_force == 2  ~ "Ciudad de Guatemala",
      municipio_force == 3  ~ "El Rodeo",
      municipio_force == 4  ~ "Escuintla",
      municipio_force == 5  ~ "La Democracia",
      municipio_force == 6  ~ "La Gomera",
      municipio_force == 7  ~ "Puerto San José",
      municipio_force == 8  ~ "San Andrés Osuna",
      municipio_force == 9  ~ "San Cristóbal",
      municipio_force == 10 ~ "San Pedro Yepocapa",
      municipio_force == 11 ~ "Santa Bárbara",
      municipio_force == 12 ~ "Siquinala",
      municipio_force == 13 ~ "Mixco",
      municipio_force == 14 ~ "San Miguel Chicaj",
      municipio_force == 15 ~ "San Pedro Sacatepéquez",
      municipio_force == 16 ~ "San Pedro Sacatepéquez"  # for completeness
    )
  )


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
exclude_flu_rsv_after_cutoff <- TRUE
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
    municipio = toupper(municipio_force),
    sex = sexo_paciente,
    fecha_muestra = f_muestra
  ) %>%
  group_by(record_id, epiweek, year, age, sex, municipio, fecha_muestra, lugar) %>%
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
    
    # Zero out flu and RSV results if within the cutoff range and enabled
    inf_a_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_pos),
    inf_a_neg = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_neg),
    inf_b_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_b_pos),
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
    municipio = toupper(municipio_force),
    sex = sexo_2,
    fecha_muestra = fech_tom
  ) %>%
  group_by(record_id, epiweek, year, age, sex, municipio, fecha_muestra, lugar) %>%
  summarize(
    total_tested = n_distinct(record_id),
    ns1_pos = sum(p_ns1 == 1, na.rm = TRUE),
    ns1_neg = sum(p_ns1 == 2, na.rm = TRUE),
    igm_pos = sum(p_igm == 1, na.rm = TRUE),
    igm_neg = sum(p_igm == 2, na.rm = TRUE),
    igg_pos = sum(p_igg == 1, na.rm = TRUE),
    igg_neg = sum(p_igg == 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(source = "Deng")

# Combine both into one unified results table 
vigifinca_results <- bind_rows(resp_results, dengue_results) %>%
  arrange(year, epiweek)

# convert epiweek/year to date
vigifinca_results <- vigifinca_results %>%
  mutate(epiweek_date = as.Date(paste(year, epiweek, 0), format = "%Y %U %w"))

############################ WHEN WE HAVE THE LAB FORM
# Create epiweek column
# vigicasa <- vigicasa %>%
#  mutate(epiweek = epiweek(fecha_recoleccion_m))

# # Function to determine final result per record_id (preferring positive)
# resolve_result <- function(main, repit) {
#   case_when(
#     main == 1 | repit == 1 ~ 1,  # Prefer positive
#     main == 2 & (is.na(repit) | repit == 2 | repit == 5) ~ 2,  # Keep negative if no positive
#     (main == 3 & (is.na(repit)) | repit == 3) ~ 3,  # Keep negative if no positive
#     TRUE ~ NA_real_
#   )
# }

# # Apply function to all relevant columns and filter to keep rows with at least one non-NA result
# vigicasa <- vigicasa %>%
#   rowwise() %>%
#   mutate(
#     sars_cov2_final = resolve_result(sars_cov2, sars_cov2_repit),
#     inf_a_final = resolve_result(inf_a, inf_a_repit),
#     inf_b_final = resolve_result(inf_b, inf_b_repit),
#     vsr_final = resolve_result(vsr, vsr_repit)
#   ) %>%
#   ungroup() %>%
#   filter(
#     !is.na(sars_cov2_final) | 
#       !is.na(inf_a_final) | 
#       !is.na(inf_b_final) | 
#       !is.na(vsr_final)
#   )

# # Group by epiweek and count positives, negatives, and total tested
# vigicasa_results <- vigicasa %>%
#   mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
#   mutate(year = year(both_swab_date)) %>%  # Extract year from epiweek (assuming epiweek is in Date format)
#   group_by(record_id_first_letter, epiweek, year) %>%
#   summarize(
#     total_tested = n_distinct(record_id),  # Count unique IDs tested per epiweek
#     total_pos = n_distinct(record_id[sars_cov2_final == 1 | inf_a_final == 1 | 
#                                        inf_b_final == 1 | vsr_final == 1], na.rm = TRUE),
#     total_neg = n_distinct(record_id[(sars_cov2_final == 2 & inf_a_final == 2 & 
#                                         inf_b_final == 2 & vsr_final == 2)], na.rm = TRUE),
#     sars_cov2_pos = sum(sars_cov2_final == 1, na.rm = TRUE),
#     sars_cov2_neg = sum(sars_cov2_final == 2, na.rm = TRUE),
#     inf_a_pos = sum(inf_a_final == 1, na.rm = TRUE),
#     inf_a_neg = sum(inf_a_final == 2, na.rm = TRUE),
#     inf_b_pos = sum(inf_b_final == 1, na.rm = TRUE),
#     inf_b_neg = sum(inf_b_final == 2, na.rm = TRUE),
#     vsr_pos = sum(vsr_final == 1, na.rm = TRUE),
#     vsr_neg = sum(vsr_final == 2, na.rm = TRUE),
#     inf_a_h1n1 = sum(subtipo_infa == "H1N1", na.rm = TRUE),
#     inf_a_h3n2 = sum(subtipo_infa == "H3N2", na.rm = TRUE),
#     inf_a_nosub = sum(inf_a_final == 1 & is.na(subtipo_infa), na.rm = TRUE)
#   ) %>%
#   arrange(year, epiweek)  # Optionally, sort by year and epiweek


# Save the summary dataframe------------------------------------
vigifinca_csv_file <- "docs/vigifinca_summary.csv"
write.csv(vigifinca_results, file = vigifinca_csv_file, row.names = FALSE)

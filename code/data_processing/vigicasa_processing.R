# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Load data -----------------------------------------------------------------
vigicasa_token <- Sys.getenv("vigicasa_token")

uri <- "https://redcap.ucdenver.edu/api/"

vigicasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigicasa_token
  )$data

# Processing -----------------------------------------------------------------
# dates in the right format
vigicasa$f_muestra <- ymd(vigicasa$f_muestra)
vigicasa$fech_tom <- ymd(vigicasa$fech_tom)

# get rid of any blanks (non-filled out fichas)
vigicasa <- vigicasa %>%
  filter(!(is.na(f_muestra) & is.na(fech_tom)))

############################ RESPIRATORY RESULTS
# Set this to TRUE to exclude post-June 23 samples without flu or RSV testing
exclude_flu_rsv_in_range <- TRUE
cutoff_start <- as.Date("2025-06-23")
cutoff_end   <- as.Date("2025-07-16")

resp_results <- vigicasa %>%
  filter(
    !is.na(f_muestra) &
      if_any(starts_with("virus_detectado___"), ~ .x %in% 1)
  ) %>%
  mutate(
    epiweek = epiweek(f_muestra),
    year = year(f_muestra),
    age = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    distrito = toupper(distrito),
    department = toupper(area_salud),
    sex = sexo_paciente,
    fecha_muestra = f_muestra
  ) %>%
  group_by(record_id, epiweek, year, age, sex, distrito, department, fecha_muestra) %>%
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
    
    # Zero out flu and RSV results after cutoff if enabled
    inf_a_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_pos),
    inf_a_neg = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_a_neg),
    inf_b_pos = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_b_pos),
    inf_b_neg = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, inf_b_neg),
    vsr_pos   = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, vsr_pos),
    vsr_neg   = ifelse(exclude_flu_rsv_in_range & fecha_muestra >= cutoff_start & fecha_muestra <= cutoff_end & lugar == "Banasa", 0, vsr_neg)
    
  )

############################ DENGUE RESULTS
dengue_results <- vigicasa %>%
  filter(
    is.na(f_muestra) & 
      !is.na(fech_tom) & 
      if_any(c(p_ns1, p_igm, p_igg), ~ !is.na(.x))
  ) %>%  # Only dengue, exclude anyone with f_muestra
  mutate(
    epiweek = epiweek(fech_tom),
    year = year(fech_tom),
    age = floor(interval(start = fech_nacim, end = fech_tom) / years(1)),
    distrito = toupper(distrito_d),
    department = toupper(area_salud_d),
    sex = sexo_2,
    fecha_muestra = fech_tom,
  ) %>%
  group_by(record_id, epiweek, year, age, sex, distrito, department, fecha_muestra) %>%
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
vigicasa_results <- bind_rows(resp_results, dengue_results) %>%
  arrange(year, epiweek)

# convert epiweek/year to date
vigicasa_results <- vigicasa_results %>%
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
vigicasa_csv_file <- "docs/vigicasa_summary.csv"
write.csv(vigicasa_results, file = vigicasa_csv_file, row.names = FALSE)

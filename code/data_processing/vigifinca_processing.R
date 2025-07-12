# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Load data -----------------------------------------------------------------
vigicasa_token <- Sys.getenv("vigicasa")

uri <- "https://redcap.ucdenver.edu/api/"

vigicasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigicasa_token
  )$data

#### WHEN WE HAVE THE LAB FORM
# Processing -----------------------------------------------------------------
# Create epiweek column
vigicasa <- vigicasa %>%
  mutate(epiweek = epiweek(fecha_recoleccion_m))

# Function to determine final result per record_id (preferring positive)
resolve_result <- function(main, repit) {
  case_when(
    main == 1 | repit == 1 ~ 1,  # Prefer positive
    main == 2 & (is.na(repit) | repit == 2 | repit == 5) ~ 2,  # Keep negative if no positive
    (main == 3 & (is.na(repit)) | repit == 3) ~ 3,  # Keep negative if no positive
    TRUE ~ NA_real_
  )
}

# Apply function to all relevant columns and filter to keep rows with at least one non-NA result
vigicasa <- vigicasa %>%
  rowwise() %>%
  mutate(
    sars_cov2_final = resolve_result(sars_cov2, sars_cov2_repit),
    inf_a_final = resolve_result(inf_a, inf_a_repit),
    inf_b_final = resolve_result(inf_b, inf_b_repit),
    vsr_final = resolve_result(vsr, vsr_repit)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(sars_cov2_final) | 
      !is.na(inf_a_final) | 
      !is.na(inf_b_final) | 
      !is.na(vsr_final)
  )


# Group by epiweek and count positives, negatives, and total tested
vigicasa_results <- vigicasa %>%
  mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
  mutate(year = year(both_swab_date)) %>%  # Extract year from epiweek (assuming epiweek is in Date format)
  group_by(record_id_first_letter, epiweek, year) %>%
  summarize(
    total_tested = n_distinct(record_id),  # Count unique IDs tested per epiweek
    total_pos = n_distinct(record_id[sars_cov2_final == 1 | inf_a_final == 1 | 
                                       inf_b_final == 1 | vsr_final == 1], na.rm = TRUE),
    total_neg = n_distinct(record_id[(sars_cov2_final == 2 & inf_a_final == 2 & 
                                        inf_b_final == 2 & vsr_final == 2)], na.rm = TRUE),
    sars_cov2_pos = sum(sars_cov2_final == 1, na.rm = TRUE),
    sars_cov2_neg = sum(sars_cov2_final == 2, na.rm = TRUE),
    inf_a_pos = sum(inf_a_final == 1, na.rm = TRUE),
    inf_a_neg = sum(inf_a_final == 2, na.rm = TRUE),
    inf_b_pos = sum(inf_b_final == 1, na.rm = TRUE),
    inf_b_neg = sum(inf_b_final == 2, na.rm = TRUE),
    vsr_pos = sum(vsr_final == 1, na.rm = TRUE),
    vsr_neg = sum(vsr_final == 2, na.rm = TRUE),
    inf_a_h1n1 = sum(subtipo_infa == "H1N1", na.rm = TRUE),
    inf_a_h3n2 = sum(subtipo_infa == "H3N2", na.rm = TRUE),
    inf_a_nosub = sum(inf_a_final == 1 & is.na(subtipo_infa), na.rm = TRUE)
  ) %>%
  arrange(year, epiweek)  # Optionally, sort by year and epiweek

vigicasa_results$record_id_first_letter <- ifelse(vigicasa_results$record_id_first_letter == "Q", "Hospital de Coatepeque",
                                                  ifelse(vigicasa_results$record_id_first_letter == "C", "Hospital de Chimaltenango", NA))

vigicasa_results <- vigicasa_results %>%
  rename(hospital = "record_id_first_letter")

# Convert year and epiweek to the correct Monday of that epiweek
vigicasa_results <- vigicasa_results %>%
  mutate(epiweek_date = as.Date(paste(year, epiweek, 1), format = "%Y %U %u"))  # "%U %u" ensures week starts on Monday

# Save the summary dataframe------------------------------------
vigicasa_csv_file <- "docs/vigicasa_summary.csv"
write.csv(vigicasa_results, file = vigicasa_csv_file, row.names = FALSE)
# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Load data -----------------------------------------------------------------
gihsn_token <- Sys.getenv("gihsn_token")

uri <- "https://redcap.ucdenver.edu/api/"

gihsn <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = gihsn_token
  )$data

# Processing -----------------------------------------------------------------
# Create epiweek column
gihsn <- gihsn %>%
  mutate(epiweek = epiweek(both_swab_date))

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
gihsn <- gihsn %>%
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
gihsn_results <- gihsn %>%
  mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
  mutate(year = year(both_swab_date)) %>%  # Extract year from epiweek (assuming epiweek is in Date format)
  group_by(record_id_first_letter, epiweek, year) %>%
  summarize(
    total_tested = n_distinct(record_id),  # Count unique IDs tested per epiweek
    total_pos = n_distinct(record_id[sars_cov2_final == 1 | inf_a_final == 1 | 
                                       inf_b_final == 1 | vsr_final == 1|inf_a_abi==1|
                                       inf_b_abi==1|sars_cov2_abi==1], na.rm = TRUE),
    total_neg = n_distinct(record_id[(sars_cov2_final == 2 & inf_a_final == 2 & 
                                        inf_b_final == 2 & vsr_final == 2)|(inf_a_abi==1&
                                                                              inf_b_abi==1&sars_cov2_abi==1)], na.rm = TRUE),
    sars_cov2_pos = sum(sars_cov2_final == 1, na.rm = TRUE)+
      sum(sars_cov2_abi==1, na.rm = TRUE),
    sars_cov2_neg = sum(sars_cov2_final == 2, na.rm = TRUE)+
      sum(sars_cov2_abi == 2, na.rm = TRUE),
    inf_a_pos = sum(inf_a_final == 1, na.rm = TRUE)+
      sum(inf_a_abi == 1, na.rm = TRUE),
    inf_a_neg = sum(inf_a_final == 2, na.rm = TRUE)+
      sum(inf_a_abi == 2, na.rm = TRUE),
    inf_b_pos = sum(inf_b_final == 1, na.rm = TRUE)+
      sum(inf_b_abi == 1, na.rm = TRUE),
    inf_b_neg = sum(inf_b_final == 2, na.rm = TRUE)+
      sum(inf_b_abi == 2, na.rm = TRUE),
    vsr_pos = sum(vsr_final == 1, na.rm = TRUE),
    vsr_neg = sum(vsr_final == 2, na.rm = TRUE),
    inf_a_h1n1 = sum(subtipo_infa == "H1N1", na.rm = TRUE),
    inf_a_h3n2 = sum(subtipo_infa == "H3N2", na.rm = TRUE),
    inf_a_nosub = sum(inf_a_final == 1 & is.na(subtipo_infa), na.rm = TRUE)+
      sum(inf_a_abi == 1 & is.na(subtipo_infa), na.rm = TRUE),
    vsr_a = sum(resultados_vsra==1, na.rm=TRUE),
    vsr_b = sum(resultados_vsrb==1, na.rm=TRUE),
    vsr_nosub = sum((vsr_final==1 & is.na(resultados_vsra)&is.na(resultados_vsrb))|
                      (vsr_final==1 & resultados_vsra==2&resultados_vsrb==2), na.rm=TRUE)
  ) %>%
  arrange(year, epiweek)  # Optionally, sort by year and epiweek

gihsn_results$record_id_first_letter <- ifelse(gihsn_results$record_id_first_letter == "Q", "Hospital de Coatepeque",
                                               ifelse(gihsn_results$record_id_first_letter == "C", "Hospital de Chimaltenango", NA))

gihsn_results <- gihsn_results %>%
  rename(hospital = "record_id_first_letter")

# Convert year and epiweek to the correct Monday of that epiweek
gihsn_results <- gihsn_results %>%
  mutate(epiweek_date = as.Date(paste(year, epiweek, 0), format = "%Y %U %w"))


# Save the summary dataframe------------------------------------
gihsn_csv_file <- "docs/gihsn_summary.csv"
write.csv(gihsn_results, file = gihsn_csv_file, row.names = FALSE)

#####################################################################################
#Intentando hacer la gráfica de las edades para influenza A
gihsn <- gihsn %>%
  mutate(grupoetario=case_when(both_age_unit==2~ "<5",
                               both_age_unit==1&both_age<5 ~ "<5",
                               both_age_unit==3~ "<5",
                               both_age_unit==1&both_age>4&both_age<50~"5-49",
                               both_age_unit==1&both_age>49&both_age<65~"50-64",
                               both_age_unit==1&both_age>64 ~ ">64"))
df <- gihsn %>%
  filter(inf_a_final!="")%>% 
  filter(inf_a_final=="1")%>%
  count(grupoetario, inf_a_final)%>%
  complete(grupoetario, fill = list(n = 0)) %>%
  mutate(grupoetario = factor(grupoetario, levels = c("<5", "5-49", "50-64", ">64")))
gihsn_csv_file2 <- "docs/gihsn_ages.csv"
write.csv(df, file = gihsn_csv_file2, row.names = FALSE)

#Ahora para VSR
df2 <- gihsn %>%
  filter(vsr_final=="1")%>%
  count(grupoetario, vsr_final)%>%
  complete(grupoetario, fill = list(n = 0)) %>%
  mutate(grupoetario = factor(grupoetario, levels = c("<5", "5-49", "50-64", ">64")))
gihsn_csv_file3 <- "docs/gihsn_ages_vsr.csv"
write.csv(df2, file = gihsn_csv_file3, row.names = FALSE)



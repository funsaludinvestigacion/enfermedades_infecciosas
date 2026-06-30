
library(dplyr)
# Load data -----------------------------------------------------------------
gihsn_token <- Sys.getenv("gihsn_token")

uri <- "https://redcap.ucdenver.edu/api/"

gihsn <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = "0FCBA7123DB567841B7BF1A7FF0399F7"
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

table
# Convert year and epiweek to the correct Monday of that epiweek
gihsn_results <- gihsn_results %>%
  mutate(epiweek_date = as.Date(paste(year, epiweek, 0), format = "%Y %U %w"))

table(gihsn_results$hospital)

chim <- gihsn_results %>% filter( hospital == "Hospital de Chimaltenango")
coa <- gihsn_results %>% filter( hospital == "Hospital de Coatepeque")

chim$tested_roll <- rollsum(chim$total_tested, k= 3, fill = NA)
chim$inf_a_pos_roll <- rollsum(chim$inf_a_pos, k= 3, fill = NA)
chim$inf_b_pos_roll <- rollsum(chim$inf_b_pos, k= 3, fill = NA) 
chim$sars_cov2_pos_roll <- rollsum(chim$sars_cov2_pos, k= 3, fill = NA)
chim$vsr_pos_roll <- rollsum(chim$vsr_pos, k= 3, fill = NA)

chim <- chim %>% arrange(year, epiweek)

chim$flua_inc <- 100*chim$inf_a_pos / chim$total_tested
chim$flub_inc <- 100*chim$inf_b_pos / chim$total_tested
chim$scv2_inc <- 100*chim$sars_cov2_pos / chim$total_tested
chim$rsv_inc <- 100*chim$vsr_pos / chim$total_tested
chim$flu_gen_inc <- 100*(chim$inf_a_pos  + chim$inf_b_pos )/ chim$total_tested

chim$flua_inc_roll <- 100*chim$inf_a_pos_roll / chim$tested_roll
chim$flub_inc_roll <- 100*chim$inf_b_pos_roll / chim$tested_roll
chim$scv2_inc_roll <- 100*chim$sars_cov2_pos_roll/ chim$tested_roll
chim$rsv_inc_roll <- 100*chim$vsr_pos_roll / chim$tested_roll
chim$flu_gen_inc_roll <- 100*(chim$inf_a_pos_roll  + chim$inf_b_pos_roll )/ chim$tested_roll


coa$tested_roll <- rollsum(coa$total_tested, k= 3, fill = NA)
coa$inf_a_pos_roll <- rollsum(coa$inf_a_pos, k= 3, fill = NA)
coa$inf_b_pos_roll <- rollsum(coa$inf_b_pos, k= 3, fill = NA) 
coa$sars_cov2_pos_roll <- rollsum(coa$sars_cov2_pos, k= 3, fill = NA)
coa$vsr_pos_roll <- rollsum(coa$vsr_pos, k= 3, fill = NA)

coa <- coa %>% arrange(year, epiweek)

coa$flua_inc <- 100*coa$inf_a_pos / coa$total_tested
coa$flub_inc <- 100*coa$inf_b_pos / coa$total_tested
coa$scv2_inc <- 100*coa$sars_cov2_pos / coa$total_tested
coa$rsv_inc <- 100*coa$vsr_pos / coa$total_tested
coa$flu_gen_inc <- 100*(coa$inf_a_pos  + coa$inf_b_pos )/ coa$total_tested


coa$flua_inc_roll <- 100*coa$inf_a_pos_roll / coa$tested_roll
coa$flub_inc_roll <- 100*coa$inf_b_pos_roll / coa$tested_roll
coa$scv2_inc_roll <- 100*coa$sars_cov2_pos_roll/ coa$tested_roll
coa$rsv_inc_roll <- 100*coa$vsr_pos_roll / coa$tested_roll
coa$flu_gen_inc_roll <- 100*(coa$inf_a_pos_roll  + coa$inf_b_pos_roll )/ coa$tested_roll

overall <- gihsn_results %>% group_by(epiweek, year) %>% summarise(
  total_tested = sum(total_tested),
  inf_a_pos = sum(inf_a_pos),
  inf_b_pos = sum(inf_b_pos),
  sars_cov2_pos = sum(sars_cov2_pos),
  vsr_pos = sum(vsr_pos)
)

overall <- overall %>% arrange(year, epiweek)
overall$tested_roll <- rollsum(overall$total_tested, k= 3, fill = NA)
overall$inf_a_pos_roll <- rollsum(overall$inf_a_pos, k= 3, fill = NA)
overall$inf_b_pos_roll <- rollsum(overall$inf_b_pos, k= 3, fill = NA) 
overall$sars_cov2_pos_roll <- rollsum(overall$sars_cov2_pos, k= 3, fill = NA)
overall$vsr_pos_roll <- rollsum(overall$vsr_pos, k= 3, fill = NA)

overall$flua_inc <- 100*overall$inf_a_pos / overall$total_tested
overall$flub_inc <- 100*overall$inf_b_pos / overall$total_tested
overall$scv2_inc <- 100*overall$sars_cov2_pos / overall$total_tested
overall$rsv_inc <- 100*overall$vsr_pos / overall$total_tested
overall$flu_gen_inc <- 100*(overall$inf_a_pos  + overall$inf_b_pos )/ overall$total_tested

overall$flua_inc_roll <- 100*overall$inf_a_pos_roll / overall$tested_roll
overall$flub_inc_roll <- 100*overall$inf_b_pos_roll / overall$tested_roll
overall$scv2_inc_roll <- 100*overall$sars_cov2_pos_roll/ overall$tested_roll
overall$rsv_inc_roll <- 100*overall$vsr_pos_roll / overall$tested_roll
overall$flu_gen_inc_roll <- 100*(overall$inf_a_pos_roll  + overall$inf_b_pos_roll )/ overall$tested_roll

overall$hospital <- "Overall"

overall <- overall %>% dplyr::select(hospital, year, epiweek, total_tested, inf_a_pos, 
                                     inf_b_pos, sars_cov2_pos, vsr_pos, 
                                    flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                                    flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll)

chim <- chim %>% dplyr::select(hospital, year, epiweek, total_tested, inf_a_pos, 
                                     inf_b_pos, sars_cov2_pos, vsr_pos, 
                                     flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                                     flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll)

coa <- coa %>% dplyr::select(hospital, year, epiweek, total_tested, inf_a_pos, 
                       inf_b_pos, sars_cov2_pos, vsr_pos, 
                       flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                       flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll)

all_inc1 <- rbind(chim, coa) 
all_inc <- rbind(all_inc1, overall)

 write.csv(all_inc, "docs/gihsn_incidence_week.csv")

# Save the summary dataframe------------------------------------
gihsn_csv_file <- "docs/gihsn_summary.csv"
write.csv(gihsn_results, file = gihsn_csv_file, row.names = FALSE)


library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)

# Load data -----------------------------------------------------------------
gihsn_token <- Sys.getenv("gihsn_token")

uri <- "https://redcap.ucdenver.edu/api/"

gihsn <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = gihsn_token
  )$data

# Processing -----------------------------------------------------------------
gihsn <- gihsn %>%
  mutate(epiweek = epiweek(both_swab_date),
         week_start = floor_date(both_swab_date, unit = "week", week_start = 7) 
  )

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
table(gihsn$sars_cov2_final,gihsn$both_lab2_sarscov2_yesno , useNA = 'always')

# Group by week_start and count positives, negatives, and total tested
gihsn_results <- gihsn %>%
  mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
  group_by(record_id_first_letter, week_start) %>%
  summarize(
    total_tested = n_distinct(record_id),  # Count unique IDs tested per week_start
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
  arrange( week_start)  # Optionally, sort by year and week_start

gihsn_results$record_id_first_letter <- ifelse(gihsn_results$record_id_first_letter == "Q", "Hospital de Coatepeque",
                                               ifelse(gihsn_results$record_id_first_letter == "C", "Hospital de Chimaltenango", NA))

gihsn_results <- gihsn_results %>%
  rename(hospital = record_id_first_letter)

# Convert year and week_start to the correct Monday of that week_start


#El número de camas hospital de Chimaltenango: 232
#El número de camas hospital de Coatepeque: 152

chim <- gihsn_results %>% filter( hospital == "Hospital de Chimaltenango")
coa <- gihsn_results %>% filter( hospital == "Hospital de Coatepeque")

denom_chim <- 232
denom_coa  <- 152
denom_o <- 152 + 232

chim$denom <- denom_chim
inc_vars <- c("denom", "total_tested", "inf_a_pos", "inf_b_pos", "sars_cov2_pos", "vsr_pos")




roll3 <- function(x) rollapply(x, width = 3, FUN = sum, partial = TRUE, align = "center")

chim <- chim %>%
  arrange(week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll"))

chim$flua_inc <- 100*chim$inf_a_pos / chim$denom
chim$flub_inc <- 100*chim$inf_b_pos / chim$denom
chim$scv2_inc <- 100*chim$sars_cov2_pos / chim$denom
chim$rsv_inc <- 100*chim$vsr_pos / chim$denom
chim$flu_gen_inc <- 100*(chim$inf_a_pos  + chim$inf_b_pos )/ chim$denom

chim$flua_inc_roll <- 100*chim$inf_a_pos_roll / chim$denom_roll
chim$flub_inc_roll <- 100*chim$inf_b_pos_roll / chim$denom_roll
chim$scv2_inc_roll <- 100*chim$sars_cov2_pos_roll/ chim$denom_roll
chim$rsv_inc_roll <- 100*chim$vsr_pos_roll / chim$denom_roll
chim$flu_gen_inc_roll <- 100*(chim$inf_a_pos_roll  + chim$inf_b_pos_roll )/ chim$denom_roll


coa$denom <- denom_coa

coa <- coa %>%
  arrange( week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll"))


coa$flua_inc <- 100*coa$inf_a_pos / coa$denom
coa$flub_inc <- 100*coa$inf_b_pos / coa$denom
coa$scv2_inc <- 100*coa$sars_cov2_pos / coa$denom
coa$rsv_inc <- 100*coa$vsr_pos / coa$denom
coa$flu_gen_inc <- 100*(coa$inf_a_pos  + coa$inf_b_pos )/ coa$denom


coa$flua_inc_roll <- 100*coa$inf_a_pos_roll / coa$denom_roll
coa$flub_inc_roll <- 100*coa$inf_b_pos_roll / coa$denom_roll
coa$scv2_inc_roll <- 100*coa$sars_cov2_pos_roll/ coa$denom_roll
coa$rsv_inc_roll <- 100*coa$vsr_pos_roll / coa$denom_roll
coa$flu_gen_inc_roll <- 100*(coa$inf_a_pos_roll  + coa$inf_b_pos_roll )/ coa$denom_roll

overall <- gihsn_results %>% group_by(week_start) %>% summarise(
  total_tested = sum(total_tested),
  inf_a_pos = sum(inf_a_pos),
  inf_b_pos = sum(inf_b_pos),
  sars_cov2_pos = sum(sars_cov2_pos),
  vsr_pos = sum(vsr_pos)
)


overall$denom <- denom_o

overall <- overall %>%
  arrange(week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll"))



overall$flua_inc <- 100*overall$inf_a_pos / overall$denom
overall$flub_inc <- 100*overall$inf_b_pos / overall$denom
overall$scv2_inc <- 100*overall$sars_cov2_pos / overall$denom
overall$rsv_inc <- 100*overall$vsr_pos / overall$denom
overall$flu_gen_inc <- 100*(overall$inf_a_pos  + overall$inf_b_pos )/ overall$denom

overall$flua_inc_roll <- 100*overall$inf_a_pos_roll / overall$denom_roll
overall$flub_inc_roll <- 100*overall$inf_b_pos_roll / overall$denom_roll
overall$scv2_inc_roll <- 100*overall$sars_cov2_pos_roll/ overall$denom_roll
overall$rsv_inc_roll <- 100*overall$vsr_pos_roll / overall$denom_roll
overall$flu_gen_inc_roll <- 100*(overall$inf_a_pos_roll  + overall$inf_b_pos_roll )/ overall$denom_roll

overall$hospital <- "Overall"

overall <- overall %>% dplyr::select(hospital,  week_start, total_tested, inf_a_pos, 
                                     inf_b_pos, sars_cov2_pos, vsr_pos, 
                                    flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                                    flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll, denom, denom_roll)

chim <- chim %>% dplyr::select(hospital, week_start, total_tested, inf_a_pos, 
                                     inf_b_pos, sars_cov2_pos, vsr_pos, 
                                     flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                                     flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll, denom, denom_roll)

coa <- coa %>% dplyr::select(hospital, week_start, total_tested, inf_a_pos, 
                       inf_b_pos, sars_cov2_pos, vsr_pos, 
                       flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                       flub_inc, flub_inc_roll,  scv2_inc, scv2_inc_roll, rsv_inc, rsv_inc_roll, denom, denom_roll)

all_inc1 <- rbind(chim, coa) 
all_inc <- rbind(all_inc1, overall)

 write.csv(all_inc, "docs/gihsn_incidence_week.csv")
# Save the summary dataframe------------------------------------
gihsn_csv_file <- "docs/gihsn_summary.csv"
write.csv(gihsn_results, file = gihsn_csv_file, row.names = FALSE)


#### symptoms 

gihsn_symptom_vars <- c("both_ilisystemic_malaise", "both_ilisystemic_headache",
                       "both_ilisystemic_myalgia", "both_ilirespiratory_cough",
                       "both_ilirespiratory_sorethroat", "both_ilirespiratory_shortnessbreath",
                       "both_ilirespiratory_wheezing","both_ilirespiratory_nasalcongestion",
                        "both_othersymptoms_nausea", "both_othersymptoms_diarrhea",
                       "both_othersymptoms_smell", "both_othersymptoms_chestpain")

symptom_weekly <- gihsn %>%
  group_by(week_start) %>%
  summarise(
    across(all_of(gihsn_symptom_vars), ~ sum(.x == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(gihsn_symptom_vars)), gihsn_symptom_vars)))
# ── 3. Join numerator + denominator
symptom_incidence_o <- symptom_weekly %>%
  left_join(overall, by = "week_start") 

symptom_incidence_o <- symptom_incidence_o %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

# ── 5. Rolling sums & rates (3-week window)
symptom_incidence_o <- symptom_incidence_o %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    across(all_of(gihsn_symptom_vars), ~ 1000 * get(paste0(cur_column(), "_roll")) / denom_roll, .names = "{.col}_inc_roll")
  ) 
# ── 6. Tidy final column order
symptom_incidence_o <- symptom_incidence_o %>%
  select(
    week_start, hospital,
    all_of(inc_vars), all_of(gihsn_symptom_vars),
    ends_with("_inc"), ends_with("_roll")
  ) 

#### symptoms by hospital

gihsn <- gihsn %>%
  mutate(
    record_id_first_letter = substr(record_id, 1, 1),
    hospital = case_when(
      record_id_first_letter == "Q" ~ "Hospital de Coatepeque",
      record_id_first_letter == "C" ~ "Hospital de Chimaltenango",
      TRUE ~ NA_character_
    )
  )

symptom_weekly <- gihsn %>%
  group_by(week_start,hospital) %>%
  summarise(
    across(all_of(gihsn_symptom_vars), ~ sum(.x == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(gihsn_symptom_vars)), gihsn_symptom_vars)))
# ── 3. Join numerator + denominator
symptom_incidence_hosp <- symptom_weekly %>%
  left_join(all_inc1, by = c("week_start", "hospital") )

chim <- symptom_incidence_hosp %>% filter( hospital == "Hospital de Chimaltenango")
coa <- symptom_incidence_hosp %>% filter( hospital == "Hospital de Coatepeque")

symptom_incidence_chim <- chim %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

# ── 5. Rolling sums & rates (3-week window)
symptom_incidence_chim <- symptom_incidence_chim %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    across(all_of(gihsn_symptom_vars), ~ 1000 * get(paste0(cur_column(), "_roll")) / denom_roll, .names = "{.col}_inc_roll")
  ) 
# ── 6. Tidy final column order
symptom_incidence_chim <- symptom_incidence_chim %>%
  select(
    week_start, hospital,
    all_of(inc_vars), all_of(gihsn_symptom_vars),
    ends_with("_inc"), ends_with("_roll"))
  
    symptom_incidence_coa <- coa %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

# ── 5. Rolling sums & rates (3-week window)
    symptom_incidence_coa <- symptom_incidence_coa %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    across(all_of(gihsn_symptom_vars), ~ 1000 * get(paste0(cur_column(), "_roll")) / denom_roll, .names = "{.col}_inc_roll")
  ) 
# ── 6. Tidy final column order
    symptom_incidence_coa <- symptom_incidence_coa %>%
  select(
    week_start, hospital,
    all_of(inc_vars), all_of(gihsn_symptom_vars),
    ends_with("_inc"), ends_with("_roll"))
    
### coinfection and percent pos 
   coinfection_corr <- gihsn %>% mutate(
     inf_a_final = ifelse(inf_a_abi %in% c(1,2),inf_a_abi, inf_a_final),
     inf_b_final = ifelse(inf_b_abi %in% c(1,2),inf_b_abi, inf_b_final),
     sars_cov2_final = ifelse(sars_cov2_abi %in% c(1,2),sars_cov2_abi, sars_cov2_final),
     coinfection =  ifelse( inf_a_final == 1 & 
                               (inf_b_final == 1 | vsr_final == 1 |sars_cov2_final ==1),1,
                             ifelse( inf_b_final == 1 & 
                                       (inf_a_final == 1 | vsr_final == 1 |sars_cov2_final ==1),1,
                                     ifelse( inf_b_final == 1 & 
                                               (inf_a_final == 1 | vsr_final == 1 |sars_cov2_final ==1),1,
                                             ifelse( vsr_final == 1 & 
                                                       (inf_a_final == 1 | inf_b_final == 1 |sars_cov2_final ==1),1,
                                                     ifelse( sars_cov2_final == 1 & 
                                                               (inf_a_final == 1 | inf_b_final == 1 |vsr_final ==1),1, 0))))))
    
    coinfection_corr$sars_cov2_final_corr <- ifelse(coinfection_corr$coinfection == 1, 0,coinfection_corr$sars_cov2_final)                                   
    coinfection_corr$inf_a_pos_final_corr <- ifelse(coinfection_corr$coinfection == 1, 0,coinfection_corr$inf_a_final)                                   
    coinfection_corr$inf_b_final_corr <- ifelse(coinfection_corr$coinfection == 1, 0,coinfection_corr$inf_b_final)                                   
    coinfection_corr$vsr_pos_final_corr <- ifelse(coinfection_corr$coinfection == 1, 0,coinfection_corr$vsr_final)                                   
    
    coinfection_corr_week_hosp <- coinfection_corr %>%
      mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
      group_by(record_id_first_letter, week_start) %>%
      summarize(
      sars_cov2_pos_co = sum(sars_cov2_final == 1, na.rm = TRUE),
      inf_a_pos_co = sum(inf_a_final == 1, na.rm = TRUE),
      inf_b_pos_co = sum(inf_b_final == 1, na.rm = TRUE),
      vsr_pos_co = sum(vsr_final == 1, na.rm = TRUE),
      conif = sum(coinfection == 1, na.rm = TRUE),
 ) %>% ungroup()
    
    
    
    coinfection_corr_week_o <- coinfection_corr %>%
      group_by( week_start) %>%
      summarize(
        sars_cov2_pos_co = sum(sars_cov2_final == 1, na.rm = TRUE),
        inf_a_pos_co = sum(inf_a_final == 1, na.rm = TRUE),
        inf_b_pos_co = sum(inf_b_final == 1, na.rm = TRUE),
        vsr_pos_co = sum(vsr_final == 1, na.rm = TRUE),
        conif = sum(coinfection == 1, na.rm = TRUE),
      )
    
    coinfection_corr_week_hosp$hospital <- ifelse(coinfection_corr_week_hosp$record_id_first_letter == "Q", "Hospital de Coatepeque",
                                                   ifelse(coinfection_corr_week_hosp$record_id_first_letter == "C", "Hospital de Chimaltenango", NA))
    coinfection_corr_week_hosp <- coinfection_corr_week_hosp %>%
    select(!record_id_first_letter)  
    
    coinfection_corr_week_o$hospital <- "Overall"

    all_coinfections <- rbind(coinfection_corr_week_hosp, coinfection_corr_week_o)
    
all_inc_symp <- rbind(symptom_incidence_o, symptom_incidence_chim)
all_inc_symp <- rbind(all_inc_symp, symptom_incidence_coa)

all_inc_symp <- left_join(all_inc_symp ,all_coinfections, by = c("week_start",   "hospital") )

all_inc_symp <- all_inc_symp %>% 
  mutate(
inf_a_perc = 100*inf_a_pos_co / total_tested,
inf_b_perc = 100*inf_b_pos_co / total_tested,
sars_cov2_perc = 100*sars_cov2_pos_co / total_tested,
vsr_perc = 100*vsr_pos_co / total_tested,
allpos = sum(inf_a_perc,inf_b_perc, sars_cov2_perc, vsr_perc),
allneg = 100- allpos )


write.csv(all_inc_symp, "docs/gihsn_incidence_week.csv")

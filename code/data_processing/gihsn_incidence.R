

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}
pacman::p_load(REDCapR, dplyr, tidyr, stringr, lubridate, zoo)

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
         both_swab_date = as.Date(both_swab_date),
         week_start = floor_date(both_swab_date, unit = "week", week_start = 7) 
  )



# Function to determine final result per record_id (preferring positive)
resolve_result <- function(main, repit) {
  case_when(
   main == 1 | repit == 1 ~ 1,  # Prefer positive
   main == 2 | repit == 2 ~ 2,  # Prefer negs
    TRUE ~ NA_real_
  )
}



# Apply function to all relevant columns and filter to keep rows with at least one non-NA result
gihsn <- gihsn %>%
  rowwise() %>%
  mutate(
    sars_cov2_final1 = resolve_result(sars_cov2, sars_cov2_repit),
    sars_cov2_final2 = resolve_result(sars_cov2_abi, sars_cov2_abi_2),
    sars_cov2_final = resolve_result(sars_cov2_final1, sars_cov2_final2),
    inf_a_final1 = resolve_result(inf_a, inf_a_repit),
    inf_a_final2 = resolve_result(inf_a_abi, inf_a_abi_2),
    inf_a_final = resolve_result(inf_a_final1, inf_a_final2),
    inf_b_final1 = resolve_result(inf_b, inf_b_repit),
    inf_b_final2 = resolve_result(inf_b_abi, inf_b_abi_2),
    inf_b_final = resolve_result(inf_b_final1, inf_b_final2),
    vsr_final = resolve_result(vsr, vsr_repit)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(sars_cov2_final) | 
      !is.na(inf_a_final) | 
      !is.na(inf_b_final) | 
      !is.na(vsr_final)
  )

table(gihsn$vsr_final,gihsn$sars_cov2_final,useNA = "always" )

table(gihsn$vsr_final,gihsn$inf_a_final,useNA = "always" )

# Group by week_start and count positives, negatives, and total tested
gihsn_results <- gihsn %>%
  mutate(record_id_first_letter = substr(record_id, 1, 1)) %>%  # Extract the first letter of record_id
  group_by(record_id_first_letter, week_start) %>%
  summarize(
    total_tested = n_distinct(record_id),  # Count unique IDs tested per week_start
    sars_cov2_pos = sum(sars_cov2_final == 1, na.rm = TRUE),
    inf_a_pos = sum(inf_a_final == 1, na.rm = TRUE),
    inf_b_pos = sum(inf_b_final == 1, na.rm = TRUE),
    vsr_pos = sum(vsr_final == 1, na.rm = TRUE),
    inf_a_h1n1 = sum(both_lab_flu_h1n1 == 1, na.rm = TRUE),
    inf_a_h3n2 = sum(both_lab_flu_h3n2 == 1, na.rm = TRUE),
    inf_a_nosub = sum(inf_a_final == 1 & both_lab_flu_a_nosub == 1, na.rm = TRUE),
    inf_a_other = sum(inf_a_final == 1 & inf_a_nosub == 0 & inf_a_h1n1 == 0& inf_a_h3n2 == 0,  na.rm = TRUE)) %>%
  arrange( week_start)  


# Fix hospital names 
gihsn_results$record_id_first_letter <- ifelse(gihsn_results$record_id_first_letter == "Q", "Hospital de Coatepeque",
                                               ifelse(gihsn_results$record_id_first_letter == "C", "Hospital de Chimaltenango", NA))


gihsn_results <- gihsn_results %>%
  rename(hospital = record_id_first_letter)


#El número de camas hospital de Chimaltenango: 232
#El número de camas hospital de Coatepeque: 152

chim <- gihsn_results %>% filter( hospital == "Hospital de Chimaltenango")
coa <- gihsn_results %>% filter( hospital == "Hospital de Coatepeque")

denom_chim <- 232
denom_coa  <- 152
denom_o <- 152 + 232

chim$denom <- denom_chim
inc_vars <- c("denom", "total_tested", "inf_a_pos", "inf_b_pos", "sars_cov2_pos", "vsr_pos",                              
              "inf_a_h1n1", "inf_a_h3n2", "inf_a_nosub", "inf_a_other")



# calculate rolling averages and incidence based on estimated bed capacity 
roll3 <- function(x) rollapply(x, width = 3, FUN = sum, partial = TRUE, align = "center")

chim <- chim %>%
  arrange(week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(across(all_of(inc_vars), ~ 1000 * .x / denom, .names = "{.col}_inc")) %>%
  mutate(across(all_of(paste0(setdiff(inc_vars, "denom"), "_roll")),
                ~ 1000 * .x / denom_roll,
                .names = "{sub('_roll$', '_inc_roll', .col)}"))
coa$denom <- denom_coa

coa <- coa %>%
  arrange( week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
mutate(across(all_of(inc_vars), ~ 1000 * .x / denom, .names = "{.col}_inc")) %>%
  mutate(across(all_of(paste0(setdiff(inc_vars, "denom"), "_roll")),
                ~ 1000 * .x / denom_roll,
                .names = "{sub('_roll$', '_inc_roll', .col)}"))


overall <- gihsn_results %>% group_by(week_start) %>% summarise(
  total_tested = sum(total_tested),
  inf_a_pos = sum(inf_a_pos),
  inf_b_pos = sum(inf_b_pos),
  sars_cov2_pos = sum(sars_cov2_pos),
  vsr_pos = sum(vsr_pos),
  inf_a_h1n1 = sum(inf_a_h1n1),
  inf_a_h3n2 = sum(inf_a_h3n2),
  inf_a_nosub = sum(inf_a_nosub),
  inf_a_other = sum(inf_a_other))


overall$denom <- denom_o

overall <- overall %>%
  arrange(week_start) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(across(all_of(inc_vars), ~ 1000 * .x / denom, .names = "{.col}_inc")) %>%
  mutate(across(all_of(paste0(setdiff(inc_vars, "denom"), "_roll")),
                ~ 1000 * .x / denom_roll,
                .names = "{sub('_roll$', '_inc_roll', .col)}"))

overall$hospital <- "Overall"



overall <- overall %>% dplyr::select(week_start, hospital, all_of(inc_vars), ends_with("_inc"), ends_with("_roll"))

chim <- chim  %>% dplyr::select(week_start, hospital, all_of(inc_vars), ends_with("_inc"), ends_with("_roll"))

coa <- coa %>% dplyr::select(week_start, hospital, all_of(inc_vars), ends_with("_inc"), ends_with("_roll"))

all_inc1 <- rbind(chim, coa) 
all_inc <- rbind(all_inc1, overall)
### FINAL OVERALL INC DATAFRAME 



#### Symptoms  

gihsn_symptom_vars <- c("both_ilisystemic_malaise", "both_ilisystemic_headache",
                       "both_ilisystemic_myalgia", "both_ilirespiratory_cough",
                       "both_ilirespiratory_sorethroat", "both_ilirespiratory_shortnessbreath",
                       "both_ilirespiratory_wheezing","both_ilirespiratory_nasalcongestion",
                        "both_othersymptoms_nausea", "both_othersymptoms_diarrhea",
                       "both_othersymptoms_smell", "both_othersymptoms_chestpain")

#OVERALL 

###create symptom variables by week and make sure all weeks are accounted for 
symptom_weekly <- gihsn %>%
  group_by(week_start) %>%
  summarise(
    across(all_of(gihsn_symptom_vars), ~ sum(.x == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(gihsn_symptom_vars)), gihsn_symptom_vars)))

###Join with other dataset to get denoms 
symptom_incidence_o <- symptom_weekly %>%
  left_join(overall, by = "week_start") 

##create raw incidence calculations 
symptom_incidence_o <- symptom_incidence_o %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

##create rolling calculations 
symptom_incidence_o <- symptom_incidence_o %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(across(all_of(paste0(gihsn_symptom_vars, "_roll")),
                ~ 1000 * .x / denom_roll,
                .names = "{sub('_roll$', '_inc_roll', .col)}"))

##keep needed vars  
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


symptom_incidence_hosp <- symptom_weekly %>%
  left_join(all_inc1, by = c("week_start", "hospital") )

chim <- symptom_incidence_hosp %>% filter( hospital == "Hospital de Chimaltenango")
coa <- symptom_incidence_hosp %>% filter( hospital == "Hospital de Coatepeque")

symptom_incidence_chim <- chim %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

symptom_incidence_chim <- symptom_incidence_chim %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(across(all_of(paste0(gihsn_symptom_vars, "_roll")),
                ~ 1000 * .x / denom_roll,
                .names = "{sub('_roll$', '_inc_roll', .col)}"))
symptom_incidence_chim <- symptom_incidence_chim %>%
  select(
    week_start, hospital,
    all_of(inc_vars), all_of(gihsn_symptom_vars),
    ends_with("_inc"), ends_with("_roll"))
  
    symptom_incidence_coa <- coa %>%
  mutate(across(all_of(gihsn_symptom_vars), ~ 1000 * .x / denom, .names = "{.col}_inc"))

    symptom_incidence_coa <- symptom_incidence_coa %>%
  arrange(week_start) %>%
  mutate(across(all_of(gihsn_symptom_vars), roll3, .names = "{.col}_roll")) %>%
      mutate(across(all_of(paste0(gihsn_symptom_vars, "_roll")),
                    ~ 1000 * .x / denom_roll,
                    .names = "{sub('_roll$', '_inc_roll', .col)}"))
symptom_incidence_coa <- symptom_incidence_coa %>%
  select(
    week_start, hospital,
    all_of(inc_vars), all_of(gihsn_symptom_vars),
    ends_with("_inc"), ends_with("_roll"))
    
### coinfection and percent pos 
   coinfection_corr <- gihsn %>% mutate(
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
coinfec_perc = 100*conif/ total_tested,
allpos = inf_a_perc + inf_b_perc + sars_cov2_perc + vsr_perc + coinfec_perc ,
allneg = 100- allpos )

write.csv(all_inc_symp, "docs/gihsn_incidence_week.csv")





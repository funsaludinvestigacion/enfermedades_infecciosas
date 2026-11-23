# Load libraries -----------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}
pacman::p_load(REDCapR, dplyr, tidyr, stringr, lubridate, zoo)

# Load data -----------------------------------------------------------------
vigicasa_token <- Sys.getenv("vigicasa_token")

uri <- "https://redcap.ucdenver.edu/api/"

vigicasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = vigicasa_token
  )$data

# Processing -----------------------------------------------------------------

##date from epi form (respiratory)
vigicasa$f_muestra <- ymd(vigicasa$f_muestra)

##date from epi form (dengue)
vigicasa$fech_tom <- ymd(vigicasa$fech_tom)

##date from surveillance form 
vigicasa$fecha_vigilancia <- ymd(vigicasa$fecha_vigilancia)
vigicasa$month <-  month(vigicasa$fecha_vigilancia)

### set week start date for both RedCAP forms 
vigicasa$week_start_test <- floor_date(vigicasa$f_muestra, unit = "week", week_start = 7)  
vigicasa$week_start_surv <- floor_date(vigicasa$fecha_vigilancia, unit = "week", week_start = 7)  
vigicasa$week_start <- floor_date(vigicasa$fecha_vigilancia, unit = "week", week_start = 7)  


#### records by week (surveillance denominator starting in 2026)
vigicasa_record_week <- vigicasa %>% filter(vigilancia_realizada == 1) %>% 
  group_by(record_id, week_start) %>% 
  summarise(surv_per_week = n())                                                             

#### records by week, make sure all weeks are available and fill historic data 
### with estimated number of participants - 250 

hist_participants <- 250

counts_weekly <- vigicasa_record_week %>% group_by( week_start)  %>% 
  summarise(surveilled = n()) %>% 
  tidyr::complete(
    week_start = seq(as.Date("2025-06-15"), as.Date("2026-01-10"), by = "week"),
    fill = list(surveilled = hist_participants)
  ) %>% filter(week_start > "2025-01-01")

###fix the partial week 
counts_weekly$surveilled <- ifelse(counts_weekly$week_start == "2026-01-04", 
                                   hist_participants, counts_weekly$surveilled )


#### Create surveilled dataset, this is each occurance someone was surveilled starting in Jan 2026 with the surveillance form
realizada <- vigicasa %>% filter(vigilancia_realizada == 1)

##### TESTED (RESP), isolates tested each week, goes back in time before surveillance form was in use
tested <- vigicasa %>%
  filter(!is.na(f_muestra))

#### count number of TESTS each week, also equal to the number of participants with ARI
tests_weekly <- tested %>% group_by( week_start_test)  %>% 
  summarise(tested = n()) 


### RESP RESULTS; isolate tested participants and identify relevant demographics, 
### and create result summary 
resp_results <- tested %>%
  mutate(
    week_start =  floor_date(f_muestra, unit = "week", week_start = 7)) %>% 
  group_by(record_id, week_start, f_muestra) %>%
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
    inf_a_h1n1 = sum(subtipo_infa == "H1N1", na.rm = TRUE),
    inf_a_h3n2 = sum(subtipo_infa == "H3N2", na.rm = TRUE),
    inf_a_nosub = sum(virus_detectado___2 == 1 & is.na(subtipo_infa), na.rm = TRUE),
    month = month(f_muestra),
    .groups = "drop"
  ) 

###weekly resp results (overall, no stratification)
resp_weekly <- resp_results %>% group_by( week_start) %>%
  summarise(
    total_tested = sum(total_tested),
    total_pos    = sum(total_pos),
    total_neg    = sum(total_neg),
    sars_cov2_pos = sum(sars_cov2_pos),
    sars_cov2_neg = sum(sars_cov2_neg),
    inf_a_pos    = sum(inf_a_pos),
    inf_a_neg    = sum(inf_a_neg),
    inf_b_pos    = sum(inf_b_pos),
    inf_b_neg    = sum(inf_b_neg),
    vsr_pos      = sum(vsr_pos),
    vsr_neg      = sum(vsr_neg),
    inf_a_h1n1   = sum(inf_a_h1n1),   
    inf_a_h3n2   = sum(inf_a_h3n2),  
    inf_a_nosub = sum(inf_a_nosub),
    .groups = "drop"
  )

### fix the data so we can merge things with the resp dataset by week_start
tests_weekly$week_start <- tests_weekly$week_start_test

### join surveilled and tested counts
resp_incidence_w <- left_join(resp_weekly, counts_weekly)
resp_incidence_w <- left_join(resp_incidence_w, tests_weekly)

##calculate incidence values 
resp_incidence_w$ari_inc     <- 1000 * resp_incidence_w$tested       / resp_incidence_w$surveilled
resp_incidence_w$flua_inc     <- 1000 * resp_incidence_w$inf_a_pos    / resp_incidence_w$surveilled
resp_incidence_w$flub_inc     <- 1000 * resp_incidence_w$inf_b_pos    / resp_incidence_w$surveilled
resp_incidence_w$scv2_inc     <- 1000 * resp_incidence_w$sars_cov2_pos / resp_incidence_w$surveilled
resp_incidence_w$rsv_inc      <- 1000 * resp_incidence_w$vsr_pos      / resp_incidence_w$surveilled
resp_incidence_w$flu_gen_inc  <- 1000 * (resp_incidence_w$inf_a_pos + resp_incidence_w$inf_b_pos) / resp_incidence_w$surveilled
resp_incidence_w$h1n1_inc     <- 1000 * resp_incidence_w$inf_a_h1n1  / resp_incidence_w$surveilled  
resp_incidence_w$h3n2_inc     <- 1000 * resp_incidence_w$inf_a_h3n2  / resp_incidence_w$surveilled  

# create list of variables to roll on and create incidence variables for later on 
inc_vars <- c("surveilled", "tested", "inf_a_pos", "inf_b_pos",
              "sars_cov2_pos", "vsr_pos", "inf_a_h1n1", "inf_a_h3n2")

### used this instead of rollsum to create roll sums that allow for partial weeks at edges of dataset
roll3 <- function(x) rollapply(x, width = 3, FUN = sum,
                               partial = TRUE, align = "center")

resp_incidence_w <- resp_incidence_w %>%
  arrange(week_start) %>%                      
  mutate(
    ###creates all the roll sum variables needed to calculate rolling averages
  across(all_of(inc_vars), roll3, .names = "{.col}_roll"),
  ###create incidence variables after roll
    ari_inc_roll     = 1000 * tested_roll        / surveilled_roll,
    flua_inc_roll    = 1000 * inf_a_pos_roll     / surveilled_roll,
    flub_inc_roll    = 1000 * inf_b_pos_roll     / surveilled_roll,
    scv2_inc_roll    = 1000 * sars_cov2_pos_roll / surveilled_roll,
    rsv_inc_roll     = 1000 * vsr_pos_roll       / surveilled_roll,
    flu_gen_inc_roll = 1000 * (inf_a_pos_roll + inf_b_pos_roll) / surveilled_roll,
    h1n1_inc_roll    = 1000 * inf_a_h1n1_roll    / surveilled_roll,
    h3n2_inc_roll    = 1000 * inf_a_h3n2_roll    / surveilled_roll) %>%
    select(week_start, all_of(inc_vars),
         ari_inc, ari_inc_roll,
         flu_gen_inc, flu_gen_inc_roll,
         flua_inc, flua_inc_roll,
         flub_inc, flub_inc_roll,
         scv2_inc, scv2_inc_roll,
         rsv_inc, rsv_inc_roll,
         h1n1_inc, h1n1_inc_roll,
         h3n2_inc, h3n2_inc_roll)


##########correct for coinfections for the stacked bar graph
resp_results_coinfection_corr <- resp_results %>% mutate(
  coinfection =  ifelse( inf_a_pos == 1 & 
                        (inf_b_pos == 1 | vsr_pos == 1 |sars_cov2_pos ==1),1,
                         ifelse( inf_b_pos == 1 & 
                         (inf_a_pos == 1 | vsr_pos == 1 |sars_cov2_pos ==1),1,
                         ifelse( vsr_pos == 1 & 
                          (inf_a_pos == 1 | inf_b_pos == 1 |sars_cov2_pos ==1),1,
                          ifelse( sars_cov2_pos == 1 & 
                        (inf_a_pos == 1 | inf_b_pos == 1 |vsr_pos ==1),1, 0)))))

resp_results_coinfection_corr$inf_a_pos <- ifelse(resp_results_coinfection_corr$coinfection == 1, 0,resp_results_coinfection_corr$inf_a_pos)                                   
resp_results_coinfection_corr$inf_b_pos <- ifelse(resp_results_coinfection_corr$coinfection == 1, 0,resp_results_coinfection_corr$inf_b_pos)                                   
resp_results_coinfection_corr$sars_cov2_pos <- ifelse(resp_results_coinfection_corr$coinfection == 1, 0,resp_results_coinfection_corr$sars_cov2_pos)                                   
resp_results_coinfection_corr$vsr_pos <- ifelse(resp_results_coinfection_corr$coinfection == 1, 0,resp_results_coinfection_corr$vsr_pos)                                   

###weekly tests and infections by week with the coinfection correction
resp_weekly_coinf <- resp_results_coinfection_corr %>% group_by(week_start) %>%
  summarise(
    tested = sum(total_tested),
    total_pos    = sum(total_pos),
    total_neg    = sum(total_neg),
    sars_cov2_pos = sum(sars_cov2_pos),
    sars_cov2_neg = sum(sars_cov2_neg),
    inf_a_pos    = sum(inf_a_pos),
    inf_a_neg    = sum(inf_a_neg),
    inf_b_pos    = sum(inf_b_pos),
    inf_b_neg    = sum(inf_b_neg),
    vsr_pos      = sum(vsr_pos),
    vsr_neg      = sum(vsr_neg),
    inf_a_h1n1   = sum(inf_a_h1n1),   # added
    inf_a_h3n2   = sum(inf_a_h3n2),   # added
    inf_a_nosub = sum(inf_a_nosub),
    coinfec = sum(coinfection),
    .groups = "drop"
  )

### overall weekly metrics and incidence
write.csv(resp_incidence_w, "docs/vigicasa_resp_weekly.csv") 

### overall weekly metrics corrected for coinfections  
write.csv(resp_weekly_coinf, "docs/vigicasa_resp_weekly_coinf.csv") 


####Create demographic-grouped incidence calculations

### Age group breaks and labels
age_breaks  <- c(0, 5, 25, 50, 65, Inf)
age_labels  <- c("0-4", "5-24", "25-49", "50-64", "65+")

### calculate surveilled denominator - 
###we don't have estimates for this before form is used so data only available beginning in 2026
realizada_age <- realizada %>%
  mutate(
    age      = edad,
    age_grp  = cut(age, breaks = age_breaks, labels = age_labels,
                   right = FALSE, include.lowest = TRUE),
    week_start = week_start_surv,
  ) 


##### Pull sex variable from earlier form and join
####back by participant id to the surveillance form
sex <- vigicasa %>% mutate(sex = sexo)%>%
  filter(!is.na(sexo)) %>%
  group_by(record_id, sex) %>% tally() 


realizada_age_sex <- left_join(realizada_age, sex)


#surveilled counts per week per age group 
surv_age_week <- realizada_age_sex %>%
  group_by(record_id, week_start, age_grp) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start, age_grp) %>%
  summarise(surveilled = n(), .groups = "drop")

#surveilled counts per week by sex 
surv_sex_week <- realizada_age_sex %>%
  group_by(record_id, week_start, sex) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start, sex) %>%
  summarise(surveilled = n(), .groups = "drop")

### filter to the tested form using f_muestra, also calculate age and age group.
### sex is available in this form. 
resp_results_age_sex <- vigicasa %>%
  filter(
    !is.na(f_muestra) 
  ) %>%
  mutate(
    week_start  = floor_date(f_muestra, unit = "week", week_start = 7),
    age         = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    age_grp     = cut(age, breaks = age_breaks, labels = age_labels,
                      right = FALSE, include.lowest = TRUE),
    year        = year(f_muestra),
    sex       = sexo_paciente
  )


##counts by age groups 
resp_weekly_age <- resp_results_age_sex %>%
  group_by(week_start, age_grp) %>%
  summarise(
    tested    = n_distinct(record_id),
    sars_cov2_pos   = sum(virus_detectado___4 == 1, na.rm = TRUE),
    inf_a_pos       = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_b_pos       = sum(virus_detectado___3 == 1, na.rm = TRUE),
    vsr_pos         = sum(virus_detectado___5 == 1, na.rm = TRUE),
    inf_a_h1n1      = sum(subtipo_infa == "H1N1",   na.rm = TRUE),
    inf_a_h3n2      = sum(subtipo_infa == "H3N2",   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #### fill in missing weeks where no one from that age grou was tested 
  complete(
    week_start, age_grp,
    fill = list(
      tested  = 0,
      sars_cov2_pos = 0,
      inf_a_pos     = 0,
      inf_b_pos     = 0,
      vsr_pos       = 0,
      inf_a_h1n1    = 0,
      inf_a_h3n2    = 0
    )
  ) %>%
  mutate(year = lubridate::year(week_start))
resp_weekly_age <- resp_weekly_age %>% filter(week_start >"2026-01-05")


resp_weekly_sex <- resp_results_age_sex %>%
  group_by(week_start, sex) %>%
  summarise(
    tested    = n_distinct(record_id),
    sars_cov2_pos   = sum(virus_detectado___4 == 1, na.rm = TRUE),
    inf_a_pos       = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_b_pos       = sum(virus_detectado___3 == 1, na.rm = TRUE),
    vsr_pos         = sum(virus_detectado___5 == 1, na.rm = TRUE),
    inf_a_h1n1      = sum(subtipo_infa == "H1N1",   na.rm = TRUE),
    inf_a_h3n2      = sum(subtipo_infa == "H3N2",   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    week_start, sex,
    fill = list(
      tested  = 0,
      sars_cov2_pos = 0,
      inf_a_pos     = 0,
      inf_b_pos     = 0,
      vsr_pos       = 0,
      inf_a_h1n1    = 0,
      inf_a_h3n2    = 0
    )
  ) %>%
  mutate(year = lubridate::year(week_start)) %>%
  relocate(year, .before = week_start)
resp_weekly_sex <- resp_weekly_sex %>% filter(week_start >"2026-01-05")

inc_vars <- c("surveilled", "tested", "inf_a_pos", "inf_b_pos",
              "sars_cov2_pos", "vsr_pos", "inf_a_h1n1", "inf_a_h3n2")
###Join with the surveilled counts (cant do all together because they are on different redcap forms )
resp_incidence_age <- resp_weekly_age %>%
  left_join(surv_age_week,   by = c("week_start", "age_grp")) 

resp_incidence_sex <- resp_weekly_sex %>%
  left_join(surv_sex_week,   by = c("week_start", "sex")) 

###incidence rates 

### uses the same roll 3 and inc vars as before 

resp_incidence_age <- resp_incidence_age %>%
  arrange(age_grp, week_start) %>%
  group_by(age_grp) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    ari_inc     = 1000 * tested        / surveilled,
    flua_inc    = 1000 * inf_a_pos     / surveilled,
    flub_inc    = 1000 * inf_b_pos     / surveilled,
    scv2_inc    = 1000 * sars_cov2_pos / surveilled,
    rsv_inc     = 1000 * vsr_pos       / surveilled,
    flu_gen_inc = 1000 * (inf_a_pos + inf_b_pos) / surveilled,
    h1n1_inc    = 1000 * inf_a_h1n1   / surveilled,
    h3n2_inc    = 1000 * inf_a_h3n2   / surveilled,
    
    ari_inc_roll      = 1000 * tested_roll        / surveilled_roll,
    flua_inc_roll     = 1000 * inf_a_pos_roll     / surveilled_roll,
    flub_inc_roll     = 1000 * inf_b_pos_roll     / surveilled_roll,
    scv2_inc_roll     = 1000 * sars_cov2_pos_roll / surveilled_roll,
    rsv_inc_roll      = 1000 * vsr_pos_roll       / surveilled_roll,
    flu_gen_inc_roll  = 1000 * (inf_a_pos_roll + inf_b_pos_roll) / surveilled_roll,
    h1n1_inc_roll     = 1000 * inf_a_h1n1_roll    / surveilled_roll,
    h3n2_inc_roll     = 1000 * inf_a_h3n2_roll    / surveilled_roll
  ) %>%
  ungroup() 


resp_incidence_sex <- resp_incidence_sex %>%
  arrange(sex, week_start) %>%
  group_by(sex) %>%
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    ari_inc      = 1000 * tested        / surveilled,
    flua_inc     = 1000 * inf_a_pos     / surveilled,
    flub_inc     = 1000 * inf_b_pos     / surveilled,
    scv2_inc     = 1000 * sars_cov2_pos / surveilled,
    rsv_inc      = 1000 * vsr_pos       / surveilled,
    flu_gen_inc  = 1000 * (inf_a_pos + inf_b_pos) / surveilled,
    h1n1_inc     = 1000 * inf_a_h1n1    / surveilled,
    h3n2_inc     = 1000 * inf_a_h3n2    / surveilled,
    
    ari_inc_roll      = 1000 * tested_roll        / surveilled_roll,
    flua_inc_roll     = 1000 * inf_a_pos_roll     / surveilled_roll,
    flub_inc_roll     = 1000 * inf_b_pos_roll     / surveilled_roll,
    scv2_inc_roll     = 1000 * sars_cov2_pos_roll / surveilled_roll,
    rsv_inc_roll      = 1000 * vsr_pos_roll       / surveilled_roll,
    flu_gen_inc_roll  = 1000 * (inf_a_pos_roll + inf_b_pos_roll) / surveilled_roll,
    h1n1_inc_roll     = 1000 * inf_a_h1n1_roll    / surveilled_roll,
    h3n2_inc_roll     = 1000 * inf_a_h3n2_roll    / surveilled_roll
  ) %>%
  ungroup() 

resp_incidence_sex$sex <- ifelse(resp_incidence_sex$sex == 1, "Male", "Female")

# ──  surveilled persons by week & municipio ──────────────────
## municipio is collected in three different places and none of them are complete so we must compile
### order in likelyhood of most recent -> resp form, deng form, enroll form
municipios_p <- vigicasa %>%
  filter(!is.na(municipio_p)) %>%
  mutate(municipio_recent_1 = municipio_p) %>%
  arrange(record_id, desc(f_visita_f)) %>%
  distinct(record_id, .keep_all = TRUE) %>% dplyr::select(record_id, municipio_recent_1 )

municipios_d <- vigicasa %>%
  filter(!is.na(municipio_d)) %>%
  mutate(municipio_recent_2 = municipio_d) %>%
  arrange(record_id, desc(f_visita_f)) %>%
  distinct(record_id, .keep_all = TRUE) %>% dplyr::select(record_id, municipio_recent_2 )


municipios_enroll <- vigicasa %>%
  filter(!is.na(municipio)) %>%
  mutate(municipio_recent_3 = municipio) %>%
  arrange(record_id, desc(f_visita_f)) %>%
  distinct(record_id, .keep_all = TRUE) %>% dplyr::select(record_id, municipio_recent_3 )


municipios1 <- full_join(municipios_p, municipios_d)

municipios <- full_join(municipios1, municipios_enroll )

municipios$municipio_recent <- (ifelse(is.na(municipios$municipio_recent_1) & 
                                         is.na(municipios$municipio_recent_2),municipios$municipio_recent_3, 
                                       ifelse(is.na(municipios$municipio_recent_1),municipios$municipio_recent_2 ,
                                              municipios$municipio_recent_1 )))

#### Join lists of municipios with surveilled dataset

realizada1 <- left_join(realizada, municipios)

#### Persons surveilled per week per municipio 
surv_muni_week <- realizada1 %>%
  group_by(record_id, week_start, municipio_recent) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start, municipio_recent) %>%
  summarise(surveilled = n(), .groups = "drop")

#### Tested & positive counts by week and municipio

vigicasa2 <- left_join(vigicasa, municipios)


resp_weekly_muni <- vigicasa2 %>%
  group_by(year, week_start, municipio_recent) %>%
  summarise(
    total_tested    = n_distinct(record_id),
    sars_cov2_pos   = sum(virus_detectado___4 == 1, na.rm = TRUE),
    inf_a_pos       = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_b_pos       = sum(virus_detectado___3 == 1, na.rm = TRUE),
    vsr_pos         = sum(virus_detectado___5 == 1, na.rm = TRUE),
    inf_a_h1n1      = sum(subtipo_infa == "H1N1",   na.rm = TRUE),
    inf_a_h3n2      = sum(subtipo_infa == "H3N2",   na.rm = TRUE),
    .groups = "drop"
  )

tested_muni_week <- vigicasa2 %>%
  filter(!is.na(f_muestra)) %>%
  mutate(
    week_start = floor_date(f_muestra, unit = "week", week_start = 7)
  ) %>%
  group_by(record_id, week_start, municipio_recent) %>%
  summarise(tests = n(), .groups = "drop") %>%
  group_by(week_start, municipio_recent) %>%
  summarise(tested = n(), .groups = "drop")

resp_incidence_muni <- resp_weekly_muni %>%
  left_join(surv_muni_week,   by = c("week_start", "municipio_recent")) %>%
  left_join(tested_muni_week, by = c("week_start", "municipio_recent"))

###calculate raw incidence and rolled incidence 


resp_incidence_muni <- resp_incidence_muni %>%
  arrange(municipio_recent, week_start) %>%
  group_by(municipio_recent) %>% 
  mutate(across(all_of(inc_vars), roll3, .names = "{.col}_roll")) %>%
  mutate(
    ari_inc      = 1000 * tested        / surveilled,
    flua_inc     = 1000 * inf_a_pos     / surveilled,
    flub_inc     = 1000 * inf_b_pos     / surveilled,
    scv2_inc     = 1000 * sars_cov2_pos / surveilled,
    rsv_inc      = 1000 * vsr_pos       / surveilled,
    flu_gen_inc  = 1000 * (inf_a_pos + inf_b_pos) / surveilled,
    h1n1_inc     = 1000 * inf_a_h1n1    / surveilled,
    h3n2_inc     = 1000 * inf_a_h3n2    / surveilled,
    
    ari_inc_roll      = 1000 * tested_roll        / surveilled_roll,
    flua_inc_roll     = 1000 * inf_a_pos_roll     / surveilled_roll,
    flub_inc_roll     = 1000 * inf_b_pos_roll     / surveilled_roll,
    scv2_inc_roll     = 1000 * sars_cov2_pos_roll / surveilled_roll,
    rsv_inc_roll      = 1000 * vsr_pos_roll       / surveilled_roll,
    flu_gen_inc_roll  = 1000 * (inf_a_pos_roll + inf_b_pos_roll) / surveilled_roll,
    h1n1_inc_roll     = 1000 * inf_a_h1n1_roll    / surveilled_roll,
    h3n2_inc_roll     = 1000 * inf_a_h3n2_roll    / surveilled_roll
  ) %>%
  ungroup() 

### only have accurate denominators for muni incidence in 2026
resp_incidence_muni <- resp_incidence_muni %>% filter(as.Date(week_start) > "2026-01-17")


write.csv(resp_incidence_muni, "docs/vigicasa_resp_incidence_muni.csv")

##### tested dengue 

vigicasa_deng <- vigicasa %>%
  filter(!is.na(fech_tom))

vigicasa_deng$deng_pos <- ifelse(vigicasa_deng$p_ns1 == 1 |  
                                   vigicasa_deng$p_pcr == 1 |
                                   vigicasa_deng$p_igm == 1 , 1, 0)

##### sample test week
vigicasa_deng$week_start_test <- floor_date(vigicasa_deng$fech_tom, unit = "week", week_start = 7) 

deng_tests_weekly <- vigicasa_deng %>% group_by( week_start_test)  %>% 
  summarise(tested = n()) 




deng_results <- vigicasa_deng  %>%
  mutate(
    week_start =  floor_date(fech_tom, unit = "week", week_start = 7), # changed to fecha_vigilancia
    year = year(fech_tom)
  ) %>%
  group_by(record_id, year,  week_start, fech_tom) %>%
  summarize(
    total_tested = n_distinct(record_id),
    ns1_pos = n_distinct(record_id[p_ns1 == 1 ], na.rm = TRUE),
    ns1_neg = n_distinct(record_id[p_ns1 == 2], na.rm = TRUE),   
    igg_pos = n_distinct(record_id[p_igg == 1], na.rm = TRUE),
    igg_neg = n_distinct(record_id[p_igg == 2], na.rm = TRUE),
    igm_pos = n_distinct(record_id[p_igm == 1], na.rm = TRUE),
    igm_neg = n_distinct(record_id[p_igm == 2], na.rm = TRUE),
    pcr_pos = n_distinct(record_id[p_pcr == 1], na.rm = TRUE),
    pcr_neg = n_distinct(record_id[p_pcr == 2], na.rm = TRUE),
    serot_dengue_1_pos = n_distinct(record_id[serot_dengue == 1], na.rm = TRUE),
    serot_dengue_2_pos = n_distinct(record_id[serot_dengue == 2], na.rm = TRUE),
    serot_dengue_3_pos = n_distinct(record_id[serot_dengue == 3], na.rm = TRUE),
    serot_dengue_4_pos = n_distinct(record_id[serot_dengue == 4], na.rm = TRUE),
    serot_dengue_na = n_distinct(record_id[serot_dengue == 5], na.rm = TRUE),
    concl_dengue = n_distinct(record_id[conclusi_n_caso == 1], na.rm = TRUE),
    concl_dengue_sin = n_distinct(record_id[tip_dengue == 1], na.rm = TRUE),
    concl_dengue_con = n_distinct(record_id[tip_dengue == 2], na.rm = TRUE),
    concl_dengue_grave = n_distinct(record_id[tip_dengue == 3], na.rm = TRUE),
    deng_pos = sum(deng_pos),
    month = month(fech_tom),
    .groups = "drop"
  ) 

deng_results_weekly <- deng_results %>% group_by(year, week_start) %>%
  summarise(
    total_tested_deng = sum(total_tested),
    ns1_pos    = sum(ns1_pos),
    deng_pos   = sum(deng_pos),
    ns1_neg    = sum(ns1_neg),
    igg_pos = sum(igg_pos),
    igg_neg = sum(igg_neg),
    igm_pos    = sum(igm_pos),
    igm_neg    = sum(igm_neg),
    pcr_pos    = sum(pcr_pos),
    pcr_neg    = sum(pcr_neg),
    serot_dengue_1_pos = sum(serot_dengue_1_pos),
    serot_dengue_2_pos = sum(serot_dengue_2_pos),
    serot_dengue_3_pos = sum(serot_dengue_3_pos),  
    serot_dengue_4_pos = sum(serot_dengue_4_pos),   
    concl_dengue = sum(concl_dengue),
    concl_dengue_sin = sum(concl_dengue_sin),
    concl_dengue_con = sum(concl_dengue_con),
    concl_dengue_grave = sum(concl_dengue_grave),
    .groups = "drop"
  )


deng_tests_weekly$week_start <- deng_tests_weekly$week_start_test
deng_tests_weekly$tested_deng <- deng_tests_weekly$tested
deng_incidence_w <- left_join(deng_results_weekly, counts_weekly)
deng_incidence_w <- left_join(deng_incidence_w, deng_tests_weekly)

deng_incidence_w$ali_inc      <- 1000 * deng_incidence_w$tested_deng  / deng_incidence_w$surveilled
deng_incidence_w$deng_inc     <- 1000 * deng_incidence_w$deng_pos    / deng_incidence_w$surveilled


#### Roll SUMS 

deng_incidence_w <- deng_incidence_w %>% 
  mutate(surveilled_roll = roll3(surveilled),
         tested_deng_roll = roll3(tested_deng),
         deng_roll = roll3(igm_pos))

deng_incidence_w$ali_inc_roll         <- 1000 * deng_incidence_w$tested_deng_roll  / deng_incidence_w$surveilled_roll
deng_incidence_w$deng_inc_roll        <- 1000 * deng_incidence_w$deng_roll   / deng_incidence_w$surveilled_roll

deng_incidence_w <- deng_incidence_w %>% rename(ali = tested_deng,
                                                surveilled_deng = surveilled
) %>% dplyr::select(!c(tested))

resp_incidence_w <-  left_join(resp_incidence_w, deng_incidence_w)
resp_incidence_w <- resp_incidence_w %>% 
  dplyr::select(week_start, surveilled, tested, inf_a_pos,
                inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,  
                ari_inc, ari_inc_roll,
                flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                flub_inc, flub_inc_roll, scv2_inc, scv2_inc_roll, 
                rsv_inc, rsv_inc_roll,
                h1n1_inc, h1n1_inc_roll, h3n2_inc, h3n2_inc_roll,
                total_tested_deng, pcr_pos, igm_pos, ns1_pos, ali,
                ali_inc, ali_inc_roll, deng_roll,deng_inc, deng_inc_roll)
write.csv(resp_incidence_w, "docs/vigicasa_resp_weekly.csv")


##### Dengue results by age group #####
###use same age breaks as respiratory



deng_results_age <- vigicasa_deng %>%
  mutate(
    week_start = floor_date(fech_tom, unit = "week", week_start = 7),
    age        = edad_2,
    age_grp    = cut(age, breaks = age_breaks, labels = age_labels,
                     right = FALSE, include.lowest = TRUE),
    year       = year(fech_tom)
  ) %>%
  group_by(record_id, week_start, age_grp, fech_tom) %>%
  summarize(
    total_tested       = n_distinct(record_id),
    deng_pos = sum(deng_pos),
    ns1_pos            = n_distinct(record_id[p_ns1 == 1], na.rm = TRUE),
    ns1_neg            = n_distinct(record_id[p_ns1 == 2], na.rm = TRUE),
    igg_pos            = n_distinct(record_id[p_igg == 1], na.rm = TRUE),
    igg_neg            = n_distinct(record_id[p_igg == 2], na.rm = TRUE),
    igm_pos            = n_distinct(record_id[p_igm == 1], na.rm = TRUE),
    igm_neg            = n_distinct(record_id[p_igm == 2], na.rm = TRUE),
    pcr_pos            = n_distinct(record_id[p_pcr == 1], na.rm = TRUE),
    pcr_neg            = n_distinct(record_id[p_pcr == 2], na.rm = TRUE),
    serot_dengue_1_pos = n_distinct(record_id[serot_dengue == 1], na.rm = TRUE),
    serot_dengue_2_pos = n_distinct(record_id[serot_dengue == 2], na.rm = TRUE),
    serot_dengue_3_pos = n_distinct(record_id[serot_dengue == 3], na.rm = TRUE),
    serot_dengue_4_pos = n_distinct(record_id[serot_dengue == 4], na.rm = TRUE),
    serot_dengue_na    = n_distinct(record_id[serot_dengue == 5], na.rm = TRUE),
    concl_dengue       = n_distinct(record_id[conclusi_n_caso == 1], na.rm = TRUE),
    concl_dengue_sin   = n_distinct(record_id[tip_dengue == 1], na.rm = TRUE),
    concl_dengue_con   = n_distinct(record_id[tip_dengue == 2], na.rm = TRUE),
    concl_dengue_grave = n_distinct(record_id[tip_dengue == 3], na.rm = TRUE),
    .groups = "drop"
  )


deng_results_sex <- vigicasa_deng %>%
  mutate(
    week_start = floor_date(fech_tom, unit = "week", week_start = 7),
    sex        = sexo_2,
    year       = year(fech_tom)
  ) %>%
  group_by(record_id, week_start, sex, fech_tom) %>%
  summarize(
    total_tested       = n_distinct(record_id),
    deng_pos = sum(deng_pos),
    ns1_pos            = n_distinct(record_id[p_ns1 == 1], na.rm = TRUE),
    ns1_neg            = n_distinct(record_id[p_ns1 == 2], na.rm = TRUE),
    igg_pos            = n_distinct(record_id[p_igg == 1], na.rm = TRUE),
    igg_neg            = n_distinct(record_id[p_igg == 2], na.rm = TRUE),
    igm_pos            = n_distinct(record_id[p_igm == 1], na.rm = TRUE),
    igm_neg            = n_distinct(record_id[p_igm == 2], na.rm = TRUE),
    pcr_pos            = n_distinct(record_id[p_pcr == 1], na.rm = TRUE),
    pcr_neg            = n_distinct(record_id[p_pcr == 2], na.rm = TRUE),
    serot_dengue_1_pos = n_distinct(record_id[serot_dengue == 1], na.rm = TRUE),
    serot_dengue_2_pos = n_distinct(record_id[serot_dengue == 2], na.rm = TRUE),
    serot_dengue_3_pos = n_distinct(record_id[serot_dengue == 3], na.rm = TRUE),
    serot_dengue_4_pos = n_distinct(record_id[serot_dengue == 4], na.rm = TRUE),
    serot_dengue_na    = n_distinct(record_id[serot_dengue == 5], na.rm = TRUE),
    concl_dengue       = n_distinct(record_id[conclusi_n_caso == 1], na.rm = TRUE),
    concl_dengue_sin   = n_distinct(record_id[tip_dengue == 1], na.rm = TRUE),
    concl_dengue_con   = n_distinct(record_id[tip_dengue == 2], na.rm = TRUE),
    concl_dengue_grave = n_distinct(record_id[tip_dengue == 3], na.rm = TRUE),
    .groups = "drop"
  )

deng_results_age_weekly <- deng_results_age %>%
  group_by( week_start, age_grp) %>%
  summarise(
    total_tested_deng  = sum(total_tested),
    deng_pos           = sum(deng_pos),
    ns1_pos            = sum(ns1_pos),
    ns1_neg            = sum(ns1_neg),
    igg_pos            = sum(igg_pos),
    igg_neg            = sum(igg_neg),
    igm_pos            = sum(igm_pos),
    igm_neg            = sum(igm_neg),
    pcr_pos            = sum(pcr_pos),
    pcr_neg            = sum(pcr_neg),
    serot_dengue_1_pos = sum(serot_dengue_1_pos),
    serot_dengue_2_pos = sum(serot_dengue_2_pos),
    serot_dengue_3_pos = sum(serot_dengue_3_pos),
    serot_dengue_4_pos = sum(serot_dengue_4_pos),
    concl_dengue       = sum(concl_dengue),
    concl_dengue_sin   = sum(concl_dengue_sin),
    concl_dengue_con   = sum(concl_dengue_con),
    concl_dengue_grave = sum(concl_dengue_grave),
    .groups = "drop"
  ) %>%
  complete(
    nesting( week_start), age_grp,
    fill = list(
      total_tested_deng = 0, deng_pos = 0, ns1_pos = 0, ns1_neg = 0, igg_pos = 0, igg_neg = 0,
      igm_pos = 0, igm_neg = 0, pcr_pos = 0, pcr_neg = 0,
      serot_dengue_1_pos = 0, serot_dengue_2_pos = 0,
      serot_dengue_3_pos = 0, serot_dengue_4_pos = 0,
      concl_dengue = 0, concl_dengue_sin = 0, concl_dengue_con = 0, concl_dengue_grave = 0
    )
  )



deng_results_sex_weekly <- deng_results_sex %>%
  group_by( week_start, sex) %>%
  summarise(
    total_tested_deng  = sum(total_tested),
    deng_pos           = sum(deng_pos),
    ns1_pos            = sum(ns1_pos),
    ns1_neg            = sum(ns1_neg),
    igg_pos            = sum(igg_pos),
    igg_neg            = sum(igg_neg),
    igm_pos            = sum(igm_pos),
    igm_neg            = sum(igm_neg),
    pcr_pos            = sum(pcr_pos),
    pcr_neg            = sum(pcr_neg),
    serot_dengue_1_pos = sum(serot_dengue_1_pos),
    serot_dengue_2_pos = sum(serot_dengue_2_pos),
    serot_dengue_3_pos = sum(serot_dengue_3_pos),
    serot_dengue_4_pos = sum(serot_dengue_4_pos),
    concl_dengue       = sum(concl_dengue),
    concl_dengue_sin   = sum(concl_dengue_sin),
    concl_dengue_con   = sum(concl_dengue_con),
    concl_dengue_grave = sum(concl_dengue_grave),
    .groups = "drop"
  ) %>%
  complete(
    nesting(week_start), sex,
    fill = list(
      total_tested_deng = 0, deng_pos = 0, ns1_pos = 0, ns1_neg = 0, igg_pos = 0, igg_neg = 0,
      igm_pos = 0, igm_neg = 0, pcr_pos = 0, pcr_neg = 0,
      serot_dengue_1_pos = 0, serot_dengue_2_pos = 0,
      serot_dengue_3_pos = 0, serot_dengue_4_pos = 0,
      concl_dengue = 0, concl_dengue_sin = 0, concl_dengue_con = 0, concl_dengue_grave = 0
    )
  )
# join to age-group surveillance denominator (surv_age_week already built above)
deng_incidence_age <- deng_results_age_weekly %>%
  left_join(surv_age_week,      by = c("week_start", "age_grp")) %>%
  mutate(
    ali_inc  = 1000 * total_tested_deng / surveilled,
    deng_inc = 1000 * deng_pos     / surveilled
  ) %>%
  arrange(age_grp, week_start) %>%
  group_by(age_grp) %>%
  mutate(
    surveilled_roll  = rollsum(surveilled,  k = 3, fill = NA),
    tested_deng_roll = rollsum(total_tested_deng, k = 3, fill = NA),
    deng_roll        = rollsum(deng_pos,     k = 3, fill = NA),
    ali_inc_roll     = 1000 * tested_deng_roll / surveilled_roll,
    deng_inc_roll    = 1000 * deng_roll        / surveilled_roll
  ) %>%
  ungroup() %>%
  rename(surveilled_deng = surveilled)%>%
  mutate(ali = total_tested_deng)

deng_incidence_sex <- deng_results_sex_weekly %>%
  left_join(surv_sex_week,      by = c("week_start", "sex")) %>%
  mutate(
    ali_inc  = 1000 * total_tested_deng / surveilled,
    deng_inc = 1000 * deng_pos     / surveilled
  ) %>%
  arrange(sex, week_start) %>%
  group_by(sex) %>%
  mutate(
    surveilled_roll  = rollsum(surveilled,  k = 3, fill = NA),
    tested_deng_roll = rollsum(total_tested_deng, k = 3, fill = NA),
    deng_roll        = rollsum(deng_pos,     k = 3, fill = NA),
    ali_inc_roll     = 1000 * tested_deng_roll / surveilled_roll,
    deng_inc_roll    = 1000 * deng_roll        / surveilled_roll
  ) %>%
  ungroup() %>%
  rename(surveilled_deng = surveilled)%>%
  mutate(ali = total_tested_deng)
# merge into resp_incidence_age
resp_incidence_age <- resp_incidence_age %>%
  left_join(
    deng_incidence_age %>%
      dplyr::select(week_start, age_grp, total_tested_deng, pcr_pos, igm_pos, ns1_pos,
                    ali, ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll),
    by = c("week_start", "age_grp")
  )

deng_incidence_sex$sex <-  ifelse(deng_incidence_sex$sex == 1, "Male", "Female")

resp_incidence_sex <- resp_incidence_sex %>%
  left_join(
    deng_incidence_sex %>%
      dplyr::select(week_start, sex, total_tested_deng, pcr_pos, igm_pos, ns1_pos,
                    ali, ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll),
    by = c("week_start", "sex")
  )
write.csv(resp_incidence_sex, "docs/vigicasa_resp_incidence_sex.csv")

resp_incidence_age <- resp_incidence_age %>%
  select(
    age_grp, week_start, surveilled, tested,
    inf_a_pos, inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,
    ari_inc,     ari_inc_roll,
    flu_gen_inc, flu_gen_inc_roll,
    flua_inc,    flua_inc_roll,
    flub_inc,    flub_inc_roll,
    scv2_inc,    scv2_inc_roll,
    rsv_inc,     rsv_inc_roll,
    h1n1_inc,    h1n1_inc_roll,
    h3n2_inc,    h3n2_inc_roll,
    total_tested_deng, pcr_pos, igm_pos, ns1_pos, ali,
    ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll
  )

write.csv(resp_incidence_age, "docs/vigicasa_resp_incidence_age.csv")


##### Dengue results by municipio #####



deng_results_muni <- vigicasa2 %>%
  filter(!is.na(fech_tom)) %>%
  mutate(
    week_start = floor_date(fech_tom, unit = "week", week_start = 7),
    year       = year(fech_tom)
  ) %>%
  group_by(record_id, year, week_start, municipio_recent, fech_tom) %>%
  summarize(
    total_tested       = n_distinct(record_id),
    ns1_pos            = n_distinct(record_id[p_ns1 == 1], na.rm = TRUE),
    ns1_neg            = n_distinct(record_id[p_ns1 == 2], na.rm = TRUE),
    igg_pos            = n_distinct(record_id[p_igg == 1], na.rm = TRUE),
    igg_neg            = n_distinct(record_id[p_igg == 2], na.rm = TRUE),
    igm_pos            = n_distinct(record_id[p_igm == 1], na.rm = TRUE),
    igm_neg            = n_distinct(record_id[p_igm == 2], na.rm = TRUE),
    pcr_pos            = n_distinct(record_id[p_pcr == 1], na.rm = TRUE),
    pcr_neg            = n_distinct(record_id[p_pcr == 2], na.rm = TRUE),
    serot_dengue_1_pos = n_distinct(record_id[serot_dengue == 1], na.rm = TRUE),
    serot_dengue_2_pos = n_distinct(record_id[serot_dengue == 2], na.rm = TRUE),
    serot_dengue_3_pos = n_distinct(record_id[serot_dengue == 3], na.rm = TRUE),
    serot_dengue_4_pos = n_distinct(record_id[serot_dengue == 4], na.rm = TRUE),
    serot_dengue_na    = n_distinct(record_id[serot_dengue == 5], na.rm = TRUE),
    concl_dengue       = n_distinct(record_id[conclusi_n_caso == 1], na.rm = TRUE),
    concl_dengue_sin   = n_distinct(record_id[tip_dengue == 1], na.rm = TRUE),
    concl_dengue_con   = n_distinct(record_id[tip_dengue == 2], na.rm = TRUE),
    concl_dengue_grave = n_distinct(record_id[tip_dengue == 3], na.rm = TRUE),
    .groups = "drop"
  )

deng_results_muni_weekly <- deng_results_muni %>%
  group_by(year, week_start, municipio_recent) %>%
  summarise(
    total_tested_deng  = sum(total_tested),
    ns1_pos            = sum(ns1_pos),
    ns1_neg            = sum(ns1_neg),
    igg_pos            = sum(igg_pos),
    igg_neg            = sum(igg_neg),
    igm_pos            = sum(igm_pos),
    igm_neg            = sum(igm_neg),
    pcr_pos            = sum(pcr_pos),
    pcr_neg            = sum(pcr_neg),
    serot_dengue_1_pos = sum(serot_dengue_1_pos),
    serot_dengue_2_pos = sum(serot_dengue_2_pos),
    serot_dengue_3_pos = sum(serot_dengue_3_pos),
    serot_dengue_4_pos = sum(serot_dengue_4_pos),
    concl_dengue       = sum(concl_dengue),
    concl_dengue_sin   = sum(concl_dengue_sin),
    concl_dengue_con   = sum(concl_dengue_con),
    concl_dengue_grave = sum(concl_dengue_grave),
    .groups = "drop"
  )

deng_incidence_muni <- deng_results_muni_weekly %>%
  left_join(surv_muni_week,       by = c("week_start", "municipio_recent")) %>%
  mutate(
    ali_inc  = 1000 * total_tested_deng / surveilled,
    deng_inc = 1000 * igm_pos     / surveilled
  ) %>%
  arrange(municipio_recent, week_start) %>%
  group_by(municipio_recent) %>%
  mutate(
    surveilled_roll  = roll3(surveilled),
    tested_deng_roll = roll3(total_tested_deng),
    deng_roll        = roll3(igm_pos),
    ali_inc_roll     = 1000 * tested_deng_roll / surveilled_roll,
    deng_inc_roll    = 1000 * deng_roll        / surveilled_roll
  ) %>%
  ungroup() %>%
  rename(surveilled_deng = surveilled)%>%
  mutate(ali = total_tested_deng)


resp_incidence_muni <- resp_incidence_muni %>%
  left_join(
    deng_incidence_muni %>%
      dplyr::select(week_start, municipio_recent, total_tested_deng, pcr_pos, igm_pos, ns1_pos,
                    ali, ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll),
    by = c("week_start", "municipio_recent")
  ) %>%
  mutate(
    across(
      c(total_tested_deng, pcr_pos, igm_pos, ns1_pos, ali, deng_roll, ali_inc, ali_inc_roll, deng_inc, deng_inc_roll),
      ~ replace_na(.x, 0)
    )
  )

resp_incidence_muni$municipio_recent <- ifelse(resp_incidence_muni$municipio_recent == 1, "Coatepeque", 
                                   ifelse(resp_incidence_muni$municipio_recent == 2, "La Blanca",
                                          ifelse(resp_incidence_muni$municipio_recent == 3, "Caballo Blanco - (Valle Lirio)", "Otro")))

write.csv(resp_incidence_muni, "docs/resp_incidence_muni.csv")


symptom_vars <- c(
  "fiebre", "tos", "dolor_oido", "congestion_nasal", "escurrimiento_nasal",
  "dolor_garganta", "vomito_despues", "silbido_respiro", "dificultad_respirar",
  "dolor_muscular", "dolor_cabeza", "dolor_articular", "sarpullido", "ojos_rojos",
  "articulares_hinchados", "dolor_ojos", "diarrea", "fatiga", "perdida_peso",
  "convulsiones", "labios_azules", "perdida_gusto", "dolor_cuerpo", "dolor_hueso",
  "irritabilidad", "letargo", "dificultad_comer"
)
# ── 1. Denominator: surveilled persons by week (mirrors surv_muni_week / surv_age_week logic)
surv_week <- realizada %>%
  filter(vigilancia_realizada == 1) %>%
  mutate(week_start = week_start_surv) %>%
  group_by(record_id, week_start) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start) %>%
  summarise(surveilled = n(), .groups = "drop")

# ── 2. Weekly symptom counts (numerator)
symptom_results <- vigicasa %>%
  filter(!is.na(fecha_primer_sinto)) %>%
  mutate(week_start = floor_date(fecha_primer_sinto, unit = "week", week_start = 7)) %>%
  group_by(record_id, week_start) %>%
  summarise(
    across(all_of(symptom_vars), ~ as.integer(any(.x == 1, na.rm = TRUE))),
    .groups = "drop"
  )

symptom_results$ili <-ifelse(symptom_results$fiebre == 1 & (symptom_results$tos == 1 |symptom_results$dolor_garganta == 1),1,0 )

symptom_vars <- c(
  "fiebre", "tos", "dolor_oido", "congestion_nasal", "escurrimiento_nasal",
  "dolor_garganta", "vomito_despues", "silbido_respiro", "dificultad_respirar",
  "dolor_muscular", "dolor_cabeza", "dolor_articular", "sarpullido", "ojos_rojos",
  "articulares_hinchados", "dolor_ojos", "diarrea", "fatiga", "perdida_peso",
  "convulsiones", "labios_azules", "perdida_gusto", "dolor_cuerpo", "dolor_hueso",
  "irritabilidad", "letargo", "dificultad_comer", "ili"
)

symptom_weekly <- symptom_results %>%
  group_by(week_start) %>%
  summarise(
    across(all_of(symptom_vars), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  complete(week_start, fill = as.list(setNames(rep(0, length(symptom_vars)), symptom_vars)))

#join numerator + denominator
symptom_incidence <- symptom_weekly %>% filter(week_start > "2026-01-05") %>% 
  left_join(surv_week, by = "week_start") %>%
  mutate(across(all_of(symptom_vars), ~ replace_na(.x, 0)))

#  Point-in-time incidence rates (per 1,000 surveilled)
symptom_incidence <- symptom_incidence %>%
  mutate(across(all_of(symptom_vars), ~ 1000 * .x / surveilled, .names = "{.col}_inc"))

###roll sums and roll incidence

symptom_incidence <- symptom_incidence %>%
  arrange(week_start) %>%
  mutate(
    surveilled_roll = roll3(surveilled),
    across(all_of(symptom_vars), roll3, .names = "{.col}_roll")
  ) %>%
  mutate(
    across(all_of(symptom_vars), ~ 1000 * get(paste0(cur_column(), "_roll")) / surveilled_roll, .names = "{.col}_inc_roll")
  ) 

write.csv(symptom_incidence, "docs/vigicasa_symptom_incidence.csv")

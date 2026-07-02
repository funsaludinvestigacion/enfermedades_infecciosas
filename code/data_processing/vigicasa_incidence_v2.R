# Load libraries -----------------------------------------------------------
library(REDCapR)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(zoo)

# Load data -----------------------------------------------------------------
vigicasa_token <- Sys.getenv("vigicasa_token")

uri <- "https://redcap.ucdenver.edu/api/"

vigicasa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = "vigicasa_token"
  )$data

# Processing -----------------------------------------------------------------
# dates in the right format
vigicasa$f_muestra <- ymd(vigicasa$f_muestra)
vigicasa$fech_tom <- ymd(vigicasa$fech_tom)
vigicasa$fecha_vigilancia <- ymd(vigicasa$fecha_vigilancia)
vigicasa$month <-  month(vigicasa$fecha_vigilancia)
vigicasa$year<-  year(vigicasa$fecha_vigilancia)
vigicasa$epiweek<- epiweek(vigicasa$fecha_vigilancia)   
vigicasa$week_start <- floor_date(vigicasa$fecha_vigilancia, unit = "week", week_start = 7)  # changed to fecha_vigilancia
vigicasa$week_start_test <- floor_date(vigicasa$f_muestra, unit = "week", week_start = 7)  # changed to fecha_vigilancia
vigicasa$week_start_surv <- floor_date(vigicasa$fecha_vigilancia, unit = "week", week_start = 7)  # changed to fecha_vigilancia

vigicasa_record_day <- vigicasa %>% filter(vigilancia_realizada == 1) %>% 
  group_by(record_id,  week_start, fecha_vigilancia) %>% 
  summarise(surv_per_day = n()) 

vigicasa_record_week <- vigicasa %>% filter(vigilancia_realizada == 1) %>% 
  group_by(record_id, week_start) %>% 
           summarise(surv_per_week = n())                                                             


counts_weekly <- vigicasa_record_week %>% group_by( week_start)  %>% 
  summarise(surveilled = n()) 


realizada <- vigicasa %>% filter(vigilancia_realizada == 1)
table(realizada$edad, useNA = "always")
table(realizada$edad, useNA = "always")
table(realizada$departamento)
realizada_IDs <- unique(realizada$record_id)

enroll <- vigicasa %>%  filter(!is.na(fecha_enrolamiento))
table(enroll$etnia, useNA =  "always")
table(enroll$sexo, useNA =  "always")
table(enroll$actualmente_trabaja, useNA =  "always")
table(enroll$en_que_trabaja___1, useNA =  "always")
table(enroll$en_que_trabaja___2, useNA =  "always")
table(enroll$en_que_trabaja___3, useNA =  "always")
table(enroll$en_que_trabaja___4, useNA =  "always")
table(enroll$en_que_trabaja___5, useNA =  "always")
table(enroll$en_que_trabaja___6, useNA =  "always")
table(enroll$en_que_trabaja___7, useNA =  "always")
table(enroll$en_que_trabaja___8, useNA =  "always")
table(enroll$en_que_trabaja___9, useNA =  "always")
table(enroll$en_que_trabaja___10, useNA =  "always")


##### tested

vigicasa1 <- vigicasa %>%
  filter(!is.na(f_muestra))



table(vigicasa1$edad)

vigicasa_tested_week <- vigicasa1 %>% 
  group_by(record_id,  week_start_test) %>% summarise (tests = n())

tests_weekly <- vigicasa_tested_week %>% group_by( week_start_test)  %>% 
  summarise(tested = n()) 


resp_results <- vigicasa %>%
  filter(
    !is.na(f_muestra) &
      if_any(starts_with("virus_detectado___"), ~ .x %in% 1)
  ) %>%
  mutate(
    week_start =  floor_date(f_muestra, unit = "week", week_start = 7), # changed to fecha_vigilancia
    age = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    distrito = toupper(distrito),
    department = toupper(area_salud),
    sex = sexo_paciente,
    fecha_muestra = f_muestra
  ) %>%
  group_by(record_id, week_start, year, age, sex, distrito, department, fecha_muestra) %>%
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
    month = month(fecha_muestra),
    .groups = "drop"
  ) 

resp_weekly <- resp_results %>% group_by(year, week_start) %>%
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
    inf_a_h1n1   = sum(inf_a_h1n1),   # added
    inf_a_h3n2   = sum(inf_a_h3n2),   # added
    inf_a_nosub = sum(inf_a_nosub),
    .groups = "drop"
  )

counts_weekly$week_start <- counts_weekly$week_start
tests_weekly$week_start <- tests_weekly$week_start_test
  

resp_incidence_w <- left_join(resp_weekly, counts_weekly)
resp_incidence_w <- left_join(resp_incidence_w, tests_weekly)

resp_incidence_w$ili_inc      <- 1000 * resp_incidence_w$tested       / resp_incidence_w$surveilled
resp_incidence_w$flua_inc     <- 1000 * resp_incidence_w$inf_a_pos    / resp_incidence_w$surveilled
resp_incidence_w$flub_inc     <- 1000 * resp_incidence_w$inf_b_pos    / resp_incidence_w$surveilled
resp_incidence_w$scv2_inc     <- 1000 * resp_incidence_w$sars_cov2_pos / resp_incidence_w$surveilled
resp_incidence_w$rsv_inc      <- 1000 * resp_incidence_w$vsr_pos      / resp_incidence_w$surveilled
resp_incidence_w$flu_gen_inc  <- 1000 * (resp_incidence_w$inf_a_pos + resp_incidence_w$inf_b_pos) / resp_incidence_w$surveilled
resp_incidence_w$h1n1_inc     <- 1000 * resp_incidence_w$inf_a_h1n1  / resp_incidence_w$surveilled  # added
resp_incidence_w$h3n2_inc     <- 1000 * resp_incidence_w$inf_a_h3n2  / resp_incidence_w$surveilled  # added


library(zoo)



#### Roll SUMS 
#### Roll SUMS 
resp_incidence_w$surveilled_roll      <- rollsum(resp_incidence_w$surveilled,      k = 3, fill = NA)
resp_incidence_w$tested_roll          <- rollsum(resp_incidence_w$tested,          k = 3, fill = NA)
resp_incidence_w$inf_a_pos_roll       <- rollsum(resp_incidence_w$inf_a_pos,       k = 3, fill = NA)
resp_incidence_w$inf_b_pos_roll       <- rollsum(resp_incidence_w$inf_b_pos,       k = 3, fill = NA) 
resp_incidence_w$sars_cov2_pos_roll   <- rollsum(resp_incidence_w$sars_cov2_pos,   k = 3, fill = NA)
resp_incidence_w$vsr_pos_roll         <- rollsum(resp_incidence_w$vsr_pos,         k = 3, fill = NA)
resp_incidence_w$inf_a_h1n1_roll      <- rollsum(resp_incidence_w$inf_a_h1n1,      k = 3, fill = NA)  # added
resp_incidence_w$inf_a_h3n2_roll      <- rollsum(resp_incidence_w$inf_a_h3n2,      k = 3, fill = NA)  # added

resp_incidence_w$ili_inc_roll         <- 1000 * resp_incidence_w$tested_roll          / resp_incidence_w$surveilled_roll
resp_incidence_w$flua_inc_roll        <- 1000 * resp_incidence_w$inf_a_pos_roll       / resp_incidence_w$surveilled_roll
resp_incidence_w$flub_inc_roll        <- 1000 * resp_incidence_w$inf_b_pos_roll       / resp_incidence_w$surveilled_roll
resp_incidence_w$scv2_inc_roll        <- 1000 * resp_incidence_w$sars_cov2_pos_roll   / resp_incidence_w$surveilled_roll
resp_incidence_w$rsv_inc_roll         <- 1000 * resp_incidence_w$vsr_pos_roll         / resp_incidence_w$surveilled_roll
resp_incidence_w$flu_gen_inc_roll     <- 1000 * (resp_incidence_w$inf_a_pos_roll + resp_incidence_w$inf_b_pos_roll) / resp_incidence_w$surveilled_roll
resp_incidence_w$h1n1_inc_roll        <- 1000 * resp_incidence_w$inf_a_h1n1_roll      / resp_incidence_w$surveilled_roll  # added
resp_incidence_w$h3n2_inc_roll        <- 1000 * resp_incidence_w$inf_a_h3n2_roll      / resp_incidence_w$surveilled_roll  # added


resp_incidence_w <- resp_incidence_w %>%  
  dplyr::select(week_start, surveilled, tested, inf_a_pos,
                inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,   # added inf_a_h1n1, inf_a_h3n2
                ili_inc, ili_inc_roll,
                flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                flub_inc, flub_inc_roll, scv2_inc, scv2_inc_roll, 
                rsv_inc, rsv_inc_roll,
                h1n1_inc, h1n1_inc_roll, h3n2_inc, h3n2_inc_roll) %>%        # added h1n1/h3n2 inc + roll
  filter(!is.na(surveilled))

resp_incidence_w <- resp_incidence_w %>% filter(week_start > "2026-01-17")
write.csv(resp_incidence_w, "docs/vigicasa_resp_weekly.csv") 
# ── Age-group label helper ──────────────────────────────────────────────────
age_breaks  <- c(0, 5, 25, 50, 65, Inf)
age_labels  <- c("0-4", "5-24", "25-49", "50-64", "65+")


# ── 1. Denominator: surveilled persons by week & age group ──────────────────
# Age is available in 'realizada'; compute it the same way resp_results does
realizada_age <- realizada %>%
  mutate(
    age      = edad,
    age_grp  = cut(age, breaks = age_breaks, labels = age_labels,
                   right = FALSE, include.lowest = TRUE),
    week_start = week_start_surv          # already on the dataset
  ) %>%
  filter(vigilancia_realizada == 1)

# Unique person-weeks per age group  (mirrors counts_weekly logic)
surv_age_week <- realizada_age %>%
  group_by(record_id, week_start, age_grp) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start, age_grp) %>%
  summarise(surveilled = n(), .groups = "drop")

# ── 2. Tested & positive counts by week & age group ────────────────────────
resp_results_age <- vigicasa %>%
  filter(
    !is.na(f_muestra) &
      if_any(starts_with("virus_detectado___"), ~ .x %in% 1)
  ) %>%
  mutate(
    week_start  = floor_date(f_muestra, unit = "week", week_start = 7),
    age         = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    age_grp     = cut(age, breaks = age_breaks, labels = age_labels,
                      right = FALSE, include.lowest = TRUE),
    year        = year(f_muestra)
  )

resp_weekly_age <- resp_results_age %>%
  group_by(year, week_start, age_grp) %>%
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

# ── 3. Tested persons per week & age group (denominator for ILI rate) ───────
tested_age_week <- vigicasa %>%
  filter(!is.na(f_muestra)) %>%
  mutate(
    week_start = floor_date(f_muestra, unit = "week", week_start = 7),
    age        = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    age_grp    = cut(age, breaks = age_breaks, labels = age_labels,
                     right = FALSE, include.lowest = TRUE)
  ) %>%
  group_by(record_id, week_start, age_grp) %>%
  summarise(tests = n(), .groups = "drop") %>%
  group_by(week_start, age_grp) %>%
  summarise(tested = n(), .groups = "drop")

# ── 4. Join numerators + denominators ───────────────────────────────────────
resp_incidence_age <- resp_weekly_age %>%
  left_join(surv_age_week,   by = c("week_start", "age_grp")) %>%
  left_join(tested_age_week, by = c("week_start", "age_grp"))

# ── 5. Point-in-time incidence rates (per 1 000 surveilled) ─────────────────
resp_incidence_age <- resp_incidence_age %>%
  mutate(
    ili_inc      = 1000 * tested        / surveilled,
    flua_inc     = 1000 * inf_a_pos     / surveilled,
    flub_inc     = 1000 * inf_b_pos     / surveilled,
    scv2_inc     = 1000 * sars_cov2_pos / surveilled,
    rsv_inc      = 1000 * vsr_pos       / surveilled,
    flu_gen_inc  = 1000 * (inf_a_pos + inf_b_pos) / surveilled,
    h1n1_inc     = 1000 * inf_a_h1n1   / surveilled,
    h3n2_inc     = 1000 * inf_a_h3n2   / surveilled
  )

# ── 6. Rolling sums & rates (3-week window, within each age group) ───────────
resp_incidence_age <- resp_incidence_age %>%
  arrange(age_grp, week_start) %>%
  group_by(age_grp) %>%
  mutate(
    across(
      c(surveilled, tested, inf_a_pos, inf_b_pos,
        sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2),
      ~ rollsum(.x, k = 3, fill = NA, align = "center"),
      .names = "{.col}_roll"
    )
  ) %>%
  mutate(
    ili_inc_roll      = 1000 * tested_roll        / surveilled_roll,
    flua_inc_roll     = 1000 * inf_a_pos_roll     / surveilled_roll,
    flub_inc_roll     = 1000 * inf_b_pos_roll     / surveilled_roll,
    scv2_inc_roll     = 1000 * sars_cov2_pos_roll / surveilled_roll,
    rsv_inc_roll      = 1000 * vsr_pos_roll       / surveilled_roll,
    flu_gen_inc_roll  = 1000 * (inf_a_pos_roll + inf_b_pos_roll) / surveilled_roll,
    h1n1_inc_roll     = 1000 * inf_a_h1n1_roll    / surveilled_roll,
    h3n2_inc_roll     = 1000 * inf_a_h3n2_roll    / surveilled_roll
  ) %>%
  ungroup() %>%
  filter(!is.na(surveilled))
# ── 7. Tidy final column order ───────────────────────────────────────────────
resp_incidence_age <- resp_incidence_age %>%
  select(
    age_grp, week_start, surveilled, tested,
    inf_a_pos, inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,
    ili_inc,     ili_inc_roll,
    flu_gen_inc, flu_gen_inc_roll,
    flua_inc,    flua_inc_roll,
    flub_inc,    flub_inc_roll,
    scv2_inc,    scv2_inc_roll,
    rsv_inc,     rsv_inc_roll,
    h1n1_inc,    h1n1_inc_roll,
    h3n2_inc,    h3n2_inc_roll
  )

resp_incidence_age <- resp_incidence_age %>% filter(as.Date(week_start) > "2026-01-17")
write.csv(resp_incidence_age, "docs/resp_incidence_age.csv") 

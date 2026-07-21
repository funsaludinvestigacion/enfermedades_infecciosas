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


q <- realizada %>% filter(edad > 64) %>% group_by(record_id)%>% summarise(recent = max(fecha_vigilancia))
table(q$record_id)

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
    !is.na(f_muestra) 
  ) %>%
  mutate(
    week_start  = floor_date(f_muestra, unit = "week", week_start = 7),
    age         = floor(interval(start = f_nacimiento, end = f_muestra) / years(1)),
    age_grp     = cut(age, breaks = age_breaks, labels = age_labels,
                      right = FALSE, include.lowest = TRUE),
    year        = year(f_muestra)
  )

# ── 2. Weekly counts by age group (with zero-filled age × week combos) ──────
resp_weekly_age <- resp_results_age %>%
  group_by(week_start, age_grp) %>%
  summarise(
    total_tested    = n_distinct(record_id),
    sars_cov2_pos   = sum(virus_detectado___4 == 1, na.rm = TRUE),
    inf_a_pos       = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_b_pos       = sum(virus_detectado___3 == 1, na.rm = TRUE),
    vsr_pos         = sum(virus_detectado___5 == 1, na.rm = TRUE),
    inf_a_h1n1      = sum(subtipo_infa == "H1N1",   na.rm = TRUE),
    inf_a_h3n2      = sum(subtipo_infa == "H3N2",   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    week_start, age_grp,
    fill = list(
      total_tested  = 0,
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
  summarise(tested = n(), .groups = "drop") %>%
  complete(
    week_start, age_grp,
    fill = list(tested = 0)
  )
# ── 2. Weekly counts by age group (with zero-filled age × week combos) ──────
resp_weekly_age <- resp_results_age %>%
  group_by(week_start, age_grp) %>%
  summarise(
    total_tested    = n_distinct(record_id),
    sars_cov2_pos   = sum(virus_detectado___4 == 1, na.rm = TRUE),
    inf_a_pos       = sum(virus_detectado___2 == 1, na.rm = TRUE),
    inf_b_pos       = sum(virus_detectado___3 == 1, na.rm = TRUE),
    vsr_pos         = sum(virus_detectado___5 == 1, na.rm = TRUE),
    inf_a_h1n1      = sum(subtipo_infa == "H1N1",   na.rm = TRUE),
    inf_a_h3n2      = sum(subtipo_infa == "H3N2",   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    week_start, age_grp,
    fill = list(
      total_tested  = 0,
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
  summarise(tested = n(), .groups = "drop") %>%
  complete(
    week_start, age_grp,
    fill = list(tested = 0)
  )
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

# ── 1. Denominator: surveilled persons by week & municipio ──────────────────


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
realizada1 <- left_join(realizada, municipios)

q <- realizada1 %>% group_by(record_id, municipio_recent) %>% tally()

realizada_muni <- realizada1 %>%
  mutate(
    week_start = week_start_surv          # already on the dataset
  ) %>%
  filter(vigilancia_realizada == 1)

# Unique person-weeks per municipio  (mirrors counts_weekly logic)
surv_muni_week <- realizada_muni %>%
  group_by(record_id, week_start, municipio_recent) %>%
  summarise(surv_per_week = n(), .groups = "drop") %>%
  group_by(week_start, municipio_recent) %>%
  summarise(surveilled = n(), .groups = "drop")

# ── 2. Tested & positive counts by week & municipio ────────────────────────

vigicasa2 <- left_join(vigicasa, municipios)

resp_results_muni <- vigicasa2 %>%
  filter(
    !is.na(f_muestra) &
      if_any(starts_with("virus_detectado___"), ~ .x %in% 1)
  ) %>%
  mutate(
    week_start  = floor_date(f_muestra, unit = "week", week_start = 7),
    year        = year(f_muestra)
  )

resp_weekly_muni <- resp_results_muni %>%
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

# ── 3. Tested persons per week & municipio (denominator for ILI rate) ───────
tested_muni_week <- vigicasa2 %>%
  filter(!is.na(f_muestra)) %>%
  mutate(
    week_start = floor_date(f_muestra, unit = "week", week_start = 7)
  ) %>%
  group_by(record_id, week_start, municipio_recent) %>%
  summarise(tests = n(), .groups = "drop") %>%
  group_by(week_start, municipio_recent) %>%
  summarise(tested = n(), .groups = "drop")

# ── 4. Join numerators + denominators ───────────────────────────────────────
resp_incidence_muni <- resp_weekly_muni %>%
  left_join(surv_muni_week,   by = c("week_start", "municipio_recent")) %>%
  left_join(tested_muni_week, by = c("week_start", "municipio_recent"))

# ── 5. Point-in-time incidence rates (per 1 000 surveilled) ─────────────────
resp_incidence_muni <- resp_incidence_muni %>%
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

# ── 6. Rolling sums & rates (3-week window, within each municipio) ──────────
resp_incidence_muni <- resp_incidence_muni %>%
  arrange(municipio_recent, week_start) %>%
  group_by(municipio_recent) %>%
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
resp_incidence_muni <- resp_incidence_muni %>%
  select(
    municipio_recent, week_start, surveilled, tested,
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


resp_incidence_muni <- resp_incidence_muni %>% filter(as.Date(week_start) > "2026-01-17")


write.csv(resp_incidence_muni, "docs/resp_incidence_muni.csv")

##### tested dengue 

vigicasa_deng <- vigicasa %>%
  filter(!is.na(fech_tom))

vigicasa_deng$week_start_test <- floor_date(vigicasa_deng$fech_tom, unit = "week", week_start = 7)  # changed to fecha_vigilancia

deng_tests_weekly <- vigicasa_deng %>% group_by( week_start_test)  %>% 
  summarise(tested = n()) 


deng_results <- vigicasa %>%
  filter(
    !is.na(fech_tom) 
  ) %>%
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
    month = month(fech_tom),
    .groups = "drop"
  ) 

deng_results_weekly <- deng_results %>% group_by(year, week_start) %>%
  summarise(
    total_tested_deng = sum(total_tested),
    ns1_pos    = sum(ns1_pos),
    ns1_neg    = sum(ns1_neg),
    igg_pos = sum(igg_pos),
    igg_neg = sum(igg_neg),
    igm_pos    = sum(igm_pos),
    igm_neg    = sum(igm_neg),
    pcr_pos    = sum(pcr_pos),
    pcr_neg    = sum(pcr_neg),
    serot_dengue_1_pos = sum(serot_dengue_1_pos),
    serot_dengue_2_pos = sum(serot_dengue_2_pos),
    serot_dengue_3_pos = sum(serot_dengue_3_pos),   # added
    serot_dengue_4_pos = sum(serot_dengue_4_pos),   # added
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
deng_incidence_w$deng_inc     <- 1000 * deng_incidence_w$igm_pos    / deng_incidence_w$surveilled





#### Roll SUMS 
#### Roll SUMS 
deng_incidence_w$surveilled_roll      <- rollsum(deng_incidence_w$surveilled,      k = 3, fill = NA)
deng_incidence_w$tested_deng_roll     <- rollsum(deng_incidence_w$tested_deng,          k = 3, fill = NA)
deng_incidence_w$deng_roll          <- rollsum(deng_incidence_w$igm_pos,          k = 3, fill = NA)

deng_incidence_w$ali_inc_roll         <- 1000 * deng_incidence_w$tested_deng_roll  / deng_incidence_w$surveilled_roll
deng_incidence_w$deng_inc_roll        <- 1000 * deng_incidence_w$deng_roll   / deng_incidence_w$surveilled_roll

deng_incidence_w <- deng_incidence_w %>% rename(ali = tested_deng,
                                                surveilled_deng = surveilled
) %>% dplyr::select(!c(tested))

resp_incidence_w <-  left_join(resp_incidence_w, deng_incidence_w)
resp_incidence_w <- resp_incidence_w %>% 
  dplyr::select(week_start, surveilled, tested, inf_a_pos,
                inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,   # added inf_a_h1n1, inf_a_h3n2
                ili_inc, ili_inc_roll,
                flu_gen_inc, flu_gen_inc_roll, flua_inc, flua_inc_roll,
                flub_inc, flub_inc_roll, scv2_inc, scv2_inc_roll, 
                rsv_inc, rsv_inc_roll,
                h1n1_inc, h1n1_inc_roll, h3n2_inc, h3n2_inc_roll,
                total_tested_deng, pcr_pos, igm_pos, ns1_pos, ali,
                ali_inc, ali_inc_roll, deng_roll,deng_inc, deng_inc_roll) %>%        
  filter(!is.na(surveilled))

resp_incidence_w <- resp_incidence_w %>% filter(week_start > "2026-01-17")
write.csv(resp_incidence_w, "docs/vigicasa_resp_weekly.csv")


##### Dengue results by age group #####

deng_tests_age_week <- vigicasa %>%
  filter(!is.na(fech_tom)) %>%
  mutate(
    week_start = floor_date(fech_tom, unit = "week", week_start = 7),
    age        = floor(interval(start = f_nacimiento, end = fech_tom) / years(1)),
    age_grp    = cut(age, breaks = age_breaks, labels = age_labels,
                     right = FALSE, include.lowest = TRUE)
  ) %>%
  group_by(record_id, week_start, age_grp) %>%
  summarise(tests = n(), .groups = "drop") %>%
  group_by(week_start, age_grp) %>%
  summarise(tested_deng = n(), .groups = "drop") %>%
  complete(week_start, age_grp, fill = list(tested_deng = 0))

deng_results_age <- vigicasa %>%
  filter(!is.na(fech_tom)) %>%
  mutate(
    week_start = floor_date(fech_tom, unit = "week", week_start = 7),
    age        = floor(interval(start = f_nacimiento, end = fech_tom) / years(1)),
    age_grp    = cut(age, breaks = age_breaks, labels = age_labels,
                     right = FALSE, include.lowest = TRUE),
    year       = year(fech_tom)
  ) %>%
  group_by(record_id, year, week_start, age_grp, fech_tom) %>%
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

deng_results_age_weekly <- deng_results_age %>%
  group_by(year, week_start, age_grp) %>%
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
  ) %>%
  complete(
    week_start, age_grp,
    fill = list(
      total_tested_deng = 0, ns1_pos = 0, ns1_neg = 0, igg_pos = 0, igg_neg = 0,
      igm_pos = 0, igm_neg = 0, pcr_pos = 0, pcr_neg = 0,
      serot_dengue_1_pos = 0, serot_dengue_2_pos = 0,
      serot_dengue_3_pos = 0, serot_dengue_4_pos = 0,
      concl_dengue = 0, concl_dengue_sin = 0, concl_dengue_con = 0, concl_dengue_grave = 0
    )
  )

# join to age-group surveillance denominator (surv_age_week already built above)
deng_incidence_age <- deng_results_age_weekly %>%
  left_join(surv_age_week,      by = c("week_start", "age_grp")) %>%
  left_join(deng_tests_age_week, by = c("week_start", "age_grp")) %>%
  mutate(
    ali_inc  = 1000 * tested_deng / surveilled,
    deng_inc = 1000 * igm_pos     / surveilled
  ) %>%
  arrange(age_grp, week_start) %>%
  group_by(age_grp) %>%
  mutate(
    surveilled_roll  = rollsum(surveilled,  k = 3, fill = NA),
    tested_deng_roll = rollsum(tested_deng, k = 3, fill = NA),
    deng_roll        = rollsum(igm_pos,     k = 3, fill = NA),
    ali_inc_roll     = 1000 * tested_deng_roll / surveilled_roll,
    deng_inc_roll    = 1000 * deng_roll        / surveilled_roll
  ) %>%
  ungroup() %>%
  rename(ali = tested_deng, surveilled_deng = surveilled)

# merge into resp_incidence_age
resp_incidence_age <- resp_incidence_age %>%
  left_join(
    deng_incidence_age %>%
      dplyr::select(week_start, age_grp, total_tested_deng, pcr_pos, igm_pos, ns1_pos,
                    ali, ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll),
    by = c("week_start", "age_grp")
  )

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
    h3n2_inc,    h3n2_inc_roll,
    total_tested_deng, pcr_pos, igm_pos, ns1_pos, ali,
    ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll
  )

write.csv(resp_incidence_age, "docs/resp_incidence_age.csv")


##### Dengue results by municipio #####

deng_tests_muni_week <- vigicasa2 %>%
  filter(!is.na(fech_tom)) %>%
  mutate(week_start = floor_date(fech_tom, unit = "week", week_start = 7)) %>%
  group_by(record_id, week_start, municipio_recent) %>%
  summarise(tests = n(), .groups = "drop") %>%
  group_by(week_start, municipio_recent) %>%
  summarise(tested_deng = n(), .groups = "drop")

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
  left_join(deng_tests_muni_week, by = c("week_start", "municipio_recent")) %>%
  mutate(
    ali_inc  = 1000 * tested_deng / surveilled,
    deng_inc = 1000 * igm_pos     / surveilled
  ) %>%
  arrange(municipio_recent, week_start) %>%
  group_by(municipio_recent) %>%
  mutate(
    surveilled_roll  = rollsum(surveilled,  k = 3, fill = NA),
    tested_deng_roll = rollsum(tested_deng, k = 3, fill = NA),
    deng_roll        = rollsum(igm_pos,     k = 3, fill = NA),
    ali_inc_roll     = 1000 * tested_deng_roll / surveilled_roll,
    deng_inc_roll    = 1000 * deng_roll        / surveilled_roll
  ) %>%
  ungroup() %>%
  rename(ali = tested_deng, surveilled_deng = surveilled)

resp_incidence_muni <- resp_incidence_muni %>%
  left_join(
    deng_incidence_muni %>%
      dplyr::select(week_start, municipio_recent, total_tested_deng, pcr_pos, igm_pos, ns1_pos,
                    ali, ali_inc, ali_inc_roll, deng_roll, deng_inc, deng_inc_roll),
    by = c("week_start", "municipio_recent")
  )

resp_incidence_muni <- resp_incidence_muni %>%
  select(
    municipio_recent, week_start, surveilled, tested,
    inf_a_pos, inf_b_pos, sars_cov2_pos, vsr_pos, inf_a_h1n1, inf_a_h3n2,
    ili_inc,     ili_inc_roll,
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

resp_incidence_muni$municipio_recent <- ifelse(resp_incidence_muni$municipio_recent == 1, "Coatepeque", 
                                   ifelse(resp_incidence_muni$municipio_recent == 2, "La Blanca",
                                          ifelse(resp_incidence_muni$municipio_recent == 3, "Caballo Blanco - (Valle Lirio)", "Otro")))

write.csv(resp_incidence_muni, "docs/resp_incidence_muni.csv")

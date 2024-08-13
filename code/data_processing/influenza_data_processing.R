# Load the necessary libraries ------------------------------------------------------
library(REDCapR)
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lubridate)
library(tidyr)

# Load in the data ------------------------------------------------------------------
influenza_token <- Sys.getenv("influenza_token")

uri <- "https://redcap.ucdenver.edu/api/"

influenza <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = influenza_token
  )$data


# Every week, 15 Agri workers are randomly chosen to be surveilled
# Workers are called, and asked if they have any symptoms.
# If they do, they go through a visita sintomatica, as well as a visita the next week, and a month later.
# If not, they skip this visit, but both a week later and month later are asked about symptoms.
# The goal from this study is to identify the most common symptoms.

# Primera llamada
influenza$epiweek_1_llamada <- lubridate::floor_date(
  influenza$fecha_hora_controles, unit = "week", week_start = 1)

# Visita síntomatica
influenza$epiweek_sintomatica <- lubridate::floor_date(
  influenza$fecha_hora_visit_sint, unit = "week", week_start = 1)


# Symptomatic visits by week ---------------------------------------------------------------
# Record who had a symptomatic visit initiated but la visita no se realizó
#fecha_hora_visit_sint
#visita_sintom

# Create epiweek
influenza$epiweek_sintomatica <- lubridate::floor_date(influenza$fecha_hora_visit_sint, unit = "week", week_start = 1)

columns_agri_sintomas_sintomatica <- c(
  # "tos_sintomatica", # en los ultimos 7 dias
  # "fiebre_sintomatica", # en los ultimos 7 dias
  # "difi_respi_visit_sintom", # en los ultimos 7 dias
  "tos_enroll",
  "tos_flema_enroll",
  "garganta_irri_enroll",
  "dolor_cabeza_enroll",
  "congestion_nasal_enroll",
  "fiebre_enroll",
  "dolor_cuerpo_enroll",
  "fatiga_enroll",
  "dolor_cuello_enroll",
  "sue_o_interrumpido_enroll",
  "silbido_enroll",
  "falta_aire_enroll",
  "perdida_apetito_enroll",
  "dism_olores",
  "dism_sabores",
  "vomito_visit_sintom",
  "diarrea_visit_sintom"
)

columns_agri_sintomas_sintomatica_rename <- c(
  "Tos_seca" = "tos_enroll",
  "Tos_con_flema" = "tos_flema_enroll",
  "Dolor_de_garganta" = "garganta_irri_enroll",
  "Dolor_de_cabeza" = "dolor_cabeza_enroll",
  "Congestion_nasal" = "congestion_nasal_enroll",
  "Fiebre" = "fiebre_enroll",
  "Dolor_de_cuerpo" = "dolor_cuerpo_enroll",
  "Fatiga" = "fatiga_enroll",
  "Dolor_de_cuello" = "dolor_cuello_enroll",
  "Sueño_interrumpido" = "sue_o_interrumpido_enroll",
  "Silbancias" = "silbido_enroll",
  "Falta_de_aire" = "falta_aire_enroll",
  "Perdida_de_apetito" = "perdida_apetito_enroll",
  "Disminución_de_olores" = "dism_olores",
  "Disminución_de_sabores" = "dism_sabores",
  "Vomito" = "vomito_visit_sintom",
  "Diarrea" = "diarrea_visit_sintom"
)

# Visita - Primera semana -------------------------------------------------

influenza$epiweek_semana1 <- lubridate::floor_date(influenza$fecha_hora_visita_1sem, unit = "week", week_start = 1)

columns_agri_sintomas_semana1 <- c(
  "tos_1sem",
  "tos_flemas_1sem",
  "dolor_garganta_1sem",
  "dolor_cabeza_1sem",
  "congestion_nasal_1sem",
  "fiebre_1sem",
  "dolor_cuerpo_1sem",
  "fatiga_1sem",
  "dolor_cuello_1sem",
  "sue_o_interrumpido_1sem",
  "silbido_respirar_1sem",
  "falta_aire_1sem",
  "perdia_apetito_1sem",
  "olores_sintom",
  "sabores_sitom",
  "vomito_7dias",
  "diarrea_7dias"
  # "grupo" # control o caso
)

columns_agri_sintomas_semana1_rename <- c(
  "Tos_seca" = "tos_1sem",
  "Tos_con_flema" = "tos_flemas_1sem",
  "Dolor_de_garganta" = "dolor_garganta_1sem",
  "Dolor_de_cabeza" = "dolor_cabeza_1sem",
  "Congestion_nasal" = "congestion_nasal_1sem",
  "Fiebre" = "fiebre_1sem",
  "Dolor_de_cuerpo" = "dolor_cuerpo_1sem",
  "Fatiga" = "fatiga_1sem",
  "Dolor_de_cuello" = "dolor_cuello_1sem",
  "Sueño_interrumpido" = "sue_o_interrumpido_1sem",
  "Silbancias" = "silbido_respirar_1sem",
  "Falta_de_aire" = "falta_aire_1sem",
  "Perdida_de_apetito" = "perdia_apetito_1sem",
  "Disminución_de_olores" = "olores_sintom",
  "Disminución_de_sabores" = "sabores_sitom",
  "Vomito" = "vomito_7dias",
  "Diarrea" = "diarrea_7dias"
)


# Visita - Cuarta semana ------------------------------------------------------

# Create epiweek
influenza$epiweek_semana4 <- lubridate::floor_date(influenza$fecha_hora_visita_1mes, unit = "week", week_start = 1)

columns_agri_sintomas_semana4 <- c(
  # "tos_7dias_1mes", # in past 7 days -- not going to include
  # "fiebre_7dias_1mes", # in past 7 days -- not going to include
  "tos_4sem",
  "tos_flem_4sem",
  "garg_i_4sem",
  "dol_cab_4sem",
  "conges_na_4sem",
  "sens_fieb_4sem",
  "dol_cuer_gen_4sem",
  "fatiga_4sem",
  "dol_cue_4sem",
  "inter_sue_4sem",
  "silb_resp_4sem",
  "falt_air_4sem",
  "perd_apet_4sem",
  "dism_olor_4sem",
  "dism_sab_4sem",
  "vomito_28dias",
  "diarrea_28dias"
  # "grupo_1mes" # Defines caso and control
)

columns_agri_sintomas_semana4_rename <- c(
  "Tos_seca" = "tos_4sem",
  "Tos_con_flema" = "tos_flem_4sem",
  "Dolor_de_garganta" = "garg_i_4sem",
  "Dolor_de_cabeza" = "dol_cab_4sem",
  "Congestion_nasal" = "conges_na_4sem",
  "Fiebre" = "sens_fieb_4sem",
  "Dolor_de_cuerpo" = "dol_cuer_gen_4sem",
  "Fatiga" = "fatiga_4sem",
  "Dolor_de_cuello" = "dol_cue_4sem",
  "Sueño_interrumpido" = "inter_sue_4sem",
  "Silbancias" = "silb_resp_4sem",
  "Falta_de_aire" = "falt_air_4sem",
  "Perdida_de_apetito" = "perd_apet_4sem",
  "Disminución_de_olores" = "dism_olor_4sem",
  "Disminución_de_sabores" = "dism_sab_4sem",
  "Vomito" = "vomito_28dias",
  "Diarrea" = "diarrea_28dias"
)

# Looking at muestras from FunSalud Laboratory (only from symptomatic individuals)------------------

# Create epiweek
influenza$epiweek_recolec <- lubridate::floor_date(influenza$fecha_recolec, unit = "week", week_start = 1)

# Issue 1: How to deal with follow-up results (if sample is unprocessed or indeterminant (3 or 5))
# We can create a new column for this
create_new_column <- function(df, col1, col2, new_col) {
  df[[new_col]] <- ifelse(is.na(df[[col1]]), NA,
                          ifelse(df[[col1]] == 1, 1,
                                 ifelse(df[[col1]] == 2, 2,
                                        ifelse(df[[col1]] == 3 & is.na(df[[col2]]), 3,
                                               ifelse(df[[col1]] == 3 & !is.na(df[[col2]]) & df[[col2]] == 3, df[[col2]], NA)
                                        )
                                 )
                          )
  )
  return(df)
}

# Columns to iterate over
columns_to_iterate <- list(
  list("resul_inf_a", "resul_inf_a_2", "resul_inf_a_all"),
  list("resul_rsv", "resul_vsr_2", "resul_rsv_all"),
  list("resul_inf_b", "resul_inf_b_2", "resul_inf_b_all"),
  list("resul_ocr_sars", "resul_pcr_sasrs2", "resul_sars_all"), # note typo here in redcap column name
  list("resul_covid_19", "resul_covid_19_2", "resul_covid_19_all")
)

# Apply the function to each pair of columns
for (cols in columns_to_iterate) {
  influenza <- create_new_column(influenza, cols[[1]], cols[[2]], cols[[3]])
}

# Create a new column summarizing SARS and COVID
influenza$resul_sars_covid_all <- ifelse(is.na(influenza$resul_sars_all) & is.na(influenza$resul_covid_19_all), NA,
                                         ifelse(is.na(influenza$resul_sars_all) & !is.na(influenza$resul_covid_19_all), influenza$resul_covid_19_all,
                                                ifelse(is.na(influenza$resul_covid_19_all) & !is.na(influenza$resul_sars_all), influenza$resul_sars_all,
                                                       pmin(influenza$resul_covid_19_all, influenza$resul_sars_all))))

# Create a new column summarizing Influenza
influenza <- influenza%>%
  dplyr::mutate(resul_inf_all = ifelse(is.na(resul_inf_a_all) & is.na(resul_inf_b_all), NA,
                                         ifelse(is.na(resul_inf_a_all) & (!is.na(resul_inf_b_all)), resul_inf_b_all,
                                                ifelse((!is.na(resul_inf_a_all)) & is.na(resul_inf_b_all), resul_inf_a_all,
                                                       pmin(resul_inf_a_all, resul_inf_b_all, na.rm = TRUE)))))


# Create a new column summarizing all viruses together
influenza$resul_virus_all <- pmin(influenza$resul_inf_a_all,
                                 influenza$resul_inf_b_all,
                                 influenza$resul_rsv_all,
                                 influenza$resul_sars_all,
                                 influenza$resul_covid_19_all, na.rm = TRUE)


# Next, we want to filter out the samples that are not regularly taken
# From what I understand,
# we only want samples taken during the visita sintomática to understand the trends

influenza_muestras <- influenza %>%
  dplyr::filter(tipo_visita_lab___2==1)


# Issue 2: What if the same individual is tested multiple times in the same week?
# We don't care if the result is the same
# We do care if one is positive (always prefer the positive value)
# Since the minimum value is the preferred data type (1 = positve, 2 = negative), select for minimum value

filter_group_slice <- function(data, column) {
  data %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::group_by(record_id, epiweek_recolec) %>%
    dplyr::slice(which.min(!!sym(column)))%>%
    dplyr::select(c(record_id, epiweek_recolec, column))
}

columns_to_process <- c("resul_inf_a_all", 
                        "resul_inf_b_all",
                        "resul_inf_all",
                        "resul_rsv_all", 
                        "resul_sars_all",
                        "resul_covid_19_all", 
                        "resul_sars_covid_all",
                        "resul_virus_all")

# Create a place to store summarized data
summary_dataframes <- list()

# Apply the function to each column and create new dataframes
for (col in columns_to_process) {
  summary_dataframes[[col]] <- filter_group_slice(influenza_muestras, col)
}

# Merge all dataframes together so that we have data per person per week
merged_summary <- Reduce(function(x, y) merge(x, y, by = c("record_id", "epiweek_recolec"), all = TRUE), summary_dataframes)

# Need to make column where we calculate if a person tested negative for all viruses
# Note that most people are tested for every virus over the course of the study
# However, if testing is inconsistent, we may be seeing results not from a different pathogen,
# but only from a pathogen that wasn't tested at the time.

# Also note that we ignore NA values
merged_summary <- merged_summary %>%
  rowwise() %>%
  mutate(resul_neg_all = if_else(all(c_across(all_of(columns_to_process)) == 2, na.rm = TRUE), 1, 0)) %>%
  ungroup()

# Combine positivity with symptom data -----------------------------------------------------------

# Make copy of positivity data for ease
agri_positivity_data <- merged_summary

# Note: The following code is repeated 3 times. It could be made into a function
# For purposes of looking at potential edge cases while writing this code,
# I processed each seperately without using a function
# For the visita síntomatica -------------------------------------------------------
# (will look by date variable over a three week period
# as individuals are only in the study once every three months)

symptomatic_visit <- influenza%>%
  filter(redcap_repeat_instrument=="visita_sintomtica")%>%
  dplyr::select(c(record_id, all_of(columns_agri_sintomas_sintomatica), epiweek_sintomatica))

# Custom function to merge datasets with the date condition
merge_with_date_condition <- function(df1, df2, by = "record_id", date_col1 = "epiweek_recolec", date_col2 = "epiweek_sintomatica", date_diff = 21) {
  df1 <- df1 %>% rename(epiweek1 = !!sym(date_col1))
  df2 <- df2 %>% rename(epiweek2 = !!sym(date_col2))
  
  merged_df <- df1 %>%
    full_join(df2, by = by, relationship="many-to-many") %>%
    filter(abs(difftime(epiweek1, epiweek2, units = "days")) <= date_diff)
  
  return(merged_df)
}

# Apply the function
symptomatic_visit_pos <- merge_with_date_condition(agri_positivity_data,
                                                   symptomatic_visit)

# Define where the symptoms came from
symptomatic_visit_pos$visit_type <- "Semana 0 : Al tiempo de la recoleción de muestra de PCR"

# Make the symptoms binary
symptomatic_visit_pos_bin <- symptomatic_visit_pos %>%
  mutate_at(vars(all_of(columns_agri_sintomas_sintomatica)), ~ ifelse(. > 0, 1, 0))

# Rename all of the columns
symptomatic_visit_pos_bin_renamed <- symptomatic_visit_pos_bin %>%
  dplyr::rename(!!!columns_agri_sintomas_sintomatica_rename)

# For the visita de primera semana --------------------------------------------

semana1_visit <- influenza%>%
  filter(redcap_repeat_instrument=="visita_1_semana")%>%
  dplyr::select(c(record_id, all_of(columns_agri_sintomas_semana1), epiweek_semana1))

# Apply the function (date flexibility maintained at 21 days)
semana1_visit_pos <- merge_with_date_condition(agri_positivity_data,
                                               semana1_visit,
                                               date_col1 = "epiweek_recolec",
                                               date_col2 = "epiweek_semana1",
                                               date_diff = 21)

# Define where the symptoms came from
semana1_visit_pos$visit_type <- "Semana 1: una semana despues de la recoleción"

# Make the symptoms binary
semana1_visit_pos_bin <- semana1_visit_pos %>%
  mutate_at(vars(all_of(columns_agri_sintomas_semana1)), ~ ifelse(. > 0, 1, 0))

# Rename all of the columns
semana1_visit_pos_bin_renamed <- semana1_visit_pos_bin %>%
  dplyr::rename(!!!columns_agri_sintomas_semana1_rename)

# For the visita de cuarta semana ---------------------------------------------

semana4_visit <- influenza%>%
  filter(redcap_repeat_instrument=="visita_4_semanas")%>%
  dplyr::select(c(record_id, all_of(columns_agri_sintomas_semana4), epiweek_semana4))

# Apply the function (date flexibility updated to 42 days)
semana4_visit_pos <- merge_with_date_condition(agri_positivity_data,
                                               semana4_visit,
                                               date_col1 = "epiweek_recolec",
                                               date_col2 = "epiweek_semana4",
                                               date_diff = 42)

# Define where the symptoms came from
semana4_visit_pos$visit_type <- "Semana 4: cuatro semanas despues de la recoleción"

# Make the symptoms binary
semana4_visit_pos_bin <- semana4_visit_pos %>%
  mutate_at(vars(all_of(columns_agri_sintomas_semana4)), ~ ifelse(. > 0, 1, 0))

# Rename all of the columns
semana4_visit_pos_bin_renamed <- semana4_visit_pos_bin %>%
  dplyr::rename(!!!columns_agri_sintomas_semana4_rename)



# Merge all together and clean --------------------------------------------
symptom_summary_pre <- rbind(symptomatic_visit_pos_bin_renamed,
                             semana1_visit_pos_bin_renamed)
symptom_summary <- rbind(symptom_summary_pre, semana4_visit_pos_bin_renamed)

# Replace underscores with spaces in all columns
symptom_summary <- symptom_summary %>%
  mutate_all(~ gsub("_", " ", .))


# Positivity count ----------------------------------------------------------
# Take the dataset and project it to be the long way

generate_summary <- function(data, column) {
  data %>%
    dplyr::group_by(epiweek_recolec) %>%
    dplyr::summarise(
      count_all = sum(!is.na(.data[[column]]), na.rm = TRUE),
      count_neg = sum(.data[[column]] == 2, na.rm = TRUE),
      count_pos = sum(.data[[column]] == 1, na.rm = TRUE),
      count_undetermined = sum(.data[[column]] == 3, na.rm = TRUE),
      count_unprocessed = sum(.data[[column]] == 5, na.rm = TRUE),
      pct_pos = ifelse(count_pos == 0, 0,
                       count_pos / sum(count_neg + count_pos, na.rm = TRUE) * 100)
    ) %>%
    dplyr::mutate(disease = column)
}

# Apply the function to each column and combine results into a long dataframe
summary_combined <- lapply(columns_to_process, function(col) generate_summary(merged_summary, col)) %>%
  dplyr::bind_rows()%>%
  tidyr::pivot_wider(names_from = disease,
                     values_from = c(count_all, count_neg, count_pos, pct_pos))


# Save the summary dataframe -----------------------------------------------
influenza_csv_file <- "docs/influenza_summary_updated.csv"
write.csv(summary_combined, file = influenza_csv_file, row.names = FALSE)
# ---------------------------------------------------------------------------

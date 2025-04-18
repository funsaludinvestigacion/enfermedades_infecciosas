# Load library -------------------------------------------------------------------
library(REDCapR)
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lubridate)
library(digest)

# Load data ----------------------------------------------------------------------
readRenviron(".Renviron")
agri_casa_token = Sys.getenv("agri_casa_token")

uri <- "https://redcap.ucdenver.edu/api/"

agri_casa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = agri_casa_token
  )$data

# agri_casa2 <- agri_casa
# agri_casa <- agri_casa2
agri_casa$epiweek_v_rutina <- lubridate::floor_date(agri_casa$fecha_visita_vig_rut, unit = "week", week_start = 1)

#los ultimos dos numeros del id corresponde al individuo, y el primero cinco a la familia
# Crear un ID de familia
agri_casa$id_familia <- substr(agri_casa$record_id, 1, 5)


# CREATE A VIRUS INCIDENCE TRACKER DATASET---------------------------------------

# Begin processing pcr positive data --------------------------------------------------

# Crear fecha
agri_casa$epiweek_muestra_funsalud <-lubridate::floor_date(agri_casa$fecha_recoleccion_m, unit = "week", week_start = 1)

# Look at singleplex results (symtomatic and visitas intensivas) -------------
# Only contains information for SARS-COV-2

# Make singleplex repetition numeric and match the code for the COBAS results
agri_casa$ctgen_rep_resul_num <- ifelse(
  is.na(agri_casa$ctgen_rep_resul), NA,
  ifelse(
    agri_casa$ctgen_rep_resul=="POSITIVO", 1,
    ifelse(
      agri_casa$ctgen_rep_resul=="NEGATIVO", 2,
      ifelse(
        agri_casa$ctgen_rep_resul=="INVÁLIDO" | agri_casa$ctgen_rep_resul=="INCONCLUSO", 3, NA)
    )
  )
)

# Create overall numeric variable using original and repeated results
agri_casa$vctg_resul_num <- ifelse(
  is.na(agri_casa$vctg_resul) & is.na(agri_casa$ctgen_rep_resul_num), NA,
  ifelse(
    agri_casa$vctg_resul == "POSITIVO", 1,
    ifelse(
      agri_casa$vctg_resul == "NEGATIVO", 2,
      ifelse(
        (agri_casa$vctg_resul == "INVÁLIDO" | agri_casa$vctg_resul == "INCONCLUSO") & !is.na(agri_casa$ctgen_rep_resul_num),
        agri_casa$ctgen_rep_resul_num,
        ifelse(
          (agri_casa$vctg_resul == "INVÁLIDO" | agri_casa$vctg_resul == "INCONCLUSO") & is.na(agri_casa$ctgen_rep_resul_num),
          3,
          NA)
      )
    )
  )
)


# Check that no information was lost
# table(is.na(agri_casa$vctg_resul_num))[1] >= table(is.na(agri_casa$vctg_resul))[1]

# Look at multiplex results (symtomatic and visitas intensivas) -------------
# Create overall numeric variable using original and repeated results

# Make multiplex repetition numeric and match the code for the COBAS results
#### Note that this does not include QIAGEN results because those were never done
# as of 16-01-2025. Those variables would need to be added in res_mp_infa_2, res_mp_infa_4
# res_mp_infb_2, res_mp_infb_4, res_mp_cov_2, res_mp_cov_4

# For infa variables
agri_casa$multiInfArep <- ifelse(
  is.na(agri_casa$res_mp_infa_3), NA,
  ifelse(
    agri_casa$res_mp_infa_3=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_infa_3=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_infa_3=="INVÁLIDO" | agri_casa$res_mp_infa_3 =="INCONCLUSO", 3, NA)
    )
  )
)

agri_casa$res_mp_infa <- ifelse(
  is.na(agri_casa$res_mp_infa), NA,
  ifelse(
    agri_casa$res_mp_infa=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_infa=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_infa=="INVÁLIDO" | agri_casa$res_mp_infa_3 =="INCONCLUSO", 3, NA)
    )
  )
)

# Create overall numeric variable using original and repeated results
agri_casa$multiplex_infa <- ifelse(
  is.na(agri_casa$res_mp_infa) & is.na(agri_casa$multiInfArep), NA,
  ifelse(
    !is.na(agri_casa$res_mp_infa) & is.na(agri_casa$multiInfArep), agri_casa$res_mp_infa,
    ifelse(
      !is.na(agri_casa$res_mp_infa) & !is.na(agri_casa$multiInfArep), 
      pmin(agri_casa$res_mp_infa, agri_casa$multiInfArep, na.rm = TRUE),
      NA
    )
  )
)


# For infb variables
agri_casa$multiInfBrep <- ifelse(
  is.na(agri_casa$res_mp_infb_3), NA,
  ifelse(
    agri_casa$res_mp_infb_3=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_infb_3=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_infb_3=="INVÁLIDO" | agri_casa$res_mp_infb_3 =="INCONCLUSO", 3, NA)
    )
  )
)

agri_casa$res_mp_infb <- ifelse(
  is.na(agri_casa$res_mp_infb), NA,
  ifelse(
    agri_casa$res_mp_infb=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_infb=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_infb=="INVÁLIDO" | agri_casa$res_mp_infb_3 =="INCONCLUSO", 3, NA)
    )
  )
)

# Create overall numeric variable using original and repeated results
agri_casa$multiplex_infb <- ifelse(
  is.na(agri_casa$res_mp_infb) & is.na(agri_casa$multiInfBrep), NA,
  ifelse(
    !is.na(agri_casa$res_mp_infb) & is.na(agri_casa$multiInfBrep), agri_casa$res_mp_infb,
    ifelse(
      !is.na(agri_casa$res_mp_infb) & !is.na(agri_casa$multiInfBrep), 
      pmin(agri_casa$res_mp_infb, agri_casa$multiInfBrep, na.rm = TRUE),
      NA
    )
  )
)

# For covid variables
agri_casa$multiCOVrep <- ifelse(
  is.na(agri_casa$res_mp_covid_3), NA,
  ifelse(
    agri_casa$res_mp_covid_3=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_covid_3=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_covid_3=="INVÁLIDO" | agri_casa$res_mp_covid_3 =="INCONCLUSO", 3, NA)
    )
  )
)

agri_casa$res_mp_covid <- ifelse(
  is.na(agri_casa$res_mp_covid), NA,
  ifelse(
    agri_casa$res_mp_covid=="POSITIVO", 1,
    ifelse(
      agri_casa$res_mp_covid=="NEGATIVO", 2,
      ifelse(
        agri_casa$res_mp_covid=="INVÁLIDO" | agri_casa$res_mp_covid =="INCONCLUSO", 3, NA)
    )
  )
)

# Create overall numeric variable using original and repeated results
agri_casa$multiplex_covid <- ifelse(
  is.na(agri_casa$res_mp_covid) & is.na(agri_casa$multiCOVrep), NA,
  ifelse(
    !is.na(agri_casa$res_mp_covid) & is.na(agri_casa$multiCOVrep), agri_casa$res_mp_covid,
    ifelse(
      !is.na(agri_casa$res_mp_covid) & !is.na(agri_casa$multiCOVrep), 
      pmin(agri_casa$res_mp_covid, agri_casa$multiCOVrep, na.rm = TRUE),
      NA
    )
  )
)


# Look at COBAS results (PCR for symptomatics) and singleplex --------------------------------
# Issue 1: How to deal with follow-up results (if sample is unprocessed or indeterminate)
# We can create a new column for this
create_new_column_agri_casa <- function(df, col1, col2, new_col) {
  df[[new_col]] <- ifelse(is.na(df[[col1]]) & is.na(df[[col2]]), NA,
                          ifelse(df[[col1]] == 1, 1,
                                 ifelse(df[[col1]] == 2, 2,
                                        ifelse(df[[col1]] == 3 & is.na(df[[col2]]), 3,
                                               ifelse(df[[col1]] == 4 & is.na(df[[col2]]), 4,
                                                      # Use second result if first results are inconclusive or invalid
                                                      # If anything else, then put NA
                                                      ifelse(df[[col1]] >= 3 & !is.na(df[[col2]]), df[[col2]], NA)
                                               )
                                        )
                                 )
                          )
  )
  return(df)
}

# Columns to iterate over
columns_to_iterate_agricasa <- list(
  list("sars_cov2", "sarvs_cov2_repit", "sars_cov2_all_funsalud"),
  list("inf_a", "inf_a_repit", "inf_a_all_funsalud"),
  list("inf_b", "inf_b_repit", "inf_b_all_funsalud"),
  list("vsr", "vsr_repit", "vsr_all_funsalud"))

# Apply the function to each pair of columns
for (cols in columns_to_iterate_agricasa) {
  agri_casa <- create_new_column_agri_casa(agri_casa, cols[[1]], cols[[2]], cols[[3]])
}

# Look at VSR results only? NO HAY NADA EN ESTA SECCION ----------------------
# table(agri_casa$pcr_srv, useNA = "always")

# Find positive or negative status by week per individual ----------------------
# Issue 2: What if the same individual is tested multiple times in the same week?
# We don't care if the result is the same
# We do care if one is positive (always prefer the positive value)
# Since the minimum value is the preferred data type (1 = positive, 2 = negative), select for minimum value
filter_group_slice_agri_casa <- function(data, column) {
  data %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::group_by(record_id, epiweek_muestra_funsalud) %>%
    dplyr::slice(which.min(!!sym(column))) %>%
    dplyr::select(all_of(c("record_id", "epiweek_muestra_funsalud", column)))
}

columns_to_process_agri_casa <- c("sars_cov2_all_funsalud",
                                  "inf_a_all_funsalud",
                                  "inf_b_all_funsalud",
                                  "vsr_all_funsalud",
                                  "vctg_resul_num",
                                  "multiplex_infa",
                                  "multiplex_infb",
                                  "multiplex_covid")

# Create a place to store summarized data
summary_dataframes_agri_casa <- list()

# Apply the function to each column and create new dataframes
for (col in columns_to_process_agri_casa) {
  summary_dataframes_agri_casa[[col]] <- filter_group_slice_agri_casa(agri_casa, col)
}

# Merge all dataframes together so that we have data per person per week
merged_summary_agri_casa <- Reduce(function(x, y) merge(x, y, by = c("record_id", "epiweek_muestra_funsalud"),
                                                        all = TRUE), summary_dataframes_agri_casa)

# Create a combined column for sars_cov2 using singleplex and COBAS results---------------
#merged_summary_agri_casa <- merged_summary_agri_casa%>%
 # dplyr::mutate(sars_cov2_singleplex_cobas = ifelse(is.na(vctg_resul_num) &
  #                                                              is.na(sars_cov2_all_funsalud), NA,
   #                                                           pmin(vctg_resul_num, sars_cov2_all_funsalud, na.rm = TRUE)))


# Create a combined column for multiplex_covid using singleplex and COBAS results ---------------
merged_summary_agri_casa <- merged_summary_agri_casa %>%
  dplyr::mutate(
    # Combine sars_cov2 from cobas with singleplex_cobas multiplex_covid
    sars_cov2_combined = ifelse(
      is.na(vctg_resul_num) & is.na(sars_cov2_all_funsalud) & 
        is.na(multiplex_covid), 
      NA, 
      pmin(
        pmin(vctg_resul_num, sars_cov2_all_funsalud, na.rm = TRUE), 
        multiplex_covid, 
        na.rm = TRUE
      )),
    # Combine multiplex_infa with inf_a_all_funsalud
    multiplex_infa_combined = ifelse(
      is.na(inf_a_all_funsalud) & is.na(multiplex_infa), 
      NA, 
      pmin(inf_a_all_funsalud, multiplex_infa, na.rm = TRUE)
    ),
    # Combine multiplex_infb with inf_b_all_funsalud
    multiplex_infb_combined = ifelse(
      is.na(inf_b_all_funsalud) & is.na(multiplex_infb), 
      NA, 
      pmin(inf_b_all_funsalud, multiplex_infb, na.rm = TRUE)
    )
  )

# Begin processing intensive visit data --------------------------------------------------

# ISSUE 3: PCR won't always capture everyone.
# It is better to check how many people went through a visita intensiva

# Start by filtering for the first visit in the first week
first_intensive_df <- agri_casa%>%
  # Filter for first visit on first week, with subject = 1 (patient)
  dplyr::filter(semana_seg_visit_intens==1 & num_vist_sem_visit_intens==1 & tipo_sujeto_visit_intens==1)

# Create a date column for the positive test by week
first_intensive_df$date_positive_intens <- ifelse(!is.na(first_intensive_df$fecha_prue_visit_intens),
                                                  first_intensive_df$fecha_prue_visit_intens,
                                                  ifelse( !is.na(first_intensive_df$fecha_visit_intens), 
                                                          first_intensive_df$fecha_visit_intens, NA))

first_intensive_df$date_positive_intens <- as.Date(first_intensive_df$date_positive_intens, origin = "1970-01-01")

# Create an epiweek column to obscure personal protective information
first_intensive_df$epiweek_positive_intes <- lubridate::floor_date(first_intensive_df$date_positive_intens, unit = "week", week_start = 1)


# Create denominator -----------------------------------------------------------------------------
# ISSUE 4: We need a denominator. How many people were considered/eligible for the visita intensiva or a pcr test?
# We can look by week at the total number of individuals undergoing visita intensiva y vigilancia de rutina
# if PCR test, then underwent a visita de rutina or a visita intensiva around the same time.
# We will not use total number of PCR tests as the denominator, as PCR tests are only run if an individual is symptomatic
# or a part of the visita intensiva (someone else in their household is symptomatic)

incidence_intens_denominator <- agri_casa%>%
  # Filter for accomplished vigilancia de rutina or accomplished visita intensiva
  dplyr::filter(realizado_vig_rut==1 | se_realizo_visit_intens==1)

# Rows for vigilancia de rutina and visita intensivas are mutually exclusive
# table(incidence_intens_denominator$realizado_vig_rut, incidence_intens_denominator$se_realizo_visit_intens, useNA = "always")

# Create date variable and epiweek variable based on routine and intensive visits
incidence_intens_denominator_count <- incidence_intens_denominator%>%
  mutate(date_denominator = ifelse(realizado_vig_rut==1, fecha_visita_vig_rut,
                                                        ifelse(se_realizo_visit_intens==1, fecha_visit_intens, NA)),
         epiweek_denominator = lubridate::floor_date(as.Date(date_denominator, origin = "1970-01-01"), unit = "week", week_start = 1))%>%
  # select one row per individual per week
  dplyr::group_by(record_id, epiweek_denominator)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  # count number of individuals
  dplyr::group_by(epiweek_denominator)%>%
  dplyr::summarise(denominator = n_distinct(record_id, na.rm = TRUE))


# Combine PCR and Intensive Data ----------------------------------------------------------------

merged_summary_agri_casa_subset <- merged_summary_agri_casa %>%
  dplyr::select("record_id",
                "epiweek_muestra_funsalud",
                "sars_cov2_combined",
                "multiplex_infa_combined",
                "multiplex_infb_combined",
                "vsr_all_funsalud")

# Separate the enfermedad_activacion column into three separate columns
# a no is a 2, to match the coding from funsalud pcr tests
first_intensive_df_subset <- first_intensive_df%>%
  dplyr::select(record_id, epiweek_positive_intes,
                enfermedad_activacion)%>%
  dplyr::mutate(sars_cov_intens = ifelse(is.na(enfermedad_activacion), NA,
    ifelse(enfermedad_activacion==1, 1, 2)),
    influenza_intens = ifelse(is.na(enfermedad_activacion), NA,
                              ifelse(enfermedad_activacion==2, 1, 2)),
    vsr_intens = ifelse(is.na(enfermedad_activacion), NA,
                              ifelse(enfermedad_activacion==3, 1, 2)))

pcr_intens_visit_incidence_summary_pre <- merge(merged_summary_agri_casa_subset, first_intensive_df_subset, by.x=c("record_id", "epiweek_muestra_funsalud"),
      by.y=c("record_id", "epiweek_positive_intes"), all=TRUE)


# Create overall columns
pcr_intens_visit_incidence_summary <- pcr_intens_visit_incidence_summary_pre%>%
  dplyr::mutate(
    sars_cov2_all = pmin(sars_cov2_combined, sars_cov_intens, na.rm = TRUE),
    influenza_all = pmin(multiplex_infa_combined, multiplex_infb_combined, influenza_intens, na.rm = TRUE),
    vsr_all = pmin(vsr_all_funsalud, vsr_intens, na.rm = TRUE)
  )%>%
  # because our denominator is coming from another dataset (total number of individuals with routine and intensive visits)
  # we can filter out any rows where there is not a 1 (positive result) in our columns of interest
  dplyr::filter((sars_cov2_all==1 | influenza_all==1 | vsr_all==1))


# Clean data to only include NEW cases -----------------------------------------
# ISSUE 5: we also want to filter out any results where a person was positive the previous three weeks
# We already have only ONE value per week (the minimum value)
# We filter because we are only looking at NEW infections
generate_summary_agri_casa <- function(data, column) {
  # Ensure epiweek_muestra_funsalud is in Date format
  data <- data %>%
    mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud, origin = "1970-01-01"))
  
  # Arrange data by record_id and epiweek_muestra_funsalud
  data <- data %>%
    dplyr::arrange(record_id, epiweek_muestra_funsalud)
  
  data <- data %>%
    dplyr::arrange(record_id, epiweek_muestra_funsalud)
  
  # Create a lagged column to check the value in the previous week
  data <- data %>%
    group_by(record_id) %>%
    mutate(last_record_date = lag(epiweek_muestra_funsalud, order_by = epiweek_muestra_funsalud),
           last_record_value = lag(.data[[column]], order_by = epiweek_muestra_funsalud),
           three_weeks_ago_date = epiweek_muestra_funsalud - 21) %>%
    ungroup()
  
  # Keep rows where the last_record_date is NA (have not been positive before)
  # Keep rows where the last record occurred more than three weeks ago
  data <- data %>%
    dplyr::filter(is.na(last_record_date) | (
      last_record_date <= three_weeks_ago_date))

  # Generate the summary
  summary <- data %>%
    group_by(epiweek_muestra_funsalud) %>%
    summarise(
      count_pos = sum(.data[[column]]==1, na.rm = TRUE)
    ) %>%
    mutate(disease = column)
  
  return(summary)
}

# Columns to process
columns_to_process_agri_casa <- c("sars_cov2_all",
                                  "influenza_all",
                                  "vsr_all")

# Apply the function to each column and combine results into a long dataframe
summary_pos_agri_casa <- lapply(columns_to_process_agri_casa,
                                     function(col) generate_summary_agri_casa(pcr_intens_visit_incidence_summary, col)) %>%
  dplyr::bind_rows()%>%
  tidyr::pivot_wider(names_from = disease,
                     values_from = c(count_pos))


# Combine the denominator by epiweek with the positive cases-----------
summary_pos_denom_agri_casa <- merge(summary_pos_agri_casa, incidence_intens_denominator_count,
                                    by.x="epiweek_muestra_funsalud", by.y="epiweek_denominator", all=TRUE)%>%
  dplyr::filter(! is.na(epiweek_muestra_funsalud))%>%
  # NAs should be 0 because if NA, means no disease detected that week
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)),
         epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud, origin = "1970-01-01"))%>%
  # Create overall virus of interest counter
  mutate(virus_all = rowSums(across(c(sars_cov2_all, influenza_all, vsr_all)), na.rm = TRUE))



# Look at ILI syndrome counts--------------------------------------
# Create epiweek
agri_casa$epiweek_v_rutina <- lubridate::floor_date(agri_casa$fecha_visita_vig_rut, unit = "week", week_start = 1)


# Create a dataset for ILI syndromic illness (difficulty breathing, fever, cough)
# We need to match the symptoms in the routine visits with the symptoms in the intensive visits
agri_symptoms_ILI <- agri_casa%>%
    dplyr::select("record_id",
                  "fecha_visita_vig_rut",
                  "realizado_vig_rut",
                  "fecha_visit_intens",
                  "se_realizo_visit_intens",
                  "tos_visit_ints",
                  "tos_flm_visit_ints",
                  "sensa_fiebre_visit_ints",
                  "falta_aire_visit_ints",
                  "tos_vig_rut",
                  "fiebre_vig_rut",
                  "dif_resp_vig_rut")%>%
  # only those with data are included
  dplyr::filter(se_realizo_visit_intens == 1 | realizado_vig_rut == 1)%>%
  # create an epiweek variable
  mutate(date_symptoms = ifelse(realizado_vig_rut==1 & is.na(se_realizo_visit_intens), fecha_visita_vig_rut,
                                   ifelse(se_realizo_visit_intens==1 & is.na(realizado_vig_rut), fecha_visit_intens, NA)),
         epiweek_symptoms = lubridate::floor_date(as.Date(date_symptoms, origin = "1970-01-01"), unit = "week", week_start = 1))%>%
  dplyr::group_by(epiweek_symptoms)%>%
  dplyr::summarise(tos_count = n_distinct(record_id[tos_visit_ints >= 2 | tos_flm_visit_ints >= 2 | tos_vig_rut == 1]),
                   fiebre_count = n_distinct(record_id[sensa_fiebre_visit_ints >= 2 | fiebre_vig_rut == 1]),
                   falta_aire_count = n_distinct(record_id[falta_aire_visit_ints >= 2 | dif_resp_vig_rut == 1]),
                   total_ili_count = n_distinct(record_id[tos_visit_ints >= 2 | tos_flm_visit_ints >= 2 | tos_vig_rut == 1 |
                                                            sensa_fiebre_visit_ints >= 2 | fiebre_vig_rut == 1 |
                                                            falta_aire_visit_ints >= 2 | dif_resp_vig_rut == 1]))



# Combine with testing data and denominators
summary_combined_agri_casa <- merge(summary_pos_denom_agri_casa, agri_symptoms_ILI,
                                    by.x="epiweek_muestra_funsalud", by.y="epiweek_symptoms", all=TRUE)

# Save the summary dataframe--------------------------------------
#agri_casa_csv_file <- "/Users/gabigionet/Library/CloudStorage/OneDrive-UCB-O365/PhD_Projects/Trifinio/Guatemala_Infectious_Incidence/docs/agri_casa_summary_updated.csv"
agri_casa_csv_file <- "docs/agri_casa_summary_updated.csv"
write.csv(summary_combined_agri_casa,
          file = agri_casa_csv_file, row.names = FALSE)


# CREATE A SYMPTOM TRACKER DATASET--------------------------------------------

# The goal here is to look at how many people have experienced the following symptoms in the last 24 hours
# Please note that this does not mean NEW symptoms; symptoms could have carried over from the last week
columns_sintomas_vigilancia_rutina <- c("tos_vig_rut", "dol_gargan_vig_rut", "dol_cabeza_vig_rut",
                                        "cong_nasal_vig_rut", "fiebre_vig_rut", "dol_cuerp_musc_vig_rut",
                                        "fatica_vig_rut", "vomitos_vig_rut", "diarrea_vig_rut",
                                        "dif_resp_vig_rut", "perd_olf_gust_vig_rut", "nausea_vig_rut",
                                        "sibilancias_vig_rut", "mala_alim_vig_rut", "letargo_vig_rut")

columns_sintomas_v_intens <- c("tos_visit_ints",
                               "tos_flm_visit_ints",
                               "gargt_irrit_visit_ints",
                               "dlr_cabeza_visit_ints",
                               "congest_nasal_visit_ints",
                               "sensa_fiebre_visit_ints",
                               "dlr_cuerp_dgnl_visit_ints",
                               "fatiga_visit_ints",
                               "dlr_cuello_visit_ints",
                               "interrup_sue_visit_ints",
                               "silbd_resp_visit_ints",
                               "falta_aire_visit_ints",
                               "perd_apet_visit_ints",
                               "nausea_visit_ints",
                               "diarrea_visit_ints",
                               "vomito_visit_ints",
                               "dism_gust_visit_ints",
                               "dism_olf_visit_ints",
                               "dism_aud_visit_ints",
                               "dism_bal_visit_ints",
                               "mala_alim_visit_ints",
                               "letargo_visit_ints"
)

# Select only relevant symptoms (recorded in intensive and routine visits)
agri_casa_symptoms <- agri_casa%>%
  dplyr::select(c("record_id",
                  fecha_visita_vig_rut,
                  realizado_vig_rut,
                  fecha_visit_intens,
                  se_realizo_visit_intens,
                  all_of(columns_sintomas_vigilancia_rutina),
                  all_of(columns_sintomas_v_intens)))%>%
  dplyr::filter(se_realizo_visit_intens == 1 | realizado_vig_rut == 1)%>%
  # create an epiweek variable
  mutate(date_symptoms = ifelse(realizado_vig_rut==1 & is.na(se_realizo_visit_intens), fecha_visita_vig_rut,
                                ifelse(se_realizo_visit_intens==1 & is.na(realizado_vig_rut), fecha_visit_intens, NA)),
         epiweek_symptoms = lubridate::floor_date(as.Date(date_symptoms, origin = "1970-01-01"), unit = "week", week_start = 1))


# ISSUE 6: We don't know what someone will choose on the symptom tracker (there are a lot of possible combinations!)
# Need to save the dataset by individual, as one individual can have more than one of the symptoms specified here at the same time
# Otherwise, we would double-count that person.
# However the epiweek denominator shouldn't change, so we will want to export that in our summary dataset just for ease
# Add denominator -----------------------------------
agri_casa_symptoms_summary = merge(agri_casa_symptoms, incidence_intens_denominator_count,
                                   by.x="epiweek_symptoms", by.y="epiweek_denominator", all=TRUE)%>%
  dplyr::filter(! is.na(epiweek_symptoms))%>%
  # NAs should be 0 because if NA, means no disease detected that week
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)),
         epiweek_symptoms = as.Date(epiweek_symptoms, origin = "1970-01-01"))%>%
  # Eliminate dates if possible
  dplyr::select(-c(fecha_visita_vig_rut, realizado_vig_rut, fecha_visit_intens, se_realizo_visit_intens))


# NOTE: we did not include individuals who went through the seguimiento largo, per Dan's thoughts that
# there were not many illnesses there or symptoms.
# if we need to increase our denominator, it would be most accurate to use data from the seguimiento largo
# however, in theory, it should not change the overall positivity rate of symptoms 
# (only some participants are at risk of disease and population should be equal to vigilancia de rutina)

# For additional data security, we will re-code participants-----------------------

# Hash the record_id column to create anonymized IDs
# We use the sha256 algorithm because it is deterministic
agri_casa_symptoms_summary_anonymized <- agri_casa_symptoms_summary %>%
  mutate(
    anonymized_id = sapply(record_id, function(x) digest::digest(x, algo = "sha256"))
  )%>%
  dplyr::select(-record_id)

# Save the summary dataframe------------------------------------
agri_casa_symptom_csv_file <- "docs/agri_casa_symptom_summary_updated.csv"
write.csv(agri_casa_symptoms_summary_anonymized, file = agri_casa_symptom_csv_file, row.names = FALSE)


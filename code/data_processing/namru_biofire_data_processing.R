# Load libraries -----------------------------------------------------------
library(REDCapR)
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lubridate)
library(digest)
library(tidyr)
library(dplyr)
library(patchwork)

# Load data -----------------------------------------------------------------
namru_biofire_token <- Sys.getenv("namru_biofire_token")
new_biofire <- Sys.getenv("new_biofire")

uri <- "https://redcap.ucdenver.edu/api/"

namru_biofire <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = namru_biofire_token
  )$data

# New biofire data base ---------------------------------------------------------
# As of 1/1/2025 we have a new biofire database, here I join it to the old one
# because many columns are the same.
namru_biofire_new <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token = new_biofire
  )$data

# Get common and unique column names
common_cols <- intersect(names(namru_biofire), names(namru_biofire_new))
unique_cols_old <- setdiff(names(namru_biofire), common_cols)
unique_cols_new <- setdiff(names(namru_biofire_new), common_cols)

# Add missing columns to namru_biofire_new (with NA)
for (col in unique_cols_old) {
  namru_biofire_new[[col]] <- NA  
}

# Add missing columns to namru_biofire (with NA)
for (col in unique_cols_new) {
  namru_biofire[[col]] <- NA  
}

# Reorder namru_biofire_new to match namru_biofire
namru_biofire_new <- namru_biofire_new[, names(namru_biofire)]

for (col in names(namru_biofire)) {
  if (col %in% unique_cols_new) {
    if (!identical(class(namru_biofire[[col]]), class(namru_biofire_new[[col]]))) {
      target_type <- class(namru_biofire_new[[col]])[1]  # Take the first class if multiple exist
      
      if (inherits(namru_biofire_new[[col]], "character")) {
        namru_biofire[[col]] <- as.character(namru_biofire[[col]])
      } else if (inherits(namru_biofire_new[[col]], "double") | inherits(namru_biofire_new[[col]], "numeric")) {
        namru_biofire[[col]] <- as.numeric(namru_biofire[[col]])
      } else if (inherits(namru_biofire_new[[col]], "integer")) {
        namru_biofire[[col]] <- as.integer(namru_biofire[[col]])
      } else if (inherits(namru_biofire_new[[col]], "logical")) {
        namru_biofire[[col]] <- as.logical(namru_biofire[[col]])
      } else if (inherits(namru_biofire_new[[col]], "Date")) {
        namru_biofire[[col]] <- as.Date(namru_biofire[[col]], origin = "1970-01-01")
      } else {
        message(paste("Skipping unknown type for column:", col))
      }
    }
  }
}

# Convert columns in namru_biofire_new to match namru_biofire's types
for (col in names(namru_biofire)) {
  if (!identical(class(namru_biofire[[col]]), class(namru_biofire_new[[col]])) & !(col %in% unique_cols_new)) {
    target_type <- class(namru_biofire[[col]])[1]  # Ensure a single class is used
    
    if (inherits(namru_biofire[[col]], "character")) {
      namru_biofire_new[[col]] <- as.character(namru_biofire_new[[col]])
    } else if (inherits(namru_biofire[[col]], "double") | inherits(namru_biofire[[col]], "numeric")) {
      namru_biofire_new[[col]] <- as.numeric(namru_biofire_new[[col]])
    } else if (inherits(namru_biofire[[col]], "integer")) {
      namru_biofire_new[[col]] <- as.integer(namru_biofire_new[[col]])
    } else if (inherits(namru_biofire[[col]], "logical")) {
      namru_biofire_new[[col]] <- as.logical(namru_biofire_new[[col]])
    } else if (inherits(namru_biofire[[col]], "Date")) {
      namru_biofire_new[[col]] <- as.Date(namru_biofire_new[[col]], origin = "1970-01-01")
    } else {
      message(paste("Skipping unknown type for column:", col))
    }
  }
}

# Now bind rows safely
namru_biofire <- bind_rows(namru_biofire, namru_biofire_new)

# Create epiweek--------------------------------------------------------------
namru_biofire$epiweek_recoleccion <- lubridate::floor_date(namru_biofire$fecha_recoleccion, unit = "week", week_start = 1)

# Is each individual currently mentioned once in the dataset? - WHAT WOULD YOU DO IF NOT? AGRI/AGRICASA HAVE MULTIPLE
nrow(unique(namru_biofire%>%
              dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)))%>%
              select(record_id))
     )==nrow(namru_biofire%>%
               dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)))%>%
               select(record_id))

# Find columns with pathogen results -----------------------------------------
# Columns of interest
columns_of_interest_biofire <- c(
  "res_dengue",
  "res_dengue_2",
  "dengue_igg",
  "dengue_igg_2",
  "dengue_igm",
  "dengue_igm_2",
  "patogenos_positivos_sangre___1",
  "patogenos_positivos_sangre___2",
  "patogenos_positivos_sangre___3",
  "patogenos_positivos_sangre___4",
  "patogenos_positivos_sangre___5",
  "patogenos_positivos_sangre___6",
  "patogenos_positivos_sangre___7",
  "patogenos_positivos_sangre___8",
  "patogenos_positivos_sangre___9",
  "patogenos_positivos_sangre___10",
  "patogenos_positivos_sangre___11",
  "patogenos_positivos_sangre___12",
  "patogenos_positivos_sangre___13",
  "patogenos_positivos_sangre___14",
  "patogenos_positivos_sangre___15",
  "patogenos_positivos_sangre___16",
  "patogenos_positivos_sangre___17",
  "patogenos_positivos_sangre___18",
  "patogenos_positivos_sangre___19",
  "patogenos_positivos_hisnaso___1",
  "patogenos_positivos_hisnaso___2",
  "patogenos_positivos_hisnaso___3",
  "patogenos_positivos_hisnaso___4",
  "patogenos_positivos_hisnaso___5",
  "patogenos_positivos_hisnaso___6",
  "patogenos_positivos_hisnaso___7",
  "patogenos_positivos_hisnaso___8",
  "patogenos_positivos_hisnaso___9",
  "patogenos_positivos_hisnaso___10",
  "patogenos_positivos_hisnaso___11",
  "patogenos_positivos_hisnaso___12",
  "patogenos_positivos_hisnaso___13",
  "patogenos_positivos_hisnaso___14",
  "patogenos_positivos_hisnaso___15",
  "patogenos_positivos_hisnaso___16",
  "patogenos_positivos_hisnaso___17",
  "patogenos_positivos_hisnaso___18",
  "patogenos_positivos_hisnaso___19",
  "patogenos_positivos_hisnaso___20",
  "patogenos_positivos_hisnaso___21"
)

# Output data ------------------------------------------------------------------
# We want to output a list with the epiweek of a given test, the result (neg or pos), the pathogen, and the coded ID.
# Find total amount of tests run, as determined by result_sangre_complt and result_hispd_nasof
# Por ahora, excluimos los heces (no son colectados)
# table(namru_biofire$result_heces)

namru_biofire_subset <- namru_biofire%>%
  dplyr::select(record_id,
                #id_agri_lab,
                #id_agricasa_lab,
                #id_norovirus_lab,
                #id_pulsoximetria_lab,
                #id_xeno_lab, 
                epiweek_recoleccion,
                res_dengue,
                res_dengue_2,
                dengue_igg,
                dengue_igg_2,
                dengue_igm,
                dengue_igm_2,
                result_sangre_complt,
                result_hispd_nasof,
                all_of(columns_of_interest_biofire))%>%
  dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)) | (! is.na(res_dengue)))

# Clean data to only include NEW cases -----------------------------------------
# We want to filter out any results where a person was positive the previous three weeks
# We already have only ONE value per week (the minimum value)
# We filter because we are only looking at NEW infections
generate_summary_biofire <- function(data, column) {
  # Ensure epiweek_recoleccion is in Date format
  data <- data %>%
    mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion, origin = "1970-01-01"))
  
  # Arrange data by record_id and epiweek_recoleccion
  data <- data %>%
    dplyr::arrange(record_id, epiweek_recoleccion)
  
  # Create a lagged column to check the value in the previous week
  data <- data %>%
    group_by(record_id) %>%
    mutate(last_record_date = lag(epiweek_recoleccion, order_by = epiweek_recoleccion),
           last_record_value = lag(.data[[column]], order_by = epiweek_recoleccion),
           diff_last_record_date = epiweek_recoleccion - last_record_date) %>%
    ungroup()%>%
    # Replace a 1 with a 0 if the last_record_date with a 1 in that column occurred less than three weeks ago
    mutate(!!sym(column) := ifelse(
      (diff_last_record_date < 21) & (last_record_value==1) & (.data[[column]] == 1), 0, .data[[column]]))
  
  return(data)
}

# Apply the function to each column and combine results into a long dataframe
# Initialize the result with the original data
namru_biofire_summary <- namru_biofire_subset

# Loop through each column and apply the generate_summary_biofire function
for (column in columns_of_interest_biofire) {
  temp_result <- generate_summary_biofire(namru_biofire_subset, column)
}

# combine the repeat with the original
namru_biofire_summary$res_dengue <- ifelse(is.na(namru_biofire_summary$res_dengue_2), namru_biofire_summary$res_dengue,
                                          ifelse(!is.na(namru_biofire_summary$res_dengue_2) & namru_biofire_summary$res_dengue_2 != 3, namru_biofire_summary$res_dengue_2, NA))

namru_biofire_summary$dengue_igg <- ifelse(is.na(namru_biofire_summary$dengue_igg_2), namru_biofire_summary$dengue_igg,
                                           ifelse(!is.na(namru_biofire_summary$dengue_igg_2) & namru_biofire_summary$dengue_igg_2 != 3, namru_biofire_summary$dengue_igg_2, NA))

namru_biofire_summary$dengue_igm <- ifelse(is.na(namru_biofire_summary$dengue_igm_2), namru_biofire_summary$dengue_igm,
                                           ifelse(!is.na(namru_biofire_summary$dengue_igm_2) & namru_biofire_summary$dengue_igm_2 != 3, namru_biofire_summary$dengue_igm_2, NA))

namru_biofire_summary <- namru_biofire_summary %>%
  select(-res_dengue_2, -dengue_igg_2, -dengue_igm_2)

# For additional data security, we will re-code participants-----------------------

# Hash the record_id column to create anonymized IDs
# We use the sha256 algorithm because it is deterministic
namru_biofire_summary_anonymized <- namru_biofire_summary %>%
  mutate(
    anonymized_id = sapply(record_id, function(x) digest::digest(x, algo = "sha256"))
  )%>%
  dplyr::select(-record_id)

# Save file ---------------------------------------------------
namru_biofire_csv_file <- "docs/namru_biofire_summary_updated.csv"
write.csv(namru_biofire_summary_anonymized, file = namru_biofire_csv_file, row.names = FALSE)


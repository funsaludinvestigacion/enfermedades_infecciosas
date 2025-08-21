library(sf)          # for spatial data
library(dplyr)
library(stringi)
library(fuzzyjoin)

# load in and correct the map --------------------------------------------------
guate_json <- st_read("code/spatial/agrip_04_Limites_municipales_340.json", quiet = TRUE)

guate_json <- guate_json %>%
  st_set_crs(32616) %>%
  rename(department = departamen) %>%
  mutate(
    municipio = municipio %>%
      tolower() %>%
      stri_trans_general("Latin-ASCII") %>%
      trimws(),
    
    department = department %>%
      tolower() %>%
      stri_trans_general("Latin-ASCII") %>%
      trimws()
  ) %>%
  st_transform(4326)

# get only necessary spatial columns from guate_json
guate_json <- guate_json %>%
  select(municipio, department, shape_leng, shape_area, geometry)

# adjust vigiFINCA data and write it out ----------------------------------------

# Read vigifinca summary data
vigifinca_summary <- read.csv("docs/vigifinca_summary.csv", stringsAsFactors = FALSE) %>%
  #rename(municipio = municipio_force) %>%
  mutate(
    municipio = municipio %>%
      tolower() %>%
      stri_trans_general("Latin-ASCII") %>%
      trimws()
    #,
    
    #department = department %>%
     # tolower() %>%
      #stri_trans_general("Latin-ASCII") %>%
      #trimws()
  )

# adjust a few manually that don't have a match
vigifinca_summary$municipio <- ifelse(vigifinca_summary$municipio == "ciudad de guatemala", "guatemala", vigifinca_summary$municipio)

# fuzzy join by municipios only
vigifinca_joined <- stringdist_left_join(
  vigifinca_summary,
  guate_json,
  by = "municipio",
  method = "jw",
  max_dist = 0.15,
  distance_col = "dist"
) %>%
  group_by(row_id = row_number()) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  # Clean up fuzzy join artifacts
  rename(
    municipio_original = municipio.x,
    municipio_clean = municipio.y
  ) %>%
  mutate(
    municipio = coalesce(municipio_clean, municipio_original)
  ) %>%
  select(
    -row_id, -municipio_original, -municipio_clean,
    -dist,
    -ends_with(".x"),
    any_of(c("municipio.dist"))
  ) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y")) %>%
  
  # Convert to sf object using cleaned geometry
  st_as_sf()

### write it out and write out the json as is because we will need that for the base layer
saveRDS(vigifinca_joined, "code/vigifinca_joined.rds")
saveRDS(guate_json, "code/guate_json.rds")


###### IN CASE THE JSON DOESN'T READ IN WELL FOR ANYONE ELSE
# library(stringi)     # for string cleaning and accents

# Fix encoding of accents
#fix_encoding <- function(x) {
#  x <- str_replace_all(x, "Ã¡", "á")
#  x <- str_replace_all(x, "Ã©", "é")
#  x <- str_replace_all(x, "Ã­", "í")
#  x <- str_replace_all(x, "Ã³", "ó")
#  x <- str_replace_all(x, "Ãº", "ú")
#  x <- str_replace_all(x, "Ã±", "ñ")
#  x <- str_replace_all(x, "Ã", "Á")  # Fallback for unknowns
#  x
#}

#guate_json <- guate_json %>%
#  mutate(
#    municipio = fix_encoding(municipio),
#  departamen = fix_encoding(departamen),
#    municipio_clean = municipio %>%
#      stri_trans_general("Latin-ASCII") %>%
#      tolower() %>%
#      trimws()
#  )

#st_write(guate_json, "code/guate_muni/agrip_04_Limites_municipales_340.json", driver = "GeoJSON", delete_dsn = TRUE)


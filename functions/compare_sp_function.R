#' Compare species annotations using NCBI and antibiogram metadata
#'
#' This function compares species annotations from CAMDA with those provided by NCBI and, 
#' when missing, supplements with data from antibiogram metadata.
#'
#' It first attempts to match species between `training_metadata_db` and `sra_metadata_db` 
#' using the `accession` field. If no match is found or the NCBI information is missing, 
#' the function tries to match using the `antibiograms_metadata_db`.
#'
#' @param training_metadata_db A data frame containing the original metadata with CAMDA species annotations. 
#'        Must include columns: `accession` and `scientific_name_CAMDA`.
#' @param sra_metadata_db A data frame containing NCBI metadata. 
#'        Must include columns: `accession` and `scientific_name_NCBI`.
#' @param antibiograms_metadata_db A data frame containing metadata from antibiograms. 
#'        Must include columns: `accession` and `scientific_name_Antibiogram`.
#'
#' @return A data frame with the `accession`, the CAMDA species name, the matched species name 
#'         from NCBI or antibiogram (if available), and a `status` column indicating:
#'         - `"Matched_NCBI"`: exact match with NCBI species name
#'         - `"Not-matched_NCBI"`: mismatch with NCBI species name
#'         - `"Missing_NCBI"`: no NCBI data available
#'         - `"Matched_Antibiogram"`: exact match with antibiogram species name
#'         - `"Not-matched_Antibiogram"`: mismatch with antibiogram species name
#'         - `"Missing_All"`: no data available in either source
#'
#' @examples
#' result <- comparar_especies(training_metadata_db, sra_metadata_db, antibiograms_metadata_db)
#'
#' @export


compare_sp <- function(training_metadata_db, sra_metadata_db, antibiograms_metadata_db) {
  library(dplyr)
  
  # Paso 1: Unir con datos de metadat, NCBI y Antibiogram
  base_all <- left_join(training_metadata_db, sra_metadata_db, by = "accession") %>%
    left_join(antibiograms_metadata_db, by = "accession") %>%
    select(accession, scientific_name_CAMDA, scientific_name_NCBI, scientific_name_Antibiogram) %>%
    distinct() # eliminar filas repetidas
  
  # Paso 2: Analizar la informacion con NCBI y antiograms y colocar un estatus
  base_status <- base_all %>%
    mutate(status = case_when(
      # CAMDA matches both NCBI and Antibiogram
      !is.na(scientific_name_NCBI) & !is.na(scientific_name_Antibiogram) &
        scientific_name_CAMDA == scientific_name_NCBI &
        scientific_name_CAMDA == scientific_name_Antibiogram ~ "Matched_NCBI_Antibiogram",
      
      # CAMDA matches only NCBI
      !is.na(scientific_name_NCBI) &
        scientific_name_CAMDA == scientific_name_NCBI ~ "Matched_NCBI",
      
      # CAMDA matches only Antibiogram
      !is.na(scientific_name_Antibiogram) &
        scientific_name_CAMDA == scientific_name_Antibiogram ~ "Matched_Antibiogram",
      
      # CAMDA does not match any, but at least one source has info
      (!is.na(scientific_name_NCBI) | !is.na(scientific_name_Antibiogram)) &
        (scientific_name_CAMDA != scientific_name_NCBI) ~ "Mismatch_with_CAMDA",
      
      # No useful data in either source
      is.na(scientific_name_NCBI) & is.na(scientific_name_Antibiogram) ~ "Missing_All"
    )) %>%
    # Paso 3: analizar fuentes de informqcion
    mutate(status_reference = case_when(
      # Ambos tienen información y no coinciden
      !is.na(scientific_name_NCBI) & !is.na(scientific_name_Antibiogram) &
        scientific_name_NCBI != scientific_name_Antibiogram ~ "Verify_sources",
      
      # Solo NCBI tiene información, Antibiogram es NA
      !is.na(scientific_name_NCBI) & is.na(scientific_name_Antibiogram) ~ "Verify_sources",
      
      # Solo Antibiogram tiene información, NCBI es NA
      is.na(scientific_name_NCBI) & !is.na(scientific_name_Antibiogram) ~ "Verify_sources",
      
      # Ambos están vacíos
      is.na(scientific_name_NCBI) & is.na(scientific_name_Antibiogram) ~ "No_sources",
      
      # Ambos existen y coinciden
      !is.na(scientific_name_NCBI) & !is.na(scientific_name_Antibiogram) &
        scientific_name_NCBI == scientific_name_Antibiogram ~ "Good_sources"
    ))
  
  # Obtain results
  return(base_status)
}

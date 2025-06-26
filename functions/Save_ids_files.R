#' Guardar archivos de IDs de entrenamiento y prueba para una especie
#'
#' Esta función filtra un data frame con información de acceso y guarda 
#' dos archivos `.txt`: uno para los IDs de entrenamiento y otro para los de prueba. 
#' Los archivos se guardan en una ruta estructurada según la especie.
#'
#' @param df_ids Data frame que contiene al menos las columnas `accession` e `info`.
#' @param especie Nombre de la especie (string). Se usará como prefijo en los archivos generados.
#' @param ruta_salida Carpeta donde se guardarán los archivos. Por defecto es "quality_rawdata/species_ids_carpetas".
#'
#' @return No retorna un objeto en R, pero guarda archivos en disco.
#' @examples
#' \dontrun{
#' save_ids(ecoli_ids, "ecoli")
#' save_ids(paeruginosa_ids, "paeruginosa", ruta_salida = "datos/ids/")
#' }
#' @export
save_ids <- function(df_ids, especie, ruta_salida = "quality_rawdata/species_ids_carpetas") {
  if (!dir.exists(ruta_salida)) {
    dir.create(ruta_salida, recursive = TRUE)
  }
  
  #Extraer los IDs de training
  training_ids <- df_ids %>%
    filter(info == "Training_data_download") %>%
    pull(accession)
  #Extraer los IDs de test
  test_ids <- df_ids %>%
    filter(info == "Test_data_download") %>%
    pull(accession)
  
  # Almacenar archivo de trainig
  write.table(training_ids,
              file = file.path(ruta_salida, paste0(especie, "_training_ids.txt")),
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  # Almacenar archivo de test
  write.table(test_ids,
              file = file.path(ruta_salida, paste0(especie, "_test_ids.txt")),
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  message("Archivos guardados para ", especie)
}

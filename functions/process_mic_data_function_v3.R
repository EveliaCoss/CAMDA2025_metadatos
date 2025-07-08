#' Procesar datos MIC y asignar fenotipos interpretativos por especie
#'
#' Esta función procesa un archivo \code{.tsv} con datos de Concentración Mínima Inhibitoria (MIC),
#' recategoriza los valores en intervalos definidos según un umbral máximo (\code{max_mic})
#' y asigna un fenotipo interpretativo ("Susceptible" o "Resistant") usando tablas codificadas por género bacteriano.
#'
#' @param input_file Ruta al archivo de entrada en formato TSV. Debe contener las columnas:
#'   \code{measurement_value}, \code{accession}, \code{new_genus}, y \code{phenotype}.
#' @param output_file Ruta al archivo TSV donde se guardará el \code{data.frame} procesado.
#' @param max_mic Valor máximo de MIC utilizado para definir los cortes. Opciones soportadas: \code{64}, \code{256}, \code{1024}.
#'
#' @return Un \code{data.frame} con las columnas originales más:
#'   \itemize{
#'     \item \code{recategorized_mic}: MIC discretizado según los intervalos definidos.
#'     \item \code{phenotype_assigned}: Fenotipo asignado automáticamente o recuperado del valor original.
#'   }
#'
#' @details
#' La lógica interpretativa utiliza una matriz codificada por género para asignar "Susceptible" (s)
#' o "Resistant" (r) a partir del valor de MIC recategorizado. Si no se puede asignar un fenotipo por tabla,
#' se usa el valor original en la columna \code{phenotype}. Valores \code{"Intermediate"} son tratados como \code{"Susceptible"}.
#'
#' @examples
#' \dontrun{
#' process_mic_data("datos.tsv", "salida.tsv", max_mic = 256)
#' }
#'
#' @export

process_mic_data <- function(input_file, output_file, max_mic = 256) {
  # Validar existencia del archivo de entrada
  if (!file.exists(input_file)) {
    stop("El archivo de entrada no existe: ", input_file)
  }
  
  # Lectura del archivo de salida
  if (!is.character(output_file) || nchar(output_file) == 0) {
    stop("Debes proporcionar una ruta válida para el archivo de salida.")
  }
  
  # Lectura del archivo de entrada
  df <- tryCatch({
    read.delim(input_file, stringsAsFactors = FALSE, sep = "\t")
  }, error = function(e) {
    stop("Error al leer el archivo TSV: ", e$message)
  })
  
  # Validar presencia de columnas clave
  required_cols <- c("measurement_value", "accession", "new_genus", "phenotype")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas requeridas en el input: ", paste(missing_cols, collapse = ", "))
  }
  
  # Forzar conversiones a numérico para el MIC
  df$measurement_value <- suppressWarnings(as.numeric(df$measurement_value))
  if (all(is.na(df$measurement_value))) {
    stop("Todos los valores de 'measurement_value' son NA tras la conversión.")
  }
  
  # Definiciones dinámicas según max_mic
  supported_mics <- c(64, 256, 1024)
  if (!(max_mic %in% supported_mics)) {
    stop("Valor de 'max_mic' no soportado. Opciones válidas: 64, 256, 1024.")
  }
  
  # Configuración de bins y fenotipos según el nivel de resolución
  bin_label_map <- list(
    "64" = list(
      bins   = c(0, 0.09, 0.185, 0.375, 0.75, 1.5, 3, 6, 12, 24, 48, 10000),
      labels = c(0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64),
      pheno  = list("s" = 7, "r" = 4)
    ),
    "256" = list(
      bins   = c(0, 0.09, 0.185, 0.375, 0.75, 1.5, 3, 6, 12, 24, 48, 96, 192, 1200),
      labels = c(0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256),
      pheno  = list("s" = 7, "r" = 6)
    ),
    "1024" = list(
      bins   = c(0, 0.09, 0.185, 0.375, 0.75, 1.5, 3, 6, 12, 24, 48, 96, 192, 384, 768, 2000),
      labels = c(0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024),
      pheno  = list("s" = 7, "r" = 8)
    )
  )
  
  # Recategorizar valores de MIC
  mic_conf <- bin_label_map[[as.character(max_mic)]]
  bins <- mic_conf$bins
  labels <- mic_conf$labels
  
  # Recategorización de valores MIC en cortes predefinidos
  df$recategorized_mic <- cut(df$measurement_value,
                              breaks = bins,
                              labels = labels,
                              right = FALSE)
  
  # Generar código de fenotipo s/r según configuración
  s_count <- mic_conf$pheno$s
  r_count <- mic_conf$pheno$r
  full_code <- paste0(strrep("s", s_count), strrep("r", r_count))
  
  # Construcción de la matriz interpretativa por género
  phenotype_table <- list(
    "Klebsiella"     = strsplit(full_code, "")[[1]],
    "Escherichia"    = strsplit(full_code, "")[[1]],
    "Salmonella"     = strsplit(full_code, "")[[1]],
    "Streptococcus"  = strsplit(paste0(strrep("s", s_count - 3), strrep("r", r_count + 3)), "")[[1]],
    "Staphylococcus" = strsplit(full_code, "")[[1]],
    "Pseudomonas"    = strsplit(paste0(strrep("s", s_count + 2), strrep("r", r_count - 2)), "")[[1]],
    "Acinetobacter"  = strsplit(paste0(strrep("s", s_count + 1), strrep("r", r_count - 1)), "")[[1]],
    "Campylobacter"  = strsplit(paste0(strrep("s", s_count + 1), strrep("r", r_count - 1)), "")[[1]],
    "Neisseria"      = strsplit(paste0(strrep("s", s_count - 2), strrep("r", r_count + 2)), "")[[1]]
  )
  
  phenotype_df <- as.data.frame(do.call(rbind, phenotype_table), stringsAsFactors = FALSE)
  rownames(phenotype_df) <- names(phenotype_table)
  colnames(phenotype_df) <- as.character(labels)
  
  # Asignación defensiva de fenotipo interpretativo
  df <- df %>%
    rowwise() %>%
    mutate(
      phenotype_assigned = {
        genus_use <- ifelse(!is.na(new_genus), new_genus, genus)
        mic_cat <- as.character(recategorized_mic)
        
        if (!is.na(mic_cat) &&
            genus_use %in% rownames(phenotype_df) &&
            mic_cat %in% colnames(phenotype_df)) {
          val <- tryCatch(phenotype_df[genus_use, mic_cat], error = function(e) NA)
          if (val == "s") "Susceptible" else if (val == "r") "Resistant" else NA_character_
        } else if (is.na(mic_cat) && !is.na(phenotype)) {
          tools::toTitleCase(tolower(phenotype))
        } else {
          NA_character_
        }
      },
      phenotype_assigned = ifelse(phenotype_assigned == "Intermediate", "Susceptible", phenotype_assigned)
    ) %>%
    ungroup()
  
  # Exportación del archivo de salida
  tryCatch({
    write.table(df, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  }, error = function(e) {
    stop("Error al escribir el archivo de salida: ", e$message)
  })
  
  return(df)
}

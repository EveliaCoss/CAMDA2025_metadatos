#' Procesar datos MIC desde un archivo TSV y asigna un nuevos fenotipos y MIC por generos
#'
#' Esta función procesa un archivo con datos de Concentración Mínima Inhibitoria (MIC)
#' en formato \code{.tsv}, recategoriza los valores en intervalos definidos y asigna un
#' fenotipo ("Susceptible" o "Resistant") basado en reglas interpretativas específicas
#' para cada género bacteriano.
#'
#' Incluye programación defensiva para validar la estructura del archivo, asegurar que los
#' datos sean consistentes y manejar errores con mensajes claros.
#'
#' @param input_file Ruta al archivo de entrada en formato TSV con columnas \code{measurement_value},
#'   \code{accession} y \code{new_genus}.
#' @param output_file Ruta del archivo TSV donde se guardará el \code{data.frame} procesado.
#'
#' @return Un \code{data.frame} con columnas adicionales \code{recategorized_mic} y
#'   \code{phenotype_assigned}.
#'
#' @details
#' - Verifica que el archivo de entrada exista y que las columnas requeridas estén presentes.
#' - Convierte \code{measurement_value} a numérico, omitiendo valores no convertibles.
#' - Agrupa valores MIC en 11 categorías según puntos de corte preestablecidos.
#' - Asigna fenotipos según género y el valor MIC recategorizado, usando una matriz codificada.
#' - Guarda los resultados en formato \code{.tsv} sin comillas ni fila de índices.
#'
#' @examples
#' \dontrun{
#' process_mic_data("mic_input.tsv", "mic_output.tsv")
#' }
#'
#' @export

process_mic_data <- function(input_file, output_file) {
  # Validar que el archivo de entrada existe
  if (!file.exists(input_file)) {
    stop("El archivo de entrada no existe: ", input_file)
  }
  
  # Validar que la salida es una ruta válida
  if (!is.character(output_file) || nchar(output_file) == 0) {
    stop("Debes proporcionar una ruta válida para el archivo de salida.")
  }
  
  # Intentar leer el archivo de entrada
  df <- tryCatch({
    read.delim(input_file, stringsAsFactors = FALSE, sep = "\t")
  }, error = function(e) {
    stop("Error al leer el archivo TSV: ", e$message)
  })
  
  # Verificar existencia de columnas requeridas
  required_cols <- c("measurement_value", "accession", "new_genus", "phenotype")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas requeridas en el input: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convertir measurement_value a numérico
  df$measurement_value <- suppressWarnings(as.numeric(df$measurement_value))
  if (all(is.na(df$measurement_value))) {
    stop("Todos los valores de 'measurement_value' son NA tras la conversión.")
  }
  
  # Definir bins y etiquetas
  bins <- c(0, 0.09, 0.185, 0.375, 0.75, 1.5, 3, 6, 12, 24, 48, 10000)
  labels <- c(0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
  
  if (length(bins) - 1 != length(labels)) {
    stop("Los intervalos definidos por 'bins' no coinciden con el número de etiquetas.")
  }
  
  # Recategorizar valores de MIC
  df$recategorized_mic <- cut(df$measurement_value,
                              breaks = bins,
                              labels = labels,
                              right = FALSE)
  
  # Definir matriz de interpretación de fenotipos
  phenotype_matrix <- list(
    "Klebsiella"       = strsplit("sssssssrrrr", "")[[1]],
    "Escherichia"      = strsplit("sssssssrrrr", "")[[1]],
    "Salmonella"       = strsplit("sssssssrrrr", "")[[1]],
    "Streptococcus"    = strsplit("ssssrrrrrrr", "")[[1]],
    "Staphylococcus"   = strsplit("sssssssrrrr", "")[[1]],
    "Pseudomonas"      = strsplit("sssssssssrr", "")[[1]],
    "Acinetobacter"    = strsplit("ssssssssrrr", "")[[1]],
    "Campylobacter"    = strsplit("ssssssssrrr", "")[[1]],
    "Neisseria"        = strsplit("sssssrrrrrr", "")[[1]]
  )
  
  phenotype_df <- as.data.frame(do.call(rbind, phenotype_matrix), stringsAsFactors = FALSE)
  rownames(phenotype_df) <- names(phenotype_matrix)
  colnames(phenotype_df) <- as.character(labels)
  
  # Asignación de fenotipo
  df$phenotype_assigned <- apply(df, 1, function(row) {
    genus <- row[["new_genus"]]
    mic <- as.character(row[["recategorized_mic"]])
    if (is.na(genus) || is.na(mic)) return(NA)
    if (!(genus %in% rownames(phenotype_df))) return(NA)
    if (!(mic %in% colnames(phenotype_df))) return(NA)
    ifelse(phenotype_df[genus, mic] == "s", "Susceptible", "Resistant")
  })
  
  # When phenotype_assigned is NA
  # 
  # If either new_genus or recategorized MIC is missing
  # If the genus is not defined in the interpretation table
  # If the MIC doesn't fall within any of the defined bins
  
  # Escribir el archivo de salida
  tryCatch({
    write.table(df, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  }, error = function(e) {
    stop("Error al escribir el archivo de salida: ", e$message)
  })
  
  return(df)
}


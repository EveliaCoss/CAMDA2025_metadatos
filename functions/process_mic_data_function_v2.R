#' Procesar datos MIC y asignar fenotipos interpretativos por género bacteriano
#'
#' Esta función procesa un archivo con valores de Concentración Mínima Inhibitoria (MIC),
#' recategoriza dichos valores según umbrales definidos y asigna un fenotipo ("Susceptible"
#' o "Resistant") de acuerdo con tablas interpretativas específicas por género bacteriano.
#'
#' La función permite seleccionar el punto de corte superior (`max_mic`) para controlar la resolución
#' de los bins usados en la recategorización.
#'
#' @param input_file Ruta al archivo de entrada en formato \code{.tsv} que debe contener las columnas
#'   \code{measurement_value}, \code{accession}, \code{new_genus}, \code{phenotype} y opcionalmente \code{genus}.
#' @param output_file Ruta del archivo \code{.tsv} donde se guardará el \code{data.frame} procesado.
#' @param max_mic Valor máximo de MIC que define el rango de categorización. Debe ser uno de: \code{64}, \code{256}, \code{1024}.
#'
#' @return Un \code{data.frame} con columnas adicionales \code{recategorized_mic} y \code{phenotype_assigned}.
#'
#' @details
#' - Los valores MIC se recategorizan en intervalos definidos por \code{max_mic}, permitiendo interpretaciones a diferentes resoluciones.
#' - La matriz de interpretación de fenotipos se construye dinámicamente a partir de reglas codificadas por género.
#' - Si \code{new_genus} está vacío, se utiliza \code{genus} como respaldo.
#' - Si no se puede asignar un fenotipo por MIC, se usa el proporcionado en la columna \code{phenotype}.
#' - Los valores \code{"Intermediate"} se recodifican como \code{"Susceptible"} para consistencia.
#' - Incluye validación defensiva de insumos y manejo robusto de errores de lectura y escritura.
#'
#' @examples
#' \dontrun{
#' process_mic_data("entrada.tsv", "salida.tsv", max_mic = 256)
#' process_mic_data("entrada.tsv", "salida.tsv", max_mic = 1024)
#' }
#'
#' @export

process_mic_data <- function(input_file, output_file, max_mic = 256) {
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
  
  # Validar valor permitido
  supported_mics <- c(64, 256, 1024)
  if (!(max_mic %in% supported_mics)) {
    stop("Valor de 'max_MIC' no soportado. Opciones válidas: 64, 256, 1024.")
  }
  
  # Definir bins y etiquetas según max_MIC
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
  
  # Acceder a la configuración correspondiente
  mic_conf <- bin_label_map[[as.character(max_mic)]]
  bins <- mic_conf$bins
  labels <- mic_conf$labels
  
  # Recategorizar valores de MIC
  df$recategorized_mic <- cut(df$measurement_value,
                              breaks = bins,
                              labels = labels,
                              right = FALSE)
  
  # Construcción dinámica de la matriz de fenotipos
  s_count <- mic_conf$pheno$s
  r_count <- mic_conf$pheno$r
  full_code <- paste0(strrep("s", s_count), strrep("r", r_count))
  
  # Definir matriz de interpretación de fenotipos
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
  
  # Construcción del dataframe a partir del phenotype_table
  phenotype_df <- as.data.frame(do.call(rbind, phenotype_table), stringsAsFactors = FALSE)
  rownames(phenotype_df) <- names(phenotype_table)
  colnames(phenotype_df) <- as.character(labels)
  
  # Asignación de fenotipo
  df <- df %>%
    rowwise() %>%
    mutate(
      phenotype_assigned = case_when(
        # Escenario 1: se tiene recategorized_mic y el género tiene reglas interpretativas
        # cuando recategorized_mic es NA y Se puede obtener un género conocido (new_genus si existe, o genus)
          !is.na(recategorized_mic) &
            !is.na(new_genus) &
            new_genus %in% rownames(phenotype_df) &
            as.character(recategorized_mic) %in% colnames(phenotype_df) ~
            ifelse(phenotype_df[new_genus, as.character(recategorized_mic)] == "s",
                   "Susceptible", "Resistant"),
        # Escenario 2: no se puede recategorizar el MIC, pero sí hay fenotipo original
        !is.na(recategorized_mic) &
          (is.na(new_genus) | !(new_genus %in% rownames(phenotype_df))) &
          !is.na(genus) &
          genus %in% rownames(phenotype_df) &
          as.character(recategorized_mic) %in% colnames(phenotype_df) ~
          ifelse(phenotype_df[genus, as.character(recategorized_mic)] == "s",
                 "Susceptible", "Resistant"),
        # Escenario 3: no se puede recategorizar el MIC, pero sí hay fenotipo original
        is.na(recategorized_mic) & !is.na(phenotype) ~ 
          tools::toTitleCase(tolower(phenotype)),
        # Escenario 4: todo lo anterior falla
        # Si no se puede obtener mic ni hay fenotipo original, asigna NA
        TRUE ~ NA_character_
      ),
      # Reemplazo final: "Intermediate" → "Susceptible"
      phenotype_assigned = ifelse(phenotype_assigned == "Intermediate", "Susceptible", phenotype_assigned)
    ) %>%
    ungroup()
  
  # Escribir el archivo de salida
  tryCatch({
    write.table(df, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  }, error = function(e) {
    stop("Error al escribir el archivo de salida: ", e$message)
  })
  
  return(df)
}


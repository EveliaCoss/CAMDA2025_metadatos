#' Crea variables con subconjuntos de datos por especie
#'
#' Esta función toma un data frame y el nombre de una columna que contiene nombres científicos 
#' (en formato "Género especie") y crea variables globales con subconjuntos de datos para cada especie. 
#' Las variables creadas se nombran con la inicial del género seguida del nombre de la especie, 
#' terminadas en "_ids" (por ejemplo, \code{ecoli_ids}).
#'
#' @param data Data frame de entrada que contiene los datos a filtrar.
#' @param column_name Nombre de la columna (como cadena) que contiene los nombres científicos.
#'
#' @return No devuelve un objeto, pero crea variables en el entorno global con los subconjuntos de datos.
#' @export
#'
#' @examples
#' create_species_variables(SRA_down_joined_data, "scientific_name_CAMDA")

create_species_variables <- function(data, column_name) {
  # Obtener las especies unicas
  unique_species <- unique(data[[column_name]])
  
  # por cada especie darme una variable de ID
  for (species in unique_species) {
    # Generar nombre de variable (ej. ecoli_ids)
    split_name <- unlist(strsplit(species, " "))
    genus_initial <- tolower(substr(split_name[1], 1, 1))
    species_name <- tolower(split_name[2])
    var_name <- paste0(genus_initial, species_name, "_ids")
    
    # Filtrar datos
    filtered_data <- data %>% filter(!!sym(column_name) == species)
    
    # Asignar a variable global
    assign(var_name, filtered_data, envir = .GlobalEnv)
  }
}

#!/bin/bash

# -------------------------------------------
# move_ids.sh
# Autora: Evelia Coss
# Fecha: 25 de junio, 2025
#
# Función para mover carpetas según una lista de IDs a una subcarpeta específica.
# Uso:
#   source scripts/move_ids.sh
#   mover_ids archivo_de_ids categoria especie
#
# Ejemplo:
#   move_ids ids.txt training salmonella
#   → Mueve carpetas con nombres de ID a salmonella/training/
# -------------------------------------------

move_ids() {
    local archivo_ids=$1      # Archivo de texto con los IDs (uno por línea)
    local categoria=$2        # Subcarpeta destino (por ejemplo, training o test)
    local especie=$3          # Carpeta principal de la especie (por ejemplo, salmonella)

    # Validación de argumentos
    if [[ ! -f "$archivo_ids" ]]; then
        echo "Archivo '$archivo_ids' no encontrado."
        return 1
    fi

    # Construcción de la ruta destino
    local destino="${especie}/${categoria}"
    mkdir -p "$destino"  # Crea la carpeta si no existe

    # Iterar sobre cada línea del archivo
    while read id; do
        if [[ -d "$id" ]]; then
            mv "$id" "$destino/"
            echo "Movido: $id → $destino/"
        else
            echo "Carpeta '$id' no encontrada."
        fi
    done < "$archivo_ids"
}

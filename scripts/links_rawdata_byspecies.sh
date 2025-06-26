#!/bin/bash

# ------------------------------------------------------------
# link_ids.sh
# Autora: Evelia Coss
# Fecha: 25 junio, 2025 
#
# Crea enlaces simbólicos a carpetas ubicadas en "raw/"
# y los organiza en "raw_byspecies/{especie}/{categoria}/".
#
# Uso:
#   source links_rawdata_byspecies.sh
#   link_ids archivo_de_ids categoria especie
# ------------------------------------------------------------

link_ids() {
    local archivo_ids=$1      # Archivo de texto con los IDs
    local categoria=$2        # training / test / other
    local especie=$3          # Nombre de la especie

    if [[ ! -f "$archivo_ids" ]]; then
        echo "Archivo '$archivo_ids' no encontrado."
        return 1
    fi

    local destino="raw_byspecies/${especie}/${categoria}"
    mkdir -p "$destino"

    while read id; do
        local origen="raw/${id}"
        local enlace="${destino}/${id}"

        if [[ -d "$origen" ]]; then
            ln -s "$(realpath "$origen")" "$enlace"
            echo "Enlace creado: $enlace proveniente de →  $origen"
        else
            echo "Carpeta no encontrada: $origen"
        fi
    done < "$archivo_ids"
}

#!/bin/bash

# --------------------------------------------------------------------
# link_fastqc_by_species.sh
# Autora: Evelia Coss
# Fecha: 25 junio, 2025 
#
# Crea symlinks de archivos FastQC (*.html y *.zip) desde quality/fastqc/
# hacia quality_byspecies/{especie}/{categoria}/
#
# Detecta:
#   - *_fastqc.html
#   - *_fastqc.zip
#   - cualquier archivo que comience con el ID
# -----------------------------------------------------------------------------------------------------

link_fastqc_by_species() {
    local archivo_ids=$1
    local categoria=$2
    local especie=$3

    if [[ ! -f "$archivo_ids" ]]; then
        echo "Archivo '$archivo_ids' no encontrado."
        return 1
    fi

    local origen_base="quality/fastqc"
    local destino_base="quality_byspecies/${especie}/${categoria}"
    mkdir -p "$destino_base"

    while read -r id; do
        archivos=( "$origen_base"/"${id}"* )
        if [[ ${#archivos[@]} -eq 0 ]]; then
            echo "No se encontraron archivos para $id"
        fi

        for archivo in "${archivos[@]}"; do
            if [[ -f "$archivo" ]]; then
                ln -sf "$(realpath "$archivo")" "${destino_base}/$(basename "$archivo")"
                echo "Enlace creado: $(basename "$archivo")"
            fi
        done
    done < "$archivo_ids"
}



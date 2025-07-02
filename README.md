# CAMDA: Antibiotic Resistance

**CAMDA2025_metadatos** is a curated collection of scripts, datasets, and documentation supporting the CAMDA 2025 challenge on antimicrobial resistance.

This repository is intended to ensure reproducibility, transparency, and efficiency in modeling workflows, with a focus on high-quality input for machine learning and deep learning models in antimicrobial resistance prediction.

## Datasets provided by CAMDA 2025

The dataset contains 9 bacterial species ("Neisseria gonorrhoeae", "Staphylococcus aureus", "Escherichia coli", "Campylobacter jejuni", "Acinetobacter baumannii", "Streptococcus pneumoniae", "Klebsiella pneumoniae", "Salmonella enterica", and "Pseudomonas aeruginosa") encompassing 4 antibiotics:

```         
- GEN: Gentamicin
- ERY: Erythromycin
- TET: Tetracycline
- CAZ: Ceftazidime
```

Datasets:

-   [training set](https://github.com/ccm-bioinfo/CAMDA2025_AntibioticResistance/blob/main/rawdata/TrainAndTest_dataset/training_dataset.csv): CAMDA training set.
-   [test set](https://github.com/ccm-bioinfo/CAMDA2025_AntibioticResistance/blob/main/rawdata/TrainAndTest_dataset/testing_template.csv): CAMDA test set.

## Pipeline

-   **Input files:**
    -   [test set](https://github.com/ccm-bioinfo/CAMDA2025_AntibioticResistance/blob/main/rawdata/TrainAndTest_dataset/testing_template.csv): CAMDA test set (`test_metadata_db`).
    -   [training set](https://github.com/ccm-bioinfo/CAMDA2025_AntibioticResistance/blob/main/rawdata/TrainAndTest_dataset/training_dataset.csv): CAMDA training set (`training_metadata_db`).
    -   [NCBI data](https://github.com/ccm-bioinfo/CAMDA2025_AntibioticResistance/blob/main/metadata/sra-metadata.csv): Metadata from NCBI (June 10, 2025) (`sra_metadata_db`)
    -   [Antibiogramas](https://zenodo.org/records/14876710): Metadata from NCBI, ENA and BV-BRC. (February 16, 2025) (`antibiograms_db`)

**Complete [codebook](https://eveliacoss.github.io/CAMDA2025_metadatos/) containing scripts, inputs, and outputs for:**

1.  [Data cleaning](https://eveliacoss.github.io/CAMDA2025_metadatos/2_Preprocessing_CAMDApublicdata.html) - Find how many species are correctly annotated in both databases CAMDA and NCBI related with CAMDA information (training and test datasets).
2.  [Verification of data downloads](https://eveliacoss.github.io/CAMDA2025_metadatos/3_Download_SRAdata.html) - To confirm successful data download, folder structure organization, and assess the quality of the retrieved files.
3.  [Generation of input files for machine learning (ML) and deep learning (DL) workflows](https://eveliacoss.github.io/CAMDA2025_metadatos/4_inpufile_MLandDL.html) - To prepare the dataset for downstream predictive modeling, we merged three key sources of information.

## Team 2025

-   Anton Pashkov
-   Francisco Santiago Nieto
-   Johana Atenea Carreón Baltazar
-   Luis Raúl Figueroa Martinez
-   Víctor Muñíz
-   César Aguilar
-   Varinia
-   Johana Castelo
-   Mariana Amparo
-   Evelia Lorena Coss Navarrete
-   Yesenia Villicaña Molina
-   Haydeé Contreras Peruyero
-   Nelly Sélem Mojica

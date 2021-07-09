# Reproducible datasets and code for:
## Egg size variation in a long-lived polyandrous shorebird in the context of senescence and breeding phenology
#### Luke J. Eberhart-Hertel, Lourenço Falcão Rodrigues, Johannes Krietsch, Anne G. Eberhart-Hertel, Medardo Cruz López, Karina Alejandra Vázquez-Rojas, Erick González-Medina, Julia Schroeder, and Clemens Küpper
#### *In review*

In this repository you can find all the raw data and code needed to reproduce our investigation of egg size variation in snowy plovers (_Charadrius nivosus_) monitored annually since 2006 at [Bahía de Ceuta](https://www.google.com/maps/@23.9197739,-106.9668912,2358m/data=!3m1!1e3 "Google Map Satellite") – an important breeding site in western Mexico.

For a complete overview of the methods and results presented in our manuscript, please view our project vignette: [Supplementary File 1](https://raw.githack.com/leberhartphillips/snowy_plover_eggs/main/Rmd/Supplementary_File_1/Supplementary_File_1.html)

<p align="center">
  <img width="600" src="./products/figures/jpg/eggs_plot_ms.jpg">
</p>  

<p align="center" > <i>Egg size variation and its relationship to chick size in snowy plovers (Charadrius nivosus). Illustrations by Luke Eberhart-Hertel.</i></p>

#### Repository Contents
[**`R/`**](https://github.com/leberhartphillips/snowy_plover_eggs/tree/main/R)

  - `project_X.R` scripts contain housekeeping code (custom functions, loading libraries, plotting themes, etc.)
  - `wrangle_X.R` scripts contain code used to wrangle the [CeutaOPEN](https://www.nature.com/articles/s41597-020-0490-y "CeutaOPEN") database into the format used in the models presented in this investigation
  - `model_X.R` scripts contain code used for modeling and exporting output
  - `results_X.R` scripts contain code used to process the model output into figures and tables

[**`data/`**](https://github.com/leberhartphillips/snowy_plover_eggs/tree/main/data)
  
  - [`BaSTA_checked_life_table_females_2006-2020.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/BaSTA_checked_life_table_females_2006-2020.rds) is the processed BaSTA file containing the encounter histories of all individually marked plovers in the population
  - [`ceuta_egg_chick_female_data.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/ceuta_egg_chick_female_data.rds) is the trait data used in the polyandry, egg volume, and lay date models
  - [`raw_encounter_histories_females_2006_2020.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/raw_encounter_histories_females_2006_2020.rds) is the raw unprocessed encounter histories of all individually marked plovers in the population with each row being an encounter.
  - [`raw_life_table_females_2006_2020.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/raw_life_table_females_2006_2020.rds) is the raw unprocessed encounter histories of all individually marked plovers in the population with each row being an individual and columns including annual detections (i.e., the main difference with the BaSTA life table file is that this contains the individual identities)

[**`output/`**](https://github.com/leberhartphillips/snowy_plover_eggs/tree/main/output)

  - [`stats_chick_mod.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_chick_mod.rds) is the output from the chick weight model
  - [`stats_polyandry_mod.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_polyandry_mod.rds) is the output from the polyandry model
  - [`stats_eggv_mod.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_eggv_mod.rds) is the output from the egg volume model
  - [`stats_laydate_mod.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output) is the output from the lay date model
  - [`stats_renesting_mod.rds`](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output) is the output from the re-nesting model

[**`products/figures/`**](https://github.com/leberhartphillips/snowy_plover_eggs/tree/main/products/figures)

  - [`egg_widths_plot.svg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`egg_lengths_plot.svg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`chickw_eggv_plot.svg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`egg_summary.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite) 
  - [`season_combo_plot.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`season_combo_plot_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`age_combo_plot_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite) 
  - [`tarsus_combo_plot_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite) 
  - [`polyandry_forest_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`egg_forest_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`laydate_forest_ms.jpg`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  

[**`products/tables/`**](https://github.com/leberhartphillips/snowy_plover_eggs/tree/main/products/tables)

  - [`eggv_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`polyandry_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`laydate_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`BaSTA_DIC_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite) 
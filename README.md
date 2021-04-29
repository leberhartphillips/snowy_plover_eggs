# Snowy Plover Egg-Size Study
## Age- and season-dependant variation in egg size and breeding phenology of female snowy plovers *Charadrius nivosus*
### April 30, 2021
#### Luke J. Eberhart-Hertel

In this repository you can find all the raw data and code needed to reproduce our investigation of egg size variation in snowy plovers (_Charadrius nivosus_) monitored annually since 2006 at [Bahía de Ceuta](https://www.google.com/maps/@23.9197739,-106.9668912,2358m/data=!3m1!1e3 "Google Map Satellite") – an important breeding site in western Mexico.

#### Repository Contents
**`R/`**

  - `project_X.R` scripts contain housekeeping code (custom functions, loading libraries, plotting themes, etc.)
  - `wrangle_X.R` scripts contain code used to wrangle the CeutaOPEN database into the format used in the models presented in this investigation
  - `explore_X.R` scripts contain code used for exploratory analysis of trait distributions
  - `model_X.R` scripts contain code used for modeling and exporting output
  - `results_X.R` scripts contain code used to process the model output into figures and tables
  - `playground.R` script contains untidy code used for testing ideas and visualizations
  - `junkyard.R` script contains untidy code saved from previous versions

**`data/`**
  
  - [BaSTA_checked_life_table_females_2006-2020.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/BaSTA_checked_life_table_females_2006-2020.rds) is the processed BaSTA file containing the encounter histories of all individually marked plovers in the population
  - [ceuta_egg_chick_female_data.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/ceuta_egg_chick_female_data.rds) is the trait data used in the polyandry, egg volume, and lay date models
  - [raw_encounter_histories_females_2006_2020.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/raw_encounter_histories_females_2006_2020.rds) is raw the unprocessed encounter histories of all individually marked plovers in the population (each row is an encounter)
  - [raw_life_table_females_2006_2020.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/data/raw_life_table_females_2006_2020.rds) is raw the unprocessed encounter histories of all individually marked plovers in the population (each row is an individual and columns include annual detections. The main difference with the BaSTA life table file is that this contains the individual identities)

**`output/`**

  - [stats_chick_mod.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_chick_mod.rds) is the output from the chick weight model
  - [stats_polyandry_mod.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_polyandry_mod.rds) is the output from the polyandry model
  - [stats_eggv_mod.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output/stats_eggv_mod.rds) is the output from the egg volume model
  - [stats_laydate_mod.rds](https://github.com/leberhartphillips/snowy_plover_eggs/blob/main/output) is the output from the lay date model

**`products/figures/`**

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

**`products/tables/`**
  - [`eggv_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`polyandry_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)
  - [`laydate_mod_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite)  
  - [`BaSTA_DIC_table.png`](https://github.com/leberhartphillips/Ceuta_CLOSED/blob/master/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v1-1.sqlite) 

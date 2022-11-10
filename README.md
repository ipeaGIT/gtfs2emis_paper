[![DOI](https://zenodo.org/badge/563972827.svg)](https://zenodo.org/badge/latestdoi/563972827)

R codes used in the research paper **"Estimating public transport emissions from General Transit Feed Specification data"**

The R scripts to reproduce the results are divided into four steps:

 -  `1_data-preparation.R`
    - It downloads the [gtfs2emis v. 0.0.1 (Zenodo)](https://zenodo.org/record/7308585/#.Y2xmEL3ML5I)[![DOI](https://zenodo.org/badge/218305263.svg)](https://zenodo.org/badge/latestdoi/218305263)
    - Filter the gtfs of SPTRANS transport agency
    - Downloads the input data (gtfs, raster of Sao Paulo city, vehicle fleet)
 -  `2_core_functions.R`
    - Runs the `transport_model()` and `emission_model()` for gtfs2emis
    - Uses auxiliar functions to estimate emissions by: hour of the day, by grid of Sao Paulo city
    , by age of fleet, by grid and time of the day
    - Saves the main outputs (from `transport_model()` and `emissions_model()`), and pre-processed outputs
    (from `emis_summary()`, `emis_to_dt()` and `emis_grid()` operations)
 -  `3_plots.R`
    - Generates all the plots shown in the research paper
    - Figures is saved in `Figures/`
 -  `4_main_statistics.R`
    - Generate summary tables and specific information for the discussion section
    

The full article of gtfs2emis can be acessed [Vieira, Pereira and Andrade (2022)](https://doi.org/10.31219/osf.io/8m2cy).

This project considers 4 different GHCN-Daily stations near Allendale County, SC and computes ETCCDI Climate Indices on their observations.


Directory Structure 

├───plot_indices.R
├───Botteon_Data_analysis.pdf
├───station_period_of_record.txt
├───manual.pdf
├───README.md
├───indices
├───log
├───original_datasets
├───cleaned_datasets
└───quality_controlled_datasets

plot_indices.R contains the code to produce the plots and Mann Kendall tests included in the report.

Botteon_Data_analysis.pdf contains the report on the analysis on 4 indices on 4 stations.

station_period_of_record.txt contains the period of record for 6 stations considered for the project.

manual.pdf contains the RClimDex instructions and details each index.

indices contains the calculated indices from the RClimDex package, 27 different indices each for 4 stations.

log contains the log outputs from RClimDex when quality control is performed.

original_datasets contains the untouched csv files downloaded from Blackboard.

cleaned_datasets contains the station data formatted to year|month|day|prcp|tmax|tmin to fit RClimDex.


quality_controlled_datasets contains the RClimDex indcal.csv output from quality control.

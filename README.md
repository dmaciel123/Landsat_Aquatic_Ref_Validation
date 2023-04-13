# Validity of the Landsat surface reflectance archive for aquatic science: Implications for cloud-based analysis

This repository hosts the dataset and codes used to generate and evaluate accuracy of Landsat Collection 2 Level 2 products. For that, R-Code is provided to calculate Landsat accuracy 
based on in-situ water reflectance simulated to Landsat-5/TM, Landsat-7/ETM+, Landsat-8/OLI and Landsat-9/OLI-2. 

For reproduction of this study, users should first clone de GitHub repository and execute the R scripts. A .CSV file (Matchups.csv) is provided with the in-situ simulated reflectance and Landsat extracted data.

The script for extracting Landsat data using Microsoft Planetary Computer STAC is available in the folder (Scripts/Extract_landsat_5.R). For changing between sensors, just change the sensors name. 

The time-series analysis R code is also available to plot the time-series evaluated in the study. For time-series generation, a Google Earth Engine code is provided here: https://code.earthengine.google.com/1fd5b6993ac5bada5486ba6b7f22c9ea

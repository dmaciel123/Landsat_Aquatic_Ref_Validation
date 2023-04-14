# Validity of the Landsat surface reflectance archive for aquatic science: Implications for cloud-based analysis

This repository contains the dataset and codes used to generate and evaluate accuracy of Landsat Collection 2 Level 2 surface reflectance (ρ<sub>s</sub>) products for aquatic applications. More information could be obtained in the published study: Maciel et al. [2023](lin)

For reproductibility purposes, R-Code is provided to calculate Landsat Level-2 products accuracy based on in-situ (ρ<sub>s</sub>) simulated to Landsat-5/TM, Landsat-7/ETM+, Landsat-8/OLI and Landsat-9/OLI-2. 
 
The simulation (i.e., application of the Relative Spectral Response) for each sensor was performed using the R package bandSimulation available [here](https://github.com/dmaciel123/BandSimulation). 

The compilation of this dataset is based on three different data sources.

1)  GLObal Reflectance community dataset for Imaging and optical sensing of Aquatic environments (GLORIA) (Lehmann et al., [2023](https://doi.org/10.1038/s41597-023-01973-y))
2)  Augmented GLORIA (Pahlevan et al. [2022](https://doi.org/10.1016/j.rse.2021.112860))
3)  Dataset from Instrumentation Laboratory for Aquatic Ecosystems (LabISA-INPE, Brazil) (Maciel et al., [2021](https://doi.org/10.1016/j.isprsjprs.2021.10.009))


# Figures and statistical analysis

For reproduction of this study, users should first clone de GitHub repository and execute the R scripts. A .CSV file (Matchups.csv) is provided with the in-situ simulated reflectance and Landsat extracted data. A detailed description of Metadata is available in the file Metadata-LO-Letters-data.docx.

The script "01_Generate_correlations_heatmap.R" run the statistical analysis and figures plot for the Figure 2 of the main manuscript and for the Figure S2 of the Supplementary Material. It also calculates all the statistical metrics.

# Microsoft Planetary Computer data extraction

The extraction of ρ<sub>s</sub> was performed using the [Microsoft Planetary Computer](https://planetarycomputer.microsoft.com/) (MPC). The MPC hosts a STAC catalog with a copy of the USGS Collection 2 Level 2 products. The rSTAC package was used to provide the connection between STAC and R and for the retrieval of the data. Extraction was performed using the MPC platform, but the code used is available in the Scripts folder [Scripts/Extract_landsat_5.R](https://github.com/dmaciel123/Landsat_Aquatic_Ref_Validation/blob/master/Scripts/00_Planetary_computer_Extraction.R). 

We strongly reccomend the readers to use the MPC platform to extract the data. The example is provided for Landsat-5/TM - but users could change the sensor in the script easilly. 

# Time-series plots

The time-series was obtained using the Google Earth Engine platform and the script used to generate the time-series is available [here]( https://code.earthengine.google.com/1fd5b6993ac5bada5486ba6b7f22c9ea). After the generation of the time-series for the specific location, user should save the graph with the reflectance values and also the graph with the pixel count number. It allows to filter for dates with a small number of pixels in our area of interest. The time-series analysis R code is available to plot the time-series. 

More information about the time-series is provided in the Supplementary Material of Maciel et al. (2023) publication. 


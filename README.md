# Master's Thesis Title
Cooling Centres as an Urban Heat Risk Adaptation Strategy: A Comparative Geospatial Analysis of Accessibility and Population Heat Risk in Selected European Cities

## Project Overview
This thesis examines the accessibility and spatial optimisation of cooling centre networks as an urban heat adaptation strategy in selected European cities. This study develops a transparent, replicable geospatial framework that creates a granular heat risk index (100m grid resolution) based on micro-climate, socio-demographic, and built-environmental data. This is integrated with accessibility and location-allocation modelling using open-source data and R-based computation. Four cities — Athens, Bologna, Barcelona, and Vienna — serve as case studies representing diverse climatic, demographic, and infrastructural contexts. The thesis contributes a quantitative, policy-relevant approach to guide efficient and inclusive cooling centre planning, supporting evidence-based strategies for climate-resilient and socially responsive cities.

## Research Questions
The geospatial framework presented here, answers the following research questions: 
1.	How does heat risk vary inside cities and hence affect the local necessity for cooling centres?
2.	How well do existing cooling centres serve residents with increased heat risk in terms of accessibility measured in travel time in selected cities?
3.	Where should additional cooling centres be placed to maximise coverage and shorten travel times, by prioritising heat risk impacted populations?

## Data
**Existing cooling centre locations** are downloaded from the respective city's websites:<br>
•	Athens: addresses available online (Athens Coordination Center for Migrant and Refugee issues – ACCMR, 2024)  coordinates retrieved from GoogleMaps<br>
•	Barcelona: data set with all cooling centres including coordinates available online (Ajuntament de Barcelona, 2021)<br>
•	Bologna: addresses available online via interactive map (Comune di Bologna, 2025)  coordinates retrieved from GoogleMaps<br>
•	Vienna: addresses available online (Stadt Wien, 2025b)  coordinates retrieved from GoogleMaps<br>

**Potential cooling centre candidates** are downloaded from OSM as seen in the scripts inside the "OSM Download Scripts" folders.

Main input data for the heat risk index is disclosed in this table: 
| **Risk Factor**                                | **Metric**                                                       | **Resolution**                                                   | **Data Set & Source**                                                                                                                                                          |
|------------------------------------------------|------------------------------------------------------------------|------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Population Density (Demand)**                         | Number of residents per grid cell                                | 100m                                                             | GHS Population Grid (R2023) by European Commission, Joint Research Centre (Schiavina et al., 2023)                                                                             |
| **Heat Stress (Hazard)**                                | WBGT: days with moderate heat stress (WBGT >25 °C) per grid cell | 100m                                                             | PROVIDE Climate Dashboard (Climate Analytics, 2022)                                                                                                                            |
| **Building Density (Built Environment)**                           | m³ of building mass per grid cell                                | 100m                                                             | GHS Built-Up Volume (R2023) by European Commission, Joint Research Centre (Pesaresi & Politis, 2023)                                                                           |
| **Green Vegetation Cover (Built Environment)** | Fraction of green vegetation cover per grid cell                 | 333m                                                             | Fraction of Vegetation Cover 2014-present (raster 300 m), global, 10-daily - version 1 (Copernicus Emergency Management Service, 2023)                                         |
| **Age (Socio-Demographic)**                                        | Fraction of residents over 65 years of age per grid cell         | 1km                                                              | Eurostat Census Grid 2021 (Eurostat, 2025)                                                                                                                                     |
| **Income (Socio-Demographic)**                                     | Annual gross income per person                                   | Depending on city: per district/ zone/ postal code/ census tract | Sources per City: <br>Athens: “Average reported income by Postal Code in the Athens metropolitan area for the years 2003, 2008, 2010 and 2013” (Pantazis & Psycharis, 2016)   <br>  Barcelona: “Average gross taxable income per person (€) in the city of Barcelona” (Instituto Nacional de Estadística (INE) & Atles de distribució de renda de les llars, 2022) <br>  Bologna: “Reddito imponibile medio ai fini Irpef dei residenti per quartiere e zona a Bologna dal 2007 al 2022” (Comune di Bologna & Cittá Metropolitana di Bologna, n.d.)  <br> Vienna: „Lohnsteuerpflichtige Einkommen nach Gemeindebezirken – Insgesamt 2022“ (Lohnsteuerpflichtige Einkommen Nach Gemeindebezirken – Insgesamt, 2022)
                          |


## Methodology and Output
The full analysis is run separately for each city. 
#### Initial Analysis
For each input data set, the code follows a structure of 
1) cropping input data to desired city boundary shape
2) projecting to target CRS EPSG:4326
3) first plotted map output
4) projecting values onto normalised index (0-1)
5) plot index values in map format
#### Heat Risk Index
All six input rasters are aligned in CRS, origin, and resolution based on the heat stress raster as a template and then stacked. Next, the arithmetic mean for each grid cell is calculated, creating the composite heat risk index. The heat risk index is plotted in map format.
#### Accessibility of Existing Cooling Centres
First, existing cooling centre locations (x/y coordinates) are plotted into the heat risk index map to analyse their suitability.<br>
Second, the travel time function of the locationallocation R package is used to calculate each grid cell's walking time in minutes to the nearest cooling centre. Three outputs are generated: <br>
1) Raster layer with travel time results<br>
2) Map indicating travel times in minutes for each cell by colour grading<br>
3) Travel time statistics graph indicating the share of population (Y) that accesses the nearest cooling centre within X minutes.<br>
#### Allocation of Additional Cooling Centres
The allocation is executed in three scenarios, using the allocation discrete function of the locationallocation R package. Hereby, a distinct set of locations is allocated (potential cooling centres) instead of choosing locations randomly.<br>
Scenario 1: Allocating X cooling centres to ensure 80% of the population is within a 15-minute walk, prioritising heat risk over population density.<br>
Scenario 2: Allocating X cooling centres to ensure 99% of the population (virtually everyone) is within a 15-minute walk, equally weighting heat risk and population density.<br>
Scenario 3: Limiting allocation to 3 new cooling centres, heavily prioritising heat risk for a short-term relief of the most vulnerable. <br>
Scenario parameters can be easily adapted as desired, based on the locationallocation package definitions.
#### Combined Plots for Results
For better visualisation and comparability in the thesis, the results of indidvidual analyses are group into plots per city or per parameter. The code is found in the separate "results_combined_plots" file. 

## Repository Structure
Main analysis files for each city containing all preparationa and methodology steps are in the main folder, named "cityname_full_analysis". The four files are largely the same, except for minor differences in the case of differing source data. <br>
The file "results_combined_plots" contains all plots created for visualisation within the thesis document, especially for the results section.<br>
The folder "Data" contains relevant input data to enable replication of the results. The code is already designed to load necessary input data from that folder.<br>
The folder "OSM Download Scripts" contains all files used to extract candidate facilities from the OSM platform, a necessary preparational step.<br>

## Reproducibility
Download the repository. Open the the full analysis file of the city of interest. <br>
Run the code from top to bottom, as later sections build on earlier results. The code starts with installing and loading relevant packages. Each section is clearly marked for easy navigation and visual results are plotted in R before being saved locally for visual validity checks.<br>
Note that all input data is loaded from the "Data" folder so make sure the "Data" folder is accessible in the project. For some sections, the lines around input data are commented out as those where the original steps required to crop a large raster to the city boundary. Instead, there is a shortened version simply loading the cropped raster from the "Data" folder.

## Original Code and Attribution
For the first part, the preparational analysis and heat risk index creation, the code largely uses base R, sf, terra, raster, ggplot 2, dplyr, and scales packages. <br>
The accessibility (travel time) and location allocation analysis is based on the locationallocation package created by Giacomo Falchetta. The repository can be accessed via this link: https://github.com/giacfalk/locationallocation

## Citation
Wilkens, Emily (2025). *Cooling Centres as an Urban Heat Risk Adaptation Strategy: A Comparative Geospatial Analysis of Accessibility and Population Heat Risk in Selected European Cities*. Master’s Thesis, Vienna University of Economics and Business.  
GitHub repository: masterthesis-coolingcentres. https://github.com/emiwilk/masterthesis-coolingcentres

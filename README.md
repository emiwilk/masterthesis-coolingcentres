## Master's Thesis
Cooling Centres as an Urban Heat Risk Adaptation Strategy: A Comparative Geospatial Analysis of Accessibility and Population Heat Risk in Selected European Cities

## Project Overview
This thesis examines the accessibility and spatial optimisation of cooling centre networks as an urban heat adaptation strategy in selected European cities. This study develops a transparent, replicable geospatial framework that creates a granular heat risk index (100m grid resolution) based on micro-climate, socio-demographic, and built-environmental data. This is integrated with accessibility and location-allocation modelling using open-source data and R-based computation. Four cities — Athens, Bologna, Barcelona, and Vienna — serve as case studies representing diverse climatic, demographic, and infrastructural contexts. The thesis contributes a quantitative, policy-relevant approach to guide efficient and inclusive cooling centre planning, supporting evidence-based strategies for climate-resilient and socially responsive cities.

## Research Question
The geospatial framework presented here, answers the following research questions: 
1.	How does heat risk vary inside cities and hence affect the local necessity for cooling centres?
2.	How well do existing cooling centres serve residents with increased heat risk in terms of accessibility measured in travel time in selected cities?
3.	Where should additional cooling centres be placed to maximise coverage and shorten travel times, by prioritising heat risk impacted populations?

## Data
Main input data is disclosed in this table: 
| **Risk Factor**                                | **Metric**                                                       | **Resolution**                                                   | **Data Set & Source**                                                                                                                                                          |
|------------------------------------------------|------------------------------------------------------------------|------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Population Density**                         | Number of residents per grid cell                                | 100m                                                             | GHS Population Grid (R2023) by European Commission, Joint Research Centre (Schiavina et al., 2023)                                                                             |
| **(Demand)**                                   |                                                                  |                                                                  |                                                                                                                                                                                |
| **Heat Stress**                                | WBGT: days with moderate heat stress (WBGT >25 °C) per grid cell | 100m                                                             | PROVIDE Climate Dashboard (Climate Analytics, 2022)                                                                                                                            |
| **(Hazard)**                                   |                                                                  |                                                                  |                                                                                                                                                                                |
| **Building Density**                           | m³ of building mass per grid cell                                | 100m                                                             | GHS Built-Up Volume (R2023) by European Commission, Joint Research Centre (Pesaresi & Politis, 2023)                                                                           |
| **(Built Environment)**                        |                                                                  |                                                                  |                                                                                                                                                                                |
| **Green Vegetation Cover (Built Environment)** | Fraction of green vegetation cover per grid cell                 | 333m                                                             | Fraction of Vegetation Cover 2014-present (raster 300 m), global, 10-daily - version 1 (Copernicus Emergency Management Service, 2023)                                         |
| **Age**                                        | Fraction of residents over 65 years of age per grid cell         | 1km                                                              | Eurostat Census Grid 2021 (Eurostat, 2025)                                                                                                                                     |
| **(Socio-Demographic)**                        |                                                                  |                                                                  |                                                                                                                                                                                |
| **Income**                                     | Annual gross income per person                                   | Depending on city: per district/ zone/ postal code/ census tract | Sources per City:                                                                                                                                                              |
| **(Socio-Demographic)**                        |                                                                  |                                                                  | Athens: “Average reported income by Postal Code in the Athens metropolitan area for the years 2003, 2008, 2010 and 2013” (Pantazis & Psycharis, 2016)                          |
| ****                                           |                                                                  |                                                                  | Barcelona: “Average gross taxable income per person (€) in the city of Barcelona” (Instituto Nacional de Estadística (INE) & Atles de distribució de renda de les llars, 2022) |
| ****                                           |                                                                  |                                                                  | Bologna: “Reddito imponibile medio ai fini Irpef dei residenti per quartiere e zona a Bologna dal 2007 al 2022” (Comune di Bologna & Cittá Metropolitana di Bologna, n.d.)     |
| ****                                           |                                                                  |                                                                  | Vienna: „Lohnsteuerpflichtige Einkommen nach Gemeindebezirken – Insgesamt 2022“ (Lohnsteuerpflichtige Einkommen Nach Gemeindebezirken – Insgesamt, 2022)                       |


## Methodology
Overview of applied models, reused codebases, and your modifications.

## Repository Structure
Main analysis files for each city containing all preparationa and methodology steps are in the main folder, named "cityname_full_analysis". The four files are largely the same, except for minor differences in the case of differing source data. 
The file "results_combined_plots" contains all plots created for visualisation within the thesis document, especially for the results section.
The folder "Data" contains relevant input data to enable replication of the results. The code is already designed to load necessary input data from that folder.
The folder "OSM Download Scripts" contains all files used to extract candidate facilities from the OSM platform, a necessary preparational step.

## Reproducibility
Steps to rerun the analysis and regenerate results.

## Results
Brief description of outputs generated by the code.

## Original Code and Attribution
References to reused libraries and repositories.

## Citation
Wilkens, Emily (2025). *Cooling Centres as an Urban Heat Risk Adaptation Strategy: A Comparative Geospatial Analysis of Accessibility and Population Heat Risk in Selected European Cities*. Master’s Thesis, Vienna University of Economics and Business.  
GitHub repository: masterthesis-coolingcentres. https://github.com/emiwilk/masterthesis-coolingcentres

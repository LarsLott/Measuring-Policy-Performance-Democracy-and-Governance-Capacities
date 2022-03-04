# Measuring Policy Performance, Democracy and Governance Capacieties.

Title: Replication File for “Measuring Policy Performance, Democracy, and Governance Capacities: A Conceptual and Methodological Assessment of the Sustainable Governance Indicators (SGI)” 

Author: Croissant, Aurel and Pelke, Lars

Correspondence: Lars Pelke

Date: March 2022

Required Software: R and RStudio (4.0 or Above). 
Additional required software packages and libraries are specified in the replication code.

Overview: These files replicate all analyses in Croissant and Pelke (2022): “Measuring Policy Performance, Democracy, and Governance Capacities: A Conceptual and Methodological Assessment of the Sustainable Governance Indicators (SGI)”
To replicate all analyses and figures, download all data files to a folder entitled “/data”, create a second folder entitled “/outputs” in the same directory, and run all code files in numerical order.

## R-Scripts 

- 01_main_analysis_paper.R: Replicates the data wrangling process and the research article. In addition, it compiles data need fored subsequent analysis.

- 99_sgi_time_series_handling.R: Code for gathering cross-section time-series dataset from different Excel-files. ,


## Data Files (relative path "~/data") 

Replication data the research article:
- bti: Bertelsmann Transformation Index, dta-file
- sgi: Sustainable Governance Indicators; seperate xls-files
- vdem: Varieties of Democracy Version 11, Coppegde et al. 2021, RDS-file
- wgi: Worldwide Governance Indicators, dta-file


# Readme file for analyzing the stoichiometry data of *Platymonas subcordiformis*

This file explains how to reproduce the analysis on the temperature effects on C:N:P:Chl data of *Platymonas subcordiformis* in the manuscript entitled "Growth rate and temperature interact to affect phytoplankton stoichiometry" submitted to *Journal of Phycology*.

## Author

Bingzhang Chen

## Contact email

[bingzhang.chen\@strath.ac.uk](mailto:bingzhang.chen@strath.ac.uk)

## Brief summary of the study

This study involves running a semi-continuous culture experiment on a green alga, *Platymonas subcordiformis*, under nutrient-replete, nitrogen-limited, and phosphate-limited conditions at five temperatures. We measured the cell volume, carbon (C), nitrogen (N), phosphorus (P), and chlorophyll (Chl) a at each combination of growth rate and temperature. We then estimated the minimal N:C and P:C ratios by fitting the data to the Droop model. We also ran linear models to evaluate what factors affect the phytoplankton stoichiometry (C:N:P:Chl) and cell size.

## Contributor

Bingzhang Chen is responsible for writing the R scripts.

## LICENSE

All the data and codes are covered by the MIT license. Please see the LICENSE file for details.

# Metadata

## Software and packages

The codes are written in R 4.5.0.

## How to run the code

Use R/RStudio to open **Semi_Main.R** and follow the order to run each R script.



## R scripts

-   **Semi_Main.R**: the main R script that calls other R scripts.

-   **Data_prep.R**: the R script that preprocesses the data.

-   **Fig1_Q0_temp_lit.R**: the R script that plots Fig. 1 in the manuscript showing the relationships between minimal nutrient quota and temperature compiled from the literature.

-   **Fig2_Droop_nls.R**: the R script that plots Fig. 2 in the manuscript showing the relationships between growth rate and N:C or P:C ratios at five temperatures imposed by fitted Droop equations using nonlinear least-squares fitting.

-   **Fig3_Q_Temp_nls.R**: the R script that plots Fig. 3 in the manuscript showing the estimated minimal N:C and P:C ratios and maximal growth rates at each temperature.

-   **Fig4_cell_volume.R**: the R script that plots Fig. 4 in the manuscript showing how the cell volumes vary with growth rate at different temperatures.

-   **Fig5_C2V.R**: the R script that plots Fig. 5 in the manuscript showing the relationship between cellular carbon and volume.

-   **Fig6_N2P.R**: the R script that plots Fig. 6 in the manuscript showing how N:P ratios change with growth rate and temperature.

-   **Fig7_Chl2C.R**: the R script that plots Fig. 7 in the manuscript showing how Chl:C ratios change with growth rate at five temperatures.

## Source data

-   **Semi_temp.csv**: this file contains the raw data of cell volume (**CellVol**, $\mu m$$^3$), the type of nutrient limitation (**Lim**; E: exponential growth; P: phosphorus limitation; N: nitrogen limitation), cellular particulate organic phosphorus (**POP**, $\mu$mol cell$^{-1}$), cellular particulate organic carbon (**POC**, $\mu$mol cell$^{-1}$) and nitrogen (**PON**, $\mu$mol cell$^{-1}$) as well as cellular chlorophyll (**Chl**, $10^{-3}$ $\mu$g cell$^{-1}$) under each level of growth rate (**Growth**, d$^{-1}$) and temperature (**Temp**, $^\circ$C).

-   **Q0_temp_summary.csv**: this file contains the data of minimal nutrient quota and temperature extracted from a number of studies in the literature.

## Funding

This work is funded by a Leverhulme Trust Research, UK Project Grant (RPG-2020-389) and National Science Foundation of China (41376130).

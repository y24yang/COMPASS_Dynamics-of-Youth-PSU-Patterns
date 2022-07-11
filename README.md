# COMPASS - Dynamics of Youth Polysubstance Use (PSU) Patterns
In this project, we explore the dynamic transitions of polysubstance use (PSU) patterns from a large cohort of Canadian secondary school students using [COMPASS](https://uwaterloo.ca/compass-system/about) data, and address the gap that limited evidence exists to examine the factors that impact the dynamics of PSU patterns among youth.

## Table of Contents
* [General Information](#general-information)
* [Technologies Used](#technologies-used)
* [Features](#features)
* [Future Work](#future-work)
* [Acknowledgements](#acknowledgements)
* [Contact](#contact)

## General Information
This repository documents the project aims, technologies used, features, and future work. 

## Technologies Used
In this project, data analysis was performed using the R language, open-source software to compute statistics and perform graphics. In particular, the following key packages were employed.
- [FactoMineR](https://cran.r-project.org/web/packages/FactoMineR/FactoMineR.pdf), [missMDA](https://cran.r-project.org/web/packages/missMDA/missMDA.pdf), and [naniar](https://cran.r-project.org/web/packages/naniar/naniar.pdf) packages for missing data analysis and visualization
- Multiple imputation, [MICE](https://cran.r-project.org/web/packages/mice/mice.pdf) package
- LASSO (L1 regularization), [glmnetcr](https://cran.r-project.org/web/packages/glmnetcr/glmnetcr.pdf) package for ordinal response
- Latent Markov Model (LMM), [LMest](https://cran.r-project.org/web/packages/LMest/LMest.pdf) package for generalized LMMs
- Computing Environment: RStudio Server 1.4 was set up on Ubuntu 18.04 with a 64 GiB RAM virtual machine running on Microsoft Azure

## Features
- Data cleansing 
- Missing data analysis
- Multiple imputation
- Feature selection 
- Dynamics modelling 

## Future Work
- Explore age difference and other characteristic differences in the dynamic transition of use patterns among youth by conducting a stratified analysis using the LMM method
- Add mental health assessment and data elements that reflect school environments, school health policies and practices to examine the dynamics of PSU over a longer period
- External validation

## Acknowledgements
We acknowledge the receipt of the Applied Health Sciences (AHS) scholarship and Microsoft AI for Good grant. The COMPASS study has been supported by the Canadian Institutes of Health Research, Health Canada, the Canadian Centre on Substance Abuse, the SickKids Foundation, and the Ministère de la Santé et des Services sociaux of the province of Québec. We also acknowledge the assistance received from Professor Fulvia Pennoni from the Department of Statistics and Quantitative Methods at the University of Milano-Bicocca and Francesco Bartolucci, Professor of Statistics from the Department of Economics at the University of Perugia for their expertise on longitudinal and panel data analysis and latent variable models.

## Contact
Created by [Yang (Rena) Yang](y24yang@uwaterloo.ca) - feel free to contact me!

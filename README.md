# Wittgenstein Center for Demography and Human Capital 

## Project: Demography of Adult Skills

### Introduction

This repository refers to the codes, data and procedures for constructing the dataset on the Skill-Adjusted Mean Years of Schooling for 185 countries in the period 1970-2015.

More details on the methodology can be found at:

Reiter, C., Özdemir, C., Yildiz, D., Goujon, A., Guimaraes, R., & Lutz, W. (2020). The Demography of Skills-Adjusted Human Capital [Working paper]. International Institute for Applied System Analysis. http://pure.iiasa.ac.at/id/eprint/16477/


### Project Coordinators

Wolfgang Lutz

Anne Goujon

### Researcher Team

Caner Özdemir

Claudia Reiter

Dilek Yildiz

Raquel Guimaraes 

# Usage instructions

## Folder structure

This repository contains two folders:

1. SAMYS_empirical
2. adj_fact

Estimation of the complete dataset starts with the SAMYS_empirical, and then follows to the calculation of the adjustment factor.

### Estimation of the SAMYS for 44 countries with full complete data.

Need to summarize here based on Claudia Files

### Estimation of the SAMYS for the rest of the world (regression adjustment)

Attention: Data preparation files are renamed with the initial word "prep_"

The user can replicate SAMYS calculations with the codes listed in "Estimation" section
We also provide additional code used for data preparation and visualisation

The R Code for SAMYS estimation can be found on the script "./code/samys_estimation_1970_2015.R"

The data required are available "./data" folder, and regional averages are calculated in the R-script "./code/regional_averages_1970_2015.R"

We provide also additional files, as follows

- R Code to compute the QEI regression model: "./code/qei_estimation.R"
- Literacy is estimated in six steps starting with "prep_literacy_1_merge_unesco.R"
- Literacy data preperation also includes a Stata code at step 5
- Stata code for teacher-pupil ratio: "prep_tp_ratio.do"
- Stata code for education expenditure: "prep_edu_exp.do"

Folder description

./code: code for data preparation and for regression models
./data: raw and manipulated data
./figures
./results
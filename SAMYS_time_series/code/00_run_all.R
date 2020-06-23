#============================================================#
#=====> SAMYS Time Series Estimation for 185 countries <=====#
#============================================================#

# All the necessary R files called can be sourced from this file. 
# No need to run seperate R files. 
# Steps 5, 10 and 11 require running Stata do files. 

# The results are provided in the folder "./results/" in csv format.

# Remove all objects from memory
rm(list = ls())

#### Data preparation ####
# Step 1: Read literacy data from UNESCO country files
source('./code/01_prep_literacy_1.R')

# Step 2: Merge World Bank male and female literacy rates
source('./code/02_prep_literacy_2.R')

# Step 3: Merge literacy data from UNESCO files and WB data 
source('./code/03_prep_literacy_3.R')

# Step 4: Estimate missing literacy data for European countries
source('./code/04_prep_literacy_4.R')

# Step 5: Estimate missing literacy data for rest of the countries
## RUN './code/04_prep_literacy_5.do' Stata do file

# Step 6: Put the data in correct format
source('./code/06_prep_literacy_6.R')

# Step 7: WIC Data Explorer proportion with lower than upper secondary education
source('./code/07_prep_ls.R')

# Step 8: WIC Data Explorer Mean Years of Schooling
source('./code/08_prep_mys.R')

# Step 9: Calculate DHS empirical SAMYS
source('./code/09_prep_dhs_empirical_samys.R')

# Step 10: Prepare teacher pupil ratio 
## RUN './code/10_prep_tp_ratio.do' Stata do file

# Step 11: Prepare education expenditure
## RUN './code/11_prep_edu_exp.do' Stata do file
 
# Step 12: WIC Data Explorer old age dependency
source('./code/12_prep_dep.R')

#### Estimation ####
# Step 13 Estimate Quality of Education Indicator
source('./code/13_est_qei.R')

# Step 13 Estimate SAMYS
source('./code/14_est_samys_1970_2015.R')

#### Calculate regional averages ####
source('./code/15_regional_averages_1970_2015.R')



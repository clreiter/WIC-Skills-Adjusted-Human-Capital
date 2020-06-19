setwd("/Users/claudiareiter/Documents/GitHub/WiC-Human-Capital-Quality-Projections/SAMYS_empirical")
rm(list = ls())


#LOAD PACKAGES
#install.packages("intsvy")
library("intsvy")
#install.packages("plyr")
library("plyr")
#install.packages("dplyr")
library("dplyr")
#install.packages("zoo")
library("zoo")
#install.packages("foreign")
library("foreign")
#install.packages("openxlsx", dependencies = TRUE)
library("openxlsx")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("tidyr")
library("tidyr")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#1 ESTIMATE STANDARD OF COMPARISON (POPULATION-WEIGHTED OECD MEAN PIAAC LITERACY SCORES)
#-----------------------------------------------------------------------------------------------------------

#read PIAAC data
piaac_all_countries<-read.csv("Input/PIAAC_data.csv")   

#-----------------------------------------------------------------------------------------------------------
#OECD MEAN PIAAC LITERACY SCORES BY COUNTRY (Population aged 20-64)

#keep only PIAAC observations with age >= 20
piaac<-subset(piaac_all_countries, AGEG5LFS>1)

#calculate mean literacy score by country for all PIAAC countries
lit_mean_piaac_country<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID"), data=piaac)
lit_mean_piaac_country<-plyr::rename(lit_mean_piaac_country, c("CNTRYID"="iso", "Freq"="n", "Mean"="mean_country"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac_country, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#read WIC OECD population size
OECD_pop<-read.csv("Input/WIC_oecd_pop.csv")
#keep only population for total country (age 20-64)
OECD_pop<-subset(OECD_pop, age==0 & sex==0 & educ==0)

#calculate population-weighted OECD PIAAC literacy mean
lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, mean_country!="NA")  #remove country-age-sex-combinations with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$mean_country * lit_mean_OECD_weighted$share  #calculate weighted means

OECD_average<-aggregate(cbind(n, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#2 CALCULATE BASE YEAR (2015) EMPIRICAL SAMYS (POP 20-64)
#-----------------------------------------------------------------------------------------------------------
rm(list= ls()[!(ls() %in% c("piaac_all_countries", "lit_mean_piaac_country", "OECD_average"))])

#2015 SAMYS BY COUNTRY (population 20-64)

#use PIAAC mean literacy scores by country
lit_mean_piaac_country$age<-0
lit_mean_piaac_country$sex<-0
lit_mean_piaac_country$educ<-0

#calculate skills adjustment factor
samys_by_country_piaac<-merge(OECD_average, lit_mean_piaac_country, by=c("age", "sex", "educ"))
samys_by_country_piaac$adj_factor<-samys_by_country_piaac$mean_country/samys_by_country_piaac$weighted_mean

#read WIC MYS 
wic_mys<-read.csv("Input/WIC_mys.csv")
#keep only MYS for total population (age 20-64)
wic_mys<-subset(wic_mys, age==0 & sex==0 & educ==0 & year==2015)

samys_by_country_piaac<-merge(wic_mys, samys_by_country_piaac, by=c("iso", "age", "sex", "educ"))
samys_by_country_piaac$samys<-samys_by_country_piaac$mys*samys_by_country_piaac$adj_factor
samys_by_country_piaac<-samys_by_country_piaac[c("iso", "country", "age", "sex","educ", "mys", "adj_factor", "samys")]

#-----------------------------------------------------------------------------------------------------------
#add STEP data (SAMYS by country, age, sex, educ are aggregated based on population distribution)
step_lit_mean<-read.csv("Input/STEP_mean_lit.csv")
step_lit_mean<-subset(step_lit_mean, age>15 & sex>0 & educ>0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)

#fill missing values with data of subsequent education groups (same country-age-sex-group) for education categories 1 and 2, and fill with data from lower education groups for education categories 3 and 4 
step_lit_mean<-step_lit_mean[with(step_lit_mean, order(iso, age, sex, educ)),]

step_lit_mean_educ234<-step_lit_mean %>% subset(educ!=1) %>%
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=FALSE))

step_lit_mean_educ123<- step_lit_mean %>% subset(educ!=4) %>%
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=TRUE))

step_lit_mean_educ1<-subset(step_lit_mean_educ123, educ==1)
step_lit_mean_educ2<-subset(step_lit_mean_educ123, educ==2)
step_lit_mean_educ3<-subset(step_lit_mean_educ234, educ==3)
step_lit_mean_educ4<-subset(step_lit_mean_educ234, educ==4)
step_lit_mean<-rbind(step_lit_mean_educ1, step_lit_mean_educ2, step_lit_mean_educ3, step_lit_mean_educ4)
step_lit_mean<-step_lit_mean[with(step_lit_mean, order(iso, age, sex, educ)),]

#---------------------------------------------------------------------------------------------------------------------------------
#FOR ESTIMATION OF URBAN-RURAL ADJUSTMENT FACTOR USING DHS DATA SEE "STEP_urban_adjustment_DHS.xlsx IN INPUT FOLDER
#---------------------------------------------------------------------------------------------------------------------------------

#STEP urban/rural adjustments
correction<-read.xlsx("Input/STEP_urban_adjustment_DHS.xlsx", sheet=2)
step_lit_mean<-merge(step_lit_mean, correction)
step_lit_mean$mean_country_corrected<-step_lit_mean$mean_country*step_lit_mean$urban_adjustment

#aggregate literacy scores based on population distribution
wic_pop<-read.csv("Input/WIC_pop.csv")
wic_pop<-aggregate(pop~iso+year+age+sex+educ, FUN=sum, data=wic_pop)
wic_pop_2015<-subset(wic_pop, year==2015)         #use only 2015 values
step_lit_mean<-merge(step_lit_mean, wic_pop_2015, by=c("iso", "age", "sex", "educ"))
step_lit_mean_weighted<-step_lit_mean %>% 
  dplyr::group_by(iso) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
step_lit_mean_weighted$weighted_score<-step_lit_mean_weighted$mean_country_corrected*step_lit_mean_weighted$share
step_lit_mean_country<-aggregate(weighted_score~iso+country+year, FUN=sum, data=step_lit_mean_weighted)

#calculate SAMY
step_lit_mean_country$age<-0
step_lit_mean_country$sex<-0
step_lit_mean_country$educ<-0

#calculate skills adjustment factor
samys_by_country_step<-merge(OECD_average, step_lit_mean_country, by=c("age", "sex", "educ"))
samys_by_country_step$adj_factor<-samys_by_country_step$weighted_score/samys_by_country_step$weighted_mean

samys_by_country_step<-merge(wic_mys, samys_by_country_step, by=c("iso", "country", "age", "sex", "educ"))
samys_by_country_step$samys<-samys_by_country_step$mys*samys_by_country_step$adj_factor
samys_by_country_step<-samys_by_country_step[c("iso", "country", "age", "sex","educ", "mys", "adj_factor", "samys")]

samys_by_country_2015<-rbind(samys_by_country_piaac, samys_by_country_step)
samys_by_country_2015$year<-2015

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#3 ESTIMATE STANDARD AGEING PATTERN
#-----------------------------------------------------------------------------------------------------------
rm(list= ls()[!(ls() %in% c("piaac_all_countries", "samys_by_country_2015", "OECD_average"))])

#read IALS data
ials_original<-read.spss("Input/IALS_data.sav", to.data.frame=TRUE)

#recode countries
ials<-dplyr::mutate(ials_original, CNTRYID=ifelse (CNTRID==1 | CNTRID==2, 124, 0))              #Canada (Englisch & French)
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==3 | CNTRID==4 | CNTRID==22, 756, CNTRYID))    #Switzerland (German & French & Italian)
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==5, 276, CNTRYID))                             #Germany
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==6, 840, CNTRYID))                             #USA
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==7, 372, CNTRYID))                             #Ireland
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==8, 528, CNTRYID))                             #Netherlands
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==9, 616, CNTRYID))                             #Poland
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==11, 752, CNTRYID))                            #Sweden
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==13, 554, CNTRYID))                            #New Zealand
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==14 | CNTRID==15, 826, CNTRYID))               #UK (GBR & Northern Ireland)
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==16, 56, CNTRYID))                             #Belgium (Flanders)
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==17, 380, CNTRYID))                            #Italy
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==18, 578, CNTRYID))                            #Norway (Bokmal)
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==20, 705, CNTRYID))                            #Slovenia
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==21, 203, CNTRYID))                            #Czechia
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==23, 208, CNTRYID))                            #Denmark
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==24, 246, CNTRYID))                            #Finland
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==25, 348, CNTRYID))                            #Hungary
ials<-dplyr::mutate(ials, CNTRYID=ifelse (CNTRID==29, 152, CNTRYID))                            #Chile

#rename replicate factors (for intsvy package)
ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

#recode age corresponding to PIAAC
#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, USA, Netherlands) - 17 years
ials_1994_2011<-subset(ials, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==528 | CNTRYID==840)
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>12 & AGE<18, 4, 0))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>17 & AGE<23, 5, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>22 & AGE<28, 6, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>27 & AGE<33, 7, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>32 & AGE<38, 8, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>37 & AGE<43, 9, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>42 & AGE<48, 10, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>47 & AGE<53, 11, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>52 & AGE<58, 12, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>57 & AGE<64, 13, age))
ials_1994_2011<-dplyr::mutate(ials_1994_2011, age=ifelse (AGE>63 & AGE<68, 14, age))
ials_1994_2011<-subset(ials_1994_2011, age>0)

#countries with IALS in 1996 and PIAAC in 2011 (UK, Belgium) - 15 years
ials_1996_2011<-subset(ials, CNTRYID==56 | CNTRYID==826)
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>14 & AGE<20, 4, 0))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>19 & AGE<25, 5, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>24 & AGE<30, 6, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>29 & AGE<35, 7, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>34 & AGE<40, 8, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>39 & AGE<45, 9, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>44 & AGE<50, 10, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>49 & AGE<55, 11, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>54 & AGE<60, 12, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>59 & AGE<66, 13, age))
ials_1996_2011<-dplyr::mutate(ials_1996_2011, age=ifelse (AGE>65 & AGE<71, 14, age))
ials_1996_2011<-subset(ials_1996_2011, age>0)

#countries with IALS in 1998 and PIAAC in 2011 (Norway, Finland, Czechia, Denmark, Italy) - 13 years
ials_1998_2011<-subset(ials, CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380)
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>11 & AGE<17, 3, 0))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>16 & AGE<22, 4, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>21 & AGE<27, 3, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>26 & AGE<32, 4, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>31 & AGE<37, 5, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>36 & AGE<42, 6, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>41 & AGE<47, 7, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>46 & AGE<52, 8, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>51 & AGE<57, 9, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>56 & AGE<62, 10, age))
ials_1998_2011<-dplyr::mutate(ials_1998_2011, age=ifelse (AGE>61 & AGE<67, 11, age))
ials_1998_2011<-subset(ials_1998_2011, age>0)

#countries with IALS in 1996 and PIAAC in 2014 (New Zealand) - 18 years
ials_1996_2014<-subset(ials, CNTRYID==554)
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>11 & AGE<17, 4, 0))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>16 & AGE<22, 5, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>21 & AGE<27, 6, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>26 & AGE<32, 7, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>31 & AGE<37, 8, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>36 & AGE<42, 9, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>41 & AGE<47, 10, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>46 & AGE<52, 11, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>51 & AGE<57, 12, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>56 & AGE<62, 13, age))
ials_1996_2014<-dplyr::mutate(ials_1996_2014, age=ifelse (AGE>61 & AGE<67, 14, age))
ials_1996_2014<-subset(ials_1996_2014, age>0)

#countries with IALS in 1998 and PIAAC in 2014 (Chile, Slovenia) - 16 years
ials_1998_2014<-subset(ials, CNTRYID==152 | CNTRYID==705)
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>13 & AGE<19, 4, 0))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>18 & AGE<24, 5, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>23 & AGE<29, 6, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>28 & AGE<34, 7, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>33 & AGE<39, 8, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>38 & AGE<44, 9, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>43 & AGE<49, 10, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>48 & AGE<54, 11, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>53 & AGE<59, 12, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>58 & AGE<64, 13, age))
ials_1998_2014<-dplyr::mutate(ials_1998_2014, age=ifelse (AGE>63 & AGE<69, 14, age))
ials_1998_2014<-subset(ials_1998_2014, age>0)

#countries with IALS in 1998 and PIAAC in 2017 (Hungary) - 19 years
ials_1998_2017<-subset(ials, CNTRYID==348)
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>10 & AGE<16, 4, 0))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>15 & AGE<21, 5, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>20 & AGE<26, 6, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>25 & AGE<31, 7, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>30 & AGE<36, 8, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>35 & AGE<41, 9, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>40 & AGE<46, 10, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>45 & AGE<51, 11, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>50 & AGE<56, 12, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>55 & AGE<61, 13, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>60 & AGE<66, 14, age))
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>65 & AGE<71, 15, age))
ials_1998_2017<-subset(ials_1998_2017, age>0)

ials_rev<-rbind(ials_1994_2011, ials_1996_2011, ials_1996_2014, ials_1998_2011, ials_1998_2014, ials_1998_2017)
ials_rev$test<-"IALS"

#configuration for using intsvy package with IALS data
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

#calculate IALS literacy mean by country, age and education (2 categories)
lit_mean_ials_age_educ<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("age", "test", "educ", "CNTRYID"), data=ials_rev, config=piaac_conf)
lit_mean_ials_age_educ$age<-as.numeric(as.character(lit_mean_ials_age_educ$age))
lit_mean_ials_age_educ<-subset(lit_mean_ials_age_educ, age>3 & age<12)
lit_mean_ials<-aggregate(Mean ~ age+test+educ, FUN=mean, data=lit_mean_ials_age_educ)

#keep only IALS countries in PIAAC data
piaac<-subset(piaac_all_countries, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==528 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380 | CNTRYID==554 |  CNTRYID==152 | CNTRYID==705 | CNTRYID==348 | CNTRYID==840 | CNTRYID==826)

#define 2 broad education categories in PIAAC data
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 

#calculate PIAAC literacy mean by country and education (2 categories)
lit_mean_piaac_age_educ<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "educ", "CNTRYID"), data=piaac)
lit_mean_piaac_age_educ$test<-"PIAAC"
lit_mean_piaac_age_educ<-plyr::rename(lit_mean_piaac_age_educ, c("AGEG5LFS"="age"))
lit_mean_piaac_age_educ<-lit_mean_piaac_age_educ[!(lit_mean_piaac_age_educ$age==4 & lit_mean_piaac_age_educ$CNTRYID==348),]
lit_mean_piaac<-aggregate(Mean ~ age+test+educ, FUN=mean, data=lit_mean_piaac_age_educ)

#combine IALS and PIAAC data
IALS_PIAAC<-rbind(lit_mean_ials, lit_mean_piaac)

#---------------------------------------------------------------------------------------------------------------
#calculate period-adjustment factor

#remove countries without age information in IALS
ials<-subset(ials, CNTRYID!=124)    #remove Canada
#remove countries which did not participate in PIAAC
ials<-subset(ials, CNTRYID!=756)    #remove Switzerland

#calculate IALS literacy mean by country and education (2 categories)
lit_mean_ials_by_country<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("educ", "CNTRYID"), data=ials, config=piaac_conf)
lit_mean_ials_by_country$test<-"IALS"
lit_mean_ials_by_country<-plyr::rename(lit_mean_ials_by_country, c("Mean"="mean_ials"))
lit_mean_ials_country<-aggregate(mean_ials~educ+test, FUN=mean, data=lit_mean_ials_by_country)

#calculate PIAAC literacy mean by country and education (2 categories)
lit_mean_piaac_by_country<-piaac.mean.pv(pvlabel = "LIT", by=c("educ", "CNTRYID"), data=piaac)
lit_mean_piaac_by_country$test<-"PIAAC"
lit_mean_piaac_by_country<-plyr::rename(lit_mean_piaac_by_country, c("Mean"="mean_piaac"))
lit_mean_piaac_country<-aggregate(mean_piaac~educ+test, FUN=mean, data=lit_mean_piaac_by_country)

period_adj<-merge(lit_mean_ials_country, lit_mean_piaac_country, by=c("educ"))
period_adj<-period_adj[c("educ", "mean_ials", "mean_piaac")]
period_adj$adj_factor<-period_adj$mean_piaac/period_adj$mean_ials

ageing_pattern<-merge(IALS_PIAAC, period_adj, by=c("educ"))
ageing_pattern$adj_Mean<-ifelse(ageing_pattern$test=="IALS", ageing_pattern$Mean*ageing_pattern$adj_factor, ageing_pattern$Mean)
ageing_pattern$age<-as.numeric(as.character(ageing_pattern$age))
ageing_pattern$age<-(ageing_pattern$age*5)+10
ageing_pattern<-subset(ageing_pattern, age>25 & age<65)
ageing_pattern$age<-as.factor(ageing_pattern$age)
ageing_pattern$educ<-factor(ageing_pattern$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

#plot standard ageing pattern
#ggplot(ageing_pattern, aes(x=test, y=adj_Mean, color=age, group=age)) + 
  #geom_line(size=1) + 
  #geom_point() + facet_wrap(~educ, ncol=2, nrow=1, scales="free_x") + 
  #xlab("Test") + ylab("Period-adjusted literacy mean") + 
  #scale_color_discrete(name="Age at PIAAC", labels=c("30-34", "35-39", "40-44", "45-49","50-54", "55-59", "60-64")) + 
  #theme_bw()
#ggsave("Output/standard_ageing_pattern.jpg", width =6.8, height=2.9)

#---------------------------------------------------------------------------------------------------------------------------------
#FOR ESTIMATION OF SKILL GROWTH FUNCTION AND RECONSTRUCTION OF PIAAC/STEP SCORES SEE "reconstruction_score.xlsx" IN INPUT FOLDER
#---------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#4 RECONSTRUCTION OF SAMYS 1970-2015 (POP 20-64)
#-----------------------------------------------------------------------------------------------------------
rm(list= ls()[!(ls() %in% c("samys_by_country_2015", "OECD_average"))])

#read reconstructed PIAAC/STEP scores
scores_over_time_wide<-read.xlsx("Input/reconstruction_scores.xlsx", sheet=3)
scores_over_time_wide<-scores_over_time_wide[,c(1,2,4,5,6,9:17)]
scores_over_time<-gather(scores_over_time_wide, year, score, score_2010:score_1970, factor_key=TRUE)
scores_over_time$year<-gsub("score_", "",scores_over_time$year)

#read WIC population to aggregate SAMYS based on population distribution
wic_pop<-read.csv("Input/WIC_pop.csv")
wic_pop<-subset(wic_pop, age>0 & sex>0 & educ>0)
wic_pop<-aggregate(pop~iso+year+age+sex+educ, FUN=sum, data=wic_pop)

reconstruction<-merge(scores_over_time, wic_pop, by=c("iso", "year", "age", "sex", "educ"))

#SAMYS by country (Pop 20-64) and year (1970-2015)

#aggregate scores based on population distribution
samys_country<-reconstruction %>% subset(age>15) %>% 
  dplyr::group_by(iso, year) %>% dplyr::mutate(share = pop/sum(pop)) 
samys_country<-samys_country[c("iso", "country", "year", "age", "sex", "educ", "score", "share")]
samys_country$score_weighted<-samys_country$share*samys_country$score
samys_country<-aggregate(score_weighted~iso+year, FUN=sum, data=samys_country)

#merge with pop-weighted OECD literacy mean to calculate skills adjustment
oecd_avg<-subset(OECD_average, age==0 & sex==0 & educ==0)
samys_country<-merge(oecd_avg, samys_country)
samys_country$adj_factor<-samys_country$score_weighted/samys_country$weighted_mean

#read WIC MYS
wic_mys<-read.csv("Input/WIC_mys.csv")

#combine with aggregated reconstructed scores
samys_country<-merge(wic_mys, samys_country, by=c("iso", "year", "age", "sex", "educ"))
samys_country$samys<-samys_country$adj_factor*samys_country$mys
samys_country<-samys_country[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "samys")]

samys_1970_2015<-rbind(samys_country, samys_by_country_2015)
write.csv(samys_1970_2015, "Output/samys_1970-2015.csv", row.names=FALSE)


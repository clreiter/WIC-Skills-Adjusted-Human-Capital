setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC")   #VID
setwd("C:/Users/reiter/OneDrive - IIASA/PIAAC")   #IIASA
setwd("/Users/claudiareiter/OneDrive - IIASA/PIAAC")   #Laptop
rm(list = ls())

library(foreign)

#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

#recode age corresponding to PIAAC
#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, USA, Netherlands) - 17 years
ials_1994_2011<-subset(ials, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528)
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
ials_1996_2011<-subset(ials, CNTRYID==826 | CNTRYID==56)
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
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>15 & AGE<21, 5, 0))
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


library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

#lit_mean_ials_1994_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1994_2011, config=piaac_conf)
lit_mean_ials_1994_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1994_2011, config=piaac_conf)
lit_mean_ials_1994_2011$year<-1994
lit_mean_ials_1994_2011$test<-"IALS"
lit_mean_ials_1994_2011$age<-as.numeric(as.character(lit_mean_ials_1994_2011$age))
lit_mean_ials_1994_2011$age_piaac<-10+(lit_mean_ials_1994_2011$age*5)
lit_mean_ials_1994_2011$age_test<-lit_mean_ials_1994_2011$age_piaac-17

#lit_mean_ials_1996_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1996_2011, config=piaac_conf)
lit_mean_ials_1996_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1996_2011, config=piaac_conf)
lit_mean_ials_1996_2011$year<-1996
lit_mean_ials_1996_2011$test<-"IALS"
lit_mean_ials_1996_2011$age<-as.numeric(as.character(lit_mean_ials_1996_2011$age))
lit_mean_ials_1996_2011$age_piaac<-10+(lit_mean_ials_1996_2011$age*5)
lit_mean_ials_1996_2011$age_test<-lit_mean_ials_1996_2011$age_piaac-15

#lit_mean_ials_1998_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1998_2011, config=piaac_conf)
lit_mean_ials_1998_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1998_2011, config=piaac_conf)
lit_mean_ials_1998_2011$year<-1998
lit_mean_ials_1998_2011$test<-"IALS"
lit_mean_ials_1998_2011$age<-as.numeric(as.character(lit_mean_ials_1998_2011$age))
lit_mean_ials_1998_2011$age_piaac<-10+(lit_mean_ials_1998_2011$age*5)
lit_mean_ials_1998_2011$age_test<-lit_mean_ials_1998_2011$age_piaac-13

#lit_mean_ials_1996_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1996_2014, config=piaac_conf)
lit_mean_ials_1996_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1996_2014, config=piaac_conf)
lit_mean_ials_1996_2014$year<-1996
lit_mean_ials_1996_2014$test<-"IALS"
lit_mean_ials_1996_2014$age<-as.numeric(as.character(lit_mean_ials_1996_2014$age))
lit_mean_ials_1996_2014$age_piaac<-10+(lit_mean_ials_1996_2014$age*5)
lit_mean_ials_1996_2014$age_test<-lit_mean_ials_1996_2014$age_piaac-18

#lit_mean_ials_1998_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1998_2014, config=piaac_conf)
lit_mean_ials_1998_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1998_2014, config=piaac_conf)
lit_mean_ials_1998_2014$year<-1998
lit_mean_ials_1998_2014$test<-"IALS"
lit_mean_ials_1998_2014$age<-as.numeric(as.character(lit_mean_ials_1998_2014$age))
lit_mean_ials_1998_2014$age_piaac<-10+(lit_mean_ials_1998_2014$age*5)
lit_mean_ials_1998_2014$age_test<-lit_mean_ials_1998_2014$age_piaac-16

#lit_mean_ials_1998_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=ials_1998_2017, config=piaac_conf)
lit_mean_ials_1998_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_1998_2017, config=piaac_conf)
lit_mean_ials_1998_2017$year<-1998
lit_mean_ials_1998_2017$test<-"IALS"
lit_mean_ials_1998_2017$age<-as.numeric(as.character(lit_mean_ials_1998_2017$age))
lit_mean_ials_1998_2017$age_piaac<-10+(lit_mean_ials_1998_2017$age*5)
lit_mean_ials_1998_2017$age_test<-lit_mean_ials_1998_2017$age_piaac-19

lit_mean_ials<-rbind(lit_mean_ials_1994_2011, lit_mean_ials_1996_2011, lit_mean_ials_1996_2014, lit_mean_ials_1998_2011, lit_mean_ials_1998_2014, lit_mean_ials_1998_2017)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso", "age"="cohort"))

#-----------------------------------------------------------------------------------------------------

#ALL
all_original<-read.spss("ALL03-08/Data/SPSS/ALL.sav", to.data.frame=TRUE)

#recode countries
all<-dplyr::mutate(all_original, CNTRYID=ifelse (CNTRID=="Canada (English)" | CNTRID=="ukda (French)", 124, 0))                                            #Canada (Englisch & French)
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Switzerland (German)" | CNTRID=="Switzerland (French)" | CNTRID=="Switzerland (Italian)", 756, CNTRYID))     #Switzerland (German & French & Italian)
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="USA", 840, CNTRYID))                            #USA
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Netherlands", 528, CNTRYID))                            #Netherlands
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="New Zealand", 554, CNTRYID))                            #New Zealand
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Italy (Italian)", 380, CNTRYID))                            #Italy
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Norway (Bokmal)", 578, CNTRYID))                            #Norway (Bokmal)
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Hungary", 348, CNTRYID))                            #Hungary
all<-dplyr::mutate(all, CNTRYID=ifelse (CNTRID=="Bermuda (English)", 60, CNTRYID))                             #Bermuda

all<-plyr::rename(all, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#all<-plyr::rename(all, c("GENDER"="sex"))

#recode education
all<-dplyr::mutate(all, educ=ifelse (A4==1 | A4==2, 1, 0))  
all<-dplyr::mutate(all, educ=ifelse (A4>2 & A4<12, 2, educ)) 
all<-subset(all, educ>0)
all$AGE_RESP<-as.numeric(as.character(all$AGE_RESP))


#recode age corresponding to PIAAC
#countries with ALL in 2003 and PIAAC in 2011 (Italy, USA, Norway) - 8 years
all_2003_2011<-subset(all, CNTRYID==380 | CNTRYID==840 | CNTRYID==578)
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>16 & AGE_RESP<22, 3, 0))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>21 & AGE_RESP<27, 4, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>26 & AGE_RESP<32, 5, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>31 & AGE_RESP<37, 6, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>36 & AGE_RESP<42, 7, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>41 & AGE_RESP<47, 8, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>46 & AGE_RESP<52, 9, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>51 & AGE_RESP<57, 10, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>56 & AGE_RESP<62, 11, age))
all_2003_2011<-dplyr::mutate(all_2003_2011, age=ifelse (AGE_RESP>61 & AGE_RESP<67, 12, age))
all_2003_2011<-subset(all_2003_2011, age>0)

#countries with ALL in 2007 and PIAAC in 2014 (Netherlands, New Zealand) - 7 years
all_2007_2014<-subset(all, CNTRYID==528 | CNTRYID==554)
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>17 & AGE_RESP<23, 3, 0))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>22 & AGE_RESP<28, 4, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>27 & AGE_RESP<33, 5, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>32 & AGE_RESP<38, 6, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>37 & AGE_RESP<43, 7, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>42 & AGE_RESP<48, 8, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>47 & AGE_RESP<53, 9, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>52 & AGE_RESP<58, 10, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>57 & AGE_RESP<63, 11, age))
all_2007_2014<-dplyr::mutate(all_2007_2014, age=ifelse (AGE_RESP>62 & AGE_RESP<68, 12, age))
all_2007_2014<-subset(all_2007_2014, age>0)

#countries with ALL in 2007 and PIAAC in 2017 (Hungary) - 10 years
all_2007_2017<-subset(all, CNTRYID==348)
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>14 & AGE_RESP<20, 3, 0))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>19 & AGE_RESP<25, 4, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>24 & AGE_RESP<30, 5, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>29 & AGE_RESP<35, 6, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>34 & AGE_RESP<40, 7, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>39 & AGE_RESP<45, 8, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>44 & AGE_RESP<50, 9, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>49 & AGE_RESP<55, 10, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>54 & AGE_RESP<60, 11, age))
all_2007_2017<-dplyr::mutate(all_2007_2017, age=ifelse (AGE_RESP>59 & AGE_RESP<66, 12, age))
all_2007_2017<-subset(all_2007_2017, age>0)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"

#lit_mean_all_2003_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=all_2003_2011, config=piaac_conf)
lit_mean_all_2003_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=all_2003_2011, config=piaac_conf)
lit_mean_all_2003_2011$year<-2003
lit_mean_all_2003_2011$test<-"ALL"
lit_mean_all_2003_2011$age<-as.numeric(as.character(lit_mean_all_2003_2011$age))
lit_mean_all_2003_2011$age_piaac<-10+(lit_mean_all_2003_2011$age*5)
lit_mean_all_2003_2011$age_test<-lit_mean_all_2003_2011$age_piaac-8

#lit_mean_all_2007_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=all_2007_2014, config=piaac_conf)
lit_mean_all_2007_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=all_2007_2014, config=piaac_conf)
lit_mean_all_2007_2014$year<-2007
lit_mean_all_2007_2014$test<-"ALL"
lit_mean_all_2007_2014$age<-as.numeric(as.character(lit_mean_all_2007_2014$age))
lit_mean_all_2007_2014$age_piaac<-10+(lit_mean_all_2007_2014$age*5)
lit_mean_all_2007_2014$age_test<-lit_mean_all_2007_2014$age_piaac-7

#lit_mean_all_2007_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "sex", "educ"), data=all_2007_2017, config=piaac_conf)
lit_mean_all_2007_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=all_2007_2017, config=piaac_conf)
lit_mean_all_2007_2017$year<-2007
lit_mean_all_2007_2017$test<-"ALL"
lit_mean_all_2007_2017$age<-as.numeric(as.character(lit_mean_all_2007_2017$age))
lit_mean_all_2007_2017$age_piaac<-10+(lit_mean_all_2007_2017$age*5)
lit_mean_all_2007_2017$age_test<-lit_mean_all_2007_2017$age_piaac-10

lit_mean_all<-rbind(lit_mean_all_2003_2011, lit_mean_all_2007_2014, lit_mean_all_2007_2017)
lit_mean_all<-plyr::rename(lit_mean_all, c("CNTRYID"="iso", "age"="cohort"))

#--------------------------------------------------------------------------------------------------------------------------------------------------

#PIAAC
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

#lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528 | CNTRYID==826 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, 2011, 0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==152 | CNTRYID==705 | CNTRYID==554, 2014, year))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==348, 2017, year))
lit_mean_piaac<-subset(lit_mean_piaac, year>0)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age_piaac<-10+(lit_mean_piaac$age*5)
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528, (age_piaac-17),0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==826 | CNTRYID==56, (age_piaac-15),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, (age_piaac-13),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==554, (age_piaac-18),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==152 | CNTRYID==705, (age_piaac-16),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==348, (age_piaac-19),age_test))
lit_mean_piaac<-subset(lit_mean_piaac, age_test>0)
#lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("age"="cohort", "CNTRYID"="iso", "GENDER_R"="sex"))
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("age"="cohort", "CNTRYID"="iso"))

IALS_ALL_PIAAC<-rbind(lit_mean_ials, lit_mean_all, lit_mean_piaac)
isono<-read.csv("OECD-average/isono.csv")
IALS_ALL_PIAAC<-merge(IALS_ALL_PIAAC, isono, by=c("iso"))
#write.csv(IALS_ALL_PIAAC, "Cohort Analysis/IALS_ALL_PIAAC_cohort_age-sex-educ.csv", row.names=FALSE)
write.csv(IALS_ALL_PIAAC, "Cohort Analysis/IALS_ALL_PIAAC_cohort_age-educ.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------

#PERIOD EFFECT BY EDUCATION
#ials
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>19 & AGE<25, 20, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>24 & AGE<30, 25, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>29 & AGE<35, 30, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>34 & AGE<40, 35, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>39 & AGE<45, 40, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>44 & AGE<55, 45, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>49 & AGE<60, 50, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>54 & AGE<65, 55, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>59 & AGE<66, 60, age_test))
ials<-subset(ials, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test", "educ"), data=ials, config=piaac_conf)

lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528, 1994, 0))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==826 | CNTRYID==56 | CNTRYID==554, 1996, year))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380 | CNTRYID==152 | CNTRYID==705 | CNTRYID==348, 1998, year))
lit_mean_ials<-subset(lit_mean_ials, year>0)

lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))
lit_mean_ials$age_piaac<-lit_mean_ials$age_test
lit_mean_ials$cohort<-"NA"
lit_mean_ials$test<-"IALS"

#all
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>14 & AGE_RESP<20, 15, 0))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>19 & AGE_RESP<25, 20, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>24 & AGE_RESP<30, 25, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>29 & AGE_RESP<35, 30, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>34 & AGE_RESP<40, 35, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>39 & AGE_RESP<45, 40, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>44 & AGE_RESP<55, 45, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>49 & AGE_RESP<60, 50, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>54 & AGE_RESP<65, 55, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>59 & AGE_RESP<66, 60, age_test))
all<-subset(all, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"
lit_mean_all<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test", "educ"), data=all, config=piaac_conf)

lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==380 | CNTRYID==840 | CNTRYID==578, 2003, 0))
lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==528 | CNTRYID==554 | CNTRYID==348, 2007, year))
lit_mean_all<-subset(lit_mean_all, year>0)

lit_mean_all<-plyr::rename(lit_mean_all, c("CNTRYID"="iso"))
lit_mean_all$age_piaac<-lit_mean_all$age_test
lit_mean_all$cohort<-"NA"
lit_mean_all$test<-"ALL"

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528 | CNTRYID==826 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, 2011, 0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==152 | CNTRYID==705 | CNTRYID==554, 2014, year))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==348, 2017, year))
lit_mean_piaac<-subset(lit_mean_piaac, year>0)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age_piaac"))
lit_mean_piaac$age_piaac<-as.numeric(as.character(lit_mean_piaac$age_piaac))
lit_mean_piaac$age_piaac<-10+(lit_mean_piaac$age_piaac*5)
lit_mean_piaac$age_test<-lit_mean_piaac$age_piaac
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))
lit_mean_piaac$cohort<-"NA"

IALS_ALL_PIAAC_period<-rbind(lit_mean_ials, lit_mean_all, lit_mean_piaac)
isono<-read.csv("OECD-average/isono.csv")
IALS_ALL_PIAAC_period<-merge(IALS_ALL_PIAAC_period, isono, by=c("iso"))
IALS_ALL_PIAAC_period$age_piaac<-as.numeric(as.character(IALS_ALL_PIAAC_period$age_piaac))
write.csv(IALS_ALL_PIAAC_period, "Cohort Analysis/IALS_ALL_PIAAC_age_educ_period.csv", row.names=FALSE)
#----------------------------------------------------------------------------------------------------------------------


#draw graphs by education
IALS_ALL_PIAAC_cohort<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_cohort_age-educ.csv")
IALS_ALL_PIAAC_cohort$test<-factor(IALS_ALL_PIAAC_cohort$test, levels=c("IALS", "ALL", "PIAAC"))
IALS_ALL_PIAAC_cohort$educ<-factor(IALS_ALL_PIAAC_cohort$educ, levels=c(1,2,3), label=c("Lower secondary or less", "Upper secondary", "Post-secondary"))
IALS_ALL_PIAAC_period<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_age_educ_period.csv")
IALS_ALL_PIAAC_period$test<-factor(IALS_ALL_PIAAC_period$test, levels=c("IALS", "ALL", "PIAAC"))
IALS_ALL_PIAAC_period$educ<-factor(IALS_ALL_PIAAC_period$educ, levels=c(1,2,3), labels=c("Lower secondary or less", "Upper secondary", "Post-secondary"))


library(ggplot2)
library(ggforce)
library(gridExtra)


pdf("Cohort Analysis/cohort_period_educ.pdf", paper="a4r", width = 11, height = 8)
for(i in 1:15){
c<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country+educ, nrow=3, ncol=1, page=i) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80)) 
p<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country+educ, nrow=3, ncol=1, page=i) + xlab("Age")
g<-grid.arrange(arrangeGrob (c, p, ncol=2))
print(g)
}
dev.off()

Germany_cohort<-subset(IALS_ALL_PIAAC_cohort, country=="Germany")
c_Germany<-ggplot(Germany_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~educ, nrow=3, ncol=1, page=1) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80)) 
Germany_period<-subset(IALS_ALL_PIAAC_period, country=="Germany")
p_Germany<-ggplot(Germany_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~educ, nrow=3, ncol=1, page=1) + xlab("Age")
g_Germany<-grid.arrange(arrangeGrob (c_Germany, p_Germany, ncol=2))
ggsave("CohortAnalysis/example_Germany.jpg", g_Germany)

#----------------------------------------------------------------------------------------------------------------------
#period adjustment factor by educ
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
piaac_mean_country<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "educ"), data=piaac)
piaac_mean_country<-plyr::rename(piaac_mean_country, c("Mean"="mean_piaac"))

piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
ials_mean_country<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "educ"), data=ials, config=piaac_conf)
ials_mean_country<-plyr::rename(ials_mean_country, c("Mean"="mean_ials"))
period_adjustment<-merge(piaac_mean_country, ials_mean_country, by=c("CNTRYID", "educ"), all=TRUE)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"
all_mean_country<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "educ"), data=all, config=piaac_conf)
all_mean_country<-plyr::rename(all_mean_country, c("Mean"="mean_all"))

period_adjustment<-merge(period_adjustment, all_mean_country, by=c("CNTRYID", "educ"), all=TRUE)
period_adjustment<-plyr::rename(period_adjustment, c("CNTRYID"="iso"))
isono<-read.csv("OECD-average/isono.csv")
period_adjustment<-merge(period_adjustment, isono, by=c("iso"))
period_adjustment<-period_adjustment[c("iso", "country", "educ", "mean_ials", "mean_all", "mean_piaac")]

period_adjustment$piaac_adj<-period_adjustment$mean_ials/period_adjustment$mean_piaac
period_adjustment<-dplyr::mutate(period_adjustment, all_adj=ifelse (iso==124 | iso==348 | iso==380 | iso==528 | iso==554 | iso==578 | iso==840 | iso==756, (period_adjustment$mean_ials/period_adjustment$mean_all), 1))
period_adjustment<-dplyr::mutate(period_adjustment, piaac_adj=ifelse (iso==124 | iso==348 | iso==380 | iso==528 | iso==554 | iso==578 | iso==840, ((period_adjustment$mean_all*period_adjustment$all_adj)/period_adjustment$mean_piaac), piaac_adj))

IALS_ALL_PIAAC_cohort_age_educ<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_cohort_age-educ.csv")
IALS_ALL_PIAAC_cohort_age_educ<-merge(IALS_ALL_PIAAC_cohort_age_educ, period_adjustment, by=c("iso", "country", "educ"))
IALS_ALL_PIAAC_cohort_age_educ<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age_educ, adjusted_mean=ifelse (test=="PIAAC", Mean*piaac_adj, 0))
IALS_ALL_PIAAC_cohort_age_educ<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age_educ, adjusted_mean=ifelse (test=="ALL", Mean*all_adj, adjusted_mean))
IALS_ALL_PIAAC_cohort_age<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age_educ, adjusted_mean=ifelse (test=="IALS", Mean, adjusted_mean))

library(ggplot2)
library(ggforce)
ageing_pattern<-subset(IALS_ALL_PIAAC_cohort_age, cohort>2 & cohort<11)
ageing_pattern$cohort<-as.factor(as.numeric(ageing_pattern$cohort))
ageing_pattern$educ<-factor(ageing_pattern$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

pdf("Cohort Analysis/Ageing_pattern_educ.pdf", paper="a4r", width = 11, height = 7)
for(i in 1:15){
  print (ggplot(ageing_pattern, aes(x=year, y=adjusted_mean, color=cohort))  + geom_point() + geom_line() + xlab("Year") + ylab("Period-adjusted mean literacy score") +  scale_color_discrete(name="Age in PIAAC", labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60+")) + facet_wrap_paginate(~country+educ, ncol=2, nrow=1, page=i, scales="free_x"))
}
dev.off()



#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

#ONLY AGE PATTERN

#ials
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"

lit_mean_ials_1994_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1994_2011, config=piaac_conf)
lit_mean_ials_1994_2011$year<-1994
lit_mean_ials_1994_2011$test<-"IALS"
lit_mean_ials_1994_2011$age<-as.numeric(as.character(lit_mean_ials_1994_2011$age))
lit_mean_ials_1994_2011$age_piaac<-10+(lit_mean_ials_1994_2011$age*5)
lit_mean_ials_1994_2011$age_test<-lit_mean_ials_1994_2011$age_piaac-17

lit_mean_ials_1996_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1996_2011, config=piaac_conf)
lit_mean_ials_1996_2011$year<-1996
lit_mean_ials_1996_2011$test<-"IALS"
lit_mean_ials_1996_2011$age<-as.numeric(as.character(lit_mean_ials_1996_2011$age))
lit_mean_ials_1996_2011$age_piaac<-10+(lit_mean_ials_1996_2011$age*5)
lit_mean_ials_1996_2011$age_test<-lit_mean_ials_1996_2011$age_piaac-15

lit_mean_ials_1998_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1998_2011, config=piaac_conf)
lit_mean_ials_1998_2011$year<-1998
lit_mean_ials_1998_2011$test<-"IALS"
lit_mean_ials_1998_2011$age<-as.numeric(as.character(lit_mean_ials_1998_2011$age))
lit_mean_ials_1998_2011$age_piaac<-10+(lit_mean_ials_1998_2011$age*5)
lit_mean_ials_1998_2011$age_test<-lit_mean_ials_1998_2011$age_piaac-13

lit_mean_ials_1996_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1996_2014, config=piaac_conf)
lit_mean_ials_1996_2014$year<-1996
lit_mean_ials_1996_2014$test<-"IALS"
lit_mean_ials_1996_2014$age<-as.numeric(as.character(lit_mean_ials_1996_2014$age))
lit_mean_ials_1996_2014$age_piaac<-10+(lit_mean_ials_1996_2014$age*5)
lit_mean_ials_1996_2014$age_test<-lit_mean_ials_1996_2014$age_piaac-18

lit_mean_ials_1998_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1998_2014, config=piaac_conf)
lit_mean_ials_1998_2014$year<-1998
lit_mean_ials_1998_2014$test<-"IALS"
lit_mean_ials_1998_2014$age<-as.numeric(as.character(lit_mean_ials_1998_2014$age))
lit_mean_ials_1998_2014$age_piaac<-10+(lit_mean_ials_1998_2014$age*5)
lit_mean_ials_1998_2014$age_test<-lit_mean_ials_1998_2014$age_piaac-16

lit_mean_ials_1998_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=ials_1998_2017, config=piaac_conf)
lit_mean_ials_1998_2017$year<-1998
lit_mean_ials_1998_2017$test<-"IALS"
lit_mean_ials_1998_2017$age<-as.numeric(as.character(lit_mean_ials_1998_2017$age))
lit_mean_ials_1998_2017$age_piaac<-10+(lit_mean_ials_1998_2017$age*5)
lit_mean_ials_1998_2017$age_test<-lit_mean_ials_1998_2017$age_piaac-19

lit_mean_ials<-rbind(lit_mean_ials_1994_2011, lit_mean_ials_1996_2011, lit_mean_ials_1996_2014, lit_mean_ials_1998_2011, lit_mean_ials_1998_2014, lit_mean_ials_1998_2017)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso", "age"="cohort"))

#all
piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"

lit_mean_all_2003_2011<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=all_2003_2011, config=piaac_conf)
lit_mean_all_2003_2011$year<-2003
lit_mean_all_2003_2011$test<-"ALL"
lit_mean_all_2003_2011$age<-as.numeric(as.character(lit_mean_all_2003_2011$age))
lit_mean_all_2003_2011$age_piaac<-10+(lit_mean_all_2003_2011$age*5)
lit_mean_all_2003_2011$age_test<-lit_mean_all_2003_2011$age_piaac-8

lit_mean_all_2007_2014<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=all_2007_2014, config=piaac_conf)
lit_mean_all_2007_2014$year<-2007
lit_mean_all_2007_2014$test<-"ALL"
lit_mean_all_2007_2014$age<-as.numeric(as.character(lit_mean_all_2007_2014$age))
lit_mean_all_2007_2014$age_piaac<-10+(lit_mean_all_2007_2014$age*5)
lit_mean_all_2007_2014$age_test<-lit_mean_all_2007_2014$age_piaac-7

lit_mean_all_2007_2017<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age"), data=all_2007_2017, config=piaac_conf)
lit_mean_all_2007_2017$year<-2007
lit_mean_all_2007_2017$test<-"ALL"
lit_mean_all_2007_2017$age<-as.numeric(as.character(lit_mean_all_2007_2017$age))
lit_mean_all_2007_2017$age_piaac<-10+(lit_mean_all_2007_2017$age*5)
lit_mean_all_2007_2017$age_test<-lit_mean_all_2007_2017$age_piaac-10

lit_mean_all<-rbind(lit_mean_all_2003_2011, lit_mean_all_2007_2014, lit_mean_all_2007_2017)
lit_mean_all<-plyr::rename(lit_mean_all, c("CNTRYID"="iso", "age"="cohort"))

#piaac
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "CNTRYID"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528 | CNTRYID==826 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, 2011, 0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==152 | CNTRYID==705 | CNTRYID==554, 2014, year))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==348, 2017, year))
lit_mean_piaac<-subset(lit_mean_piaac, year>0)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age_piaac<-10+(lit_mean_piaac$age*5)
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528, (age_piaac-17),0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==826 | CNTRYID==56, (age_piaac-15),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, (age_piaac-13),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==554, (age_piaac-18),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==152 | CNTRYID==705, (age_piaac-16),age_test))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, age_test = ifelse(CNTRYID==348, (age_piaac-19),age_test))
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("age"="cohort", "CNTRYID"="iso"))

IALS_ALL_PIAAC_cohort<-rbind(lit_mean_ials, lit_mean_all, lit_mean_piaac)
isono<-read.csv("OECD-average/isono.csv")
IALS_ALL_PIAAC_cohort<-merge(IALS_ALL_PIAAC_cohort, isono, by=c("iso"))
write.csv(IALS_ALL_PIAAC_cohort, "Cohort Analysis/IALS_ALL_PIAAC_cohort_age.csv", row.names=FALSE)

#draw graphs
IALS_ALL_PIAAC_cohort<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_cohort_age.csv")
IALS_ALL_PIAAC_cohort$test<-factor(IALS_ALL_PIAAC_cohort$test, levels=c("IALS", "ALL", "PIAAC"))
library(ggplot2)
library(ggforce)
c1<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=1) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80))
c2<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=2) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80))
c3<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=3) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80))
c4<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=4) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80))
c5<-ggplot(IALS_ALL_PIAAC_cohort, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=5) + xlab("Age at PIAAC") + theme(legend.position = "none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#PERIOD EFFECT
#ials
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>19 & AGE<25, 20, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>24 & AGE<30, 25, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>29 & AGE<35, 30, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>34 & AGE<40, 35, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>39 & AGE<45, 40, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>44 & AGE<55, 45, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>49 & AGE<60, 50, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>54 & AGE<65, 55, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>59 & AGE<66, 60, age_test))
ials<-subset(ials, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test"), data=ials, config=piaac_conf)

lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528, 1994, 0))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==826 | CNTRYID==56 | CNTRYID==554, 1996, year))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380 | CNTRYID==152 | CNTRYID==705 | CNTRYID==348, 1998, year))
lit_mean_ials<-subset(lit_mean_ials, year>0)

lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))
lit_mean_ials$age_piaac<-lit_mean_ials$age_test
lit_mean_ials$cohort<-"NA"
lit_mean_ials$test<-"IALS"

#all
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>14 & AGE_RESP<20, 15, 0))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>19 & AGE_RESP<25, 20, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>24 & AGE_RESP<30, 25, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>29 & AGE_RESP<35, 30, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>34 & AGE_RESP<40, 35, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>39 & AGE_RESP<45, 40, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>44 & AGE_RESP<55, 45, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>49 & AGE_RESP<60, 50, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>54 & AGE_RESP<65, 55, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>59 & AGE_RESP<66, 60, age_test))
all<-subset(all, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"
lit_mean_all<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test"), data=all, config=piaac_conf)

lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==380 | CNTRYID==840 | CNTRYID==578, 2003, 0))
lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==528 | CNTRYID==554 | CNTRYID==348, 2007, year))
lit_mean_all<-subset(lit_mean_all, year>0)

lit_mean_all<-plyr::rename(lit_mean_all, c("CNTRYID"="iso"))
lit_mean_all$age_piaac<-lit_mean_all$age_test
lit_mean_all$cohort<-"NA"
lit_mean_all$test<-"ALL"


lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "CNTRYID"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528 | CNTRYID==826 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, 2011, 0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==152 | CNTRYID==705 | CNTRYID==554, 2014, year))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==348, 2017, year))
lit_mean_piaac<-subset(lit_mean_piaac, year>0)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age_piaac"))
lit_mean_piaac$age_piaac<-as.numeric(as.character(lit_mean_piaac$age_piaac))
lit_mean_piaac$age_piaac<-10+(lit_mean_piaac$age_piaac*5)
lit_mean_piaac$age_test<-lit_mean_piaac$age_piaac
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))
lit_mean_piaac$cohort<-"NA"

IALS_ALL_PIAAC_period<-rbind(lit_mean_ials, lit_mean_all, lit_mean_piaac)
IALS_ALL_PIAAC_period<-merge(IALS_ALL_PIAAC_period, isono, by=c("iso"))
IALS_ALL_PIAAC_period$age_piaac<-as.numeric(as.character(IALS_ALL_PIAAC_period$age_piaac))
write.csv(IALS_ALL_PIAAC_period, "Cohort Analysis/IALS_ALL_PIAAC_period.csv", row.names=FALSE)

IALS_ALL_PIAAC_period<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_period.csv")
IALS_ALL_PIAAC_period$test<-factor(IALS_ALL_PIAAC_period$test, levels = c("IALS", "ALL", "PIAAC"))
p1<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=1) + xlab("Age")
p2<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=2) + xlab("Age")
p3<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=3) + xlab("Age")
p4<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=4) + xlab("Age")
p5<-ggplot(IALS_ALL_PIAAC_period, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country, nrow=4, ncol=1, page=5) + xlab("Age")


library(gridExtra)
g<-grid.arrange(arrangeGrob (c1, p1, ncol=2))
ggsave("Cohort Analysis/page1.jpg", g, width=10, height=5.2)
g<-grid.arrange(arrangeGrob (c2, p2, ncol=2))
ggsave("Cohort Analysis/page2.jpg", g, width=10, height=5.2)
g<-grid.arrange(arrangeGrob (c3, p3, ncol=2))
ggsave("Cohort Analysis/page3.jpg", g, width=10, height=5.2)
g<-grid.arrange(arrangeGrob (c4, p4, ncol=2))
ggsave("Cohort Analysis/page4.jpg", g, width=10, height=5.2)
g<-grid.arrange(arrangeGrob (c5, p5, ncol=2))
ggsave("Cohort Analysis/page5.jpg", g, width=10, height=5.2)



#------------------------------------------------------------------------------------------------------------------------------
#Hungary (iso 348, IALS 1998, ALL 2007, PIAAC 2017)
#Italy (iso 380, IALS 1998, ALL 2003, PIAAC 2011)
#Norway (iso 578, IALS 1998, ALL 2003, PIAAC 2011)
#New Zealand (iso 554, IALS 1996, ALL 2007, PIAAC 2014)
#Netherlands (iso 528, IALS 1994, ALL 2007, PIAAC 2011 )


#NORWAY (EPC & PIAAC Conference)
rm(list = ls())
IALS_ALL_PIAAC_cohort_age<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_cohort_age.csv")
Norway_cohort<-subset(IALS_ALL_PIAAC_cohort_age, country=="Norway")
Norway_cohort<-subset(Norway, cohort>2 & cohort<11)
Norway$cohort<-as.factor(as.numeric(Norway$cohort))
Norway$U<-Norway$Mean+1.96*(Norway$SD/sqrt(Norway$Freq))
Norway$L<-Norway$Mean-1.96*(Norway$SD/sqrt(Norway$Freq))
ggplot(Norway, aes(x=year, y=Mean, color=cohort))  + geom_point() + geom_line() + ylim(250, 320) + xlab("Year") + ylab("Mean Literacy Score") +  scale_color_discrete(name="Age in PIAAC", labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60+")) + geom_errorbar(aes(ymax=U, ymin=L))
ggsave("Cohort Analysis/Norway.jpg", width=7.5, height=4)

Hungary<-subset(IALS_ALL_PIAAC_cohort_age, country=="Hungary")
Hungary<-subset(Hungary, cohort>2 & cohort<12)
Hungary$cohort<-as.factor(as.numeric(Hungary$cohort))
ggplot(Hungary, aes(x=year, y=Mean, color=cohort))  + geom_point() + geom_line() + ylim(240, 320) + xlab("Year") + ylab("Mean Literacy Score") + ggtitle("Cohort Perspective") 

Italy<-subset(IALS_ALL_PIAAC_cohort_age, country=="Italy")
Italy$cohort<-as.factor(as.numeric(Italy$cohort))
ggplot(Italy, aes(x=year, y=Mean, color=cohort))  + geom_point() + geom_line()  + xlab("Year") + ylab("Mean Literacy Score") + ggtitle("Cohort Perspective") 

NewZealand<-subset(IALS_ALL_PIAAC_cohort_age, country=="New Zealand")
NewZealand<-subset(NewZealand, cohort>2 & cohort<12)
NewZealand$cohort<-as.factor(as.numeric(NewZealand$cohort))
ggplot(NewZealand, aes(x=year, y=Mean, color=cohort))  + geom_point() + geom_line() + xlab("Year") + ylab("Mean Literacy Score") + ggtitle("Cohort Perspective") 

Netherlands<-subset(IALS_ALL_PIAAC_cohort_age, country=="Netherlands")
Netherlands<-subset(Netherlands, cohort>2 & cohort<12)
Netherlands$cohort<-as.factor(as.numeric(Netherlands$cohort))
ggplot(NewZealand, aes(x=year, y=Mean, color=cohort))  + geom_point() + geom_line() + xlab("Year") + ylab("Mean Literacy Score") + ggtitle("Cohort Perspective") 

#------------------------------------------------------------------------------------------------------------------------------


#period adjustment factor
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac_mean_country<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID"), data=piaac_all_countries)
piaac_mean_country<-plyr::rename(piaac_mean_country, c("Mean"="mean_piaac"))

piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
ials_mean_country<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID"), data=ials, config=piaac_conf)
ials_mean_country<-plyr::rename(ials_mean_country, c("Mean"="mean_ials"))
period_adjustment<-merge(piaac_mean_country, ials_mean_country, by=c("CNTRYID"), all=TRUE)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"
all_mean_country<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID"), data=all, config=piaac_conf)
all_mean_country<-plyr::rename(all_mean_country, c("Mean"="mean_all"))

period_adjustment<-merge(period_adjustment, all_mean_country, by=c("CNTRYID"), all=TRUE)
period_adjustment<-plyr::rename(period_adjustment, c("CNTRYID"="iso"))
isono<-read.csv("OECD-average/isono.csv")
period_adjustment<-merge(period_adjustment, isono, by=c("iso"))
period_adjustment<-period_adjustment[c("iso", "country", "mean_ials", "mean_all", "mean_piaac")]

period_adjustment$piaac_adj<-period_adjustment$mean_ials/period_adjustment$mean_piaac
period_adjustment<-dplyr::mutate(period_adjustment, all_adj=ifelse (iso==124 | iso==348 | iso==380 | iso==528 | iso==554 | iso==578 | iso==840 | iso==756, (period_adjustment$mean_ials/period_adjustment$mean_all), 1))
period_adjustment<-dplyr::mutate(period_adjustment, piaac_adj=ifelse (iso==124 | iso==348 | iso==380 | iso==528 | iso==554 | iso==578 | iso==840, ((period_adjustment$mean_all*period_adjustment$all_adj)/period_adjustment$mean_piaac), piaac_adj))

IALS_ALL_PIAAC_cohort_age<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_cohort_age.csv")
IALS_ALL_PIAAC_cohort_age<-merge(IALS_ALL_PIAAC_cohort_age, period_adjustment, by=c("iso", "country"))
IALS_ALL_PIAAC_cohort_age<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age, adjusted_mean=ifelse (test=="PIAAC", Mean*piaac_adj, 0))
IALS_ALL_PIAAC_cohort_age<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age, adjusted_mean=ifelse (test=="ALL", Mean*all_adj, adjusted_mean))
IALS_ALL_PIAAC_cohort_age<-dplyr::mutate(IALS_ALL_PIAAC_cohort_age, adjusted_mean=ifelse (test=="IALS", Mean, adjusted_mean))


ageing_pattern<-subset(IALS_ALL_PIAAC_cohort_age, cohort>2 & cohort<11)
ageing_pattern$cohort<-as.factor(as.numeric(ageing_pattern$cohort))

pdf("Cohort Analysis/Ageing_pattern.pdf", paper="a4r", width = 11, height = 8)
for(i in 1:15){
  print (ggplot(ageing_pattern, aes(x=year, y=adjusted_mean, color=cohort))  + geom_point() + geom_line() + xlab("Year") + ylab("Period-adjusted mean literacy score") +  scale_color_discrete(name="Age in PIAAC", labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60+")) + facet_wrap_paginate(~country, ncol=1, nrow=1, page=i, scales="free_y"))
}
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------
#only born in country
#period perspective

#ials
ials<-subset(ials, A1==1)
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>19 & AGE<25, 20, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>24 & AGE<30, 25, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>29 & AGE<35, 30, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>34 & AGE<40, 35, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>39 & AGE<45, 40, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>44 & AGE<55, 45, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>49 & AGE<60, 50, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>54 & AGE<65, 55, age_test))
ials<-dplyr::mutate(ials, age_test=ifelse (AGE>59 & AGE<66, 60, age_test))
ials<-subset(ials, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test"), data=ials, config=piaac_conf)

lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528, 1994, 0))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==826 | CNTRYID==56 | CNTRYID==554, 1996, year))
lit_mean_ials<-dplyr::mutate(lit_mean_ials, year=ifelse (CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380 | CNTRYID==152 | CNTRYID==705 | CNTRYID==348, 1998, year))
lit_mean_ials<-subset(lit_mean_ials, year>0)

lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))
lit_mean_ials$age_piaac<-lit_mean_ials$age_test
lit_mean_ials$cohort<-"NA"
lit_mean_ials$test<-"IALS"

#all
all<-subset(all, A1==1)
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>14 & AGE_RESP<20, 15, 0))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>19 & AGE_RESP<25, 20, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>24 & AGE_RESP<30, 25, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>29 & AGE_RESP<35, 30, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>34 & AGE_RESP<40, 35, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>39 & AGE_RESP<45, 40, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>44 & AGE_RESP<55, 45, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>49 & AGE_RESP<60, 50, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>54 & AGE_RESP<65, 55, age_test))
all<-dplyr::mutate(all, age_test=ifelse (AGE_RESP>59 & AGE_RESP<66, 60, age_test))
all<-subset(all, age_test>0)

piaac_conf[["variables"]][["weightFinal"]]<-"POPWT"
lit_mean_all<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age_test"), data=all, config=piaac_conf)

lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==380 | CNTRYID==840 | CNTRYID==578, 2003, 0))
lit_mean_all<-dplyr::mutate(lit_mean_all, year=ifelse (CNTRYID==528 | CNTRYID==554 | CNTRYID==348, 2007, year))
lit_mean_all<-subset(lit_mean_all, year>0)

lit_mean_all<-plyr::rename(lit_mean_all, c("CNTRYID"="iso"))
lit_mean_all$age_piaac<-lit_mean_all$age_test
lit_mean_all$cohort<-"NA"
lit_mean_all$test<-"ALL"

piaac<-subset(piaac, J_Q04a_T==1)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "CNTRYID"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==840 | CNTRYID==528 | CNTRYID==826 | CNTRYID==56 | CNTRYID==578 | CNTRYID==246 | CNTRYID==203 | CNTRYID==208 | CNTRYID==380, 2011, 0))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==152 | CNTRYID==705 | CNTRYID==554, 2014, year))
lit_mean_piaac<-dplyr::mutate(lit_mean_piaac, year = ifelse (CNTRYID==348, 2017, year))
lit_mean_piaac<-subset(lit_mean_piaac, year>0)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age_piaac"))
lit_mean_piaac$age_piaac<-as.numeric(as.character(lit_mean_piaac$age_piaac))
lit_mean_piaac$age_piaac<-10+(lit_mean_piaac$age_piaac*5)
lit_mean_piaac$age_test<-lit_mean_piaac$age_piaac
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))
lit_mean_piaac$cohort<-"NA"

IALS_ALL_PIAAC_period<-rbind(lit_mean_ials, lit_mean_all, lit_mean_piaac)
IALS_ALL_PIAAC_period<-merge(IALS_ALL_PIAAC_period, isono, by=c("iso"))
IALS_ALL_PIAAC_period$age_piaac<-as.numeric(as.character(IALS_ALL_PIAAC_period$age_piaac))
IALS_ALL_PIAAC_period$migration_status="excluding foreign-born"
write.csv(IALS_ALL_PIAAC_period, "Cohort Analysis/IALS_ALL_PIAAC_period_native.csv", row.names=FALSE)

#-------------------------------------------------------------------------------------------------------
#ALL-NATIVE-COMPARISON
IALS_ALL_PIAAC_period<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_period.csv")
IALS_ALL_PIAAC_period$migration_status="all"
IALS_ALL_PIAAC_period_natives<-read.csv("Cohort Analysis/IALS_ALL_PIAAC_period_native.csv")
all_native_comparison<-rbind(IALS_ALL_PIAAC_period, IALS_ALL_PIAAC_period_natives)
all_native_comparison$test<-factor(all_native_comparison$test, levels = c("IALS", "ALL", "PIAAC"))

library(ggplot2)
library(ggforce)
pdf("Cohort Analysis/period_migration.pdf", paper="a4r", width = 11, height = 8)
for(i in 1:17){
print(ggplot(all_native_comparison, aes(x=age_piaac, y=Mean, colour=test)) + geom_point() + geom_line() + facet_wrap_paginate(~country+migration_status, nrow=1, ncol=2, page=i) + xlab("Age"))
  }
dev.off()

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
rm(list=ls())
library(foreign)
#MERGING ALL COUNTRIES
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

#randomly splitting age groups for Canada
canada<-subset(ials, CNTRYID==124)
canada1625<-subset(canada, AGEINT==1)
canada2635<-subset(canada, AGEINT==2)
canada3645<-subset(canada, AGEINT==3)
canada4655<-subset(canada, AGEINT==4)
canada5665<-subset(canada, AGEINT==5)

canada1625$id<-c(1:725)
canada2635$id<-c(1:644)
canada3645$id<-c(1:655)
canada4655$id<-c(1:503)
canada5665$id<-c(1:469)

canada16<-sample(canada1625$id, 362, replace=FALSE)
canada21<-canada1625$id[-canada16]
canada16 <- subset(canada1625, id %in% canada16)
canada16$age<-5
canada21 <- subset(canada1625, id %in% canada21)
canada21$age<-6

canada26<-sample(canada2635$id, 322, replace=FALSE)
canada31<-canada2635$id[-canada26]
canada26 <- subset(canada2635, id %in% canada26) 
canada26$age<-7
canada31 <- subset(canada2635, id %in% canada31)
canada31$age<-8

canada36<-sample(canada3645$id, 328, replace=FALSE)
canada41<-canada3645$id[-canada36]
canada36 <- subset(canada3645, id %in% canada36) 
canada36$age<-9
canada41 <- subset(canada3645, id %in% canada41)
canada41$age<-10

canada46<-sample(canada4655$id, 252, replace=FALSE)
canada51<-canada4655$id[-canada46]
canada46 <- subset(canada4655, id %in% canada46) 
canada46$age<-11
canada51 <- subset(canada4655, id %in% canada51)
canada51$age<-12

canada56<-sample(canada5665$id, 235, replace=FALSE)
canada61<-canada5665$id[-canada56]
canada56 <- subset(canada5665, id %in% canada56) 
canada56$age<-13
canada61 <- subset(canada5665, id %in% canada61)
canada61$age<-14

canada<-rbind(canada16, canada21, canada26, canada31, canada36, canada41, canada46, canada51, canada56, canada61)
canada<-canada[,-492]
canada$year<-1994
canada$age_piaac<-(canada$age*5)+10
canada$age_test<-(canada$age_piaac-18)

#randomly splitting age groups for UK
uk<-subset(ials, CNTRYID==826)
uk1625<-subset(uk, AGEINT==1)
uk2635<-subset(uk, AGEINT==2)
uk3645<-subset(uk, AGEINT==3)
uk4655<-subset(uk, AGEINT==4)
uk5665<-subset(uk, AGEINT==5)

uk1625$id<-c(1:1044)
uk2635$id<-c(1:1794)
uk3645$id<-c(1:1552)
uk4655$id<-c(1:1203)
uk5665$id<-c(1:1125)

uk16<-sample(uk1625$id, 522, replace=FALSE)
uk21<-uk1625$id[-uk16]
uk16 <- subset(uk1625, id %in% uk16)
uk16$age<-4
uk21 <- subset(uk1625, id %in% uk21)
uk21$age<-5

uk26<-sample(uk2635$id, 897, replace=FALSE)
uk31<-uk2635$id[-uk26]
uk26 <- subset(uk2635, id %in% uk26) 
uk26$age<-6
uk31 <- subset(uk2635, id %in% uk31)
uk31$age<-7

uk36<-sample(uk3645$id, 776, replace=FALSE)
uk41<-uk3645$id[-uk36]
uk36 <- subset(uk3645, id %in% uk36) 
uk36$age<-8
uk41 <- subset(uk3645, id %in% uk41)
uk41$age<-9

uk46<-sample(uk4655$id, 602, replace=FALSE)
uk51<-uk4655$id[-uk46]
uk46 <- subset(uk4655, id %in% uk46) 
uk46$age<-10
uk51 <- subset(uk4655, id %in% uk51)
uk51$age<-11

uk56<-sample(uk5665$id, 563, replace=FALSE)
uk61<-uk5665$id[-uk56]
uk56 <- subset(uk5665, id %in% uk56) 
uk56$age<-12
uk61 <- subset(uk5665, id %in% uk61)
uk61$age<-13

uk<-rbind(uk16, uk21, uk26, uk31, uk36, uk41, uk46, uk51, uk56, uk61)
uk<-uk[,-492]
uk$year<-1996
uk$age_piaac<-(uk$age*5)+10
uk$age_test<-(uk$age_piaac-15)


#randomly splitting age groups for USA
usa<-subset(ials, CNTRYID==840)
usa1625<-subset(usa, AGEINT==1)
usa2635<-subset(usa, AGEINT==2)
usa3645<-subset(usa, AGEINT==3)
usa4655<-subset(usa, AGEINT==4)
usa5665<-subset(usa, AGEINT==5)

usa1625$id<-c(1:560)
usa2635$id<-c(1:671)
usa3645$id<-c(1:706)
usa4655$id<-c(1:569)
usa5665$id<-c(1:476)

usa16<-sample(usa1625$id, 280, replace=FALSE)
usa21<-usa1625$id[-usa16]
usa16 <- subset(usa1625, id %in% usa16)
usa16$age<-5
usa21 <- subset(usa1625, id %in% usa21)
usa21$age<-6

usa26<-sample(usa2635$id, 335, replace=FALSE)
usa31<-usa2635$id[-usa26]
usa26 <- subset(usa2635, id %in% usa26) 
usa26$age<-7
usa31 <- subset(usa2635, id %in% usa31)
usa31$age<-8

usa36<-sample(usa3645$id, 353, replace=FALSE)
usa41<-usa3645$id[-usa36]
usa36 <- subset(usa3645, id %in% usa36) 
usa36$age<-9
usa41 <- subset(usa3645, id %in% usa41)
usa41$age<-10

usa46<-sample(usa4655$id, 285, replace=FALSE)
usa51<-usa4655$id[-usa46]
usa46 <- subset(usa4655, id %in% usa46) 
usa46$age<-11
usa51 <- subset(usa4655, id %in% usa51)
usa51$age<-12

usa56<-sample(usa5665$id, 238, replace=FALSE)
usa61<-usa5665$id[-usa56]
usa56 <- subset(usa5665, id %in% usa56) 
usa56$age<-13
usa61 <- subset(usa5665, id %in% usa61)
usa61$age<-14

usa<-rbind(usa16, usa21, usa26, usa31, usa36, usa41, usa46, usa51, usa56, usa61)
usa<-usa[,-492]
usa$year<-1994
usa$age_piaac<-(usa$age*5)+10
usa$age_test<-(usa$age_piaac-17)

#recode age corresponding to PIAAC

#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, USA, Netherlands) - 17 years
ials_1994_2011<-subset(ials, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==528)
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
ials_1994_2011$year<-1994
ials_1994_2011$age_piaac<-10+(ials_1994_2011$age*5)
ials_1994_2011$age_test<-ials_1994_2011$age_piaac-17

#countries with IALS in 1996 and PIAAC in 2011 (UK, Belgium) - 15 years
ials_1996_2011<-subset(ials, CNTRYID==56)
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
ials_1996_2011$year<-1996
ials_1996_2011$age_piaac<-10+(ials_1996_2011$age*5)
ials_1996_2011$age_test<-ials_1996_2011$age_piaac-15

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
ials_1998_2011$year<-1998
ials_1998_2011$age_piaac<-10+(ials_1998_2011$age*5)
ials_1998_2011$age_test<-ials_1998_2011$age_piaac-13

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
ials_1996_2014$year<-1996
ials_1996_2014$age_piaac<-10+(ials_1996_2014$age*5)
ials_1996_2014$age_test<-ials_1996_2014$age_piaac-18

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
ials_1998_2014$year<-1998
ials_1998_2014$age_piaac<-10+(ials_1998_2014$age*5)
ials_1998_2014$age_test<-ials_1998_2014$age_piaac-16

#countries with IALS in 1998 and PIAAC in 2017 (Hungary) - 19 years
ials_1998_2017<-subset(ials, CNTRYID==348)
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>15 & AGE<21, 5, 0))
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
ials_1998_2017$year<-1998
ials_1998_2017$age_piaac<-10+(ials_1998_2017$age*5)
ials_1998_2017$age_test<-ials_1998_2017$age_piaac-19

ials_rev<-rbind(ials_1994_2011, ials_1996_2011, ials_1996_2014, ials_1998_2011, ials_1998_2014, ials_1998_2017, canada, uk, usa)
ials_rev$test<-"IALS"
regions<-read.csv("Cohort Analysis/iso-region.csv")
regions<-plyr::rename(regions, c("iso"="CNTRYID"))
ials_rev<-merge(ials_rev, regions, by=c("CNTRYID"))

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

lit_mean_ials_rev<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("age", "test", "educ"), data=ials_rev, config=piaac_conf)

#-----------------------------------------------------------------------------------------------------
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-subset(piaac_all_countries, CNTRYID==208 | CNTRYID==246 |CNTRYID==372 |CNTRYID==578 |CNTRYID==752 |CNTRYID==826 |CNTRYID==203 |CNTRYID==348 |CNTRYID==616 |CNTRYID==56 | CNTRYID==276 |CNTRYID==528 |CNTRYID==380 |CNTRYID==705 |CNTRYID==124 |CNTRYID==840 | CNTRYID==152 |CNTRYID==554 )

piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

#lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"

IALS_PIAAC<-rbind(lit_mean_ials_rev, lit_mean_piaac)
write.csv(IALS_PIAAC, "Cohort Analysis/IALS_PIAAC_world-pattern-unweighted.csv", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------
#period perspective
#ials
ials<-dplyr::mutate(ials, age=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age=ifelse (AGE>19 & AGE<25, 20, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>24 & AGE<30, 25, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>29 & AGE<35, 30, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>34 & AGE<40, 35, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>39 & AGE<45, 40, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>44 & AGE<55, 45, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>49 & AGE<60, 50, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>54 & AGE<65, 55, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>59 & AGE<66, 60, age))
ials<-subset(ials, age>0)

canada<-dplyr::mutate(canada, age=ifelse (age==5, 15, age))
canada<-dplyr::mutate(canada, age=ifelse (age==6, 20, age))
canada<-dplyr::mutate(canada, age=ifelse (age==7, 25, age))
canada<-dplyr::mutate(canada, age=ifelse (age==8, 30, age))
canada<-dplyr::mutate(canada, age=ifelse (age==9, 35, age))
canada<-dplyr::mutate(canada, age=ifelse (age==10, 40, age))
canada<-dplyr::mutate(canada, age=ifelse (age==11, 45, age))
canada<-dplyr::mutate(canada, age=ifelse (age==12, 50, age))
canada<-dplyr::mutate(canada, age=ifelse (age==13, 55, age))
canada<-dplyr::mutate(canada, age=ifelse (age==14, 60, age))
canada<-canada[-c(493, 494, 495)]

uk<-dplyr::mutate(uk, age=ifelse (age==4, 15, age))
uk<-dplyr::mutate(uk, age=ifelse (age==5, 20, age))
uk<-dplyr::mutate(uk, age=ifelse (age==6, 25, age))
uk<-dplyr::mutate(uk, age=ifelse (age==7, 30, age))
uk<-dplyr::mutate(uk, age=ifelse (age==8, 35, age))
uk<-dplyr::mutate(uk, age=ifelse (age==9, 40, age))
uk<-dplyr::mutate(uk, age=ifelse (age==10, 45, age))
uk<-dplyr::mutate(uk, age=ifelse (age==11, 50, age))
uk<-dplyr::mutate(uk, age=ifelse (age==12, 55, age))
uk<-dplyr::mutate(uk, age=ifelse (age==13, 60, age))
uk<-uk[-c(493, 494, 495)]

usa<-dplyr::mutate(usa, age=ifelse (age==5, 15, age))
usa<-dplyr::mutate(usa, age=ifelse (age==6, 20, age))
usa<-dplyr::mutate(usa, age=ifelse (age==7, 25, age))
usa<-dplyr::mutate(usa, age=ifelse (age==8, 30, age))
usa<-dplyr::mutate(usa, age=ifelse (age==9, 35, age))
usa<-dplyr::mutate(usa, age=ifelse (age==10, 40, age))
usa<-dplyr::mutate(usa, age=ifelse (age==11, 45, age))
usa<-dplyr::mutate(usa, age=ifelse (age==12, 50, age))
usa<-dplyr::mutate(usa, age=ifelse (age==13, 55, age))
usa<-dplyr::mutate(usa, age=ifelse (age==14, 60, age))
usa<-usa[-c(493, 494, 495)]

ials<-rbind(ials, canada, uk, usa)

piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("age", "educ"), data=ials, config=piaac_conf)
lit_mean_ials$test<-"IALS"

lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10

IALS_PIAAC_period<-rbind(lit_mean_ials, lit_mean_piaac)
write.csv(IALS_PIAAC_period, "Cohort Analysis/IALS_PIAAC_world-pattern_unweighted_period.csv", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------
world_unweighted<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-unweighted.csv")
world_unweighted$age<-(world_unweighted$age*5)+10
world_unweighted<-subset(world_unweighted, test=="PIAAC" | test=="IALS" & age<80 & age>30)
world_unweighted$educ<-factor(world_unweighted$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
library(ggforce)
#cohort perspective
cohort<-ggplot(world_unweighted, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap(~educ, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))

world_unweighted_period<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern_unweighted_period.csv")
world_unweighted_period$educ<-factor(world_unweighted_period$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))
period<-ggplot(world_unweighted_period, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap(~educ, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
#geom_errorbar(aes(ymin=Mean-(1.96*(SD/sqrt(Freq))), ymax=Mean+(1.96*(SD/sqrt(Freq)))))

library(gridExtra)
c<-grid.arrange(arrangeGrob(cohort, period, ncol=2, widths=c(0.9, 1.1)))
ggsave("Cohort Analysis/world_cohort_period_unweighted.png", c, width=8, height=3.75)

#---------------------------------------------------------------------------------------------------------------
#calculate period-adjustment factor
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-subset(ials, CNTRYID!=759)    #remove Switzerland

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

library(intsvy)
library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by="educ", data=ials, config=piaac_conf)
lit_mean_ials$test<-"IALS"
lit_mean_ials<-plyr::rename(lit_mean_ials, c("Mean"="mean_ials"))

piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("Mean"="mean_piaac"))

period_adj<-merge(lit_mean_ials, lit_mean_piaac, by=c("educ"))
period_adj<-period_adj[c("educ", "mean_ials", "mean_piaac")]
period_adj$adj_factor<-period_adj$mean_piaac/period_adj$mean_ials

world_unweighted<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-unweighted.csv")
world_unweighted<-merge(world_unweighted, period_adj, by=c("educ"))
world_unweighted$adj_Mean<-ifelse(world_unweighted$test=="IALS", world_unweighted$Mean*world_unweighted$adj_factor, world_unweighted$Mean)
world_unweighted$age<-(world_unweighted$age*5)+10
world_unweighted<-subset(world_unweighted, age>30 & age<65)
world_unweighted$age<-as.factor(world_unweighted$age)
world_unweighted$educ<-factor(world_unweighted$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
ggplot(world_unweighted, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap(~educ, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern.jpg", width =8, height=4)
write.csv(world_unweighted, "Cohort Analysis/world_unweighted_ageing.csv", row.names=FALSE)
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

#WEIGHTED WORLD
rm(list=ls())

#MERGING ALL COUNTRIES
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

#countries with 10-year age groups
canada<-subset(ials, CNTRYID==124)
usa<-subset(ials, CNTRYID==840)
uk<-subset(ials, CNTRYID==826)
ials10<-rbind(canada, usa, uk)

piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

lit_mean_ials10<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "AGEINT", "educ"), data=ials10, config=piaac_conf)

#age Canada and USA
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse((CNTRYID==124 | CNTRYID==840) & AGEINT==1, 5, 0))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse((CNTRYID==124 | CNTRYID==840) & AGEINT==2, 7, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse((CNTRYID==124 | CNTRYID==840) & AGEINT==3, 9, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse((CNTRYID==124 | CNTRYID==840) & AGEINT==4, 11, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse((CNTRYID==124 | CNTRYID==840) & AGEINT==5, 13, age))

#age UK
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse(CNTRYID==826 & AGEINT==1, 4, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse(CNTRYID==826 & AGEINT==2, 6, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse(CNTRYID==826 & AGEINT==3, 8, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse(CNTRYID==826 & AGEINT==4, 10, age))
lit_mean_ials10<-dplyr::mutate(lit_mean_ials10, age=ifelse(CNTRYID==826 & AGEINT==5, 12, age))

lit_mean_ials10<-subset(lit_mean_ials10, age>0)

lit_mean_ials10_2<-lit_mean_ials10
lit_mean_ials10_2$age<-lit_mean_ials10_2$age+1
lit_mean_ials10<-rbind(lit_mean_ials10_2, lit_mean_ials10)


#recode age corresponding to PIAAC
#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, Netherlands) - 17 years
ials_1994_2011<-subset(ials, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==528)
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
ials_1996_2011<-subset(ials, CNTRYID==56)
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
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>15 & AGE<21, 5, 0))
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

lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials_rev, config=piaac_conf)

lit_mean_ials10<-lit_mean_ials10[,-2]
lit_mean_ials<-rbind(lit_mean_ials10, lit_mean_ials)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))

wic_pop<-read.csv("Cohort Analysis/wic_pop16-64.csv")
pop1995<-subset(wic_pop, year==1995)
pop1995<-aggregate(pop~iso+educ, data=pop1995, FUN=sum)

lit_mean_ials<-merge(lit_mean_ials, pop1995, by=c("iso", "educ"))
lit_mean_ials_weighted<-lit_mean_ials %>% 
  dplyr::group_by(age, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_ials_weighted$weighted_mean<-lit_mean_ials_weighted$Mean*lit_mean_ials_weighted$share
lit_mean_ials_weighted<-aggregate(weighted_mean~age+educ, FUN=sum, data=lit_mean_ials_weighted)
lit_mean_ials_weighted$test<-"IALS"
lit_mean_ials_weighted$age<-as.numeric(as.character(lit_mean_ials_weighted$age))
lit_mean_ials_weighted$age<-(lit_mean_ials_weighted$age*5)+10

#PIAAC
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-subset(piaac_all_countries, CNTRYID==208 | CNTRYID==246 |CNTRYID==372 |CNTRYID==578 |CNTRYID==752 |CNTRYID==826 |CNTRYID==203 |CNTRYID==348 |CNTRYID==616 |CNTRYID==56 | CNTRYID==276 |CNTRYID==528 |CNTRYID==380 |CNTRYID==705 |CNTRYID==124 |CNTRYID==840 | CNTRYID==152 |CNTRYID==554 )

piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "AGEG5LFS", "educ"), data=piaac)

lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))

pop2015<-subset(wic_pop, year==2015)
pop2015<-aggregate(pop~iso+educ, data=pop2015, FUN=sum)

lit_mean_piaac<-merge(lit_mean_piaac, pop2015, by=c("iso", "educ"))
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))
lit_mean_piaac_weighted<-lit_mean_piaac %>% 
  dplyr::group_by(age, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_piaac_weighted$weighted_mean<-lit_mean_piaac_weighted$Mean*lit_mean_piaac_weighted$share
lit_mean_piaac_weighted<-aggregate(weighted_mean~age+educ, FUN=sum, data=lit_mean_piaac_weighted)
lit_mean_piaac_weighted$age<-as.numeric(as.factor(lit_mean_piaac_weighted$age))
lit_mean_piaac_weighted$age<-(lit_mean_piaac_weighted$age*5)+10
lit_mean_piaac_weighted$test<-"PIAAC"

IALS_PIAAC_weighted<-rbind(lit_mean_ials_weighted, lit_mean_piaac_weighted)
write.csv(IALS_PIAAC_weighted, "Cohort Analysis/IALS_PIAAC_world-pattern-weighted.csv", row.names=FALSE)


#period perspective
#ials
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)
ials<-dplyr::mutate(ials, age=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age=ifelse (AGE>19 & AGE<25, 20, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>24 & AGE<30, 25, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>29 & AGE<35, 30, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>34 & AGE<40, 35, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>39 & AGE<45, 40, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>44 & AGE<55, 45, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>49 & AGE<60, 50, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>54 & AGE<65, 55, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>59 & AGE<66, 60, age))
ials<-subset(ials, age>0)

lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials, config=piaac_conf)
lit_mean_ials10<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "AGEINT", "educ"), data=ials10, config=piaac_conf)
lit_mean_ials10<-plyr::rename(lit_mean_ials10, c("AGEINT"="age"))
lit_mean_ials10$age<-as.numeric(as.factor(lit_mean_ials10$age))
lit_mean_ials10$age<-(lit_mean_ials10$age*10)+5
lit_mean_ials10<-subset(lit_mean_ials10, age<65 & age!="NA")

lit_mean_ials10_2<-lit_mean_ials10
lit_mean_ials10_2$age<-lit_mean_ials10_2$age+5
lit_mean_ials<-rbind(lit_mean_ials, lit_mean_ials10, lit_mean_ials10_2)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))
lit_mean_ials<-merge(lit_mean_ials, pop1995, by=c("educ", "iso"))
lit_mean_ials_weighted<-lit_mean_ials %>% 
  dplyr::group_by(age, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_ials_weighted$weighted_mean<-lit_mean_ials_weighted$Mean*lit_mean_ials_weighted$share
lit_mean_ials_weighted<-aggregate(weighted_mean~age+educ, FUN=sum, data=lit_mean_ials_weighted)
lit_mean_ials_weighted$test<-"IALS"

piaac<-subset(piaac_all_countries, CNTRYID==208 | CNTRYID==246 |CNTRYID==372 |CNTRYID==578 |CNTRYID==752 |CNTRYID==826 |CNTRYID==203 |CNTRYID==348 |CNTRYID==616 |CNTRYID==56 | CNTRYID==276 |CNTRYID==528 |CNTRYID==380 |CNTRYID==705 |CNTRYID==124 |CNTRYID==840 | CNTRYID==152 |CNTRYID==554 )
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "AGEG5LFS", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10

lit_mean_piaac<-merge(lit_mean_piaac, pop2015, by=c("educ", "iso"))
lit_mean_piaac_weighted<-lit_mean_piaac %>% 
  dplyr::group_by(age, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_piaac_weighted$weighted_mean<-lit_mean_piaac_weighted$Mean*lit_mean_piaac_weighted$share
lit_mean_piaac_weighted<-aggregate(weighted_mean~age+educ, FUN=sum, data=lit_mean_piaac_weighted)
lit_mean_piaac_weighted$test<-"PIAAC"

IALS_PIAAC_period_weighted<-rbind(lit_mean_ials_weighted, lit_mean_piaac_weighted)
write.csv(IALS_PIAAC_period_weighted, "Cohort Analysis/IALS_PIAAC_world-pattern_weighted_period.csv", row.names=FALSE)


#_________________________________________________________________________
world_weighted<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-weighted.csv")
world_weighted<-subset(world_weighted, test=="PIAAC" | test=="IALS" & age<80 & age>30)
world_weighted$educ<-factor(world_weighted$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
library(ggforce)
#cohort perspective
cohort<-ggplot(world_weighted, aes(x=age, y=weighted_mean, color=test)) +  geom_point() + facet_wrap(~educ, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))

world_weighted_period<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern_weighted_period.csv")
world_weighted_period$educ<-factor(world_weighted_period$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))
period<-ggplot(world_weighted_period, aes(x=age, y=weighted_mean, color=test)) +  geom_point() + geom_line() + facet_wrap(~educ, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
#geom_errorbar(aes(ymin=Mean-(1.96*(SD/sqrt(Freq))), ymax=Mean+(1.96*(SD/sqrt(Freq)))))

library(gridExtra)
c<-grid.arrange(arrangeGrob(cohort, period, ncol=2, widths=c(0.9, 1.1)))
ggsave("Cohort Analysis/world_cohort_period_weighted.png", c, width=8, height=3.75)
#_________________________________________________________________________

#calculate period-adjustment factor
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-subset(ials, CNTRYID!=759)    #remove Switzerland

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "educ"), data=ials, config=piaac_conf)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("CNTRYID"="iso"))
lit_mean_ials<-merge(lit_mean_ials, pop1995, by=c("iso", "educ"))
lit_mean_ials_weighted<-lit_mean_ials %>% 
  dplyr::group_by(educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_ials_weighted$weighted_mean<-lit_mean_ials_weighted$Mean*lit_mean_ials_weighted$share
lit_mean_ials_weighted<-aggregate(weighted_mean~educ, FUN=sum, data=lit_mean_ials_weighted)
lit_mean_ials_weighted$test<-"IALS"
lit_mean_ials_weighted<-plyr::rename(lit_mean_ials_weighted, c("weighted_mean"="weighted_mean_ials"))

piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))
lit_mean_piaac<-merge(lit_mean_piaac, pop2015, by=c("educ", "iso"))

lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("Mean"="mean_piaac"))
lit_mean_piaac_weighted<-lit_mean_ials %>% 
  dplyr::group_by(educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_piaac_weighted$weighted_mean<-lit_mean_piaac_weighted$Mean*lit_mean_piaac_weighted$share
lit_mean_piaac_weighted<-aggregate(weighted_mean~educ, FUN=sum, data=lit_mean_piaac_weighted)
lit_mean_piaac_weighted$test<-"IALS"
lit_mean_piaac_weighted<-plyr::rename(lit_mean_piaac_weighted, c("weighted_mean"="weighted_mean_piaac"))

period_adj<-merge(lit_mean_ials_weighted, lit_mean_piaac_weighted, by=c("educ"))
period_adj<-period_adj[c("educ", "weighted_mean_ials", "weighted_mean_piaac")]
period_adj$adj_factor<-period_adj$weighted_mean_piaac/period_adj$weighted_mean_ials

world_weighted<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-weighted.csv")
world_weighted<-merge(world_weighted, period_adj, by=c("educ"))
world_weighted$adj_Mean<-ifelse(world_weighted$test=="IALS", world_weighted$weighted_mean*world_weighted$adj_factor, world_weighted$weighted_mean)
world_weighted<-subset(world_weighted, age>30 & age<65)
world_weighted$age<-as.factor(world_weighted$age)
world_weighted$educ<-factor(world_weighted$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
ggplot(world_weighted, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap(~educ, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-weighted_ageing-pattern.jpg", width =8, height=4)



#WORLD GRAPH - country availability
library("sf")
library("ggplot2")
world<-read.csv("Cohort Analysis/world.csv")
world<-subset(world, region!="Antarctica")
ggplot(world, aes(x=long, y=lat)) + geom_polygon(aes(group=group, fill=regional), colour="black")+ theme_void() + scale_fill_manual(values=c("#e4eaed", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628"))
ggsave("Cohort Analysis/world.jpg", width=8.5, height=3.3)

#__________________________________________________________________





#Unweighted - Regions

rm(list=ls())
library(foreign)
#MERGING ALL COUNTRIES
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

#randomly splitting age groups for Canada
canada<-subset(ials, CNTRYID==124)
canada1625<-subset(canada, AGEINT==1)
canada2635<-subset(canada, AGEINT==2)
canada3645<-subset(canada, AGEINT==3)
canada4655<-subset(canada, AGEINT==4)
canada5665<-subset(canada, AGEINT==5)

canada1625$id<-c(1:725)
canada2635$id<-c(1:644)
canada3645$id<-c(1:655)
canada4655$id<-c(1:503)
canada5665$id<-c(1:469)

canada16<-sample(canada1625$id, 362, replace=FALSE)
canada21<-canada1625$id[-canada16]
canada16 <- subset(canada1625, id %in% canada16)
canada16$age<-5
canada21 <- subset(canada1625, id %in% canada21)
canada21$age<-6

canada26<-sample(canada2635$id, 322, replace=FALSE)
canada31<-canada2635$id[-canada26]
canada26 <- subset(canada2635, id %in% canada26) 
canada26$age<-7
canada31 <- subset(canada2635, id %in% canada31)
canada31$age<-8

canada36<-sample(canada3645$id, 328, replace=FALSE)
canada41<-canada3645$id[-canada36]
canada36 <- subset(canada3645, id %in% canada36) 
canada36$age<-9
canada41 <- subset(canada3645, id %in% canada41)
canada41$age<-10

canada46<-sample(canada4655$id, 252, replace=FALSE)
canada51<-canada4655$id[-canada46]
canada46 <- subset(canada4655, id %in% canada46) 
canada46$age<-11
canada51 <- subset(canada4655, id %in% canada51)
canada51$age<-12

canada56<-sample(canada5665$id, 235, replace=FALSE)
canada61<-canada5665$id[-canada56]
canada56 <- subset(canada5665, id %in% canada56) 
canada56$age<-13
canada61 <- subset(canada5665, id %in% canada61)
canada61$age<-14

canada<-rbind(canada16, canada21, canada26, canada31, canada36, canada41, canada46, canada51, canada56, canada61)
canada<-canada[,-492]
canada$year<-1994
canada$age_piaac<-(canada$age*5)+10
canada$age_test<-(canada$age_piaac-18)

#randomly splitting age groups for UK
uk<-subset(ials, CNTRYID==826)
uk1625<-subset(uk, AGEINT==1)
uk2635<-subset(uk, AGEINT==2)
uk3645<-subset(uk, AGEINT==3)
uk4655<-subset(uk, AGEINT==4)
uk5665<-subset(uk, AGEINT==5)

uk1625$id<-c(1:1044)
uk2635$id<-c(1:1794)
uk3645$id<-c(1:1552)
uk4655$id<-c(1:1203)
uk5665$id<-c(1:1125)

uk16<-sample(uk1625$id, 522, replace=FALSE)
uk21<-uk1625$id[-uk16]
uk16 <- subset(uk1625, id %in% uk16)
uk16$age<-4
uk21 <- subset(uk1625, id %in% uk21)
uk21$age<-5

uk26<-sample(uk2635$id, 897, replace=FALSE)
uk31<-uk2635$id[-uk26]
uk26 <- subset(uk2635, id %in% uk26) 
uk26$age<-6
uk31 <- subset(uk2635, id %in% uk31)
uk31$age<-7

uk36<-sample(uk3645$id, 776, replace=FALSE)
uk41<-uk3645$id[-uk36]
uk36 <- subset(uk3645, id %in% uk36) 
uk36$age<-8
uk41 <- subset(uk3645, id %in% uk41)
uk41$age<-9

uk46<-sample(uk4655$id, 602, replace=FALSE)
uk51<-uk4655$id[-uk46]
uk46 <- subset(uk4655, id %in% uk46) 
uk46$age<-10
uk51 <- subset(uk4655, id %in% uk51)
uk51$age<-11

uk56<-sample(uk5665$id, 563, replace=FALSE)
uk61<-uk5665$id[-uk56]
uk56 <- subset(uk5665, id %in% uk56) 
uk56$age<-12
uk61 <- subset(uk5665, id %in% uk61)
uk61$age<-13

uk<-rbind(uk16, uk21, uk26, uk31, uk36, uk41, uk46, uk51, uk56, uk61)
uk<-uk[,-492]
uk$year<-1996
uk$age_piaac<-(uk$age*5)+10
uk$age_test<-(uk$age_piaac-15)


#randomly splitting age groups for USA
usa<-subset(ials, CNTRYID==840)
usa1625<-subset(usa, AGEINT==1)
usa2635<-subset(usa, AGEINT==2)
usa3645<-subset(usa, AGEINT==3)
usa4655<-subset(usa, AGEINT==4)
usa5665<-subset(usa, AGEINT==5)

usa1625$id<-c(1:560)
usa2635$id<-c(1:671)
usa3645$id<-c(1:706)
usa4655$id<-c(1:569)
usa5665$id<-c(1:476)

usa16<-sample(usa1625$id, 280, replace=FALSE)
usa21<-usa1625$id[-usa16]
usa16 <- subset(usa1625, id %in% usa16)
usa16$age<-5
usa21 <- subset(usa1625, id %in% usa21)
usa21$age<-6

usa26<-sample(usa2635$id, 335, replace=FALSE)
usa31<-usa2635$id[-usa26]
usa26 <- subset(usa2635, id %in% usa26) 
usa26$age<-7
usa31 <- subset(usa2635, id %in% usa31)
usa31$age<-8

usa36<-sample(usa3645$id, 353, replace=FALSE)
usa41<-usa3645$id[-usa36]
usa36 <- subset(usa3645, id %in% usa36) 
usa36$age<-9
usa41 <- subset(usa3645, id %in% usa41)
usa41$age<-10

usa46<-sample(usa4655$id, 285, replace=FALSE)
usa51<-usa4655$id[-usa46]
usa46 <- subset(usa4655, id %in% usa46) 
usa46$age<-11
usa51 <- subset(usa4655, id %in% usa51)
usa51$age<-12

usa56<-sample(usa5665$id, 238, replace=FALSE)
usa61<-usa5665$id[-usa56]
usa56 <- subset(usa5665, id %in% usa56) 
usa56$age<-13
usa61 <- subset(usa5665, id %in% usa61)
usa61$age<-14

usa<-rbind(usa16, usa21, usa26, usa31, usa36, usa41, usa46, usa51, usa56, usa61)
usa<-usa[,-492]
usa$year<-1994
usa$age_piaac<-(usa$age*5)+10
usa$age_test<-(usa$age_piaac-17)

#recode age corresponding to PIAAC

#countries with IALS in 1994 and PIAAC in 2011 (Germany, Ireland, Sweden, Poland, USA, Netherlands) - 17 years
ials_1994_2011<-subset(ials, CNTRYID==276 | CNTRYID==372 | CNTRYID==752 | CNTRYID==616 | CNTRYID==528)
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
ials_1994_2011$year<-1994
ials_1994_2011$age_piaac<-10+(ials_1994_2011$age*5)
ials_1994_2011$age_test<-ials_1994_2011$age_piaac-17

#countries with IALS in 1996 and PIAAC in 2011 (UK, Belgium) - 15 years
ials_1996_2011<-subset(ials, CNTRYID==56)
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
ials_1996_2011$year<-1996
ials_1996_2011$age_piaac<-10+(ials_1996_2011$age*5)
ials_1996_2011$age_test<-ials_1996_2011$age_piaac-15

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
ials_1998_2011$year<-1998
ials_1998_2011$age_piaac<-10+(ials_1998_2011$age*5)
ials_1998_2011$age_test<-ials_1998_2011$age_piaac-13

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
ials_1996_2014$year<-1996
ials_1996_2014$age_piaac<-10+(ials_1996_2014$age*5)
ials_1996_2014$age_test<-ials_1996_2014$age_piaac-18

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
ials_1998_2014$year<-1998
ials_1998_2014$age_piaac<-10+(ials_1998_2014$age*5)
ials_1998_2014$age_test<-ials_1998_2014$age_piaac-16

#countries with IALS in 1998 and PIAAC in 2017 (Hungary) - 19 years
ials_1998_2017<-subset(ials, CNTRYID==348)
ials_1998_2017<-dplyr::mutate(ials_1998_2017, age=ifelse (AGE>15 & AGE<21, 5, 0))
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
ials_1998_2017$year<-1998
ials_1998_2017$age_piaac<-10+(ials_1998_2017$age*5)
ials_1998_2017$age_test<-ials_1998_2017$age_piaac-19

ials_rev<-rbind(ials_1994_2011, ials_1996_2011, ials_1996_2014, ials_1998_2011, ials_1998_2014, ials_1998_2017, canada, uk, usa)
ials_rev$test<-"IALS"
regions<-read.csv("Cohort Analysis/iso-region.csv")
regions<-plyr::rename(regions, c("iso"="CNTRYID"))
ials_rev<-merge(ials_rev, regions, by=c("CNTRYID"))

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

lit_mean_ials_rev<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("region", "age", "test", "educ"), data=ials_rev, config=piaac_conf)

#-----------------------------------------------------------------------------------------------------
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-subset(piaac_all_countries, CNTRYID==208 | CNTRYID==246 |CNTRYID==372 |CNTRYID==578 |CNTRYID==752 |CNTRYID==826 |CNTRYID==203 |CNTRYID==348 |CNTRYID==616 |CNTRYID==56 | CNTRYID==276 |CNTRYID==528 |CNTRYID==380 |CNTRYID==705 |CNTRYID==124 |CNTRYID==840 | CNTRYID==152 |CNTRYID==554 )

piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

piaac<-merge(piaac, regions, by=c("CNTRYID"))

#lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("region", "AGEG5LFS", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))

IALS_PIAAC<-rbind(lit_mean_ials_rev, lit_mean_piaac)
write.csv(IALS_PIAAC, "Cohort Analysis/IALS_PIAAC_world-pattern-unweighted_by-region.csv", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------
#period perspective
#ials
ials<-dplyr::mutate(ials, age=ifelse (AGE>14 & AGE<20, 15, 0))
ials<-dplyr::mutate(ials, age=ifelse (AGE>19 & AGE<25, 20, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>24 & AGE<30, 25, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>29 & AGE<35, 30, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>34 & AGE<40, 35, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>39 & AGE<45, 40, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>44 & AGE<55, 45, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>49 & AGE<60, 50, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>54 & AGE<65, 55, age))
ials<-dplyr::mutate(ials, age=ifelse (AGE>59 & AGE<66, 60, age))
ials<-subset(ials, age>0)

canada<-dplyr::mutate(canada, age=ifelse (age==5, 15, age))
canada<-dplyr::mutate(canada, age=ifelse (age==6, 20, age))
canada<-dplyr::mutate(canada, age=ifelse (age==7, 25, age))
canada<-dplyr::mutate(canada, age=ifelse (age==8, 30, age))
canada<-dplyr::mutate(canada, age=ifelse (age==9, 35, age))
canada<-dplyr::mutate(canada, age=ifelse (age==10, 40, age))
canada<-dplyr::mutate(canada, age=ifelse (age==11, 45, age))
canada<-dplyr::mutate(canada, age=ifelse (age==12, 50, age))
canada<-dplyr::mutate(canada, age=ifelse (age==13, 55, age))
canada<-dplyr::mutate(canada, age=ifelse (age==14, 60, age))
canada<-canada[-c(493, 494, 495)]

uk<-dplyr::mutate(uk, age=ifelse (age==4, 15, age))
uk<-dplyr::mutate(uk, age=ifelse (age==5, 20, age))
uk<-dplyr::mutate(uk, age=ifelse (age==6, 25, age))
uk<-dplyr::mutate(uk, age=ifelse (age==7, 30, age))
uk<-dplyr::mutate(uk, age=ifelse (age==8, 35, age))
uk<-dplyr::mutate(uk, age=ifelse (age==9, 40, age))
uk<-dplyr::mutate(uk, age=ifelse (age==10, 45, age))
uk<-dplyr::mutate(uk, age=ifelse (age==11, 50, age))
uk<-dplyr::mutate(uk, age=ifelse (age==12, 55, age))
uk<-dplyr::mutate(uk, age=ifelse (age==13, 60, age))
uk<-uk[-c(493, 494, 495)]

usa<-dplyr::mutate(usa, age=ifelse (age==5, 15, age))
usa<-dplyr::mutate(usa, age=ifelse (age==6, 20, age))
usa<-dplyr::mutate(usa, age=ifelse (age==7, 25, age))
usa<-dplyr::mutate(usa, age=ifelse (age==8, 30, age))
usa<-dplyr::mutate(usa, age=ifelse (age==9, 35, age))
usa<-dplyr::mutate(usa, age=ifelse (age==10, 40, age))
usa<-dplyr::mutate(usa, age=ifelse (age==11, 45, age))
usa<-dplyr::mutate(usa, age=ifelse (age==12, 50, age))
usa<-dplyr::mutate(usa, age=ifelse (age==13, 55, age))
usa<-dplyr::mutate(usa, age=ifelse (age==14, 60, age))
usa<-usa[-c(493, 494, 495)]

ials<-rbind(ials, canada, uk, usa)
regions<-read.csv("Cohort Analysis/iso-region.csv")
regions<-plyr::rename(regions, c("iso"="CNTRYID"))
ials<-merge(ials, regions, by=c("CNTRYID"))

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("region", "age", "educ"), data=ials, config=piaac_conf)
lit_mean_ials$test<-"IALS"

piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-subset(piaac_all_countries, CNTRYID==208 | CNTRYID==246 |CNTRYID==372 |CNTRYID==578 |CNTRYID==752 |CNTRYID==826 |CNTRYID==203 |CNTRYID==348 |CNTRYID==616 |CNTRYID==56 | CNTRYID==276 |CNTRYID==528 |CNTRYID==380 |CNTRYID==705 |CNTRYID==124 |CNTRYID==840 | CNTRYID==152 |CNTRYID==554 )

piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

piaac<-merge(piaac, regions, by=c("CNTRYID"))

#lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("region", "AGEG5LFS", "educ"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.factor(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10

IALS_PIAAC_period<-rbind(lit_mean_ials, lit_mean_piaac)
write.csv(IALS_PIAAC_period, "Cohort Analysis/IALS_PIAAC_world-pattern_unweighted_period_by-region.csv", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------
world_unweighted_region<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-unweighted_by-region.csv")
world_unweighted_region$age<-(world_unweighted_region$age*5)+10
world_unweighted_region<-subset(world_unweighted_region, test=="PIAAC" | test=="IALS" & age<80 & age>30)
world_unweighted_region$educ<-factor(world_unweighted_region$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
library(ggforce)
#cohort perspective
cohort1<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=1, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort2<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=2, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort3<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=3, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort4<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=4, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort5<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=5, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort6<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=6, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))
cohort7<-ggplot(world_unweighted_region, aes(x=age, y=Mean, color=test)) +  geom_point() + facet_wrap_paginate(~region+educ, page=7, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age at PIAAC") + ylab("Literacy Mean") + theme(legend.position="none") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70))


world_unweighted_period_region<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern_unweighted_period_by-region.csv")
world_unweighted_period_region$educ<-factor(world_unweighted_period_region$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))
period1<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=1, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period2<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=2, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period3<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=3, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period4<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=4, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period5<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=5, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period6<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=6, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 
period7<-ggplot(world_unweighted_period_region, aes(x=age, y=Mean, color=test)) +  geom_point() + geom_line() + facet_wrap_paginate(~region+educ, page=7, ncol=1, nrow=2) + scale_color_discrete(name="Test", labels=c("IALS 1994-1998", "PIAAC 2011-2017")) + xlab("Age") + ylab("Literacy Mean") 

#geom_errorbar(aes(ymin=Mean-(1.96*(SD/sqrt(Freq))), ymax=Mean+(1.96*(SD/sqrt(Freq)))))

library(gridExtra)
c1<-grid.arrange(arrangeGrob(cohort1, period1, ncol=2, widths=c(0.9, 1.1)))
c2<-grid.arrange(arrangeGrob(cohort2, period2, ncol=2, widths=c(0.9, 1.1)))
c3<-grid.arrange(arrangeGrob(cohort3, period3, ncol=2, widths=c(0.9, 1.1)))
c4<-grid.arrange(arrangeGrob(cohort4, period4, ncol=2, widths=c(0.9, 1.1)))
c5<-grid.arrange(arrangeGrob(cohort5, period5, ncol=2, widths=c(0.9, 1.1)))
c6<-grid.arrange(arrangeGrob(cohort6, period6, ncol=2, widths=c(0.9, 1.1)))
c7<-grid.arrange(arrangeGrob(cohort7, period7, ncol=2, widths=c(0.9, 1.1)))
ggsave("Cohort Analysis/world_cohort_period_unweighted_region1.png", c1, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region2.png", c2, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region3.png", c3, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region4.png", c4, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region5.png", c5, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region6.png", c6, width=8.3, height=4.1)
ggsave("Cohort Analysis/world_cohort_period_unweighted_region7.png", c7, width=8.3, height=4.1)
#---------------------------------------------------------------------------------------------------------------
#calculate period-adjustment factor
#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-subset(ials, CNTRYID!=759)    #remove Switzerland

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)
ials<-merge(ials, regions, by=c("CNTRYID"))

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30
lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("educ", "region"), data=ials, config=piaac_conf)
lit_mean_ials$test<-"IALS"
lit_mean_ials<-plyr::rename(lit_mean_ials, c("Mean"="mean_ials"))

piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
piaac<-merge(piaac, regions, by=c("CNTRYID"))
#piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("educ", "region"), data=piaac)
lit_mean_piaac$test<-"PIAAC"
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("Mean"="mean_piaac"))

period_adj<-merge(lit_mean_ials, lit_mean_piaac, by=c("educ", "region"))
period_adj<-period_adj[c("region", "educ", "mean_ials", "mean_piaac")]
period_adj$adj_factor<-period_adj$mean_piaac/period_adj$mean_ials

world_unweighted_region<-read.csv("Cohort Analysis/IALS_PIAAC_world-pattern-unweighted_by-region.csv")
world_unweighted_region<-merge(world_unweighted_region, period_adj, by=c("region", "educ"))
world_unweighted_region$adj_Mean<-ifelse(world_unweighted_region$test=="IALS", world_unweighted_region$Mean*world_unweighted_region$adj_factor, world_unweighted_region$Mean)
world_unweighted_region$age<-(world_unweighted_region$age*5)+10
world_unweighted_region<-subset(world_unweighted_region, age>30 & age<65)
world_unweighted_region$age<-as.factor(world_unweighted_region$age)
world_unweighted_region$educ<-factor(world_unweighted_region$educ, labels=c("Lower secondary or less", "Upper secondary or higher"))

library(ggplot2)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=1, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region1.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=2, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region2.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=3, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region3.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=4, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region4.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=5, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region5.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=6, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region6.jpg", width =8, height=4)
ggplot(world_unweighted_region, aes(x=test, y=adj_Mean, color=age, group=age)) + geom_line() + geom_point() + facet_wrap_paginate(~region+educ, page=7, ncol=2, nrow=1, scales="free_x") + xlab("Test") + ylab("Period-adjusted literacy mean") + scale_color_discrete(name="Age at PIAAC", labels=c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
ggsave("Cohort Analysis/world-unweighted_ageing-pattern_region7.jpg", width =8, height=4)




#FINAL: UNWEIGHTED COUNTRY AVERAGE using 20 year age groups
rm(list = ls())

#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

ials<-subset(ials, AGEINT>0 & AGEINT<6)

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "AGEINT", "educ"), data=ials, config=piaac_conf)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("AGEINT"="age_ials", "Mean"="mean_ials"))
lit_mean_ials$age_ials<-as.numeric(as.factor(lit_mean_ials$age_ials))
lit_mean_ials$age_ials<-lit_mean_ials$age_ials*10+5
lit_mean_ials$age_piaac<-lit_mean_ials$age_ials+20
lit_mean_ials<-lit_mean_ials[c(1,2,9,3,5)]


#PIAAC
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 

lit_mean_piaac<-piaac.mean.pv(pvlabel="LIT", by=c("CNTRYID", "AGEG10LFS_T", "educ"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG10LFS_T"="age_piaac", "Mean"="mean_piaac"))
lit_mean_piaac$age_piaac<-as.numeric(as.factor(lit_mean_piaac$age_piaac))
lit_mean_piaac$age_piaac<-lit_mean_piaac$age_piaac*10+5
lit_mean_piaac$age_ials<-lit_mean_piaac$age_piaac-20
lit_mean_piaac<-lit_mean_piaac[c(1,9,2,3,5)]

cohort_pattern<-merge(lit_mean_ials, lit_mean_piaac, by=c("CNTRYID", "age_ials", "age_piaac", "educ"))


#period adjustment factor
lit_mean_ials_period<-lit_mean_ials[c(1,2,4,5)]
lit_mean_ials_period<-plyr::rename(lit_mean_ials_period, c("age_ials"="age"))
lit_mean_piaac_period<-lit_mean_piaac[c(1,3,4,5)]
lit_mean_piaac_period<-plyr::rename(lit_mean_piaac_period, c("age_piaac"="age"))
period<-merge(lit_mean_ials_period, lit_mean_piaac_period, by=c("CNTRYID", "age", "educ"))
period$ials_adj_factor<-period$mean_piaac/period$mean_ials
period<-period[c(1,2,3,6)]
period<-plyr::rename(period, c("age"="age_ials"))


ageing_pattern<-merge(cohort_pattern, period, by=c("CNTRYID", "age_ials", "educ"))
ageing_pattern$mean_ials_adj<-ageing_pattern$mean_ials*ageing_pattern$ials_adj_factor
ageing_pattern$skill_change<-(ageing_pattern$mean_piaac-ageing_pattern$mean_ials_adj)/ageing_pattern$mean_ials_adj
ageing_pattern<-aggregate(skill_change~age_piaac+age_ials+educ, FUN=mean, data=ageing_pattern)

cohort_pattern$skill_change<-(cohort_pattern$mean_piaac-cohort_pattern$mean_ials)/cohort_pattern$mean_ials
cohort_pattern<-aggregate(skill_change~age_piaac+age_ials+educ, FUN=mean, data=cohort_pattern)



#-----------------------------------------------------------------------------------------------------------------------------


#FINAL: UNWEIGHTED COUNTRY AVERAGE using 15 year age groups
rm(list = ls())

#IALS
ials_original<-read.spss("IALS94-98/Data/spss_data_compiled/IALSPUMFNPV2_E.sav", to.data.frame=TRUE)

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

ials<-plyr::rename(ials, c("REPLIC01"="REPLIC1", "REPLIC02"="REPLIC2", "REPLIC03"="REPLIC3", "REPLIC04"="REPLIC4", "REPLIC05"="REPLIC5", "REPLIC06"="REPLIC6", "REPLIC07"="REPLIC7", "REPLIC08"="REPLIC8", "REPLIC09"="REPLIC9"))
#ials<-plyr::rename(ials, c("GENDER"="sex"))
#ials<-subset(ials, sex!=9)

#recode education (2 broad education groups)
ials$A8<-ifelse(is.na(ials$A8), ials$A8RCD, ials$A8)
ials<-dplyr::mutate(ials, educ=ifelse (A8==0 | A8==1 | A8==2, 1, 0))  
ials<-dplyr::mutate(ials, educ=ifelse (A8==3 | A8==5 | A8==6 | A8==7, 2, educ)) 
ials<-subset(ials, educ>0)

ials<-mutate(ials, age=ifelse(AGE>=15&AGE<30, 15, 0))
ials<-mutate(ials, age=ifelse(AGE>=30&AGE<45, 30, age))
ials<-mutate(ials, age=ifelse(AGE>=45&AGE<60, 45, age))
ials<-mutate(ials, age=ifelse(AGE>=60&AGE<75, 60, age))
ials<-subset(ials, age>0)

library(intsvy)
piaac_conf<-intsvy.config(base.config=piaac_conf)
piaac_conf[["variables"]][["weightFinal"]]<-"WEIGHT"
piaac_conf[["variables"]][["weightBRR"]]<-"REPLIC"
piaac_conf[["parameters"]][["BRRreps"]]<-30

lit_mean_ials<-intsvy.mean.pv(pvnames=paste0("PVLIT", 1:10), by=c("CNTRYID", "age", "educ"), data=ials, config=piaac_conf)
lit_mean_ials<-plyr::rename(lit_mean_ials, c("age"="age_ials", "Mean"="mean_ials"))
lit_mean_ials$age_ials<-as.numeric(as.character(lit_mean_ials$age_ials))
lit_mean_ials$age_piaac<-lit_mean_ials$age_ials+15
lit_mean_ials<-lit_mean_ials[c(1,2,9,3,5)]


#PIAAC
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1 | EDCAT7==2, 1, 0))  #educ 1 = lower secondary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3 | EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 2, educ))     #educ 2 = upper secondary and higher
piaac<-subset(piaac, educ>0)  #remove observations without educ information 

piaac<-mutate(piaac, age=ifelse(AGEG5LFS>=1 & AGEG5LFS<4, 15, 0))
piaac<-mutate(piaac, age=ifelse(AGEG5LFS>=4 & AGEG5LFS<7, 30, age))
piaac<-mutate(piaac, age=ifelse(AGEG5LFS>=7 & AGEG5LFS<10, 45, age))
piaac<-mutate(piaac, age=ifelse(AGEG5LFS>=10, 60, age))
piaac<-subset(piaac, age>0)


lit_mean_piaac<-piaac.mean.pv(pvlabel="LIT", by=c("CNTRYID", "age", "educ"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("age"="age_piaac", "Mean"="mean_piaac"))
lit_mean_piaac$age_piaac<-as.numeric(as.character(lit_mean_piaac$age_piaac))
lit_mean_piaac$age_ials<-lit_mean_piaac$age_piaac-15
lit_mean_piaac<-lit_mean_piaac[c(1,9,2,3,5)]

cohort_pattern<-merge(lit_mean_ials, lit_mean_piaac, by=c("CNTRYID", "age_ials", "age_piaac", "educ"))


#period adjustment factor
lit_mean_ials_period<-lit_mean_ials[c(1,2,4,5)]
lit_mean_ials_period<-plyr::rename(lit_mean_ials_period, c("age_ials"="age"))
lit_mean_piaac_period<-lit_mean_piaac[c(1,3,4,5)]
lit_mean_piaac_period<-plyr::rename(lit_mean_piaac_period, c("age_piaac"="age"))
period<-merge(lit_mean_ials_period, lit_mean_piaac_period, by=c("CNTRYID", "age", "educ"))
period$ials_adj_factor<-period$mean_piaac/period$mean_ials
period<-period[c(1,2,3,6)]
period<-plyr::rename(period, c("age"="age_ials"))


ageing_pattern<-merge(cohort_pattern, period, by=c("CNTRYID", "age_ials", "educ"))
ageing_pattern$mean_ials_adj<-ageing_pattern$mean_ials*ageing_pattern$ials_adj_factor
ageing_pattern$skill_change<-(ageing_pattern$mean_piaac-ageing_pattern$mean_ials_adj)/ageing_pattern$mean_ials_adj
ageing_pattern<-aggregate(cbind(skill_change, mean_ials_adj, mean_piaac)~age_piaac+age_ials+educ, FUN=mean, data=ageing_pattern)
write.csv(ageing_pattern, "Cohort Analysis/ageing_pattern_world.csv", row.names=FALSE)

cohort_pattern$skill_change<-(cohort_pattern$mean_piaac-cohort_pattern$mean_ials)/cohort_pattern$mean_ials
cohort_pattern<-aggregate(skill_change~age_piaac+age_ials+educ, FUN=mean, data=cohort_pattern)

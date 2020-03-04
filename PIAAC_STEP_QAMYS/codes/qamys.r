setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC")        #DROPBOX VID
#setwd("C:/Users/reiter/OneDrive - IIASA/PIAAC")          #ONEDRIVE IIASA
setwd("/Users/claudiareiter/OneDrive - IIASA/PIAAC")     #ONEDRIVE LAPTOP

#LOAD PACKAGES
#install.packages("intsvy")
library("intsvy")
#install.packages("dplyr")
library("dplyr")
#install.packages("plyr")
library("plyr")
#install.packages("tidyverse")
library("tidyverse")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("reshape2")
library("reshape2")
#install.packages("tidyr")
library("tidyr")
#install.packages("ggforce")
library("ggforce")
#install.packages("remotes")
library("remotes")
#remotes::install_github("dgrtwo/drlib")
library("drlib")

rm(list = ls())


#QAMYS including education duration
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso", "Mean"="mean_country", "Freq"="n_country"))
lit_mean_piaac$age<-as.numeric(as.factor(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10

missing_values<-read.csv("OECD-average/QAMYS/missing-values_age_sex_educ.csv")
lit_mean_piaac<-rbind(missing_values, lit_mean_piaac)

#install.packages("zoo")
library(zoo)

lit_mean_piaac<-lit_mean_piaac[with(lit_mean_piaac, order(iso, age, sex, educ)),]

lit_mean_piaac_educ234<-lit_mean_piaac %>% subset(educ!=1) %>%
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=FALSE))

lit_mean_piaac_educ123<-lit_mean_piaac %>% subset(educ!=4) %>%
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=TRUE))

lit_mean_piaac_educ1<-subset(lit_mean_piaac_educ123, educ==1)
lit_mean_piaac_educ2<-subset(lit_mean_piaac_educ123, educ==2)
lit_mean_piaac_educ3<-subset(lit_mean_piaac_educ234, educ==3)
lit_mean_piaac_educ4<-subset(lit_mean_piaac_educ234, educ==4)
lit_mean_piaac<-rbind(lit_mean_piaac_educ1, lit_mean_piaac_educ2, lit_mean_piaac_educ3, lit_mean_piaac_educ4)

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean<-subset(step_lit_mean, age>0 & sex>0 & educ>0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)

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

lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

qamys_dur<-read.csv("OECD-average/QAMYS/QAMYS_edu-duration_wide.csv")
qamys_dur<-melt(qamys_dur, id.vars = c("iso","sex","age"))
qamys_dur$variable<-as.numeric(gsub("edu","",qamys_dur$variable))
qamys_dur<-plyr::rename(qamys_dur,c("value"="dur", "variable"="educ"))
qamys_dur<-subset(qamys_dur,age<65)
write.csv(qamys_dur,"OECD-average/QAMYS/QAMYS_edu-duration_long.csv", row.names=FALSE)
# 
OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age>0 & sex>0 & educ>0)

qamys_by_age_sex_educ<-merge(qamys_dur,lit_mean, by=c("age", "sex", "educ", "iso"))
qamys_by_age_sex_educ<-merge(qamys_by_age_sex_educ, OECD_avg, by=c("age", "sex", "educ"))
qamys_by_age_sex_educ$adj_factor<-qamys_by_age_sex_educ$mean_country/qamys_by_age_sex_educ$weighted_mean
qamys_by_age_sex_educ<-qamys_by_age_sex_educ %>% mutate(adj_factor = replace_na(adj_factor, 1))
qamys_by_age_sex_educ$dur_adj<-qamys_by_age_sex_educ$dur*qamys_by_age_sex_educ$adj_factor

wic_pop<-read.csv("OECD-average/wic_pop.csv")
wic_pop<-subset(wic_pop, year==2015) #use 2015 population
wic_pop<-subset(wic_pop, age>0)      #remove age groups 0-14
wic_pop<-subset(wic_pop, age<11)     #remove age groups 65+
wic_pop<-subset(wic_pop, educ!=99)   #remove educ category "Total"
wic_pop<-subset(wic_pop, educ<5)     #remove post-secondary sub-categories
wic_pop<-dplyr::mutate(wic_pop, educ = ifelse (educ==-1 | educ==0, 1, educ))
wic_pop<-aggregate(pop ~ age+sex+educ+iso, FUN=sum, data=wic_pop)   #sum up education categories with educ=1 (no educ, incomplete primary, primary)
wic_pop<-subset(wic_pop, educ>0)  
wic_pop$age<-(wic_pop$age*5)+10

qamys<-merge(qamys_by_age_sex_educ, wic_pop, by=c("iso", "age", "sex", "educ"))
qamys<-ddply(qamys,.(age, sex, iso), transform, prop=pop/sum(pop))
qamys$qamys<-qamys$dur_adj*qamys$prop
qamys<-aggregate(qamys ~ iso+age+sex, FUN=sum, data=qamys)

mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
mys<-subset(mys, age>0 & sex>0 & year==2015)
qamys<-merge(qamys, mys, by=c("age", "sex", "iso"))
qamys<-qamys[,-c(7)]

qamys_by_age_sex_educ<-qamys_by_age_sex_educ[,-c(5,14)]
qamys_by_age_sex_educ<-merge(qamys_by_age_sex_educ, qamys, by=c("age", "sex","iso"))
qamys_by_age_sex_educ<-qamys_by_age_sex_educ[,c(3,14,1,2,4,10,11,5,6,16,12,13)]
qamys_by_age_sex_educ<-plyr::rename(qamys_by_age_sex_educ, c("weighted_mean"="weighted_OECD_avg", "n"="n_OECD"))
write.csv(qamys_by_age_sex_educ, "OECD-average/QAMYS/qamys_age_sex_educ.csv", row.names=FALSE)

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


rm(list = ls())

#QAMYS by country
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-subset(piaac_all_countries, AGEG5LFS>1)

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "Freq"="n_country", "Mean"="mean_country"))
lit_mean_piaac$age<-0
lit_mean_piaac$sex<-0
lit_mean_piaac$educ<-0

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean<-subset(step_lit_mean, age==0 & sex==0 & educ==0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)
lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age==0 & sex==0 & educ==0)
qamys_by_country<-merge(OECD_avg, lit_mean)
qamys_by_country$adj_factor<-qamys_by_country$mean_country/qamys_by_country$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age==0 & sex==0 & educ==0 & year==2015)

qamys_by_country<-merge(wic_mys, qamys_by_country, by=c("iso", "age", "sex", "educ"))
qamys_by_country$qamys<-qamys_by_country$mys*qamys_by_country$adj_factor
qamys_by_country<-qamys_by_country[c("iso", "country", "age", "sex","educ", "weighted_mean", "n", "mean_country", "n_country", "mys", "adj_factor", "qamys")]
qamys_by_country<-plyr::rename(qamys_by_country, c("n"="n_OECD", "weighted_mean"="weighted_OECD_avg"))

#-------------------------------------------------------------------------------------------------
#QAMYS by country and age
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "AGEG5LFS"), data=piaac_all_countries)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "Freq"="n_country", "Mean"="mean_country", "AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10
lit_mean_piaac$sex<-0
lit_mean_piaac$educ<-0

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean<-subset(step_lit_mean, age>0 & sex==0 & educ==0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)
lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age>0 & sex==0 & educ==0)
qamys_by_country_age<-merge(OECD_avg, lit_mean, by=c("age", "sex", "educ"))
qamys_by_country_age$adj_factor<-qamys_by_country_age$mean_country/qamys_by_country_age$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age>0 & sex==0 & educ==0 & year==2015)

qamys_by_country_age<-merge(wic_mys, qamys_by_country_age, by=c("iso", "age", "sex", "educ"))
qamys_by_country_age$qamys<-qamys_by_country_age$mys*qamys_by_country_age$adj_factor
qamys_by_country_age<-qamys_by_country_age[c("iso", "country", "age", "sex","educ", "weighted_mean", "n", "mean_country", "n_country", "mys", "adj_factor", "qamys")]
qamys_by_country_age<-plyr::rename(qamys_by_country_age, c("n"="n_OECD", "weighted_mean"="weighted_OECD_avg"))


#-------------------------------------------------------------------------------------------------
#QAMYS by country and sex
piaac<-subset(piaac_all_countries, GENDER_R==1 | GENDER_R==2)
piaac<-subset(piaac, AGEG5LFS>1)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "GENDER_R"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "Freq"="n_country", "Mean"="mean_country", "GENDER_R"="sex"))
lit_mean_piaac$age<-0
lit_mean_piaac$educ<-0

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean<-subset(step_lit_mean, age==0 & sex>0 & educ==0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)
lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age==0 & sex>0 & educ==0)
qamys_by_country_sex<-merge(OECD_avg, lit_mean, by=c("age", "sex", "educ"))
qamys_by_country_sex$adj_factor<-qamys_by_country_sex$mean_country/qamys_by_country_sex$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age==0 & sex>0 & educ==0 & year==2015)

qamys_by_country_sex<-merge(wic_mys, qamys_by_country_sex, by=c("iso", "age", "sex", "educ"))
qamys_by_country_sex$qamys<-qamys_by_country_sex$mys*qamys_by_country_sex$adj_factor
qamys_by_country_sex<-qamys_by_country_sex[c("iso", "country", "age", "sex","educ", "weighted_mean", "n", "mean_country", "n_country", "mys", "adj_factor", "qamys")]
qamys_by_country_sex<-plyr::rename(qamys_by_country_sex, c("n"="n_OECD", "weighted_mean"="weighted_OECD_avg"))


#-------------------------------------------------------------------------------------------------
#QAMYS by country, age and sex
piaac<-subset(piaac_all_countries, GENDER_R==1 | GENDER_R==2)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "GENDER_R", "AGEG5LFS"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "Freq"="n_country", "Mean"="mean_country", "GENDER_R"="sex", "AGEG5LFS"="age"))
lit_mean_piaac$age<-as.numeric(as.character(lit_mean_piaac$age))
lit_mean_piaac$age<-(lit_mean_piaac$age*5)+10
lit_mean_piaac$educ<-0

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean<-subset(step_lit_mean, age>0 & sex>0 & educ==0)
step_lit_mean$iso<-as.factor(step_lit_mean$iso)
lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age>0 & sex>0 & educ==0)
qamys_by_country_age_sex<-merge(OECD_avg, lit_mean, by=c("sex", "age", "educ"))
qamys_by_country_age_sex$adj_factor<-qamys_by_country_age_sex$mean_country/qamys_by_country_age_sex$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age>0 & sex>0 & educ==0 & year==2015)

qamys_by_country_age_sex<-merge(wic_mys, qamys_by_country_age_sex, by=c("iso", "age", "sex", "educ"))
qamys_by_country_age_sex$qamys<-qamys_by_country_age_sex$mys*qamys_by_country_age_sex$adj_factor
qamys_by_country_age_sex<-qamys_by_country_age_sex[c("iso", "country", "age", "sex","educ", "weighted_mean", "n", "mean_country", "n_country", "mys", "adj_factor", "qamys")]
qamys_by_country_age_sex<-plyr::rename(qamys_by_country_age_sex, c("n"="n_OECD", "weighted_mean"="weighted_OECD_avg"))


#QAMYS by country, age, sex and educ
qamys_by_country_age_sex_educ<-read.csv("OECD-average/QAMYS/qamys_age_sex_educ.csv")
qamys<-rbind(qamys_by_country, qamys_by_country_age, qamys_by_country_sex, qamys_by_country_age_sex, qamys_by_country_age_sex_educ)
write.csv(qamys,"OECD-average/QAMYS/qamys_final.csv", row.names=FALSE )




#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#HLO qamys
hlo<-read.csv("OECD-average/hlo.csv")
hlo<-aggregate(HLO ~ country+iso+year, FUN=mean, data=hlo)
hlo$age<-15
hlo$sex<-0
hlo$educ<-0
hlo<-plyr::rename(hlo, c("HLO"="hlo_mean_country"))

hlo_OECD_avg<-read.csv("OECD-average/hlo_weighted_OECD-avg.csv")
hlo_qamys<-merge(hlo, hlo_OECD_avg, by=c("age", "sex", "educ"))
hlo_qamys$adj_factor<-hlo_qamys$hlo_mean_country/hlo_qamys$hlo_oecd_avg

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age==15 & sex==0 & educ==0)

hlo_qamys<-merge(wic_mys, hlo_qamys, by=c("iso", "country", "age", "sex", "educ", "year"))
hlo_qamys$qamys<-hlo_qamys$mys*hlo_qamys$adj_factor
hlo_qamys<-hlo_qamys[c("iso", "country", "age", "sex","educ", "hlo_oecd_avg", "hlo_mean_country", "mys", "adj_factor", "qamys")]
hlo_qamys<-plyr::rename(hlo_qamys, c("hlo_oecd_avg"="weighted_OECD_avg"))
write.csv(hlo_qamys, "OECD-average/QAMYS/hlo_qamys.csv", row.names=FALSE)








#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#PIAAC MYS
#rm(list = ls())

# #Austria
# austria<-read.csv("Data/CSV/prgautp1.csv")
# aut<-dplyr::mutate(austria, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# aut<-dplyr::mutate(aut, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# aut<-dplyr::mutate(aut, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# aut<-dplyr::mutate(aut, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# aut<-subset(aut, educ>0)  #remove observations without educ information 
# aut<-subset(aut, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_aut<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=aut)
# 
# #Belgium
# belgium<-read.csv("Data/CSV/prgbelp1.csv")
# bel<-dplyr::mutate(belgium, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# bel<-dplyr::mutate(bel, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# bel<-dplyr::mutate(bel, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# bel<-dplyr::mutate(bel, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# bel<-subset(bel, educ>0)  #remove observations without educ information 
# bel<-subset(bel, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_bel<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=bel)
# 
# #Canada
# canada<-read.csv("Data/CSV/prgcanp1.csv")
# can<-dplyr::mutate(canada, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# can<-dplyr::mutate(can, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# can<-dplyr::mutate(can, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# can<-dplyr::mutate(can, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# can<-subset(can, educ>0)  #remove observations without educ information 
# can<-subset(can, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_can<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=can)
# 
# #Chile
# chile<-read.csv("Data/CSV/prgchlp1.csv")
# chl<-dplyr::mutate(chile, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# chl<-dplyr::mutate(chl, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# chl<-dplyr::mutate(chl, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# chl<-dplyr::mutate(chl, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# chl<-subset(chl, educ>0)  #remove observations without educ information 
# chl<-subset(chl, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_chl<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=chl)
# 
# #Cyprus
# cyprus<-read.csv("Data/CSV/prgcypp1.csv")
# cyp<-dplyr::mutate(cyprus, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# cyp<-dplyr::mutate(cyp, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# cyp<-dplyr::mutate(cyp, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# cyp<-dplyr::mutate(cyp, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# cyp<-subset(cyp, educ>0)  #remove observations without educ information 
# cyp<-subset(cyp, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_cyp<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=cyp)
# 
# #Czechia
# czechia<-read.csv("Data/CSV/prgczep1.csv")
# cze<-dplyr::mutate(czechia, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# cze<-dplyr::mutate(cze, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# cze<-dplyr::mutate(cze, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# cze<-dplyr::mutate(cze, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# cze<-subset(cze, educ>0)  #remove observations without educ information 
# cze<-subset(cze, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_cze<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=cze)
# 
# #Germany
# germany<-read.csv("Data/CSV/prgdeup1.csv")
# deu<-dplyr::mutate(germany, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# deu<-dplyr::mutate(deu, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# deu<-dplyr::mutate(deu, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# deu<-dplyr::mutate(deu, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# deu<-subset(deu, educ>0)  #remove observations without educ information 
# deu<-subset(deu, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_deu<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=deu)
# 
# #Denmark
# denmark<-read.csv("Data/CSV/prgdnkp1.csv")
# dnk<-dplyr::mutate(denmark, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# dnk<-dplyr::mutate(dnk, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# dnk<-dplyr::mutate(dnk, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# dnk<-dplyr::mutate(dnk, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# dnk<-subset(dnk, educ>0)  #remove observations without educ information 
# dnk<-subset(dnk, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_dnk<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=dnk)
# 
# #Ecuador
# ecuador<-read.csv("Data/CSV/prgecup1.csv")
# ecu<-dplyr::mutate(ecuador, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# ecu<-dplyr::mutate(ecu, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# ecu<-dplyr::mutate(ecu, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# ecu<-dplyr::mutate(ecu, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# ecu<-subset(ecu, educ>0)  #remove observations without educ information 
# ecu<-subset(ecu, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_ecu<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=ecu)
# 
# #Spain
# spain<-read.csv("Data/CSV/prgespp1.csv")
# esp<-dplyr::mutate(spain, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# esp<-dplyr::mutate(esp, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# esp<-dplyr::mutate(esp, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# esp<-dplyr::mutate(esp, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# esp<-subset(esp, educ>0)  #remove observations without educ information 
# esp<-subset(esp, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_esp<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=esp)
# 
# #Estonia
# estonia<-read.csv("Data/CSV/prgestp1.csv")
# est<-dplyr::mutate(estonia, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# est<-dplyr::mutate(est, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# est<-dplyr::mutate(est, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# est<-dplyr::mutate(est, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# est<-subset(est, educ>0)  #remove observations without educ information 
# est<-subset(est, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_est<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=est)
# 
# #Finland
# finland<-read.csv("Data/CSV/prgfinp1.csv")
# fin<-dplyr::mutate(finland, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# fin<-dplyr::mutate(fin, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# fin<-dplyr::mutate(fin, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# fin<-dplyr::mutate(fin, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# fin<-subset(fin, educ>0)  #remove observations without educ information 
# fin<-subset(fin, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_fin<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=fin)
# 
# #France
# france<-read.csv("Data/CSV/prgfrap1.csv")
# fra<-dplyr::mutate(france, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# fra<-dplyr::mutate(fra, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# fra<-dplyr::mutate(fra, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# fra<-dplyr::mutate(fra, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# fra<-subset(fra, educ>0)  #remove observations without educ information 
# fra<-subset(fra, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_fra<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=fra)
# 
# #UK
# uk<-read.csv("Data/CSV/prggbrp1.csv")
# gbr<-dplyr::mutate(uk, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# gbr<-dplyr::mutate(gbr, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# gbr<-dplyr::mutate(gbr, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# gbr<-dplyr::mutate(gbr, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# gbr<-subset(gbr, educ>0)  #remove observations without educ information 
# gbr<-subset(gbr, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_gbr<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=gbr)
# 
# #Greece
# greece<-read.csv("Data/CSV/prggrcp1.csv")
# grc<-dplyr::mutate(greece, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# grc<-dplyr::mutate(grc, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# grc<-dplyr::mutate(grc, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# grc<-dplyr::mutate(grc, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# grc<-subset(grc, educ>0)  #remove observations without educ information 
# grc<-subset(grc, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_grc<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=grc)
# 
# #Hungary
# hungary<-read.csv("Data/CSV/prghunp1.csv")
# hun<-dplyr::mutate(hungary, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# hun<-dplyr::mutate(hun, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# hun<-dplyr::mutate(hun, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# hun<-dplyr::mutate(hun, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# hun<-subset(hun, educ>0)  #remove observations without educ information 
# hun<-subset(hun, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_hun<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=hun)
# 
# #Ireland
# ireland<-read.csv("Data/CSV/prgirlp1.csv")
# irl<-dplyr::mutate(ireland, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# irl<-dplyr::mutate(irl, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# irl<-dplyr::mutate(irl, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# irl<-dplyr::mutate(irl, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# irl<-subset(irl, educ>0)  #remove observations without educ information 
# irl<-subset(irl, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_irl<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=irl)
# 
# #Israel
# israel<-read.csv("Data/CSV/prgisrp1.csv")
# isr<-dplyr::mutate(israel, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# isr<-dplyr::mutate(isr, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# isr<-dplyr::mutate(isr, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# isr<-dplyr::mutate(isr, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# isr<-subset(isr, educ>0)  #remove observations without educ information 
# isr<-subset(isr, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_isr<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=isr)
# 
# #Italy
# italy<-read.csv("Data/CSV/prgitap1.csv")
# ita<-dplyr::mutate(italy, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# ita<-dplyr::mutate(ita, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# ita<-dplyr::mutate(ita, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# ita<-dplyr::mutate(ita, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# ita<-subset(ita, educ>0)  #remove observations without educ information 
# ita<-subset(ita, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_ita<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=ita)
# 
# #Japan
# japan<-read.csv("Data/CSV/prgjpnp1.csv")
# jpn<-dplyr::mutate(japan, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# jpn<-dplyr::mutate(jpn, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# jpn<-dplyr::mutate(jpn, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# jpn<-dplyr::mutate(jpn, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# jpn<-subset(jpn, educ>0)  #remove observations without educ information 
# jpn<-subset(jpn, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_jpn<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=jpn)
# 
# #Kazakhstan
# kazakhstan<-read.csv("Data/CSV/prgkazp1.csv")
# kaz<-dplyr::mutate(kazakhstan, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# kaz<-dplyr::mutate(kaz, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# kaz<-dplyr::mutate(kaz, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# kaz<-dplyr::mutate(kaz, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# kaz<-subset(kaz, educ>0)  #remove observations without educ information 
# kaz<-subset(kaz, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_kaz<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=kaz)
# 
# #Korea  
# korea<-read.csv("Data/CSV/prgkorp1.csv")
# kor<-dplyr::mutate(korea, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# kor<-dplyr::mutate(kor, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# kor<-dplyr::mutate(kor, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# kor<-dplyr::mutate(kor, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# kor<-subset(kor, educ>0)  #remove observations without educ information 
# kor<-subset(kor, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_kor<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=kor)
# 
# #Lithuania
# lithuania<-read.csv("Data/CSV/prgltup1.csv")
# ltu<-dplyr::mutate(lithuania, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# ltu<-dplyr::mutate(ltu, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# ltu<-dplyr::mutate(ltu, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# ltu<-dplyr::mutate(ltu, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# ltu<-subset(ltu, educ>0)  #remove observations without educ information 
# ltu<-subset(ltu, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_ltu<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=ltu)
# 
# #Mexico
# mexico<-read.csv("Data/CSV/prgmexp1.csv")
# mex<-dplyr::mutate(mexico, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# mex<-dplyr::mutate(mex, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# mex<-dplyr::mutate(mex, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# mex<-dplyr::mutate(mex, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# mex<-subset(mex, educ>0)  #remove observations without educ information 
# mex<-subset(mex, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_mex<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=mex)
# 
# #Netherlands
# netherlands<-read.csv("Data/CSV/prgnldp1.csv")
# nld<-dplyr::mutate(netherlands, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# nld<-dplyr::mutate(nld, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# nld<-dplyr::mutate(nld, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# nld<-dplyr::mutate(nld, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# nld<-subset(nld, educ>0)  #remove observations without educ information 
# nld<-subset(nld, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_nld<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=nld)
# 
# #Norway
# norway<-read.csv("Data/CSV/prgnorp1.csv")
# nor<-dplyr::mutate(norway, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# nor<-dplyr::mutate(nor, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# nor<-dplyr::mutate(nor, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# nor<-dplyr::mutate(nor, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# nor<-subset(nor, educ>0)  #remove observations without educ information 
# nor<-subset(nor, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_nor<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=nor)
# 
# #New Zealand
# newzealand<-read.csv("Data/CSV/prgnzlp1.csv")
# nzl<-dplyr::mutate(newzealand, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# nzl<-dplyr::mutate(nzl, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# nzl<-dplyr::mutate(nzl, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# nzl<-dplyr::mutate(nzl, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# nzl<-subset(nzl, educ>0)  #remove observations without educ information 
# nzl<-subset(nzl, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_nzl<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=nzl)
# 
# #Peru
# peru<-read.csv("Data/CSV/prgperp1.csv")
# per<-dplyr::mutate(peru, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# per<-dplyr::mutate(per, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# per<-dplyr::mutate(per, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
# per<-dplyr::mutate(per, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# per<-subset(per, educ>0)  #remove observations without educ information 
# per<-subset(per, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_per<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=per)
# 
# #Poland
# poland<-read.csv("Data/CSV/prgpolp1.csv")
# pol<-dplyr::mutate(poland, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = uppol secondary
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# pol<-subset(pol, educ>0)  #remove observations without educ information 
# pol<-subset(pol, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_pol<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=pol)
# 
# #Poland
# poland<-read.csv("Data/CSV/prgpolp1.csv")
# pol<-dplyr::mutate(poland, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = uppol secondary
# pol<-dplyr::mutate(pol, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# pol<-subset(pol, educ>0)  #remove observations without educ information 
# pol<-subset(pol, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_pol<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=pol)
# 
# #Russia
# russia<-read.csv("Data/CSV/prgrusp1.csv")
# rus<-dplyr::mutate(russia, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# rus<-dplyr::mutate(rus, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# rus<-dplyr::mutate(rus, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = uprus secondary
# rus<-dplyr::mutate(rus, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# rus<-subset(rus, educ>0)  #remove observations without educ information 
# rus<-subset(rus, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_rus<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=rus)
# 
# #Singapore
# singapore<-read.csv("Data/CSV/prgsgpp1.csv")
# sgp<-dplyr::mutate(singapore, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# sgp<-dplyr::mutate(sgp, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# sgp<-dplyr::mutate(sgp, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upsgp secondary
# sgp<-dplyr::mutate(sgp, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# sgp<-subset(sgp, educ>0)  #remove observations without educ information 
# sgp<-subset(sgp, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_sgp<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=sgp)
# 
# #Slovakia
# slovakia<-read.csv("Data/CSV/prgsvkp1.csv")
# svk<-dplyr::mutate(slovakia, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# svk<-dplyr::mutate(svk, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# svk<-dplyr::mutate(svk, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upsvk secondary
# svk<-dplyr::mutate(svk, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# svk<-subset(svk, educ>0)  #remove observations without educ information 
# svk<-subset(svk, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_svk<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=svk)
# 
# #Slovenia
# slovenia<-read.csv("Data/CSV/prgsvnp1.csv")
# svn<-dplyr::mutate(slovenia, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# svn<-dplyr::mutate(svn, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# svn<-dplyr::mutate(svn, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upsvn secondary
# svn<-dplyr::mutate(svn, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# svn<-subset(svn, educ>0)  #remove observations without educ information 
# svn<-subset(svn, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_svn<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=svn)
# 
# #Sweden
# sweden<-read.csv("Data/CSV/prgswep1.csv")
# swe<-dplyr::mutate(sweden, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# swe<-dplyr::mutate(swe, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# swe<-dplyr::mutate(swe, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upswe secondary
# swe<-dplyr::mutate(swe, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# swe<-subset(swe, educ>0)  #remove observations without educ information 
# swe<-subset(swe, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_swe<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=swe)
# 
# #Turkey
# turkey<-read.csv("Data/CSV/prgturp1.csv")
# tur<-dplyr::mutate(turkey, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# tur<-dplyr::mutate(tur, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# tur<-dplyr::mutate(tur, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = uptur secondary
# tur<-dplyr::mutate(tur, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# tur<-subset(tur, educ>0)  #remove observations without educ information 
# tur<-subset(tur, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_tur<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=tur)
# 
# #US
# us<-read.csv("Data/CSV/prgusap1.csv")
# usa<-dplyr::mutate(us, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# usa<-dplyr::mutate(usa, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# usa<-dplyr::mutate(usa, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upusa secondary
# usa<-dplyr::mutate(usa, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# usa<-subset(usa, educ>0)  #remove observations without educ information 
# usa<-subset(usa, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# mys_usa<-piaac.mean(variable="YRSQUAL", by=c("GENDER_R", "AGEG5LFS", "CNTRYID", "educ"), data=usa)
# 
# mys_age_sex_educ<-rbind(mys_aut, mys_bel, mys_can, mys_chl, mys_cyp, mys_cze, mys_deu, mys_dnk, mys_ecu, mys_esp, mys_est, mys_fin, mys_fra, mys_gbr, mys_grc, mys_hun, mys_irl, mys_isr, mys_ita, mys_jpn, mys_kaz, mys_kor, mys_ltu, mys_mex, mys_nld, mys_nor, mys_nzl, mys_per, mys_pol, mys_rus, mys_sgp, mys_svk, mys_svn, mys_swe, mys_tur, mys_usa)
# write.csv(mys_age_sex_educ, "OECD-average/QAMYS/piaac_mys_age_sex_educ.csv", row.names=FALSE)
# ggplot(mys_age_sex_educ, aes(x=educ, y=Mean)) + geom_col() + facet_wrap_paginate(~AGEG5LFS+CNTRYID, nrow=3, ncol=3, page=1)




# #WITHOUT USING WEIGHTS
# piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")
# piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
# piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
# piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = uppiaac secondary
# piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
# piaac<-subset(piaac, educ>0)  #remove observations without educ information 
# piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
# piaac<-subset(piaac, YRSQUAL!="N" & YRSQUAL!="V" & YRSQUAL!="D" & YRSQUAL!="R" & YRSQUAL!="")
# piaac$YRSQUAL<-as.numeric(as.character(piaac$YRSQUAL))
# mys_piaac<-aggregate(YRSQUAL ~ AGEG5LFS+GENDER_R+educ+CNTRYID, FUN=mean, data=piaac)
# mys_piaac<-plyr::rename(mys_piaac, c("AGEG5LFS"="age", "CNTRYID"="iso", "GENDER_R"="sex"))
# write.csv(mys_piaac, "OECD-average/QAMYS/piaac_mys_age_sex_educ.csv", row.names=FALSE)
# 
# piaac$PVAVG1<-(piaac$PVLIT1+piaac$PVNUM1)/2
# piaac$PVAVG2<-(piaac$PVLIT2+piaac$PVNUM2)/2
# piaac$PVAVG3<-(piaac$PVLIT3+piaac$PVNUM3)/2
# piaac$PVAVG4<-(piaac$PVLIT4+piaac$PVNUM4)/2
# piaac$PVAVG5<-(piaac$PVLIT5+piaac$PVNUM5)/2
# piaac$PVAVG6<-(piaac$PVLIT6+piaac$PVNUM6)/2
# piaac$PVAVG7<-(piaac$PVLIT7+piaac$PVNUM7)/2
# piaac$PVAVG8<-(piaac$PVLIT8+piaac$PVNUM8)/2
# piaac$PVAVG9<-(piaac$PVLIT9+piaac$PVNUM9)/2
# piaac$PVAVG10<-(piaac$PVLIT10+piaac$PVNUM10)/2
# 
# piaac_avg_mean<-piaac.mean.pv(pvlabel = "AVG", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
# piaac_avg_mean<-plyr::rename(piaac_avg_mean, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso", "Mean"="mean"))
# 
# weighted_OECD_avg<-read.csv("OECD-average/Output/weighted_OECD_average_avg.csv")
# 
# piaac_qamys<-merge(mys_piaac, piaac_avg_mean, by=c("iso", "age", "sex", "educ"))
# piaac_qamys<-merge(piaac_qamys, weighted_OECD_avg, by=c("age", "sex", "educ"))
# piaac_qamys<-plyr::rename(piaac_qamys, c("weighted_mean"="OECD_avg", "YRSQUAL"="mys"))
# piaac_qamys$adj_factor<-piaac_qamys$mean/piaac_qamys$OECD_avg
# piaac_qamys<-piaac_qamys[c("iso", "age", "sex", "educ", "mean", "OECD_avg", "adj_factor", "mys")]
# piaac_qamys$qamys<-piaac_qamys$mys*piaac_qamys$adj_factor
# piaac_qamys$qamys <- ifelse(is.na(piaac_qamys$qamys), piaac_qamys$mys, piaac_qamys$qamys)   #using MYS when no adjustment factor is available (too small sample)
# 
# educ_distr<-read.csv("OECD-average/QAMYS/wic_educ_distribution.csv")
# educ_distr<-subset(educ_distr, educ>0 & educ<5)
# educ_distr<-subset(educ_distr, age<11)
# educ_distr<-aggregate(distribution ~ iso+age+sex+educ+country, FUN=sum, data=educ_distr)
# piaac_qamys<-merge(piaac_qamys, educ_distr, by=c("age", "sex", "educ", "iso"))
# piaac_qamys$distribution<-piaac_qamys$distribution/100
# piaac_qamys$weighted_qamys<-piaac_qamys$qamys*piaac_qamys$distribution
# piaac_qamys$weighted_mys<-piaac_qamys$mys*piaac_qamys$distribution
# write.csv(piaac_qamys, "OECD-average/QAMYS/piaac_qamys_age-sex-educ.csv", row.names=FALSE)
# 
# piaac_qamys_age_sex<-aggregate(cbind(weighted_qamys, weighted_mys)~age+sex+iso+country, FUN=sum, data=piaac_qamys)
# piaac_qamys_age_sex$diff<-piaac_qamys_age_sex$weighted_qamys - piaac_qamys_age_sex$weighted_mys
# write.csv(piaac_qamys_age_sex, "OECD-average/QAMYS/piaac_qamys_age-sex.csv", row.names=FALSE)
# 
# 
# #plot by age and sex
# piaac_qamys_age_sex$age<-factor(piaac_qamys_age_sex$age, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), label=c("15-19", "20-24", "25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64"))
# piaac_qamys_age_sex<-dplyr::mutate(piaac_qamys_age_sex, sex=ifelse (sex==1, "Male", "Female"))
# 
# j=1
# pdf("OECD-average/QAMYS/piaac_QAMYS_by_age.pdf", paper="a4r", width = 13, height = 5)
# for(i in levels(piaac_qamys_age_sex$age)){
#   piaac_qamys_by_age<-subset(piaac_qamys_age_sex, age==i)
#   piaac_qamys_by_age<-piaac_qamys_by_age[order(-piaac_qamys_by_age$diff), ,drop = FALSE]
#   print (ggplot(piaac_qamys_by_age, aes(reorder_within(country, -diff, sex), diff, fill=sex)) + xlab("Country") + ylab("Diference (QAMYS-MYS)")  + geom_col()  + ggforce::facet_wrap_paginate (~age+sex, ncol=2, page=j, scales="free_x") + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + scale_x_reordered()) 
#   j=j+1}
# dev.off()

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


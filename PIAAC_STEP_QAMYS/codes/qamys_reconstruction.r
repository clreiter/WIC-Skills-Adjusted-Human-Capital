setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC")   #VID
setwd("C:/Users/reiter/OneDrive - IIASA/PIAAC")   #IIASA
setwd("/Users/claudiareiter/OneDrive - IIASA/PIAAC")   #Laptop
rm(list = ls())

library(plyr)
library(dplyr)

qamys2015<-read.csv("OECD-average/QAMYS/qamys_final.csv")
scores2015<-qamys2015[c("iso", "country", "age", "sex", "educ", "mean_country", "weighted_OECD_avg")]
scores2015<-subset(scores2015, age>0 & sex>0 & educ>0)
scores2015<-plyr::rename(scores2015, c("mean_country"="score_2015"))

ageing_adj<-read.csv("Cohort Analysis/qamys-reconstruction/ageing_adjustment.csv")
reconstruction<-merge(ageing_adj, scores2015, by=c("age", "sex", "educ"))
reconstruction$score<-reconstruction$score_2015*reconstruction$ageing
reconstruction$adj_factor<-reconstruction$score/reconstruction$weighted_OECD_avg

wic_pop<-read.csv("Cohort Analysis/qamys-reconstruction/wic_pop_reconstruction.csv")
wic_pop<-subset(wic_pop, age>0 & sex>0 & educ>0)
wic_pop<-aggregate(pop~iso+year+age+sex+educ, FUN=sum, data=wic_pop)

reconstruction<-merge(reconstruction, wic_pop, by=c("iso", "year", "age", "sex", "educ"))
reconstruction <- reconstruction %>%
  mutate(adj_factor = if_else(is.na(adj_factor), 1, adj_factor))


#-------------------------------------------------------------------------------------------------------------------------------

#qamys by country, year, age, sex, educ
qamys_adjustment_by_age_sex_educ<-reconstruction[c("iso", "country", "year", "age", "sex", "educ", "adj_factor", "pop")]

mys_by_educ<-read.csv("OECD-average/QAMYS/QAMYS_edu-duration_long.csv")
qamys_age_sex_educ<-merge(mys_by_educ, qamys_adjustment_by_age_sex_educ, by=c("iso", "age", "sex", "educ"))
qamys_age_sex_educ$qamys<-qamys_age_sex_educ$dur*qamys_age_sex_educ$adj_factor
qamys_age_sex_educ<-qamys_age_sex_educ %>% dplyr::group_by(iso, year, age, sex) %>%
  dplyr::mutate(share=pop/sum(pop))
qamys_age_sex_educ$qamys_weighted<-qamys_age_sex_educ$qamys*qamys_age_sex_educ$share
qamys_age_sex<-aggregate(qamys_weighted~iso+year+country+age+sex, FUN=sum, data=qamys_age_sex_educ)

qamys_age_sex_educ<-qamys_age_sex_educ[c("iso", "country", "year", "age", "sex", "educ", "adj_factor")]
qamys_age_sex_educ<-merge(qamys_age_sex_educ, qamys_age_sex, by=c("iso", "country", "year", "age", "sex"))

mys<-read.csv("Cohort Analysis/qamys-reconstruction/WIC_mys.csv")
mys_by_age_sex<-subset(mys, age>0 & sex>0)
mys_by_age_sex<-mys_by_age_sex[,-5]
qamys_age_sex_educ<-merge(qamys_age_sex_educ, mys_by_age_sex, by=c("iso", "country", "year", "age", "sex"))
qamys_age_sex_educ<-plyr::rename(qamys_age_sex_educ, c("qamys_weighted"="qamys"))

#-------------------------------------------------------------------------------------------------------------------------------
#qamys by country, year, age, sex
qamys_age_sex<-reconstruction %>% 
  dplyr::group_by(iso, year, age, sex) %>% dplyr::mutate(share = pop/sum(pop)) 
qamys_age_sex<-qamys_age_sex[c("iso", "country", "year", "age", "sex", "educ", "score", "share")]
qamys_age_sex$score_weighted<-qamys_age_sex$share*qamys_age_sex$score
qamys_age_sex<-aggregate(score_weighted~iso+year+age+sex, FUN=sum, data=qamys_age_sex)

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age>0 & sex>0 & educ==0)
qamys_age_sex<-merge(OECD_avg, qamys_age_sex, by=c("age", "sex"))
qamys_age_sex$adj_factor<-qamys_age_sex$score_weighted/qamys_age_sex$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age>0 & sex>0 & educ==0)

qamys_age_sex<-merge(wic_mys, qamys_age_sex, by=c("iso", "year", "age", "sex"))
qamys_age_sex$qamys<-qamys_age_sex$adj_factor*qamys_age_sex$mys
qamys_age_sex$educ<-0
qamys_age_sex<-qamys_age_sex[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "qamys")]

#-------------------------------------------------------------------------------------------------------------------------------
#qamys by country, year, age
qamys_age<-reconstruction %>% 
  dplyr::group_by(iso, year, age) %>% dplyr::mutate(share = pop/sum(pop)) 
qamys_age<-qamys_age[c("iso", "country", "year", "age", "sex", "educ", "score", "share")]
qamys_age$score_weighted<-qamys_age$share*qamys_age$score
qamys_age<-aggregate(score_weighted~iso+year+age, FUN=sum, data=qamys_age)

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age>0 & sex==0 & educ==0)
qamys_age<-merge(OECD_avg, qamys_age, by=c("age"))
qamys_age$adj_factor<-qamys_age$score_weighted/qamys_age$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age>0 & sex==0 & educ==0)

qamys_age<-merge(wic_mys, qamys_age, by=c("iso", "year", "age"))
qamys_age$qamys<-qamys_age$adj_factor*qamys_age$mys
qamys_age$educ<-0
qamys_age$sex<-0
qamys_age<-qamys_age[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "qamys")]

#-------------------------------------------------------------------------------------------------------------------------------
#qamys by country, year, sex
qamys_sex<-reconstruction %>% subset(age>15) %>% 
  dplyr::group_by(iso, year, sex) %>% dplyr::mutate(share = pop/sum(pop)) 
qamys_sex<-qamys_sex[c("iso", "country", "year", "age", "sex", "educ", "score", "share")]
qamys_sex$score_weighted<-qamys_sex$share*qamys_sex$score
qamys_sex<-aggregate(score_weighted~iso+year+sex, FUN=sum, data=qamys_sex)

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age==0 & sex>0 & educ==0)
qamys_sex<-merge(OECD_avg, qamys_sex, by=c("sex"))
qamys_sex$adj_factor<-qamys_sex$score_weighted/qamys_sex$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age==0 & sex>0 & educ==0)

qamys_sex<-merge(wic_mys, qamys_sex, by=c("iso", "year", "sex"))
qamys_sex$qamys<-qamys_sex$adj_factor*qamys_sex$mys
qamys_sex$educ<-0
qamys_sex$age<-0
qamys_sex<-qamys_sex[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "qamys")]

#-------------------------------------------------------------------------------------------------------------------------------
#qamys by country, year
qamys_country<-reconstruction %>% subset(age>15) %>% 
  dplyr::group_by(iso, year) %>% dplyr::mutate(share = pop/sum(pop)) 
qamys_country<-qamys_country[c("iso", "country", "year", "age", "sex", "educ", "score", "share")]
qamys_country$score_weighted<-qamys_country$share*qamys_country$score
qamys_country<-aggregate(score_weighted~iso+year, FUN=sum, data=qamys_country)

OECD_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv")
OECD_avg<-subset(OECD_avg, age==0 & sex==0 & educ==0)
qamys_country<-merge(OECD_avg, qamys_country)
qamys_country$adj_factor<-qamys_country$score_weighted/qamys_country$weighted_mean

wic_mys<-read.csv("OECD-average/QAMYS/WIC_mys.csv")
wic_mys<-subset(wic_mys, age==0 & sex==0 & educ==0)

qamys_country<-merge(wic_mys, qamys_country, by=c("iso", "year", "sex"))
qamys_country$qamys<-qamys_country$adj_factor*qamys_country$mys
qamys_country$educ<-0
qamys_country$age<-0
qamys_country$sex<-0
qamys_country<-qamys_country[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "qamys")]

#-------------------------------------------------------------------------------------------------------------------------------

qamys_by_country_year_age_sex_educ<-rbind(qamys_age_sex_educ, qamys_age_sex, qamys_age, qamys_sex, qamys_country)

qamys2015$year<-2015
qamys2015<-qamys2015[c("iso", "country", "year", "age", "sex", "educ", "mys", "adj_factor", "qamys")]

qamys_1970_2015<-rbind(qamys2015, qamys_by_country_year_age_sex_educ)
write.csv(qamys_1970_2015, "Cohort Analysis/qamys-reconstruction/qamys_1970-2015.csv", row.names=FALSE)

qamys_plot<-subset(qamys_1970_2015, age==0 & sex==0 & educ==0)
pdf("Cohort Analysis/qamys-reconstruction/QAMYS_time-series.pdf")
for(i in 1:4){
  print(ggplot(qamys_plot, aes(x=year, y=qamys))+ geom_col() + geom_point(aes(x=year, y=mys))+facet_wrap_paginate(~country, nrow=4, ncol=3, page=i))
}
dev.off()



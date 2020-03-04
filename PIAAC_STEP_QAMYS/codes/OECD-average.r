setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC")          #DROPBOX VID
#setwd("C:/Users/reiter/OneDrive - IIASA/PIAAC")          #ONEDRIVE IIASA
#setwd("/Users/claudiareiter/OneDrive - IIASA/PIAAC")     #ONEDRIVE LAPTOP

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

rm(list = ls())
#-----------------------------------------------------------------------------------------------------------
#LITERACY
#-----------------------------------------------------------------------------------------------------------
#Step 1: calculate PIAAC literacy mean score by age, sex, educ
piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data

piaac<-dplyr::mutate(piaac_all_countries, educ = ifelse (EDCAT7==1, 1, 0))  #educ 1 = primary or less
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==2, 2, educ))             #educ 2 = lower secondary
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==3, 3, educ))             #educ 3 = upper secondary
piaac<-dplyr::mutate(piaac, educ = ifelse (EDCAT7==4 | EDCAT7==5 | EDCAT7==6 | EDCAT7==7 | EDCAT7==8, 4, educ))     #educ 4 = post-secondary
piaac<-subset(piaac, educ>0)  #remove observations without educ information 
piaac<-subset(piaac, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)          #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#add STEP countries
step_lit_mean<-read.csv("STEP/Data/stata/STATA Results/mean_lit_age_sex_educ.csv")
step_lit_mean$iso<-as.factor(step_lit_mean$iso)
lit_mean<-rbind(lit_mean_piaac, step_lit_mean)
lit_mean$iso<-as.numeric(as.character(lit_mean$iso))

#-----------------------------------------------------------------------------------------------------------
#Step 2: calculate pop-weighted oecd average
wic_pop<-read.csv("OECD-average/wic_pop.csv")     #read WIC population
#keep only OECD countries
OECD_pop<-subset(wic_pop, iso==36 | iso==40 | iso==56 | iso==124 | iso==152 | iso==203 | iso==208 | iso==233  | iso==246 | iso==250 | iso==276 | iso==300 | iso==348 | iso==352 | iso==372 | iso==376 | iso==380 | iso==392 | iso==428 | iso==440 | iso==442 | iso==484 | iso==528 | iso==554 | iso==578 | iso==616 | iso==620  | iso==410 | iso==703 | iso==705 | iso==724 | iso==752 | iso==756 | iso==792 | iso==826 | iso==840)
OECD_pop<-subset(OECD_pop, iso!=352)  #remove Iceland (OECD country without PIAAC data)
OECD_pop<-subset(OECD_pop, iso!=428)  #remove Latvia (OECD country without PIAAC data)
OECD_pop<-subset(OECD_pop, iso!=442)  #remove Luxembourg (OECD country without PIAAC data)    
OECD_pop<-subset(OECD_pop, iso!=620)  #remove Portugal (OECD country without PIAAC data)       
OECD_pop<-subset(OECD_pop, iso!=756)  #remove Switzerland (OECD country without PIAAC data)  
OECD_pop<-subset(OECD_pop, iso!=36)   #remove Australia (OECD country without PIAAC data)       
OECD_pop<-subset(OECD_pop, year==2015) #use 2015 population
OECD_pop<-subset(OECD_pop, age>0)      #remove age groups 0-14
OECD_pop<-subset(OECD_pop, age<11)     #remove age groups 65+
OECD_pop<-subset(OECD_pop, educ!=99)   #remove educ category "Total"
OECD_pop<-subset(OECD_pop, educ<5)     #remove post-secondary sub-categories
OECD_pop<-dplyr::mutate(OECD_pop, educ = ifelse (educ==-1 | educ==0, 1, educ))
OECD_pop<-aggregate(pop ~ age+sex+educ+iso, FUN=sum, data=OECD_pop)   #sum up education categories with educ=1 (no educ, incomplete primary, primary)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("age", "sex", "educ", "iso"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
write.csv(lit_mean_OECD_weighted, "OECD-average/Output/weighted_OECD_average_lit.csv", row.names=FALSE)

#ggplot(lit_mean_OECD_weighted, aes(x=age, y=weighted_mean, colour=educ)) + geom_point() + geom_line() + facet_wrap (~sex)

#-----------------------------------------------------------------------------------------------------------
#Step 3: calculate share below and above OECD average

#use age-sex-educ-specific cutoffs for piaac.ben.pv function
#piaac_loop<-plyr::rename(piaac, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso"))
#for (i in 1:10)
   #{for (j in 1:2)
     #{for (k in 1:4)
       #{loop<-subset(piaac_loop, age==i)
        #loop<-subset(loop, sex==j)
        #loop<-subset(loop, educ==k)
        #oecd_average<-subset(lit_mean_OECD_weighted, age==i)
        #oecd_average<-subset(oecd_average,sex==j)
        #oecd_average<-subset(oecd_average,educ==k)
        #loop<-piaac.ben.pv(pvlabel="LIT", by=c("iso", "age", "sex", "educ"), loop, cutoff=c(0,oecd_average[1,5]))
        #write.csv(loop, file = paste("OECD-average/loop/lit/loop_",i,j,k, ".csv"), row.names=FALSE)
        #}}}

dir<-("OECD-average/loop/lit")
filePaths <- list.files(dir, "\\.csv$", full.names = TRUE)
lit_results <- do.call(rbind, lapply(filePaths, read.csv))
lit_results<-subset(lit_results, Benchmarks!="Below/equal to 0")
lit_results_benchmark<-lit_results$Benchmarks
lit_results_benchmark<-gsub("\\d+", "OECD average", lit_results_benchmark)
lit_results_benchmark<-gsub("greater than OECD average to less/equal than OECD average.OECD average", "less/equal than OECD average", lit_results_benchmark)
lit_results_benchmark<-gsub("Above OECD average.OECD average", "above OECD average", lit_results_benchmark)
lit_results$Benchmarks<-lit_results_benchmark
lit_results<-subset(lit_results, Benchmarks!="Above NA")
lit_results<-subset(lit_results, Benchmarks!="greater than OECD average to less/equal than NA")
lit_results_wide<- tidyr::spread(lit_results, key = Benchmarks , value = Percentage)
lit_results_wide$sum<-lit_results_wide$`above OECD average`+lit_results_wide$`less/equal than OECD average`
lit_results_wide<-plyr::rename(lit_results_wide, c("above OECD average"="above_OECD_avg", "less/equal than OECD average"="below_OECD_avg"))

step_results_wide<-read.csv("STEP/Data/stata/STATA Results/step_oecd_avg_shares.csv")
lit_results_wide<-rbind(lit_results_wide, step_results_wide)

piaac_final_lit<-merge(lit_results_wide, lit_mean_OECD_weighted, by=c("age", "sex", "educ"))
piaac_final_lit<-plyr::rename(piaac_final_lit, c("Freq"="n_OECD"))
piaac_final_lit<-merge(piaac_final_lit, lit_mean, by=c("iso", "age", "sex", "educ"))
piaac_final_lit<-plyr::rename(piaac_final_lit, c("Freq"="n_country", "weighted_mean"="weighted_OECD_average", "Mean"="mean_country"))
piaac_final_lit<-piaac_final_lit[c(1,2,3,4,9,10,11,12,6,7)]
piaac_final_lit$above_OECD_avg<-piaac_final_lit$above_OECD_avg/100
piaac_final_lit$below_OECD_avg<-piaac_final_lit$below_OECD_avg/100

#-----------------------------------------------------------------------------------------------------------
#Step 4: calculate population below and above OECD average

#keep only PIAAC and STEP countries
piaac_pop<-subset(wic_pop,iso==40 | iso==51 | iso==56 | iso==68 | iso==124 | iso==152 | iso==170 | iso==196 | iso==203 | iso==208 | iso==218 |iso==233  | iso==246 | iso==250 | iso==268 | iso==276 | iso==288 | iso==300 | iso==348 | iso==372 | iso==376 | iso==380 | iso==392 | iso==398 | iso==404 | iso==410 | iso==440 | iso==484 | iso==528 | iso==554 | iso==578 | iso==604 | iso==616 | iso==643 | iso==702 | iso==703 | iso==704 | iso==705 | iso==724 | iso==752 | iso==792 | iso==804 | iso==826 | iso==840)
piaac_pop<-subset(piaac_pop, year==2015)    #use 2015 data
piaac_pop<-subset(piaac_pop, educ!=99)    #remove educ category "Total"
piaac_pop<-subset(piaac_pop, educ<5)      #remove post-secondary sub-categories
piaac_pop<-dplyr::mutate(piaac_pop, educ = ifelse (educ==-1 |educ==0, 1, educ))
piaac_pop<-aggregate(pop ~ age+sex+educ+iso, FUN=sum, data=piaac_pop)   #sum up education categories with educ=1 (no educ, incomplete primary, primary)

piaac_final_lit<-merge(piaac_final_lit, piaac_pop, by=c("iso", "age", "sex", "educ"))

piaac_final_lit$pop_below_OECD_avg<-piaac_final_lit$pop*piaac_final_lit$below_OECD_avg
piaac_final_lit$pop_above_OECD_avg<-piaac_final_lit$pop*piaac_final_lit$above_OECD_avg
piaac_final_lit<-melt(piaac_final_lit, id.vars=c("iso", "age", "sex", "educ", "n_OECD", "weighted_OECD_average", "n_country", "mean_country", "above_OECD_avg", "below_OECD_avg","pop"),measure.vars=c("pop_below_OECD_avg", "pop_above_OECD_avg"))

piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = educ)
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==1, "primary or less below OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==1, "primary or less above OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==2, "lower secondary above OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==2, "lower secondary below OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==3, "upper secondary above OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==3, "upper secondary below OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==4, "post-secondary above OECD avg", queduc))
piaac_final_lit<-dplyr::mutate(piaac_final_lit, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==4, "post-secondary below OECD avg", queduc))
piaac_final_lit<-plyr::rename(piaac_final_lit, c("pop"="educ_pop", "value"="queduc_pop"))

write.csv(piaac_final_lit, "OECD-average/Output/piaac_final_lit.csv", row.names=FALSE)


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#NUMERACY
#-----------------------------------------------------------------------------------------------------------
#Step 1: calculate PIAAC numeracy mean score by age, sex, educ
num_mean<-piaac.mean.pv(pvlabel = "NUM", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
num_mean<-plyr::rename(num_mean, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso"))

#remove non-OECD countries
num_mean_oecd<-subset(num_mean, iso!=196)          #remove Cyprus 
num_mean_oecd<-subset(num_mean_oecd, iso!=643)     #remove Russia
num_mean_oecd<-subset(num_mean_oecd, iso!=702)     #remove Singapore
num_mean_oecd<-subset(num_mean_oecd, iso!=604)     #remove Peru
num_mean_oecd<-subset(num_mean_oecd, iso!=398)     #remove Kazakhstan
num_mean_oecd<-subset(num_mean_oecd, iso!=218)     #remove Ecuador

#-----------------------------------------------------------------------------------------------------------
#Step 2: calculate pop-weighted oecd average
num_mean_OECD_weighted<-merge(num_mean_oecd, OECD_pop, by=c("age", "sex", "educ", "iso"))   #merge PIAAC data with pop data
num_mean_OECD_weighted<-subset(num_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
num_mean_OECD_weighted<-num_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
num_mean_OECD_weighted$weighted_mean<-num_mean_OECD_weighted$Mean * num_mean_OECD_weighted$share  #calculate weighted means
num_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=num_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
write.csv(num_mean_OECD_weighted, "OECD-average/Output/weighted_OECD_average_num.csv", row.names=FALSE)

#ggplot(num_mean_OECD_weighted, aes(x=age, y=weighted_mean, colour=educ)) + geom_point() + geom_line() + facet_wrap (~sex)

#-----------------------------------------------------------------------------------------------------------
#Step 3: calculate share below and above OECD average
#use age-sex-educ-specific cutoffs for piaac.ben.pv function
#for (i in 1:10)
  #{for (j in 1:2)
    #{for (k in 1:4)
#{loop<-subset(piaac_loop, age==i)
#loop<-subset(loop, sex==j)
#loop<-subset(loop, educ==k)
#oecd_average<-subset(num_mean_OECD_weighted, age==i)
#oecd_average<-subset(oecd_average,sex==j)
#oecd_average<-subset(oecd_average,educ==k)
#loop<-piaac.ben.pv(pvlabel="NUM", by=c("iso", "age", "sex", "educ"), loop, cutoff=c(0,oecd_average[1,5]))
#write.csv(loop, file = paste("OECD-average/loop/num/loop_",i,j,k, ".csv"), row.names=FALSE)
#}}}

dir<-("OECD-average/loop/num")
filePaths <- list.files(dir, "\\.csv$", full.names = TRUE)
num_results <- do.call(rbind, lapply(filePaths, read.csv))
num_results<-subset(num_results, Benchmarks!="Below/equal to 0")
num_results_benchmark<-num_results$Benchmarks
num_results_benchmark<-gsub("\\d+", "OECD average", num_results_benchmark)
num_results_benchmark<-gsub("greater than OECD average to less/equal than OECD average.OECD average", "less/equal than OECD average", num_results_benchmark)
num_results_benchmark<-gsub("Above OECD average.OECD average", "above OECD average", num_results_benchmark)
num_results$Benchmarks<-num_results_benchmark
num_results<-subset(num_results, Benchmarks!="Above NA")
num_results<-subset(num_results, Benchmarks!="greater than OECD average to less/equal than NA")
num_results_wide<- tidyr::spread(num_results, key = Benchmarks , value = Percentage)
num_results_wide$sum<-num_results_wide$`above OECD average`+num_results_wide$`less/equal than OECD average`

piaac_final_num<-merge(num_results_wide, num_mean_OECD_weighted, by=c("age", "sex", "educ"))
piaac_final_num<-plyr::rename(piaac_final_num, c("Freq"="n_OECD"))
piaac_final_num<-merge(piaac_final_num, num_mean, by=c("iso", "age", "sex", "educ"))
piaac_final_num<-plyr::rename(piaac_final_num, c("Freq"="n_country", "weighted_mean"="weighted_OECD_average", "Mean"="mean_country", "above OECD average"="above_OECD_avg", "less/equal than OECD average"="below_OECD_avg"))
piaac_final_num<-piaac_final_num[c(1,2,3,4,9,10,11,12,6,7)]
piaac_final_num$above_OECD_avg<-piaac_final_num$above_OECD_avg/100
piaac_final_num$below_OECD_avg<-piaac_final_num$below_OECD_avg/100

#-----------------------------------------------------------------------------------------------------------
#Step 4: calculate population below and above OECD average
piaac_final_num<-merge(piaac_final_num, piaac_pop, by=c("iso", "age", "sex", "educ"))
piaac_final_num$pop_below_OECD_avg<-piaac_final_num$pop*piaac_final_num$below_OECD_avg
piaac_final_num$pop_above_OECD_avg<-piaac_final_num$pop*piaac_final_num$above_OECD_avg
piaac_final_num<-melt(piaac_final_num, id.vars=c("iso", "age", "sex", "educ", "n_OECD", "weighted_OECD_average", "n_country", "mean_country", "above_OECD_avg", "below_OECD_avg","pop"),measure.vars=c("pop_below_OECD_avg", "pop_above_OECD_avg"))

piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = educ)
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==1, "primary or less below OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==1, "primary or less above OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==2, "lower secondary above OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==2, "lower secondary below OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==3, "upper secondary above OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==3, "upper secondary below OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==4, "post-secondary above OECD avg", queduc))
piaac_final_num<-dplyr::mutate(piaac_final_num, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==4, "post-secondary below OECD avg", queduc))
piaac_final_num<-plyr::rename(piaac_final_num, c("pop"="educ_pop", "value"="queduc_pop"))

write.csv(piaac_final_num, "OECD-average/Output/piaac_final_num.csv", row.names=FALSE)


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#LITERACY-NUMERACY-AVERAGE
#-----------------------------------------------------------------------------------------------------------
#Step 1: calculate PIAAC literacy-numeracy-average mean score by age, sex, educ

piaac$PVAVG1<-(piaac$PVLIT1+piaac$PVNUM1)/2
piaac$PVAVG2<-(piaac$PVLIT2+piaac$PVNUM2)/2
piaac$PVAVG3<-(piaac$PVLIT3+piaac$PVNUM3)/2
piaac$PVAVG4<-(piaac$PVLIT4+piaac$PVNUM4)/2
piaac$PVAVG5<-(piaac$PVLIT5+piaac$PVNUM5)/2
piaac$PVAVG6<-(piaac$PVLIT6+piaac$PVNUM6)/2
piaac$PVAVG7<-(piaac$PVLIT7+piaac$PVNUM7)/2
piaac$PVAVG8<-(piaac$PVLIT8+piaac$PVNUM8)/2
piaac$PVAVG9<-(piaac$PVLIT9+piaac$PVNUM9)/2
piaac$PVAVG10<-(piaac$PVLIT10+piaac$PVNUM10)/2

avg_mean<-piaac.mean.pv(pvlabel = "AVG", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ"), data=piaac)
avg_mean<-plyr::rename(avg_mean, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso"))

#remove non-OECD countries
avg_mean_oecd<-subset(avg_mean, iso!=196)          #remove Cyprus 
avg_mean_oecd<-subset(avg_mean_oecd, iso!=643)     #remove Russia
avg_mean_oecd<-subset(avg_mean_oecd, iso!=702)     #remove Singapore
avg_mean_oecd<-subset(avg_mean_oecd, iso!=604)     #remove Peru
avg_mean_oecd<-subset(avg_mean_oecd, iso!=398)     #remove Kazakhstan
avg_mean_oecd<-subset(avg_mean_oecd, iso!=218)     #remove Ecuador

#-----------------------------------------------------------------------------------------------------------
#Step 2: calculate pop-weighted oecd average
avg_mean_OECD_weighted<-merge(avg_mean_oecd, OECD_pop, by=c("age", "sex", "educ", "iso"))   #merge PIAAC data with pop data
avg_mean_OECD_weighted<-subset(avg_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
avg_mean_OECD_weighted<-avg_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
avg_mean_OECD_weighted$weighted_mean<-avg_mean_OECD_weighted$Mean * avg_mean_OECD_weighted$share  #calculate weighted means
avg_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=avg_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
write.csv(avg_mean_OECD_weighted, "OECD-average/Output/weighted_OECD_average_avg.csv", row.names=FALSE)

#ggplot(avg_mean_OECD_weighted, aes(x=age, y=weighted_mean, colour=educ)) + geom_point() + geom_line() + facet_wrap (~sex)

#-----------------------------------------------------------------------------------------------------------
#Step 3: calculate share below and above OECD average
#use age-sex-educ-specific cutoffs for piaac.ben.pv function
#   for (i in 1:10)
#     {for (j in 1:2)
#       {for (k in 1:4)
#       {loop<-subset(piaac_loop, age==i)
#       loop<-subset(loop, sex==j)
#       loop<-subset(loop, educ==k)
#       oecd_average<-subset(avg_mean_OECD_weighted, age==i)
#       oecd_average<-subset(oecd_average,sex==j)
#       oecd_average<-subset(oecd_average,educ==k)
#       loop<-piaac.ben.pv(pvlabel="AVG", by=c("iso", "age", "sex", "educ"), loop, cutoff=c(0,oecd_average[1,5]))
#       write.csv(loop, file = paste("OECD-average/loop/avg/loop_",i,j,k, ".csv"), row.names=FALSE)
#         }}}

dir<-("OECD-average/loop/avg")
filePaths <- list.files(dir, "\\.csv$", full.names = TRUE)
avg_results <- do.call(rbind, lapply(filePaths, read.csv))
avg_results<-subset(avg_results, Benchmarks!="Below/equal to 0")
avg_results_benchmark<-avg_results$Benchmarks
avg_results_benchmark<-gsub("\\d+", "OECD average", avg_results_benchmark)
avg_results_benchmark<-gsub("greater than OECD average to less/equal than OECD average.OECD average", "less/equal than OECD average", avg_results_benchmark)
avg_results_benchmark<-gsub("Above OECD average.OECD average", "above OECD average", avg_results_benchmark)
avg_results$Benchmarks<-avg_results_benchmark
avg_results<-subset(avg_results, Benchmarks!="Above NA")
avg_results<-subset(avg_results, Benchmarks!="greater than OECD average to less/equal than NA")
avg_results_wide<- tidyr::spread(avg_results, key = Benchmarks , value = Percentage)
avg_results_wide$sum<-avg_results_wide$`above OECD average`+avg_results_wide$`less/equal than OECD average`

piaac_final_avg<-merge(avg_results_wide, avg_mean_OECD_weighted, by=c("age", "sex", "educ"))
piaac_final_avg<-plyr::rename(piaac_final_avg, c("Freq"="n_OECD"))
piaac_final_avg<-merge(piaac_final_avg, avg_mean, by=c("iso", "age", "sex", "educ"))
piaac_final_avg<-plyr::rename(piaac_final_avg, c("Freq"="n_country", "weighted_mean"="weighted_OECD_average", "Mean"="mean_country", "above OECD average"="above_OECD_avg", "less/equal than OECD average"="below_OECD_avg"))
piaac_final_avg<-piaac_final_avg[c(1,2,3,4,9,10,11,12,6,7)]
piaac_final_avg$above_OECD_avg<-piaac_final_avg$above_OECD_avg/100
piaac_final_avg$below_OECD_avg<-piaac_final_avg$below_OECD_avg/100

#-----------------------------------------------------------------------------------------------------------
#Step 4: calculate population below and above OECD average
piaac_final_avg<-merge(piaac_final_avg, piaac_pop, by=c("iso", "age", "sex", "educ"))

piaac_final_avg$pop_below_OECD_avg<-piaac_final_avg$pop*piaac_final_avg$below_OECD_avg
piaac_final_avg$pop_above_OECD_avg<-piaac_final_avg$pop*piaac_final_avg$above_OECD_avg
piaac_final_avg<-melt(piaac_final_avg, id.vars=c("iso", "age", "sex", "educ", "n_OECD", "weighted_OECD_average", "n_country", "mean_country", "above_OECD_avg", "below_OECD_avg","pop"),measure.vars=c("pop_below_OECD_avg", "pop_above_OECD_avg"))

piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = educ)
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==1, "primary or less below OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==1, "primary or less above OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==2, "lower secondary above OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==2, "lower secondary below OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==3, "upper secondary above OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==3, "upper secondary below OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_above_OECD_avg" & educ==4, "post-secondary above OECD avg", queduc))
piaac_final_avg<-dplyr::mutate(piaac_final_avg, queduc = ifelse(variable=="pop_below_OECD_avg" & educ==4, "post-secondary below OECD avg", queduc))
piaac_final_avg<-plyr::rename(piaac_final_avg, c("pop"="educ_pop", "value"="queduc_pop"))

write.csv(piaac_final_avg, "OECD-average/Output/piaac_final_avg.csv", row.names=FALSE)


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#POPULATION PYRAMIDS

#Literacy & Numeracy
#piaac_final_lit<-read.csv("OECD-average/piaac_final_lit.csv")
#piaac_final_num<-read.csv("OECD-average/piaac_final_num.csv")

#adding population below 15 and above 65
pop_below_15_above65<-subset(piaac_pop, age<1 | age>10)
pop_below_15_above65$n_OECD<-"N"
pop_below_15_above65$weighted_OECD_average<-"N"
pop_below_15_above65$n_country<-"N"
pop_below_15_above65$mean_country<-"N"
pop_below_15_above65$variable<-"N"
pop_below_15_above65<-plyr::rename(pop_below_15_above65, c("pop"="educ_pop"))
pop_below_15_above65$queduc_pop<-pop_below_15_above65$educ_pop
pop_below_15_above65<-dplyr::mutate(pop_below_15_above65, queduc = ifelse(age<1, "below 15", "above 65"))
pop_below_15_above65$above_OECD_avg<-1
pop_below_15_above65$below_OECD_avg<-0

piaac_final_lit_pyramids<-rbind(piaac_final_lit, pop_below_15_above65)
piaac_final_lit_pyramids$assessment<-"Literacy"

piaac_final_num_pyramids<-rbind(piaac_final_num, pop_below_15_above65)
piaac_final_num_pyramids$assessment<-"Numeracy"

#remove STEP countries
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=51)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=170)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=288)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=404)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=804)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=704)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=68)
piaac_final_num_pyramids<-subset(piaac_final_num_pyramids, iso!=268)

#combining literacy and numeracy data
piaac_final_pyramids<-rbind(piaac_final_lit_pyramids, piaac_final_num_pyramids)
piaac_final_pyramids$queduc<-factor(piaac_final_pyramids$queduc, level=c("above 65", "post-secondary above OECD avg", "post-secondary below OECD avg","upper secondary above OECD avg","upper secondary below OECD avg","lower secondary above OECD avg","lower secondary below OECD avg","primary or less above OECD avg","primary or less below OECD avg","below 15"))
isono<-read.csv("OECD-average/isono.csv")
piaac_final_pyramids<-merge(piaac_final_pyramids, isono, by=c("iso"))
piaac_final_pyramids$age<-piaac_final_pyramids$age+2
piaac_final_pyramids$age<-piaac_final_pyramids$age*5

#create empty numeracydata for STEP countries
step_numeracy<-subset(piaac_final_pyramids, iso==51 | iso==170 | iso==288 | iso==404 | iso==804 | iso==704 | iso==68 | iso==268)
step_numeracy$assessment<-"Numeracy"
step_numeracy$queduc_pop<-0
piaac_final_pyramids<-rbind(piaac_final_pyramids, step_numeracy)

write.csv(piaac_final_pyramids, "OECD-average/Output/piaac_final_pyramids.csv", row.names=FALSE)

pdf("OECD-average/Output/pyramids_lit_num.pdf", paper="a4r", width = 12, height = 5.8)
for(i in 1:44){
  print (ggplot(piaac_final_pyramids, aes(x=age, y=queduc_pop, fill=queduc)) + geom_bar(data = piaac_final_pyramids %>% filter(sex==2), stat = "identity") + geom_bar(data = piaac_final_pyramids %>% filter(sex == 1), stat = "identity", mapping = aes(y = -queduc_pop)) + coord_flip() + scale_fill_manual(values=c("#969696","#006d2c","#74c476","#08306b","#2171b5", "#fd8d3c", "#fed976", "#cb181d", "#fc9272", "#cccccc")) + theme_bw() + facet_wrap_paginate (~ country + assessment, ncol=2, nrow=1, page=i, scales="free") + scale_y_continuous(labels=abs))
}
dev.off()

#-----------------------------------------------------------------------------------------------------------
#Literacy pyramids including STEP
piaac_final_pyramids_lit<-subset(piaac_final_pyramids, assessment=="Literacy")

pdf("OECD-average/Output/pyramids_lit.pdf", paper="a4r", width = 11, height = 8)
for(i in 1:44){
  print (ggplot(piaac_final_pyramids_lit, aes(x=age, y=queduc_pop, fill=queduc)) + geom_bar(data = piaac_final_pyramids_lit %>% filter(sex==2), stat = "identity") + geom_bar(data = piaac_final_pyramids_lit %>% filter(sex == 1), stat = "identity", mapping = aes(y = -queduc_pop)) + coord_flip() + scale_fill_manual(values=c("#969696","#006d2c","#74c476","#08306b","#2171b5", "#fd8d3c", "#fed976", "#cb181d", "#fc9272", "#cccccc")) + theme_bw() + facet_wrap_paginate (~ country, ncol=1, nrow=1, page=i, scales="free") + scale_y_continuous(labels=abs))
}
dev.off()

#-----------------------------------------------------------------------------------------------------------
#Literacy & Numeracy Average
piaac_final_avg_pyramids<-rbind(piaac_final_avg, pop_below_15_above65)
piaac_final_avg_pyramids$queduc<-factor(piaac_final_avg_pyramids$queduc, level=c("above 65", "post-secondary above OECD avg", "post-secondary below OECD avg","upper secondary above OECD avg","upper secondary below OECD avg","lower secondary above OECD avg","lower secondary below OECD avg","primary or less above OECD avg","primary or less below OECD avg","below 15"))
piaac_final_avg_pyramids<-merge(piaac_final_avg_pyramids, isono, by=c("iso"))
piaac_final_avg_pyramids$age<-piaac_final_avg_pyramids$age+2
piaac_final_avg_pyramids$age<-piaac_final_avg_pyramids$age*5

#remove STEP countries
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=51)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=170)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=288)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=404)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=804)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=704)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=68)
piaac_final_avg_pyramids<-subset(piaac_final_avg_pyramids, iso!=268)

write.csv(piaac_final_avg_pyramids, "OECD-average/Output/piaac_final_pyramids_lit-num-avg.csv", row.names=FALSE)

pdf("OECD-average/Output/pyramids_lit-num-avg.pdf", paper="a4r", width = 11, height = 8)
for(i in 1:36){
  print (ggplot(piaac_final_avg_pyramids, aes(x=age, y=queduc_pop, fill=queduc)) + geom_bar(data = piaac_final_avg_pyramids %>% filter(sex==2), stat = "identity") + geom_bar(data = piaac_final_avg_pyramids %>% filter(sex == 1), stat = "identity", mapping = aes(y = -queduc_pop)) + coord_flip() + scale_fill_manual(values=c("#969696","#006d2c","#74c476","#08306b","#2171b5", "#fd8d3c", "#fed976", "#cb181d", "#fc9272", "#cccccc")) + theme_bw() + facet_wrap_paginate (~ country, ncol=1, nrow=1, page=i, scales="free") + scale_y_continuous(labels=abs))
}
dev.off()


#calculate OECD pop for all age-sex-educ combinations (including total)
wic_pop<-read.csv("OECD-average/wic_pop.csv")     #read WIC population
#keep only OECD countries
OECD_pop<-subset(wic_pop, iso==36 | iso==40 | iso==56 | iso==124 | iso==152 | iso==203 | iso==208 | iso==233  | iso==246 | iso==250 | iso==276 | iso==300 | iso==348 | iso==352 | iso==372 | iso==376 | iso==380 | iso==392 | iso==428 | iso==440 | iso==442 | iso==484 | iso==528 | iso==554 | iso==578 | iso==616 | iso==620  | iso==410 | iso==703 | iso==705 | iso==724 | iso==752 | iso==756 | iso==792 | iso==826 | iso==840)
OECD_pop<-subset(OECD_pop, year==2015) #use 2015 population
OECD_pop<-subset(OECD_pop, age>0)      #remove age groups 0-14
OECD_pop<-subset(OECD_pop, age<11)     #remove age groups 65+
OECD_pop<-subset(OECD_pop, educ!=99)   #remove educ category "Total"
OECD_pop<-subset(OECD_pop, educ<5)     #remove post-secondary sub-categories
OECD_pop<-dplyr::mutate(OECD_pop, educ = ifelse (educ==-1 | educ==0, 1, educ))
OECD_pop<-aggregate(pop ~ age+sex+educ+iso, FUN=sum, data=OECD_pop)   #sum up education categories with educ=1 (no educ, incomplete primary, primary)
OECD_pop<-subset(OECD_pop, educ>0)  

#calculate population age 20-64
OECD_pop_2064<-subset(OECD_pop, age>1)  
OECD_pop_2064<-aggregate(pop ~ sex+educ+iso, FUN=sum, data=OECD_pop_2064)
OECD_pop_2064$age<-0
OECD_pop<-rbind(OECD_pop, OECD_pop_2064)

#calculate population both sex
OECD_pop_both_sex<-aggregate(pop ~ age+educ+iso, FUN=sum, data=OECD_pop)
OECD_pop_both_sex$sex<-0
OECD_pop<-rbind(OECD_pop, OECD_pop_both_sex)

#calculate population all educ
OECD_pop_all_educ<-aggregate(pop ~ age+sex+iso, FUN=sum, data=OECD_pop)
OECD_pop_all_educ$educ<-0
OECD_pop<-rbind(OECD_pop, OECD_pop_all_educ)

write.csv(OECD_pop, "OECD-average/oecd_pop_final.csv", row.names=FALSE)




#----------------------------------------------------------------------------------------
#weighted OECD average all age-sex-educ-combinations (including total)

setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC")        #DROPBOX VID
setwd("C:/Users/reiter/OneDrive - IIASA/PIAAC")          #ONEDRIVE IIASA
#setwd("/Users/claudiareiter/OneDrive - IIASA/PIAAC")     #ONEDRIVE LAPTOP


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

rm(list = ls())


#-----------------------------------------------------------------------------------------------------------
#weighted OECD average by country, age, sex, educ
#Step 1: calculate PIAAC literacy mean score
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

library(zoo)
lit_mean_piaac<-lit_mean_piaac[with(lit_mean_piaac, order(iso, age, sex, educ)),]

lit_mean_piaac_educ234<-lit_mean_piaac %>% subset(educ!=1) %>%
  group_by(iso, age, sex, educ) %>% 
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=FALSE))%>%
  ungroup()

lit_mean_piaac_educ123<-lit_mean_piaac %>% subset(educ!=4) %>%
  group_by(iso, age, sex, educ) %>% 
  mutate(mean_country = na.locf(mean_country, na.rm=TRUE, fromLast=TRUE))%>%
  ungroup()

lit_mean_piaac_educ1<-subset(lit_mean_piaac_educ123, educ==1)
lit_mean_piaac_educ2<-subset(lit_mean_piaac_educ123, educ==2)
lit_mean_piaac_educ3<-subset(lit_mean_piaac_educ234, educ==3)
lit_mean_piaac_educ4<-subset(lit_mean_piaac_educ234, educ==4)
lit_mean_piaac<-rbind(lit_mean_piaac_educ1, lit_mean_piaac_educ2, lit_mean_piaac_educ3, lit_mean_piaac_educ4)

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador


#Step 2: calculate pop-weighted oecd average
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age>0 & sex>0 & educ>0)
OECD_pop$age<-OECD_pop$age*5+10

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso", "age", "sex", "educ"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, mean_country="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$mean_country * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(n_country, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
write.csv(lit_mean_OECD_weighted,"OECD-average/PIAAC-lit_OECD-avg-age-sex-educ_old.csv", row.names=FALSE)


#-----------------------------------------------------------------------------------------------------------
#weighted OECD average by country
#Step 1: calculate PIAAC literacy mean score
#piaac_all_countries<-read.csv("Data/CSV/piaac_all-countries.csv")   #read PIAAC data
piaac<-subset(piaac_all_countries, AGEG5LFS>1)

lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador


#Step 2: calculate pop-weighted oecd average
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age==0 & sex==0 & educ==0)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
lit_mean_OECD_weighted<-plyr::rename(lit_mean_OECD_weighted, c("Freq"="n"))

weighted_oecd_avg<-read.csv("OECD-average/PIAAC-lit_OECD-avg-age-sex-educ_old.csv")
weighted_oecd_avg<-rename(weighted_oecd_avg, c("n_country"="n"))
weighted_oecd_avg<-rbind(weighted_oecd_avg, lit_mean_OECD_weighted)

#-----------------------------------------------------------------------------------------------------------
#weighted OECD average by country and sex only
#STEP1
piaac<-subset(piaac_all_countries, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
piaac<-subset(piaac, AGEG5LFS>1)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "GENDER_R"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "GENDER_R"="sex"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#STEP2
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age==0 & educ==0 & sex>0)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso", "sex"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
lit_mean_OECD_weighted<-plyr::rename(lit_mean_OECD_weighted, c("Freq"="n"))

weighted_oecd_avg<-rbind(weighted_oecd_avg, lit_mean_OECD_weighted)


#-----------------------------------------------------------------------------------------------------------
#weighted OECD average by country and age only
#STEP1
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "AGEG5LFS"), data=piaac_all_countries)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "AGEG5LFS"="age"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)          #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#STEP2
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age>0 & educ==0 & sex==0)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso", "age"))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
lit_mean_OECD_weighted<-plyr::rename(lit_mean_OECD_weighted, c("Freq"="n"))
lit_mean_OECD_weighted$age<-as.numeric(as.character(lit_mean_OECD_weighted$age))
lit_mean_OECD_weighted$age<-(lit_mean_OECD_weighted$age*5)+10

weighted_oecd_avg<-rbind(weighted_oecd_avg, lit_mean_OECD_weighted)

#-----------------------------------------------------------------------------------------------------------
#by country, age, and sex
#STEP1
piaac<-subset(piaac_all_countries, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "GENDER_R", "AGEG5LFS"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "GENDER_R"="sex", "AGEG5LFS"="age"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#STEP2
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age>0 & educ==0 & sex>0)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso", "age", "sex" ))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ age+sex+educ, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
lit_mean_OECD_weighted<-plyr::rename(lit_mean_OECD_weighted, c("Freq"="n"))
lit_mean_OECD_weighted$age<-as.numeric(as.character(lit_mean_OECD_weighted$age))
lit_mean_OECD_weighted$age<-(lit_mean_OECD_weighted$age*5)+10

weighted_oecd_avg<-rbind(weighted_oecd_avg, lit_mean_OECD_weighted)
write.csv(weighted_oecd_avg,"OECD-average/PIAAC-lit_OECD-avg-age-sex-educ.csv", row.names = FALSE)



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#OECD average HLO
hlo<-read.csv("OECD-average/hlo.csv")
#calculate HLO OECD average
hlo_oecd<-subset(hlo, year==2015)
hlo_oecd<-subset(hlo_oecd, iso==36 | iso==40 | iso==56 | iso==124 | iso==152 | iso==203 | iso==208 | iso==233  | iso==246 | iso==250 | iso==276 | iso==300 | iso==348 | iso==352 | iso==372 | iso==376 | iso==380 | iso==392 | iso==428 | iso==440 | iso==442 | iso==484 | iso==528 | iso==554 | iso==578 | iso==616 | iso==620  | iso==410 | iso==703 | iso==705 | iso==724 | iso==752 | iso==756 | iso==792 | iso==826 | iso==840)
hlo_oecd<-aggregate(HLO ~ country+iso, FUN=mean, data=hlo_oecd)
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age==1 & educ==0 & sex==0)
hlo_oecd_avg<-merge(hlo_oecd, OECD_pop, by=c("iso"))
hlo_oecd_avg$age<-as.numeric(as.character(hlo_oecd_avg$age))
hlo_oecd_avg$age<-(hlo_oecd_avg$age*5)+10
hlo_oecd_avg<-hlo_oecd_avg %>% 
  dplyr::group_by(age, sex, educ) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
hlo_oecd_avg$weighted_mean_hlo<-hlo_oecd_avg$HLO * hlo_oecd_avg$share  #calculate weighted means
hlo_oecd_avg<-aggregate(cbind(weighted_mean_hlo) ~ age+sex+educ, FUN=sum, data=hlo_oecd_avg)  #calculate weighted average by age, sex, educ
View(hlo_oecd_avg)
write.csv(hlo_oecd_avg, "OECD-average/hlo_weighted_OECD-avg.csv")



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#OECD average pop15-49 by sex
#by country, age, and sex
#STEP1
piaac<-subset(piaac_all_countries, GENDER_R==1 | GENDER_R==2)   #remove observations without gender information 
piaac<-subset(piaac, AGEG5LFS<8)
lit_mean_piaac<-piaac.mean.pv(pvlabel = "LIT", by=c("CNTRYID", "GENDER_R"), data=piaac)
lit_mean_piaac<-plyr::rename(lit_mean_piaac, c("CNTRYID"="iso", "GENDER_R"="sex"))

#remove non-OECD countries
lit_mean_oecd<-subset(lit_mean_piaac, iso!=196)    #remove Cyprus 
lit_mean_oecd<-subset(lit_mean_oecd, iso!=643)     #remove Russia
lit_mean_oecd<-subset(lit_mean_oecd, iso!=702)     #remove Singapore
lit_mean_oecd<-subset(lit_mean_oecd, iso!=604)     #remove Peru
lit_mean_oecd<-subset(lit_mean_oecd, iso!=398)     #remove Kazakhstan
lit_mean_oecd<-subset(lit_mean_oecd, iso!=218)     #remove Ecuador

#STEP2
OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age>0 & educ==0 & sex>0)
OECD_pop<-aggregate(pop~iso+sex, FUN=sum, data=OECD_pop)

lit_mean_OECD_weighted<-merge(lit_mean_oecd, OECD_pop, by=c("iso", "sex" ))   #merge PIAAC data with pop data
lit_mean_OECD_weighted<-subset(lit_mean_OECD_weighted, Mean!="NA")  #remove country-age-sex-combinatins with too less observations
lit_mean_OECD_weighted<-lit_mean_OECD_weighted %>% 
  dplyr::group_by(sex) %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
lit_mean_OECD_weighted$weighted_mean<-lit_mean_OECD_weighted$Mean * lit_mean_OECD_weighted$share  #calculate weighted means
lit_mean_OECD_weighted<-aggregate(cbind(Freq, weighted_mean) ~ sex, FUN=sum, data=lit_mean_OECD_weighted)  #calculate weighted average by age, sex, educ
lit_mean_OECD_weighted<-plyr::rename(lit_mean_OECD_weighted, c("Freq"="n"))




#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#OECD average literate population (for DHS)
literacy<-read.csv("M:/EduQuality/Dilek-Literacy/df_illiterate_prop_cc.csv")
literacy$literate_prop<-100-literacy$illiterate_prop
literacy_OECD<-subset(literacy, iso==36 | iso==40 | iso==56 | iso==124 | iso==152 | iso==203 | iso==208 | iso==233  | iso==246 | iso==250 | iso==276 | iso==300 | iso==348 | iso==352 | iso==372 | iso==376 | iso==380 | iso==392 | iso==428 | iso==440 | iso==442 | iso==484 | iso==528 | iso==554 | iso==578 | iso==616 | iso==620  | iso==410 | iso==703 | iso==705 | iso==724 | iso==752 | iso==756 | iso==792 | iso==826 | iso==840)

OECD_pop<-read.csv("OECD-average/oecd_pop_final.csv")     #read WIC population
OECD_pop<-subset(OECD_pop, age==0 & educ==0 & sex==0)
OECD_pop<-aggregate(pop~iso, FUN=sum, data=OECD_pop)

literacy_OECD_avg<-merge(literacy_OECD, OECD_pop, by=c("iso"))   #merge PIAAC data with pop data
literacy_OECD_avg<-literacy_OECD_avg %>% dplyr::mutate(share = pop/sum(pop))   #calculate population weights
literacy_OECD_avg$weighted_mean<-literacy_OECD_avg$literate_prop * literacy_OECD_avg$share  #calculate weighted means
literacy_OECD_avg<-sum(literacy_OECD_avg$weighted_mean)
print(literacy_OECD_avg)


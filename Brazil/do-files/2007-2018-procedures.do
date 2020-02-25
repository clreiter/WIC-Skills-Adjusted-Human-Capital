clear all

global directory "C:\Users\guimaraes\Dropbox\GitHub\WiC-EducationQualityMatters\Brazil\"

global data "$directory\data"

global results "$directory\results\stata"

global dofiles "$directory\do-files"

import excel "$data\Inaf2007_2018.xlsx", sheet("DadosInafConsolidados2011_2 (2") firstrow

label var quest "NÚMERO DO QUESTIONÁRIO"
label var ano "ANO DA ENTREVISTA"
label var reg "REGIÃO"
label var idade_real "IDADE REAL"
label var idad "IDADE AGRUPADA"
label var sexo "SEXO"
label var peso_final "PONDERAÇÃO PARA CORREÇÃO DE ANALFABETOS"
label var score_alfab_final "ESCORE DE ALFABETISMO"
label var inaf_alfab_5N "NÍVEL  DE ALFABETISMO - 5 NÍVEIS"
label var inst "ESCOLARIDADE - 10 CATEGORIAS"
label var inst_agupada "ESCOLARIDADE - 5 CATEGORIAS"

* The weight variable does not vary*/

drop peso

/* Generate relevant variables for QualMatt */

// Age

recode idade_real (15/19=1 "15-19") ///
			 (20/24=2  "20-24") ///
			 (25/29=3  "25-29") ///
			 (30/34=4  "30-34") ///
			 (35/39=5  "35-39") ///			
			 (40/44=6  "40-44") ///
			 (45/49=7  "45-49") ///
			 (50/54=8  "50-54") ///
			 (55/59=9  "55-59") ///
			 (60/64=10 "60-64"), gen(age_group)
		
// Sex

gen sex=sexo

label define sex 1 "Males" 2 "Females", modify

label values sex sex

// Education

/*

inst	Escolaridade - 10 categorias	
		1	Analfabeto									ISCED 0	Primary or less
		2	Sabe ler e escrever mas não cursou escola	ISCED 0	Primary or less
		3	Primário Incompleto							ISCED 1	Primary or less
		4	Primário Completo							ISCED 1	Primary or less
		5	Ginásio Incompleto							ISCED 2	Lower Secondary
		6	Ginásio Completo							ISCED 2	Lower Secondary
		7	Colégio Incompleto							ISCED 3	Upper Secondary
		8	Colégio Completo							ISCED 3	Upper Secondary
		9	Superior Incompleto							ISCED 3	Upper Secondary
		10	Superior Completo							ISCED 5-8 Tertiary

*/	
		 
recode inst (1 2=1 "ISCED 0") (3 4=2 "ISCED 1") (5 6=3 "ISCED 2") ///
	(7 8 9=4 "ISCED 3") (10=5 "ISCED 5-8"), gen(sch_isced)
	
recode inst (1 2 3 4=1 "Primary or less") (5 6=2 "Lower Secondary") ///
	(7 8 9=3 "Upper Secondary") (10=5 "Tertiary"), gen(schooling)	
	
gen inaf_score=score_alfab_final

/* Proficiency

InafAlfab

InafAlfab5n

1	Analfabeto
5	Rudimentar
2	Elementar
3	Intermediário
4	Proficiente


*/

gen inaf_cat=inaf_alfab_5N

label define inaf_cat 1 "Illiterate" 2 "Rudimentary" 3 "Elementary" 4 "Intermediate" 5 "Proficient"
label values inaf_cat inaf_cat

save "$data\Inaf2007_2018.dta", replace

*************************************************************

// 0 - Validation Analysis

* Freq by sex and age_group

use "$data\Inaf2007_2018.dta", clear

preserve
gen id=_n
collapse (count) id, by(ano age_group sex schooling)
sort ano sex schooling age
restore

use "$data\Inaf2007_2018.dta", clear

// 1 - Descriptive Analysis - Proficiency

/* DONT RUN - WILL REPLACE EDITED GRAPHS
twoway (histogram inaf_score) (kdensity inaf_score), by(ano)

twoway (kdensity inaf_score if ano==2007) (kdensity inaf_score if ano==2009) ///
	(kdensity inaf_score if ano==2011) (kdensity inaf_score if ano==2015) ///
		kdensity inaf_score if ano==2018), ///
		legend(order(1 "2007" 2 "2009" 3 "2011" 4 "2015" 5 "2018"))

* Excluding 2007
twoway (kdensity inaf_score if ano==2009) (kdensity inaf_score if ano==2011) ///
	(kdensity inaf_score if ano==2015) (kdensity inaf_score if ano==2018), ///
		legend(order(1 "2009" 2 "2011" 3 "2015" 4 "2018"))

// Graphs by age-group
preserve
	collapse (mean) meaninaf_score= inaf_score (sd) sdinaf_score=inaf_score, by(ano age_group sex)
	gen lb=mean-sd
	gen ub=mean+sd
		foreach sex in 1 2 {
			foreach ano in 2007 2009 2011 2015 2018 {
			twoway (rcap lb ub age_group if sex==`sex' & ano==`ano') /// 
			(scatter meaninaf_score age_group if sex==`sex' & ano==`ano', xlabel(1 "15-19" 2 "20-24" ///
				3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" ///
				9 "55-59" 10 "60-64", valuelabel))
			graph save Graph "$results\age-profile_`sex'_`ano'.gph", replace
		}
	}
restore

* Some statistical significance.

// Y as continuous (with tobit)
foreach ano in 2007 2009 2011 2015 2018 {
	tobit inaf_score i.sex i.schooling i.age_group if ano==`ano', ll
	margins age_group#sex, plot
	graph save Graph "$results\tobit_`ano'_age_sex.gph", replace
	margins age_group#schooling, plot
	graph save Graph "$results\tobit_`ano'_age_educ.gph", replace
}
*/

// Pooled cross-section analysis

*Lets try a Lexis diagram by single age groups

// Turns out that logit model only censors 1 obs, lets do OLS

use "$data\Inaf2007_2018.dta", clear

gen age=idade_real
gen cohort=ano-age

* nested models
reg inaf_score
reg inaf_score c.age
reg inaf_score c.age##c.age
reg inaf_score c.age##c.age cohort
reg inaf_score c.age##c.age cohort i.sex
reg inaf_score c.age##c.age cohort i.sex i.schooling


margins, at(age=(15 (5) 65)) plot
/* DONT RUN - WILL REPLACE EDITED GRAPHS
graph save Graph "$results\reg-predict.gph", replace
graph export "$results\reg-predict.png", as(png) replace
*/

margins schooling, at(age=(15 (5) 65)) plot
/* DONT RUN - WILL REPLACE EDITED GRAPHS
graph save Graph "$results\reg-schooling-predict.gph", replace
graph export "$results\reg-schooling-predict.png", as(png) replace
*/
margins sex, at(age=(15 (5) 60)) plot
/* DONT RUN - WILL REPLACE EDITED GRAPHS
graph save Graph "$results\reg-sex-predict.gph", replace
graph export "$results\reg-sex-predict.png", as(png) replace*/

*******************************************************************************

/* Estimating average years of schooling for population */

/* 
1. foreach age-sex-period, compute mean Inaf Score
2. compute for each age-sex-period-schooling, percentage above or below Inaf Score*/

use "$data\Inaf2007_2018.dta", clear

preserve
	collapse (mean) inaf_score, by(ano age_group sex)
	rename inaf_score mean_inaf_age_sex
	save "$data\mean_inaf_year_sex_age.dta", replace
restore

preserve
	gen id=_n
	collapse (count) id (mean) inaf_score, by(ano age_group sex schooling)
	rename id pop
	save "$data\pop_mean_inaf_year_sex_age_schooling.dta", replace
restore

use "$data\pop_mean_inaf_year_sex_age_schooling.dta", clear
merge m:1 ano age_group sex using "$data\mean_inaf_year_sex_age.dta"
drop _m
bysort ano age_group sex: egen totalpop=sum(pop)
gen posicao_inaf=1 if inaf_score> mean_inaf_age_sex
replace posicao_inaf=0 if inaf_score<= mean_inaf_age_sex
label define posicao_inaf 1 "Above the average" 0 "Equal or less than the average"
label values posicao_inaf posicao_inaf
export excel using "$directory\results\excel\Quality_Distribution.xlsx", firstrow(variables) replace

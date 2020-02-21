clear all

global directory "C:\Users\guimaraes\Dropbox\GitHub\WiC-EducationQualityMatters\Inaf\"

global data "$directory\data"

global results "$directory\results"

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

// 0 - Validation Analysis

* Freq by sex and age_group

preserve
gen id=_n
collapse (count) id, by(ano age_group sex schooling)
sort ano sex schooling age
restore

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

// Aggregate modelling

cd "$results"
collapse (median) medianinaf_score= inaf_score, by(ano age_group sex schooling)
recode ano (2007=1) (2009=2) (2011=3) (2015=4) (2018=5), gen(period)
gen cohort=10-age_group+period
*identification assumption. cohort 14=cohort 13
replace cohort=13 if cohort==14
save "$data\Inaf_IPC_Model.dta", replace

// Sex and education change levels of decay function
use "$data\Inaf_IPC_Model.dta", clear
reg medianinaf_score i.age_group i.period i.cohort i.sex i.schooling 
parmest, label saving(Coef_IPC_Inaf.dta, replace) 
use Coef_IPC_Inaf, clear
gen lb=estimate-std
gen ub=estimate+std
destring label, replace
replace label=_n
label define label 1 "15-19" ///
	2 "20-24" ///
	3 "25-29" ///
	4 "30-34" ///
	5 "35-39" ///
	6 "40-44" ///
	7 "45-49" ///
	8 "50-54" ///
	9 "55-59" ///
	10 "60-64" ///
	11 "2007" ///
	12 "2009" ///
	13 "2011" ///
	14 "2015" ///
	15 "2018" ///
	16 "1943-1947/1948-1952" ///
	17 "1953-1957" ///
	18 "1955-1959" ///
	19 "1958-1962" ///
	20 "1960-1964" ///
	21 "1965-1969" ///
	22 "1970-1974" ///
	23 "1975-1979" ///
	24 "1980-1984" ///
	25 "1985-1989" ///
	26 "1990-1994" ///
	27 "1995-1999" ///
	28 "2000-2004"
label values label label
rename estimate estimate_`s'
twoway 	(scatter estimate label if label<11, xlabel(1 "15-19" 2 "20-24" ///
			3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" ///
			9 "55-59" 10 "60-64", valuelabel)) ///
		(line estimate label if label<11)
graph save Graph "$results\ipc_age_effects.gph", replace

// Different education decay functions 
// No tertiary - different age groups
foreach schooling in 1 2 3 {
	use "$data\Inaf_IPC_Model.dta", clear
	reg medianinaf_score i.age_group i.period i.cohort i.sex if schooling==`schooling'
	parmest, label saving(Coef_IPC_Inaf_Educ_`schooling'.dta, replace) 
	use Coef_IPC_Inaf_Educ_`schooling'.dta, clear
	gen lb=estimate-std
	gen ub=estimate+std
	destring label, replace
	replace label=_n
	label define label 1 "15-19" ///
		2 "20-24" ///
		3 "25-29" ///
		4 "30-34" ///
		5 "35-39" ///
		6 "40-44" ///
		7 "45-49" ///
		8 "50-54" ///
		9 "55-59" ///
		10 "60-64" ///
		11 "2007" ///
		12 "2009" ///
		13 "2011" ///
		14 "2015" ///
		15 "2018" ///
		16 "1943-1947/1948-1952" ///
		17 "1953-1957" ///
		18 "1955-1959" ///
		19 "1958-1962" ///
		20 "1960-1964" ///
		21 "1965-1969" ///
		22 "1970-1974" ///
		23 "1975-1979" ///
		24 "1980-1984" ///
		25 "1985-1989" ///
		26 "1990-1994" ///
		27 "1995-1999" ///
		28 "2000-2004"
	label values label label
	rename estimate estimate_`schooling'
	twoway 	(scatter estimate label if label<11, xlabel(1 "15-19" 2 "20-24" ///
				3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" ///
				9 "55-59" 10 "60-64", valuelabel)) ///
			(line estimate label if label<11)
	graph save Graph "$results\ipc_age_effects_`schooling'.gph", replace
	save Coef_IPC_Inaf_Educ_`schooling'.dta, replace
}

// Tertiary
foreach schooling in 5 {
	use "$data\Inaf_IPC_Model.dta", clear
	drop if age==1
	gen age_group2=age_group-1
	drop age_group
	recode age_group2 (20/24=1  "20-24") ///
			 (25/29=2  "25-29") ///
			 (30/34=3  "30-34") ///
			 (35/39=4  "35-39") ///			
			 (40/44=5  "40-44") ///
			 (45/49=6  "45-49") ///
			 (50/54=7  "50-54") ///
			 (55/59=8  "55-59") ///
			 (60/64=9 "60-64"), gen(age_group)
	drop cohort
	gen cohort=10-age_group+period
	*identification assumption. cohort 14=cohort 13
	replace cohort=13 if cohort==14
	reg medianinaf_score i.age_group i.period i.cohort i.sex if schooling==`schooling'
	parmest, label saving(Coef_IPC_Inaf_Educ_`schooling'.dta, replace) 
	use Coef_IPC_Inaf_Educ_`schooling'.dta, clear
	gen lb=estimate-std
	gen ub=estimate+std
	destring label, replace
	replace label=_n
	label define label ///
		1 "20-24" ///
		2 "25-29" ///
		3 "30-34" ///
		4 "35-39" ///
		5 "40-44" ///
		6 "45-49" ///
		7 "50-54" ///
		8 "55-59" ///
		9 "60-64" ///
		10 "2007" ///
		11 "2009" ///
		12 "2011" ///
		13 "2015" ///
		14 "2018" ///
		15 "1943-1947/1948-1952" ///
		16 "1953-1957" ///
		17 "1955-1959" ///
		18 "1958-1962" ///
		29 "1960-1964" ///
		20 "1965-1969" ///
		21 "1970-1974" ///
		22 "1975-1979" ///
		23 "1980-1984" ///
		24 "1985-1989" ///
		25 "1990-1994" ///
		26 "1995-1999"
	label values label label
	rename estimate estimate_`schooling'
	twoway 	(scatter estimate label if label<10, xlabel(1 "20-24" ///
				2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49" 7 "50-54" ///
				8 "55-59" 9 "60-64", valuelabel)) ///
			(line estimate label if label<10)
	graph save Graph "$results\ipc_age_effects_`schooling'.gph", replace
	save Coef_IPC_Inaf_Educ_`schooling'.dta, replace
}

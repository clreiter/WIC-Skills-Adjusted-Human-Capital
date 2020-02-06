clear all

global directory "C:\Users\guimaraes\Dropbox\GitHub\WiC-EducationQualityMatters\Inaf\"

global data "$directory\data"

global results "$directory\results"

global dofiles "$directory\do-files"

import excel "$data\Inaf2018_Data.xlsx", sheet("JOB_0085_BD (Label)") firstrow

do "$dofiles\2018-var-labels.do"

* There are variables will all values missing

dropmiss, force

/*(P602 P6A P702 P7A P2807 P2808 P2809 P2810 P2811 P2812 P2813 P2814 P4905 ///
P4906 P4907 P5021 P5022 P5023 P5024 P5106 P5216 P5217 P5218 dropped)

*/

/* DATA CLEANING AND RECODING */

ds

local ordem `r(varlist)'

ds, has(type string)

foreach var of varlist `r(varlist)' {

	encode `var', gen(c_`var')
	drop `var'
	rename c_`var' `var'
}

order `ordem'

save "$data\Inaf2018.dta", replace

*******************************************************************************

use "$data\Inaf2018.dta", clear

/* Generate relevant variables for QualMatt */

// Age

recode Idade (15/19=1 "15-19") ///
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

gen sex=Sexo

label define sex 1 "Females" 2 "Males", modify

label values sex sex

// Education

/*
instru	7	NENHUMA						ISCED 0		Primary or less	
		6	ATÉ 3ª SÉRIE DO FUND.		ISCED 1		Primary or less
		3	4ª SÉRIE DO FUND.			ISCED 1		Primary or less
		4	5ª A 7ª SÉRIE DO FUND.		ISCED 2		Lower Secondary	
		5	8ª SÉRIE DO FUND.			ISCED 2		Lower Secondary	
		1	1º AO 2º ANO DO ENS. MÉDIO	ISCED 3		Upper Secondary
		2	3º ANO DO ENS. MÉDIO		ISCED 3		Upper Secondary
		8	SUPERIOR INC.				ISCED 3		Upper Secondary
		9	SUPERIOR COMP.				ISCED 5-8	Tertiary



*/	

label drop c_instru
label drop c_instcr

recode instru (7=1) (6=2) (3=3) (4=4) (5=5) (1=6) (2=7) (9=8) (8=9)

recode instcr (4=1) (2=2) (1=3) (3=4) (5=5)

label define instru 1 "NENHUMA" 2 "ATÉ 3ª SÉRIE DO FUND." ///
	3 "4ª SÉRIE DO FUND." 4 "5ª A 7ª SÉRIE DO FUND." ///
	5 "8ª SÉRIE DO FUND." 6 "1º AO 2º ANO DO ENS. MÉDIO" ///
	7 "3º ANO DO ENS. MÉDIO" 8 "SUPERIOR INC." ///
	9 "SUPERIOR COMP.", modify

label define instcr 1 "NENHUMA" 2 "ATÉ 4ª SÉRIE DO FUND." ///
	3 "5ª A 8ª SÉRIE DO FUND." 4 "ENS. MÉDIO" ///
	5 "SUPERIOR COMP. OU INCOMPL.", modify
	
label values instru instru

label values instcr instcr

					 
recode instru (1=1 "ISCED 0") (2 3=2 "ISCED 1") (4 5=3 "ISCED 2") ///
	(6 7 8=4 "ISCED 3") (9=5 "ISCED 5-8"), gen(sch_isced)
	
recode instru (1 2 3=1 "Primary or less") (4 5=2 "Lower Secondary") ///
	(6 7 8=3 "Upper Secondary") (9=5 "Tertiary"), gen(schooling)	
	
	
/* Proficiency

InafAlfab

1	Analfabeto
4	Rudimentar
2	Básico
3	Pleno

InafAlfab5n

1	Analfabeto
5	Rudimentar
2	Elementar
3	Intermediário
4	Proficiente


*/

tab InafAlfab
tab InafAlfab5n

label drop c_InafAlfab c_InafAlfab5n

recode InafAlfab (1=1) (4=2) (2=3) (3=4)
recode InafAlfab5n (1=1) (5=2) (2=3) (3=4) (4=5)

label define InafAlfab 1 "Illiterate" 2 "Rudimentary" 3 "Basic" 4 "Full Literacy"
label values InafAlfab InafAlfab

label define InafAlfab5n 1 "Illiterate" 2 "Rudimentary" 3 "Elementary" 4 "Intermediate" 5 "Proficient"
label values InafAlfab5n InafAlfab5n

tab InafAlfab
tab InafAlfab5n

// 0 - Validation Analysis

* Freq by sex and age_group


// 1 - Descriptive Analysis - Proficiency

histogram ProfAlfab, freq
tab InafAlfab, sum(ProfAlfab)
twoway (kdensity ProfAlfab if sex==1, lpattern(dash)) (kdensity ProfAlfab if sex==2)
graph box ProfAlfab, over(sex) over(schooling)

preserve
	collapse (mean) meanProfAlfab= ProfAlfab /// 
	(sd) sdProfAlfab=ProfAlfab, by(age_group)
	gen lb=mean-sd
	gen ub=mean+sd
	twoway (rcap lb ub age_group) /// 
	(scatter meanProfAlfab age_group, xlabel(1 "15-19" 2 "20-24" ///
	 3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" ///
	 9 "55-59" 10 "60-64", valuelabel))
	graph save Graph "$results\age-profile.gph", replace
restore


tab InafAlfab sex, sum(ProfAlfab)
tab InafAlfab schooling, sum(ProfAlfab)
tab age_group InafAlfab, sum(ProfAlfab)


* Some statistical significance.

ologit InafAlfab i.sex i.schooling i.age_group


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

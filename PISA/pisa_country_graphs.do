label define sexl 1 "Female" 2 "Male" 
label values sex sexl
sencode country, gen(cnt)
recode math se_math read se_read scie se_scie num_lit se_num_lit number_below_oecd number_above_oecd total_number prop_below_oecd prop_above_oecd (9999=.)
gen female=prop_above_oecd if sex==1
gen male=-prop_above_oecd if sex==2
gen zero=0
twoway bar male year, horizontal barwidth(3) xlabel(-1(0.2)1)  || bar female year, horizontal barwidth(3) xlabel(-1(0.2)1) || sc year zero, mlabel(year) mlabcolor(black) msymbol(1)||, plotregion(style(none)) ysca(noline) ylabel(none) xsca(noline) ///
		legend(label(1 Males) label(2 Females)) legend(order(1 2)) by(cnt, note (""))
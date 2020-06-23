/*define the corresponding path in your computer:
global path "C:\Users\enter path" */
import delimited $path\EDULIT_DS_27022020084208646.csv, encoding(UTF-8) /* adult literacy data from UIS web page*/
gen illiterate=100-value /*calculating illiteracy rate*/
kountry location, from(iso3c) to(iso3n) /*iso country codes to variable _ISO3N_*/
encode indicator, gen(gender) /*recoding gender and total as numeric*/
label define gender 1 "Both Sexes", modify /*relabelling for gender*/
label define gender 2 "Female", modify
label define gender 3 "Male", modify
keep time illiterate _ISO3N_ gender
sort gender time
egen year_sex=group(gender time), label
drop time gender
reshape wide illiterate, i(_ISO3N_) j(year_sex) /*long-to-wide formatting for seperate variables for gender groups*/
/*calculating 5 year averages for both sexes and total*/
egen tot_ill_1970_74=rmean(illiterate1 illiterate2) /*no onservetions in 1971, 1973 an 1974*/
egen tot_ill_1975_79=rmean(illiterate3 illiterate4 illiterate5 illiterate6 illiterate7)
egen tot_ill_1980_84=rmean(illiterate8 illiterate9 illiterate10 illiterate11 illiterate12)
egen tot_ill_1985_89=rmean(illiterate13 illiterate14 illiterate15 illiterate16 illiterate17)
egen tot_ill_1990_94=rmean(illiterate18 illiterate19 illiterate20 illiterate21 illiterate22)
egen tot_ill_1995_99=rmean(illiterate23 illiterate24 illiterate25 illiterate26 illiterate27)
egen tot_ill_2000_04=rmean(illiterate28 illiterate29 illiterate30 illiterate31 illiterate32)
egen tot_ill_2005_09=rmean(illiterate33 illiterate34 illiterate35 illiterate36 illiterate37)
egen tot_ill_2010_14=rmean(illiterate38 illiterate39 illiterate40 illiterate41 illiterate42)
egen tot_ill_2015_19=rmean(illiterate43 illiterate44 illiterate45 illiterate46) /*no observations for 2019*/

egen fem_ill_1970_74=rmean(illiterate47 illiterate48) /*no onservetions in 1971, 1973 an 1974*/
egen fem_ill_1975_79=rmean(illiterate49 illiterate50 illiterate51 illiterate52 illiterate53)
egen fem_ill_1980_84=rmean(illiterate54 illiterate55 illiterate56 illiterate57 illiterate58)
egen fem_ill_1985_89=rmean(illiterate59 illiterate60 illiterate61 illiterate62 illiterate63)
egen fem_ill_1990_94=rmean(illiterate64 illiterate65 illiterate66 illiterate67 illiterate68)
egen fem_ill_1995_99=rmean(illiterate69 illiterate70 illiterate71 illiterate72 illiterate73)
egen fem_ill_2000_04=rmean(illiterate74 illiterate75 illiterate76 illiterate77 illiterate78)
egen fem_ill_2005_09=rmean(illiterate79 illiterate80 illiterate81 illiterate82 illiterate83)
egen fem_ill_2010_14=rmean(illiterate84 illiterate85 illiterate86 illiterate87 illiterate88)
egen fem_ill_2015_19=rmean(illiterate89 illiterate90 illiterate91 illiterate92) /*no observations for 2019*/

egen male_ill_1970_74=rmean(illiterate93 illiterate94) /*no onservetions in 1971, 1973 an 1974*/
egen male_ill_1975_79=rmean(illiterate95 illiterate96 illiterate97 illiterate98 illiterate99)
egen male_ill_1980_84=rmean(illiterate100 illiterate101 illiterate102 illiterate103 illiterate104)
egen male_ill_1985_89=rmean(illiterate105 illiterate106 illiterate107 illiterate108 illiterate109)
egen male_ill_1990_94=rmean(illiterate110 illiterate111 illiterate112 illiterate113 illiterate114)
egen male_ill_1995_99=rmean(illiterate115 illiterate116 illiterate117 illiterate118 illiterate119)
egen male_ill_2000_04=rmean(illiterate120 illiterate121 illiterate122 illiterate123 illiterate124)
egen male_ill_2005_09=rmean(illiterate125 illiterate126 illiterate127 illiterate128 illiterate129)
egen male_ill_2010_14=rmean(illiterate130 illiterate131 illiterate132 illiterate133 illiterate134)
egen male_ill_2015_19=rmean(illiterate135 illiterate136 illiterate137 illiterate138) /*no observations for 2019*/

drop illiterate*
save $path\lit1970_2019.dta, replace

/*earlier UNESCO estimates to replace missing values*/
import delimited $path\unesco_lit_est.csv, delimiter(comma) varnames(1) clear
keep if age_st=="15+" & age_end=="NA" /*keeping only total adult population estimates*/
destring year, replace
destring iso, replace
keep year per_tot per_male per_female iso
/*clearing duplicates for CON*/ 
sort iso year
quietly by iso year: gen dup = cond(_N==1,0,_n)
tab dup
drop if dup==2
drop dup
reshape wide per_tot per_male per_female, i(iso) j(year)
/*5 year averages*/
destring per_*, replace
egen ill_tot_1970_74=rmean(per_tot1970 per_tot1971 per_tot1972 per_tot1973 per_tot1974)
egen ill_tot_1975_79=rmean(per_tot1975 per_tot1976 per_tot1977 per_tot1978 per_tot1979)
egen ill_tot_1980_84=rmean(per_tot1980 per_tot1981 per_tot1982 per_tot1983 per_tot1984)
egen ill_tot_1985_89=rmean(per_tot1985 per_tot1986 per_tot1987 per_tot1988 per_tot1989)
egen ill_tot_1990_94=rmean(per_tot1990 per_tot1991 per_tot1992 per_tot1993 per_tot1994)
egen ill_tot_1995_99=rmean(per_tot1995 per_tot1996 per_tot1997 per_tot1998 per_tot1999)
egen ill_tot_2000_04=rmean(per_tot2000 per_tot2001 per_tot2002 per_tot2003 per_tot2004)
egen ill_tot_2005_09=rmean(per_tot2005 per_tot2006 per_tot2007 per_tot2008 per_tot2009)
egen ill_tot_2010_14=rmean(per_tot2010 per_tot2011 per_tot2012 per_tot2013 per_tot2014)
egen ill_tot_2015_19=rmean(per_tot2015 per_tot2016 per_tot2017 per_tot2018 per_tot2019)

egen ill_female_1970_74=rmean(per_female1970 per_female1971 per_female1972 per_female1973 per_female1974)
egen ill_female_1975_79=rmean(per_female1975 per_female1976 per_female1977 per_female1978 per_female1979)
egen ill_female_1980_84=rmean(per_female1980 per_female1981 per_female1982 per_female1983 per_female1984)
egen ill_female_1985_89=rmean(per_female1985 per_female1986 per_female1987 per_female1988 per_female1989)
egen ill_female_1990_94=rmean(per_female1990 per_female1991 per_female1992 per_female1993 per_female1994)
egen ill_female_1995_99=rmean(per_female1995 per_female1996 per_female1997 per_female1998 per_female1999)
egen ill_female_2000_04=rmean(per_female2000 per_female2001 per_female2002 per_female2003 per_female2004)
egen ill_female_2005_09=rmean(per_female2005 per_female2006 per_female2007 per_female2008 per_female2009)
egen ill_female_2010_14=rmean(per_female2010 per_female2011 per_female2012 per_female2013 per_female2014)
egen ill_female_2015_19=rmean(per_female2015 per_female2016 per_female2017 per_female2018 per_female2019)

egen ill_male_1970_74=rmean(per_male1970 per_male1971 per_male1972 per_male1973 per_male1974)
egen ill_male_1975_79=rmean(per_male1975 per_male1976 per_male1977 per_male1978 per_male1979)
egen ill_male_1980_84=rmean(per_male1980 per_male1981 per_male1982 per_male1983 per_male1984)
egen ill_male_1985_89=rmean(per_male1985 per_male1986 per_male1987 per_male1988 per_male1989)
egen ill_male_1990_94=rmean(per_male1990 per_male1991 per_male1992 per_male1993 per_male1994)
egen ill_male_1995_99=rmean(per_male1995 per_male1996 per_male1997 per_male1998 per_male1999)
egen ill_male_2000_04=rmean(per_male2000 per_male2001 per_male2002 per_male2003 per_male2004)
egen ill_male_2005_09=rmean(per_male2005 per_male2006 per_male2007 per_male2008 per_male2009)
egen ill_male_2010_14=rmean(per_male2010 per_male2011 per_male2012 per_male2013 per_male2014)
egen ill_male_2015_19=rmean(per_male2015 per_male2016 per_male2017 per_male2018 per_male2019)

drop per_*
rename iso _ISO3N_
drop if _ISO3N_=="NA"
destring _ISO3N_,replace
save $path\old_unesco.dta, replace

/*merging two datasets: unesco data and new estimates from UIS*/
use $path\lit1970_2019.dta
merge 1:1 _ISO3N_ using $path\old_unesco.dta

/*replacing missings in the new UIS with older estimations*/
drop if _ISO3N_==.
replace tot_ill_1970_74=ill_tot_1970_74 if tot_ill_1970_74==.
replace tot_ill_1975_79=ill_tot_1975_79 if tot_ill_1975_79==.
replace tot_ill_1980_84=ill_tot_1980_84 if tot_ill_1980_84==.
replace tot_ill_1985_89=ill_tot_1985_89 if tot_ill_1985_89==.
replace tot_ill_1990_94=ill_tot_1990_94 if tot_ill_1990_94==.
replace tot_ill_1995_99=ill_tot_1995_99 if tot_ill_1995_99==.
replace tot_ill_2000_04=ill_tot_2000_04 if tot_ill_2000_04==.
replace tot_ill_2005_09=ill_tot_2005_09 if tot_ill_2005_09==.
replace tot_ill_2010_14=ill_tot_2010_14 if tot_ill_2010_14==.
replace tot_ill_2015_19=ill_tot_2015_19 if tot_ill_2015_19==.

replace fem_ill_1970_74=ill_female_1970_74 if fem_ill_1970_74==.
replace fem_ill_1975_79=ill_female_1975_79 if fem_ill_1975_79==.
replace fem_ill_1980_84=ill_female_1980_84 if fem_ill_1980_84==.
replace fem_ill_1985_89=ill_female_1985_89 if fem_ill_1985_89==.
replace fem_ill_1990_94=ill_female_1990_94 if fem_ill_1990_94==.
replace fem_ill_1995_99=ill_female_1995_99 if fem_ill_1995_99==.
replace fem_ill_2000_04=ill_female_2000_04 if fem_ill_2000_04==.
replace fem_ill_2005_09=ill_female_2005_09 if fem_ill_2005_09==.
replace fem_ill_2010_14=ill_female_2010_14 if fem_ill_2010_14==.
replace fem_ill_2015_19=ill_female_2015_19 if fem_ill_2015_19==.

replace male_ill_1970_74=ill_male_1970_74 if male_ill_1970_74==.
replace male_ill_1975_79=ill_male_1975_79 if male_ill_1975_79==.
replace male_ill_1980_84=ill_male_1980_84 if male_ill_1980_84==.
replace male_ill_1985_89=ill_male_1985_89 if male_ill_1985_89==.
replace male_ill_1990_94=ill_male_1990_94 if male_ill_1990_94==.
replace male_ill_1995_99=ill_male_1995_99 if male_ill_1995_99==.
replace male_ill_2000_04=ill_male_2000_04 if male_ill_2000_04==.
replace male_ill_2005_09=ill_male_2005_09 if male_ill_2005_09==.
replace male_ill_2010_14=ill_male_2010_14 if male_ill_2010_14==.
replace male_ill_2015_19=ill_male_2015_19 if male_ill_2015_19==.

drop ill_*

/*estimations for missing values using un detailed region average for the given year_sex group*/
kountry _ISO3N_, from(iso3n) geo(undet) /*defining regions*/
egen geo_tot_1970_74=mean(tot_ill_1970_74), by(GEO)
replace tot_ill_1970_74=geo_tot_1970_74 if tot_ill_1970_74==.
egen geo_tot_1975_79=mean(tot_ill_1975_79), by(GEO)
replace tot_ill_1975_79=geo_tot_1975_79 if tot_ill_1975_79==.
egen geo_tot_1980_84=mean(tot_ill_1980_84), by(GEO)
replace tot_ill_1980_84=geo_tot_1980_84 if tot_ill_1980_84==.
egen geo_tot_1985_89=mean(tot_ill_1985_89), by(GEO)
replace tot_ill_1985_89=geo_tot_1985_89 if tot_ill_1985_89==.
egen geo_tot_1990_94=mean(tot_ill_1990_94), by(GEO)
replace tot_ill_1990_94=geo_tot_1990_94 if tot_ill_1990_94==.
egen geo_tot_1995_99=mean(tot_ill_1995_99), by(GEO)
replace tot_ill_1995_99=geo_tot_1995_99 if tot_ill_1995_99==.
egen geo_tot_2000_04=mean(tot_ill_2000_04), by(GEO)
replace tot_ill_2000_04=geo_tot_2000_04 if tot_ill_2000_04==.
egen geo_tot_2005_09=mean(tot_ill_2005_09), by(GEO)
replace tot_ill_2005_09=geo_tot_2005_09 if tot_ill_2005_09==.
egen geo_tot_2010_14=mean(tot_ill_2010_14), by(GEO)
replace tot_ill_2010_14=geo_tot_2010_14 if tot_ill_2010_14==.
egen geo_tot_2015_19=mean(tot_ill_2015_19), by(GEO)
replace tot_ill_2015_19=geo_tot_2015_19 if tot_ill_2015_19==.

egen geo_female_1970_74=mean(fem_ill_1970_74), by(GEO)
replace fem_ill_1970_74=geo_female_1970_74 if fem_ill_1970_74==.
egen geo_female_1975_79=mean(fem_ill_1975_79), by(GEO)
replace fem_ill_1975_79=geo_female_1975_79 if fem_ill_1975_79==.
egen geo_female_1980_84=mean(fem_ill_1980_84), by(GEO)
replace fem_ill_1980_84=geo_female_1980_84 if fem_ill_1980_84==.
egen geo_female_1985_89=mean(fem_ill_1985_89), by(GEO)
replace fem_ill_1985_89=geo_female_1985_89 if fem_ill_1985_89==.
egen geo_female_1990_94=mean(fem_ill_1990_94), by(GEO)
replace fem_ill_1990_94=geo_female_1990_94 if fem_ill_1990_94==.
egen geo_female_1995_99=mean(fem_ill_1995_99), by(GEO)
replace fem_ill_1995_99=geo_female_1995_99 if fem_ill_1995_99==.
egen geo_female_2000_04=mean(fem_ill_2000_04), by(GEO)
replace fem_ill_2000_04=geo_female_2000_04 if fem_ill_2000_04==.
egen geo_female_2005_09=mean(fem_ill_2005_09), by(GEO)
replace fem_ill_2005_09=geo_female_2005_09 if fem_ill_2005_09==.
egen geo_female_2010_14=mean(fem_ill_2010_14), by(GEO)
replace fem_ill_2010_14=geo_female_2010_14 if fem_ill_2010_14==.
egen geo_female_2015_19=mean(fem_ill_2015_19), by(GEO)
replace fem_ill_2015_19=geo_female_2015_19 if fem_ill_2015_19==.

egen geo_male_1970_74=mean(male_ill_1970_74), by(GEO)
replace male_ill_1970_74=geo_male_1970_74 if male_ill_1970_74==.
egen geo_male_1975_79=mean(male_ill_1975_79), by(GEO)
replace male_ill_1975_79=geo_male_1975_79 if male_ill_1975_79==.
egen geo_male_1980_84=mean(male_ill_1980_84), by(GEO)
replace male_ill_1980_84=geo_male_1980_84 if male_ill_1980_84==.
egen geo_male_1985_89=mean(male_ill_1985_89), by(GEO)
replace male_ill_1985_89=geo_male_1985_89 if male_ill_1985_89==.
egen geo_male_1990_94=mean(male_ill_1990_94), by(GEO)
replace male_ill_1990_94=geo_male_1990_94 if male_ill_1990_94==.
egen geo_male_1995_99=mean(male_ill_1995_99), by(GEO)
replace male_ill_1995_99=geo_male_1995_99 if male_ill_1995_99==.
egen geo_male_2000_04=mean(male_ill_2000_04), by(GEO)
replace male_ill_2000_04=geo_male_2000_04 if male_ill_2000_04==.
egen geo_male_2005_09=mean(male_ill_2005_09), by(GEO)
replace male_ill_2005_09=geo_male_2005_09 if male_ill_2005_09==.
egen geo_male_2010_14=mean(male_ill_2010_14), by(GEO)
replace male_ill_2010_14=geo_male_2010_14 if male_ill_2010_14==.
egen geo_male_2015_19=mean(male_ill_2015_19), by(GEO)
replace male_ill_2015_19=geo_male_2015_19 if male_ill_2015_19==.
drop geo_*
save $path\lit1970_2019.dta, replace

/*estimations for missing european countries*/
import delimited "path\lit_europe_estimated_othersuis_wb.csv", delimiter(comma) varnames(1) clear
keep iso sex year est3
egen year_sex=group(sex year), label
drop year sex
/*long to wide formatting */
reshape wide est3, i(iso) j(year_sex) /*long-to-wide formatting for seperate variables for gender groups*/
rename est31 est_fem_1970
rename est32 est_fem_1975
rename est33 est_fem_1980
rename est34 est_fem_1985
rename est35 est_fem_1990
rename est36 est_fem_1995
rename est37 est_fem_2000
rename est38 est_fem_2005
rename est39 est_fem_2010
rename est310 est_fem_2015
rename est313 est_male_1970
rename est314 est_male_1975
rename est315 est_male_1980
rename est316 est_male_1985
rename est317 est_male_1990
rename est318 est_male_1995
rename est319 est_male_2000
rename est320 est_male_2005
rename est321 est_male_2010
rename est322 est_male_2015
drop est3*
/*averaging male and female estimates to get the estimates for total population*/
replace est_fem_1970="" if est_fem_1970=="NA"
replace est_fem_1975="" if est_fem_1975=="NA"
replace est_fem_1980="" if est_fem_1980=="NA"
replace est_fem_1985="" if est_fem_1985=="NA"
replace est_fem_1990="" if est_fem_1990=="NA"
replace est_fem_1995="" if est_fem_1995=="NA"
replace est_fem_2000="" if est_fem_2000=="NA"
replace est_fem_2005="" if est_fem_2005=="NA"
replace est_fem_2010="" if est_fem_2010=="NA"
replace est_fem_2015="" if est_fem_2015=="NA"

replace est_male_1970="" if est_male_1970=="NA"
replace est_male_1975="" if est_male_1975=="NA"
replace est_male_1980="" if est_male_1980=="NA"
replace est_male_1985="" if est_male_1985=="NA"
replace est_male_1990="" if est_male_1990=="NA"
replace est_male_1995="" if est_male_1995=="NA"
replace est_male_2000="" if est_male_2000=="NA"
replace est_male_2005="" if est_male_2005=="NA"
replace est_male_2010="" if est_male_2010=="NA"
replace est_male_2015="" if est_male_2015=="NA"

destring est_*, replace

egen est_tot_1970=rmean(est_male_1970 est_fem_1970)
egen est_tot_1975=rmean(est_male_1975 est_fem_1975)
egen est_tot_1980=rmean(est_male_1980 est_fem_1980)
egen est_tot_1985=rmean(est_male_1985 est_fem_1985)
egen est_tot_1990=rmean(est_male_1990 est_fem_1990)
egen est_tot_1995=rmean(est_male_1995 est_fem_1995)
egen est_tot_2000=rmean(est_male_2000 est_fem_2000)
egen est_tot_2005=rmean(est_male_2005 est_fem_2005)
egen est_tot_2010=rmean(est_male_2010 est_fem_2010)
egen est_tot_2015=rmean(est_male_2015 est_fem_2015)

rename iso _ISO3N_
save $path/eur_est_lit.dta, replace

/*merging to replace missings with eur estimates*/
use $path\lit1970_2019.dta
drop _merge
merge 1:1 _ISO3N_ using $path\eur_est_lit.dta

replace tot_ill_1970_74=est_tot_1970 if tot_ill_1970_74==.
replace tot_ill_1975_79=est_tot_1975 if tot_ill_1975_79==.
replace tot_ill_1980_84=est_tot_1980 if tot_ill_1980_84==.
replace tot_ill_1985_89=est_tot_1985 if tot_ill_1985_89==.
replace tot_ill_1990_94=est_tot_1990 if tot_ill_1990_94==.
replace tot_ill_1995_99=est_tot_1995 if tot_ill_1995_99==.
replace tot_ill_2000_04=est_tot_2000 if tot_ill_2000_04==.
replace tot_ill_2005_09=est_tot_2005 if tot_ill_2005_09==.
replace tot_ill_2010_14=est_tot_2010 if tot_ill_2010_14==.
replace tot_ill_2015_19=est_tot_2015 if tot_ill_2015_19==.

replace fem_ill_1970_74=est_fem_1970 if fem_ill_1970_74==.
replace fem_ill_1975_79=est_fem_1975 if fem_ill_1975_79==.
replace fem_ill_1980_84=est_fem_1980 if fem_ill_1980_84==.
replace fem_ill_1985_89=est_fem_1985 if fem_ill_1985_89==.
replace fem_ill_1990_94=est_fem_1990 if fem_ill_1990_94==.
replace fem_ill_1995_99=est_fem_1995 if fem_ill_1995_99==.
replace fem_ill_2000_04=est_fem_2000 if fem_ill_2000_04==.
replace fem_ill_2005_09=est_fem_2005 if fem_ill_2005_09==.
replace fem_ill_2010_14=est_fem_2010 if fem_ill_2010_14==.
replace fem_ill_2015_19=est_fem_2015 if fem_ill_2015_19==.

replace male_ill_1970_74=est_male_1970 if male_ill_1970_74==.
replace male_ill_1975_79=est_male_1975 if male_ill_1975_79==.
replace male_ill_1980_84=est_male_1980 if male_ill_1980_84==.
replace male_ill_1985_89=est_male_1985 if male_ill_1985_89==.
replace male_ill_1990_94=est_male_1990 if male_ill_1990_94==.
replace male_ill_1995_99=est_male_1995 if male_ill_1995_99==.
replace male_ill_2000_04=est_male_2000 if male_ill_2000_04==.
replace male_ill_2005_09=est_male_2000 if male_ill_2005_09==.
replace male_ill_2010_14=est_male_2010 if male_ill_2010_14==.
replace male_ill_2015_19=est_male_2015 if male_ill_2015_19==.

drop NAMES_STD est_* _merge GEO
kountry _ISO3N_, from(iso3n) geo(undet) /*refreshing country names and regions */

/*dropping non-WIC countries with missing data*/
drop if _ISO3N_==584|_ISO3N_==585|_ISO3N_==666|_ISO3N_==20|_ISO3N_==60|_ISO3N_==92|_ISO3N_==212|_ISO3N_==234|_ISO3N_==292|_ISO3N_==304 ///
		|_ISO3N_==438|_ISO3N_==492|_ISO3N_==520|_ISO3N_==534|_ISO3N_==580|_ISO3N_==659|_ISO3N_==663|_ISO3N_==796|_ISO3N_==833|_ISO3N_==798
/*marshall islands, palau, saint pierre and miquelon, andorra, bermuda, british virgin islands, dominica, faroe islands, gibraltar, ///
		greenland, liechtenstein, monaco, nauru, bonaire, northern mariana islands, saint kitts and nevis, saint martin(french), ///
		turks and caicos islands, isle of man, tuvalu*/

use $path\lit1970_2019.dta
export delimited using "path\literacy_gender_5year.csv", replace

/* manual inputs in excel:
- USA, New Zealand, Canada, Australia from European estimates (i.e. UK, Switzerland, etc.)
- Taiwan from Hong Kong
- Mayotte from Comoros
- French Polynesia from Suriname
- Western Sahara from Morocco
- Curacao, Saint Lucia and US Virgin Islands from Caribbean average
- For Guam we had some years only. Missing years are replaced by the closest estimate to the future (i.e. 1970-74 and 1975-79 are copied from 1980-84)
- Micronesia and Kiribati from Guam  

210 countries total, female and male illiteracy estimates from 1970-74 to 2015-19. No missing values
Steps for estimation: 
1) new literacy estimates from uis website
2) 5-year period averages constructed
3) replace missings with older unesco estimates
4) replace missings with un detailed region averages
5) replace missing countries with estimates for european countries
6) manual inputs
*/















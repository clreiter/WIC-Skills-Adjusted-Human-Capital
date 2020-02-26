global path1 "C:\Users\acer\Dropbox\research\Qualmatt\literacy"
insheet using $path1\unesco_lit_est.csv, comma names /*previuos literacy data set*/ 
save $path1\unesco_lit_est_dilek.dta, replace
replace year = "1974" in 66
gen age_g=0
recode age_g 0=1 if age_st=="15" & age_end=="19"
recode age_g 0=2 if age_st=="20"
recode age_g 0=3 if age_st=="25"
recode age_g 0=4 if age_st=="30"
recode age_g 0=5 if age_st=="35"
recode age_g 0=6 if age_st=="40"
recode age_g 0=7 if age_st=="45"
recode age_g 0=8 if age_st=="50"
recode age_g 0=9 if age_st=="55"
recode age_g 0=10 if age_st=="60"
recode age_g 0=11 if age_st=="65"
recode age_g 0=12 if age_st=="70+"
recode age_g 0=13 if age_st=="15+"
recode age_g 0=14 if age_st=="15" & age_end=="24"
recode age_g 0=.
save $path1\unesco_lit_est_dilek_20200224.dta
drop if age_g==14 | age_g==.
drop age_st age_end pop_il_tot pop_il_male pop_il_female pop_tot pop_male pop_female
rename iso iso3n
label define agel 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" 9 "55-59" ///
					10 "60-64" 11 "65-69" 12 "70+" 13 "15+" 14 "15-24"
label values age_g agel
save $path1\unesco_lit_est_dilek_20200224.dta, replace

/*clearing duplicates for CON*/ 
sort iso3n year age_g
quietly by iso3n year age_g:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup==2
drop dup
save $path1\unesco_lit_est_dilek_20200224.dta, replace

/*long to wide transformation*/
drop if year=="85" /*clearing miscoded cases*/
drop if year=="NA"
destring year, replace
drop if iso3n=="NA"
destring iso3n, replace
drop if per_tot=="NA"
destring per_tot, replace
drop if per_male=="NA"
destring per_male, replace
drop if per_female=="NA"
destring per_female, replace
save $path1\unesco_lit_est_wide.dta, replace
reshape wide per_tot per_male per_female, i(iso3n age_g) j(year)
save $path1\unesco_lit_est_wide.dta, replace

/*calculating five year averages for every country*age_group*/
gen illit_1970_74=(per_tot1970+per_tot1971+per_tot1972+per_tot1973+per_tot1974)/5
gen illit_1975_79=(per_tot1975+per_tot1976+per_tot1977+per_tot1978+per_tot1979)/5
gen illit_1980_84=(per_tot1980+per_tot1981+per_tot1982+per_tot1983+per_tot1984)/5
gen illit_1985_89=(per_tot1985+per_tot1986+per_tot1987+per_tot1988+per_tot1989)/5
gen illit_1990_94=(per_tot1990+per_tot1991+per_tot1992+per_tot1993+per_tot1994)/5
gen illit_1995_99=(per_tot1995+per_tot1996+per_tot1997+per_tot1998+per_tot1999)/5
gen illit_2000_04=(per_tot2000+per_tot2001+per_tot2002+per_tot2003+per_tot2004)/5
gen illit_2005_09=(per_tot2005+per_tot2006+per_tot2007+per_tot2008+per_tot2009)/5
gen illit_2010_14=(per_tot2010+per_tot2011+per_tot2012+per_tot2013+per_tot2014)/5
gen illit_2015_19=(per_tot2015+per_tot2016+per_tot2017+per_tot2018+per_tot2019)/5
gen illit_2020_25=(per_tot2020+per_tot2021+per_tot2022+per_tot2023+per_tot2024+per_tot2025)/6

gen male_illit_1970_74=(per_male1970+per_male1971+per_male1972+per_male1973+per_male1974)/5
gen male_illit_1975_79=(per_male1975+per_male1976+per_male1977+per_male1978+per_male1979)/5
gen male_illit_1980_84=(per_male1980+per_male1981+per_male1982+per_male1983+per_male1984)/5
gen male_illit_1985_89=(per_male1985+per_male1986+per_male1987+per_male1988+per_male1989)/5
gen male_illit_1990_94=(per_male1990+per_male1991+per_male1992+per_male1993+per_male1994)/5
gen male_illit_1995_99=(per_male1995+per_male1996+per_male1997+per_male1998+per_male1999)/5
gen male_illit_2000_04=(per_male2000+per_male2001+per_male2002+per_male2003+per_male2004)/5
gen male_illit_2005_09=(per_male2005+per_male2006+per_male2007+per_male2008+per_male2009)/5
gen male_illit_2010_14=(per_male2010+per_male2011+per_male2012+per_male2013+per_male2014)/5
gen male_illit_2015_19=(per_male2015+per_male2016+per_male2017+per_male2018+per_male2019)/5
gen male_illit_2020_25=(per_male2020+per_male2021+per_male2022+per_male2023+per_male2024+per_male2025)/6

gen fem_illit_1970_74=(per_female1970+per_female1971+per_female1972+per_female1973+per_female1974)/5
gen fem_illit_1975_79=(per_female1975+per_female1976+per_female1977+per_female1978+per_female1979)/5
gen fem_illit_1980_84=(per_female1980+per_female1981+per_female1982+per_female1983+per_female1984)/5
gen fem_illit_1985_89=(per_female1985+per_female1986+per_female1987+per_female1988+per_female1989)/5
gen fem_illit_1990_94=(per_female1990+per_female1991+per_female1992+per_female1993+per_female1994)/5
gen fem_illit_1995_99=(per_female1995+per_female1996+per_female1997+per_female1998+per_female1999)/5
gen fem_illit_2000_04=(per_female2000+per_female2001+per_female2002+per_female2003+per_female2004)/5
gen fem_illit_2005_09=(per_female2005+per_female2006+per_female2007+per_female2008+per_female2009)/5
gen fem_illit_2010_14=(per_female2010+per_female2011+per_female2012+per_female2013+per_female2014)/5
gen fem_illit_2015_19=(per_female2015+per_female2016+per_female2017+per_female2018+per_female2019)/5
gen fem_illit_2020_25=(per_female2020+per_female2021+per_female2022+per_female2023+per_female2024+per_female2025)/6
save $path1\unesco_lit_est_wide.dta, replace

keep iso3n age_g illit_*
save $path1\illit.dta, replace

/*calculating age proportions for every time period*/
gen adj_70_74=illit_1970_74 if age_g==13
egen ad_70_74=mean(adj_70_74), by(iso)
gen age_prop_70=illit_1970_74/ad_70_74

gen adj_75_79=illit_1975_79 if age_g==13
egen ad_75_79=mean(adj_75_79), by(iso)
gen age_prop_75=illit_1975_79/ad_75_79

gen adj_80_84=illit_1980_84 if age_g==13
egen ad_80_84=mean(adj_80_84), by(iso)
gen age_prop_80=illit_1980_84/ad_80_84

gen adj_85_89=illit_1985_89 if age_g==13
egen ad_85_89=mean(adj_85_89), by(iso)
gen age_prop_85=illit_1985_89/ad_85_89

gen adj_90_94=illit_1990_94 if age_g==13
egen ad_90_94=mean(adj_90_94), by(iso)
gen age_prop_90=illit_1990_94/ad_90_94

gen adj_95_99=illit_1995_99 if age_g==13
egen ad_95_99=mean(adj_95_99), by(iso)
gen age_prop_95=illit_1995_99/ad_95_99

gen adj_00_04=illit_2000_04 if age_g==13
egen ad_00_04=mean(adj_00_04), by(iso)
gen age_prop_00=illit_2000_04/ad_00_04

gen adj_05_09=illit_2005_09 if age_g==13
egen ad_05_09=mean(adj_05_09), by(iso)
gen age_prop_05=illit_2005_09/ad_05_09

gen adj_10_14=illit_2010_14 if age_g==13
egen ad_10_14=mean(adj_10_14), by(iso)
gen age_prop_10=illit_2010_14/ad_10_14

gen adj_15_19=illit_2015_19 if age_g==13
egen ad_15_19=mean(adj_15_19), by(iso)
gen age_prop_15=illit_2015_19/ad_15_19

gen adj_20_25=illit_2020_25 if age_g==13
egen ad_20_24=mean(adj_20_24), by(iso)
gen age_prop_20=illit_2020_25/ad_20_24

keep iso3n age_g illit_* age_prop_*
save $path1\illit.dta, replace

/*merge with new unesco estimations*/
use $path1\lit_unesco_missings_replaced.dta
keep iso year illiteracy_new
rename iso iso3n
reshape wide illiteracy_new, i(iso) j(year)
save $path1\illit_new.dta
merge 1:m iso3n using $path1\illit.dta
sort iso3n age_g

/*replacing new unesco estimations with old ones for 2000-2019*/
gen n_illit_2000_04=illiteracy_new2000*age_prop_00
gen n_illit_2005_09=illiteracy_new2005*age_prop_05
gen n_illit_2010_14=illiteracy_new2010*age_prop_10
gen n_illit_2015_19=illiteracy_new2015*age_prop_15
drop illit_2000_04 illit_2005_09 illit_2010_14 illit_2015_19
rename n_illit_2000_04 illit_2000_04
rename n_illit_2005_09 illit_2005_09
rename n_illit_2010_14 illit_2010_14
rename n_illit_2015_19 illit_2015_19
keep iso3n age_g illit_*
save $path1\illiteracy20200224.dta

/*wide to long reshape*/
reshape long illit_, i(iso3n age_g) j(year) string
save $path1\illiteracy_long_20200224.dta
/*preparing for R*/
decode age_g, gen(age)
replace age="NA" if age==""
recode illit_ (missing=.a)
label define miss .a "NA"
label values illit_ miss
keep iso3n year age illit_
save $path1\illiteracy_long_20200224.dta, replace
outsheet using "C:\Users\acer\Dropbox\research\Qualmatt\literacy\new_unesco_lit_est.csv", comma replace

/*Government expenditure on education as a percentage of GDP (%) from UIS*/
/*countries with all missing data are not included in the original dataset*/
insheet using "path\EDULIT_DS_04062020040732881.csv", comma clear /*csv file downloaded from UIS*/
keep location country time value
rename value edu_exp
rename time year
kountry location, from(iso3c) to(iso3n)
rename _ISO3N_ iso
reshape wide edu_exp, i(location) j(year)
replace iso = 531 in 45 /*manually enter iso code for curacao*/ 
egen edu_exp_1970=rmean(edu_exp1970 edu_exp1971 edu_exp1972 edu_exp1973 edu_exp1974)
egen edu_exp_1975=rmean(edu_exp1975 edu_exp1976 edu_exp1977 edu_exp1978 edu_exp1979)
egen edu_exp_1980=rmean(edu_exp1980 edu_exp1981 edu_exp1982 edu_exp1983 edu_exp1984)
egen edu_exp_1985=rmean(edu_exp1985 edu_exp1986 edu_exp1987 edu_exp1988 edu_exp1989)
egen edu_exp_1990=rmean(edu_exp1990 edu_exp1991 edu_exp1992 edu_exp1993 edu_exp1994)
egen edu_exp_1995=rmean(edu_exp1995 edu_exp1996 edu_exp1997 edu_exp1998 edu_exp1999)
egen edu_exp_2000=rmean(edu_exp2000 edu_exp2001 edu_exp2002 edu_exp2003 edu_exp2004)
egen edu_exp_2005=rmean(edu_exp2005 edu_exp2006 edu_exp2007 edu_exp2008 edu_exp2009)
egen edu_exp_2010=rmean(edu_exp2010 edu_exp2011 edu_exp2012 edu_exp2013 edu_exp2014)
egen edu_exp_2015=rmean(edu_exp2015 edu_exp2016 edu_exp2017 edu_exp2018 edu_exp2019)
drop edu_exp1970 edu_exp1971 edu_exp1972 edu_exp1973 edu_exp1974 edu_exp1975 edu_exp1976 ///
	edu_exp1977 edu_exp1978 edu_exp1979 edu_exp1980 edu_exp1981 edu_exp1982 edu_exp1983 ///
	edu_exp1984 edu_exp1985 edu_exp1986 edu_exp1987 edu_exp1988 edu_exp1989 edu_exp1990 ///
	edu_exp1991 edu_exp1992 edu_exp1993 edu_exp1994 edu_exp1995 edu_exp1996 edu_exp1997 ///
	edu_exp1998 edu_exp1999 edu_exp2000 edu_exp2001 edu_exp2002 edu_exp2003 edu_exp2004 ///
	edu_exp2005 edu_exp2006 edu_exp2007 edu_exp2008 edu_exp2009 edu_exp2010 edu_exp2011 ///
	edu_exp2012 edu_exp2013 edu_exp2014 edu_exp2015 edu_exp2016 edu_exp2017 edu_exp2018 edu_exp2019
reshape long edu_exp_, i(iso) j(year)
/*adding empty rows for countries that are in samys data but not in UIS*/
merge 1:1 iso year using C:\Users\acer\Dropbox\research\Qualmatt\185_countries.dta
keep edu_exp_ iso year
reshape wide edu_exp_, i(iso) j(year)

/*estimations for missing values using un detailed region average for the given year*/
kountry iso, from(iso3n) geo(undet) /*defining regions*/
/*defining regions for missing countries*/
replace GEO="Eastern Asia" if iso==158 /*Taiwan*/
replace GEO="Southern Europe" if iso==499 /*Montenegro*/
replace GEO="Caribbean" if iso==531 /*Curaçao*/
replace GEO="Southern Europe" if iso==688 /*Serbia*/
/*merging Oceania regions*/
replace GEO="Oceania" if GEO=="Melanesia"
replace GEO="Oceania" if GEO=="Polynesia"
replace GEO="Oceania" if GEO=="Micronesia"


egen geo_exp_1970_74=mean(edu_exp_1970), by(GEO)
replace edu_exp_1970=geo_exp_1970_74 if edu_exp_1970==.
egen geo_exp_1975_79=mean(edu_exp_1975), by(GEO)
replace edu_exp_1975=geo_exp_1975_79 if edu_exp_1975==.
egen geo_exp_1980_84=mean(edu_exp_1980), by(GEO)
replace edu_exp_1980=geo_exp_1980_84 if edu_exp_1980==.
egen geo_exp_1985_89=mean(edu_exp_1985), by(GEO)
replace edu_exp_1985=geo_exp_1985_89 if edu_exp_1985==.
egen geo_exp_1990_94=mean(edu_exp_1990), by(GEO)
replace edu_exp_1990=geo_exp_1990_94 if edu_exp_1990==.
egen geo_exp_1995_99=mean(edu_exp_1995), by(GEO)
replace edu_exp_1995=geo_exp_1995_99 if edu_exp_1995==.
egen geo_exp_2000_04=mean(edu_exp_2000), by(GEO)
replace edu_exp_2000=geo_exp_2000_04 if edu_exp_2000==.
egen geo_exp_2005_09=mean(edu_exp_2005), by(GEO)
replace edu_exp_2005=geo_exp_2005_09 if edu_exp_2005==.
egen geo_exp_2010_14=mean(edu_exp_2010), by(GEO)
replace edu_exp_2010=geo_exp_2010_14 if edu_exp_2010==.
egen geo_exp_2015_19=mean(edu_exp_2015), by(GEO)
replace edu_exp_2015=geo_exp_2015_19 if edu_exp_2015==.

/*if a time period is missing, previous and next time period is averaged manually*/
/*tajikistan, kazakhstan, uzbekistan, kyrgzstan, turkmenistan before 1985 are copied from russia manually*/
keep iso edu_exp_*
reshape long edu_exp_, i(iso) j(year)
outsheet using "C:\Users\acer\Dropbox\research\Qualmatt\R\1970_2015\adj_fact\data\uis_edu_exp.csv", comma replace


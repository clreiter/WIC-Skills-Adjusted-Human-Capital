/*pupil-teacher ratio in primary education from UIS */
/*define the corresponding path in your computer:
global path "C:\Users\enter path" */
insheet using "path\EDULIT_DS_09042020060958672.csv", comma clear /*csv file downloaded from UIS*/
keep location country time value
kountry location, from(iso3c) to(iso3n)
recode _ISO3N_ .=729 if country=="Sudan (pre-secession)" /*code sudan iso as 729*/
keep time value _ISO3N_
rename value tp
drop if _ISO3N_==.
reshape wide tp, i(_ISO3N_) j(time)

egen tp_1970=rmean(tp1970 tp1971 tp1972 tp1973 tp1974)
egen tp_1975=rmean(tp1975 tp1976 tp1977 tp1978 tp1979)
egen tp_1980=rmean(tp1980 tp1981 tp1982 tp1983 tp1984)
egen tp_1985=rmean(tp1985 tp1986 tp1987 tp1988 tp1989)
egen tp_1990=rmean(tp1990 tp1991 tp1992 tp1993 tp1994)
egen tp_1995=rmean(tp1995 tp1996 tp1997 tp1998 tp1999)
egen tp_2000=rmean(tp2000 tp2001 tp2002 tp2003 tp2004)
egen tp_2005=rmean(tp2005 tp2006 tp2007 tp2008 tp2009)
egen tp_2010=rmean(tp2010 tp2011 tp2012 tp2013 tp2014)
egen tp_2015=rmean(tp2015 tp2016 tp2017 tp2018 tp2019)
drop tp1970 tp1971 tp1972 tp1973 tp1974 tp1975 tp1976 ///
	tp1977 tp1978 tp1979 tp1980 tp1981 tp1982 tp1983 ///
	tp1984 tp1985 tp1986 tp1987 tp1988 tp1989 tp1990 ///
	tp1991 tp1992 tp1993 tp1994 tp1995 tp1996 tp1997 ///
	tp1998 tp1999 tp2000 tp2001 tp2002 tp2003 tp2004 ///
	tp2005 tp2006 tp2007 tp2008 tp2009 tp2010 tp2011 ///
	tp2012 tp2013 tp2014 tp2015 tp2016 tp2017 tp2018 tp2019
reshape long tp_, i(_ISO3N_) j(year)
rename _ISO3N_ iso
merge 1:1 iso year using C:\Users\acer\Dropbox\research\Qualmatt\185_countries.dta
keep tp_ iso year
reshape wide tp_, i(iso) j(year)

/*estimations for missing values using un detailed region average for the given year*/
kountry iso, from(iso3n) geo(undet) /*defining regions*/
/*defining regions for missing countries*/
replace GEO="Eastern Asia" if iso==158 /*Taiwan*/
replace GEO="Southern Europe" if iso==499 /*Montenegro*/
replace GEO="Caribbean" if iso==531 /*Curaçao*/
replace GEO="Southern Europe" if iso==688 /*Serbia*/
egen geo_tp_1970_74=mean(tp_1970), by(GEO)
replace tp_1970=geo_tp_1970_74 if tp_1970==.
egen geo_tp_1975_79=mean(tp_1975), by(GEO)
replace tp_1975=geo_tp_1975_79 if tp_1975==.
egen geo_tp_1980_84=mean(tp_1980), by(GEO)
replace tp_1980=geo_tp_1980_84 if tp_1980==.
egen geo_tp_1985_89=mean(tp_1985), by(GEO)
replace tp_1985=geo_tp_1985_89 if tp_1985==.
egen geo_tp_1990_94=mean(tp_1990), by(GEO)
replace tp_1990=geo_tp_1990_94 if tp_1990==.
egen geo_tp_1995_99=mean(tp_1995), by(GEO)
replace tp_1995=geo_tp_1995_99 if tp_1995==.
egen geo_tp_2000_04=mean(tp_2000), by(GEO)
replace tp_2000=geo_tp_2000_04 if tp_2000==.
egen geo_tp_2005_09=mean(tp_2005), by(GEO)
replace tp_2005=geo_tp_2005_09 if tp_2005==.
egen geo_tp_2010_14=mean(tp_2010), by(GEO)
replace tp_2010=geo_tp_2010_14 if tp_2010==.
egen geo_tp_2015_19=mean(tp_2015), by(GEO)
replace tp_2015=geo_tp_2015_19 if tp_2015==.

/*manually copy values of 1970 and 75 azerbaijan to uzbekistan, kazakhstan, turkmenistan, tajikistan*/
replace GEO="Oceania" if GEO=="Melanesia"
replace GEO="Oceania" if GEO=="Polynesia"
replace GEO="Oceania" if GEO=="Micronesia"
replace GEO="Oceania" if GEO=="Australia and New Zealand"
keep iso tp_* GEO
reshape long tp_, i(iso) j(year)
outsheet using "C:\Users\acer\Dropbox\research\Qualmatt\R\1970_2015\adj_fact\data\uis_tp.csv", comma replace

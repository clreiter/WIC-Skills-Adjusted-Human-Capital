insheet using "C:\Users\acer\Dropbox\research\Qualmatt\R\1970_2015\adj_fact\hlo_1970_2015.csv", comma
keep year iso cc hlo_mean
replace hlo_mean="" if hlo_mean=="NA"
destring hlo_mean, generate(hlo)
drop hlo_mean
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015
replace hlo = hlo[_n+1] if missing(hlo) & year!=2015

reshape wide hlo, i(iso) j(year)
kountry iso, from(iso3n) geo(undet)
egen geo_1970_74=mean(hlo1970), by(GEO)
replace hlo1970=geo_1970_74 if hlo1970==.
egen geo_1975_79=mean(hlo1975), by(GEO)
replace hlo1975=geo_1975_79 if hlo1975==.
egen geo_1980_84=mean(hlo1980), by(GEO)
replace hlo1980=geo_1980_84 if hlo1980==.
egen geo_1985_89=mean(hlo1985), by(GEO)
replace hlo1985=geo_1985_89 if hlo1985==.
egen geo_1990_94=mean(hlo1990), by(GEO)
replace hlo1990=geo_1990_94 if hlo1990==.
egen geo_1995_99=mean(hlo1995), by(GEO)
replace hlo1995=geo_1995_99 if hlo1995==.
egen geo_2000_04=mean(hlo2000), by(GEO)
replace hlo2000=geo_2000_04 if hlo2000==.
egen geo_2005_09=mean(hlo2005), by(GEO)
replace hlo2005=geo_2005_09 if hlo2005==.
egen geo_2010_14=mean(hlo2010), by(GEO)
replace hlo2010=geo_2010_14 if hlo2010==.
egen geo_2015_19=mean(hlo2015), by(GEO)
replace hlo2015=geo_2015_19 if hlo2015==.

drop NAMES_STD
drop GEO
kountry iso, from(iso3n) geo(un)
drop geo_*
egen geo_1970_74=mean(hlo1970), by(GEO)
replace hlo1970=geo_1970_74 if hlo1970==.
egen geo_1975_79=mean(hlo1975), by(GEO)
replace hlo1975=geo_1975_79 if hlo1975==.
egen geo_1980_84=mean(hlo1980), by(GEO)
replace hlo1980=geo_1980_84 if hlo1980==.
egen geo_1985_89=mean(hlo1985), by(GEO)
replace hlo1985=geo_1985_89 if hlo1985==.
egen geo_1990_94=mean(hlo1990), by(GEO)
replace hlo1990=geo_1990_94 if hlo1990==.
egen geo_1995_99=mean(hlo1995), by(GEO)
replace hlo1995=geo_1995_99 if hlo1995==.
egen geo_2000_04=mean(hlo2000), by(GEO)
replace hlo2000=geo_2000_04 if hlo2000==.
egen geo_2005_09=mean(hlo2005), by(GEO)
replace hlo2005=geo_2005_09 if hlo2005==.
egen geo_2010_14=mean(hlo2010), by(GEO)
replace hlo2010=geo_2010_14 if hlo2010==.
egen geo_2015_19=mean(hlo2015), by(GEO)
replace hlo2015=geo_2015_19 if hlo2015==.

drop geo_*
reshape long hlo, i(iso) j(year)
encode GEO, gen(geo_big)
drop NAMES_STD GEO
kountry iso, from(iso3n) geo(undet)
drop NAMES_STD
encode GEO, gen(geo_det)
label drop geo_big
label drop geo_det

outsheet using "C:\Users\acer\Dropbox\research\Qualmatt\R\1970_2015\adj_fact\data\r2_hlo_1970_2015.csv", comma replace

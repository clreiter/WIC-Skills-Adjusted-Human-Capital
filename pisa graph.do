label define sexl 1 "Female" 2 "Male" 
label values sex sexl
sencode country, gen(cnt)
recode math se_math read se_read scie se_scie num_lit se_num_lit number_below_oecd number_above_oecd total_number prop_below_oecd prop_above_oecd (9999=.)
recode cnt 100=87
gen oecd=0
recode oecd 0=1 if cnt==4|cnt==5|cnt==7|cnt==13|cnt==14|cnt==15|cnt==18|cnt==19|cnt==20|cnt==23|cnt==24|cnt==25|cnt==26|cnt==27|cnt==29|cnt==32|cnt==34|cnt==35|cnt==36|cnt==37|cnt==39|cnt==42 ///
			|cnt==46|cnt==47|cnt==48|cnt==52|cnt==58|cnt==59|cnt==60|cnt==64|cnt==65|cnt==88|cnt==89|cnt==90|cnt==95|cnt==98



twoway (scatter cnt prop_above_oecd if sex==1, mcolor(green) msize(tiny)), ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Proportion of students above OECD average) xsize(10) ysize(20)


graph twoway (scatter cnt prop_above_oecd if year==2000, msymbol(O) msize(tiny))  ///
			(scatter cnt prop_above_oecd if year==2003, msymbol(D) msize(tiny)) ///
			(scatter cnt prop_above_oecd if year==2006, msymbol(T) msize(tiny))  ///
			(scatter cnt prop_above_oecd if year==2009, msymbol(X) msize(tiny))  ///
			(scatter cnt prop_above_oecd if year==2012, msymbol(Oh) msize(tiny))  ///
			(scatter cnt prop_above_oecd if year==2015, msymbol(Dh) msize(tiny))  ///
			(scatter cnt prop_above_oecd if year==2018, msymbol(Th) msize(tiny)) ///
			, ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Proportion of students above OECD average) xsize(10) ysize(20) ///
			legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))


graph twoway (scatter cnt prop_above_oecd if year==2000, mcolor(gs2) msize(small) cmissing(y))  ///
			(scatter cnt prop_above_oecd if year==2003, mcolor(gs4) msize(small) cmissing(y)) ///
			(scatter cnt prop_above_oecd if year==2006, mcolor(gs6) msize(small) cmissing(y))  ///
			(scatter cnt prop_above_oecd if year==2009, mcolor(gs8) msize(small) cmissing(y))  ///
			(scatter cnt prop_above_oecd if year==2012, mcolor(gs10) msize(small) cmissing(y))  ///
			(scatter cnt prop_above_oecd if year==2015, mcolor(gs12) msize(small) cmissing(y))  ///
			(scatter cnt prop_above_oecd if year==2018, mcolor(gs14) msize(small) cmissing(y)) ///
			, ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel grid) xtitle(Proportion of students above OECD average) xsize(10) ysize(20) ///
			legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018)) 
			

graph twoway (dot cnt prop_above_oecd if year==2000, ndots(0) msymbol(O)) ///
					(dot cnt prop_above_oecd if year==2003, ndots(0) msymbol(D)) ///
					(dot cnt prop_above_oecd if year==2006, ndots(0) msymbol(T)) ///
					(dot cnt prop_above_oecd if year==2009, ndots(0) msymbol(S)) ///
					(dot cnt prop_above_oecd if year==2012, ndots(0) msymbol(+)) ///
					(dot cnt prop_above_oecd if year==2015, ndots(0) msymbol(X)) ///
					(dot cnt prop_above_oecd if year==2018, ndots(0) msymbol(A)), ///
					 ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Proportion of students above OECD average) xsize(10) ysize(20) ///
					legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))

					
			
				
graph twoway (dot cnt prop_above_oecd if sex==1 & year==2000, ndots(0) msymbol(O)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2003, ndots(0) msymbol(D)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2006, ndots(0) msymbol(T)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2009, ndots(0) msymbol(S)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2012, ndots(0) msymbol(+)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2015, ndots(0) msymbol(X)) ///
					(dot cnt prop_above_oecd if sex==1 & year==2018, ndots(0) msymbol(A)), ///
					name(g1, replace) nodraw ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Prop. of students above OECD average/FEMALE) xsize(10) ysize(20) ///
					legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))
graph twoway (dot cnt prop_above_oecd if sex==2 & year==2000, ndots(0) msymbol(O) cmissing(y)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2003, ndots(0) msymbol(D)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2006, ndots(0) msymbol(T)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2009, ndots(0) msymbol(S)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2012, ndots(0) msymbol(+)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2015, ndots(0) msymbol(X)) ///
					(dot cnt prop_above_oecd if sex==2 & year==2018, ndots(0) msymbol(A)), ///
					name(g2, replace) nodraw ylabel(#100, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Prop. of students above OECD average/MALE) xsize(10) ysize(20) ///
					legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))
graph combine g1 g2, ycommon name(combined, replace)
graph display combined

sencode country, gen(countries) ///*just for oecd countries*/
graph twoway (dot countries prop_above_oecd if sex==1 & year==2000, ndots(0) msymbol(O)) ///
					(dot countries prop_above_oecd if sex==1 & year==2003, ndots(0) msymbol(D)) ///
					(dot countries prop_above_oecd if sex==1 & year==2006, ndots(0) msymbol(T)) ///
					(dot countries prop_above_oecd if sex==1 & year==2009, ndots(0) msymbol(S)) ///
					(dot countries prop_above_oecd if sex==1 & year==2012, ndots(0) msymbol(+)) ///
					(dot countries prop_above_oecd if sex==1 & year==2015, ndots(0) msymbol(X)) ///
					(dot countries prop_above_oecd if sex==1 & year==2018, ndots(0) msymbol(A)), ///
					name(g1, replace) nodraw ylabel(#36, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Prop. of students above OECD average/FEMALE) xsize(10) ysize(20) ///
					legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))
graph twoway (dot countries prop_above_oecd if sex==2 & year==2000, ndots(0) msymbol(O) cmissing(y)) ///
					(dot countries prop_above_oecd if sex==2 & year==2003, ndots(0) msymbol(D)) ///
					(dot countries prop_above_oecd if sex==2 & year==2006, ndots(0) msymbol(T)) ///
					(dot countries prop_above_oecd if sex==2 & year==2009, ndots(0) msymbol(S)) ///
					(dot countries prop_above_oecd if sex==2 & year==2012, ndots(0) msymbol(+)) ///
					(dot countries prop_above_oecd if sex==2 & year==2015, ndots(0) msymbol(X)) ///
					(dot countries prop_above_oecd if sex==2 & year==2018, ndots(0) msymbol(A)), ///
					name(g2, replace) nodraw ylabel(#36, labels labsize(tiny) angle(horizontal) valuelabel) xtitle(Prop. of students above OECD average/MALE) xsize(10) ysize(20) ///
					legend(label (1 2000) label (2 2003) label(3 2006) label(4 2009) label(5 2012) label(6 2015) label(7 2018))
graph combine g1 g2, ycommon name(combined, replace)
graph display combined
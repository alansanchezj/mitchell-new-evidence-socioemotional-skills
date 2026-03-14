/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Simulates the developmental path of skills between 8-19 (Figure 3)
and the impact of a 25% cash transfer for the lowest quartile 
(those elegible for the JUNTOS CCT) over the life cycle (Figures 4 and 5)
Created: Dec. 2019
*******************************************************************************/

/*******************************************************************************
LOAD DATA AND SET PREFERENCES												****
*******************************************************************************/
clear
set more off, permanently
set matsize 10000
* File path - change as required. Remaining paths are relative
clear
set more off, permanently
* File path - change as required. Remaining paths are relative
if "`c(username)'" == "alans" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\NdM Dropbox\Alan Sanchez\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
	global tabstub "Output"
}
else{
	cd ""
}
* Load data
use "Data\oc_measures_peru_R1-5_v13", clear 
* Merge with new big 5 data
merge 1:1 childid using "Data\Big5_v13.dta"
drop _merge
drop total_big5c5 total_big5n5 big5c_index5 big5n_index5
rename total_big5c5_test total_big5c5
rename total_big5n5_test total_big5n5
rename big5c_index5_test big5c_index5
rename big5n_index5_test big5n_index5
* Define programs
run "Programs\programs.do"

/*******************************************************************************
1. Calculate initial conditions and estimate the baseline model				****
*******************************************************************************/

*** Run estimations 
run "Dofiles\_2_Estimates_baseline_tables2345.do"

* Estimate the income process
keep childid ln_hhinc* hschool* hstudy* hwork* hcare* hchore*
reshape long ln_hhinc hschool hstudy hwork hcare hchore, i(childid) j(round)

encode childid, gen(id)
xtset id round
gen lincome = L.ln_hhinc
bysort id (round): gen t = _n 
reg ln_hhinc lincome t, robust
tempname beta
mat `beta' = e(b)
scalar ycons = `beta'[1,3]
scalar ylag = `beta'[1,1]
scalar trend = `beta'[1,2]

* Estimate hours process 
foreach w in school study work care chore{
	gen lh`w' = L.h`w'
	reg h`w' lh`w', robust
	tempname beta
	mat `beta' = e(b)
	scalar h`w'cons = `beta'[1,2]
	scalar h`w'lag = `beta'[1,1]
	*scalar trend = `beta'[1,1]
}

* Draw a sample of 100,000 from the estimated distribution. 
drawnorm ncog0 cog0 pncog0 pcog0 income0 hschool1 hstudy1 hwork1 hcare1 hchore1, ///
		clear n(100000) cov(sigma) means(mu) seed(19901812) forcepsd

* First, calculate fittted income & hours all periods
forval i = 1/3{
	local k = `i'+1
	local p = `i'-1
	cap drop income`i'
	gen income`i' = ycons+ ylag*income`p' + trend*`k'
	if `i'>1{
		foreach w in school study work care chore{
			cap drop h`w'`i'
			gen h`w'`i' = h`w'cons+ h`w'lag*h`w'`p'
		}
	}
}
putmata income1 income2 income3 h*, replace

/*************************************************
2. Calculate baseline HC at each stage		  ****
*************************************************/

mata: 
	x = st_data(.,"ncog0 cog0 pncog0 pcog0 income1")
	phc = x[|.,3\.,4|]	

	// Period 1
	y = st_matrix("Iparms1")
	I0 = x*y'

	x = (x[|.,1\.,4|],I0)
	y1 = st_matrix("cogparms1")
	y2 = st_matrix("ncparms1") 

	cog1 = x*y1'
	ncog1 = x*y2' 
	H1 = (ncog1,cog1)

	// Period 2
	
	x = (H1, phc, income2)
	y = st_matrix("Iparms2")
	I1 = x*y'

	x = (x[|.,1\.,4|],I1)
	y1 = st_matrix("cogparms2")
	y2 = st_matrix("ncparms2")

	cog2 = x*y1'
	ncog2 = x*y2' 
	H2 = (ncog2,cog2)

	// Period 3
	x = (H2, phc, income3)
	y = st_matrix("Iparms3")
	I2 = x*y'

	x = (x[|.,1\.,4|],I2)
	y1 = st_matrix("cogparms3")
	y2 = st_matrix("ncparms3")

	cog3 = x*y1'
	ncog3 = x*y2'
	H3 = (ncog3,cog3)

	//Period 4 (early adulthood)
	x = (ncog3,cog3,hstudy3,hwork3,hcare3,hchore3,J(rows(ncog3),1,1))
	ys = st_matrix("socialparms")
	yt = st_matrix("taskeparms")

	social = x*ys'
	taske = x*yt'

	//Put matrices into stata
	I = (I0,I1,I2)
	H = (H1,H2,H3)
	st_matrix("I",I)
	st_matrix("H",H)

	SE22 = (social,taske)
	st_matrix("SE22",SE22)

end
* Create and rename new variables
svmat I, names("I")
rename I1 I0
rename I2 I1
rename I3 I2

svmat H, names("H")
rename H1 ncog1
rename H2 cog1
rename H3 ncog2 
rename H4 cog2
rename H5 ncog3
rename H6 cog3

svmat SE22, names(temp)
rename temp1 social
rename temp2 taske 

/******************************************************************
2. Calculate HC at each stage with an income transfer at 8
******************************************************************/

cap drop income1_t
gen income1_t = ln(exp(income1)*1.25) if exp(income1) < 417 //25% income increase for the lowest income quartile (elegible for JUNTOS CCT)*/
mata: 
	// Period 1
	x = st_data(.,"ncog0 cog0 pncog0 pcog0 income1_t")
	y = st_matrix("Iparms1")
	I0_t0 = x*y'

	x = (x[|.,1\.,4|],I0_t0)
	y1 = st_matrix("cogparms1")
	y2 = st_matrix("ncparms1")

	cog1_t0 = x*y1'
	ncog1_t0 = x*y2' 
	H1_t0 = (ncog1_t0,cog1_t0)

	// Period 2
	x = (H1_t0, phc, income2)
	y = st_matrix("Iparms2")
	I1_t0 = x*y'

	x = (x[|.,1\.,4|],I1_t0)
	y1 = st_matrix("cogparms2")
	y2 = st_matrix("ncparms2")

	cog2_t0 = x*y1'
	ncog2_t0 = x*y2' 
	H2_t0 = (ncog2_t0,cog2_t0)

	// Period 3
	x = (H2_t0, phc, income3)
	y = st_matrix("Iparms3")
	I2_t0 = x*y'

	x = (x[|.,1\.,4|],I2_t0)
	y1 = st_matrix("cogparms3")
	y2 = st_matrix("ncparms3")

	cog3_t0 = x*y1'
	ncog3_t0 = x*y2'
	H3_t0 = (ncog3_t0,cog3_t0)

	//Period 4 (early adulthood)
	x = (ncog3_t0,cog3_t0,hstudy3,hwork3,hcare3,hchore3,J(rows(ncog3),1,1))
	ys = st_matrix("socialparms")
	yt = st_matrix("taskeparms")

	social_t0 = x*ys'
	taske_t0 = x*yt'

	//Put matrices into Stata
	I_t0 = (I0_t0,I1_t0,I2_t0)
	H_t0 = (H1_t0,H2_t0,H3_t0)
	st_matrix("I_t0",I_t0)
	st_matrix("H_t0",H_t0)

	SE22_t0 = (social_t0,taske_t0)
	st_matrix("SE22_t0",SE22_t0)
end
* Create and rename new variables
svmat I_t0, names("I_t0")
rename I_t01 I0_t0
rename I_t02 I1_t0
rename I_t03 I2_t0

svmat H_t0, names("H_t0")
rename H_t01 ncog1_t0
rename H_t02 cog1_t0
rename H_t03 ncog2_t0
rename H_t04 cog2_t0
rename H_t05 ncog3_t0
rename H_t06 cog3_t0

svmat SE22_t0, names(SE_t0)
rename SE_t01 social_t0
rename SE_t02 taske_t0

/******************************************************************
3. Calculate HC at each stage with an income transfer at 12
******************************************************************/

cap drop income2_t
gen income2_t = ln(exp(income2)*1.25) if exp(income1) < 417 //25% increase for < Juntos threshold
putmata income2_t, replace

mata: 
	// Period 2
	x = (ncog1, cog1, phc, income2_t)
	y = st_matrix("Iparms2")
	I1_t1 = x*y'

	x = (x[|.,1\.,4|],I1_t1)
	y1 = st_matrix("cogparms2")
	y2 = st_matrix("ncparms2")

	cog2_t1 = x*y1'
	ncog2_t1 = x*y2' 
	H2_t1 = (ncog2_t1,cog2_t1)

	// Period 3
	x = (H2_t1, phc, income3)
	y = st_matrix("Iparms3")
	I2_t1 = x*y'

	x = (x[|.,1\.,4|],I2_t1)
	y1 = st_matrix("cogparms3")
	y2 = st_matrix("ncparms3")

	cog3_t1 = x*y1'
	ncog3_t1 = x*y2'
	H3_t1 = (ncog3_t1,cog3_t1)

	//Period 4 (early adulthood)
	x = (ncog3_t1,cog3_t1,hstudy3,hwork3,hcare3,hchore3,J(rows(ncog3),1,1))
	ys = st_matrix("socialparms")
	yt = st_matrix("taskeparms")

	social_t1 = x*ys'
	taske_t1 = x*yt'

	//Put matrices into Stata
	I_t1 = (I1_t1,I2_t1)
	H_t1 = (H2_t1,H3_t1)
	st_matrix("I_t1",I_t1)
	st_matrix("H_t1",H_t1)

	SE22_t1 = (social_t1,taske_t1)
	st_matrix("SE22_t1",SE22_t1)
end
* Create and rename new variables
svmat I_t1, names("I_t1")
rename I_t11 I1_t1
rename I_t12 I2_t1

svmat H_t1, names("H_t1")
rename H_t11 ncog2_t1
rename H_t12 cog2_t1
rename H_t13 ncog3_t1
rename H_t14 cog3_t1

svmat SE22_t1, names(SE_t1)
rename SE_t11 social_t1
rename SE_t12 taske_t1

/******************************************************************
3. Calculate HC at each stage with an income transfer at 15
******************************************************************/

cap drop income3_t
gen income3_t = ln(exp(income3)*1.25) if exp(income1) < 417 //25% increase for < Juntos threshold
putmata income3_t, replace	

mata: 
	// Period 3
	//x = (H2, phc, income3_t)
	x = (ncog2, cog2, phc, income3_t)
	y = st_matrix("Iparms3")
	I2_t2 = x*y'

	x = (x[|.,1\.,4|],I2_t2)
	y1 = st_matrix("cogparms3")
	y2 = st_matrix("ncparms3")

	cog3_t2 = x*y1'
	ncog3_t2 = x*y2'
	H3_t2 = (ncog3_t2,cog3_t2)

	//Period 4 (early adulthood)
	x = (ncog3_t2,cog3_t2,hstudy3,hwork3,hcare3,hchore3,J(rows(ncog3),1,1))
	ys = st_matrix("socialparms")
	yt = st_matrix("taskeparms")

	social_t2 = x*ys'
	taske_t2 = x*yt'

	//Put matrices into Stata
	I_t2 = (I2_t2)
	H_t2 = (H3_t2)
	st_matrix("I_t2",I_t2)
	st_matrix("H_t2",H_t2)

	SE22_t2 = (social_t2,taske_t2)
	st_matrix("SE22_t2",SE22_t2)	
end
* Create and rename new variables
svmat I_t2, names("I_t2")
rename I_t21 I2_t2

svmat H_t2, names("H_t2")
rename H_t21 ncog3_t2
rename H_t22 cog3_t2

svmat SE22_t2, names(SE_t2)
rename SE_t21 social_t2
rename SE_t22 taske_t2

/***********************************
SIMULATIONS
***********************************/

cap ssc install binscatter, replace 

**********************
*** FIGURE 3**********
**********************
twoway (kdensity ncog0, lcolor(black)) ///
	|| (kdensity ncog3, lcolor(black) lpattern(longdash)), ///
	graphregion(fcolor(white) lcolor(white)) ///
	ylab(0(.1).4, nogrid format(%3.2f) ang(0)) ///
	xtitle("Log latent socio-emotional skill") xsca(titlegap(5)) ///
	legend(order(1 "Age 8" 2 "Age 19") region(lcolor(white))) name(graph1, replace)	
	graph export "${graphstub}\Figure 3_panelA.pdf", replace

forval i=0/3{
	cap drop incquant`i'
	xtile incquant`i'=income`i', n(10)
}
preserve
	keep incquant* ncog*
	gen id = _n
	reshape long ncog incquant, i(id) j(round)
	collapse (mean) ncog, by(incquant round)

	twoway (line ncog round if incquant == 1, lcolor(black)) ///
		|| (line ncog round if incquant == 5, lcolor(black) lpattern(longdash)) ///
		|| (line ncog round if incquant == 10, lcolor(black) lwidth(thick) ), ///
		legend(order(1 "Bottom decile" 2 "Middle decile" 3 "Top decile") rows(1) region(lcolor(white))) ///
		graphregion(fcolor(white) lcolor(white)) ///
		ylab(-1(0.5)3, nogrid ang(0)) ytitle("Mean log latent socio-emotional skill") ysc(titlegap(5)) ///
		xlab(0 "8" 1 "12" 2 "15" 3 "19") xtitle("Age") xsc(titlegap(5)) name(graph2, replace)
restore 
graph export "${graphstub}\Figure 3_panelB.pdf", replace

graph combine graph1 graph2, graphregion(color(white)) graphregion(color(white)) xsize(10)
graph export "${graphstub}\Figure 3.pdf", replace

***********************
*** FIGURE 4***********
***********************
twoway (kdensity ncog0                      , color(black) lwidth(thick) lpat(solid)) ///
	|| (kdensity ncog3 if exp(income1) < 417, color(black) lwidth(med) lpat(solid)) ///
	|| (kdensity ncog3_t2                   , color(black) lwidth(med) lpat(dash)) ///
	|| (kdensity ncog3                      , color(black) lwidth(med) lpat(longdash)), ///
	graphregion(fcolor(white) m(r=3) lcolor(white)) plotregion(fcolor(white) lcolor(black)) bgcolor(white) ///
	ylab(, nogrid labcolor(black)) yscale(range() lcolor("139 89 70")) ///
	xlab(, val labcolor(black)) xscale(lcolor(black)) ///
	ytitle("Density") xtitle("Latent socio-emotional skill") ///
	legend(region(lcolor(white)) rows(1) size(small) color(black) ///
	order(1 "Age 8" 2 "Age 19; poor" 3 "Age 19; poor transfer age 15" 4 "Age 19")) name(graph1, replace) xsize(6)		
	graph export "${graphstub}\Figure 4_panelA.pdf", replace
	
forval i=1/3{
	local k = `i'
	foreach w in cog ncog{
		cap drop diff_`w'`i'_t0
		gen diff_`w'`i'_t0 = (`w'`i'_t0 - `w'`i')*100
	}
}
forval i=2/3{
	foreach w in cog ncog{
		cap drop diff_`w'`i'_t1
		gen diff_`w'`i'_t1 = (`w'`i'_t1 - `w'`i')*100
	}
}
forval i=3/3{
	foreach w in cog ncog{
		cap drop diff_`w'`i'_t2
		gen diff_`w'`i'_t2 = (`w'`i'_t2 - `w'`i')*100
	}
}

preserve 
	collapse (mean) diff_cog3_t0 diff_cog3_t1 diff_cog3_t2  ///
	diff_ncog3_t0 diff_ncog3_t1 diff_ncog3_t2

	cap drop id
	gen id = _n
	reshape long diff_cog3_t diff_ncog3_t, i(id) j(round)

	twoway  ///
		(connected diff_cog3_t round, lcolor(black) lwidth(thin) mcolor(black)) ///
		(connected diff_ncog3_t round, lcolor(black) lpattern(longdash) lwidth(thin) mcolor(black)), ///
		graphregion(fcolor(white) lcolor(white) ) plotregion(fcolor(white) lcolor(black) m(r=10 l=10)) bgcolor(white) ///
		xtitle(" ""Age of income transfer") ytitle("% change at 19"" ") ylab(0(0.5)3, nogrid ang(0)) ///
		xlab(0 "8" 1 "12" 2 "15") ///
		legend(region(lcolor(white)) rows(1) size(small) order(1 "Cognitive skill" 2 "Socio-emotional skill")) name(graph2, replace)		
restore 
graph export "${graphstub}\Figure 4_panelB.pdf", replace

graph combine graph1 graph2, graphregion(color(white)) graphregion(color(white)) xsize(10)
graph export "${graphstub}\Figure 4.pdf", replace

***********************
*** FIGURE 5***********
***********************
**** % effect on social and tesk e. based on round of transfer ****
forval i=0/2{
	local k = `i'
	foreach w in taske social{
		cap drop diff_`w'_t`i'
		gen diff_`w'_t`i' = (`w'_t`i' - `w')*100
	}
}

preserve 
	collapse (mean) diff_taske_t0 diff_taske_t1 diff_taske_t2  ///
	diff_social_t0 diff_social_t1 diff_social_t2

	cap drop id
	gen id = _n
	reshape long diff_taske_t diff_social_t, i(id) j(round)

	twoway  ///
		(connected diff_taske_t round,  lcolor(black) lwidth(thin) mcolor(black)) ///
		(connected diff_social_t round, lcolor(black) lwidth(thin) mcolor(black) lpattern(longdash)), ///
		graphregion(fcolor(white) lcolor(white) ) plotregion(fcolor(white) lcolor(black) m(r=10 l=10)) bgcolor(white) ///
		xtitle(" ""Age of income transfer") ytitle("% change at 22"" ") ylab(, nogrid ang(0)) ///
		xlab(0 "8" 1 "12" 2 "15") ///
		legend(region(lcolor(white)) rows(1) size(small) order(1 "Task effectiveness" 2 "Social skills")) name(graph3, replace)
restore 
graph export "${graphstub}\Figure 5.pdf", replace

/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Baseline estimates (Tables 2, 3, 4 and 5)
Created: Dec. 2019
Structure:
	1. Estimate initial conditions, measurement parameteres, and residual measures
	2. Estimate investment function in period 1
	3. Estimate production functions in period 1
	4. Repeat 2 and 3 for remaining periods using their output as inputs.
*******************************************************************************/

* Run preliminary code to set directory and load data. Set == 0 after first period 
global prelim = 1

* Initial == 1 if initial conditions need to be estimated. Only requires one run
global initial = 1

/* Investment/production == 1 to obtain estimates of investment/production
function paramaters in the given period.*/
global investment = 1
global production = 1
global p4 = 1

* Set whether or not table output is desired
global tables = 1

/*******************************************************************************
LOAD DATA AND SET PREFERENCES												****
*******************************************************************************/
if $prelim == 1{
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
} 

/*******************************************************************************
INITIAL CONDITIONS	 														****
*******************************************************************************/
if $initial == 1{
	run "Programs\initial_conditions_hours.do"
}
/*******************************************************************************
Defining endowments and Inputs/outputs in each period						****
*******************************************************************************/
* Create a clustering variable baed on urban/rural and initial sampling cluster 
cap drop clusterid
gen clusterid = typesite1*clustid1

* Set the period.
forval i = 1/3{
global period = `i'

run "Programs\inputs.do"

/*******************************************************************************
INVESTMENT FUNCTIONS PERIODS 1-3 											****
*******************************************************************************/
if $investment == 1{
	strucinv $Ioutput  /*if booksexp_pp2 < 20 & hhinc_m_r2 <20000*/  //This is just two outliers that dominate results 
	est store inv$period
	* Create residual measures to be used as inputs in to next period's investment functions
	cap drop ${Ioutput}r 
	gen ${Ioutput}r = ($Ioutput - e(mu))/e(fl) 

	* Store estimates as vector 
	tempname b
	mat `b' = e(b)
	mat Iparms$period = `b'[1,3..7]
}
/*******************************************************************************
PRODUCTION FUNCTIONS PERIODS 1-3 											****
*******************************************************************************/
* Note: pride, agency, agency is the way in the paper but why?
if $production == 1{ 
	**** ESTIMATES ****
	* Extract individual components of local outputs 
	gettoken (global)nc (global)cog: (global)Poutputs, parse(" ")
	local nametrim = strltrim("$cog")
	global cog "`nametrim'"

	*** Cognitive production
	strucprod $cog  /*if booksexp_pp2 < 20 & hhinc_m_r2 <20000*/
	est store cogprod$period
	cap drop ${cog}r 
	gen ${cog}r = (${cog} - e(mu))/e(fl)

	* Store estimates as vector 
	tempname b
	mat `b' = e(b)
	mat cogparms$period = `b'[1,3..7]

	*** Non-cognitive production
	strucprod $nc /* if booksexp_pp2 < 20 & hhinc_m_r2 <20000 */
	est store ncprod$period
	cap drop ${nc}r 
	gen ${nc}r = (${nc} - e(mu))/e(fl)

	* Store estimates as vector 
	tempname b
	mat `b' = e(b)
	mat ncparms$period = `b'[1,3..7]
	}
}
/*******************************************************************************
PRODUCTION FUNCTION PERIOD 4 (18-22)										****
This is the code from progrmas.do adjusted for there being no Investments 	****
*******************************************************************************/
if $p4 == 1{
	*** First estimate measurement system
	global relation "total_leader5 total_team5 total_peerrelation5" //total_peerrelation5//
	global control "total_agency5 total_grit5 total_big5n5 total_big5c5" 

	foreach w in relation control{

		corr ${`w'}, covariance 
		tempname a
		matrix `a' = r(C)
		matrix colnames `a' = ${`w'}
		matrix rownames `a' = ${`w'}

			mata: a= st_matrix("`a'")
			mata: n = cols(a)
			mata: b = a[1..n,1]
			mata: c = J(1,n,1)
			mata: d = b#c
			mata: e = a:/d
			mata: f = e[2..n,2..n]
			mata: _diag(f,0)
			mata: c = J(1,cols(f),1)
			mata: flvec = (1, (c*f):/(cols(f)-1))
			mata: flvec
			mata: st_matrix("flvec`w'", flvec)

		matrix colnames flvec`w' = ${`w'}
		local n: word count ${`w'}
		
		foreach var of varlist ${`w'}{
			
			cap drop mu`var'
			egen mu`var' = mean(`var')
			tempname `var'vec
			matrix ``var'vec' = flvec`w'[1,"`var'"]
			cap drop lambda`var'
			gen lambda`var' = ``var'vec'[1,1]
			cap drop `var'r
			gen `var'r = (`var' - mu`var')/lambda`var'
		}
	}

	*** Now estimate production functions
	global period = 4
	run "Programs\inputs.do"
	/*cap drop intinput
	gen intinput = ${ncoginput}*${coginput}
	* Create interactions to be used as instruments
	macro drop intinst
	foreach q of global coginst{
		foreach s of global ncoginst{
			cap drop `q'`s'
			gen `q'`s' = `q'*`s'
			global intinst $intinst `q'`s'	
		}	
	}*/

	*** Social skills production
	strucprod4 $relationoutput
	est store relation$period
	cap drop ${relationoutput}r 
	quietly sum $relationoutput
	gen ${relationoutput}r = ${relationoutput} - r(mean)

	tempname b
	mat `b' = e(b)
	mat socialparms = `b'[1,1..7]

	*** Task effectiveness production
	strucprod4 $controloutput
	est store control$period
	cap drop ${controloutput}r 
	quietly sum $controloutput
	gen ${controloutput}r = ${controloutput} - r(mean)

	tempname b
	mat `b' = e(b)
	mat taskeparms = `b'[1,1..7]
}
* Save this post estimation data
cap drop __00*
save "Data\sample_postest.dta", replace
/********************************************************************************
TABLES																		*****
********************************************************************************/

if $tables == 1{
	
***************
*** Table 2 ***
***************

esttab inv1 inv2 inv3 using "${tabstub}\Table2.rtf", replace ///
level(90) cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) label ///
nonum varwidth(35) collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" /// 
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" /// 
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t}}" ///
		cog "\u8193?ln {\i H{\sub c,t}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" ///
		pcog "\u8193?ln {\i P{\sub c}}" ////
		inc "\u8193?ln {\i Y{\sub t}}") ///
stats(sigmanu N, labels("\u0963?{\super\expnd-24 2}{\sub \u0960?\dn6\afs22 {\i c}}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		inc "{\b Resources\line{\afs20 \u8193?}}", nolabel) ///
title("{\b Table 2}\line {\i Estimates of Investment Function Parameters}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, and 19 for the three columns respectively. The output in each column is investment. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and family income, respectively. In period 3 (ages 15-19) the we use the YL wealth index as a proxy for family income as this information is not available. The wealth index is a measure of the material resources of the family which ranges from 0 to 1, and is constructed as the average of three sub-indices measuring housing quality, access to services and ownership of a range of durable goods. See Briones (2017) for detail. All inputs except of family income are treated as unobservable. The observables used as measures of each and their associated measurement parameters estimated from the measurement system outlined in Section 3 are provided in Online Appendix B.")
		
				
				
***************
*** Table 3 ***
***************

esttab ncprod1 ncprod2 ncprod3 using "${tabstub}\Table3.rtf", replace ///
level(90) cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) label ///
nonum varwidth(35) collabels(none) substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" pcog "\u8193?ln {\i P{\sub c}}" ///
		I "\u8193?ln {\i I{\sub t{\plain\sub -1}}}") ///
stats(sigmanu N, labels("\u0963?{\super\expnd-24 2}{\sub \u0951?\dn6\afs22 {\i n}}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" /// 
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}", nolabel) ///
title("{\b Table 3}\line {\i Estimates of Cobb-Douglas Socio-Emotional Production Function Parameters}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, 15, and 19 for the three columns respectively. The output in each column is socio-emotional skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment. All inputs are treated as unobservable. The observables used as measures of each and their associated measurement parameters estimated from the measurement system outlined in Section 3 are provided in Online Appendix B.")
		
		
***************
*** Table 4 ***
***************

esttab cogprod1 cogprod2 cogprod3 using "${tabstub}\Table4.rtf", replace ///
level(90) cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) label ///
nonum varwidth(35) collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///	
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" pcog "\u8193?ln {\i P{\sub c}}" ///
		I "{\u8193?ln {\i I{\sub t{\plain\sub -1}}}}") ///
stats(sigmanu N, labels("\u0963?{\super\expnd-24 2}{\sub \u0951?\dn6\afs22 {\i c}}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}", nolabel) ///
title("{\b Table 4}\line {\i Estimates of Cobb-Douglas Cognitive Production Function Parameters}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, 15, and 19 for the three columns respectively. The output in each column is cognitive skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment. All inputs are treated as unobservable. The observables used as measures of each and their associated measurement parameters estimated from the measurement system outlined in Section 3 are provided in Online Appendix B.")
		

***************
*** Table 5 ***
***************

esttab relation4 control4 using "${tabstub}\Table5.rtf", replace  ///
level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs label ///
nonum varwidth(30) collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("(1){\line\i Social skills\line{\afs20 \u8193?}}" ///
		"(2){\line\i Task effectiveness\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		tfp "\u8193?{\u0945?{\sub\i T}}" tfp1 "\u8193?Hours studying" ///
		tfp2 "\u8193?Hours working" tfp3 "\u8193?Hours caring" ///
		tfp4 "\u8193?Hours home production" rts "Returns to scale" ) ///
stats(sigmanu N, labels("\u0963?{\super\expnd-24 2}{\sub \u0951?{\i\up6\afs22\expnd-24 j}{\i\dn6\afs22 s}}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		tfp "\afs20 \u8193?" ///
		tfp2 "\afs20 \u8193?" ///
		tfp3 "\afs20 \u8193?" ///
		tfp4 "\afs20 \u8193?" ///
		rts "\afs20 \u8193?" ///
		tfp1 "{\b Total Factor Productivity} (ln {\i A{\sub T}})\line{\afs20 \u8193?}", nolabel) ///
title("{\b Table 5}\line {\i Estimates of Socio-Emotional Production Functions in Adulthood}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i T} = age 19 in each column. The left column contains lagged child socio-emotional skill and cognitive skill; the variables included in ln {\i A{\sub T}}; residual productivity {\i \u0945?{\sub T}}; and the estimates Returns to Scale (RTS). Lagged human capital is treated as unobservable. The observables used as measures for each are described in Online Appendix B. Online Appendix A outlines the method used to obtain all estimates in the table.")
}

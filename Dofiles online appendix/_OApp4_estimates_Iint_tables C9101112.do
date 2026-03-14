/*******************************************************************************
Paper: "Title TBC"
		Mark Mitchell, Catherine Porter, Alan Sanchez, 2019
Contents: Estimates of production functions with interaction of investments and
		  child human capital, without bootstrapping.
Created: 24/10/19

Structure:
	1. Estimate initial conditions, measurement parameteres, and residual measures
	2. Estimate investment function in period 1
	3. Estimate production functions in period 1
	4. Repeat 2 and 3 for remaining periods using their output as inputs.
*******************************************************************************/

* Run preliminary code to set directory and load data
global prelim = 1

* Initial == 1 if initial conditions need to be estimated.
global initial = 1

/* Investment/production == 1 to obtain estimates of investment/production
function paramaters in the given period.  Her investment can be == 0 if this
file is being run after the baseline estimations*/
global investment = 1
global production = 1
global p4 = 0
* Set for which interaction estimations are to be run in all 5 periods
global ihc = 1
global ihn = 1
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
/*
	if "`c(username)'" == "markm" | "`c(username)'" == "nxb19103"{
		cd "C:\Users\\`c(username)'\Dropbox\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
		global tabstub "Output"
	}
*/
	else{
		cd ""
	}
	* Load data
	use "Data\oc_measures_peru_R1-5", clear
	* Define programs
	run "Programs\programs.do"
} 

/*******************************************************************************
INITIAL CONDITIONS	 														****
*******************************************************************************/
if $initial == 1{
	run "Programs\initial_conditions.do"
}

/*******************************************************************************
Defining endowments and Inputs/outputs in each period						****
*******************************************************************************/
* Create a clustering variable baed on urban/rural and initial samplig cluster 
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
	strucinv $Ioutput if booksexp_pp2 < 20 & hhinc_m_r2 <20000 //This is just two outliers that dominate results 
	est store inv$period 
	* Create residual measures to be used as inputs in to next period's investment functions
	cap drop ${Ioutput}r 
	gen ${Ioutput}r = ($Ioutput - e(mu))/e(fl)
}
/*******************************************************************************
PRODUCTION FUNCTIONS 														****
*******************************************************************************/
if $production == 1{
	**** ESTIMATES ****
	* Extract individual components of local outputs 
	gettoken (global)nc (global)cog: (global)Poutputs
	local nametrim = strltrim("$cog")
	global cog "`nametrim'"

	*** I x H_c
	if $ihc == 1{
		* Create interaction with two least noisy measures: collinearity probs here
		cap drop intinput
		gen intinput = ${coginput}*${iinput}
		* Create interactions to be used as instruments
		gettoken (global) firstcog rest: (global) coginst 
		gettoken (global) firstinv rest: (global) iinst 

		cap drop ${firstcog}${firstinv} 
		gen ${firstcog}${firstinv} = ${firstcog}*${firstinv}
		macro drop intinst
		global intinst ${firstcog}${firstinv}
		/*foreach q of global coginst{
			foreach s of global iinst{
				cap drop `q'`s'
				gen `q'`s' = `q'*`s'
				global intinst $intinst `q'`s'	
				}
		}*/
		*** Cognitive production
		strucprod2 $cog if booksexp_pp2 < 20 & hhinc_m_r2 <20000 
		est store cogprod_ihc$period
		cap drop ${cog}r 
		gen ${cog}r = (${cog} - e(mu))/e(fl) 

		*** Non-cognitive production
		strucprod2 $nc if booksexp_pp2 < 20 & hhinc_m_r2 <20000 
		est store ncprod_ihc$period
		cap drop ${nc}r 
		gen ${nc}r = (${nc} - e(mu))/e(fl) 
	}
	
	*** I x H_n
	if $ihn == 1{
		cap drop intinput
		gen intinput = ${ncoginput}*${iinput}
		* Create interactions to be used as instruments
		gettoken (global) firstncog rest: (global) ncoginst 
		gettoken (global) firstinv rest: (global) iinst 

		cap drop ${firstncog}${firstinv}
		gen ${firstncog}${firstinv} = ${firstncog}*${firstinv}
		macro drop intinst
		global intinst ${firstncog}${firstinv}
		* Create interactions to be used as instruments
		/*macro drop intinst
		foreach q of global ncoginst{
			foreach s of global iinst{
					cap drop `q'`s'
					gen `q'`s' = `q'*`s'
					global intinst $intinst `q'`s'	
				}	
		}*/
		*** Cognitive production
		strucprod2 $cog if booksexp_pp2 < 20 & hhinc_m_r2 <20000 
		est store cogprod_ihn$period
		cap drop ${cog}r 
		gen ${cog}r = (${cog} - e(mu))/e(fl) //if prodsampcog$period==1//

		*** Non-cognitive production
		strucprod2 $nc if booksexp_pp2 < 20 & hhinc_m_r2 <20000 
		est store ncprod_ihn$period
		cap drop ${nc}r 
		gen ${nc}r = (${nc} - e(mu))/e(fl) //if prodsampncog$period==1//
	}	
	
}

}
/*******************************************************************************
PRODUCTION FUNCTION PERIOD 4 (18-22)										****
This is the code from progrmas.do adjusted for there being no Investments 	****
*******************************************************************************/
if $p4 == 1{
	*** First estimate measurement system
	global relation "total_team5 total_leader5 total_peerrelation5" 
	global wb "total_sesteem5 ladder_current5 total_pride5"
	global control "total_agency5 total_sefficacy5 total_grit5" 

	foreach w in relation wb control{

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
	cap drop intinput
	gen intinput = ${ncoginput}*${coginput}
	* Create interactions to be used as instruments
	macro drop intinst
	foreach q of global coginst{
		foreach s of global ncoginst{
			cap drop `q'`s'
			gen `q'`s' = `q'*`s'
			global intinst $intinst `q'`s'	
		}	
	}

	*** Relation production
	strucprod4 $relationoutput
	est store relation$period
	cap drop ${relationoutput}r 
	quietly sum $relationoutput
	gen ${relationoutput}r = ${relationoutput} - r(mean)

	*** Wellbeing production
	strucprod4 $wboutput
	est store wb$period
	cap drop ${wboutput}r 
	quietly sum $wboutput
	gen ${wboutput}r = ${wboutput} - r(mean)

	*** Wellbeing production
	strucprod4 $controloutput
	est store control$period
	cap drop ${controloutput}r 
	quietly sum $controloutput
	gen ${controloutput}r = ${controloutput} - r(mean)
}
* Save this post estimation data
cap drop __00*
save "Data\sample_postest.dta", replace
/********************************************************************************
TABLES																		*****
********************************************************************************/
if $tables == 1{
	if $investment == 1{
		*** Investment 
		esttab inv1 inv2 inv3 using "${tabstub}\invtab.tex", replace frag ///
			alignment(s) level(90) cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
			noobs drop(mu fl) label ///
			booktabs nonum gaps nonotes collabels(none) substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01) ///
			mtitles("\shortstack{Period 1\\[2.5pt]\emph{Ages 8-12}}" "\shortstack{Period 2\\[2.5pt]\emph{Ages 12-15}}" ///
				"\shortstack{Period 3\\[2.5pt]\emph{Ages 15-19}}" ///
				"\shortstack{Period 3\\[2.5pt]\emph{Ages 19-22}}") plain ///
			varlabels(ncog "\addlinespace[10pt]$\ln{H_{n,t-1}}$" ///
				cog "\addlinespace[10pt]$\ln{H_{c,t-1}}$" ///
				pncog "\addlinespace[10pt]$\ln{E_n}$" pcog "\addlinespace[10pt]$\ln{E_c}$" ////
				inc "\addlinespace[10pt]$\ln{Y_t}$" hh "\addlinespace[10pt]$\ln{N_{children}}$") ///
			stats(sigmanu N, labels("\addlinespace[10pt]\bottomrule$\sigma_{\pi_c}^2$" "N") fmt(%18.3g))
	}
	
	
	if $ihc == 1{
		
*****************
*** Table C11 ***
*****************

esttab cogprod_ihc1 cogprod_ihc2 cogprod_ihc3 using "${tabstub}\TableC11.rtf", replace ///
level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) varwidth(35) ///
nonum nonotes collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" ///
		pcog "\u8193?ln {\i P{\sub c}}" ///
		I "{\u8193?ln {\i I{\sub t{\plain\sub -1}}}}" ///
		int "{\u8193?ln {\i I{\sub t{\plain\sub -1}}} \u215? ln {\i H{\sub c,t{\plain\sub -1}}}}") ///
stats(sigmanu N, labels("{\u0963?}{\super\expnd-24 2}{\sub \u0951?\dn6\i\afs22 c}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}" ///
		int "\afs20 \u8193?" , nolabel) ///
title("{\b Table C11}\line {\i Estimates of Cognitive Production Function Parameters with Interacted Investment and Cognitive Skill}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, and 15 for the three columns respectively. The output in each column is cognitive skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment and its interaction with lagged human capital. All inputs are treated as unobservable. The observables used as measures of each are discussed in Online Appendix B. Online Appendix A outlines the method used to obtain all estimates in the table.")


****************
*** Table C9 ***
****************

esttab ncprod_ihc1 ncprod_ihc2 ncprod_ihc3 using "${tabstub}\TableC9.rtf", replace  ///
level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) varwidth(35) ///
nonum collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" ///
		pcog "\u8193?ln {\i P{\sub c}}" ///
		I "{\u8193?ln {\i I{\sub t{\plain\sub -1}}}}" ///
		int "{\u8193?ln {\i I{\sub t{\plain\sub -1}}} \u215? ln {\i H{\sub c,t{\plain\sub -1}}}}") ///
stats(sigmanu N, labels("{\u0963?}{\super\expnd-24 2}{\sub \u0951?\i\dn6\afs22 n}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}" ///
		int "\afs20 \u8193?" , nolabel) ///
title("{\b Table C9}\line {\i Estimates of Socio-Emotional Production Function Parameters with Interacted Investment and Cognitive Skill}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, 15, and 19 for the three columns respectively. The output in each column is socio-emotional skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment and its interaction with lagged human capital. All inputs are treated as unobservable. The observables used as measures of each are discussed in Online Appendix Tables B. Online Appendix A outlines the method used to obtain all estimates in the table.")

	}
	
	
	if $ihn == 1{
		
*****************
*** Table C12 ***
*****************

esttab cogprod_ihn1 cogprod_ihn2 cogprod_ihn3 using "${tabstub}\TableC12.rtf", replace ///
level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) varwidth(35) ///
nonum nonotes collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" ///
		pcog "\u8193?ln {\i P{\sub c}}" ///
		I "{\u8193?ln {\i I{\sub t{\plain\sub -1}}}}" ///
		int "{\u8193?ln {\i I{\sub t{\plain\sub -1}}} \u215? ln {\i H{\sub s,t{\plain\sub -1}}}}") ///
stats(sigmanu N, labels("{\u0963?}{\super\expnd-24 2}{\sub \u0951?\dn6\i\afs22 c}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}" ///
		int "\afs20 \u8193?" , nolabel) ///
title("{\b Table C12}\line {\i Estimates of Cognitive Production Function Parameters with Interacted Investment and Socio-Emotional Skill}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, and 15 for the three columns respectively. The output in each column is cognitive skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment and its interaction with lagged human capital. All inputs are treated as unobservable. The observables used as measures of each are discussed in Online Appendix B. Online Appendix A outlines the method used to obtain all estimates in the table.")




*****************
*** Table C10 ***
*****************

esttab ncprod_ihn1 ncprod_ihn2 ncprod_ihn3 using "${tabstub}\TableC10.rtf", replace  ///
level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
noobs drop(mu fl) varwidth(35) ///
nonum collabels(none) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Period 1\line{\i Ages 8-12\line{\afs20 \u8193?}}" ///
		"Period 2\line{\i Ages 12-15\line{\afs20 \u8193?}}" ///
		"Period 3\line{\i Ages 15-19\line{\afs20 \u8193?}}") ///
varlabels(ncog "\u8193?ln {\i H{\sub s,t{\plain\sub -1}}}" ///
		cog "\u8193?ln {\i H{\sub c,t{\plain\sub -1}}}" ///
		pncog "\u8193?ln {\i P{\sub s}}" ///
		pcog "\u8193?ln {\i P{\sub c}}" ///
		I "{\u8193?ln {\i I{\sub t{\plain\sub -1}}}}" ///
		int "{\u8193?ln {\i I{\sub t{\plain\sub -1}}} \u215? ln {\i H{\sub s,t{\plain\sub -1}}}}") ///
stats(sigmanu N, labels("{\u0963?}{\super\expnd-24 2}{\sub \u0951?\dn6\i\afs22 n}" "N") fmt(%18.3g)) ///
refcat(ncog "{\b Lagged human capital\line{\afs20 \u8193?}}" ///
		cog "\afs20 \u8193?" ///
		pncog "{\b Parental human capital (fixed over time)\line{\afs20 \u8193?}}" ///
		pcog "\afs20 \u8193?" ///
		I "{\b Investments\line{\afs20 \u8193?}}" ///
		int "\afs20 \u8193?" , nolabel) ///
title("{\b Table C10}\line {\i Estimates of Socio-Emotional Production Function Parameters with Interacted Investment and Socio-Emotional Skill}\line") ///
note("\qj {\b Notes:} Standard errors are in parentheses, and 90% confidence intervals are in square brackets. Both are calculated using the delta method. {\i t} - 1 = ages 8, 12, 15, and 19 for the three columns respectively. The output in each column is socio-emotional skill. The inputs in the left column are are lagged child socio-emotional skill and cognitive skill; parental socio-emotional and cognitive skill; and investment and its interaction with lagged human capital. All inputs are treated as unobservable. The observables used as measures of each are discussed in Online Appendix Tables B. Online Appendix A outlines the method used to obtain all estimates in the table.")

	}
	
	
	if $p4==1{
		esttab relation4 control4 using "${tabstub}_Production\prodtabnc_p4.tex", replace frag ///
			alignment(s) level(90)  cell(b(fmt(%18.3f) star) se(fmt(%18.3f) par) ci(fmt(%18.3f) par("[" "," "]"))) ///
			noobs label ///
			booktabs nonum gaps nonotes collabels(none) substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01) ///
			mtitles("\shortstack{(1)\\[2.5pt]\emph{Pro-sociality}}" ///
				"\shortstack{(3)\\[2.5pt]\emph{Locus of Control}}") plain ///
			varlabels(ncog "\addlinespace[10pt]$\ln{H_{n,t-1}}$" ///
				cog "\addlinespace[10pt]$\ln{H_{c,t-1}}$" ///
				I "\addlinespace[10pt]$\ln{I_{t-1}}$" ///
				int "\addlinespace[10pt]$$" ///
				tfp "\addlinespace[10pt]$\alpha_T$" tfp1 "\addlinespace[10pt]Hours studying" ///
				tfp2 "\addlinespace[10pt]Hours working" tfp3 "\addlinespace[10pt]Hours caring" ///
				tfp4 "\addlinespace[10pt]Hours on home production" rts "\addlinespace[10pt]Returns to scale" ) ///
			stats(sigmanu N, labels("\addlinespace[10pt]\bottomrule$\sigma_{\eta^j_s}^2$" "N") fmt(%18.3g)) ///
			refcat(ncog "\toprule \textbf{Lagged human capital}" tfp1 "\textbf{$\ln{A_{T}}$}", nolabel)
	}
}

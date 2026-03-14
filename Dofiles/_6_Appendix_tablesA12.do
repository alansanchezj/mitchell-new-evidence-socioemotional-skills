/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Calculating measurement parameters shown in Appendix Tables A1 and A2
Created: Dec. 2019
*******************************************************************************/
* Run preliminary code to set directory and load data. Set == 0 after first period 
global prelim = 1

/*******************************************************************************
LOAD DATA AND SET PREFERENCES												****
*******************************************************************************/
if $prelim == 1{
	clear all
	macro drop _all
	set more off, permanently
	set matsize 800
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
	drop total_big5c5 total_big5n5
	rename total_big5c5_test total_big5c5
	rename total_big5n5_test total_big5n5
	* Define programs
	run "Programs\programs.do"
} 
global initial = 1
global invprod = 1
global p4 = 1
global tables = 1

/*******************************************************************************
Estimating initial measurement parameters and signal/noise ratios			****
*******************************************************************************/

*******	 INITIAL MEASUREMENT PARAMETERS	*******
if $initial == 1{
	
	* Initital measures
	global pcog "careed2 carelita2 literspc1" 
	global pncog "tot_cagency2 tot_csocial2 cladder2"

	global cog "ravens1 levlwrit1 levlread1"
	global ncog "sdqcon1 sdqemo1 sdqhyper1 sdqpeer1 sdqprosoc1"

	foreach w in pcog pncog cog ncog{
		
		quietly corr ${`w'}, covariance 
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
			
			tempname a
			quietly corr ${`w'}, covariance 
			matrix `a' = r(C)
			scalar `w'var = (`a'[1,2]*`a'[1,3])/`a'[2,3]

			cap drop sig`var'
			cap drop noise`var'
			quietly sum `var'
			gen sig`var' = ((lambda`var'^2)*abs(`w'var))/r(Var)
			gen noise`var' = 1-sig`var'
			
			global `w'mu0		${`w'mu0} mu`var'
			global `w'lambda0	${`w'lambda0} lambda`var'
			global `w'sig0		${`w'sig0} sig`var'
			global `w'noise0	${`w'noise0} noise`var'
		}
	}
	di ${`w'lambda0} 
	
}
/***************************************************************************************
Estimating investment measurement parameters and signal/noise ratios in periods 1/3 ****
***************************************************************************************/
if $invprod == 1{
	forval i = 1/3{
		* Set the period. 
		global period = `i'
		run "Programs\inputs.do"
		* Investment measurement parms
		quietly ivreg2 $Ioutput $yinput ///
			( $coginput $ncoginput $pcoginput $pncoginput = ///
			$coginst $ncoginst $pcoginst $pncoginst ), robust
		tempname beta
		matrix `beta' = e(b)
		cap drop mu$Ioutput
		gen mu$Ioutput = `beta'[1,6]
		cap drop lambda$Ioutput
		gen lambda$Ioutput = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$yinput ]
		cap drop ${Ioutput}r
		gen ${Ioutput}r = (${Ioutput}-mu${Ioutput})/lambda${Ioutput}
		foreach var of varlist $iinst {
			quietly ivreg2 `var' $yinput  ///
				( $coginput $ncoginput $pcoginput $pncoginput = ///
				$coginst $ncoginst $pcoginst $pncoginst ), robust
			tempname beta
			matrix `beta' = e(b)
			
			cap drop mu`var'
			gen mu`var' = `beta'[1,6]
			cap drop lambda`var'
			gen lambda`var' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$yinput ]
			cap drop `var'r
			gen `var'r = (`var'-mu`var')/lambda`var'

			tempname thetavar
			cap drop `thetavar'
			quietly corr `var'r $iinput, cov
			gen `thetavar' = r(cov_12)
			di `thetavar'
			
			cap drop sig`var'
			quietly sum `var'
			gen sig`var' = ((lambda`var'^2)*abs(`thetavar'))/r(Var)
			cap drop noise`var'
			gen noise`var' = 1-sig`var'
			
			global imu$period		${imu$period} mu`var'
			global ilambda$period	${ilambda$period} lambda`var'
			global isig$period		${isig$period} sig`var'
			global inoise$period	${inoise$period} noise`var'
		}
		gettoken (global)first (global)rest: (global)iinst
		tempname thetavar
		cap drop `thetavar'
		quietly corr ${Ioutput}r ${first}r, cov
		gen `thetavar' = r(cov_12)
		di `thetavar'

		cap drop sig$Ioutput
		cap drop noise$Ioutput
		quietly sum $Ioutput
		gen sig$Ioutput = ((lambda${Ioutput}^2)*abs(`thetavar'))/r(Var)
		gen noise$Ioutput = 1-sig$Ioutput

		global imu$period		${imu$period} mu$Ioutput
		global ilambda$period	${ilambda$period} lambda$Ioutput
		global isig$period		${isig$period} sig$Ioutput
		global inoise$period	${inoise$period} noise$Ioutput

		/*******************************************************************************
		Estimating human capital measurement parameters and signal/noise ratios		****
		*******************************************************************************/
		gettoken (global)ncog (global)cog: (global)Poutputs
		local nametrim = strltrim("$cog")
		global cog "`nametrim'"
		*** Cognitive 
		quietly ivreg2 $cog  ///
			($coginput $ncoginput $pcoginput $pncoginput $iinput = ///
			$coginst $ncoginst $pcoginst $pncoginst $iinst ), robust
		tempname beta
		matrix `beta' = e(b)

		cap drop mu$cog
		gen mu$cog = `beta'[1,6]
		cap drop lambda$cog
		gen lambda$cog = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$iinput ]
		cap drop ${cog}r
		gen ${cog}r = (${cog}-mu${cog})/lambda${cog}

		foreach var of varlist $cogm{
			 quietly ivreg2 `var'  ///
				($coginput $ncoginput $pcoginput $pncoginput $iinput = ///
				$coginst $ncoginst $pcoginst $pncoginst $iinst ), robust
			tempname beta
			matrix `beta' = e(b)
			
			cap drop mu`var'
			gen mu`var' = `beta'[1,6]
			cap drop lambda`var'
			gen lambda`var' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$iinput ]
			cap drop `var'r
			gen `var'r = (`var'-mu`var')/lambda`var'
			
			tempname thetavar
			cap drop `thetavar'
			quietly corr `var'r ${cog}r, cov
			gen `thetavar' = r(cov_12)
			di `thetavar'
			
			cap drop sig`var'
			cap drop noise`var'
			quietly sum `var'
			gen sig`var' = ((lambda`var'^2)*abs(`thetavar'))/r(Var)
			gen noise`var' = 1-sig`var'	
			
			global cogmu$period			${cogmu$period} mu`var'
			global coglambda$period		${coglambda$period} lambda`var'
			global cogsig$period		${cogsig$period} sig`var'
			global cognoise$period		${cognoise$period} noise`var'
		}
		gettoken (global)first (global)rest: (global)cogm
		tempname thetavar
		cap drop `thetavar'
		quietly corr ${cog}r ${first}r, cov
		gen `thetavar' = r(cov_12)
		di `thetavar'

		cap drop sig$cog
		cap drop noise$cog
		quietly sum $cog
		gen sig$cog = ((lambda${cog}^2)*abs(`thetavar'))/r(Var)
		gen noise$cog = 1-sig$cog

		global cogmu$period			${cogmu$period} mu$cog
		global coglambda$period		${coglambda$period} lambda$cog
		global cogsig$period		${cogsig$period} sig$cog
		global cognoise$period		${cognoise$period} noise$cog

		*** Non-cognitive
		 ivreg2 $ncog  ///
				($coginput $ncoginput $pcoginput $pncoginput $iinput = ///
				$coginst $ncoginst $pcoginst $pncoginst $iinst  ), robust
		tempname beta
		matrix `beta' = e(b)
		cap drop mu$ncog
		gen mu$ncog = `beta'[1,6]
		cap drop lambda$ncog
		gen lambda$ncog = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$iinput ]
		cap drop ${ncog}r
		gen ${ncog}r = (${ncog}-mu${ncog})/lambda${ncog}
		foreach var of varlist $ncogm{
			quietly ivreg2 `var'  ///
				($coginput $ncoginput $pcoginput $pncoginput $iinput = ///
				$coginst $ncoginst $pcoginst $pncoginst $iinst ), robust
			tempname beta
			matrix `beta' = e(b)
			
			cap drop mu`var'
			gen mu`var' = `beta'[1,6]
			cap drop lambda`var'
			gen lambda`var' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$iinput ]
			cap drop `var'r
			gen `var'r = (`var'-mu`var')/lambda`var'
			
			tempname thetavar
			cap drop `thetavar'
			quietly corr `var'r ${ncog}r, cov
			gen `thetavar' = r(cov_12)
			di `thetavar'
			
			cap drop sig`var'
			cap drop noise`var'
			quietly sum `var'
			gen sig`var' = ((lambda`var'^2)*abs(`thetavar'))/r(Var)
			gen noise`var' = 1-sig`var'	
			
			global ncogmu$period		${ncogmu$period} mu`var'
			global ncoglambda$period	${ncoglambda$period} lambda`var'
			global ncogsig$period		${ncogsig$period} sig`var'
			global ncognoise$period		${ncognoise$period} noise`var'
		}
		gettoken (global)first (global)rest: (global)ncogm
		
		tempname thetavar
		cap drop `thetavar'
		corr ${ncog} ${first}r, cov 
		gen `thetavar' = r(cov_12)
		di `thetavar'

		cap drop sig$ncog
		cap drop noise$ncog
		quietly sum $ncog
		gen sig$ncog = ((lambda${ncog}^2)*abs(`thetavar'))/r(Var)
		gen noise$ncog = 1-sig$ncog

		global ncogmu$period		${ncogmu$period} mu$ncog
		global ncoglambda$period	${ncoglambda$period} lambda$ncog
		global ncogsig$period		${ncogsig$period} sig$ncog
		global ncognoise$period		${ncognoise$period} noise$ncog

	}
}
/***************************************************************************************
Estimating non-cog measurement parameters and signal/noise ratios in period 4		
***************************************************************************************/
if $p4 == 1{
	* Period 4 measures - renormalizations here 
	global relation "total_team5 total_leader5 total_peerrelation5" 
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
			
			tempname a
			quietly corr ${`w'}, covariance 
			matrix `a' = r(C)
			scalar `w'var = (`a'[1,2]*`a'[1,3])/`a'[2,3]

			cap drop sig`var'
			cap drop noise`var'
			quietly sum `var'
			gen sig`var' = ((lambda`var'^2)*abs(`w'var))/r(Var)
			gen noise`var' = 1-sig`var'
			
			global `w'mu4		${`w'mu4} mu`var'
			global `w'lambda4	${`w'lambda4} lambda`var'
			global `w'sig4		${`w'sig4} sig`var'
			global `w'noise4	${`w'noise4} noise`var'
		}
		di ${`w'lambda4} 
		di ${`w'mu4}
		di ${`w'sig4}
		di ${`w'noise4}
	}
}
/*******************************************************************************
Tabulating measurment paramaters											****
*******************************************************************************/
 if $tables == 1{
 	*** Define row names for matrices and tables
	
//"School uniform expenditure" is one character too long for Stata to accept it as a global; "No. of food groups consumed" has a "." that Stata won't accept. Both will have to be added to the row label afterwards in Word 

*Period 0
global ncoglab0		"{{\b Initial (age 8) socio-emotional skill}}:\u8193?Conduct problems*" ///
					"{{\b Initial (age 8) socio-emotional skill}}:\u8193?Emotional issues*" ///
					"{{\b Initial (age 8) socio-emotional skill}}:\u8193?Hyperactivity*" ///
					"{{\b Initial (age 8) socio-emotional skill}}:\u8193?Peer problems*" ///
					"{{\b Initial (age 8) socio-emotional skill}}:\u8193?Peer pro-sociality"
					
global coglab0 		"{{\b Initial (age 8) cognitive skill}}:\u8193?Ravens test score" ///
					"{{\b Initial (age 8) cognitive skill}}:\u8193?Writing level" ///
					"{{\b Initial (age 8) cognitive skill}}:\u8193?Reading level"
					
global pcoglab0 	"{{\b Parental cognitive skill}}:\u8193?Caregiver's education" ///
					"{{\b Parental cognitive skill}}:\u8193?Literacy (first language)" ///
					"{{\b Parental cognitive skill}}:\u8193?Understands paper"
					
global pncoglab0 	"{{\b Parental socio-emotional skill}}:\u8193?Agency" ///
					"{{\b Parental socio-emotional skill}}:\u8193?Pride" ///
					"{{\b Parental socio-emotional skill}}:\u8193?Subjective wellbeing"
				
	*Period 1
global ncoglab1 	"{{\b Period 1 (age 12)}}:\u8193?Agency" ///
					"{{\b Period 1 (age 12)}}:\u8193?Pride & self-esteem"  

global coglab1 		"{{\b Period 1 (age 12)}}:\u8193?PPVT score" ///
					"{{\b Period 1 (age 12)}}:\u8193?Writing level" ///
					"{{\b Period 1 (age 12)}}:\u8193?Reading level" ///
					"{{\b Period 1 (age 12)}}:\u8193?Math test score"	
					
global ilab1 		"{{\b Period 1 (age 12) investment}}:\u8193?No food groups consumed" ///
					"{{\b Period 1 (age 12) investment}}:\u8193?School uniform expenditur" ///
					"{{\b Period 1 (age 12) investment}}:\u8193?Hours at school" ///
					"{{\b Period 1 (age 12) investment}}:\u8193?Hours studying" ///
					"{{\b Period 1 (age 12) investment}}:\u8193?Book expenditure"
		
	*Period 2
global ncoglab2 	"{{\b Period 2 (age 15)}}:\u8193?Agency" ///
					"{{\b Period 2 (age 15)}}:\u8193?Pride & self-esteem"	

global coglab2 		"{{\b Period 2 (age 15)}}:\u8193?PPVT score" ///
					"{{\b Period 2 (age 15)}}:\u8193?Cloze language test score" ///
					"{{\b Period 2 (age 15)}}:\u8193?Maths test score"
					
global ilab2 		"{{\b Period 2 (age 15)}}:\u8193?No food groups consumed" ///
					"{{\b Period 2 (age 15)}}:\u8193?School uniform expenditur"  ///
					"{{\b Period 2 (age 15)}}:\u8193?Hours at school" ///
					"{{\b Period 2 (age 15)}}:\u8193?Hours studying" ///
					"{{\b Period 2 (age 15)}}:\u8193?Book expenditure"
					
	*Period 3
global ncoglab3 	"{{\b Period 3 (age 19)}}:\u8193?Self-esteem" ///
					"{{\b Period 3 (age 19)}}:\u8193?Self-efficacy" ///
					"{{\b Period 3 (age 19)}}:\u8193?Agency"	

global coglab3  	"{{\b Period 3 (age 19)}}:\u8193?Language test score" /// 
					"{{\b Period 3 (age 19)}}:\u8193?Maths test score"
					
global ilab3 		"{{\b Period 3 (age 19)}}:\u8193?Hours at school" ///
					"{{\b Period 3 (age 19)}}:\u8193?Hours studying" ///
					"{{\b Period 3 (age 19)}}:\u8193?No food groups consumed" ///
					"{{\b Period 3 (age 19)}}:\u8193?Non-food expenditure" ///
					"{{\b Period 3 (age 19)}}:\u8193?Education expenditure"
					
	*Period 4
global relationlab4 "{{\b Period 4 (age 22) social skills}}:\u8193?Leader" ///
					"{{\b Period 4 (age 22) social skills}}:\u8193?Peers" ///
					"{{\b Period 4 (age 22) social skills}}:\u8193?Teamwork"
					
global controllab4 	"{{\b Period 4 (age 22) task effectiveness}}:\u8193?Agency" ///
					"{{\b Period 4 (age 22) task effectiveness}}:\u8193?Grit " ///
					"{{\b Period 4 (age 22) task effectiveness}}:\u8193?Conscientiousness" ///
					"{{\b Period 4 (age 22) task effectiveness}}:\u8193?Emotional stability"  
					
**** Post summary stats and store matrices to facilitate format of tables
foreach z in mu lambda sig noise{
	forval i = 0/3{
		estpost sum ${ncog`z'`i'}
		mat ncog`z'`i' = e(mean)
	}	
}
foreach z in mu lambda sig noise{
	forval i = 0/3{
		estpost sum ${cog`z'`i'}
		mat cog`z'`i' = e(mean)
	}	
}
foreach z in mu lambda sig noise{
	forval i = 1/3{
		estpost sum ${i`z'`i'}
		mat i`z'`i' = e(mean)
	}	
}
foreach z in mu lambda sig noise{
	foreach w in relation control{
		estpost sum ${`w'`z'4}
		mat `w'`z'4 = e(mean)
	}	
}
forval i=0/3{
	mat ncog`i' = (ncogmu`i'', ncoglambda`i'', ncogsig`i'', ncognoise`i'')
	mat colnames ncog`i' = "$\mu_{\theta,m,t}$" "$\lambda_{\theta,m,t}$" "$ s_{\theta,m,t}$" "$1-s_{\theta,m,t}$"
	mat rownames ncog`i' = "${ncoglab`i'}"
}
forval i=0/3{
	mat cog`i' = (cogmu`i'', coglambda`i'', cogsig`i'', cognoise`i'')
	mat colnames cog`i' = "$\mu_{\theta,m,t}$" "$\lambda_{\theta,m,t}$" "$ s_{\theta,m,t}$" "$1-s_{\theta,m,t}$"
	mat rownames cog`i' = "${coglab`i'}"
}
forval i=1/3{
	mat i`i' = (imu`i'', ilambda`i'', isig`i'', inoise`i'')
	mat colnames i`i' = "$\mu_{\theta,m,t}$" "$\lambda_{\theta,m,t}$" "$ s_{\theta,m,t}$" "$1-s_{\theta,m,t}$"
	mat rownames i`i' = "${ilab`i'}"
}
foreach w in relation control{
	mat `w'4 = (`w'mu4', `w'lambda4', `w'sig4', `w'noise4')
	mat colnames `w'4 = "$\mu_{\theta,m,t}$" "$\lambda_{\theta,m,t}$" "$ s_{\theta,m,t}$" "$1-s_{\theta,m,t}$"
	mat rownames `w'4 = "${`w'lab4}"
}

*** Investment and child human capital tables

****************
*** Table A1 ***
****************

*** Period 0
gettoken (global)firstncog0: (global)ncogmu0 
esttab matrix(ncog0, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
replace nonum nogaps plain ///
varwidth(35) nomtitles ///
collabels("{\u0956?{\sub \u0952?,{\i m,t}}}" ///
			"{\u0955?{\sub \u0952?,{\i m,t}}}" ///
			"{\i s{\plain\sub \u0952?,{\i m,t}}}" ///
			"1\u8722?{\i s{\plain\sub \u0952?,{\i m,t}}}") ///
title("{\b Table A1}\line {\i Measurement Parameters Associated with Observable Socio-Emotional Skill}\line")

*** Period 1
gettoken (global)firstncog1: (global)ncogmu1
esttab matrix(ncog1, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 2
gettoken (global)firstncog2: (global)ncogmu2
esttab matrix(ncog2, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 3
gettoken (global)firstncog3: (global)ncogmu3
esttab matrix(ncog3, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 4 - social skills
esttab matrix(relation4, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 4 - task effectiveness
esttab matrix(control4, fmt(%5.3f)) using "${tabstub}\TableA1.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) ///
note("\qj {\b Notes:} * indicates negative measures that were reversed so a higher value represented a higher level of skill. The initial and periods 1-4 represent ages 8, 12, 15, 19, and 22 respectively. From left to right the columns represent the observable measure and its estimated mean, factor loading, signal, and noise respectively. All parameters are estimated as outline in Online Appendix A.")



****************
*** Table A2 ***
****************

*** Period 0
gettoken (global)firstcog0: (global)cogmu0 
esttab matrix(cog0, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain nomtitles ///
replace nonum nogaps ///
collabels("{\u0956?{\sub \u0952?,{\i m,t}}}" ///
			"{\u0955?{\sub \u0952?,{\i m,t}}}" ///
			"{\i s{\plain\sub \u0952?,{\i m,t}}}" ///
			"1\u8722?{\i s{\plain\sub \u0952?,{\i m,t}}}") ///
title("{\b Table A2}\line {\i Measurement Parameters Associated with Observable Cognitive Skill, Parental Human Capital, and Investment}\line")

*** Period 1
gettoken (global)firstcog1: (global)cogmu1
esttab matrix(cog1, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 2
gettoken (global)firstcog2: (global)cogmu2
esttab matrix(cog2, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Period 3
gettoken (global)firstcog3: (global)cogmu3
esttab matrix(cog3, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Parental socio-emotional skill
foreach w in pcog pncog{
	foreach z in mu lambda sig noise{
		quietly estpost sum ${`w'`z'0}
		mat `w'`z'0 = e(mean)
	}
}
foreach w in pcog pncog{
	mat `w'0= (`w'mu0', `w'lambda0', `w'sig0', `w'noise0')
	mat colnames `w'0 = "$\mu_{\theta,m,t}$" "$\lambda_{\theta,m,t}$" "$ s_{\theta,m,t}$" "$1-s_{\theta,m,t}$"
	mat rownames `w'0 = "${`w'lab0}"
	gettoken (global)first`w'0: (global)`w'mu0 
}
esttab matrix(pncog0, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none)

*** Parental cognitive skill
esttab matrix(pcog0, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none) 

*** Investment (Period 1)
gettoken (global)firsti1: (global)imu1 
esttab matrix(i1, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) plain ///
append nonum nogaps nomtitles collabels(none)

*** Investment (Period 2)
gettoken (global)firsti2: (global)imu2 
esttab matrix(i2, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) ///
append nonum nogaps nomtitles plain collabels(none)

*** Investment (Period 3)
gettoken (global)firsti3: (global)imu3 
esttab matrix(i3, fmt(%5.3f)) using "${tabstub}\TableA2.rtf", ///
varwidth(35) ///
append nonum nogaps nomtitles plain collabels(none) ///
note("\qj {\b Notes:} Parental human capital is assumed to be time invariant so are measured at only one point in time. From left to right the columns represent the observable measure and its estimated mean, factor loading, signal, and noise respectively. All parameters are estimated as outlined in Online Appendix A. All expenditure variables are per dependent child in the household.")
 }
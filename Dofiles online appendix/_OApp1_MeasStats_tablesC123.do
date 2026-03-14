/*******************************************************************************
Summary stats on all measures used in estimations
Created: Jly 2020
*******************************************************************************/
clear
set more off, permanently
* File path - change as required. Remaining paths are relative
if "`c(username)'" == "alans" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\NdM Dropbox\Alan Sanchez\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
}
/*
if "`c(username)'" == "markm" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\Dropbox\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
}
*/
else{
	cd ""
}
global tabstub "Output"
* Load data and merge with new Big 5
use "Data\oc_measures_peru_R1-5", clear 
merge 1:1 childid using "Data\Big5.dta"
drop _merge
drop total_big5c5 total_big5n5 big5c_index5 big5n_index5
rename total_big5c5_test total_big5c5
rename total_big5n5_test total_big5n5
rename big5c_index5_test big5c_index5
rename big5n_index5_test big5n_index5

/***********************************************
Socio-emotional skills
***********************************************/
lab var sdqcon1 "SDQ: conduct problems"
lab var sdqhyper1 "SDQ: hyperactivity"
lab var sdqprosoc1 "SDQ: pro-sociality"
lab var sdqemo1 "SDQ: eotional regulation"
lab var sdqpeer1 "SDQ: peer relations"

lab var total_pride2 "Pride \& self-esteem"
lab var total_agency2 "Agency"

lab var total_pride3 "Pride \& self-esteem"
lab var total_agency3 "Agency"

lab var total_agency4 "Agency"
lab var total_sesteem4 "Self-esteem"
lab var total_sefficacy4 "Self-efficacy"
lab var total_peerrelation4 "Peer relationships"

/***********************************************
Socio-emotional skills at 22
***********************************************/
lab var total_agency5 "Agency"
lab var total_grit5 "Grit"
lab var total_big5n5 "Big 5 emotional stability"
lab var total_big5c5 "Big 5 conscientiousness"

lab var total_leader5 "Leadership"
lab var total_team5 "Teamwork"
lab var total_peerrelation5 "Peer relationships"

****************
*** Table C1 ***
****************

quietly estpost sum sdqcon1 sdqhyper1 sdqprosoc1 sdqemo1 sdqpeer1 ///
	total_pride2 total_agency2 ///
	total_pride3 total_agency3 ///
	total_agency4 total_sesteem4 total_sefficacy4 total_peerrelation4 ///
	total_agency5 total_grit5 total_big5n5 total_big5c5 ///
	total_leader5 total_team5 total_peerrelation5, d
est store nc

local varnames sdqcon1 sdqhyper1 sdqprosoc1 sdqemo1 sdqpeer1 total_pride2 total_agency2 total_pride3 total_agency3 total_agency4 total_sesteem4 total_sefficacy4 total_peerrelation4 total_agency5 total_grit5 total_big5n5 total_big5c5 total_leader5 total_team5 total_peerrelation5 

mat nc_unique = J(1,20,.)

mat colnames nc_unique = `varnames'

local i = 1

foreach var of local varnames {
	egen tag_`var' = tag(`var')
	egen unique_`var' = sum(tag_`var')
	mkmat unique_`var' in 1/1
	
	mat nc_unique[1,`i'] = unique_`var'  
	
	drop tag_`var' unique_`var'
	local ++i
}

estadd matrix nc_unique = nc_unique: nc

esttab nc using "${tabstub}\TableC1.rtf", replace wide ///
cells("mean(fmt(3)) sd(fmt(3)) max(fmt(0)) min(fmt(0)) nc_unique(fmt(0))") ///
nomtitles collabels("Mean" "sd" "Max." "Min." "Unique values") nonum noobs label ///
refcat(sdqcon1 "{\afs20 \u8193?}\line{\b Age 8}" ///
		total_pride2 "{\afs20 \u8193?}\line{\b Age 12}" ///
		total_pride3 "{\afs20 \u8193?}\line{\b Age 15}" ///
		total_agency4 "{\afs20 \u8193?}\line{\b Age 19}" ///
		total_agency5 "{\afs20 \u8193?}\line{\b Age 22: task effectiveness}" ///
		total_leader5 "{\afs20 \u8193?}\line{\b Age 22: social skills}", nolabel) ///
varlabels(total_pride2 "Pride & self-esteem" ///
		total_pride3 "Pride & self-esteem" ///
		sdqemo1 "Emotional regulation*" ///
		sdqcon1 "Conduct issues*" ///
		sdqhyper1 "Hyperactivity*" ///
		sdqpeer1 "Peer problems*" ///
		sdqprosoc1 "Pro-sociality" ,prefix(\u8193?)) ///
varwidth(25) ///
title("{\b Table C1}\line {\i Summary Statistics of Observable Socio-Emotional Skill Measures Used in Estimating Investment and Production Functions}\line") ///
note("\qj {\b Notes:} The measures in this table are those of socio-emotional skill used to estimate the human capital production and investment functions. From left to right, the columns contain the aspect of socio-emotional skill the measures capture, their sample mean and standard deviation (sd), and the maximum, minimum and number of unique values in the sample. A * indicates the order of a measure was reversed from negative to positive so that a higher value indicates more skill.")

/***********************************************
Cognitive skills
***********************************************/
lab var ravens1 "Ravens score"
lab var levlwrit1 "Writing level"
lab var levlread1 "Reading level"

lab var score_math2 "Math score"
lab var score_ppvt2 "PPVT score"
lab var levlwrit2 "Writing level"
lab var levlread2 "Reading level"

lab var math_co3 "Math score" 
lab var ppvt_co3 "PPVT score"
lab var cloze_co3 "Cloze score"

lab var lang_raw4 "Language score"
lab var maths_raw4 "Math score"

****************
*** Table C2 ***
****************

quietly estpost sum ravens1 levlwrit1 levlread1 score_math2 score_ppvt2 ///
	levlwrit2 levlread2 math_co3 ppvt_co3 cloze_co3 maths_raw4 lang_raw4, d
est store cog 

local varnames ravens1 levlwrit1 levlread1 score_math2 score_ppvt2 levlwrit2 levlread2 math_co3 ppvt_co3 cloze_co3 maths_raw4 lang_raw4 

mat cog_unique = J(1,12,.)

mat colnames cog_unique = `varnames'

local i = 1

foreach var of local varnames {
	egen tag_`var' = tag(`var')
	egen unique_`var' = sum(tag_`var')
	mkmat unique_`var' in 1/1
	
	mat cog_unique[1,`i'] = unique_`var'  
	
	drop tag_`var' unique_`var'
	local ++i
}

estadd matrix cog_unique = cog_unique: cog

esttab cog using "${tabstub}\TableC2.rtf", replace wide  ///
cells("mean(fmt(3)) sd(fmt(3)) max(fmt(0)) min(fmt(0)) cog_unique(fmt(0))") label  ///
nomtitles collabels("Mean" "sd" "Max." "Min." "Unique values") ///
refcat(ravens1 "{\afs20 \u8193?}\line{\b Age 8}" ///
		score_math2 "{\afs20 \u8193?}\line{\b Age 12}" ///
		math_co3 "{\afs20 \u8193?}\line{\b Age 15}" ///
		maths_raw4 "{\afs20 \u8193?}\line{\b Age 19}", nolabel) ///
varlabels( ,prefix(\u8193?)) ///
noobs nonum ///
title("{\b Table C2}\line {\i Summary Statistics of Observable Cognitive Skill Measures Used in Estimating Investment and Production Functions}\line") ///
note("\qj {\b Notes:} The measures in this table are those of cognitive skill used to estimate the human capital production and investment functions. From left to right, the columns contain either the name of the test through which skill was measured of the aspect of cognition the test captured, their sample mean and standard deviation (sd), and the maximum, minimum and number of unique values in the sample.")


/***********************************************
Investments 
***********************************************/
forval i=2/4{
	lab var foodgroups`i' "Food groups"
	lab var hstudy`i' "Hours studying"
	lab var hsleep`i' "Hours sleeping"
	lab var hschool`i' "Hours in school"
}
lab var mealspday4 "Meals per day"
lab var educexp4 "Education expenditure"
forval i=1/3{
	global period = `i'
	run "Programs\inputs.do"
}
lab var booksexp_pp2 "Per-child expenditure on books"
lab var uniformexp2 "Per-child expenditure on uniforms"

lab var booksexp_pp3 "Per-child expenditure on books"
lab var uniformexp_pp3 "Per-child expenditure on uniforms"

lab var nfoodexp_pp4 "Per-child non-food expenditure"
lab var educexp4 "Educational expenditure"


/***********************************************
Endowments
***********************************************/
lab var careed2 "Education"
lab var literspc1 "Can read newspaper"
lab var carelita2 "Can understand things written in Spanish"

lab var tot_cagency2 "Agency"
lab var tot_cpride2 "Pride \& self-esteem"
lab var cladder2 "Cantril's ladder"

****************
*** Table C3 ***
****************

quietly estpost sum booksexp_pp2 uniformexp_pp2 hstudy2 hschool2 ///
	booksexp_pp3 uniformexp_pp3 foodgroups3 hstudy3 hschool3 ///
	educexp4 nfoodexp_pp4 foodgroups4 hschool4 hstudy4 ///
	tot_cagency2 tot_cpride2 cladder2 ///
	careed2 literspc1 carelita2, d
est store inv 

local varnames booksexp_pp2 uniformexp_pp2 hstudy2 hschool2 booksexp_pp3 uniformexp_pp3 foodgroups3 hstudy3 hschool3 educexp4 nfoodexp_pp4 foodgroups4 hschool4 hstudy4 tot_cagency2 tot_cpride2 cladder2 careed2 literspc1 carelita2 

mat inv_unique = J(1,20,.)

mat colnames inv_unique = `varnames'

local i = 1

foreach var of local varnames {
	egen tag_`var' = tag(`var')
	egen unique_`var' = sum(tag_`var')
	mkmat unique_`var' in 1/1
	
	mat inv_unique[1,`i'] = unique_`var'  
	
	drop tag_`var' unique_`var'
	local ++i
}

local is 1 2 5 6 10 11

foreach i of local is  {
    mat inv_unique[1,`i'] = . 
}


estadd matrix inv_unique = inv_unique: inv

esttab inv using "${tabstub}\TableC3.rtf", replace wide ///
cells("mean(fmt(3)) sd(fmt(3)) max(fmt(0)) min(fmt(0)) inv_unique(fmt(0))") label  ///
nomtitles collabels("Mean" "sd" "Max." "Min." "Unique values") ///
refcat(booksexp_pp2 "{\afs20 \u8193?}\line{\b Age 12}" ///
		booksexp_pp3 "{\afs20 \u8193?}\line{\b Age 15}" ///
		educexp4 "{\afs20 \u8193?}\line{\b Age 19}" ///
		tot_cagency2 "{\afs20 \u8193?}\line{\b Parental socio-emotional skill}" ///
		careed2 "{\afs20 \u8193?}\line{\b Parental cognitive skill}", nolabel) ///
noobs nomtitles nonum ///
varlabels(tot_cpride2 "Pride & agency" ///
		uniformexp_pp2 "Per-child expenditure on uniforms", prefix(\u8193?)) ///
varwidth(35) ///
title("{\b Table C3}\line {\i Summary Statistics of Observable Investment and Parental Skill Measures Used in Estimating Investment and Production Functions}\line") ///
note("\qj {\b Notes:} The measures in this table are those of investment and parental human capital used to estimate the human capital production and investment functions. From left to right, the columns contain a descriptions of the investment or human capital measures, their sample mean and standard deviation (sd), and the maximum, minimum and number of unique values in the sample. Variables with missing number of unique values are continuous.")
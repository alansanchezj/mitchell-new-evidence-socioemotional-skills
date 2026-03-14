/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Looking at the correlation between socio-emotional skill at 22 and outcomes (Table 8)
Created: Dec. 2019
*******************************************************************************/
set more off, permanently
* File path - change as required. Remaining paths are relative
if "`c(username)'" == "alans" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\NdM Dropbox\Alan Sanchez\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
	global graphstub "Output"
	global tabstub "Output"
}
else{
	cd ""
}
* Load data & merge in outcome data
use "Data\sample_postest.dta", clear
merge 1:1 childid using "Data\Other Outcomes_23Mar2020.dta"
drop _merge 

* Merge with new big 5 data
merge 1:1 childid using "Data\Big5_v13.dta"
drop _merge
drop total_big5c5 total_big5n5
rename total_big5c5_test total_big5c5
rename total_big5n5_test total_big5n5

*** Basic regressions of skills on outcomes ***

*** Clean some variables 
* Sex 
replace chsex5 = chsex5-1
lab def sexlab 0 "Male" 1 "Female" 
lab val chsex5 sexlab
lab var chsex5 "Female"
* Ethnicity 
cap drop eth*
tab chethnic5, gen(eth)
lab var eth1 "White"
lab var eth2 "Mstizo"
lab var eth3 "Amazon native"
lab var eth4 "Negro"

* Language 
tab chlang5, gen(lang)
lab var lang1 "Spanish"
lab var lang2 "Quecha"
gen lang6 = (lang3 == 1 | lang4 == 2 | lang5 == 3) & chlang5 != .
lab var lang6 "Other"

* Inputs
lab var total_agency5r "\toprule$\ln{H^t_{s,T+1}}$"
*lab var total_sesteem5r "$\ln{H^s_{s,T+1}}$"
lab var total_leader5r "\$\ln{H^s_{s,T+1}}$"
lab var maths_raw4r "$\ln{H_{c,T}}$"
lab var wi5 "Wealth index"

est clear 

/***********************************************************
Outcomes 
***********************************************************/
*** Smoking ***
ivreg smoke1 chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store smokeIV
quietly sum smoke1
estadd scalar mu = r(mean)

quietly quietly ivprobit smoke1 i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
 margins, dydx(total_agency5r total_leader5r maths_raw4r chsex5 wi5) predict(pr)
est store smoke

*** Drinking ***
ivreg drinking1 chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store drinkIV
quietly sum drinking1
estadd scalar mu = r(mean)

quietly ivprobit drinking1 i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		 total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans
est store drink

*** Drugs ***
ivreg drugs_any chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store drugsIV
quietly sum drugs_any
estadd scalar mu = r(mean)

quietly ivprobit drugs_any i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		 total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans 
est store drugs

*** Unprotected sex ***
ivreg risky_sex chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store sexIV
quietly sum risky_sex
estadd scalar mu = r(mean)

quietly ivprobit risky_sex i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		 total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans
est store risky

*** Carrying a weapon ***
ivreg weapon chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store weaponIV
quietly sum weapon
estadd scalar mu = r(mean)

quietly ivprobit weapon i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans
est store weapon

*** Gang behaviour ***
ivreg criminal1 chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store criminalIV
quietly sum criminal1
estadd scalar mu = r(mean)

quietly ivprobit criminal1 i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4)
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans
est store criminal

*** Teen pregnancy ***
ivreg everbirth_r5r4 chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4), vce(bootstrap, reps(1000)) 
est store birthIV 
quietly sum everbirth_r5r4
estadd scalar mu = r(mean)

quietly ivprobit everbirth_r5r4 i.chsex5 wi5 (total_agency5r total_leader5r maths_raw4r = ///
		total_big5c5 total_grit5 total_peerrelation5 total_team5 lang_raw4) 
quietly estadd margins, dydx(total_agency5r total_leader5r maths_raw4r i.chsex5 wi5) predict(pr) atmeans
est store birth


/************************************************************************************
Put together tables  
*************************************************************************************/

***************
*** Table 6 ***
*************** 

esttab smokeIV drinkIV drugsIV sexIV weaponIV criminalIV birthIV using "${tabstub}\Table6.rtf", replace ///
keep(total_agency5r total_leader5r maths_raw4r chsex5 wi5) ///
label b(3) p(3) eqlabels(none) star(* 0.10 ** 0.05 *** 0.01) ///
nonum noobs varwidth(25) ///
collabels(none) ///
cells("b(fmt(3)star)" "se(fmt(3)par)") ///
stats(mu N, fmt(%3.2f %3.0f) labels("\bottomrule Outcome mean" "N")) ///
mtitles("(1){\line Smoked\line{\afs20 \u8193?}}" ///
		"(2){\line Drank\line{\afs20 \u8193?}}" ///
		"(3){\line Drugs\line{\afs20 \u8193?}}" ///
		"(4){\line Unprotected sex\line{\afs20 \u8193?}}" ///
		"(5){\line Carried weapon\line{\afs20 \u8193?}}" ///
		"(6){\line Gang\line{\afs20 \u8193?}}" ///
		"(7){\line Child\line{\afs20 \u8193?}}") ///
varlabels(total_leader5r "ln {\i H{\super\expnd-24 s}{\sub s,T{\plain\sub +1}}}" ///
		total_agency5r "ln {\i H{\super\expnd-24 t}{\sub s,T{\plain\sub +1}}}" ///
		maths_raw4r "ln {\i H{\sub c,T}}") ///
refcat(total_leader5r "\afs20 \u8193?" ///
		maths_raw4r "\afs20 \u8193?" ///
		wi5 "\afs20 \u8193?" ///
		chsex5 "\afs20 \u8193?" , nolabel) ///
title("{\b Table 6}\line {\i Estimates of the Impact of Age 22 Socio-Emotional Skills on Risky Behaviors}\line") ///
note("\qj {\b Notes:} *, ** and *** denote statistical significance it 10%, 5% and 1% respectively. Standard errors in parentheses are calculated from 1,000 bootstrap replications. The outcomes in each column are whether not an individual has: smoked least once a month (1); ever been drunk (2); ever taken illegal drugs (3); ever had unprotected sex (4); carried a weapon in the last month (5); been arrested for being part of a gang or carrying a weapon in the last month (6); or has a child or is pregnant at age 22 (7). Female is a dummy indicating whether or not an individual is female, and the wealth index a measure of the material resources of the family which ranges from 0 to 1, constructed as the average of three sub-indices measuring housing quality, access to services and ownership of a range of durable goods. See Briones (2017) for detail. The number of observations differs in across columns due to missing responses.")



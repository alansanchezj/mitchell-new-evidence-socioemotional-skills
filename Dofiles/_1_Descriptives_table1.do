/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Selection of demographics and household characteristics (Table 1)
Created: Dec. 2019
*******************************************************************************/
clear
set more off, permanently
* File path - change as required. Remaining paths are relative
if "`c(username)'" == "alans" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\NdM Dropbox\Alan Sanchez\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
}
else{
	cd ""
}

/**********************************************************
PUT DATA TOGETHER
**********************************************************/
use "Data\sample_postest.dta", clear 
keep childid invsamp* prodsamp*
* Create baseline indicators
gen invsamp0 = 1
gen prodsamp0 = 1

/* Reshape the data to make tabulation easier. These were the first and last vars
in the dataset */
unab mystubs: *1

* Trim the numbers from the end of the variable names
foreach w of local mystubs{
	local stubs `"`stubs' `=substr("`w'",1,length("`w'")-1)'"' 
} 
di "`stubs'"
reshape long `stubs', i(childid) j(round)
lab val round
replace round = round+1
gen yc=0

* Merge with the constructed panel, income, and hh composition data
merge 1:1 childid round using "Data\peru_constructed.dta"
keep if yc==0
drop _merge
keep childid round invsamp prodsampncog chsex caresex caredu careage carerel chlang chethnic typesite wi 
rename prodsampncog prodsamp 

preserve
	use "Data\HH composition.dta", clear
	reshape long age017 age517, i(childid) j(round, string)
	replace round = substr(round,3,.)
	destring round, replace
	tempfile hhcomp
	save "`hhcomp'"
restore
preserve
	use "Data\YLIncome_allrounds.dta", clear
	keep if yc==0
	reshape long ylinc hhinc, i(childid) j(round, string)
	replace round = substr(round,5,.)
	destring round, replace
	tempfile inc
	save "`inc'"
restore

merge 1:1 childid round using "`hhcomp'"
keep if _merge == 3
drop _merge
merge 1:1 childid round using "`inc'"
drop _merge

*** Create USD income
gen hhinc_usd = .
replace hhinc_usd = hhinc/3.517 if round == 1
replace hhinc_usd = hhinc/3.274 if round == 2
replace hhinc_usd = hhinc/3.012 if round == 3


/**********************************************************
FORMAT VARIABLES TO BE OUTPUT IN TABLES
**********************************************************/

* categorical: chsex, caresex, caredu, chlang, chethnic
* Tidy sex labels
cap lab drop sexlab
lab def sexlab 0 " Male" 1 " Female"
foreach v of varlist caresex chsex{
	replace `v' = `v'-1
	lab val `v' sexlab 
}
lab var caresex "Female caregiver"
lab var chsex "Female cohort member"
* Urban/rural
replace typesite = typesite - 1
lab def urbanlab 0 "Urban" 1 "Rural"
lab val typesite urbanlab
* Combine some education categories
gen careducat = 0 if caredu == 0
replace careducat = 1 if caredu >=1 & caredu <= 6
replace careducat = 2 if caredu >6 & caredu <=11
replace careducat = 3 if caredu == 13 | caredu == 14
replace careducat = 4 if caredu == 15 | caredu == 16
replace careducat = 5 if caredu == 28
cap lab drop edlab
lab def edlab 0 "None" 1 "Primary" 2 "Secondary" ///
	3 "Technical/Vocational" 4 "University" 5 "Adult literacy"
lab val careducat edlab
* Re-jig and Combine some ages 
bysort childid round: replace careage = careage[1] if _n>1
gen careagecat = 1 if careage <=25
replace careagecat = 2 if careage >25 & careage <= 40
replace careagecat = 3 if careage >40 & careage <= 50
replace careagecat = 4 if careage >50 & careage <=60
replace careagecat = 5 if careage >60 & careage != .
lab def agelab 1 "15-25" 2 "26-40" 3 "41-50" ///
	4 "51-60" 5 "Older than 61"  
lab val careagecat agelab 
* Categorise the household composition variables
gen age017cat = 0 if age017 == 0
replace age017cat = 1 if age017 == 1
replace age017cat = 2 if age017>=2 & age017 <=4
replace age017cat = 3 if age017>=5 & age017 != .
lab def dependlab 0 "None" 1 "One" 2 "Between 2 and 4" ///
	3 "More than 5" 
lab val age017cat dependlab
* Relabel some language and ethnicity categories
cap lab drop ethlab
lab def ethlab 31 "White" 32 "Mestizo" 33 "Native of the Amazon" ///
	34 "Black"
lab val chethnic ethlab
cap lab drop langlab
lab def langlab 31 "Spanish" 32 "Quechua" 34 "Native of Jungle" ///
	35 "Spanish and Quechua" 37 "Nomatsiguenga"
lab val chlang langlab 
* Language 
tab chlang, gen(lang)
lab var lang1 " Spanish"
lab var lang2 " Quecha"
gen lang7 = (lang3 == 1 | lang4 == 2 | lang5 == 3) & chlang != .
lab var lang7 " Other"

* Create dummies for categories
foreach v of varlist careducat careagecat age017cat chlang chethnic{
	quietly tab `v', gen(`v')
}
foreach v of varlist careducat1-careducat6{
		local lab: variable label `v'
		local varlab = substr("`lab'",12,.)
		lab var `v' "`varlab'"
}
foreach v of varlist careagecat1-careagecat5{
		local lab: variable label `v'
		local varlab = substr("`lab'",13,.)
		lab var `v' "`varlab'"
}
foreach v of varlist age017cat1-age017cat4{
		local lab: variable label `v'
		local varlab = substr("`lab'",12,.)
		lab var `v' "`varlab'"
}
foreach v of varlist chlang1-chlang5{
		local lab: variable label `v'
		local varlab = substr("`lab'",9,.)
		label var `v' "`varlab'"
}
foreach v of varlist chethnic1-chethnic4{
		local lab: variable label `v'
		local varlab = substr("`lab'",11,.)
		label var `v' "`varlab'"
}
* Store these: for now don't include main language or caregivers age. //careagecat1-careagecat5 chlang1-chlang5//
global categories "caresex chsex careducat1-careducat6 age017cat1-age017cat4 lang1 lang2 lang7"

* Label the discrete/continuous variables: wi, hhinc, ylinc
lab var wi "\toprule Wealth index"
lab var hhinc_usd "Household income (USD)"
lab var ylinc "Cohort member income"

*** Table 
estpost sum $categories if round == 1
est store cat1
forval i=2/5{
	estpost sum $categories if prodsamp == 1 & round == `i'
	est store cat`i'
}
estpost sum wi if round == 1
est store wealth1
forval i=2/5{
	estpost sum wi if prodsamp == 1 & round == `i'
	est store wealth`i'
}
estpost sum hhinc_usd if round == 1, d
est store income1
forval i=2/3{
	estpost sum hhinc_usd if prodsamp == 1 & round == `i', d
	est store income`i'
}

esttab wealth1 wealth2 wealth3 wealth4 wealth5 using "Output\Table1.rtf", ///
replace cells(mean(fmt(2)) sd(fmt(2)par)) label collabels(none) nonumbers noobs plain ///
mtitle("(1)\line {\i Age 8\line{\afs20 \u8193?}}" ///
		"(2)\line {\i Age 12\line{\afs20 \u8193?}}" ///
		"(3)\line {\i Age 15\line{\afs20 \u8193?}}" ///
		"(4)\line {\i Age 19\line{\afs20 \u8193?}}" ///
		"(5)\line {\i Age 22\line{\afs20 \u8193?}}") ///
title("{\b Table 1}\line {\i Sample Characteristics in the Age Eight Baseline and Estimation Samples}\line") 

esttab income1 income2 income3 using "Output\Table1.rtf", ///
append cells(mean(fmt(%8.0fc)) sd(fmt(%8.0fc) par) p50(fmt(%8.0fc))) label collabels(none) plain  ///
nomtitles nonumbers noobs

esttab cat1 cat2 cat3 cat4 cat5 using "Output\Table1.rtf", ///
append cells(mean(fmt(2))) label collabels(none) nonumbers nomtitles  plain  ///
refcat(careducat1 "{\b Caregiver's education}" ///
		careagecat1 "{\b Caregiver's age at birth}" ///
		age017cat1 "{\b Dependent children}" ///
		chlang1 "{\b Main language}" ///
		lang1 "{\b Language}", nol) ///
varlabels(lang1 "Spanish" ///
		lang2 "Quecha" ///
		lang7 "Other") ///
stats(N, fmt(%10.0g) labels("N")) ///
note("\qj {\b Notes:} All numbers are proportions. The sex, education, and age of the caregiver were not recorded at age 19 or 22, nor was the income of the cohort member's household. {\i Dependent children} refers to the number of children aged between 0 and 17 years in the household of the cohort member. Standard errors for the mean wealth index and household income are in parentheses. For household income, the median value is also shown below the mean and its standard deviation.")

/*******************************************************************************
Paper: Title TBD,
		Mark Mitchell, Catherine Porter, Alan Sanchez, 2019.
Contents: Exploratory Factor Analyses(EFA) in each period
Created: Dec. 2019
Structure:

 For investments and child human capital in each period and \textbf{Initial} endowments:
	1. Run unconstrained EFA on estimated correlation matrix
		- This incovles estimating polychoric correlation
		matrices between observed data for categorical variables
	2. Run parallel analysis and plot the eigenvalues
	3. Run constrained EFA, roate FLs, and determine groupings 
	4. Note how investments are allocated, and those that are discarded

*******************************************************************************/

clear
set more off, permanently
* File path - change as required. Remaining paths are relative
if "`c(username)'" == "alans" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\NdM Dropbox\Alan Sanchez\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
	global graphstub "Output"
	global tabstub "Output"
}
/*
if "`c(username)'" == "markm" | "`c(username)'" == "nxb19103"{
	cd "C:\Users\\`c(username)'\Dropbox\ESRC_Skills_StrucModel\JHR_resubmission\Final for publication\Replication files"
	global graphstub "Output"
	global tabstub "Output"
}
*/
else{
	cd ""
}
* Load data
use "Data\oc_measures_peru_R1-5", clear

/************************************************************
ENDOWMENTS
************************************************************/

*** Define globals and label variables 
global pcog "careed2 carelita2 literspc1" //mumed2 least uniqueness//
global pncog "tot_cagency2 cladder2 tot_cpride2 " //tot_ctrust2 tot_csocial2 total_csocial2 least uniqueness//

lab var careed2 "Caregiver's education"
lab var carelits2 "Caregiver understands written spanish"
lab var carelita2 "Caregiver understands written language"
lab var literspc1 "Caregiver can read"
lab var tot_cagency2 "Caregiver's agency"
lab var tot_csocial2 "Caregiver's social"
lab var tot_cpride2 "Caregiver's pride"
lab var tot_ctrust2 "Caregiver's trust"
lab var cladder2 "Caregiver's Subjective wellbeing"

*** cognition
quietly factor $pcog , ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)3.5, nogrid) xlab(1(1)3.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_pcog.pdf", replace

quietly factor $pcog, ipf factors(1)
rotate, oblique quartimin 
esttab using "${tabstub}\EFA_PHC.tex", frag plain replace nonum nomtitles label ///
	cells("r_L[1](transpose) r_L[2](transpose) Psi") collabels("Factor 1" "Factor 2" "Uniqueness") ///
	stats(N, label("\addlinespace[3pt]\emph{N}") fmt(%3.0f) ) ///
	refcat(careed2 "\underline{Cognition}", nol) alignment(c)


*** non-cognitive skill
quietly factor $pncog, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(0.5)1, nogrid) xlab(1(1)5) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_pncog.pdf", replace

quietly factor $pncog, ipf factors(2)
rotate, oblique quartimin 
esttab using "${tabstub}\EFA_PHC.tex", frag plain append nonum nomtitles label ///
	cells("r_L[1](transpose) r_L[2](transpose) Psi") ///
	stats(N, label("\addlinespace[3pt]\emph{N}") fmt(%3.0f)) ///
	refcat(tot_cagency2 "\underline{Socio-emotional skill}", nol) alignment(c)


*** Both jointly
quietly factor $pncog $pcog, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(0.5)1, nogrid) xlab(1(1)5) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_PHC.pdf", replace

quietly factor $pncog $pcog, ipf factors(2)
rotate, oblique quartimin 
esttab using "${tabstub}\EFA_PHC.tex", frag plain replace nonum nomtitles label ///
	cells("r_L[1](transpose) r_L[2](transpose) Psi") collabels("Factor 1" "Factor 2" "Uniqueness") ///
	stats(N, label("\addlinespace[3pt]\emph{N}") fmt(%3.0f) ) ///
	refcat(tot_cagency2 "\toprule" careed2 "\addlinespace[10pt]", nol) alignment(c)


/************************************************************
INVESTMENTS 
************************************************************/
* Create per person expenditures:
foreach w in books food nfood uniform{
	forval i=2/3{
		cap drop `w'exp_pp`i'
		gen `w'exp_pp`i' = `w'exp`i'/age017_r`i'
	}
}
forval i=2/3{
	lab var booksexp_pp`i' "Per child book expenditure"
}
forval i=2/3{
	lab var uniformexp_pp`i' "Per child uniform expenditure"
}

foreach w in food nfood{
	cap drop `w'exp_pp4
	gen `w'exp_pp4 = `w'exp4/age017_r4
}
forval i=2/4{
	lab var foodexp_pp`i' "Per child food expenditure"
}
forval i=2/4{
	lab var nfoodexp_pp`i' "Per child non-food expenditure"
}

*** Set globals and label variables
global inv1 "booksexp_pp2 uniformexp_pp2 nfoodexp_pp2 hstudy2 hschool2 foodgroups2" //booksexp2 least uniqueness  //
global inv2 "booksexp_pp3 uniformexp_pp3 nfoodexp_pp2 hstudy3 hschool3 foodgroups3" //booksexp3 least uniqueness//
global inv3 "educexp4 nfoodexp4 hstudy4 hschool4 foodgroups4" //foodgroups4 least uniqueness (if considering 2 factors)//

forval i=2/4{
	lab var foodgroups`i' "Food groups"
	lab var hstudy`i' "Hours studying"
	lab var hsleep`i' "Hours sleeping"
	lab var hschool`i' "Hours in school"
}
lab var mealspday4 "Meals per day"
lab var educexp4 "Education expenditure"

*** Period 1
factor $inv1, ipf
quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(0.5)2, nogrid) xlab(1(1)7.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ")  legend(region(lcolor(white)))
graph export "${graphstub}\eigen_inv1.pdf", replace

*** Period 2
factor $inv2, ipf
fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)2.5, nogrid) xlab(1(1)8.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ")  legend(region(lcolor(white)))
graph export "${graphstub}\eigen_inv2.pdf", replace


*** Period 3
factor $inv3, ipf

fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(0.5)2, nogrid) xlab(1(1)7.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ")  legend(region(lcolor(white)))
graph export "${graphstub}\eigen_inv3.pdf", replace


****************
*** Table C5 ***
****************

*** Period 1
quietly factor $inv1, ipf factors(1) 
rotate, oblique quartimin 
esttab using "${tabstub}\TableC5.rtf", replace ///
nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") ///
collabels("Factor 1" "Uniqueness") ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
refcat(booksexp_pp2 "{\afs20 \u8193?}\line{\b Age 12}", nolabel) ///
varwidth(35) varlabels( ,prefix(\u8193?)) ///
title("{\b Table C5}\line {\i Factor Loadings and Unique Variance of Observable Investment Measures}\line")

*** Period 2
factor $inv2, ipf factors(1)
rotate, oblique quartimin 
esttab using "${tabstub}\TableC5.rtf", append /// 
nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") collabels(none) ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
varwidth(35) varlabels(,prefix(\u8193?)) ///
refcat(booksexp_pp3 "{\afs20 \u8193?}\line{\b Age 15}", nol) 
	
*** Period 3
factor $inv3, ipf factors(1)
rotate, oblique quartimin 
esttab using "${tabstub}\TableC5.rtf", append ///
nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") collabels(none) ///
stats(N, label("{\i N}") fmt(%3.0f)) ///
refcat(educexp4 "{\afs20 \u8193?}\line{\b Age 19}", nol) ///
varwidth(35) varlabels(,prefix(\u8193?)) ///
note("\qj {\b Notes:} The table contains rotated factor loadings and the proportion of variance in each investment measure not shared with all others after retaining one factors from an initial exploratory factor analysis. One factor was retained based on the assumption the measures proxy one latent investment and the rules-of-thumb for factor retention proposed by Kaiser (1960), Horn (1965), and Cattell (1966). Factor loadings were obtained through an oblique quartimin rotation.")
	


/************************************************************
HUMAN CAPITAL PERIODS 1-3	
************************************************************/

global cog0 "ravens1 levlwrit1 levlread1"
global ncog0 "sdqcon1 sdqemo1 sdqhyper1 sdqpeer1 sdqprosoc1" //sdqcon1 least uniqueness//
* Labels 
lab var ravens1 "Ravens test score"
lab var levlwrit1 "Writing level"
lab var levlread1 "Reading level"
lab var sdqcon1 "SDQ conduct problems"
lab var sdqemo1 "SDQ emotional symptoms"
lab var sdqhyper1 "SDQ hyperactivity"
lab var sdqpeer1 "SDQ peer problems"
lab var sdqprosoc1 "SDQ prosociality"

global cog1 "score_math2 score_ppvt2" // levlwrit2 levlread2
global ncog1 "total_agency2 total_pride2 ladder_current2_man" // total_social2 total_trust2. total_pride2 least uniqueness//
* Labels 
lab var score_math2 "Maths test score"
lab var score_ppvt2 "PPVT score"
lab var levlwrit2 "Writing level"
lab var levlread2 "Reading level" 
lab var total_agency2 "Agency"
lab var total_trust2 "Trust"
lab var total_pride2 "Pride"
lab var total_social2 "Social"
lab var ladder_current2 "Cantril's ladder"

global cog2 "math_co3 ppvt3 cloze3"
global ncog2 "total_agency3 total_pride3 ladder_current3 sdqemo3" //total_social3 ladder_current3 total_trust3 total_pride3 least uniqueness//
* Labels 
lab var math_co3 "Maths test score"
lab var ppvt3 "PPVT score"
labe var cloze3 "Cloze test score"
lab var total_agency3 "Agency"
lab var total_trust3 "Trust"
lab var total_pride3 "Pride"
lab var total_social3 "Social"
lab var ladder_current3 "Cantril's ladder"

global cog3 "maths_raw4 lang_raw4"
global ncog3 "total_agency4 total_sefficacy4 total_sesteem4 total_peerrelation4 ladder_current4 sdqemo4" //total_prelation4 ladder_current4 total_pride4 total_sesteem4 least uniqueness//
* Labels
lab var maths_raw4 "Maths test score"
lab var lang_raw4 "Language test score"
lab var total_agency4 "Agency"
lab var total_pride4 "Pride"
lab var ladder_current4 "Cantril's ladder"
lab var total_sefficacy4 "Self-efficacy"
lab var total_sesteem4 "Self-esteem"
lab var total_prelation4 "Parental relationship"
lab var total_peerrelation4 "Peer relationships"

*** Period 0
quietly factor $ncog0 $cog0, ipf
*paran $ncog0 $cog0, factor(ipf) graph

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(0.5)2, nogrid) xlab(1(1)8.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_hc0.pdf", replace

*** Period 1
quietly factor $ncog1 $cog1, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)2.5, nogrid) xlab(1(1)8.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_hc1.pdf", replace

*** Period 2
quietly factor $ncog2 $cog2, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)2.5, nogrid) xlab(1(1)8.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_hc2.pdf", replace

*** Period 3
quietly factor $ncog3 $cog3, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)3, nogrid) xlab(1(1)9.25) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_hc3.pdf", replace

****************
*** Table C4 ***
****************

*** Period 0
quietly factor $ncog0 $cog0, ipf factors(2)
rotate, oblique quartimin 

esttab using "${tabstub}\TableC4.rtf", replace nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) r_L[2](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") ///
collabels("Factor 1" "Factor 2" "Uniqueness") ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
refcat(sdqcon1 "{\afs20 \u8193?}\line{\b Age 8}", nol) ///
varwidth(35) varlabels(sdqcon1  "Conduct issues" ///
						sdqemo1 "Emotional symptoms" ///
						sdqhyper1 "Hyperactivity" ///
						sdqpeer1 "Peer problems" ///
						sdqprosoc1 "Prosociality" ,prefix(\u8193?)) ///
title("{\b Table C4}\line {\i Factor Loadings and Unique Variance of Observable Cognitive and Socio-Emotional Skill Measures}\line")

*** Period 1
factor $ncog1 $cog1, ipf factors(2)
rotate, oblique quartimin 

esttab using "${tabstub}\TableC4.rtf", append nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) r_L[2](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") collabels(none) ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
varwidth(35) varlabels( ,prefix(\u8193?)) ///
refcat(total_agency2 "{\afs20 \u8193?}\line{\b Age 12}", nol) 

*** Period 2
quietly factor $ncog2 $cog2, ipf factors(2)
rotate, oblique quartimin 

esttab using "${tabstub}\TableC4.rtf", append nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) r_L[2](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") collabels(none) ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
varwidth(35) varlabels(sdqemo3 "Emotional problems" ,prefix(\u8193?)) ///
refcat(total_agency3 "{\afs20 \u8193?}\line{\b Age 15}", nol) 

*** Period 3
quietly factor $ncog3 $cog3, ipf factors(2)
rotate, oblique quartimin 

esttab using "${tabstub}\TableC4.rtf", append nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) r_L[2](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") collabels(none) ///
stats(N, label("{\i N}") fmt(%3.0f)) ///
refcat(total_agency4 "{\afs20 \u8193?}\line{\b Age 19}", nol) ///
varwidth(35) varlabels(sdqemo4 "Emotional problems" ,prefix(\u8193?)) ///
note("\qj {\b Notes:} The table contains rotated factor loadings and the proportion of variance in each cognitive and socio-emotional skill measure not shared with all others after retaining two factors from an initial exploratory factor analysis. Two factors were retained based on the assumption the measures proxy two latent concepts, socio-emotional and cognitive skill and the rules-of-thumb for factor retention proposed by Kaiser (1960), Horn (1965), and Cattell (1966). Factor loadings were obtained through an oblique quartimin rotation.")

/************************************************************
SOCIO-EMOTIONAL SKILL PERIOD 4
************************************************************/

* Merge with new big 5 data
merge 1:1 childid using "Data\Big5.dta"
drop _merge
drop total_big5c5 total_big5n5
rename total_big5c5_test total_big5c5
rename total_big5n5_test total_big5n5

global relationm "total_leader5 total_peerrelation5 total_team5"
global wbm "total_sesteem5 ladder_current5 total_pride5"
global controlm "total_agency5 total_sefficacy5 total_grit5" //total_big5c5 total_big5n5//
global big5 "total_big5n5 total_big5c5"
* Labels
lab var total_agency5 "Agency"
lab var total_sefficacy5 "Self-efficacy"
lab var total_grit5 "Grit"
lab var total_sesteem5 "Self-esteem"
lab var total_pride5 "Pride"
lab var ladder_current5 "Subjective wellbeing"
lab var total_leader5 "Leadership"
lab var total_team5 "Teamwork"
lab var total_peerrelation5 "Peer relationships"
lab var total_big5c5 "Big 5 conscientiousness"
lab var total_big5n5 "Big 5 neuroticism"

 factor $relationm $controlm $big5, ipf

quietly fapara, reps(100) seed(1990) title(" ") ///
	plotregion(fcolor(white) m(zero) ilcolor(black)) ///
	graphregion(fcolor(white) lcolor(white) m(r=1)) bgcolor(white) ///
	ylab(-0.5(1)2.75, nogrid) xlab(1(1)8.5) yline(0, lcolor(black) lwidth(thin)) ///
	ytitle("Eigenvalue"" ") legend(region(lcolor(white)))
graph export "${graphstub}\eigen_hc4.pdf", replace


****************
*** Table C6 ***
****************

factor $relationm $controlm $big5, pf factors(2)
rotate, oblique quartimin 

esttab using "${tabstub}\TableC6.rtf", replace nonum nomtitles label ///
cells("r_L[1](transpose fmt(%4.3f)) r_L[2](transpose fmt(%4.3f)) Psi(fmt(%4.3f))") ///
collabels("Factor 1" "Factor 2" "Uniqueness") ///
stats(N, label("{\i N}") fmt(%3.0f) ) ///
varwidth(35) varlabels(,prefix(\u8193?)) ///
refcat(total_leader5 "{\afs20 \u8193?}\line{\b Social skills}" ///
		total_agency5 "{\afs20 \u8193?}\line{\b Task effectiveness}", nol) ///
title("{\b Table C6}\line {\i Factor Loadings and Unique Variance of Observable Socio-Emotional Skill Measures at Age 22}")

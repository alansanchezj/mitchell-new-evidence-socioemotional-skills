/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Skills gradients (Figures 1 and 2)
Created: Dec. 2019
*******************************************************************************/
clear
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
use "Data\oc_measures_peru_R1-5_v13.dta"

*** FIGURE 1
graph twoway (lfit sdqcon1	   wi1, lcolor(black)) (scatter sdqcon1	    wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Conduct problems)   xtitle(Wealth index) leg(off) name(graph1, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit sdqemo1 	   wi1, lcolor(black)) (scatter sdqemo1 	wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Emotional symptoms) xtitle(Wealth index) leg(off) name(graph2, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit sdqpeer1    wi1, lcolor(black)) (scatter sdqpeer1 	wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Peer problems)      xtitle(Wealth index) leg(off) name(graph3, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit sdqprosoc1  wi1, lcolor(black)) (scatter sdqprosoc1 wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Prosociability)      xtitle(Wealth index) leg(off) name(graph4, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit sdqhyper1   wi1, lcolor(black)) (scatter sdqhyper1 	wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Hyperactivity)      xtitle(Wealth index) leg(off) name(graph5, replace) bgcolor(white) graphregion(color(white))
graph combine graph1 graph2 graph3 graph4 graph5, rows(3) cols(2) com graphregion(color(white)) graphregion(color(white)) iscale(.5) ysize(6) 
graph export "${graphstub}\Figure 1.pdf", replace

*** FIGURE 2
graph twoway (lfit total_agency4	wi1, lcolor(black)) (scatter total_agency4	  wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Agency)        xtitle(Wealth index) leg(off) name(graph1, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_sefficacy4 wi1, lcolor(black)) (scatter total_sefficacy4 wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Self-efficacy) xtitle(Wealth index) leg(off) name(graph2, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_sesteem4	wi1, lcolor(black)) (scatter total_sesteem4   wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Self-esteem)   xtitle(Wealth index) leg(off) name(graph3, replace) bgcolor(white) graphregion(color(white))
graph combine graph1 graph2 graph3, rows(3) cols(2) graphregion(color(white)) graphregion(color(white)) iscale(.5) ysize(6) 
graph export "${graphstub}\Figure 2.pdf", replace

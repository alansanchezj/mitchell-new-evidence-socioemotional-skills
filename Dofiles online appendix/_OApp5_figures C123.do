/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Skills gradients (Online Appendix, Figures C1, C2 and C3)
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
use "Data\oc_measures_peru_R1-5.dta"

*** FIGURE C1
graph twoway (lfit total_leader5	    wi1, lcolor(black)) (scatter total_leader5	     wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Leadership)        xtitle(Wealth index) leg(off) name(graph1, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_team5          wi1, lcolor(black)) (scatter total_team5         wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Teamwork)          xtitle(Wealth index) leg(off) name(graph2, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_peerrelation5	wi1, lcolor(black)) (scatter total_peerrelation5 wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Peer relationship) xtitle(Wealth index) leg(off) name(graph3, replace) bgcolor(white) graphregion(color(white))
graph combine graph1 graph2 graph3, rows(3) cols(2) com graphregion(color(white)) graphregion(color(white)) iscale(.5) ysize(6) 
graph export "${graphstub}\Figure C1.pdf", replace

*** FIGURE C2
graph twoway (lfit total_agency5 wi1, lcolor(black)) (scatter total_agency5 wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Agency)                    xtitle(Wealth index) leg(off) name(graph1, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_grit5 	 wi1, lcolor(black)) (scatter total_grit5 	wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Grit)                      xtitle(Wealth index) leg(off) name(graph2, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_big5n5  wi1, lcolor(black)) (scatter total_big5n5  wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Big 5 emotional stability) xtitle(Wealth index) leg(off) name(graph3, replace) bgcolor(white) graphregion(color(white))
graph twoway (lfit total_big5c5  wi1, lcolor(black)) (scatter total_big5c5  wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Big 5 conscientiousness)   xtitle(Wealth index) leg(off) name(graph4, replace) bgcolor(white) graphregion(color(white))
graph combine graph1 graph2 graph3 graph4, rows(3) cols(2) graphregion(color(white)) graphregion(color(white)) iscale(.5) ysize(6) 
graph export "${graphstub}\Figure C2.pdf", replace

*** FIGURE C3
graph bar wi1, over(levlwrit2, label(labsize(*0.7))) ytitle(Wealth index) leg(off) name(graph1, replace) bgcolor(white) graphregion(color(white))

graph bar wi1, over(levlread2, label(labsize(*0.7))) ytitle(Wealth index) leg(off) name(graph2, replace) bgcolor(white) graphregion(color(white))

graph twoway (lfit ravens1  wi1, lcolor(black)) (scatter ravens1  wi1, mcolor(black) msize(small)  mfcolor(white)), ytitle(Raven test score) xtitle(Wealth index) leg(off) name(graph3, replace) bgcolor(white) graphregion(color(white))

graph combine graph1 graph2 graph3, rows(2) cols(2) graphregion(color(white)) graphregion(color(white)) 
graph export "${graphstub}\Figure C3.pdf", replace    
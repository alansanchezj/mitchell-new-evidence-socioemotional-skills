/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Defining endowments and Inputs/outputs in each period						*****
*********************************************************************************/

* Endowments are time-invariant so do not depend on the period. 
global pcoginput "careed2r" //mumed2r//
global pncoginput "tot_cagency2r" //tot_cagency2r//

global pcoginst "literspc1 carelita2" // daded2 mumlita2 dadlita2 dadlits2 mumlits2 //
global pncoginst "tot_cpride2 cladder2 tot_csocial2" //cladder2 tot_ctrust2  tot_cpride2//

if $period == 1{
	* Inputs/outputs
	global coginput "ravens1r"
	global ncoginput "sdqcon1r"
	*global yinput "ln_wi1"
	global yinput "ln_hhinc2"
	global hhinput "ln_hh2"
	
	* Make monetary investments per-capita, and hundreds
	cap drop uniformexp_pp2
	gen uniformexp_pp2 = (uniformexp2/age517_r2)/100 if age517_r2 >0 & age517_r2 !=.
	replace uniformexp_pp2 = uniformexp2/100 if age517_r2 == 0
	cap drop booksexp_pp2
	gen booksexp_pp2 = (booksexp2/age517_r2)/100 if age517_r2 >0 & age517_r2!=.
	replace booksexp_pp2 = booksexp2/100 if age517_r2 == 0

	cap drop foodexp_pp2 
	gen foodexp_pp2 = (foodexp2/hhsize2)/100 if hhsize2>0 & hhsize2!=.
	replace foodexp_pp2 = foodexp2/100 if hhsize2 == 0
	cap drop nfoodexp_pp2 
	gen nfoodexp_pp2 = (nfoodexp2/hhsize2)/100 if hhsize2>0 & hhsize2!=.
	replace nfoodexp_pp2 = (nfoodexp2)/100 if hhsize2 == 0
	
	global Ioutput "booksexp_pp2"
	global I_mprime "uniformexp_pp2"
	
	global iinput "booksexp_pp2r"
	
	global Poutputs "total_pride2 score_math2"
	global P_mprime "total_agency2 score_ppvt2" 
	* Alternative measures	- for measurement system		
	global cogm "score_ppvt2 levlwrit2 levlread2"    
	global ncogm "total_agency2" 

	* Instruments			
	global coginst "levlwrit1 levlread1" 
	global ncoginst "sdqhyper1 sdqprosoc1 sdqemo1 sdqpeer1"  
	global iinst "foodgroups2 uniformexp_pp2 hstudy2 hschool2" 
}
if $period == 2{
	* Inputs/outputs
	global coginput "score_math2r"
	global ncoginput "total_pride2r"
	*global yinput "ln_wi2" //Using ln_wi* doesn't affect the results qualitatively//
	global yinput "ln_hhinc3"
	global hhinput "ln_hh3"
	
	* Make monetary investments per-capita
	cap drop uniformexp_pp3
	gen uniformexp_pp3 = (uniformexp3/age517_r3)/100 if age517_r3>0 & age517_r3!=.
	replace uniformexp_pp3 = uniformexp3/100 if age517_r3 == 0
	cap drop booksexp_pp3
	gen booksexp_pp3 = (booksexp3/age517_r3)/100 if age517_r3>0 & age517_r3!=.
	replace booksexp_pp3 = booksexp3/100 if age517_r3 == 0

	cap drop foodexp_pp3 
	gen foodexp_pp3 = (foodexp3/hhsize3)/100 if hhsize3>0 & hhsize3!=.
	replace foodexp_pp3 = foodexp3/100 if hhsize3==0
	cap drop nfoodexp_pp3  
	gen nfoodexp_pp3 = (nfoodexp3/hhsize3)/100 if hhsize3>0 & hhsize3!=.
	replace nfoodexp_pp3 = nfoodexp3/100 if hhsize3==0

	global Ioutput "booksexp_pp3" //booksexp_pp3/
	global I_mprime "uniformexp_pp3"
	
	global iinput "booksexp_pp3r"

	global Poutputs "total_pride3 math_co3"
	global P_mprime "total_agency3 ppvt_co3"
	* Alternative measures			
	global cogm "ppvt_co3 cloze3"    
	global ncogm "total_agency3" //total_trust3 ladder_current3//

	* Instruments - when estimating interactions, take out ll but nfoodexp_pp3 - collinearity			
	global coginst "score_ppvt2 levlwrit2 levlread2" //
	global ncoginst "total_agency2" 
	global iinst "uniformexp_pp3 foodgroups3 hschool3 hstudy3" 
}
if $period == 3{
	* Inputs/outputs
	global coginput "math_co3r"
	global ncoginput "total_pride3r"
	global yinput "ln_wi4" //No icome info. here//
	*global yinput "ln_hhinc3"
	global hhinput "ln_hh4"

	* Make monetary investments per-capita
	cap drop foodexp_pp4
	gen foodexp_pp4 = (foodexp4/hhsize4)/100 if hhsize4>0 & hhsize4!=.
	replace foodexp_pp4 = foodexp4/100 if hhsize4==0
	cap drop nfoodexp_pp4
	gen nfoodexp_pp4 = (nfoodexp4/hhsize4)/100 if hhsize4>0 & hhsize4!=.
	replace nfoodexp_pp4 = nfoodexp4/100 if hhsize4==0

	global Ioutput "educexp4"
	global I_mprime "nfoodexp_pp4"
	
	global iinput "educexp4r"
	
	global Poutputs "total_agency4 maths_raw4"
	global P_mprime "total_sefficacy4 lang_raw4"
	* Alternative measures		
	global cogm "lang_raw4"
	global ncogm "total_sefficacy4 total_sesteem4"

	* Instruments			
	global coginst "ppvt_co3 cloze_co3"  
	global ncoginst "total_agency3" 
	global iinst "foodgroups4 nfoodexp_pp4 hschool4 hstudy4" 
}
if $period == 4{
	* Inputs/outputs
	global coginput "maths_raw4r"
	global ncoginput "total_agency4r"
	global yinput ""
	//BLANK FOR NOW//
	global Ioutput ""
	global I_mprime ""
	
	global iinnput ""

	* Define TFP
	global tfp1input "hstudy5" //educexp_r4r5_v1//
	global tfp2input "hwork5"
	global tfp3input "hcare5"
	global tfp4input "htask5"

	* three groupings here
	global relationoutput "total_leader5" //total_peerrelation5//
	global relation_mprime "total_team5" //total_sesteem5//
	*global wboutput "total_sesteem5"
	*global wb_mprime "ladder_current5"
	global controloutput "total_agency5"
	global control_mprime "total_grit5"
	* Alternative measures			
	global relationm "total_peerrelation5 total_team5" //total_sesteem5//
	*global wbm "ladder_current5 total_pride5"
	global controlm "total_grit5 total_big5c5 total_big5n5" //total_big5c5 total_big5n5//
	* Instruments		
	global coginst "lang_raw4"
	global ncoginst "total_sesteem4 total_sefficacy4 total_peerrelation4"
	//ladder_current4 total_pride4 total_prelation4 total_peerrelation4//
}

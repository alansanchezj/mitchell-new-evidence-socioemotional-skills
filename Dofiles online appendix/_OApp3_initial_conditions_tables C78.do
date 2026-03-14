/*******************************************************************************
Defining initial measures and estimating the distribution of initial conditions*
*******************************************************************************/

***Load data and set preferences***

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


local cog0m "ravens1 levlwrit1 levlread1"
*local ncog0m "revrs_sdqcon1 revrs_sdqemo1 revrs_sdqhyper1 revrs_sdqpeer1 sdqprosoc1"
local ncog0m "sdqcon1 sdqemo1 sdqhyper1 sdqpeer1 sdqprosoc1"
*local pcogm "mumed2 daded2 dadlita2 dadlits2 mumlita2 mumlita2" 
local pcogm "careed2 carelita2 literspc1" 
local pncogm "tot_cagency2 tot_cpride2 cladder2 cfarlad2" //tot_cagency2 tot_csocial2//
//"cagency_index2 csocial_index2 cpride_index2" //

/* Below calculates: mean vector & covariance matrix of initial
 conditions, and factor loadings and residualised measures*/

foreach w in cog0m ncog0m pcogm pncogm{
	quietly corr ``w'', covariance 
	tempname a
	matrix `a' = r(C)
	matrix colnames `a' = ``w''
	matrix rownames `a' = ``w''
	
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
	
	matrix colnames flvec`w' = ``w''
	local n: word count ``w''
	foreach var of varlist ``w''{
		
		tempvar `var'm
		cap drop ``var'm'
		egen ``var'm' = mean(`var')
		tempname `var'vec `var'sc
		matrix ``var'vec' = flvec`w'[1,"`var'"]
		scalar ``var'sc' = ``var'vec'[1,1]
		cap drop `var'r
		gen `var'r = (`var' - ``var'm')/``var'sc'
	}
}

foreach w in cog0m ncog0m pcogm pncogm{
	quietly corr ``w'', covariance 
	tempname a
	matrix `a' = r(C)
	scalar `w'var = (`a'[1,2]*`a'[1,3])/`a'[2,3]
	
	gettoken m1`w' rest: `w'
}

tempvar ysd ymean
egen `ysd' = sd(ln_hhinc1)
scalar yvar = `ysd'^2
tempname ivarcovar ivar
matrix `ivar' = (ncog0mvar, cog0mvar, pncogmvar, pcogmvar, yvar)
quietly corr `m1ncog0m' `m1cog0m' `m1pncogm' `m1pcogm' `m1phm' wi1, covariance
matrix `ivarcovar' = r(C)

mata: a = st_matrix("`ivarcovar'")
mata: b = st_matrix("`ivar'")
mata: _diag(a,b)
mata: st_matrix("sigma", a)

mat rownames sigma = "ncog0m" "cog0m" "pncogm" "pcogm" "income"
mat colnames sigma = "ncog0m" "cog0m" "pncogm" "pcogm" "income"

egen `ymean' = mean(ln_hhinc1)
scalar mu_y = `ymean'

matrix mu = (0,0,0,0,mu_y)

****************
*** Table C7 ***
****************
preserve

forvalues i=2/5{
	mat sigma[1,`i'] = 9999
}

forvalues i=3/5{
	mat sigma[2,`i'] = 9999
}

forvalues i=4/5{
	mat sigma[3,`i'] = 9999
}

mat sigma[4,5] = 9999 
 
esttab matrix(sigma, fmt(3)) using "Output\TableC7.rtf", replace ///
refcat(ncog0m "\afs20 \u8193?",nolabel) ///
nonotes nonum nomtitle nolines ///
substitute("9999.000" " ") ///
collabels("ln {\i H{\sub s{\plain\sub ,0}}}" /// 
		"ln {\i H{\sub c{\plain\sub ,0}}}" ///
		"ln {\i P{\sub s}}" ///
		"ln {\i P{\sub c}}" ///
		"ln {\i Y}{\sub 0}") ///
varlabels(ncog0m "ln {\i H{\sub s{\plain\sub ,0}}}" /// 
		cog0m "ln {\i H{\sub c{\plain\sub ,0}}}" ///
		pncogm "ln {\i P{\sub s}}" ///
		pcogm "ln {\i P{\sub c}}" ///
		income "ln {\i Y}{\sub 0}") ///
title("{\b Table C7}\line {\i Variance Covariance Matrix of the Initial Conditions}\line") 

restore


****************
*** Table C8 ***
****************

esttab matrix(mu, fmt(0 0 0 0 2)) using "Output\TableC8.rtf", replace ///
nonotes nonum nomtitle nolines ///
collabels("ln {\i H{\sub s{\plain\sub ,0}}}" /// 
		"ln {\i H{\sub c{\plain\sub ,0}}}" ///
		"ln {\i P{\sub s}}" ///
		"ln {\i P{\sub c}}" ///
		"ln {\i Y}{\sub 0}") ///
varlabels(r1 " ") ///
refcat(r1 "\afs20 \u8193?", nolabel) ///
title("{\b Table C8}\line {\i Mean Vector of the Initial Conditions}\line") 





/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Content: Age 12 var/covar matrix of hours and family resources. 
*******************************************************************************/
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

foreach w in school study work care chore{
	tempvar `w'sd `w'mean
	egen ``w'sd' = sd(h`w'2) //wrong
	scalar h`w'var = ``w'sd'^2
}


tempname ivarcovar ivar
matrix `ivar' = (ncog0mvar, cog0mvar, pncogmvar, pcogmvar, yvar, hschoolvar, hstudyvar, hworkvar, hcarevar, hchorevar)
quietly corr `m1ncog0m' `m1cog0m' `m1pncogm' `m1pcogm' `m1phm' wi1 hschool2 hstudy2 hwork2 hcare2 hchore2, covariance
matrix `ivarcovar' = r(C)

mata: a = st_matrix("`ivarcovar'")
mata: b = st_matrix("`ivar'")
mata: _diag(a,b)
mata: st_matrix("sigma", a)

mat rownames sigma = "ncog0m" "cog0m" "pncogm" "pcogm" "income" "hours school" "hours studying" "hours working" "hours caring" "hours chores"
mat colnames sigma = "ncog0m" "cog0m" "pncogm" "pcogm" "income" "hours school" "hours studying" "hours working" "hours caring" "hours chores"

egen `ymean' = mean(ln_hhinc1)
scalar mu_y = `ymean'

foreach w in school study work care chore{
	egen ``w'mean' = mean(h`w'2) 
	scalar mu_`w' = ``w'mean'
}


matrix mu = (0,0,0,0,mu_y,mu_school,mu_study,mu_work,mu_care,mu_chore)

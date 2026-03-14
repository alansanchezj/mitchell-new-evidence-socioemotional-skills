/*******************************************************************************
Paper: Title Human Capital Development: New Evidence on the
Production of Socio-emotional Skills,
Mark Mitchell, Marta Favara, Catherine Porter, Alan Sanchez.
Contents: Defining programs to calculate structural parameters. This should be
		  run, in the same session, before any estimations can be carried out.
Created: 24/10/19

Structure:
	1. Define "strucinv" to calculate investment functions
	2. Define "strucprod" to calculate production functions
	3. Define "strucprod2" to calculate production functions with interactions
*******************************************************************************/

/*******************************************************************************
INVESTMENT 																	****
*******************************************************************************/

cap program drop strucinv
program define strucinv, eclass
args x 

	ivreg2 `x' $yinput ///
		( $coginput $ncoginput $pcoginput $pncoginput = ///
		$coginst $ncoginst $pcoginst $pncoginst ), cluster(clusterid)	
		
	* Mark estimation sample
	cap drop invsamp$period
	gen invsamp$period = e(sample)==1
	
	tempname beta cov
	matrix `beta' = e(b)
	matrix `cov' = e(V) 
	* Measurement mean is estimated by the intercept of the reduced form regression 
	scalar mumean = `beta'[1,6]
	scalar muse = `cov'[6,6]
	* No. obs
	scalar N = e(N)
	
	* Calculate and store residuals
	tempvar nu
	cap drop `nu'
	predict double `nu' if e(sample)==1, resid	
	
	* Extract individual y inputs
	*gettoken (global)y1input (global)y2input: (global)yinput
	
	* Factor loading: sum of reduced form coefficients
	lincom _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$yinput ]
	scalar fl = r(estimate)
	scalar flse = r(se)
	
	* Now divide residuals by FL
	replace `nu' = `nu'/fl

	* Structural coefficients: reduced form coefficients divided by factor loading
	foreach w in cog ncog pcog pncog y{
		nlcom _b[${`w'input} ]/(_b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] + _b[$pncoginput ] + _b[$yinput ] )
		tempname c d
		matrix `c' = r(b)
		matrix `d' = r(V)
		* Store estimates
		scalar `w'pest = `c'[1,1]
		scalar `w'sest = `d'[1,1]	
	}
	
		*** Calculate variance of shocks here - alternative measure has been pre-selected ***
		* identical regression with alternative measure
		ivreg2 $I_mprime $yinput ///
			($coginput $ncoginput $pcoginput $pcoginput $pncoginput = ///
			$coginst $ncoginst $pcoginst $pncoginst ), cluster(clusterid)		
		tempname b		
		matrix `b' = e(b)
	
		* Factor loading
		tempname flprime
		scalar `flprime' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$yinput ]
		
		* Create residual measure
		tempvar Mrprime 
		gen `Mrprime' = ($I_mprime - `b'[1,6])/`flprime'	

		* Caclulate the variance of shocks
		corr `Mrprime' `nu', covariance
		scalar sigmanu = abs(r(cov_12))

		* Post estimation matrices
		tempname b V
		mat `b' = (mumean, fl, ncogpest, cogpest, pncogpest, pcogpest, ypest) 	
		mat `V' = (muse,0,0,0,0,0,0 \ 0,flse,0,0,0,0,0 \ ///
				0,0,ncogsest,0,0,0,0 \ 0,0,0,cogsest,0,0,0 \ ///
				0,0,0,0,pncogsest,0,0 \ 0,0,0,0,0,pcogsest,0 \ ///
				0,0,0,0,0,0,ysest )
		
		mat colnames `b' = mu fl ncog cog pncog pcog inc
		mat colnames `V' = mu fl ncog cog pncog pcog inc
		mat rownames `V' = mu fl ncog cog pncog pcog inc
			
		ereturn post `b' `V'
		estadd local cmd "strucinv"

		ereturn scalar mu = mumean
		ereturn scalar fl = fl
		ereturn scalar sigmanu = sigmanu
		ereturn scalar N = N

end		

/*******************************************************************************
PRODUCTION 	
		 													
First - Program estimating the baseline structural production function paramaters
********************************************************************************/
cap program drop strucprod
program define strucprod, eclass
args x 
	
	* Reduced form regression with dedicated instruments for each proxy input
	ivreg2 `x'  ///
		($coginput $ncoginput $pcoginput $pncoginput $iinput = ///
		$coginst $ncoginst $pcoginst $pncoginst $iinst ), cluster(clusterid)	

	* Mark estimation sample
	if "`x'" == "$cog"{
		cap drop prodsampcog$period
		gen prodsampcog$period = e(sample) ==1
	}
	else if "`x'" == "$nc"{
		cap drop prodsampncog$period
		gen prodsampncog$period = e(sample) ==1
	}

	tempname beta cov
	matrix `beta' = e(b)
	matrix `cov' = e(V) 
	* Measurement mean is estimated by the intercept of the reduced form regression 
	scalar mumean = `beta'[1,6]
	scalar muse = `cov'[6,6]
	* No. obs
	scalar N = e(N)
	
	* Calculate and store residuals
	tempname nu
	cap drop `nu'
	predict `nu', resid	
	
	* Factor loading: sum of the coefficients from the reduced form regression
	lincom _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ] 
	scalar fl = r(estimate)
	scalar flse = r(se)
	
	* Now divide residuals by FL
	replace `nu' = `nu'/fl
	
	* Structural coefficients: reduced form coefficients divided by factor loading
	foreach w in cog ncog pcog pncog i{
		nlcom _b[${`w'input}]/(_b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ])
		tempname c d
		matrix `c' = r(b)
		matrix `d' = r(V)
		* Store parameter and SE estimates
		scalar `w'pest = `c'[1,1]
		scalar `w'sest = `d'[1,1]
	}

	*** Calculate variance of shocks here - alternative measure has been pre-selected ***
	gettoken ncprime cogprime : (global) P_mprime , parse(" ")

	if "`x'" == "$cog"{
	* identical regression with alternative measure
	ivreg2 `cogprime'  ///
		( $coginput $ncoginput $pcoginput $pncoginput $iinput = ///
		$coginst $ncoginst $pcoginst $pncoginst $iinst ), cluster(clusterid)		
	}
	else if "`x'" == "$nc"{
	* identical regression with alternative measure
	ivreg2 `ncprime'  ///
		( $coginput $ncoginput $pcoginput $pncoginput $iinput = ///
		$coginst $ncoginst $pcoginst $pncoginst $iinst ), cluster(clusterid)		
	}
	tempname b		
	matrix `b' = e(b)

	* Factor loading
	tempname flprime
	scalar `flprime' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ]
	
	* Create residual measure
	tempvar Mrprime
	if "`x'" == "$cog"{
		gen `Mrprime' = (`cogprime' - `b'[1,6])/`flprime'
	}
	else if "`x'" == "$nc"{
		gen `Mrprime' = (`ncprime' - `b'[1,6])/`flprime'
	}

	* Caclulate the variance of shocks
	corr `Mrprime' `nu', covariance
	scalar sigmanu = abs(r(cov_12))
	
	* Post estimation matrices
	tempname b V j k
	mat `b' = (mumean, fl, ncogpest, cogpest, pncogpest, pcogpest, ipest ) 	
	mat `V' = (muse,0,0,0,0,0,0 \ 0,flse,0,0,0,0,0 \ ///
		0,0,ncogsest,0,0,0,0 \ 0,0,0,cogsest,0,0,0 \ ///
		0,0,0,0,pncogsest,0,0 \ 0,0,0,0,0,pcogsest,0 \ 0,0,0,0,0,0,isest )

	mat colnames `b' = mu fl ncog cog pncog pcog I
	mat colnames `V' = mu fl ncog cog pncog pcog I 
	mat rownames `V' = mu fl ncog cog pncog pcog I 

	ereturn post `b' `V'
	estadd local cmd "strucprod"
	
	ereturn scalar mu = mumean
	ereturn scalar fl = fl
	ereturn scalar sigmanu = sigmanu
	ereturn scalar N = N

end

/*******************************************************************************
PRODUCTION 
		 													
Now - Program estimating the baseline structural production function paramaters
in period 4
********************************************************************************/
cap program drop strucprod4
program define strucprod4, eclass
args x 
	* Reduced form regression with dedicated instruments for each proxy input
	ivreg2 `x' $tfp1input $tfp2input $tfp3input $tfp4input ///
		( $ncoginput $coginput = ///
		$coginst $ncoginst ), robust	

	* Mark estimation sample
	if "`x'"=="$relationoutput"{
		cap drop prodsamprel
		gen prodsamprel = e(sample)==1
	}
	else if "`x'"=="$wboutput"{
		cap drop prodsampwb
		gen prodsampwb = e(sample)==1
	}
	else if "`x'"=="$controloutput"{
		cap drop prodsampcont
		gen prodsampcont = e(sample)==1	
	}
	cap drop prodsampncog$period
	gen prodsampncog$period = e(sample)==1


	* Measurement mean is as the mean of the raw measurement due to normalisations
	* Structural coefficients = reduced form coefficients because of normalisation on FL
	foreach w in cog ncog tfp1 tfp2 tfp3 tfp4{
		* Store parameter and SE estimates
		scalar `w'pest = _b[${`w'input}]
		scalar `w'sest = _se[${`w'input}]^2
	}
	/* Interaction
	scalar intpest = _b[intinput]
	scalar intsest = _se[intinput]^2*/

	* No. obs
	scalar N = e(N)
	* Calculate and store residuals
	tempname nu
	cap drop `nu'
	predict `nu', resid	
	* Calculate TFP
	tempname mu
	quietly sum `x'
	scalar `mu'=r(mean)
	lincom _b[_cons]-`mu'
	scalar tfp = r(estimate)
	scalar tfpse = r(se)
	* Calculate RTS
	lincom _b[$coginput ] + _b[$ncoginput ]
	scalar rts = r(estimate)
	scalar rtssest = r(se)^2

	* Factor loading: normalised to 1
	*=>don't need to divide residuals by FL
	
	* identical regression with alternative measure
	if "`x'"=="$relationoutput"{
		* Caclulate the variance of shocks
		corr ${relation_mprime}r `nu', covariance
		scalar sigmanu = abs(r(cov_12))	
	}
	else if "`x'"=="$wboutput"{
		* Caclulate the variance of shocks
		corr ${wb_mprime}r `nu', covariance
		scalar sigmanu = abs(r(cov_12))	
	}
	else if "`x'"=="$controloutput"{
		* Caclulate the variance of shocks
		corr ${control_mprime}r `nu', covariance
		scalar sigmanu = abs(r(cov_12))		
	}

	* Post estimation matrices
	tempname b V j k
	mat `b' = (ncogpest, cogpest, tfp1pest,  tfp2pest,  tfp3pest,  tfp4pest, tfp, rts ) 	
	mat `V' = (ncogsest,0,0,0,0,0,0,0 \ 0,cogsest,0,0,0,0,0,0 \  ///
		0,0,tfp1sest,0,0,0,0,0 \ 0,0,0,tfp2sest,0,0,0,0 \ ///
		0,0,0,0,tfp3sest,0,0,0 \ 0,0,0,0,0,tfp4sest,0,0 \ ///
		0,0,0,0,0,0,tfpse,0 \ 0,0,0,0,0,0,0,rtssest )

	mat colnames `b' = ncog cog tfp1 tfp2 tfp3 tfp4 tfp rts
	mat colnames `V' = ncog cog tfp1 tfp2 tfp3 tfp4 tfp rts
	mat rownames `V' = ncog cog tfp1 tfp2 tfp3 tfp4 tfp rts

	ereturn post `b' `V'
	estadd local cmd "strucprod4" 
	
	ereturn scalar sigmanu = sigmanu
	ereturn scalar N = N
end


/******************************************************************************* 	
PRODUCTION

Program estimating the structural production function paramaters with the
interaction of investments included 
*******************************************************************************/

* Define the programe that estimates structural production paramaters
cap program drop strucprod2
program define strucprod2, eclass
	args x 
	* Reduced form regression with dedicated instruments for each proxy input
	ivreg2 `x' ///
		($coginput $ncoginput $pncoginput $pcoginput $iinput intinput = ///
		$coginst $ncoginst $pncoginst $pcoginst $iinst $intinst ), robust
	
	* Mark estimation sample
	if "`x'" == "$cog"{
		cap drop prodsampcog$period
		gen prodsampcog$period = e(sample)==1		
	}
	else if "`x'" == "$nc"{
		cap drop prodsampncog$period
		gen prodsampncog$period = e(sample)==1	
	}

	tempname beta cov
	matrix `beta' = e(b)
	matrix `cov' = e(V)
	* Measurement mean
	scalar mumean = `beta'[1,7]
	scalar muse = `cov'[7,7]
	* No. obs
	scalar N = e(N)
	
	* Calculate and store residuals
	tempname nu
	cap drop `nu'
	predict `nu', resid	
	
	* Factor loading
	lincom _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ] + _b[intinput ]
	scalar fl = r(estimate)
	scalar flse = r(se)

	* Structural coefficients: reduced form coefficients divided by factor loading
	foreach w in cog ncog pcog pncog i{
		nlcom _b[${`w'input} ]/(_b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ] + _b[intinput ])
		tempname c d
		matrix `c' = r(b)
		matrix `d' = r(V)
		* Store parameter and SE estimates
		scalar `w'pest = `c'[1,1]
		scalar `w'sest = `d'[1,1]
	}
	
	* Recover coefficient on the interaction 
	nlcom _b[intinput ]/(_b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ] + _b[intinput ])
	tempname c d
	matrix `c' = r(b)
	matrix `d' = r(V)
	scalar intpest = `c'[1,1]
	scalar intsest = `d'[1,1]
	
	*** Calculate variance of shocks here - alternative measure has been pre-selected ***
	gettoken (local) ncprime cogprime : (global) P_mprime , parse(" ")

	if "`x'" == "$cog"{
	* identical regression with alternative measure
	ivreg2 `cogprime'  ///
		( $coginput $ncoginput $pncoginput $pcoginput  $iinput intinput = ///
		$coginst $ncoginst $pncoginst $pcoginst $iinst $intinst), robust	
	}
	else if "`x'" == "$nc"{
	* identical regression with alternative measure
	ivreg2 `ncprime' ///
		( $coginput $ncoginput  $pncoginput $pcoginput  $iinput intinput = ///
		$coginst $ncoginst $pncoginst $pcoginst $iinst $intinst), robust	
	}
	tempname b		
	matrix `b' = e(b)

	* Factor loading
	tempname flprime
	scalar `flprime' = _b[$coginput ] + _b[$ncoginput ] + _b[$pcoginput ] +  _b[$pncoginput ] + _b[$iinput ] + _b[intinput ]
	
	* Create residual measure
	tempvar Mrprime
	if "`x'" == "$cog"{
		gen `Mrprime' = (`cogprime' - `b'[1,7])/`flprime'
	}
	else if "`x'" == "$nc"{
		gen `Mrprime' = (`ncprime' - `b'[1,7])/`flprime'
	}

	* Caclulate the variance of shocks
	corr `Mrprime' `nu', covariance
	scalar sigmanu = abs(r(cov_12))
	
	* Post estimation matrices
	tempname b V j k
	mat `b' = (mumean, fl, ncogpest, cogpest, pncogpest, pcogpest, ipest, intpest ) 	
	mat `V' = (muse,0,0,0,0,0,0,0 \ 0,flse,0,0,0,0,0,0 \ ///
		0,0,ncogsest,0,0,0,0,0 \ 0,0,0,cogsest,0,0,0,0 \ ///
		0,0,0,0,pncogsest,0,0,0 \ 0,0,0,0,0,pcogsest,0,0 \ 0,0,0,0,0,0,isest,0 \ ///
		0,0,0,0,0,0,0,intsest )

	mat colnames `b' = mu fl ncog cog pncog pcog I int
	mat colnames `V' = mu fl ncog cog pncog pcog I int
	mat rownames `V' = mu fl ncog cog pncog pcog I int

	ereturn post `b' `V'
	estadd local cmd "strucprod2"
	
	ereturn scalar mu = mumean
	ereturn scalar fl = fl
	ereturn scalar sigmanu = sigmanu
	ereturn scalar N = N
	
end

*******************************************************************************
*** Replication file for "Should We Talk about the Weather?  How Party Competition and Coalition Participation Influence Parties' Attention to Economic Issues", by Edward Goldring, Brandon Beomseob Park and Laron K. Williams, Party Politics
***
***
*** NOTE:
*** MAC users might have to update the locations of weights matrices in the subfolders below.
*******************************************************************************

clear
set matsize 1200

*** Set up working directory
*cd ""

*******************************************************************************
*** Germany, 2000-2010 PSRM Sagarzazu and Kluver
*******************************************************************************
use "Germany.dta", clear

keep if issue == 4
drop if ts <= 492
sort issue ts party
gen id = _n

mkmat perc_tm1, matrix(X)

gen status = 1
replace status = 2 if (G == 1 & PM == 0)
replace status = 3 if (G == 1 & PM == 1)
qui tab status, gen(s)
foreach s of numlist 1(1)3 {
	gen s`s'_perc_tm1 = s`s' * perc_tm1
}

qui tab party, gen(p)
foreach p of numlist 1(1)4 {
	gen p`p'_perc_tm1 = p`p' * perc_tm1
}

gen G_months2election = G * months2election
gen G_months_sq = G * months_sq
gen PM_months2election = PM * months2election
gen PM_months_sq = PM * months_sq

*******************************************************************************
*** Generate various temporally-lagged spatial-lags (TLSLs)
*******************************************************************************

*** Contiguity (row-standardized)
preserve
	use "W\Wcontiguity_rs.dta", clear
	mkmat cont*, matrix(W)
	
	matrix WX = W * X
	svmat WX
	keep id WX
	rename WX contiguity_rs_tlsl
	lab var contiguity_rs_tlsl "TLSL: contiguity, row-standardized"
	
	sort id
	tempfile cont_rs
	save `cont_rs'
restore

*** Coalition partners (row-standardized)
preserve
	use "W\Wcoalition_rs.dta", clear
	mkmat coal*, matrix(W)
	
	matrix WX = W * X
	svmat WX
	keep id WX
	rename WX coalition_rs_tlsl
	lab var coalition_rs_tlsl "TLSL: coalition partners, row-standardized"
	
	sort id
	tempfile coal_rs
	save `coal_rs'
restore


sort id
merge id using `cont_rs'
drop _merge

sort id
merge id using `coal_rs'
drop _merge

tempfile data
save `data', replace

*******************************************************************************
*** Replication
*******************************************************************************

use `data', clear

local W contiguity
local stub con
local rs _rs

preserve
	use "W\W`W'`rs'.dta", clear
	mkmat `stub'*, matrix(W)
	
	cap spmat drop w
	spmat dta w `stub'*, id(id)	
	drop id
	tempfile w
	save `w', replace
	
	spatwmat using `w', name(w)
	
	keep in 1/4
	keep `W'493*
	mkmat _all, matrix(Wcont)
restore

sort issue ts party

*** Ordinary least squares (OLS) regression
reg perc p*perc_tm1 mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2  

*** Spatial Durbin model (best model)
spreg ml perc `W'`rs'_tlsl coalition_rs_tlsl p*_perc_tm1 mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2, id(id) dlmat(w, eig) 

****************************************************************************
*** Long-Term Effects for OLS
****************************************************************************

qui reg perc p*perc_tm1 mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2
mat b = e(b)
qui sum mip_econ_tm1 if e(sample)
local sd = r(sd)

foreach p of numlist 1(1)4 {
	di _newline(2) "Party = `p'"
	di "LTE of Economic Salience = " b[1,5]/(1-b[1,`p'])
		di "1-standard deviation ("round(`sd', .01) ") LTE of Economic Salience = " `sd' * (b[1,5]/(1-b[1,`p']))
	di "LTE of Consumer Confidence = " b[1,6]/(1-b[1,`p'])

	di "LTE of EP Election = " b[1,7]/(1-b[1,`p'])	
	
	di "LTE of General Election (Opposition) = " b[1,8]/(1-b[1,`p'])

	di "LTE of General Election (Government) = " (b[1,8]+b[1,10])/(1-b[1,`p'])	
	
	di "LTE of Government (No GE) = " b[1,9]/(1-b[1,`p'])	
	
	di "LTE of Government (GE) = " (b[1,9]+b[1,10])/(1-b[1,`p'])		
}

****************************************************************************
*** Multicollinearity
****************************************************************************

qui reg contiguity_rs_tlsl p*_perc_tm1 coalition_rs_tlsl mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2
di "Variance inflation factor = " 1/(1-e(r2))

qui reg coalition_rs_tlsl p*_perc_tm1 contiguity_rs_tlsl mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2
di "Variance inflation factor = " 1/(1-e(r2))

****************************************************************************
*** Secondary tests
****************************************************************************

*** Summary statistics for emphasis by party
bys party: sum perc, det

*** Test the lag structure
reg perc mip_econ mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2

*** Are the phis statistically different?
spreg ml perc p*_perc_tm1 `W'`rs'_tlsl coalition_rs_tlsl mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2, id(id) dlmat(w, eig)

test p2_perc_tm1 = p1_perc_tm1
test p3_perc_tm1 = p1_perc_tm1
test p2_perc_tm1 = p4_perc_tm1
test p3_perc_tm1 = p4_perc_tm1

*******************************************************************************
*******************************************************************************
*** Substantive effects for spatial econometric models
*******************************************************************************
*******************************************************************************


*******************************************************************************
*** Illustration of all spatial-temporal effects
***
*** German coalition: SPD + Greens
*******************************************************************************

use `data', clear

local W contiguity
local stub con
local rs _rs

preserve
	use "W\W`W'`rs'.dta", clear
	mkmat `stub'*, matrix(W)
	
	cap spmat drop w
	spmat dta w `stub'*, id(id)	
	drop id
	tempfile w
	save `w', replace
	
	spatwmat using `w', name(w)
	
	keep in 1/4
	keep `W'493*
	mkmat _all, matrix(Wcont)
restore

sort issue ts party

*** Ordinary least squares (OLS) regression
reg perc mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2
qui sum mip_econ_tm1 if e(sample)
local sd = r(sd)

*** Spatial Durbin model (best model)
spreg ml perc p*_perc_tm1 `W'`rs'_tlsl coalition_rs_tlsl mip_econ_tm1 conf_ch ep_election g_election2 G G_g_election2, id(id) dlmat(w, eig)
mat b = e(b)
mat V = e(V)

local W coalition
local stub coal
local rs _rs

preserve
	use "W\W`W'`rs'.dta", clear
	keep in 1/4
	keep `W'493*
	mkmat _all, matrix(Wcoal)
restore

* What is the change in X?
scalar X = `sd'

* How many simulations?
local draws = 1000

* Now use -corr2data- to simulate N draws from the multivariate normal distribution based on the maximum likelihood estimates.
set seed 8675309

clear
corr2data b1 - b12 alpha rho sigma, n(`draws') means(b) cov(V)

* Make this into an Nx2 matrix
mkmat _all, matrix(draws)

* Which column is rho?
local rho = 14

* Which columns are phi?
local phi1 = 1
local phi4 = 4

* Which column is theta?
*local theta = 5				/* Contiguity TLSL */
local theta = 6					/* Coalition TLSL */

* Which parameter (beta)?
*local beta = 8					/* Change in consumer confidence */
local beta = 7					/* MIP economy t-1 */

* Local macro representing the number of observations
local N = rowsof(Wcont)

* Identity matrix
mat I = I(`N')

* A draws x 1 matrix of missing values; we will fill in these values with the direct, spatial and total effects in the loop.
foreach t of numlist 0(1)10 {
	mat tp`t'_total_draws = J(`draws',4,.)
	mat tp`t'_ctotal_draws = J(`draws',4,.)
	mat tp`t'_direct_draws = J(`draws',4,.)
	mat tp`t'_spatial_draws = J(`draws',4,.)
}

mat total_draws = J(`draws',4,.)

*** Make all the matrices accessible in mata
mata: I = st_matrix("I")
mata: Wcont = st_matrix("Wcont")
mata: Wcoal = st_matrix("Wcoal")
mata: X = st_numscalar("X")

local d = 1
while `d' <= `draws' {
	scalar beta = draws[`d',`beta']
	scalar theta = draws[`d',`theta']
	scalar rho = draws[`d',`rho']
	matrix phi = draws[`d',`phi1'..`phi4']

	mata: beta = st_numscalar("beta")
	mata: theta = st_numscalar("theta")
	mata: rho = st_numscalar("rho")
	mata: phi = st_matrix("phi")
	
	mata: pd = luinv(I-rho*Wcont)*X*beta	
	
	mata: tp0_direct = diagonal(pd)'
	mata: tp0_spatial = colsum(pd) :- tp0_direct
	mata: tp0_total = tp0_direct + tp0_spatial
	mata: tp0_ctotal = tp0_direct + tp0_spatial
	
	mata: st_matrix("tp0_direct", tp0_direct)
	mata: st_matrix("tp0_spatial", tp0_spatial)
	mata: st_matrix("tp0_total", tp0_total)
	mata: st_matrix("tp0_ctotal", tp0_ctotal)
	
	mata: tp1_direct = phi :* tp0_total
	mata: tp1_spatial = (theta*Wcoal*tp0_total')'
	mata: tp1_total = tp1_direct + tp1_spatial
	mata: tp1_ctotal = tp1_direct + tp1_spatial + tp0_total
	
	mata: st_matrix("tp1_direct", tp1_direct)
	mata: st_matrix("tp1_spatial", tp1_spatial)
	mata: st_matrix("tp1_total", tp1_total)
	mata: st_matrix("tp1_ctotal", tp1_ctotal)
	
	mata: tp2_direct = phi :* tp1_total
	mata: tp2_spatial = (theta*Wcoal*tp1_total')'
	mata: tp2_total = tp2_direct + tp2_spatial
	mata: tp2_ctotal = tp2_direct + tp2_spatial + tp1_ctotal
	
	mata: st_matrix("tp2_direct", tp2_direct)
	mata: st_matrix("tp2_spatial", tp2_spatial)
	mata: st_matrix("tp2_total", tp2_total)
	mata: st_matrix("tp2_ctotal", tp2_ctotal)
	
	mata: tp3_direct = phi :* tp2_total
	mata: tp3_spatial = (theta*Wcoal*tp2_total')'
	mata: tp3_total = tp3_direct + tp3_spatial
	mata: tp3_ctotal = tp3_direct + tp3_spatial + tp2_ctotal
	
	mata: st_matrix("tp3_direct", tp3_direct)
	mata: st_matrix("tp3_spatial", tp3_spatial)
	mata: st_matrix("tp3_total", tp3_total)
	mata: st_matrix("tp3_ctotal", tp3_ctotal)
	
	mata: tp4_direct = phi :* tp3_total
	mata: tp4_spatial = (theta*Wcoal*tp3_total')'
	mata: tp4_total = tp4_direct + tp4_spatial
	mata: tp4_ctotal = tp4_direct + tp4_spatial + tp3_ctotal
	
	mata: st_matrix("tp4_direct", tp4_direct)
	mata: st_matrix("tp4_spatial", tp4_spatial)
	mata: st_matrix("tp4_total", tp4_total)
	mata: st_matrix("tp4_ctotal", tp4_ctotal)
	
	mata: tp5_direct = phi :* tp4_total
	mata: tp5_spatial = (theta*Wcoal*tp4_total')'
	mata: tp5_total = tp5_direct + tp5_spatial
	mata: tp5_ctotal = tp5_direct + tp5_spatial + tp4_ctotal
	
	mata: st_matrix("tp5_direct", tp5_direct)
	mata: st_matrix("tp5_spatial", tp5_spatial)
	mata: st_matrix("tp5_total", tp5_total)
	mata: st_matrix("tp5_ctotal", tp5_ctotal)
	
	mata: tp6_direct = phi :* tp5_total
	mata: tp6_spatial = (theta*Wcoal*tp5_total')'
	mata: tp6_total = tp6_direct + tp6_spatial
	mata: tp6_ctotal = tp6_direct + tp6_spatial + tp5_ctotal
	
	mata: st_matrix("tp6_direct", tp6_direct)
	mata: st_matrix("tp6_spatial", tp6_spatial)
	mata: st_matrix("tp6_total", tp6_total)
	mata: st_matrix("tp6_ctotal", tp6_ctotal)
	
	mata: tp7_direct = phi :* tp6_total
	mata: tp7_spatial = (theta*Wcoal*tp6_total')'
	mata: tp7_total = tp7_direct + tp7_spatial
	mata: tp7_ctotal = tp7_direct + tp7_spatial + tp6_ctotal
	
	mata: st_matrix("tp7_direct", tp7_direct)
	mata: st_matrix("tp7_spatial", tp7_spatial)
	mata: st_matrix("tp7_total", tp7_total)
	mata: st_matrix("tp7_ctotal", tp7_ctotal)
	
	mata: tp8_direct = phi :* tp7_total
	mata: tp8_spatial = (theta*Wcoal*tp7_total')'
	mata: tp8_total = tp8_direct + tp8_spatial
	mata: tp8_ctotal = tp8_direct + tp8_spatial + tp7_ctotal
	
	mata: st_matrix("tp8_direct", tp8_direct)
	mata: st_matrix("tp8_spatial", tp8_spatial)
	mata: st_matrix("tp8_total", tp8_total)
	mata: st_matrix("tp8_ctotal", tp8_ctotal)
	
	mata: tp9_direct = phi :* tp8_total
	mata: tp9_spatial = (theta*Wcoal*tp8_total')'
	mata: tp9_total = tp9_direct + tp9_spatial
	mata: tp9_ctotal = tp9_direct + tp9_spatial + tp8_ctotal
	
	mata: st_matrix("tp9_direct", tp9_direct)
	mata: st_matrix("tp9_spatial", tp9_spatial)
	mata: st_matrix("tp9_total", tp9_total)
	mata: st_matrix("tp9_ctotal", tp9_ctotal)
	
	mata: tp10_direct = phi :* tp9_total
	mata: tp10_spatial = (theta*Wcoal*tp9_total')'
	mata: tp10_total = tp10_direct + tp10_spatial
	mata: tp10_ctotal = tp10_direct + tp10_spatial + tp9_ctotal
	
	mata: st_matrix("tp10_direct", tp10_direct)
	mata: st_matrix("tp10_spatial", tp10_spatial)
	mata: st_matrix("tp10_total", tp10_total)
	mata: st_matrix("tp10_ctotal", tp10_ctotal)
	
	mata: total_effect = tp0_total + tp1_total + tp2_total + tp3_total + tp4_total + tp5_total + tp6_total + tp7_total + tp8_total + tp9_total + tp10_total
	mata: st_matrix("total_effect", total_effect)
	
	foreach p of numlist 1(1)4 {
		foreach t of numlist 0(1)10 {
			mat tp`t'_direct_draws[`d',`p'] = tp`t'_direct[1,`p']
			mat tp`t'_spatial_draws[`d',`p'] = tp`t'_spatial[1,`p']
			mat tp`t'_ctotal_draws[`d',`p'] = tp`t'_ctotal[1,`p']
			mat tp`t'_total_draws[`d',`p'] = tp`t'_total[1,`p']
		}
		mat total_draws[`d',`p'] = total_effect[1,`p']
	}
	
	if mod(`d',10) == 0 {
		nois display "." _c
		if mod(`d',100) == 0 {
			nois display ""
		}
	}	
	local d = `d' + 1
}	
	
mat effect = J(45, 20, 0)

local b = 1
foreach t of numlist 0(1)10 {
	svmat tp`t'_direct_draws
	foreach p of numlist 1(1)4 {
		local c = (`p' * 5) - 4
		local cp1 = `c' + 1
		local cp2 = `c' + 2
		local cp3 = `c' + 3 
		local cp4 = `c' + 4
	
		sum tp`t'_direct_draws`p', meanonly
		mat effect[`b',`c'] = r(mean)
	
		_pctile tp`t'_direct_draws`p', p(2.5 5 95 97.5)
		mat effect[`b',`cp1'] = r(r2)
		mat effect[`b',`cp2'] = r(r3)
		mat effect[`b',`cp3'] = r(r1)
		mat effect[`b',`cp4'] = r(r4)	
	}
	local b = `b' + 1
	
	svmat tp`t'_spatial_draws
	foreach p of numlist 1(1)4 {
		local c = (`p' * 5) - 4
		local cp1 = `c' + 1
		local cp2 = `c' + 2
		local cp3 = `c' + 3 
		local cp4 = `c' + 4
	
		sum tp`t'_spatial_draws`p', meanonly
		mat effect[`b',`c'] = r(mean)
	
		_pctile tp`t'_spatial_draws`p', p(2.5 5 95 97.5)
		mat effect[`b',`cp1'] = r(r2)
		mat effect[`b',`cp2'] = r(r3)
		mat effect[`b',`cp3'] = r(r1)
		mat effect[`b',`cp4'] = r(r4)
	}
	local b = `b' + 1
	
	svmat tp`t'_total_draws
	foreach p of numlist 1(1)4 {
		local c = (`p' * 5) - 4
		local cp1 = `c' + 1
		local cp2 = `c' + 2
		local cp3 = `c' + 3 
		local cp4 = `c' + 4
	
		sum tp`t'_total_draws`p', meanonly
		mat effect[`b',`c'] = r(mean)
	
		_pctile tp`t'_total_draws`p', p(2.5 5 95 97.5)
		mat effect[`b',`cp1'] = r(r2)
		mat effect[`b',`cp2'] = r(r3)
		mat effect[`b',`cp3'] = r(r1)
		mat effect[`b',`cp4'] = r(r4)	
	}	
	local b = `b' + 1

	svmat tp`t'_ctotal_draws
	foreach p of numlist 1(1)4 {
		local c = (`p' * 5) - 4
		local cp1 = `c' + 1
		local cp2 = `c' + 2
		local cp3 = `c' + 3 
		local cp4 = `c' + 4
	
		sum tp`t'_ctotal_draws`p', meanonly
		mat effect[`b',`c'] = r(mean)
	
		_pctile tp`t'_ctotal_draws`p', p(2.5 5 95 97.5)
		mat effect[`b',`cp1'] = r(r2)
		mat effect[`b',`cp2'] = r(r3)
		mat effect[`b',`cp3'] = r(r1)
		mat effect[`b',`cp4'] = r(r4)	
	}	
	local b = `b' + 1
}

svmat total_draws
foreach p of numlist 1(1)4 {
	local c = (`p' * 5) - 4
	local cp1 = `c' + 1
	local cp2 = `c' + 2
	local cp3 = `c' + 3 
	local cp4 = `c' + 4

	sum total_draws`p', meanonly
	mat effect[45,`c'] = r(mean)
	
	_pctile total_draws`p', p(2.5 5 95 97.5)
	mat effect[45,`cp1'] = r(r2)
	mat effect[45,`cp2'] = r(r3)
	mat effect[45,`cp3'] = r(r1)
	mat effect[45,`cp4'] = r(r4)	
}

matrix rownames effect = tp0_direct tp0_spatial tp0_total tp0_ctotal tp1_direct tp1_spatial tp1_total tp1_ctotal tp2_direct tp2_spatial tp2_total tp2_ctotal tp3_direct tp3_spatial tp3_total tp3_ctotal tp4_direct tp4_spatial tp4_total tp4_ctotal tp5_direct tp5_spatial tp5_total tp5_ctotal tp6_direct tp6_spatial tp6_total tp6_ctotal tp7_direct tp7_spatial tp7_total tp7_ctotal tp8_direct tp8_spatial tp8_total tp8_ctotal tp9_direct tp9_spatial tp9_total tp9_ctotal tp10_direct tp10_spatial tp10_total tp10_ctotal total
*matrix colnames effect = G G_lo90 G_hi90 G_lo95 G_hi95 SPD SPD_lo90 SPD_hi90 SPD_lo95 SPD_hi95 FDP FDP_lo90 FDP_hi90 FDP_lo95 FDP_hi95 CDU CDU_lo90 CDU_hi90 CDU_lo95 CDU_hi95

preserve
	clear

	matrix colnames effect = effect1 lo901 hi901 lo951 hi951 effect2 lo902 hi902 lo952 hi952 effect3 lo903 hi903 lo953 hi953 effect4 lo904 hi904 lo954 hi954
	svmat effect, names(col)
	
	gen str12 effecttype = ""
	local b = 1
	qui foreach v in tp0_direct tp0_spatial tp0_total tp0_ctotal tp1_direct tp1_spatial tp1_total tp1_ctotal tp2_direct tp2_spatial tp2_total tp2_ctotal tp3_direct tp3_spatial tp3_total tp3_ctotal tp4_direct tp4_spatial tp4_total tp4_ctotal tp5_direct tp5_spatial tp5_total tp5_ctotal tp6_direct tp6_spatial tp6_total tp6_ctotal tp7_direct tp7_spatial tp7_total tp7_ctotal tp8_direct tp8_spatial tp8_total tp8_ctotal tp9_direct tp9_spatial tp9_total tp9_ctotal tp10_direct tp10_spatial tp10_total tp10_ctotal total {
		replace effecttype = "`v'" in `b'
		local b = `b' + 1
	}
	
	order effecttype
	
	foreach p of numlist 1(1)4 {
		di _newline(2) "Party = `p' " 
		foreach t of numlist 0(1)3 {
			list effectt *`p' if effecttype == "tp`t'_direct"
			list effectt *`p' if effecttype == "tp`t'_spatial"
		}

		list effectt *`p' if effecttype == "total"
	}
	
	*** Reshape for better ggplot-ing
	reshape long effect lo90 hi90 lo95 hi95, i(effecttype) j(party)
	
	drop if effecttype == "total"
	
	*** Generate party name
	gen str11 partyname = "Greens" if party == 1
	replace partyname = "SPD" if party == 2
	replace partyname = "FDP" if party == 3
	replace partyname = "CDU" if party == 4
	
	*** Generate time and effect type variables
	split effecttype, p("tp" "_")
	drop effecttype effecttype1
	rename effecttype2 order
	rename effecttype3 effectname
	destring order, force replace 
	
	sort effectname party order
	
	saveold "Spatial-Long-Term Effects.dta", replace version(12)
restore


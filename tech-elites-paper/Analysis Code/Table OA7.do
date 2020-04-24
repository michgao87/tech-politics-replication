
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", replace

keep if !missing(weight)

// aweight used per the documentation of the ebalance package.

// OA7a
foreach var in regulation redistribution globalism social {
	reg `var' dem_donors rep_donors dem_public rep_public ind_public ///
		if samplegroup1==1 [aweight=weight], robust
}

// OA7b
foreach var in regulation redistribution globalism social {
	reg `var' dem_donors rep_donors collegedem_public noncollegedem_public ///
		collegerep_public noncollegerep_public collegeind_public noncollegeind_public ///
		if samplegroup2==1 [aweight=weight], robust
}


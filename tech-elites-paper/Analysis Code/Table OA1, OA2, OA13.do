
use "CS Undergrad Survey Data/combined_withmeans_withugrads.dta", clear
	
// These are the outcome variables for all the regressions below.
local dummies dem_donors collegedem_public biomajors csmajors // Tech entrepreneurs are omitted category.

// Table OA 1
foreach dv in regulation redist globalism social {
	reg `dv' `dummies', robust
}

// Table OA 2
foreach dv in reg_redist notreg_redist notreg_notredist reg_notredist {
	reg `dv' `dummies', robust
}

// Table OA 13
foreach dv in reg_uber_like_taxis flowersussrq uberussrq ///
	pref_for_private gov_goodjob entrepreneurstoomuchcredit laborinfluenceprivate {
		reg `var' `dummies', robust
}




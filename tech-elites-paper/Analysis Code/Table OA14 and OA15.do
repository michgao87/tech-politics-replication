
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

// Define millionaires in mass sample
gen mass_millionaire = sample == 1 & millionaire == 1
keep if tech == 1 | mass_millionaire == 1

// Table OA14
foreach dv in regulation redistribution globalism social {
	reg `dv' mass_millionaire, robust
}

// Table OA15
keep if millionaire == 1
foreach dv in regulation redistribution globalism social {
	reg `dv' mass_millionaire, robust
}

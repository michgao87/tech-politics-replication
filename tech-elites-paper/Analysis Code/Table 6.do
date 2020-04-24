
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

// Create variable that identifies which sample each respondent comes from
gen samplegroup1cat = .
recode samplegroup1cat (.=1) if tech==1
recode samplegroup1cat (.=2) if dem_donors==1
recode samplegroup1cat (.=3) if rep_donors==1
recode samplegroup1cat (.=4) if dem_public==1
recode samplegroup1cat (.=5) if rep_public==1
recode samplegroup1cat (.=6) if ind_public==1

// Rename survey items so that code can be run more efficiently 
rename reg_drones reg_ind_drones
rename reg_selfdriving reg_ind_selfdriving
rename reg_wallstreet reg_ind_wallstreet
rename reg_internetdata reg_ind_internetdata
rename reg_healthinsurance reg_ind_ealthinsurance
rename reg_oil reg_ind_oil

// Stacks data so that observation is respondent-industry 
gen case_id = _n
reshape long reg_ind_, i(case_id) j(sector) string

// Create dummy variable where 1 = technology-related industries; 0 = non-tech industries
gen tech_ind = sector=="drones"|sector=="internetdata"|sector=="selfdriving"

rename tech tech_sample
rename tech_ind tech_policy

// Run Regression

reg reg_ind_ tech_sample##tech_policy if samplegroup1==1 & ///
	inlist(samplegroup1cat, 1, 2, 4), cluster(case_id)



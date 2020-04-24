
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

// Create variable for sample groups

gen samplegroup1cat = .
recode samplegroup1cat (.=1) if tech==1
recode samplegroup1cat (.=2) if dem_donors==1
recode samplegroup1cat (.=3) if rep_donors==1
recode samplegroup1cat (.=4) if dem_public==1
recode samplegroup1cat (.=5) if rep_public==1
recode samplegroup1cat (.=6) if ind_public==1


label var dem_donors "Democratic Donors"
label var rep_donors "Republican Donors"
label var dem_public "Democratic Citizens"
label var rep_public "Republican Citizens"
label var ind_public "Independent Citizens"
label var collegedem_public "College-Educated Democratic Citizens"
label var collegerep_public "College-Educated Republican Citizens"
label var collegeind_public "College-Educated Independent Citizens"
label var noncollegedem_public "No College Democratic Citizens"
label var noncollegerep_public "No College Republican Citizens"
label var noncollegeind_public "No College Independent Citizens"

// Create list of policy domain scales - these are the outcome variables for Tables OA3a, OA3b, and OA8a.
local policydvs regulation redistribution globalism social

// DVs for the analyses below
local sg1vars dem_donors rep_donors dem_public rep_public ind_public
local sg2vars dem_donors rep_donors collegedem_public collegerep_public collegeind_public noncollegedem_public noncollegerep_public noncollegeind_public


// Calculate difference between average age in tech sample and respondent's age
gen age = yob + 14 // age at the time of the survey
sum age if sample == 3
gen age_minus_tech_avg_age = age - r(mean)


// Table OA3a
foreach var in `policydvs' {
	reg `var' `sg1vars' if samplegroup1==1, robust
}

// Table OA3b
foreach var in `policydvs' {
	reg `var' `sg2vars' if samplegroup2==1, robust
}

// Table OA8a
foreach var in `policydvs' {
	xi: reg `var' `sg1vars' ib5.education i.gender age_minus_tech_avg_age ib6.hhincome if samplegroup1==1, robust
}


// Predispositions - these are the outcome varaibles for Tables OA4a, OA4b, and OA8b
local predisps entrepreneurstoomuchcredit racialresentment cosmopolitanism2 authoritarianism

// Table OA4a
foreach var in `predisps' {
	reg `var' `sg1vars' if samplegroup1==1, robust
}

// Table OA4b
foreach var in `predisps' {
	reg `var' `sg2vars' if samplegroup2==1, robust
}

// Table OA8b
foreach var in `predisps' {
	xi: reg `var' `sg1vars' ib5.education i.gender age_minus_tech_avg_age ib6.hhincome if samplegroup1==1, robust
}



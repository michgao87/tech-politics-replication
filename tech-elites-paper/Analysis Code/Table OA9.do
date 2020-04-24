
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear
destring zip, replace force
gen haszip = !missing(zip)
tabstat haszip, by(sample)

// Create variable for sample groups.

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

// Run models predicting regulation attitudes with and without zip code fixed effects
reg regulation dem_public rep_public ind_public if zip != ., robust // column 1
areg regulation dem_public rep_public ind_public if zip != ., a(zip) // column 2



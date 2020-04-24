
use "Tech Donor and Public Survey Data/datacombined_withmeans_anon.dta", clear

gen sampletext = "Tech" if sample == 3
replace sampletext = "Democrats (Mass Public)" if sample == 1 & (pid1 == 1 | pid2leans == 1)
replace sampletext = "Republicans (Mass Public)" if sample == 1 & (pid1 == 2 | pid2leans == 2)
replace sampletext = "Democratic Donors" if sample == 2 & party == "D"
replace sampletext = "Republican Donors" if sample == 2 & party == "R"

encode businessrand, gen(brandenc) // Creates dummy variables for the conditions in the experiment
gen govt_reg_business_harm_standard = 5 - govt_reg_business_harm // Rescales dependent variable 
reg govt_reg_business_harm_standard i.brandenc if sampletext == "Tech" // column 1
reg govt_reg_business_harm_standard i.brandenc if sampletext == "Democratic Donors" // column 2
reg govt_reg_business_harm_standard i.brandenc if sampletext == "Democrats (Mass Public)" // column 3

encode sampletext, gen(sampletextenc)
reg govt_reg_business_harm_standard i.brandenc##ib5.sampletextenc if ///
	inlist(sampletext, "Tech", "Democratic Donors", "Democrats (Mass Public)") // column4

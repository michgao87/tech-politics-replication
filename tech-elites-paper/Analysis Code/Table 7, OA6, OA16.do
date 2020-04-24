
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

// Label samples
gen sampletext = "Tech" if sample == 3
replace sampletext = "Democrats (Mass Public)" if sample == 1 & (pid1 == 1 | pid2leans == 1)
replace sampletext = "Republicans (Mass Public)" if sample == 1 & (pid1 == 2 | pid2leans == 2)
replace sampletext = "Democratic Donors" if sample == 2 & party == "D"
replace sampletext = "Republican Donors" if sample == 2 & party == "R"


// Reverse the direction of this scale so more liberal is positve.
replace govruns = 1 - govruns

gen pref_for_private = privatesectorruns - govruns
replace pref_for_private = 4 * pref_for_private

tabstat privatesectorruns govruns pref_for_private, by(sampletext)

encode sampletext, gen(sampleencoded)


// Table 7
reg pref_for_private ib5.sampleenc

replace gov_goodjob = 4 - 3*gov_goodjob // Recode to run from 0-1
reg gov_goodjob ib5.sampleenc

replace entrepreneurstoomuchcredit = 4 - 3*entrepreneurstoomuchcredit // Recode to run from 0-1
reg entrepreneurstoomuchcredit ib5.sampleenc


// Table OA 6
replace pref_for_private = (pref_for_private + 4)/8 // Recode to run from 0-1
replace gov_goodjob = (gov_goodjob - 1)/3 // Recode to run from 0-1
replace entrepreneurstoomuchcredit = (entrepreneurstoomuchcredit-1)/3 // Recode to run from 0-1
summ pref_for_private gov_goodjob entrepreneurstoomuchcredit

reg regulation pref_for_private if sample == 1
reg regulation gov_goodjob if sample == 1
reg regulation entrepreneurstoomuchcredit if sample == 1


// Table OA 16
//// Limit technology sample to just Democrats
drop if sample == 3 & pid3 != 1

// pref for private sector
reg pref_for_private ib5.sampleenc

// govt does good job running social programs
reg gov_goodjob ib5.sampleenc

// enterpreneurs too much credit
reg entrepreneurstoomuchcredit ib5.sampleenc

// tolerance for inequlaity
reg inequalityincomedisparities ib5.sampleenc

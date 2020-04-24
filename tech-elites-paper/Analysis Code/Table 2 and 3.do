
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

// Table 2
recode libertarian (0/.5 = 1) (.51 / 1 = 0), gen(libertarian_any_agree)

tab libertarian_any_agree if tech == 1

tab libertarian_any_agree if dem_donors == 1
tab libertarian_any_agree if rep_donors == 1

tab libertarian_any_agree if dem_public == 1 // Democrats in mass sample
tab libertarian_any_agree if rep_public == 1 // Republicans in mass sample


// Table 3
tab regandredist4way if tech == 1

tab regandredist4way if dem_donors == 1
tab regandredist4way if rep_donors == 1

tab regandredist4way if dem_public == 1
tab regandredist4way if rep_public == 1
tab regandredist4way if dem_public == 1
tab regandredist4way if millionaire == 1 & sample == 1

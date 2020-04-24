
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear

reg regulation entrepreneurstoomuchcredit if sample==1, robust //column 1
reg redistribution racialresentment if sample==1, robust // column 2
reg globalism cosmopolitanism2 if sample==1, robust // column 3
reg social authoritarianism if sample==1, robust // column 4

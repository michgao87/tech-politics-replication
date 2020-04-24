
use "Tech Donor and Public Survey Data/combined_withmeans_anon.dta", clear


// Raising prices is fair for Uber.
tabstat uberussrq if sample == 1 & pid3 == 3 // Republican Citizens
tabstat uberussrq if sample == 2 & party == "R" // Republican Donors

tabstat uberussrq if sample == 1 & pid3 == 1 // Democratic Citizens
tabstat uberussrq if sample == 2 & party == "D" // Democratic Donors

tabstat uberussrq if sample == 3 // All Technology Entrepreneurs
tabstat uberussrq if sample == 3 & pid3 == 1 // Democratic Technology Entrepreneurs


// Raising prices is fair for florists.
tabstat flowersussrq if sample == 1 & pid3 == 3 // Republican Citizens
tabstat flowersussrq if sample == 2 & party == "R" // Republican Donors

tabstat flowersussrq if sample == 1 & pid3 == 1 // Democratic Citizens
tabstat flowersussrq if sample == 2 & party == "D" // Democratic Donors

tabstat flowersussrq if sample == 3 // All Technology Entrepreneurs
tabstat flowersussrq if sample == 3 & pid3 == 1 // Democratic Technology Entrepreneurs

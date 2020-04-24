// This code prepares the survey data from the three main samples (tech entrepreneurs, donors, mass public) along with the college student sample for analysis.

set more off
clear all

// Set to your directory.
cd "CS Undergrad Survey Data"

use "combinedwithstanfordugrads.dta", clear

/// This code generates thee- and seven-point party identification scales based on the original branched survey items.

gen pid3 = .
recode pid3 (.=1) if pid1==1
recode pid3 (.=2) if pid1==3
recode pid3 (.=3) if pid1==2

gen pid7 = .
recode pid7 (.=1) if pid1==1&pid2dem==1
recode pid7 (.=2) if pid1==1&pid2dem==2
recode pid7 (.=3) if pid1==3&pid2leanselectedchoice==1
recode pid7 (.=4) if pid1==3&(pid2leanselectedchoice==3|pid2leanselectedchoice==4)
recode pid7 (.=5) if pid1==3&pid2leanselectedchoice==2
recode pid7 (.=6) if pid1==2&pid2rep==2
recode pid7 (.=7) if pid1==2&pid2rep==1


/// This code defines the various subsamples of the combined data

gen tech = sample==3
gen dem_donors = sample==2&party=="D"
gen collegedem_public = sample==1&(pid7==1|pid7==2|pid7==3)&(education==4|education==5)
gen csmajors = sample==4
gen biomajors = sample==5
keep if tech | dem_donors | collegedem_public | csmajors | biomajors
assert tech + dem_donors + collegedem_public + csmajors + biomajors == 1

label var dem_donors "Dem Donors"
label var collegedem_public "Dem College Public"
label var csmajors "CS Majors"
label var biomajors "Bio Majors"



/// This code creates dummy variables to responses to the regulation/redistribution question

gen reg_redist = regandredist4way==1 if regandredist4way!=.
gen notreg_redist = regandredist4way==2 if regandredist4way!=.
gen notreg_notredist = regandredist4way==4 if regandredist4way!=.
gen reg_notredist = regandredist4way==3 if regandredist4way!=.

gen in4way_redist = notreg_redist==1 | reg_redist==1 if regandredist4way != .
gen in4way_reg = reg_notredist==1 | reg_redist == 1 if regandredist4way != .

// This code rescales the questions on the influence of private/public sector labor unions

recode laborinfluenceprivate (3=2)
replace laborinfluenceprivate = laborinfluenceprivate-1

recode laborinfluencepublic (3=2)
replace laborinfluencepublic = laborinfluencepublic-1

// This code rescales the item on libertarianism

replace libertarian = (libertarian-1)/3


// The following code rescales the policy scale and predisposition items

// Coding of Variables (0-1 such that 1 is the response that tech sample should be highest on)

/// Items on Regulation Policy Attitudes and Predispositions on Entrepreneurship

replace reg_uber_like_taxis = reg_uber_like_taxis-1
replace gig_workers = (gig_workers-2)/-1
replace too_hard_fire_workers = (too_hard_fire_workers-2)/-1
replace reg_drones = (reg_drones-1)/2
replace reg_selfdriving = (reg_selfdriving-1)/2
replace reg_wallstreet = (reg_wallstreet-1)/2
replace reg_internetdata = (reg_internetdata-1)/2
replace reg_healthinsurance = (reg_healthinsurance-1)/2
replace reg_oil = (reg_oil-1)/2
gen govt_reg_business_harm2 = (govt_reg_business_harm-4)/-3 if businessrand=="business"
replace flowersussrq = (flowersussrq-2)/-1
replace uberussrq = (uberussrq-2)/-1
replace entrepreneurstoomuchcredit = (entrepreneurstoomuchcredit-1)/3
egen ussrq_combined = rowmean (flowersussrq uberussrq) 

egen regulation = rowmean (reg_uber_like_taxis gig_workers too_hard_fire_workers reg_drones reg_selfdriving reg_internetdata govt_reg_business_harm2 ussrq_combined)



/// Items on Redistribution Policy Attitudes and Predispositions on Racial Resentment

replace fedspend_poor = (fedspend_poor-3)/-2
replace fedspend_infrastructure = (fedspend_infrastructure-3)/-2
replace fedspend_foreignaid = (fedspend_foreignaid-3)/-2
replace fedspend_farmsubsidies = (fedspend_farmsubsidies-3)/-2
replace fedprograms_all = (fedprograms_all-20)/-4
replace fedprograms_poorest = (fedprograms_poorest-20)/-4
replace fedprograms_groups = (fedprograms_groups-20)/-4
replace govruns = (govruns-11)/4
replace privatesectorruns = (privatesectorruns-15)/-4
replace taxes_million = (taxes_million-15)/-4
replace taxes_250k = (taxes_250k-15)/-4
replace taxes_40k = (taxes_40k-15)/-4
replace taxes_sales = (taxes_sales-11)/4
recode healthcare (11=1) (12=2) (14=3) (15=4)
replace healthcare = (healthcare-4)/-3
replace gov_goodjob = (gov_goodjob-1)/3
replace inequalityincomedisparities = inequalityincomedisparities-1
recode racialresentment1 (11=1) (12=2) (14=3) (15=4)
replace racialresentment1 = (racialresentment1-4)/-3
recode racialresentment2 (11=1) (12=2) (14=3) (15=4)
replace racialresentment2 = (racialresentment2-1)/3
egen racialresentment = rowmean (racialresentment1 racialresentment2)

egen redistribution = rowmean (fedspend_poor fedprograms_poorest taxes_million taxes_250k healthcare)



/// Items on Globalism Policy Attitudes and Predispositions on Cosmopolitanism 

recode concentrateonproblemsathome (11=1) (12=2) (14=3) (15=4)
replace concentrateonproblemsathome = (concentrateonproblemsathome-1)/3
replace tradevsjobs = tradevsjobs-1
replace  freetradeagreementsgood =  (freetradeagreementsgood-2)/-1
replace immigration = (immigration-5)/-4
recode citizen_of_world  (11=1) (12=2) (14=3) (15=4)
replace  citizen_of_world =  (citizen_of_world-4)/-3
replace doyoucurrentlyholdapassport = (doyoucurrentlyholdapassport-2)/-1
replace europe = europe
replace canmex = canmex
replace asia = asia
replace indianfood = indianfood
replace sushi = sushi
egen cosmopolitanism2 = rowmean (citizen_of_world doyoucurrentlyholdapassport europe canmex asia indianfood sushi)

egen globalism = rowmean (concentrateonproblemsathome tradevsjobs freetradeagreementsgood immigration)




/// Items on Social Policy Attitudes and Predispositions on Authoritarianism

replace ssm = (ssm-4)/-3
replace deathpenalty = deathpenalty-1
replace guncontrol = guncontrol-1
recode abortion (4=1) (5=2) (6=3) (8=4)
replace abortion = (abortion-1)/3
replace authoritarianism1 = (authoritarianism1-2)/-1
replace authoritarianism2 = authoritarianism2-1
replace authoritarianism3 = (authoritarianism3-2)/-1
replace authoritarianism4 = (authoritarianism4-2)/-1
egen authoritarianism = rowmean (authoritarianism1 authoritarianism2 authoritarianism3 authoritarianism4)

egen social = rowmean (ssm deathpenalty guncontrol abortion)


/// Rescales items for Business Regulation Survey Experiment

gen govt_reg_business_harm_business = (govt_reg_business_harm-4)/-3 if businessrand=="business"
gen govt_reg_business_harm_finance =  (govt_reg_business_harm-4)/-3 if businessrand=="the financial industry (such as banks)"
gen govt_reg_business_harm_pharma = (govt_reg_business_harm-4)/-3 if businessrand=="the pharmaceutical industry"
gen govt_reg_business_harm_tech = (govt_reg_business_harm-4)/-3 if businessrand=="the technology industry"


// Rescale item on preference for private sector administration of government programs

replace govruns = 1 - govruns
gen pref_for_private = privatesectorruns - govruns
replace pref_for_private = 4 * pref_for_private


// Rescale item on belief that government does good job running social programs

replace gov_goodjob = 4 - 3*gov_goodjob



save "combined_withmeans_withugrads.dta", replace





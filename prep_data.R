library(foreign)
library(haven)
library(tidyverse)

# Prep survey datasets
# My R code based on 'tech-elites-paper/Data Prep Code - Run First' Stata .do files 

# Prepare CS Undergrad Survey Data

undergrad_survey <- 
  read_dta("tech-elites-paper/CS Undergrad Survey Data/combinedwithstanfordugrads.dta")

undergrad_survey <- undergrad_survey %>% 
  mutate(
    # generates three- and seven-point party identification scales 
    # based on the original branched survey items.
    
    pid3 = case_when(
      pid1 == 1 ~ 1,
      pid1 == 3 ~ 2,
      pid1 == 2 ~ 3),
    pid7 = case_when(
      pid1 == 1 & pid2dem == 1 ~ 1,
      pid1 == 1 & pid2dem == 2 ~ 2,
      pid1 == 3 & pid2leanselectedchoice == 1 ~ 3,
      pid1 == 3 & (pid2leanselectedchoice == 3 | pid2leanselectedchoice == 4) ~ 4,
      pid1 == 3 & pid2leanselectedchoice == 2 ~ 5,
      pid1 == 2 & pid2rep == 2 ~ 6,
      pid1 == 2 & pid2rep == 1 ~ 7
    ),
    
    # defines various subsamples of the combined data
    
    tech = ifelse(sample == 3, 1, 0),
    dem_donors = ifelse(sample==2&party=="D", 1, 0),
    collegedem_public = ifelse(sample==1&(pid7==1|pid7==2|pid7==3)&
                                 (education==4|education==5), 1, 0),
    csmajors = ifelse(sample==4, 1, 0),
    biomajors = ifelse(sample==5, 1, 0)
  ) %>% 
  filter(tech == 1 | dem_donors == 1 | collegedem_public == 1 |
           csmajors == 1 | biomajors == 1) %>% 
  mutate(
    # creates dummy variables to responses to the regulation/redistribution question
    
    reg_redist = ifelse(regandredist4way == 1, 1, 0),
    nonreg_redist = ifelse(regandredist4way == 2, 1, 0),
    notreg_notredist = ifelse(regandredist4way == 4, 1, 0),
    reg_notredist = ifelse(regandredist4way == 3, 1, 0),

    # rescales 
    
    laborinfluenceprivate = ifelse(laborinfluenceprivate == 3, 2, 
                                   laborinfluenceprivate),
    laborinfluenceprivate = laborinfluenceprivate - 1,
    laborinfluencepublic = ifelse(laborinfluencepublic == 3, 2, 
                                  laborinfluencepublic),
    laborinfluencepublic = laborinfluencepublic - 1,
    
    libertarian = (libertarian-1)/3,
    
    # Items on Regulation Policy Attitudes and Predispositions on Entrepreneurship 
    
    reg_uber_like_taxis = reg_uber_like_taxis-1,
    gig_workers = (gig_workers-2)/-1,
    too_hard_fire_workers = (too_hard_fire_workers-2)/-1,
    reg_drones = (reg_drones-1)/2,
    reg_selfdriving = (reg_selfdriving-1)/2,
    reg_wallstreet = (reg_wallstreet-1)/2,
    reg_internetdata = (reg_internetdata-1)/2,
    reg_healthinsurance = (reg_healthinsurance-1)/2,
    reg_oil = (reg_oil-1)/2,
    govt_reg_business_harm2 = ifelse(businessrand=="business", 
                                     (govt_reg_business_harm-4)/-3, 0),
    flowersussrq = (flowersussrq-2)/-1,
    uberussrq = (uberussrq-2)/-1,
    entrepreneurstoomuchcredit = (entrepreneurstoomuchcredit-1)/3,
    
    # Items on Redistribution Policy Attitudes and Predispositions on Racial Resentment
    
    fedspend_poor = (fedspend_poor-3)/-2,
    fedspend_infrastructure = (fedspend_infrastructure-3)/-2,
    fedspend_foreignaid = (fedspend_foreignaid-3)/-2,
    fedspend_farmsubsidies = (fedspend_farmsubsidies-3)/-2,
    fedprograms_all = (fedprograms_all-20)/-4,
    fedprograms_poorest = (fedprograms_poorest-20)/-4,
    fedprograms_groups = (fedprograms_groups-20)/-4,
    govruns = (govruns-11)/4,
    privatesectorruns = (privatesectorruns-15)/-4,
    taxes_million = (taxes_million-15)/-4,
    taxes_250k = (taxes_250k-15)/-4,
    taxes_40k = (taxes_40k-15)/-4,
    taxes_sales = (taxes_sales-11)/4,
    gov_goodjob = (gov_goodjob-1)/3,
    inequalityincomedisparities = inequalityincomedisparities-1,
    healthcare = case_when(
      healthcare == 11 ~ 1,
      healthcare == 12 ~ 2,
      healthcare == 14 ~ 3,
      healthcare == 15 ~ 4),
    healthcare = (healthcare-4)/-3,
    racialresentment1 = case_when(
      racialresentment1 == 11 ~ 1,
      racialresentment1 == 12 ~ 2,
      racialresentment1 == 14 ~ 3,
      racialresentment1 == 15 ~ 4),
    racialresentment1 = (racialresentment1-4)/-3,
    racialresentment2 = case_when(
      racialresentment2 == 11 ~ 1,
      racialresentment2 == 12 ~ 2,
      racialresentment2 == 14 ~ 3,
      racialresentment2 == 15 ~ 4),
    racialresentment2 = (racialresentment2-1)/3,
    
    # Items on Globalism Policy Attitudes and Predispositions on Cosmopolitanism 
    
    tradevsjobs = tradevsjobs-1,
    freetradeagreementsgood =  (freetradeagreementsgood-2)/-1,
    immigration = (immigration-5)/-4,
    doyoucurrentlyholdapassport = (doyoucurrentlyholdapassport-2)/-1,
    concentrateonproblemsathome = case_when(
      concentrateonproblemsathome == 11 ~ 1,
      concentrateonproblemsathome == 12 ~ 2,
      concentrateonproblemsathome == 14 ~ 3,
      concentrateonproblemsathome == 15 ~ 4),
    concentrateonproblemsathome = (concentrateonproblemsathome-1)/3,
    citizen_of_world = case_when(
      citizen_of_world == 11 ~ 1,
      citizen_of_world == 12 ~ 2,
      citizen_of_world == 14 ~ 3,
      citizen_of_world == 15 ~ 4),
    citizen_of_world = (citizen_of_world-4)/-3,
    
    # Items on Social Policy Attitudes and Predispositions on Authoritarianism
    
    ssm = (ssm-4)/-3,
    deathpenalty = deathpenalty-1,
    guncontrol = guncontrol-1,
    authoritarianism1 = (authoritarianism1-2)/-1,
    authoritarianism2 = authoritarianism2-1,
    authoritarianism3 = (authoritarianism3-2)/-1,
    authoritarianism4 = (authoritarianism4-2)/-1,
    abortion = case_when(
      abortion == 4 ~ 1,
      abortion == 5 ~ 2,
      abortion == 6 ~ 3,
      abortion == 8 ~ 4),
    abortion = (abortion-1)/3,
    
    # Rescales items for Business Regulation Survey Experiment
    
    govt_reg_business_harm_business = 
      ifelse(businessrand=="business", (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_finance = 
      ifelse(businessrand=="the financial industry (such as banks)", 
             (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_pharma = 
      ifelse(businessrand=="the pharmaceutical industry", 
             (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_tech = 
      ifelse(businessrand=="the technology industry", 
             (govt_reg_business_harm-4)/-3, NA),
    
    govruns = 1 - govruns,
    pref_for_private = 4 * (privatesectorruns - govruns),
    gov_goodjob = 4 - 3*gov_goodjob
    
  )

undergrad_survey$ussrq_combined = rowMeans(undergrad_survey[,11:12], na.rm = T)
undergrad_survey$regulation = rowMeans(undergrad_survey[,c(1:5, 7, 98, 104)], 
                                       na.rm = T)
undergrad_survey$racialresentment = rowMeans(undergrad_survey[,30:31], na.rm = T)
undergrad_survey$redistribution = rowMeans(undergrad_survey[,c(14, 19, 23:24, 27)], 
                                           na.rm = T)
undergrad_survey$cosmopolitanism2 = rowMeans(undergrad_survey[,c(36:37, 61:65)], 
                                             na.rm = T)
undergrad_survey$globalism = rowMeans(undergrad_survey[,c(32:35)], na.rm = T)
undergrad_survey$authoritarianism = rowMeans(undergrad_survey[,c(42:45)], na.rm = T)
undergrad_survey$social = rowMeans(undergrad_survey[,c(38:41)], na.rm = T)


undergrad_survey <- undergrad_survey %>% 
  mutate(reg_uber_like_taxis = as.factor(reg_uber_like_taxis),
         gig_workers = as.factor(gig_workers),
         too_hard_fire_workers = as.factor(too_hard_fire_workers),
         reg_drones = as.factor(reg_drones), 
         reg_selfdriving = as.factor(reg_selfdriving),
         reg_internetdata = as.factor(reg_internetdata),
         govt_reg_business_harm2 = as.factor(govt_reg_business_harm2),
         ussrq_combined = as.factor(ussrq_combined),
         fedspend_poor = as.factor(fedspend_poor),
         fedprograms_poorest = as.factor(fedprograms_poorest),
         taxes_million = as.factor(taxes_million),
         taxes_250k = as.factor(taxes_250k),
         healthcare = as.factor(healthcare),
         concentrateonproblemsathome = as.factor(concentrateonproblemsathome),
         tradevsjobs = as.factor(tradevsjobs),
         freetradeagreementsgood = as.factor(freetradeagreementsgood),
         immigration = as.factor(immigration),
         ssm = as.factor(ssm),
         deathpenalty = as.factor(deathpenalty),
         guncontrol = as.factor(guncontrol),
         abortion = as.factor(abortion))


write_dta(undergrad_survey, 
          "tech-elites-paper/CS Undergrad Survey Data/combined_withmeans_withugrads.dta")


# Prepare Tech Survey Data

tech_survey <- 
  read_dta("tech-elites-paper/Tech Donor and Public Survey Data/combined_anon.dta")

tech_survey <- tech_survey %>% 
  mutate(
    
    # generates three- and seven-point party identification scales 
    # based on the original branched survey items.
    
    pid3 = case_when(
      pid1 == 1 ~ 1,
      pid1 == 3 ~ 2,
      pid1 == 2 ~ 3),
    pid7 = case_when(
      pid1 == 1 & pid2dem == 1 ~ 1,
      pid1 == 1 & pid2dem == 2 ~ 2,
      pid1 == 3 & pid2leanselectedchoice == 1 ~ 3,
      pid1 == 3 & (pid2leanselectedchoice == 3 | pid2leanselectedchoice == 4) ~ 4,
      pid1 == 3 & pid2leanselectedchoice == 2 ~ 5,
      pid1 == 2 & pid2rep == 2 ~ 6,
      pid1 == 2 & pid2rep == 1 ~ 7
    ),
    
    # defines the various subsamples of the combined data
    
    tech = ifelse(sample == 3, 1, 0),
    dem_donors = ifelse(sample == 2 & party == "D", 1, 0),
    rep_donors = ifelse(sample == 2 & party == "R", 1, 0),
    dem_public = ifelse(sample==1&(pid7==1|pid7==2|pid7==3), 1, 0),
    rep_public = ifelse(sample==1&(pid7==5|pid7==6|pid7==7), 1, 0),
    ind_public = ifelse(sample==1&pid7==4, 1, 0),
    samplegroup1 = ifelse(tech==1|dem_donors==1|rep_donors==1|
                            dem_public==1|rep_public==1|ind_public==1, 1, 0),
    
    # creates dummy variables that separate mass partisans into 
    # those with and without college degrees
    
    collegedem_public = ifelse(sample==1&(pid7==1|pid7==2|pid7==3)&
                                 (education==4|education==5), 1, 0),
    noncollegedem_public = ifelse(sample==1&(pid7==1|pid7==2|pid7==3)&
                                    (education==1|education==2|education==3), 1, 0),
    collegerep_public = ifelse(sample==1&(pid7==5|pid7==6|pid7==7)&
                                 (education==4|education==5), 1, 0),
    noncollegerep_public = ifelse(sample==1&(pid7==5|pid7==6|pid7==7)&
                                    (education==1|education==2|education==3), 1, 0),
    collegeind_public = ifelse(sample==1&pid7==4&(education==4|education==5), 1, 0),
    noncollegeind_public = ifelse(sample==1&pid7==4&
                                    (education==1|education==2|education==3), 1 , 0),
    samplegroup2 = ifelse(tech==1|dem_donors==1|rep_donors==1|collegedem_public==1|
                          collegerep_public==1|collegeind_public==1|noncollegedem_public==1|
                            noncollegerep_public==1|noncollegeind_public==1, 1, 0),
    
    # creates dummy variables to responses to the regulation/redistribution question
    
    reg_redist = ifelse(regandredist4way == 1, 1, 0),
    nonreg_redist = ifelse(regandredist4way == 2, 1, 0),
    notreg_notredist = ifelse(regandredist4way == 4, 1, 0),
    reg_notredist = ifelse(regandredist4way == 3, 1, 0),
    
    # rescales 

    laborinfluenceprivate = ifelse(laborinfluenceprivate == 3, 2, 
                                   laborinfluenceprivate),
    laborinfluenceprivate = laborinfluenceprivate - 1,
    laborinfluencepublic = ifelse(laborinfluencepublic == 3, 2, 
                                  laborinfluencepublic),
    laborinfluencepublic = laborinfluencepublic - 1,
    
    libertarian = (libertarian-1)/3,
    
    # Items on Regulation Policy Attitudes and Predispositions on Entrepreneurship 
    
    reg_uber_like_taxis = reg_uber_like_taxis-1,
    gig_workers = (gig_workers-2)/-1,
    too_hard_fire_workers = (too_hard_fire_workers-2)/-1,
    reg_drones = (reg_drones-1)/2,
    reg_selfdriving = (reg_selfdriving-1)/2,
    reg_wallstreet = (reg_wallstreet-1)/2,
    reg_internetdata = (reg_internetdata-1)/2,
    reg_healthinsurance = (reg_healthinsurance-1)/2,
    reg_oil = (reg_oil-1)/2,
    govt_reg_business_harm2 = ifelse(businessrand=="business", 
                                     (govt_reg_business_harm-4)/-3, NA),
    flowersussrq = as.double((flowersussrq-2)/-1),
    uberussrq = as.double((uberussrq-2)/-1),
    entrepreneurstoomuchcredit = (entrepreneurstoomuchcredit-1)/3,
    
    # Items on Redistribution Policy Attitudes and Predispositions on Racial Resentment
    
    fedspend_poor = (fedspend_poor-3)/-2,
    fedspend_infrastructure = (fedspend_infrastructure-3)/-2,
    fedspend_foreignaid = (fedspend_foreignaid-3)/-2,
    fedspend_farmsubsidies = (fedspend_farmsubsidies-3)/-2,
    fedprograms_all = (fedprograms_all-20)/-4,
    fedprograms_poorest = (fedprograms_poorest-20)/-4,
    fedprograms_groups = (fedprograms_groups-20)/-4,
    govruns = (govruns-11)/4,
    privatesectorruns = (privatesectorruns-15)/-4,
    taxes_million = (taxes_million-15)/-4,
    taxes_250k = (taxes_250k-15)/-4,
    taxes_40k = (taxes_40k-15)/-4,
    taxes_sales = (taxes_sales-11)/4,
    gov_goodjob = (gov_goodjob-1)/3,
    inequalityincomedisparities = inequalityincomedisparities-1,
    healthcare = case_when(
      healthcare == 11 ~ 1,
      healthcare == 12 ~ 2,
      healthcare == 14 ~ 3,
      healthcare == 15 ~ 4),
    healthcare = (healthcare-4)/-3,
    racialresentment1 = case_when(
      racialresentment1 == 11 ~ 1,
      racialresentment1 == 12 ~ 2,
      racialresentment1 == 14 ~ 3,
      racialresentment1 == 15 ~ 4),
    racialresentment1 = (racialresentment1-4)/-3,
    racialresentment2 = case_when(
      racialresentment2 == 11 ~ 1,
      racialresentment2 == 12 ~ 2,
      racialresentment2 == 14 ~ 3,
      racialresentment2 == 15 ~ 4),
    racialresentment2 = (racialresentment2-1)/3,
    
    # Items on Globalism Policy Attitudes and Predispositions on Cosmopolitanism 
    
    tradevsjobs = tradevsjobs-1,
    freetradeagreementsgood =  (freetradeagreementsgood-2)/-1,
    immigration = (immigration-5)/-4,
    doyoucurrentlyholdapassport = (doyoucurrentlyholdapassport-2)/-1,
    concentrateonproblemsathome = case_when(
      concentrateonproblemsathome == 11 ~ 1,
      concentrateonproblemsathome == 12 ~ 2,
      concentrateonproblemsathome == 14 ~ 3,
      concentrateonproblemsathome == 15 ~ 4),
    concentrateonproblemsathome = (concentrateonproblemsathome-1)/3,
    citizen_of_world = case_when(
      citizen_of_world == 11 ~ 1,
      citizen_of_world == 12 ~ 2,
      citizen_of_world == 14 ~ 3,
      citizen_of_world == 15 ~ 4),
    citizen_of_world = (citizen_of_world-4)/-3,
    
    # Items on Social Policy Attitudes and Predispositions on Authoritarianism
    
    ssm = (ssm-4)/-3,
    deathpenalty = deathpenalty-1,
    guncontrol = guncontrol-1,
    authoritarianism1 = (authoritarianism1-2)/-1,
    authoritarianism2 = authoritarianism2-1,
    authoritarianism3 = (authoritarianism3-2)/-1,
    authoritarianism4 = (authoritarianism4-2)/-1,
    abortion = case_when(
      abortion == 4 ~ 1,
      abortion == 5 ~ 2,
      abortion == 6 ~ 3,
      abortion == 8 ~ 4),
    abortion = (abortion-1)/3,
    
    # Rescales items for Business Regulation Survey Experiment
    
    govt_reg_business_harm_business = ifelse(businessrand=="business", 
                                             (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_finance = 
      ifelse(businessrand=="the financial industry (such as banks)", 
             (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_pharma = 
      ifelse(businessrand=="the pharmaceutical industry", 
             (govt_reg_business_harm-4)/-3, NA),
    govt_reg_business_harm_tech = 
      ifelse(businessrand=="the technology industry", 
             (govt_reg_business_harm-4)/-3, NA)
  ) 

tech_survey$ussrq_combined = rowMeans(tech_survey[,15:16], na.rm = T)
tech_survey$regulation = rowMeans(tech_survey[,c(5:9, 11, 109, 114)], na.rm = T)
tech_survey$racialresentment = rowMeans(tech_survey[,34:35], na.rm = T)
tech_survey$redistribution = rowMeans(tech_survey[,c(18, 23, 27:28, 31)], na.rm = T)
tech_survey$cosmopolitanism2 = rowMeans(tech_survey[,c(40:41, 70:74)], na.rm = T)
tech_survey$globalism = rowMeans(tech_survey[,c(36:39)], na.rm = T)
tech_survey$authoritarianism = rowMeans(tech_survey[,c(46:49)], na.rm = T)
tech_survey$social = rowMeans(tech_survey[,c(42:45)], na.rm = T)

tech_survey <- tech_survey %>% 
  mutate(reg_uber_like_taxis = as.factor(reg_uber_like_taxis),
         gig_workers = as.factor(gig_workers),
         too_hard_fire_workers = as.factor(too_hard_fire_workers),
         reg_drones = as.factor(reg_drones), 
         reg_selfdriving = as.factor(reg_selfdriving),
         reg_internetdata = as.factor(reg_internetdata),
         govt_reg_business_harm2 = as.factor(govt_reg_business_harm2),
         ussrq_combined = as.factor(ussrq_combined),
         fedspend_poor = as.factor(fedspend_poor),
         fedprograms_poorest = as.factor(fedprograms_poorest),
         taxes_million = as.factor(taxes_million),
         taxes_250k = as.factor(taxes_250k),
         healthcare = as.factor(healthcare),
         concentrateonproblemsathome = as.factor(concentrateonproblemsathome),
         tradevsjobs = as.factor(tradevsjobs),
         freetradeagreementsgood = as.factor(freetradeagreementsgood),
         immigration = as.factor(immigration),
         ssm = as.factor(ssm),
         deathpenalty = as.factor(deathpenalty),
         guncontrol = as.factor(guncontrol),
         abortion = as.factor(abortion))


# Write file
write_dta(tech_survey, 
          "tech-elites-paper/Tech Donor and Public Survey Data/combined_withmeans_anon.dta")


This README describes the replication process for "Predispositions and The Political Behavior of American Economic Elites: Evidence from Technology Entrepreneurs" by David Broockman, Gregory Ferenstein, and Neil Malhotra.

Date: August 13, 2018

--------
Before beginning, create the following folder structure:
README.txt [in root directory]

./Analysis Code:
Figure 1a, 1c, and 1e.do		Table 2 and 3.do
Figure 1b and 1d.do			Table 5.do
Figure 2.R				Table 6.do
Figure 3.R				Table 7, OA6, OA16.do
Figure 4 and 5.R			Table OA1, OA2, OA13.do
Figure 6.R				Table OA14 and OA15.do
Figure 7, OA3, OA4, OA5, OA6, OA8.R	Table OA3, OA4, OA8.do
Figure 8.R				Table OA5.do
Figure OA7.R				Table OA7.do
Figure OA9.R				Table OA9.do
Table OA17.do

./CS Undergrad Survey Data:
combinedwithstanfordugrads.dta

./Codebooks:
combined_anon.dta and combinedwithstanfordugrads.dta Codebooks.pdf
f400_wtech.dta Codebook.pdf
opensecrets.dta Codebook.pdf

./Data Prep Code - Run First:
Prepare CS Undergrad Survey.do
Prepare Tech, Donor, and Public Surveys.do

./Figures:
[Intentionally empty]

./Other Data:
f400_wtech.dta	opensecrets.dta

./Tech Donor and Public Survey Data:
combined_anon.dta
-------


IMPORTANT:

All of the .do and .R code files assume that your working directory begins as the root directory of this replication archive.

Before beginning your analysis, run the two .do files in the "Data Prep Code - Run First" folder. "Data Prep Code - Run First/Prepare Tech, Donor, and Public Surveys.do" will create "combined_withmeans_anon.dta", which many of the analysis scripts rely on. "combined_withmeans_anon.dta" contains the means of the policy and predisposition scales. Many of the individual survey questions are also rescaled in "Prepare Tech, Donor, and Public Surveys.do" to run from 0-1, with higher values indicating the values that we hypothesized in the pre-analysis plan the technology sample would be higher on. The paper gives details on these. "Data Prep Code - Run First/Prepare CS Undergrad Survey.do" performs the same set of recordings but for the data combined with the undergraduate data, recoding "CS Undergrad Survey Data/combinedwithstanfordugrads.dta" into "CS Undergrad Survey Data/combined_withmeans_withugrads.dta" for analysis.

There are three folders with data files, "Tech Donor and Public Survey Data", which contains the dataset for the main analyses; "CS Undergrad Survey Data", which contains the dataset with the CS undergraduates for the CS undergraduate analyses; and "Other Data", which contains the OpenSecrets and Forbes 400 datasets.

Codebooks for these datasets are in the "Codebooks" folder. There are three codebook files in that older. "combined_anon.dta and combinedwithstanfordugrads.dta Codebooks.docx" is the codebook for both combined_anon.dta and combinedwithstanfordugrads.dta.

To replicate the Figures and Tables, see the scripts in the "Analysis Code" folder. The names of these scripts give the corresponding Figure or Table's results that are replicated in that script. E.g., "Table 7, OA6, OA16.do" produces the results reported in Tables 7, OA6, and OA16.

We subset to "samplegroup1 == 1" when running regressions that do not break out college-educated and non-college-educated groups in the mass public. We subset to "samplegroup2 == 1" when groups are being separately compared by education.

Figures OA1 and OA2 and Tables OA10, OA11, and OA12 (which all document features of the original sampling frames) cannot be created from this replication datafile because releasing these variables would allow survey respondents to be re-identified. Please contact the authors at dbroockman@stanford.edu if you would like to run analysis on the identified version of the dataset.

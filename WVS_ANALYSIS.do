
/*
  WVS_PAPER1_analysis.do
  Full analysis do-file for WVS Wave 7 (Latin America) dataset only
  Requirements:
    - Stata 17 or newer
    - sem package (ssc install sem)
    - asdoc, estout, coefplot installed
  Usage:
    . do WVS_PAPER1_analysis.do
*/

version 17.0, clear
set more off

// 0. Check data file
capture confirm file "$WVS_FILE"
if _rc {
    di as error "File '$WVS_FILE' not found."
    exit 1
}
use "$WVS_FILE", clear

display as text "Loaded WVS data: " _dta()

// 1. Confirm required vars
local req B_COUNTRY A_YEAR Q29 Q30 Q31 Q32 Q33 Q35 Q36 Q182 Y001 Q111 Q150 Q260 Q262 Q275 Q107 Q106 ideology
foreach v of local req {
    capture confirm variable `v'
    if _rc {
        di as error "Missing var: `v'"
        exit 1
    }
}

display as text "All required variables present."

// 2. Unique country-year obs
rename B_COUNTRY country
rename A_YEAR   year
bysort country year: gen Nobs = _N
bysort country year: keep if _n==1

display as text "One obs per country-year."

// 3. Filter Latin America (11 countries) & exclude Brazil, PR
keep if inlist(country,32,68,152,170,218,320,484,558,604,858,862)
drop if inlist(country,76,630)

display as text "Filtered to 11 Latin American countries."

// 4. Create subregion
gen byte subreg=.
replace subreg=1 if country==484
replace subreg=2 if inlist(country,320,558)
replace subreg=3 if inlist(country,68,170,218,604,862)
replace subreg=4 if inlist(country,32,152,858)
label define subreg_lbl 1 "Mexico" 2 "Central America" 3 "Andean" 4 "Southern Cone"
label values subreg subreg_lbl
label var subreg "Subregion"

display as text "Subregion var created."

// 5. Recoding and z-scores: gender items
local gvars Q29 Q30 Q31 Q32 Q33 Q35
foreach v of local gvars {
    replace `v'= . if `v'<0
    egen `v'_z= std(`v')
}
pca Q29_z Q30_z Q31_z Q32_z Q33_z Q35_z, comp(2) rotate(varimax)
egen gender_stereo = rowmean(Q29_z Q30_z Q31_z Q33_z)
egen gender_roles   = rowmean(Q32_z Q35_z)
egen gender_stereo_z = std(gender_stereo)
egen gender_roles_z   = std(gender_roles)
label var gender_stereo_z "Gender stereotypes (z)"
label var gender_roles_z   "Domestic roles (z)"

display as text "Gender subindices done."

// 6. Cultural indices: LGBT & Postmaterialism (Y001 only)
replace Q36   = . if Q36<1 | Q36>5
replace Q182  = . if Q182<1 | Q182>10
egen lgbt       = rowmean(Q36 Q182)
egen lgbt_z     = std(lgbt)
label var lgbt_z "LGBT subindex (z)"

replace Y001  = . if Y001<1 | Y001>5
gen postmat    = (Y001-1)/4  // 0-1 scaled
egen postmat_z = std(postmat)
label var postmat_z "Postmaterialism (Y001 only, z)"

display as text "Cultural indices ready."

// 7. Control variables: demographics
gen female     = .
replace female  = (Q260==2) if inlist(Q260,1,2)
label var female "female (1=yes)"

gen age_ord    = Q262
replace age_ord = . if age_ord<1 | age_ord>6
egen age_z      = std(age_ord)
label var age_z "Age (z)"

gen educ_ord   = Q275
replace educ_ord= . if educ_ord<0 | educ_ord>8
egen educ_z     = std(educ_ord)
label var educ_z "Education (z)"

display as text "Demographics done."

// 8. Economic att: state affinity & redistribution
gen state_aff    = 11 - Q107
egen state_aff_z  = std(state_aff)
label var state_aff_z "State affinity (z)"

gen redis        = 11 - Q106
egen redis_z      = std(redis)
label var redis_z "Redistribution (z)"

display as text "Economic variables done."

// 9. Descriptive stats & missing values summary
display as text "Descriptive statistics and missing data summary:" 
misstable summarize ideology gender_stereo_z gender_roles_z lgbt_z postmat_z age_z educ_z redis_z state_aff_z
asdoc tabstat ideology gender_stereo_z gender_roles_z lgbt_z postmat_z age_z educ_z redis_z state_aff_z, stats(N mean sd min max) save(WVS_descriptives.doc) replace

// 10. Correlation matrix
asdoc pwcorr ideology gender_stereo_z gender_roles_z lgbt_z postmat_z age_z educ_z redis_z state_aff_z, obs sig save(WVS_correlations.doc) replace

// 11. Regression models for H1: cultural values predicting ideology
reg ideology gender_stereo_z gender_roles_z lgbt_z postmat_z age_z educ_z female, cluster(country)
eststo H1_OLS
sem (IdeoLat -> gender_stereo_z gender_roles_z lgbt_z postmat_z) (IdeoLat <- age_z educ_z state_aff_z female), method(ml)
estat gof, stats(all)

// 12. Regression H2: adding economic predictors
reg ideology gender_stereo_z gender_roles_z lgbt_z postmat_z redis_z state_aff_z age_z educ_z female, cluster(country)
eststo H2_OLS

// 13. Interaction H2b: cultural x redis
gen high_redist = (redis_z>=0)
reg ideology c.gender_stereo_z##i.high_redist c.lgbt_z##i.high_redist c.postmat_z##i.high_redist age_z educ_z female, cluster(country)

// 14. Modulation by development H3
// Assuming variable GDPpc_z exists
capture confirm variable GDPpc_z
if _rc di as error "GDPpc_z missing: create externally." else {
    reg ideology c.redis_z##c.GDPpc_z c.gender_stereo_z##c.GDPpc_z c.lgbt_z##c.GDPpc_z age_z educ_z female, cluster(country)
}


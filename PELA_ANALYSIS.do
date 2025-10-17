/*
  PELA_ANALYSIS.do
  Comprehensive analysis do-file for PELA dataset only
  Requirements:
    - Stata 17 or newer
    - sem package (ssc install sem)
    - asdoc, estout, coefplot installed
  Usage:
    . do PELA_ANALYSIS.do
*/

version 17.0, clear
set more off

env global PELA_FILE "PELA PORQUE EL OTRO ESTABA MAL.dta"

// 0. Validate data file exists
capture confirm file "$PELA_FILE"
if _rc {
    di as error "PELA data file '$PELA_FILE' not found. Place it in working directory."
    exit 1
}
use "$PELA_FILE", clear

display as text "Loaded PELA data: " _dta()

// 1. Confirm required variables
local req pais iso3n legis SOCD4 SOCD5 SOCD7 ING3a ING3b ING3c ideology
local req2 GEN101-GEN108 GEN201-GEN206 GEN401-GEN405 VAL101 CLIMA2 ROES101 ROES104
foreach v of local req req2 {
    capture confirm variable `v'
    if _rc {
        di as error "Missing var: `v'"
        exit 1
    }
}

display as text "All required variables confirmed."

// 2. Keep most recent legislative year per country
bysort pais (legis): keep if _n==_N

display as text "Filtered to latest legislative year per country."

// 3. Convert country codes
kountry pais, from(iso3n) to(iso3c)
rename _ISO3C iso3c
kountry pais, from(iso3n) to(cown)
rename _COWN ccode

// 4. Control variables
// 4.1 Gender
label define sex_lbl 1 "Male" 2 "Female"
label values SOCD4 sex_lbl
label var SOCD4 "Gender"
// 4.2 Age
replace SOCD5 = . if SOCD5>85 | SOCD5==99
label var SOCD5 "Age (years)"
// 4.3 Education
replace SOCD7 = . if SOCD7==9
label define edu_lbl 1 "None" 2 "Primary" 3 "Secondary" 4 "Bachelor (lower)" 5 "Bachelor (upper)" 6 "Postgraduate"
label values SOCD7 edu_lbl
label var SOCD7 "Education"
// 4.4 Income
foreach v in ING3a ING3b ING3c {
    replace `v' = . if `v'==9
}
gen income_cat = ING3a
replace income_cat = ING3b if missing(income_cat)
replace income_cat = ING3c if missing(income_cat)
label define inc_lbl 1 "Very low" 2 "Low" 3 "Lower medium" 4 "Medium" 5 "Upper medium" 6 "High" 7 "Very high"
label values income_cat inc_lbl
label var income_cat "Income category"
egen income_z = std(income_cat)
label var income_z "Income (z)"

display as text "Control variables recoded."

// 5. Subregion classification
gen byte subreg = .
replace subreg = 1 if ccode==70
replace subreg = 2 if inlist(ccode,90,91,92,93,94,95)
replace subreg = 3 if ccode==42
replace subreg = 4 if inlist(ccode,100,101,130,135,145)
replace subreg = 5 if ccode==76
replace subreg = 6 if inlist(ccode,150,155,160,165)
label define sub_lbl 1 "Mexico" 2 "Central America" 3 "Caribbean" 4 "Andes" 5 "Brazil" 6 "Southern Cone"
label values subreg sub_lbl
label var subreg "Subregion"

display as text "Subregions created."

// 6. Gender equality – recode & PCA
local gvars GEN101 GEN103 GEN104 GEN105 GEN106 GEN107 GEN108 GEN401 GEN402 GEN403 GEN404 GEN405
foreach v of local gvars {
    replace `v' = . if inlist(`v',8,9)
    replace `v' = 6-`v' if inrange(`v',1,5)
}
pca GEN101 GEN103 GEN104 GEN105 GEN201 GEN202 GEN203 GEN204 GEN401 GEN403 GEN405, components(2) rotate(varimax)
predict gender_capac score(1)
predict gender_quota score(2)
egen gender_capac_z = std(gender_capac)
label var gender_capac_z "Gender capacities (z)"
egen gender_quota_z = std(gender_quota)
label var gender_quota_z "Gender quotas (z)"

display as text "Gender PCA indices generated."

// 7. Cultural indices: LGBT & Postmaterialism
replace VAL101 = . if inlist(VAL101,8,9)
egen lgbt_z = std(VAL101)
label var lgbt_z "LGBT tolerance (z)"

replace CLIMA2 = . if inlist(CLIMA2,98,99)
gen postmat_env = 11-CLIMA2
egen postmat_env_z = std(postmat_env)
label var postmat_env_z "Postmaterial environmentalism (z)"

display as text "Cultural indices ready."

// 8. Economic indices
replace ROES104 = . if inlist(ROES104,8,9)
egen redist_z = std(ROES104)
label var redist_z "Redistribution (z)"
replace ROES101 = . if inlist(ROES101,8,9)
egen stateown_z = std(ROES101)
label var stateown_z "State ownership (z)"

display as text "Economic indices ready."

// 9. Descriptives & missing data
misstable summarize ideology income_z gender_capac_z gender_quota_z lgbt_z postmat_env_z redist_z stateown_z age_z educ_z
asdoc tabstat ideology income_z gender_capac_z gender_quota_z lgbt_z postmat_env_z redist_z stateown_z age_z educ_z, stats(N mean sd min max) save(PELA_descriptives.doc) replace

// 10. Correlation matrix
asdoc pwcorr ideology income_z gender_capac_z gender_quota_z lgbt_z postmat_env_z redist_z stateown_z age_z educ_z, obs sig save(PELA_correlations.doc) replace

// 11. H1: cultural predictors of ideology
regress ideology income_z gender_capac_z gender_quota_z lgbt_z postmat_env_z age_z educ_z SOCD4, cluster(ccode)
eststo H1

// 12. SEM latent ideology
twofactor ideologyinv ideology redist_z gender_capac_z gender_quota_z lgbt_z postmat_env_z, method(ml)
// alternatively:
sem (IdeoLat -> redist_z gender_capac_z gender_quota_z lgbt_z postmat_env_z) (IdeoLat <- age_z educ_z stateown_z SOCD4), method(ml)
estat gof, stats(all)

// 13. H2: adding stateown effects
regress ideology redist_z stateown_z gender_capac_z gender_quota_z lgbt_z postmat_env_z age_z educ_z SOCD4, cluster(ccode)
eststo H2

// 14. H2b: interaction with redist
gen high_redist = redist_z>=0
regress ideology c.gender_capac_z##i.high_redist c.lgbt_z##i.high_redist c.postmat_env_z##i.high_redist age_z educ_z SOCD4, cluster(ccode)

// 15. H3: moderation by development (GDPpc_z external)
capture confirm variable GDPpc_z
if !_rc {
    regress ideology c.redist_z##c.GDPpc_z c.gender_capac_z##c.GDPpc_z c.lgbt_z##c.GDPpc_z age_z educ_z SOCD4, cluster(ccode)
} else {
    di as error "Variable GDPpc_z missing. Add external GDP data first."
}


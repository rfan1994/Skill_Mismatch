clear all
set more off
cd "~/Desktop/Skill_Mismatch Result"
* cd "~/thindrives/Desktop/Skill_Mismatch Result"
scalar a = 0.1

* =============================================================
* Annual: UR = tech + skill + mismatch 
capture erase "Stata/Regression/State_level_Data1.dta"

import excel "Data/FRED/State_level_Education.xls", sheet("Annual") firstrow clear
rename * Education*
rename EducationYear Year
reshape long Education, i(Year) j(State) string
save "Stata/Regression/State_level_Education1.dta", replace

import excel "Data/USPTO/State_level_Patent.xls", sheet("Annual") firstrow clear
rename * Patent*
rename PatentYear Year
reshape long Patent, i(Year) j(State) string
save "Stata/Regression/State_level_Patent1.dta", replace

import excel "Data/NSF/State_level_RD.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * RD*
rename RDYear Year
reshape long RD, i(Year) j(State) string
save "Stata/Regression/State_level_RD1.dta", replace

import excel "Data/NSF/State_level_SE.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * SE*
rename SEYear Year
reshape long SE, i(Year) j(State) string
save "Stata/Regression/State_level_SE1.dta", replace

import excel "Data/BLS/State_level_ALP.xlsx", sheet("Annual") firstrow clear
rename * ALP*
rename ALPYear Year
reshape long ALP, i(Year) j(State) string
save "Stata/Regression/State_level_ALP1.dta", replace

import excel "Data/BLS/State_level_Production.xlsx", sheet("Annual") firstrow clear
rename * Production*
rename ProductionYear Year
reshape long Production, i(Year) j(State) string
save "Stata/Regression/State_level_Production1.dta", replace

import excel "Data/BLS/State_level_WorkingHour.xlsx", sheet("Annual") firstrow clear
rename * WorkingHour*
rename WorkingHourYear Year
reshape long WorkingHour, i(Year) j(State) string
save "Stata/Regression/State_level_WorkingHour1.dta", replace

import excel "Data/FRED/UR.xls", sheet("Annual") firstrow clear
rename UR UR_mean
save "Stata/Regression/UR1.dta", replace

local files : dir "Stata/Regression" files "*1.dta"

import excel "Data/FRED/State_level_UR.xls", sheet("Annual") firstrow clear
rename * UR*
rename URYear Year
reshape long UR, i(Year) j(State) string

foreach file in `files' {
	capture merge 1:1 Year State using "Stata/Regression/`file'"
	if _rc == 111 {
		merge n:1 Year using "Stata/Regression/`file'"
	}
	keep if _merge==3
	drop _merge
	erase "Stata/Regression/`file'"
}

do "Stata/Regression/State_Code.do"
encode State, gen(State1) label(State_Code)
drop State
rename State1 State
replace Patent = log(Patent)
replace Production = Production/WorkingHour
bysort Year: egen Patent_mean = mean(Patent) 
bysort Year: egen Patent_sd = sd(Patent) 
bysort Year: egen Education_mean = mean(Education)
bysort Year: egen Education_sd = sd(Education)
bysort Year: egen RD_mean = mean(RD)
bysort Year: egen RD_sd = sd(RD)
bysort Year: egen SE_mean = mean(SE)
bysort Year: egen SE_sd = sd(SE)
bysort Year: egen Production_mean = mean(Production)
bysort Year: egen Production_sd = sd(Production)

replace Education = 1+a*(Education-Education_mean)/Education_sd
replace Patent = 1+a*(Patent-Patent_mean)/Patent_sd
replace RD = 1+a*(RD-RD_mean)/RD_sd
replace SE = 1+a*(SE-SE_mean)/SE_sd
replace Production = 1+a*(Production-Production_mean)/Production_sd
tsset State Year
tsfilter hp UR2 = UR, s(100) trend(UR_LR)
drop UR2
tsfilter hp UR_mean2 = UR_mean, s(100) trend(UR_mean_LR)
drop UR_mean2

save "Stata/Regression/State_level_Data1.dta", replace

keep State Year Patent RD SE Education
order State Year Patent RD SE Education
export excel using "Data/FRED/State_level_Data1.xlsx", sheetreplace firstrow(variables) nolabel

* =============================================================
* mean_UR = mean_tech + mean_skill + mean_mismatch 
* sd_UR = mean_tech + mean_skill + mean_mismatch 
capture erase "Stata/Regression/State_level_Data2.dta"

import excel "Data/FRED/State_level_Education.xls", sheet("Annual") firstrow clear
rename * Education*
rename EducationYear Year
reshape long Education, i(Year) j(State) string
collapse (mean) Education_State_mean = Education, by(State)
save "Stata/Regression/State_level_Education2.dta", replace

import excel "Data/USPTO/State_level_Patent.xls", sheet("Annual") firstrow clear
rename * Patent*
rename PatentYear Year
reshape long Patent, i(Year) j(State) string
collapse (mean) Patent_State_mean = Patent, by(State)
save "Stata/Regression/State_level_Patent2.dta", replace

import excel "Data/NSF/State_level_RD.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * RD*
rename RDYear Year
reshape long RD, i(Year) j(State) string
collapse (mean) RD_State_mean = RD, by(State)
save "Stata/Regression/State_level_RD2.dta", replace

import excel "Data/NSF/State_level_SE.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * SE*
rename SEYear Year
reshape long SE, i(Year) j(State) string
collapse (mean) SE_State_mean = SE, by(State)
save "Stata/Regression/State_level_SE2.dta", replace

import excel "Data/BLS/State_level_Production.xlsx", sheet("Annual") firstrow clear
rename * Production*
rename ProductionYear Year
reshape long Production, i(Year) j(State) string
collapse (mean) Production_State_mean = Production, by(State)
save "Stata/Regression/State_level_Production2.dta", replace

import excel "Data/BLS/State_level_WorkingHour.xlsx", sheet("Annual") firstrow clear
rename * WorkingHour*
rename WorkingHourYear Year
reshape long WorkingHour, i(Year) j(State) string
collapse (mean) WorkingHour_State_mean = WorkingHour, by(State)
save "Stata/Regression/State_level_WorkingHour2.dta", replace

local files : dir "Stata/Regression" files "*2.dta"

import excel "Data/FRED/State_level_UR.xls", sheet("Monthly") firstrow clear
rename * UR*
rename URYear Year
rename URMonth Month
reshape long UR, i(Year Month) j(State) string
collapse (mean) UR_State_mean = UR (sd) UR_State_sd = UR, by(State)

foreach file in `files' {
	merge 1:1 State using "Stata/Regression/`file'"
	keep if _merge==3
	drop _merge
	erase "Stata/Regression/`file'"
}

do "Stata/Regression/State_Code.do"
encode State, gen(State1) label(State_Code)
drop State
rename State1 State
replace Patent_State_mean = log(Patent_State_mean)
replace Production_State_mean = Production_State_mean/WorkingHour_State_mean
egen Patent_mean = mean(Patent_State_mean) 
egen Patent_sd = sd(Patent_State_mean) 
egen Education_mean = mean(Education_State_mean)
egen Education_sd = sd(Education_State_mean)
egen RD_mean = mean(RD_State_mean)
egen RD_sd = sd(RD_State_mean)
egen SE_mean = mean(SE_State_mean)
egen SE_sd = sd(SE_State_mean)
egen Production_mean = mean(Production_State_mean)
egen Production_sd = sd(Production_State_mean)

replace Education_State_mean = 1+a*(Education_State_mean-Education_mean)/Education_sd 
replace Patent_State_mean = 1+a*(Patent_State_mean-Patent_mean)/Patent_sd
replace RD_State_mean = 1+a*(RD_State_mean-RD_mean)/RD_sd
replace SE_State_mean = 1+a*(SE_State_mean-SE_mean)/SE_sd
replace Production_State_mean = 1+a*(Production_State_mean-Production_mean)/Production_sd
save "Stata/Regression/State_level_Data2.dta", replace

keep State Patent_State_mean RD_State_mean SE_State_mean Education_State_mean
order State Patent_State_mean RD_State_mean SE_State_mean Education_State_mean
export excel using "Data/FRED/State_level_Data2.xlsx", sheetreplace firstrow(variables) nolabel

* =============================================================
* Unemployment rate recover
capture erase "Stata/Regression/State_level_Data3.dta"

import excel "Data/FRED/State_level_Recover1.xls", firstrow clear
do "Stata/Regression/State_Code.do"
encode State, gen(State1) label(State_Code)
drop State
rename State1 State
merge 1:1 State using "Stata/Regression/State_level_Data2.dta"
keep if _merge==3
drop _merge

save "Stata/Regression/State_level_Data3.dta", replace

keep State Recession1 Recession2 Recession3 Education_State_mean Patent_State_mean
order State Recession1 Recession2 Recession3 Education_State_mean Patent_State_mean
export excel using "Data/FRED/State_level_Data3.xlsx", sheetreplace firstrow(variables)

* =============================================================
* Employment recover
capture erase "Stata/Regression/State_level_Data4.dta"

import excel "Data/FRED/State_level_Recover2.xls", firstrow clear
do "Stata/Regression/State_Code.do"
encode State, gen(State1) label(State_Code)
drop State
rename State1 State
merge 1:1 State using "Stata/Regression/State_level_Data2.dta"
keep if _merge==3
drop _merge

save "Stata/Regression/State_level_Data4.dta", replace

keep State Recession1 Recession2 Recession3 Education_State_mean Patent_State_mean
order State Recession1 Recession2 Recession3 Education_State_mean Patent_State_mean
export excel using "Data/FRED/State_level_Data4.xlsx", sheetreplace firstrow(variables)

* =============================================================
* Monthly: UR = tech + skill + mismatch 
capture erase "Stata/Regression/State_level_Data5.dta"

import excel "Data/FRED/State_level_Education.xls", sheet("Annual") firstrow clear
rename * Education*
rename EducationYear Year
reshape long Education, i(Year) j(State) string
save "Stata/Regression/State_level_Education5.dta", replace

import excel "Data/USPTO/State_level_Patent.xls", sheet("Annual") firstrow clear
rename * Patent*
rename PatentYear Year
reshape long Patent, i(Year) j(State) string
save "Stata/Regression/State_level_Patent5.dta", replace

import excel "Data/NSF/State_level_RD.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * RD*
rename RDYear Year
reshape long RD, i(Year) j(State) string
save "Stata/Regression/State_level_RD5.dta", replace

import excel "Data/NSF/State_level_SE.xlsx", sheet("Annual") firstrow clear
destring AK-WY, replace ignore("NA")
rename * SE*
rename SEYear Year
reshape long SE, i(Year) j(State) string
save "Stata/Regression/State_level_SE5.dta", replace

import excel "Data/FRED/State_level_Employment.xls", sheet("Monthly") firstrow clear
rename * Employment*
rename EmploymentYear Year
rename EmploymentMonth Month
reshape long Employment, i(Year Month) j(State) string
save "Stata/Regression/State_level_Employment5.dta", replace

import excel "Data/FRED/State_level_LFPR.xls", sheet("Monthly") firstrow clear
rename * LFPR*
rename LFPRYear Year
rename LFPRMonth Month
reshape long LFPR, i(Year Month) j(State) string
save "Stata/Regression/State_level_LFPR5.dta", replace

import excel "Data/BLS/State_level_ALP.xlsx", sheet("Annual") firstrow clear
rename * ALP*
rename ALPYear Year
reshape long ALP, i(Year) j(State) string
save "Stata/Regression/State_level_ALP5.dta", replace

import excel "Data/BLS/State_level_Production.xlsx", sheet("Annual") firstrow clear
rename * Production*
rename ProductionYear Year
reshape long Production, i(Year) j(State) string
save "Stata/Regression/State_level_Production5.dta", replace

import excel "Data/BLS/State_level_WorkingHour.xlsx", sheet("Annual") firstrow clear
rename * WorkingHour*
rename WorkingHourYear Year
reshape long WorkingHour, i(Year) j(State) string
save "Stata/Regression/State_level_WorkingHour5.dta", replace

import excel "Data/BLS/State_level_Cost.xlsx", sheet("Annual") firstrow clear
rename * Cost*
rename CostYear Year
reshape long Cost, i(Year) j(State) string
save "Stata/Regression/State_level_Cost5.dta", replace

import excel "Data/FRED/UR.xls", sheet("Monthly") firstrow clear
rename UR UR_mean
save "Stata/Regression/UR5.dta", replace

import excel "Data/FRED/Employment.xls", sheet("Monthly") firstrow clear
rename Employment Employment_total
save "Stata/Regression/Employment5.dta", replace

import excel "Data/FRED/LFPR.xls", sheet("Monthly") firstrow clear
rename LFPR LFPR_mean
save "Stata/Regression/LFPR5.dta", replace

import excel "Data/FRED/Working_Age.xls", sheet("Monthly") firstrow clear
replace Working_Age = Working_Age/10e6
save "Stata/Regression/Working_Age5.dta", replace

local files : dir "Stata/Regression" files "*5.dta"

import excel "Data/FRED/State_level_UR.xls", sheet("Monthly") firstrow clear
rename * UR*
rename URYear Year
rename URMonth Month
reshape long UR, i(Year Month) j(State) string
foreach file in `files' {
	capture merge 1:1 Year Month State using "Stata/Regression/`file'"
	if _rc == 111 {
		capture merge n:1 Year State using "Stata/Regression/`file'"
		if _rc == 111 {
			capture merge n:1 Year Month using "Stata/Regression/`file'"
			if _rc == 111 {
				merge n:1 Year using "Stata/Regression/`file'"
			}
		}
	}
	keep if _merge==3
	drop _merge
	erase "Stata/Regression/`file'"
}

do "Stata/Regression/State_Code.do"
encode State, gen(State1) label(State_Code)
drop State
rename State1 State
replace Patent = log(Patent)
replace Production = Production/WorkingHour
bysort Year: egen Patent_mean = mean(Patent) 
bysort Year: egen Patent_sd = sd(Patent) 
bysort Year: egen Education_mean = mean(Education)
bysort Year: egen Education_sd = sd(Education)
bysort Year: egen RD_mean = mean(RD)
bysort Year: egen RD_sd = sd(RD)
bysort Year: egen SE_mean = mean(SE)
bysort Year: egen SE_sd = sd(SE)
bysort Year: egen Production_mean = mean(Production)
bysort Year: egen Production_sd = sd(Production)

replace Education = 1+a*(Education-Education_mean)/Education_sd 
replace Patent = 1+a*(Patent-Patent_mean)/Patent_sd
replace RD = 1+a*(RD-RD_mean)/RD_sd
replace SE = 1+a*(SE-SE_mean)/SE_sd
replace Production = 1+a*(Production-Production_mean)/Production_sd
gen Time = ym(Year,Month) 
format Time %tm
tsset State Time
tsfilter hp UR2 = UR, s(14400) trend(UR_LR)
drop UR2
tsfilter hp Employment2 = Employment, s(14400) trend(Employment_LR)
drop Employment2
tsfilter hp LFPR2 = LFPR, s(14400) trend(LFPR_LR)
drop LFPR2
tsfilter hp UR_mean2 = UR_mean, s(14400) trend(UR_mean_LR)
drop UR_mean2
tsfilter hp LFPR_mean2 = LFPR_mean, s(14400) trend(LFPR_mean_LR)
drop LFPR_mean2
save "Stata/Regression/State_level_Data5.dta", replace

* ============================================================= 
* diff = diff_lag

import excel "Data/FRED/Age_level_UR.xls", sheet("Monthly") firstrow clear
keep if Year>1985
gen Time = ym(Year,Month) 
format Time %tm
tsset Time
gen diff1 = Age_25_34-Age_45_54
gen diff1_lag = L.diff1
gen diff2 = Age_35_44-Age_45_54
gen diff2_lag = L.diff2
save "Stata/Regression/Age_level_Data1.dta", replace

set more off
cd "~/Desktop/Skill_Mismatch Result"
cd "~/thindrives/Desktop/Skill_Mismatch Result"

* =============================================================
* Regression
* =============================================================
* Annual: UR = tech + skill + mismatch 

use "Stata/Regression/State_level_Data1.dta", clear
gen Mismatch = Patent-Education
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education*Mismatch
gen Patent_Mismatch = Patent*Mismatch
xtset State Year
xtreg UR Production Education Patent Mismatch Education_Mismatch Patent_Mismatch UR_mean, fe

* =============================================================
* sd_UR = mean_tech + mean_skill + mean_mismatch 

use "Stata/Regression/State_level_Data2.dta", clear
rename *_State_mean *
rename UR_State_sd UR_sd

gen Technology = (Patent+SE+RD)/3
gen Mismatch = Technology-Education
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education*Mismatch
gen Technology_Mismatch = Technology*Mismatch
reg UR Education Technology Mismatch 
reg UR_sd Education Technology Mismatch
reg UR Education Technology Mismatch Education_Mismatch Technology_Mismatch
reg UR_sd Education Technology Mismatch Education_Mismatch Technology_Mismatch

replace Mismatch = Patent-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen Patent_Mismatch = Patent*Mismatch
reg UR Education Patent Mismatch 
reg UR_sd Education Patent Mismatch
reg UR Education Patent Mismatch Education_Mismatch Patent_Mismatch
reg UR_sd Education Patent Mismatch Education_Mismatch Patent_Mismatch

replace Mismatch = SE-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen SE_Mismatch = SE*Mismatch
reg UR Education SE Mismatch
reg UR_sd Education SE Mismatch 
reg UR Education SE Mismatch Education_Mismatch SE_Mismatch
reg UR_sd Education SE Mismatch Education_Mismatch SE_Mismatch

replace Mismatch = RD-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen RD_Mismatch = RD*Mismatch
reg UR Education RD Mismatch 
reg UR_sd Education RD Mismatch  
reg UR Education RD Mismatch Education_Mismatch RD_Mismatch
reg UR_sd Education RD Mismatch Education_Mismatch RD_Mismatch

correlate Patent RD SE

* =============================================================
* Recession = mean_tech + mean_skill + mean_mismatch 

use "Stata/Regression/State_level_Data3.dta", clear
gen Mismatch = Patent_State_mean-Education_State_mean
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education_State_mean*Mismatch
gen Patent_Mismatch = Patent_State_mean*Mismatch
replace Mismatch=0 if Mismatch<0
reg Recession1 Production_mean ///
               Education_State_mean Patent_State_mean Mismatch ///
               Education_Mismatch Patent_Mismatch
reg Recession2 Production_mean ///
               Education_State_mean Patent_State_mean Mismatch ///
			   Education_Mismatch Patent_Mismatch
reg Recession3 Production_mean ///
		       Education_State_mean Patent_State_mean Mismatch ///
			   Education_Mismatch Patent_Mismatch

* =============================================================
* Recession = mean_tech + mean_skill + mean_mismatch 

use "Stata/Regression/State_level_Data4.dta", clear
gen Mismatch = Patent_State_mean-Education_State_mean
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education_State_mean*Mismatch
gen Patent_Mismatch = Patent_State_mean*Mismatch
replace Mismatch=0 if Mismatch<0
reg Recession1 Production_mean ///
			   Education_State_mean Patent_State_mean Mismatch ///
               Education_Mismatch Patent_Mismatch
reg Recession2 Production_mean ///
		       Education_State_mean Patent_State_mean Mismatch ///
			   Education_Mismatch Patent_Mismatch
reg Recession3 Production_mean ///
			   Education_State_mean Patent_State_mean Mismatch ///
			   Education_Mismatch Patent_Mismatch

* =============================================================
* Monthly: UR = tech + skill + mismatch 

use "Stata/Regression/State_level_Data5.dta", clear
xtset State Time
gen Technology = (Patent+SE+RD)/3
gen Mismatch = Technology-Education
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education*Mismatch
gen Technology_Mismatch = Technology*Mismatch
xtreg UR Education Technology Mismatch UR_mean Production, fe
xtreg UR_LR Education Technology Mismatch UR_mean_LR Production, fe
xtreg Employment Education Technology Mismatch Working_Age Production, fe
xtreg Employment_LR Education Technology Mismatch Working_Age Production, fe
xtreg UR Education Technology Mismatch ///
         Education_Mismatch Technology_Mismatch UR_mean Production, fe
xtreg UR_LR Education Technology Mismatch ///
            Education_Mismatch Technology_Mismatch UR_mean_LR Production, fe
xtreg Employment Education Technology Mismatch ///
                 Education_Mismatch Technology_Mismatch Working_Age Production, fe
xtreg Employment_LR Education Technology Mismatch ///
                    Education_Mismatch Technology_Mismatch Working_Age Production, fe
					
replace Mismatch = Patent-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen Patent_Mismatch = Patent*Mismatch
xtreg UR Education Patent Mismatch UR_mean Production, fe
xtreg UR_LR Education Patent Mismatch UR_mean_LR Production, fe
xtreg Employment Education Patent Mismatch Working_Age Production, fe
xtreg Employment_LR Education Patent Mismatch Working_Age Production, fe
xtreg UR Education Patent Mismatch ///
         Education_Mismatch Patent_Mismatch UR_mean Production, fe
xtreg UR_LR Education Patent Mismatch ///
            Education_Mismatch Patent_Mismatch UR_mean_LR Production, fe
xtreg Employment Education Patent Mismatch ///
                 Education_Mismatch Patent_Mismatch Working_Age Production, fe
xtreg Employment_LR Education Patent Mismatch ///
                    Education_Mismatch Patent_Mismatch Working_Age Production, fe

replace Mismatch = SE-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen SE_Mismatch = SE*Mismatch
xtreg UR Education SE Mismatch UR_mean Production, fe
xtreg UR_LR Education SE Mismatch UR_mean_LR Production, fe
xtreg Employment Education SE Mismatch Working_Age Production, fe
xtreg Employment_LR Education SE Mismatch Working_Age Production, fe
xtreg UR Education SE Mismatch ///
         Education_Mismatch SE_Mismatch UR_mean Production, fe
xtreg UR_LR Education SE Mismatch ///
            Education_Mismatch SE_Mismatch UR_mean_LR Production, fe
xtreg Employment Education SE Mismatch ///
                 Education_Mismatch SE_Mismatch Working_Age Production, fe
xtreg Employment_LR Education SE Mismatch ///
                    Education_Mismatch SE_Mismatch Working_Age Production, fe

replace Mismatch = RD-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen RD_Mismatch = RD*Mismatch
xtreg UR Education RD Mismatch UR_mean Production, fe
xtreg UR_LR Education RD Mismatch UR_mean_LR Production, fe
xtreg Employment Education RD Mismatch Working_Age Production, fe
xtreg Employment_LR Education RD Mismatch Working_Age Production, fe
xtreg UR Education RD Mismatch ///
         Education_Mismatch RD_Mismatch UR_mean Production, fe
xtreg UR_LR Education RD Mismatch ///
            Education_Mismatch RD_Mismatch UR_mean_LR Production, fe
xtreg Employment Education RD Mismatch ///
                 Education_Mismatch RD_Mismatch Working_Age Production, fe
xtreg Employment_LR Education RD Mismatch ///
                    Education_Mismatch RD_Mismatch Working_Age Production, fe

* =============================================================
* diff = diff_lag

use "Stata/Regression/Age_level_Data1.dta", clear
tsset Time
reg diff1 diff1_lag Time
reg diff2 diff2_lag Time

* =============================================================
* Analytical 

use "Stata/Regression/State_level_Data5.dta", clear
xtset State Time
gen Tech_Edu1 = Patent/Education
xtreg UR Education Patent UR_mean Production, fe

predict gap, res
xtreg gap Patent Tech_Edu, fe

* =============================================================
* Export regression result
* =============================================================

use "Stata/Regression/State_level_Data2.dta", clear
rename *_State_mean *
rename UR_State_sd UR_sd

gen Technology = (Patent+SE+RD)/3
gen Mismatch = Technology-Education
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education*Mismatch
gen Technology_Mismatch = Technology*Mismatch
reg UR_sd Education Technology Mismatch
outreg2 using "Stata/Regression/mismatch1", tex replace 
reg UR_sd Education Technology Mismatch Education_Mismatch Technology_Mismatch
outreg2 using "Stata/Regression/mismatch2", tex replace 

replace Mismatch = Patent-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen Patent_Mismatch = Patent*Mismatch
reg UR_sd Education Patent Mismatch
outreg2 using "Stata/Regression/mismatch3", tex replace 
reg UR_sd Education Patent Mismatch Education_Mismatch Patent_Mismatch
outreg2 using "Stata/Regression/mismatch4", tex replace 

replace Mismatch = SE-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen SE_Mismatch = SE*Mismatch
reg UR_sd Education SE Mismatch 
outreg2 using "Stata/Regression/mismatch5", tex replace 
reg UR_sd Education SE Mismatch Education_Mismatch SE_Mismatch
outreg2 using "Stata/Regression/mismatch6", tex replace 

replace Mismatch = RD-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen RD_Mismatch = RD*Mismatch
reg UR_sd Education RD Mismatch  
outreg2 using "Stata/Regression/mismatch7", tex replace 
reg UR_sd Education RD Mismatch Education_Mismatch RD_Mismatch
outreg2 using "Stata/Regression/mismatch8", tex replace 

* =============================================================
use "Stata/Regression/State_level_Data5.dta", clear
xtset State Time
gen Technology = (Patent+SE+RD)/3
gen Mismatch = Technology-Education
replace Mismatch=0 if Mismatch<0
gen Education_Mismatch = Education*Mismatch
gen Technology_Mismatch = Technology*Mismatch
xtreg UR Education Technology Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch1", tex append 	
xtreg UR_LR Education Technology Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch1", tex append 	
xtreg Employment Education Technology Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch1", tex append 	
xtreg Employment_LR Education Technology Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch1", tex append 	
xtreg UR Education Technology Mismatch ///
         Education_Mismatch Technology_Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch2", tex append 	
xtreg UR_LR Education Technology Mismatch ///
            Education_Mismatch Technology_Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch2", tex append 	
xtreg Employment Education Technology Mismatch ///
                 Education_Mismatch Technology_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch2", tex append 	
xtreg Employment_LR Education Technology Mismatch ///
                    Education_Mismatch Technology_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch2", tex append 	
					
replace Mismatch = Patent-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen Patent_Mismatch = Patent*Mismatch
xtreg UR Education Patent Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch3", tex append 	
xtreg UR_LR Education Patent Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch3", tex append 	
xtreg Employment Education Patent Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch3", tex append 	
xtreg Employment_LR Education Patent Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch3", tex append 	
xtreg UR Education Patent Mismatch ///
         Education_Mismatch Patent_Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch4", tex append 
xtreg UR_LR Education Patent Mismatch ///
            Education_Mismatch Patent_Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch4", tex append 
xtreg Employment Education Patent Mismatch ///
                 Education_Mismatch Patent_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch4", tex append 
xtreg Employment_LR Education Patent Mismatch ///
                    Education_Mismatch Patent_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch4", tex append 

replace Mismatch = SE-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen SE_Mismatch = SE*Mismatch
xtreg UR Education SE Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch5", tex append 
xtreg UR_LR Education SE Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch5", tex append 
xtreg Employment Education SE Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch5", tex append 
xtreg Employment_LR Education SE Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch5", tex append 
xtreg UR Education SE Mismatch ///
         Education_Mismatch SE_Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch6", tex append 
xtreg UR_LR Education SE Mismatch ///
            Education_Mismatch SE_Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch6", tex append 
xtreg Employment Education SE Mismatch ///
                 Education_Mismatch SE_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch6", tex append 
xtreg Employment_LR Education SE Mismatch ///
                    Education_Mismatch SE_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch6", tex append 

replace Mismatch = RD-Education
replace Mismatch=0 if Mismatch<0
replace Education_Mismatch = Education*Mismatch
gen RD_Mismatch = RD*Mismatch
xtreg UR Education RD Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch7", tex append 
xtreg UR_LR Education RD Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch7", tex append 
xtreg Employment Education RD Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch7", tex append 
xtreg Employment_LR Education RD Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch7", tex append 
xtreg UR Education RD Mismatch ///
         Education_Mismatch RD_Mismatch UR_mean Production, fe
outreg2 using "Stata/Regression/mismatch8", tex append 
xtreg UR_LR Education RD Mismatch ///
            Education_Mismatch RD_Mismatch UR_mean_LR Production, fe
outreg2 using "Stata/Regression/mismatch8", tex append 
xtreg Employment Education RD Mismatch ///
                 Education_Mismatch RD_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch8", tex append 
xtreg Employment_LR Education RD Mismatch ///
                    Education_Mismatch RD_Mismatch Working_Age Production, fe
outreg2 using "Stata/Regression/mismatch8", tex append 


* =============================================================
use "Stata/Regression/State_level_Data5.dta", clear
xtset State Time
gen Tech_Edu = abs(Patent/Education-1)
label variable Education "Skill"
label variable Patent "Technology"
label variable Production "Productivity"
label variable UR "Unemployment rate"
label variable UR_mean "Average Unemployment"
label variable Tech_Edu "Mismatch"
xtreg UR Education Patent UR_mean Production Tech_Edu, fe
outreg2 using "Stata/Regression/Analytical", label tex replace

* =============================================================
use "Stata/Regression/State_level_Data2.dta", clear
rename *_State_mean *
rename UR_State_sd UR_sd
gen Tech_Edu = abs(Patent/Education-1)
label variable Education "Skill"
label variable Patent "Technology"
label variable Production "Productivity"
label variable UR_sd "Unemployment volatility"
label variable Tech_Edu "Mismatch"
reg UR_sd Education Patent Production Tech_Edu
outreg2 using "Stata/Regression/Analytical", label tex append




set more off
cd "~/Desktop/Skill_Mismatch Result"
* cd "~/thindrives/Desktop/Skill_Mismatch Result"

use "Stata/Regression/State_level_Data2.dta", clear
keep State Education_State_mean Patent_State_mean
export delimited Education_State_mean Patent_State_mean using "Stata/Simulation/State_mean.txt", ///
       novar replace

use "Stata/Regression/State_level_Data5.dta", clear
keep Time State Education Patent
reshape wide Education Patent, i(State) j(Time)
export delimited Education* using "Stata/Simulation/mu_a.txt", ///
       novar replace
export delimited Patent* using "Stata/Simulation/mu_y.txt", ///
       novar replace	   


* =============================================================
local  j = "6"

infile number Model sigma k1 k2 x phi1 phi2 phi3 a y u_m LFPR_m f_m d_m u_s LFPR_s f_s d_s u_z LFPR_z f_z d_z ///
using "Fortran/BC_Moment/`j'/BC moment.txt", clear
drop if _n == 1
save "Stata/Simulation/BC moment`j'.dta", replace

forval i = 1/9 {
infile t z w_mean w_var w_var_bet mu_a mu_y a_mean a_var y_mean y_var corr_ay ///
using "Fortran/BC_Moment/`j'/IR_wage `i'.txt", clear
drop if _n == 1
save "Stata/Simulation/IR_wage`i'.dta", replace
infile t z u LFPR EN f f_u d EE EU_endo ALP_u ALP Vacancy_e Vacancy_u tech train schooling R_D ///
using "Fortran/BC_Moment/`j'/IR `i'.txt", clear
drop if _n == 1
merge 1:1 t z using "Stata/Simulation/IR_wage`i'.dta", keepusing(a_mean)
drop _merge
gen number = `i'
save "Stata/Simulation/IR`i'.dta", replace
}

forval i = 10/50 {
infile t z w_mean w_var w_var_bet mu_a mu_y a_mean a_var y_mean y_var corr_ay ///
using "Fortran/BC_Moment/`j'/IR_wage`i'.txt", clear
drop if _n == 1
save "Stata/Simulation/IR_wage`i'.dta", replace
infile t z u LFPR EN f f_u d EE EU_endo ALP_u ALP Vacancy_e Vacancy_u tech train schooling R_D ///
using "Fortran/BC_Moment/`j'/IR`i'.txt", clear
drop if _n == 1
merge 1:1 t z using "Stata/Simulation/IR_wage`i'.dta", keepusing(a_mean)
drop _merge
gen number = `i'
save "Stata/Simulation/IR`i'.dta", replace
}

clear
forval i = 1/50 {
append using "Stata/Simulation/IR`i'.dta"
erase "Stata/Simulation/IR`i'.dta"
erase "Stata/Simulation/IR_wage`i'.dta"
}

save "Stata/Simulation/IR_Panel`j'.dta", replace

* =============================================================
local j = "6"
local a = 0.1
use "Stata/Simulation/BC moment`j'.dta", clear
gen Mismatch = y-a
replace Mismatch=0 if Mismatch<0
reg u_s a y Mismatch
outreg2 using "Stata/Simulation/Simulation`j'", tex replace

use "Stata/Simulation/IR_Panel`j'.dta", clear
rename a_mean a
rename tech y
gen Employment = LFPR*(1-u)
gen Mismatch = y-a
replace Mismatch=0 if Mismatch<0
gen a_Mismatch = a*Mismatch 
gen y_Mismatch = y*Mismatch
reg u a y Mismatch z ALP
outreg2 using "Stata/Simulation/Simulation`j'", tex append
reg Employment a y Mismatch z ALP 
outreg2 using "Stata/Simulation/Simulation`j'", tex append



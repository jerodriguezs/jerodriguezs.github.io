/*************************************************************************
**************************************************************************			       	                  
Do-file: Paper Figures
Author : Julio Rodriguez
		 julio.rodriguez@bsg.ox.ac.uk
Updated: October, 2024
**************************************************************************
*************************************************************************/



/* [> set directory <] */ // specify the folder containing the data file "analysis_data.dta"
gl dire ".../"




/**********************************************************************/
/*  SECTION 1: Take-up Rate. Proportion of Women Who Made an Initial Visit to CM Centers
    Notes: */
/**********************************************************************/
clear all

/* [> date <] */
set obs 4
g date = .
forval i = 1/4 {
    replace date = `i' if _n == `i'
}
lab def datelab 1 "23 Aug 2013" 2 "28 Oct 2013" 3 "26 Nov 2013" 4 "19 May 2014"
lab val date datelab

/* [> N visited CM <] */
expand 3
g assignment = ""
sort date
foreach i of numlist 1 4 7 10 {
    replace assignment = "CM" if _n == `i'
    replace assignment = "HU" if _n == `i'+1
    replace assignment = "NP" if _n == `i'+2
}

g ncm = .
replace ncm = 827 if assignment == "CM" & date == 1
replace ncm = 828 if assignment == "CM" & date == 2
replace ncm = 833 if assignment == "CM" & date == 3
replace ncm = 836 if assignment == "CM" & date == 4
replace ncm = 136 if assignment == "HU" & date == 1
replace ncm = 151 if assignment == "HU" & date == 2
replace ncm = 161 if assignment == "HU" & date == 3
replace ncm = 211 if assignment == "HU" & date == 4
replace ncm = 110 if assignment == "NP" & date == 1
replace ncm = 126 if assignment == "NP" & date == 2
replace ncm = 133 if assignment == "NP" & date == 3
replace ncm = 174 if assignment == "NP" & date == 4

/* [> % visited CM <] */
g perc = ncm*100/1284 if assignment == "CM"
replace perc = ncm*100/1276 if assignment == "HU"
replace perc = ncm*100/1278 if assignment == "NP"

/* [> plot <] */ 
gen perc_formatted = string(perc, "%9.1f")
twoway  (connect perc date if assignment == "CM", lc(gs7) lp(dash_dot) lw(.5) mc(gs7) msi(2.5) msym(t) mlabel(perc_formatted) mlabcol(gs7) mlabpos(12)) ///
        (connect perc date if assignment == "HU", lc(black) lp(shortdash) lw(.5) mc(black) msi(2.5) msym(s) mlabel(perc_formatted) mlabcol(black) mlabpos(12)) ///
        (connect perc date if assignment == "NP", lc(gs12) lp(solid) lw(.5) ms(5) mc(gs12) msi(2.5) msym(o) mlabel(perc_formatted) mlabcol(gs12) mlabpos(6)), ///
        xti("", size(small))                                                    ///
        yti("% visited CM", size(small) orient(vert))                           ///
        xlab(, val labs(vsmall))                                                ///
        ylab(0(20)80, labs(small) angle(hor))                                   ///
        leg(order(1 "CM" 2 "HU" 3 "NP") pos(6) col(3) size(small))              ///
        graphregion(color(white)) plotregion(color(white))                      ///
        note("", pos(7) size(vsmall))


graph export "${dire}figure1.png", width(700) height(450) replace
graph drop _all
/*------------------------------------ End of SECTION 1 ------------------------------------*/






/**********************************************************************/
/*  SECTION 2: Figure 2. ITT on The Probability of Having a Subsequent Visit to CM and Women’s Education
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}/analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

* y-axis variable: difference (endline - baseline)
bys woman_id (endline): gen diff_psv_cm_adm = psv_cm_adm - psv_cm_adm[_n-1]

* N obs.
qui sum diff_psv_cm_adm if endline == 1 & !mi(yeduc_bl) & promo_asign == 1
local n_cm = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
qui sum diff_psv_cm_adm if endline == 1 & !mi(yeduc_bl) & promo_asign == 2
local n_hu = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
qui sum diff_psv_cm_adm if endline == 1 & !mi(yeduc_bl) & promo_asign == 3
local n_np = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
twoway  (lfitci diff_psv_cm_adm yeduc_bl [pw = ips] if endline == 1 & promo_asign == 1, estopts(cluster(psu)) lc(black) lp(solid) clc(black) fc(black%70) fi(inten10) alw(none))    ///
        (lfitci diff_psv_cm_adm yeduc_bl [pw = ips] if endline == 1 & promo_asign == 2, estopts(cluster(psu)) lc(black) lp(dash) clc(black) fc(black%70) fi(inten10) alw(none)) ///
        (lfitci diff_psv_cm_adm yeduc_bl [pw = ips] if endline == 1 & promo_asign == 3, estopts(cluster(psu)) lc(black) lp(shortdash) clc(black) fc(black%70) fi(inten10) alw(none)),   ///
        xti("Years of Eduction (Baseline)", size(small))                        ///
        yti("Difference (Post - Pre)", size(small) orient(vert))                ///
        xlab(, labs(small))                                               ///
        ylab(, labs(small) angle(hor))                                          ///
        leg(order(2 "CM (N =`n_cm')" 4 "HU (N =`n_hu')" 6 "NP (N =`n_np')" 1 "95% C.I.") pos(6) col(4) size(vsmall))    ///
        graphregion(color(white)) plotregion(color(white))                      ///
        note("", pos(7) size(vsmall))
graph export "${dire}figure2.png", width(700) height(450) replace
/*------------------------------------ End of SECTION 2 ------------------------------------*/






/**********************************************************************/
/*  SECTION 3: Figure 3. ITT on Aggregate Services’ Index and Women’s Age
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}/analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

* y-axis variable: difference (endline - baseline)
bys woman_id (endline): gen diff_ind_tot = ind_tot - ind_tot[_n-1]

* N obs.
qui sum diff_ind_tot if endline == 1 & !mi(age_bl) & promo_asign == 1
local n_cm = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
qui sum diff_ind_tot if endline == 1 & !mi(age_bl) & promo_asign == 2
local n_hu = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
qui sum diff_ind_tot if endline == 1 & !mi(age_bl) & promo_asign == 3
local n_np = subinstr(`"`: di %9.0fc r(N)'"', " ", "", .)
twoway  (lfitci diff_ind_tot age_bl [pw = ips] if endline == 1 & promo_asign == 1, estopts(cluster(psu)) lc(black) lp(solid) clc(black) fc(black%70) fi(inten10) alw(none))    ///
        (lfitci diff_ind_tot age_bl [pw = ips] if endline == 1 & promo_asign == 2, estopts(cluster(psu)) lc(black) lp(dash) clc(black) fc(black%70) fi(inten10) alw(none)) ///
        (lfitci diff_ind_tot age_bl [pw = ips] if endline == 1 & promo_asign == 3, estopts(cluster(psu)) lc(black) lp(shortdash) clc(black) fc(black%70) fi(inten10) alw(none)),   ///
        xti("Years of Age (Baseline)", size(small))                        ///
        yti("Difference (Post - Pre)", size(small) orient(vert))                ///
        xlab(, labs(small))                                               ///
        ylab(, labs(small) angle(hor))                                          ///
        leg(order(2 "CM (N =`n_cm')" 4 "HU (N =`n_hu')" 6 "NP (N =`n_np')" 1 "95% C.I.") pos(6) col(4) size(vsmall))    ///
        graphregion(color(white)) plotregion(color(white))                      ///
        note("", pos(7) size(vsmall))
graph export "${dire}figure3.png", width(700) height(450) replace
/*------------------------------------ End of SECTION 3 ------------------------------------*/


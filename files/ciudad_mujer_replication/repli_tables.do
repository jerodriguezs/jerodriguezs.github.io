/*************************************************************************
**************************************************************************			       	                  
Do-file: Paper Tables
Author : Julio Rodriguez
		 julio.rodriguez@bsg.ox.ac.uk
Updated: October, 2024
**************************************************************************
*************************************************************************/



/* [> set directory <] */ // specify the folder containing the data file "analysis_data.dta"
gl dire ".../"




/**********************************************************************/
/*  SECTION 1: Table 2. Balance of Women’s Socioeconomic Characteristics
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> baseline obs <] */
keep if baseline == 1


/* [> list of variables <] */ 
gl vars age hh_head couple couple_hh n_child_bl n_child5_hh n_child_6_12_hh literate yeduc employed ihs_labinc_u socsec_u socecon_ind hh_owner


/* [> results matrix <] */ 
local num : list sizeof global(vars)
mat R = J(`num'*2,10,.)
mat coln R = varname CM HU NP CM_HU Pval_CM_HU CM_NP Pval_CM_NP CM_HU_perc CM_NP_perc


/* [> estimates <] */ 
local r = 0
foreach var in $vars {
    local ++r

    /* [> cm vs hu <] */ 
    qui reg `var' promo_cm_hu [pw = ips], cluster(psu)
    mat R[`r',3]    = _b[_cons]
    mat R[`r'+1,3]  = _se[_cons]
    mat R[`r',5]    = _b[promo_cm_hu]
    mat R[`r'+1,5]  = _se[promo_cm_hu]
    mat R[`r',6]    = r(table)[4,1]
    qui lincom _cons + promo_cm_hu
    mat R[`r',2]    = r(estimate)
    mat R[`r'+1,2]  = r(se)
    mat R[`r',9]    = 100*R[`r',5]/R[`r',3]

    /* [> cm vs np <] */ 
    qui reg `var' promo_cm_np [pw = ips], cluster(psu)
    mat R[`r',4]    = _b[_cons]
    mat R[`r'+1,4]  = _se[_cons]
    mat R[`r',7]    = _b[promo_cm_np]
    mat R[`r'+1,7]  = _se[promo_cm_np]
    mat R[`r',8]    = r(table)[4,1]
    mat R[`r',10]   = 100*R[`r',7]/R[`r',4]

    local ++r
}

/* [> export table <] */ 
clear
svmat R, names(col)

tostring varname, replace
local r = 0
foreach var in $vars {
    local ++r
    replace varname = "`var'" if _n == `r'
    local ++r
}

export delimit "${dire}table2.csv", replace
/*------------------------------------ End of SECTION 1 ------------------------------------*/






/**********************************************************************/
/*  SECTION 2: Table 3. Balance of Outcome Variables
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> baseline obs <] */ 
keep if baseline == 1


/* [> list of variables <] */ 
gl vars ind_tot ind_srh ind_pfe ind_viol ind_pv ind_sea satis_life


/* [> results matrix <] */ 
local num : list sizeof global(vars)
mat R = J(`num'*2,10,.)
mat coln R = varname CM HU NP CM_HU Pval_CM_HU CM_NP Pval_CM_NP CM_HU_perc CM_NP_perc


/* [> estimates <] */ 
local r = 0
foreach var in $vars {
    local ++r

    /* [> cm vs hu <] */ 
    qui reg `var' promo_cm_hu [pw = ips], cluster(psu)
    mat R[`r',3]    = _b[_cons]
    mat R[`r'+1,3]  = _se[_cons]
    mat R[`r',5]    = _b[promo_cm_hu]
    mat R[`r'+1,5]  = _se[promo_cm_hu]
    mat R[`r',6]    = r(table)[4,1]
    qui lincom _cons + promo_cm_hu
    mat R[`r',2]    = r(estimate)
    mat R[`r'+1,2]  = r(se)
    mat R[`r',9]    = 100*R[`r',5]/R[`r',3]

    /* [> cm vs np <] */ 
    qui reg `var' promo_cm_np [pw = ips], cluster(psu)
    mat R[`r',4]    = _b[_cons]
    mat R[`r'+1,4]  = _se[_cons]
    mat R[`r',7]    = _b[promo_cm_np]
    mat R[`r'+1,7]  = _se[promo_cm_np]
    mat R[`r',8]    = r(table)[4,1]
    mat R[`r',10]   = 100*R[`r',7]/R[`r',4]

    local ++r
}

/* [> export table <] */ 
clear
svmat R, names(col)

tostring varname, replace
local r = 0
foreach var in $vars {
    local ++r
    replace varname = "`var'" if _n == `r'
    local ++r
}

export delimited "${dire}table3.csv", replace
/*------------------------------------ End of SECTION 2 ------------------------------------*/






/**********************************************************************/
/*  SECTION 3: Table 4. IV First-Stage
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

est clear


/*----------------------------------------------------*/
   /* [>   I.  FEs   <] */ 
/*----------------------------------------------------*/
/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> cm vs hu <] */ 
qui xtivreg2 ind_tot_zhu (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first ffirst savefirst savefp(f_)
mat first = e(first)
est restore f_useCM_post
qui estadd scalar f_first = first[4,1]
qui estadd scalar p_r2 = first[3,1]
est store cmhu_fe

/* [> cm vs np <] */ 
qui xtivreg2 ind_tot_znp (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first ffirst savefirst savefp(f_)
mat first = e(first)
est restore f_useCM_post
qui estadd scalar f_first = first[4,1]
qui estadd scalar p_r2 = first[3,1]
est store cmnp_fe


/*----------------------------------------------------*/
   /* [>   II.  Cross-Section   <] */ 
/*----------------------------------------------------*/
/* [> baseline covariates <] */ 
gl covs_bl  age_bl hh_head_bl couple_bl couple_hh_bl n_child_bl n_hab_bl n_child5_hh_bl literate_bl yeduc_bl ///
            socecon_ind_bl hh_owner_bl prec_hou_bl employed_bl ihs_labinc_u_bl socsec_u_bl

/* [> cm vs hu <] */ 
qui ivreg2 ind_tot_zhu (useCM = promoCM) ind_tot_zhu_bl ${covs_bl} if endline == 1 & (promo_asign == 1 | promo_asign == 2) [pw = ips], cluster(psu) first ffirst savefirst savefp(f_)
mat first = e(first)
est restore f_useCM
qui estadd scalar f_first  = first[4,1]
qui estadd scalar p_r2     = first[3,1]
est store cmhu_cs

/* [> cm vs np <] */ 
qui ivreg2 ind_tot_znp (useCM = promoCM) ind_tot_znp_bl ${covs_bl} if endline == 1 & (promo_asign == 1 | promo_asign == 3) [pw = ips], cluster(psu) first ffirst savefirst savefp(f_)
mat first = e(first)
est restore f_useCM
qui estadd scalar f_first  = first[4,1]
qui estadd scalar p_r2     = first[3,1]
est store cmnp_cs

 
/*----------------------------------------------------*/
   /* [>   III.  Export results   <] */ 
/*----------------------------------------------------*/
estout  cmhu_fe cmnp_fe cmhu_cs cmnp_cs using "${dire}table4.txt", ///
        keep(promoCM*) ///
        c(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(f_first p_r2 N, fmt(%9.1f %9.3f %9.0fc) labels("F-statistic" "Partial-R2" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Fixed-Effects" "Cross-Section", pattern(1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 3 ------------------------------------*/






/**********************************************************************/
/*  SECTION 4: Table 5. Impacts on Demand
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_use psv_cm_adm nsv_cm_adm pnhsv_cm_adm nnhsv_cm_adm

est clear


/*----------------------------------------------------*/
   /* [>   I.  FEs   <] */ 
/*----------------------------------------------------*/
/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_use {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_psv_cm_adm d_nsv_cm_adm d_pnhsv_cm_adm d_nnhsv_cm_adm


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_use {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_fe_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_use {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_fe_`var'
}


/*----------------------------------------------------*/
   /* [>   II.  Cross-section   <] */ 
/*----------------------------------------------------*/
/* [> baseline covariates <] */ 
gl covs_bl  age_bl hh_head_bl couple_bl couple_hh_bl n_child_bl n_hab_bl n_child5_hh_bl literate_bl yeduc_bl ///
            socecon_ind_bl hh_owner_bl prec_hou_bl employed_bl ihs_labinc_u_bl socsec_u_bl


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${depvar_use} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl}) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_use {
    local rwp_cmhu_cs_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_use {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM promoCM ${covs_bl})
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 `var' (useCM = promoCM) ${covs_bl} if endline == 1 & (promo_asign == 1 | promo_asign == 2) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_cs_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${depvar_use} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl}) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_use {
    local rwp_cmnp_cs_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_use {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM promoCM ${covs_bl})
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 `var' (useCM = promoCM) ${covs_bl} if endline == 1 & (promo_asign == 1 | promo_asign == 3) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_cs_`var'
}

 
/*----------------------------------------------------*/
   /* [>   III.  Export results   <] */ 
/*----------------------------------------------------*/
/* [> FEs <] */ 
estout  cmhu_fe_psv_cm_adm cmnp_fe_psv_cm_adm ///
        cmhu_fe_nsv_cm_adm cmnp_fe_nsv_cm_adm ///
        cmhu_fe_pnhsv_cm_adm cmnp_fe_pnhsv_cm_adm ///
        cmhu_fe_nnhsv_cm_adm cmnp_fe_nnhsv_cm_adm ///
        using "${dire}table5_fe.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Prob. subseq. visit CM" "N subseq. visits CM" "Prob. non rep. health subseq. visit CM" "N non rep. health subseq. visit CM", pattern(1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace

/* [> Cross-Section <] */ 
estout  cmhu_cs_psv_cm_adm cmnp_cs_psv_cm_adm ///
        cmhu_cs_nsv_cm_adm cmnp_cs_nsv_cm_adm ///
        cmhu_cs_pnhsv_cm_adm cmnp_cs_pnhsv_cm_adm ///
        cmhu_cs_nnhsv_cm_adm cmnp_cs_nnhsv_cm_adm ///
        using "${dire}table5_cs.xls", ///
        keep(useCM) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Prob. subseq. visit CM" "N subseq. visits CM" "Prob. non rep. health subseq. visit CM" "N non rep. health subseq. visit CM", pattern(1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 4 ------------------------------------*/






/**********************************************************************/
/*  SECTION 5: Table 6. Impact on Use of Social Services and Life Satisfaction
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_zhu ind_tot_zhu ind_srh_zhu ind_viol_zhu ind_pfe_zhu ind_pv_zhu ind_sea_zhu satis_life
gl depvar_znp ind_tot_znp ind_srh_znp ind_viol_znp ind_pfe_znp ind_pv_znp ind_sea_znp satis_life

est clear


/*----------------------------------------------------*/
   /* [>   I.  FEs   <] */ 
/*----------------------------------------------------*/
/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwofl command doesn't allow to include FEs
foreach v in $depvar_zhu $depvar_znp {
    cap gen `v'_t0 = `v' if baseline == 1
    cap gen `v'_t1 = `v' if endline == 1
    cap bys woman_id: egen `v'_0 = total(`v'_t0)
    cap bys woman_id: egen `v'_1 = total(`v'_t1)
    cap gen d_`v' = `v'_1 - `v'_0
}
gl y_diff_zhu d_ind_tot_zhu d_ind_srh_zhu d_ind_viol_zhu d_ind_pfe_zhu d_ind_pv_zhu d_ind_sea_zhu d_satis_life
gl y_diff_znp d_ind_tot_znp d_ind_srh_znp d_ind_viol_znp d_ind_pfe_znp d_ind_pv_znp d_ind_sea_znp d_satis_life


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff_zhu} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff_zhu {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_zhu {
    if "`var'" == "ind_viol_zhu" {
        gl inc_viol inc_viol
    }
    else {
        gl inc_viol ""
    }
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline ${inc_viol} if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_fe_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff_znp} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff_znp {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_znp {
    if "`var'" == "ind_viol_znp" {
        gl inc_viol inc_viol
    }
    else {
        gl inc_viol ""
    }
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline ${inc_viol} if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_fe_`var'
}


/*----------------------------------------------------*/
   /* [>   II.  Cross-section   <] */ 
/*----------------------------------------------------*/
/* [> baseline covariates <] */ 
gl covs_bl  age_bl hh_head_bl couple_bl couple_hh_bl n_child_bl n_hab_bl n_child5_hh_bl literate_bl yeduc_bl ///
            socecon_ind_bl hh_owner_bl prec_hou_bl employed_bl ihs_labinc_u_bl socsec_u_bl

/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${depvar_zhu} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl} ind_*_zhu_bl) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_zhu {
    local rwp_cmhu_cs_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_zhu {
    if "`var'" == "ind_viol_zhu" {
        gl inc_viol inc_viol
    }
    else {
        gl inc_viol ""
    }
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM promoCM ${covs_bl})
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 `var' (useCM = promoCM) `var'_bl ${covs_bl} ${inc_viol} if endline == 1 & (promo_asign == 1 | promo_asign == 2) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_cs_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${depvar_znp} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl} ind_*_znp_bl) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_znp {
    local rwp_cmnp_cs_`v' = e(rw_`v')
}
/* [> cm vs. np <] */ 
foreach var in $depvar_znp {
    if "`var'" == "ind_viol_znp" {
        gl inc_viol inc_viol
    }
    else {
        gl inc_viol ""
    }
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM promoCM ${covs_bl})
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 `var' (useCM = promoCM) `var'_bl ${covs_bl} ${inc_viol} if endline == 1 & (promo_asign == 1 | promo_asign == 3) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_cs_`var'
}

 
/*----------------------------------------------------*/
   /* [>   III.  Export results   <] */ 
/*----------------------------------------------------*/
/* [> FEs <] */ 
estout  cmhu_fe_ind_tot_zhu cmnp_fe_ind_tot_znp ///
        cmhu_fe_ind_srh_zhu cmnp_fe_ind_srh_znp ///
        cmhu_fe_ind_pfe_zhu cmnp_fe_ind_pfe_znp ///
        cmhu_fe_ind_viol_zhu cmnp_fe_ind_viol_znp ///
        cmhu_fe_ind_pv_zhu cmnp_fe_ind_pv_znp ///
        cmhu_fe_ind_sea_zhu cmnp_fe_ind_sea_znp ///
        cmhu_fe_satis_life cmnp_fe_satis_life ///
        using "${dire}table6_fe.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Aggregate index" "Index sex. and repro. health" "Index prom. employment" "Index violence" "Index patrim. violence" "Index econ. auton." "Life satisfaction", pattern(1 0 1 0 1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace

/* [> Cross-Section <] */ 
estout  cmhu_cs_ind_tot_zhu cmnp_cs_ind_tot_znp ///
        cmhu_cs_ind_srh_zhu cmnp_cs_ind_srh_znp ///
        cmhu_cs_ind_pfe_zhu cmnp_cs_ind_pfe_znp ///
        cmhu_cs_ind_viol_zhu cmnp_cs_ind_viol_znp ///
        cmhu_cs_ind_pv_zhu cmnp_cs_ind_pv_znp ///
        cmhu_cs_ind_sea_zhu cmnp_cs_ind_sea_znp ///
        cmhu_cs_satis_life cmnp_cs_satis_life ///
        using "${dire}table6_cs.xls", ///
        keep(useCM) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Aggregate index" "Index sex. and repro. health" "Index prom. employment" "Index violence" "Index patrim. violence" "Index econ. auton." "Life satisfaction", pattern(1 0 1 0 1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 5 ------------------------------------*/






/**********************************************************************/
/*  SECTION 6: Table 7. Compliers and Non-Compliers Sociodemographic Profile 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear


/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> baseline obs <] */ 
keep if baseline == 1


/* [> file to export results <] */ 
putexcel set "${dire}table7.xlsx", sheet(Sheet1) modify
putexcel    D4 = "CM vs. HU" I4 = "CM vs. NP"   ///
            D5 = "Mean" G5 = "Difference" I5 = "Mean" L5 = "Difference" ///
            D6 = "Compliers" E6 = "Always-Takers" F6 = "Never-Takers" I6 = "Compliers" J6 = "Always-Takers" K6 = "Never-Takers" ///
            G7 = "C - AT" H7 = "C - NT" L7 = "C - AT" M7 = "C - NT"  ///
            C8 = "Age" C10 = "Head household" C12 = "Married or with a partner" C14 = "Partner lives in the household" C16 = "N alive born children" ///
            C18 = "N children 0-5 years old living in the household" C20 = "N children 6-12 years old living in the household" C22 = "Literate" ///
            C24 = "Years of education" C26 = "Employed" C28 = "Labor income (IHS)" C30 = "Affiliated to a public or private soc. sec. system"   ///
            C32 = "Socioeconomic index" C34 = "Household owner"


local c = 0
foreach v of varlist age hh_head couple couple_hh n_child_bl n_child5_hh n_child_6_12_hh literate yeduc employed ihs_labinc_u socsec_u socecon_ind hh_owner {
    local ++c

    /*----------------------------------------------------*/
       /* [>   I.  CM vs. HU   <] */ 
    /*----------------------------------------------------*/
    /* [> instrument <] */ 
    gen inst = 1 if promo_asign == 1
    replace inst = 0 if promo_asign == 2
    
    /* [> obs <] */ 
    qui sum `v' if !mi(useCM) & !mi(inst) & promo_asign < 3
    local obs = r(N)

    /* [> estimates <] */ 
    qui ivdesc `v' useCM inst if promo_asign < 3
    mat A = r(ivdesc)
    local n_c = subinstr(`"`: di %12.0fc round(A[2,3]*`obs',1)'"', " ", "", .)
    local n_nt = subinstr(`"`: di %12.0fc round(A[3,3]*`obs',1)'"', " ", "", .)
    local n_at = subinstr(`"`: di %12.0fc round(A[4,3]*`obs',1)'"', " ", "", .)
    local mu_c : di %6.3fc A[2,1]
    local se_c = subinstr(`"`: di %6.3fc A[2,2]'"', " ", "", .)
    local mu_nt : di %6.3fc A[3,1]
    local se_nt = subinstr(`"`: di %6.3fc A[3,2]'"', " ", "", .)
    local mu_at : di %6.3fc A[4,1]
    local se_at = subinstr(`"`: di %6.3fc A[4,2]'"', " ", "", .)
    local d_c_nt : di %6.3fc `mu_c'-`mu_nt'
    local se_d_nt = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_nt'^2)'"', " ", "", .)
    local d_c_at : di %6.3fc `mu_c'-`mu_at'
    local se_d_at = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_at'^2)'"', " ", "", .)
    local p_dcnt = 2*(1 - normal(abs(`d_c_nt'/`se_d_nt')))
    local p_dcat = 2*(1 - normal(abs(`d_c_at'/`se_d_at')))
    gl stars_dcnt = cond(`p_dcnt'<.01,"***", cond(`p_dcnt'<.05,"**", cond(`p_dcnt'<.1,"*","")))
    gl stars_dcat = cond(`p_dcat'<.01,"***", cond(`p_dcat'<.05,"**", cond(`p_dcat'<.1,"*","")))

    local r1 = `c'+7
    local r2 = `r1'+1

    /* [> export results <] */ 
    putexcel    D7 = ("(N = `n_c')") E7 = ("(N = `n_at')") F7 = ("(N = `n_nt')")    ///
                D`r1' = ("`mu_c'") E`r1' = ("`mu_at'") F`r1' = ("`mu_nt'")          ///
                G`r1' = ("`d_c_at'${stars_dcat}") H`r1' = ("`d_c_nt'${stars_dcnt}") ///
                D`r2' = ("(`se_c')") E`r2' = ("(`se_at')") F`r2' = ("(`se_nt')")    ///
                G`r2' = ("(`se_d_at')") H`r2' = ("(`se_d_nt')")
    drop inst

    /*----------------------------------------------------*/
       /* [>   II.  CM vs. NP   <] */ 
    /*----------------------------------------------------*/
    /* [> instrument <] */ 
    gen inst = 1 if promo_asign == 1
    replace inst = 0 if promo_asign == 3

    /* [> obs <] */ 
    qui sum `v' if !mi(useCM) & !mi(inst) & promo_asign != 2
    local obs = r(N)
    
    /* [> estimates <] */ 
    qui ivdesc `v' useCM inst if promo_asign != 2
    mat B = r(ivdesc)
    local n_c = subinstr(`"`: di %12.0fc round(B[2,3]*`obs',1)'"', " ", "", .)
    local n_nt = subinstr(`"`: di %12.0fc round(B[3,3]*`obs',1)'"', " ", "", .)
    local n_at = subinstr(`"`: di %12.0fc round(B[4,3]*`obs',1)'"', " ", "", .)
    local mu_c : di %6.3fc B[2,1]
    local se_c = subinstr(`"`: di %6.3fc B[2,2]'"', " ", "", .)
    local mu_nt : di %6.3fc B[3,1]
    local se_nt = subinstr(`"`: di %6.3fc B[3,2]'"', " ", "", .)
    local mu_at : di %6.3fc B[4,1]
    local se_at = subinstr(`"`: di %6.3fc B[4,2]'"', " ", "", .)
    local d_c_nt : di %6.3fc `mu_c'-`mu_nt'
    local se_d_nt = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_nt'^2)'"', " ", "", .)
    local d_c_at : di %6.3fc `mu_c'-`mu_at'
    local se_d_at = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_at'^2)'"', " ", "", .)    
    local p_dcnt = 2*(1 - normal(abs(`d_c_nt'/`se_d_nt')))
    local p_dcat = 2*(1 - normal(abs(`d_c_at'/`se_d_at')))
    gl stars_dcnt = cond(`p_dcnt'<.01,"***", cond(`p_dcnt'<.05,"**", cond(`p_dcnt'<.1,"*","")))
    gl stars_dcat = cond(`p_dcat'<.01,"***", cond(`p_dcat'<.05,"**", cond(`p_dcat'<.1,"*","")))

    putexcel    I7 = ("(N = `n_c')") J7 = ("(N = `n_at')") K7 = ("(N = `n_nt')")    ///
                I`r1' = ("`mu_c'") J`r1' = ("`mu_at'") K`r1' = ("`mu_nt'")          ///
                L`r1' = ("`d_c_at'${stars_dcat}") M`r1' = ("`d_c_nt'${stars_dcnt}") ///
                I`r2' = ("(`se_c')") J`r2' = ("(`se_at')") K`r2' = ("(`se_nt')")    ///
                L`r2' = ("(`se_d_at')") M`r2' = ("(`se_d_nt')")
    drop inst
    
    local ++c
}
/*------------------------------------ End of SECTION 6 ------------------------------------*/






/**********************************************************************/
/*  SECTION 7: Table 8. Service Use in Baseline for Compliers and Non-Compliers
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear


/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> baseline obs <] */ 
keep if baseline == 1


/* [> file to export results <] */ 
putexcel set "${dire}table8.xlsx", sheet(Sheet1) modify
putexcel    D4 = "CM vs. HU" I4 = "CM vs. NP"   ///
            D5 = "Mean" G5 = "Difference" I5 = "Mean" L5 = "Difference" ///
            D6 = "Compliers" E6 = "Always-Takers" F6 = "Never-Takers" I6 = "Compliers" J6 = "Always-Takers" K6 = "Never-Takers" ///
            G7 = "C - AT" H7 = "C - NT" L7 = "C - AT" M7 = "C - NT"  ///
            C8 = "Aggregate index of public services" ///
            C10 = "Index of sexual and reproductive healthservices" ///
            C12 = "Index of services to promote female employment" ///
            C14 = "Index of psychological, medical and legal services for survivors of physical,sexual, and/or emotional violence" ///
            C16 = "Index of legal services for victims of patrimonial violence" ///
            C18 = "Index of legal services to strengthen economic autonomy" ///
            C20 = "Very Satisfied/Satisfied with life in general"


local c = 0
local ov = 0
foreach outv of varlist ind_tot ind_srh ind_viol ind_pfe ind_pv ind_sea satis_life {
    local ++ov
    local ++c

    /*----------------------------------------------------*/
       /* [>   I.  CM vs. HU   <] */ 
    /*----------------------------------------------------*/
    if `ov' < 7 {
        local v `outv'_zhu
    }
    else {
        local v `outv'
    }

    /* [> instrument <] */ 
    gen inst = 1 if promo_asign == 1
    replace inst = 0 if promo_asign == 2
    
    /* [> obs <] */ 
    qui sum `v' if !mi(useCM) & !mi(inst) & promo_asign < 3
    local obs = r(N)

    /* [> estimates <] */ 
    qui ivdesc `v' useCM inst if promo_asign < 3
    mat A = r(ivdesc)
    local n_c = subinstr(`"`: di %12.0fc round(A[2,3]*`obs',1)'"', " ", "", .)
    local n_nt = subinstr(`"`: di %12.0fc round(A[3,3]*`obs',1)'"', " ", "", .)
    local n_at = subinstr(`"`: di %12.0fc round(A[4,3]*`obs',1)'"', " ", "", .)
    local mu_c : di %6.3fc A[2,1]
    local se_c = subinstr(`"`: di %6.3fc A[2,2]'"', " ", "", .)
    local mu_nt : di %6.3fc A[3,1]
    local se_nt = subinstr(`"`: di %6.3fc A[3,2]'"', " ", "", .)
    local mu_at : di %6.3fc A[4,1]
    local se_at = subinstr(`"`: di %6.3fc A[4,2]'"', " ", "", .)
    local d_c_nt : di %6.3fc `mu_c'-`mu_nt'
    local se_d_nt = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_nt'^2)'"', " ", "", .)
    local d_c_at : di %6.3fc `mu_c'-`mu_at'
    local se_d_at = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_at'^2)'"', " ", "", .)
    local p_dcnt = 2*(1 - normal(abs(`d_c_nt'/`se_d_nt')))
    local p_dcat = 2*(1 - normal(abs(`d_c_at'/`se_d_at')))
    gl stars_dcnt = cond(`p_dcnt'<.01,"***", cond(`p_dcnt'<.05,"**", cond(`p_dcnt'<.1,"*","")))
    gl stars_dcat = cond(`p_dcat'<.01,"***", cond(`p_dcat'<.05,"**", cond(`p_dcat'<.1,"*","")))

    local r1 = `c'+7
    local r2 = `r1'+1

    /* [> export results <] */ 
    putexcel    D7 = ("(N = `n_c')") E7 = ("(N = `n_at')") F7 = ("(N = `n_nt')")    ///
                D`r1' = ("`mu_c'") E`r1' = ("`mu_at'") F`r1' = ("`mu_nt'")          ///
                G`r1' = ("`d_c_at'${stars_dcat}") H`r1' = ("`d_c_nt'${stars_dcnt}") ///
                D`r2' = ("(`se_c')") E`r2' = ("(`se_at')") F`r2' = ("(`se_nt')")    ///
                G`r2' = ("(`se_d_at')") H`r2' = ("(`se_d_nt')")
    drop inst


    /*----------------------------------------------------*/
       /* [>   II.  CM vs. NP   <] */ 
    /*----------------------------------------------------*/
    if `ov' < 7 {
        local v `outv'_znp
    }
    else {
        local v `outv'
    }

    gen inst = 1 if promo_asign == 1
    replace inst = 0 if promo_asign == 3

    qui sum `v' if !mi(useCM) & !mi(inst) & promo_asign != 2
    local obs = r(N)
    qui ivdesc `v' useCM inst if promo_asign != 2
    mat B = r(ivdesc)
    local n_c = subinstr(`"`: di %12.0fc round(B[2,3]*`obs',1)'"', " ", "", .)
    local n_nt = subinstr(`"`: di %12.0fc round(B[3,3]*`obs',1)'"', " ", "", .)
    local n_at = subinstr(`"`: di %12.0fc round(B[4,3]*`obs',1)'"', " ", "", .)
    local mu_c : di %6.3fc B[2,1]
    local se_c = subinstr(`"`: di %6.3fc B[2,2]'"', " ", "", .)
    local mu_nt : di %6.3fc B[3,1]
    local se_nt = subinstr(`"`: di %6.3fc B[3,2]'"', " ", "", .)
    local mu_at : di %6.3fc B[4,1]
    local se_at = subinstr(`"`: di %6.3fc B[4,2]'"', " ", "", .)
    local d_c_nt : di %6.3fc `mu_c'-`mu_nt'
    local se_d_nt = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_nt'^2)'"', " ", "", .)
    local d_c_at : di %6.3fc `mu_c'-`mu_at'
    local se_d_at = subinstr(`"`: di %6.3fc sqrt(`se_c'^2 + `se_at'^2)'"', " ", "", .)
    local p_dcnt = 2*(1 - normal(abs(`d_c_nt'/`se_d_nt')))
    local p_dcat = 2*(1 - normal(abs(`d_c_at'/`se_d_at')))
    gl stars_dcnt = cond(`p_dcnt'<.01,"***", cond(`p_dcnt'<.05,"**", cond(`p_dcnt'<.1,"*","")))
    gl stars_dcat = cond(`p_dcat'<.01,"***", cond(`p_dcat'<.05,"**", cond(`p_dcat'<.1,"*","")))

    /* [> export results <] */ 
    putexcel    I7 = ("(N = `n_c')") J7 = ("(N = `n_at')") K7 = ("(N = `n_nt')")    ///
                I`r1' = ("`mu_c'") J`r1' = ("`mu_at'") K`r1' = ("`mu_nt'")          ///
                L`r1' = ("`d_c_at'${stars_dcat}") M`r1' = ("`d_c_nt'${stars_dcnt}") ///
                I`r2' = ("(`se_c')") J`r2' = ("(`se_at')") K`r2' = ("(`se_nt')")    ///
                L`r2' = ("(`se_d_at')") M`r2' = ("(`se_d_nt')")
    drop inst

    local ++c
}
/*------------------------------------ End of SECTION 7 ------------------------------------*/






/**********************************************************************/
/*  SECTION 8: Table 9. Impacts on Demand. Heterogeneous Effects 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id


/* [> list of outcomes <] */ 
gl depvar_use psv_cm_adm nsv_cm_adm pnhsv_cm_adm nnhsv_cm_adm


est clear

foreach het in si yed age nchild {
    /*----------------------------------------------------*/
       /* [>   I.  Estimates   <] */ 
    /*----------------------------------------------------*/
    foreach var in $depvar_use {
        /* [> cm vs. hu <] */ 
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' useCM_`het'_post promoCM_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui xtivreg2 `var' (useCM_post useCM_`het'_post = promoCM_post promoCM_`het'_post) endline `het'_post if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu)
        qui estadd scalar meanc = `meanc'
        est store cmhu_`het'_`var'

        /* [> cm vs. np <] */ 
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' useCM_`het'_post promoCM_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui xtivreg2 `var' (useCM_post useCM_`het'_post = promoCM_post promoCM_`het'_post) endline `het'_post if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu)
        qui estadd scalar meanc = `meanc'
        est store cmnp_`het'_`var'
    }
    /*----------------------------------------------------*/
       /* [>   II.  Export results   <] */ 
    /*----------------------------------------------------*/
    estout  cmhu_`het'_psv_cm_adm cmnp_`het'_psv_cm_adm ///
            cmhu_`het'_nsv_cm_adm cmnp_`het'_nsv_cm_adm ///
            cmhu_`het'_pnhsv_cm_adm cmnp_`het'_pnhsv_cm_adm ///
            cmhu_`het'_nnhsv_cm_adm cmnp_`het'_nnhsv_cm_adm ///
            using "${dire}table9_`het'.xls", ///
            keep(useCM_`het'_post) ///
            cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
            stats(meanc N, fmt(%9.3f %9.0fc) labels("Control Group's Mean" "N")) ///
            collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
            mgroups("Prob. subseq. visit CM" "N subseq. visits CM" "Prob. non rep. health subseq. visit CM" "N non rep. health subseq. visit CM", pattern(1 0 1 0 1 0 1 0)) ///
            mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU"  "CM vs. HU") ///
            note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
            replace
}
/*------------------------------------ End of SECTION 8 ------------------------------------*/






/**********************************************************************/
/*  SECTION 9: Table 10. Impact on Use of Social Services and Life Satisfaction. Heterogeneous Effects 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id


/* [> list of outcomes <] */ 
gl depvar_zhu ind_tot_zhu ind_srh_zhu ind_pfe_zhu ind_viol_zhu ind_pv_zhu ind_sea_zhu satis_life
gl depvar_znp ind_tot_znp ind_srh_znp ind_pfe_znp ind_viol_znp ind_pv_znp ind_sea_znp satis_life


est clear

foreach het in si yed age nchild {
    /*----------------------------------------------------*/
       /* [>   I.  Estimates   <] */ 
    /*----------------------------------------------------*/
    /* [> cm vs. hu <] */
    foreach var in $depvar_zhu {
        if "`var'" == "ind_viol_zhu" {
            gl inc_viol inc_viol
        }
        else {
            gl inc_viol ""
        }
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' useCM_`het'_post promoCM_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui xtivreg2 `var' (useCM_post useCM_`het'_post = promoCM_post promoCM_`het'_post) `het'_post endline ${inc_viol} if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu)
        qui estadd scalar meanc = `meanc'
        est store cmhu_`het'_`var'
    }
    /* [> cm vs. np <] */ 
    foreach var in $depvar_znp {
        if "`var'" == "ind_viol_znp" {
            gl inc_viol inc_viol
        }
        else {
            gl inc_viol ""
        }
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' useCM_`het'_post promoCM_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui xtivreg2 `var' (useCM_post useCM_`het'_post = promoCM_post promoCM_`het'_post) `het'_post endline ${inc_viol} if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu)
        qui estadd scalar meanc = `meanc'
        est store cmnp_`het'_`var'
    }
    /*----------------------------------------------------*/
       /* [>   II.  Export results   <] */ 
    /*----------------------------------------------------*/
    estout  cmhu_`het'_ind_tot_zhu cmnp_`het'_ind_tot_znp ///
            cmhu_`het'_ind_srh_zhu cmnp_`het'_ind_srh_znp ///
            cmhu_`het'_ind_pfe_zhu cmnp_`het'_ind_pfe_znp ///
            cmhu_`het'_ind_viol_zhu cmnp_`het'_ind_viol_znp ///
            cmhu_`het'_ind_pv_zhu cmnp_`het'_ind_pv_znp ///
            cmhu_`het'_ind_sea_zhu cmnp_`het'_ind_sea_znp ///
            cmhu_`het'_satis_life cmnp_`het'_satis_life ///
            using "${dire}table10_`het'.xls", ///
            keep(useCM_`het'_post) ///
            cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
            stats(meanc N, fmt(%9.3f %9.0fc) labels("Control Group's Mean" "N")) ///
            collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
            mgroups("Aggregate index" "Index sex. and repro. health" "Index prom. employment" "Index violence" "Index patrim. violence" "Index econ. auton." "Life satisfaction", pattern(1 0 1 0 1 0 1 0 1 0 1 0 1 0)) ///
            mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU") ///
            note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
            replace
}
/*------------------------------------ End of SECTION 9 ------------------------------------*/






/**********************************************************************/
/*  SECTION 10: Table A1. Sample Composition and Attrition
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear


/* [> file to export results <] */ 
putexcel set "${dire}tableA1.xls", sheet(Sheet1) modify
putexcel    E7 = "Promotion Group" H7 = "Total" ///
            E8 = "CM" F8 = "HU" G8 = "NP"   ///
            D9 = "Original selected sample"  ///
            D10 = "Baseline"    ///
            D11 = "Endline"     ///
            D12 = "Attrition (%)"

mat R = J(4,4,0)
forval p = 1/3 {
    qui count if baseline == 1 & promo_asign == `p'
    mat R[1,`p'] = r(N)
    mat R[1,4] = R[1,4] + R[1,`p']
    qui count if endline == 1 & promo_asign == `p'
    mat R[2,`p'] = r(N)
    mat R[2,4] = R[2,4] + R[2,`p']
    qui count if baseline == 1 & panel_bal == 1 & promo_asign == `p'
    mat R[3,`p'] = r(N)
    mat R[3,4] = R[3,4] + R[3,`p']

    mat R[4,`p'] = 100*(R[2,`p']-R[3,`p'])/R[2,`p']
}
mat R[4,4] = 100*(R[2,4]-R[3,4])/R[2,4]

/* [> export results <] */ 
putexcel    E9  = mat(R)
/*------------------------------------ End of SECTION 10 ------------------------------------*/






/**********************************************************************/
/*  SECTION 11: Table A2. Intention-to-Treat (ITT) Impacts on Demand
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear


/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_use psv_cm_adm nsv_cm_adm pnhsv_cm_adm nnhsv_cm_adm


/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${depvar_use} [pw = ips], method(areg) abs(woman_id) indepvar(promoCM_post promoHU_post) controls(endline) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_use {
    local rwp_itt_pcm_`v' = e(rw_`v'_promoCM_post)
    local rwp_itt_phu_`v' = e(rw_`v'_promoHU_post)
}
/* [> itt estimates <] */ 
est clear
foreach var in $depvar_use {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' promoCM_post promoHU_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui areg `var' promoCM_post promoHU_post endline [pw = ips], abs(woman_id) vce(cluster psu)
    qui estadd scalar rwp_pcm = `rwp_itt_pcm_`var''
    qui estadd scalar rwp_phu = `rwp_itt_phu_`var''
    qui estadd scalar meanc = `meanc'
    est store itt_`var'
}

/* [> export results <] */ 
estout  itt_psv_cm_adm itt_nsv_cm_adm ///
        itt_pnhsv_cm_adm itt_nnhsv_cm_adm ///
        using "${dire}tableA2.xls", ///
        keep(promoCM_post promoHU_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp_pcm rwp_phu meanc N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("R&W p-value Promot. CM" "R&W p-value Promot. HU" "Control Group's Mean (NP)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mlabels("Prob. subseq. visit CM" "N subseq. visits CM" "Prob. non rep. health subseq. visit CM" "N non rep. health subseq. visit CM") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 11 ------------------------------------*/






/**********************************************************************/
/*  SECTION 12: Table A3. Intention-to-Treat (ITT) Impacts on Use of Services and Life Satisfaction
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear


/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_znp ind_tot_znp ind_srh_znp ind_pfe_znp ind_viol_znp ind_pv_znp ind_sea_znp satis_life

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${depvar_znp} [pw = ips], method(areg) abs(woman_id) indepvar(promoCM_post promoHU_post) controls(endline) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_znp {
    local rwp_itt_pcm_`v' = e(rw_`v'_promoCM_post)
    local rwp_itt_phu_`v' = e(rw_`v'_promoHU_post)
}
/* [> itt estimates <] */ 
est clear
foreach var in $depvar_znp {
    if "`var'" == "ind_viol_znp" {
        gl inc_viol inc_viol
    }
    else {
        gl inc_viol ""
    }
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' promoCM_post promoHU_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui areg `var' promoCM_post promoHU_post endline ${inc_viol} [pw = ips], abs(woman_id) vce(cluster psu)
    qui estadd scalar rwp_pcm = `rwp_itt_pcm_`var''
    qui estadd scalar rwp_phu = `rwp_itt_phu_`var''
    qui estadd scalar meanc = `meanc'
    est store itt_`var'
}

/* [> export results <] */ 
estout  itt_ind_tot_znp itt_ind_srh_znp ///
        itt_ind_pfe_znp itt_ind_viol_znp ///
        itt_ind_pv_znp itt_ind_sea_znp itt_satis_life ///
        using "${dire}tableA3.xls", ///
        keep(promoCM_post promoHU_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp_pcm rwp_phu meanc N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("R&W p-value Promot. CM" "R&W p-value Promot. HU" "Control Group's Mean (NP)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mlabels("Aggregate index" "Index sex. and repro. health" "Index prom. employment" "Index violence" "Index patrim. violence" "Index econ. auton." "Life satisfaction") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 12 ------------------------------------*/






/**********************************************************************/
/*  SECTION 13: Table A4. Intention-to-Treat (ITT) Impacts on Demand. Heterogeneous Effects
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_use psv_cm_adm nsv_cm_adm pnhsv_cm_adm nnhsv_cm_adm


est clear
foreach het in si yed age nchild {
    /* [> itt estimates <] */ 
    foreach var in $depvar_use {
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' promoCM_`het'_post promoHU_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui areg `var' promoCM_post promoHU_post promoCM_`het'_post promoHU_`het'_post `het'_post endline [pw = ips], abs(woman_id) vce(cluster psu)
        qui estadd scalar meanc = `meanc'
        est store itt_`het'_`var'
    }
    /* [> export results <] */ 
    estout  itt_`het'_psv_cm_adm itt_`het'_nsv_cm_adm ///
            itt_`het'_pnhsv_cm_adm itt_`het'_nnhsv_cm_adm ///
            using "${dire}tableA4_`het'.xls", ///
            keep(promoCM_`het'_post promoHU_`het'_post) ///
            cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
            stats(meanc N, fmt(%9.3f %9.0fc) labels("Control Group's Mean (NP)" "N")) ///
            collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
            mlabels("Prob. subseq. visit CM" "N subseq. visits CM" "Prob. non rep. health subseq. visit CM" "N non rep. health subseq. visit CM") ///
            note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
            replace
}
/*------------------------------------ End of SECTION 13 ------------------------------------*/






/**********************************************************************/
/*  SECTION 14: Table A5. Intention-to-Treat (ITT) Impacts on Use of Services and Life Satisfaction. Heterogeneous Effects 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_znp ind_tot_znp ind_srh_znp ind_pfe_znp ind_viol_znp ind_pv_znp ind_sea_znp satis_life


est clear
foreach het in si yed age nchild {
    /* [> itt estimates <] */ 
    foreach var in $depvar_znp {
        if "`var'" == "ind_viol_znp" {
            gl inc_viol inc_viol
        }
        else {
            gl inc_viol ""
        }
        tempvar miss_count
        egen `miss_count' = rowmiss(`var' promoCM_`het'_post promoHU_`het'_post)
        qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
        local meanc = e(b)[1,1]
        qui areg `var' promoCM_post promoHU_post promoCM_`het'_post promoHU_`het'_post `het'_post endline ${inc_viol} [pw = ips], abs(woman_id) vce(cluster psu)
        qui estadd scalar meanc = `meanc'
        est store itt_`het'_`var'
    }
    /* [> export results <] */ 
    estout  itt_`het'_ind_tot_znp itt_`het'_ind_srh_znp ///
            itt_`het'_ind_pfe_znp itt_`het'_ind_viol_znp ///
            itt_`het'_ind_pv_znp itt_`het'_ind_sea_znp ///
            itt_`het'_satis_life ///
            using "${dire}tableA5_`het'.xls", ///
            keep(promoCM_`het'_post promoHU_`het'_post) ///
            cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
            stats(meanc N, fmt(%9.3f %9.0fc) labels("Control Group's Mean (NP)" "N")) ///
            collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
            mlabels("Aggregate index" "Index sex. and repro. health" "Index prom. employment" "Index violence" "Index patrim. violence" "Index econ. auton." "Life satisfaction") ///
            note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
            replace
}
/*------------------------------------ End of SECTION 14 ------------------------------------*/






/**********************************************************************/
/*  SECTION 15: Table A6. Impact on Use of Sexual and Reproductive Health Services 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_srh citolpap_12m_sc mamog_12m_sc cont_pren_12m cont_odont_12m contpost_12m

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_srh {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_citolpap_12m_sc d_mamog_12m_sc d_cont_pren_12m d_cont_odont_12m d_contpost_12m


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_srh {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'                   // control's group mean
    mat arpval = e(archi2p)                             // Anderson-Rubin Wald chi-sq matrix
    qui estadd scalar ar_p = arpval[1,1]                // Anderson-Rubin Wald chi-sq p-value
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_srh {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'                   // control's group mean
    mat arpval = e(archi2p)                             // Anderson-Rubin Wald chi-sq matrix
    qui estadd scalar ar_p = arpval[1,1]                // Anderson-Rubin Wald chi-sq p-value
    est store cmnp_`var'
}


/* [> export results <] */ 
estout  cmhu_citolpap_12m_sc cmnp_citolpap_12m_sc ///
        cmhu_mamog_12m_sc cmnp_mamog_12m_sc ///
        cmhu_cont_pren_12m cmnp_cont_pren_12m ///
        cmhu_cont_odont_12m cmnp_cont_odont_12m ///
        cmhu_contpost_12m cmnp_contpost_12m ///
        using "${dire}tableA6.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Cytology/Pap" "Mammography" "Prenatal checkup" "Dental checkup" "Postnatal checkup", pattern(1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. HU" "CM vs. NP" "CM vs. HU") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 15 ------------------------------------*/






/**********************************************************************/
/*  SECTION 16: Table A7. Impact on Use of Legal Services for Patrimonial Violence
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_pv recpat_12m alimony_12m prop_leg_12m

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_pv {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_recpat_12m d_alimony_12m d_prop_leg_12m


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_pv {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */
foreach var in $depvar_pv { 
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}


/* [> export results <] */ 
estout  cmhu_recpat_12m cmnp_recpat_12m ///
        cmhu_alimony_12m cmnp_alimony_12m ///
        cmhu_prop_leg_12m cmnp_prop_leg_12m ///
        using "${dire}tableA7.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Paternity Ackn." "Alimony" "Leg. Property", pattern(1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 16 ------------------------------------*/






/**********************************************************************/
/*  SECTION 17: Table A8. Impact on Use of Services to Strength Economic Autonomy
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_sea ident_card_12m birth_cert_12m

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_sea {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_ident_card_12m d_birth_cert_12m


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_sea {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_sea {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}


/* [> export results <] */ 
estout  cmhu_ident_card_12m cmnp_ident_card_12m ///
        cmhu_birth_cert_12m cmnp_birth_cert_12m ///
        using "${dire}tableA8.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Help obtain ID" "Help obtain birth cert.", pattern(1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 17 ------------------------------------*/






/**********************************************************************/
/*  SECTION 18: Table A9. Impact on Use of Psychological, Medical, and Legal Services for Survivors of Physical, Sexual, and/or Emotional Violence
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_viol sr_emosupp sr_legsupp sr_injprot sr_medaid sr_transport sr_complaint

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_viol {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_sr_emosupp d_sr_legsupp d_sr_injprot d_sr_medaid d_sr_transport d_sr_complaint


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_viol {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */
foreach var in $depvar_viol { 
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}


/* [> export results <] */ 
estout  cmhu_sr_emosupp cmnp_sr_emosupp ///
        cmhu_sr_legsupp cmnp_sr_legsupp ///
        cmhu_sr_injprot cmnp_sr_injprot ///
        cmhu_sr_medaid cmnp_sr_medaid ///
        cmhu_sr_transport cmnp_sr_transport ///
        cmhu_sr_complaint cmnp_sr_complaint ///
        using "${dire}tableA9.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("S/R Emot. support" "S/R Legal support" "S/R Injunct. for Protection" "S/R Medical aid" "S/R Transportation support" "S/R Support put complaint", pattern(1 0 1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 18 ------------------------------------*/






/**********************************************************************/
/*  SECTION 19: Table A10. Impact on Use of Services to Promote Female Employment 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_pfe job_train_12m job_plss_12m orient_bus_12m cred_monsup_12m

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_pfe {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_job_train_12m d_job_plss_12m d_orient_bus_12m d_cred_monsup_12m


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_pfe {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_pfe {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}


/* [> export results <] */ 
estout  cmhu_job_train_12m cmnp_job_train_12m ///
        cmhu_job_plss_12m cmnp_job_plss_12m ///
        cmhu_orient_bus_12m cmnp_orient_bus_12m ///
        cmhu_cred_monsup_12m cmnp_cred_monsup_12m ///
        using "${dire}tableA10.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Job training" "Job placement SS" "Orientation own business" "Credit/monetary support for busin.", pattern(1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 19 ------------------------------------*/






/**********************************************************************/
/*  SECTION 20: Table A11. Impact on Reproductive Health Outcomes
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_rho use_contr use_mod_contr

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_rho {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_use_contr d_use_mod_contr


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */
foreach var in $depvar_rho { 
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_rho { 
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}

/* [> export results <] */ 
estout  cmhu_use_contr cmnp_use_contr ///
        cmhu_use_mod_contr cmnp_use_mod_contr ///
        using "${dire}tableA11.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Uses any contraceptive method" "Uses a modern contraceptive method", pattern(1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 20 ------------------------------------*/






/**********************************************************************/
/*  SECTION 21: Table A12. Impact on Health Outcomes [cross-section]
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1

/* [> endline obs <] */ 
keep if endline == 1


/* [> list of outcomes <] */ 
gl depvar_ho any mammog hypert diab obes

/* [> baseline covariates <] */ 
gl covs_bl  age_bl hh_head_bl couple_bl couple_hh_bl n_child_bl n_hab_bl n_child5_hh_bl literate_bl yeduc_bl ///
            socecon_ind_bl hh_owner_bl prec_hou_bl employed_bl ihs_labinc_u_bl socsec_u_bl

est clear


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf treatm_any treatm_mammog treatm_hypert treatm_diab treatm_obes if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl} diag_*) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_ho {
    local rwp_cmhu_cs_`v' = e(rw_treatm_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_ho {
    tempvar miss_count
    egen `miss_count' = rowmiss(treatm_`var' useCM promoCM diag_`var' ${covs_bl})
    qui mean treatm_`var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 treatm_`var' (useCM = promoCM) diag_`var' ${covs_bl} if (promo_asign == 1 | promo_asign == 2) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_treatm_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf treatm_any treatm_mammog treatm_hypert treatm_diab treatm_obes if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM) iv(promoCM) controls(${covs_bl} diag_*) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $depvar_ho {
    local rwp_cmnp_cs_`v' = e(rw_treatm_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_ho {
    tempvar miss_count
    egen `miss_count' = rowmiss(treatm_`var' useCM promoCM diag_`var' ${covs_bl})
    qui mean treatm_`var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui ivreg2 treatm_`var' (useCM = promoCM) diag_`var' ${covs_bl} if (promo_asign == 1 | promo_asign == 3) [pw = ips], cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_cs_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_treatm_`var'
}


/* [> export results <] */ 
estout  cmhu_treatm_any cmnp_treatm_any ///
        cmhu_treatm_mammog cmnp_treatm_mammog ///
        cmhu_treatm_hypert cmnp_treatm_hypert ///
        cmhu_treatm_diab cmnp_treatm_diab ///
        cmhu_treatm_obes cmnp_treatm_obes ///
        using "${dire}tableA12.xls", ///
        keep(useCM) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Follows any treatment" "Treatment Mammog." "Treatment Hypert." "Treatment Diab." "Treatment Obes.", pattern(1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 21 ------------------------------------*/






/**********************************************************************/
/*  SECTION 22: Table A13. Impact on Labor Outcomes
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_labo employed ihs_labinc_u socsec_u

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_labo {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_employed d_ihs_labinc_u d_socsec_u


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_labo {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_labo {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}

/* [> export results <] */ 
estout  cmhu_employed cmnp_employed ///
        cmhu_ihs_labinc_u cmnp_ihs_labinc_u ///
        cmhu_socsec_u cmnp_socsec_u ///
        using "${dire}tableA13.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Employment" "IHS Labor Income" "Formal Work", pattern(1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. NP" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 22 ------------------------------------*/






/**********************************************************************/
/*  SECTION 23: Table A14. Impact on Intimate Partner Violence 
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_ipv ipv_any_12m ipv_phys_12m ipv_sex_12m ipv_emo_12m ipv_pat_12m

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id

/* [> variables for romano & wolf multiple testing <] */ // rwolf command doesn't allow to include FEs
foreach v in $depvar_ipv {
    gen `v'_t0 = `v' if baseline == 1
    gen `v'_t1 = `v' if endline == 1
    bys woman_id: egen `v'_0 = total(`v'_t0)
    bys woman_id: egen `v'_1 = total(`v'_t1)
    gen d_`v' = `v'_1 - `v'_0
}
gl y_diff d_ipv_any_12m d_ipv_phys_12m d_ipv_sex_12m d_ipv_emo_12m d_ipv_pat_12m


/* [> cm vs. hu romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 2) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) controls(inc_viol) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmhu_fe_`v' = e(rw_`v')
}
/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_ipv {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline inc_viol if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmhu_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np romano & wolf p-values <] */ 
qui rwolf ${y_diff} if (promo_asign == 1 | promo_asign == 3) & endline == 1 [pw = ips], method(ivregress) indepvar(useCM_post) iv(promoCM_post) controls(inc_viol) vce(cluster psu) cluster(psu) seed(12345) reps(1000)
foreach v in $y_diff {
    local rwp_cmnp_fe_`v' = e(rw_`v')
}
/* [> cm vs. np estimates <] */ 
foreach var in $depvar_ipv {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline inc_viol if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar rwp = `rwp_cmnp_fe_d_`var''
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}

/* [> export results <] */ 
estout  cmhu_ipv_any_12m cmnp_ipv_any_12m ///
        cmhu_ipv_phys_12m cmnp_ipv_phys_12m ///
        cmhu_ipv_sex_12m cmnp_ipv_sex_12m ///
        cmhu_ipv_emo_12m cmnp_ipv_emo_12m ///
        cmhu_ipv_pat_12m cmnp_ipv_pat_12m ///
        using "${dire}tableA14.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(rwp meanc ar_p N, fmt(%9.3f %9.3f %9.3f %9.0fc) labels("Romano & Wolf p-value" "Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("IPV Any" "IPV Phys." "IPV Sex." "IPV Emo." "IPV Pat.", pattern(1 0 1 0 1 0 1 0 1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP" "CM vs. HU" "CM vs. NP" "CM vs. NP" "CM vs. NP" "CM vs. NP" "CM vs. NP" "CM vs. NP" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 23 ------------------------------------*/






/**********************************************************************/
/*  SECTION 24: Table A15. Impact on Female Empowerment
    Notes: */
/**********************************************************************/
/* [> load data <] */ 
use "${dire}analysis_data.dta", clear

/* [> balanced panel <] */ 
keep if panel_bal == 1


/* [> list of outcomes <] */ 
gl depvar_idm_zhu i_decmak_zhu
gl depvar_idm_znp i_decmak_znp

est clear


/* [> set woman's id for FEs <] */ 
xtset, clear
xtset woman_id


/* [> cm vs. hu estimates <] */ 
foreach var in $depvar_idm_zhu {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 2 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 2) [pw = ips], fe cluster(psu) first
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmhu_`var'
}

/* [> cm vs. np estimates <] */ 
foreach var in $depvar_idm_znp {
    tempvar miss_count
    egen `miss_count' = rowmiss(`var' useCM_post promoCM_post)
    qui mean `var' if `miss_count' == 0 & promo_asign == 3 & endline == 1 [pw = ips], cluster(psu)
    local meanc = e(b)[1,1]
    qui xtivreg2 `var' (useCM_post = promoCM_post) endline if (promo_asign == 1 | promo_asign == 3) [pw = ips], fe cluster(psu) first
    qui estadd scalar meanc = `meanc'
    mat arpval = e(archi2p)
    qui estadd scalar ar_p = arpval[1,1]
    est store cmnp_`var'
}

/* [> export results <] */ 
estout  cmhu_i_decmak_zhu cmnp_i_decmak_znp ///
        using "${dire}tableA15.xls", ///
        keep(useCM_post) ///
        cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par)) ///
        stats(meanc ar_p N, fmt(%9.3f %9.3f %9.0fc) labels("Control Group's Mean" "Anderson-Rubin test (p-value)" "N")) ///
        collabels(none) varwidth(30) modelwidth(10) unstack label starlevels(* 0.1 ** 0.05 *** 0.01) ///
        mgroups("Dec. making at home index", pattern(1 0)) ///
        mlabels("CM vs. HU" "CM vs. NP") ///
        note("*** p<0.01, ** p<0.05, * p<0.1. Standard errors clustered at PSU level in parentheses. All observations weighted by the inverse probability of being selected in the sample (IPW).") ///
        replace
/*------------------------------------ End of SECTION 24 ------------------------------------*/


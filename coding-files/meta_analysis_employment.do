************************************
** Change working directory and start log
*cd E:\Dropbox\BID-GRADE\meta-analisis
*cd C:\Users\duriarte\Dropbox\BID-GRADE\meta-analisis
cd C:\Users\bteruya\Dropbox\BID-GRADE\meta-analisis

******************************************************************************
*EMPLOYMENT SECTION 
******************************************************************************	
use "data\employment_meta_categories_v2.dta", clear

************************************
** Macro definitions

// general options for meta-analysis, see documentation
global gen_options sortby(y) random label(namevar=evaluation_id)

// general options for forest plot, see documentation
global 	graph_options  boxsca(50) boxopt(mfcolor(%50) mlwidth(none)) ///
		nowt graphregion(color(white) lwidth(thin)) ///
		subtitle("Change in probability of employment due to intervention", size(vsmall))
//change format of graph output
global format_graph emf 
//base name for all files in folder
global plots_dir "plots\employment" 


************************************
** Meta-analysis and plots


metan y se, $gen_options $graph_options ///
title("Forestplot of Evaluation ID", size(small))
graph export "$plots_dir/employment_overall_v2.$format_graph", as($format_graph) replace

********************************************************************************
*forestplot for each category in one graph:

use "data\employment_meta_categories_v2.dta", clear


*all categories are string
codebook length_intervention2 skills training where_class implementer 
replace where_class="Public-Private" if where_class=="Public, Private"
replace length_intervention2="Less_480" if length_intervention2=="Less or equal than 480 hours"
replace length_intervention2="More_480" if length_intervention2=="More than 480 hours"
replace lac="LAC" if lac=="yes"
replace lac="Other" if lac=="no"

tempfile employment
save `employment', replace
codebook length_intervention2 skills training where_class implementer 


*creating the database were we´ll save the estimates
clear
local obs=1
set obs `obs'
gen value=""
gen ES=.
gen seES=.
gen category=""
gen ci_low=.
gen ci_upp=.
gen n_evaluations=.

label var value "value of category"
label var ES "Effect size"
label var seES "Standard Error of the effect size"
label var category "category"
label var ci_low "Lower Bound of IC"
label var ci_upp "Upper Bound of IC"
label var n_evaluations "Number of evaluations per category"

label data "Dataset of estimates of categories in the meta analysis"

tempfile plots_cat
save `plots_cat', replace


*-----------------------------------------------------------------------
*trying the loop for only one value of category
local variable "length_intervention2"
local value "Less or equal than 480 hours"
use "data\employment_meta_categories.dta", clear

metan y se if `variable'=="`value'" ,random

use  `plots_cat', clear
replace ES = r(ES) in `obs'
replace seES = r(seES) in `obs'
replace value="`variable'" in `obs'
replace category="`value'" in `obs'
local obs = `obs'+1
set obs `obs'
*it works
*now let´s do it for all categories

*-------------------------------------------------------------------------------

*only works for one word per category, the categories with several words 
*(ie. Less or equal than 480 hours)	will be done separately
local var_list skills skills skills training training ///
		where_class where_class where_class implementer implementer ///
		length_intervention2 length_intervention2 lac lac
local value_list Soft Technical Both Classroom Both ///
		Private Public Public-Private Government Mulilateral_NGO ///
		More_480 Less_480 LAC Other
local n : word count `var_list'
local obs=1

forval i = 1/`n' {	

	local variable : word `i' of `var_list'
	local value : word `i' of `value_list'
	
	use `employment', clear
	count if `variable'=="`value'"
	local N_e=r(N)

	metan y se if `variable'=="`value'" ,random
	
	use  `plots_cat', clear
	replace ES = r(ES) in `obs'
	replace seES = r(seES) in `obs'
	replace ci_low = r(ci_low) in `obs'
	replace ci_upp = r(ci_upp) in `obs'
	replace value="`variable'" in `obs'
	replace category="`value'" in `obs'
	replace n_evaluations= `N_e' in `obs'
	
	local obs = `obs'+1
	set obs `obs'
	save `plots_cat', replace
	}
	
	use `plots_cat', clear
	drop in `obs'
	save "data\employment_summary_v2.dta", replace
	
	use "data\employment_summary_v2.dta", clear
	
	replace value="Type of skills" if value=="skills"
	replace value="Type of training" if value=="training"
	replace value="Provider of classroom training" if value=="where_class"
	replace value="Implementer of evaluation" if value=="implementer"
	replace value="Length of intervention" if value=="length_intervention2"
	replace value="Region of intervention" if value=="lac"
	replace category="More than 480 hrs" if category=="More_480"
	replace category="Less or equal than 480 hrs" if category=="Less_480"
	*replace category="On-the-job" if category=="on-the-job"
	
metan ES seES , by(value) label(namevar=category) lcols(category n_evaluations) ///
$graph_options title("Forestplot of employment by different categories", size(small)) ///
nooverall nosubgroup astext(50)  effect("Combine ES by category")  ///
subtitle("Change in probability of employment due to intervention", size(vsmall)) 
graph export "$plots_dir\employment_categories_summary_V2.emf", as(emf) replace

/*
******************************************************************************
*WAGES SECTION 
******************************************************************************	
use "data\employment_meta_categories_v2.dta", clear

************************************
** Macro definitions

// general options for meta-analysis, see documentation
global gen_options sortby(y) random label(namevar=evaluation_id)

// general options for forest plot, see documentation
global 	graph_options  boxsca(50) boxopt(mfcolor(%50) mlwidth(none)) ///
		nowt graphregion(color(white) lwidth(thin)) ///
		subtitle("Change in probability of employment due to intervention", size(vsmall))
//change format of graph output
global format_graph emf 
//base name for all files in folder
global plots_dir "plots\employment" 


************************************

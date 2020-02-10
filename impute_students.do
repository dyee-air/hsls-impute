// Set source data file
global DataFile "G:\IES Coursetaking\hsls_16_student_v1_0_DROPWT.dta"
// Temporary output file.  This is the file that will be imported to R
global OutFile "G:\IES Coursetaking\tmp_hsls_16_student_impute.dta"



// 1. Recode and export the variables of interest
use "${DataFile}", clear

global AnalysisVars "X1SEX X1RACE X1DUALLANG X1SES X1TXMTH X1MTHID X1MTHUTI X1MTHEFF X1MTHINT X1STUEDEXPCT X1PAREDEXPCT X1PAR1EMP X1FAMINCOME X1CONTROL X1LOCALE X3THIMATH"
global AllVars "STU_ID SCH_ID X1NCESID X2NCESID STRAT_ID psu $AnalysisVars W4W1STU W4W1STUP1"

foreach var of varlist $AnalysisVars {
	recode `var' (-9/-6=.)
}

preserve
// NOTE: R script will automatically attempt to impute ALL variables with ANY missing values.
//		 Do not save variables with missing values in the file below unless they should be imputed.
keep $AllVars
save "${OutFile}", replace
restore


// 2. In the R script 'impute_hsls.R', set the IN_FILE and OUT_FILE to the $OutFile defined above.
// 3. Run the R script.  It should overwrite the $OutFile.
// 4. Run the merge command below to import the imputed variables.

merge 1:1 STU_ID using "${OutFile}", keepusing(IM_*)

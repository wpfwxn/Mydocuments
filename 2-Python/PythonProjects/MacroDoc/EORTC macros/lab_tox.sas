**********************************************************************************************************;
** MACRO LAB_TOX                                                                                        **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** The purpose of %LAB_TOX macro is to handle and summarize laboratory data                             **                   
** (Hematology and Biochemistry data).                                                                  **
**                                                                                                      **
** This macro let the user handle laboratory data in several steps:                                     **
**                                                                                                      **
** - Automatic scanning of all LAB variables and check if the variable names are following the DM       **
** standard naming -> production of check listings.                                                     **
**                                                                                                      **
** - Imputation of the right units used according to the DM standard naming.                            **                                                                       
** For instance, for HBMML, because of the suffix, the unit will be automatically identified as mmol/L. **
**                                                                                                      **
** - Automatic conversion of all the LAB tests values and also LLN and ULN values into SI units         **
**                                                                                                      **
** - Imputation of LLN and ULN values in case of missing values.                                        **
**                                                                                                      **
** - Computation of CTC grade (version 3 or 4)                                                          **
**                                                                                                      **
** - Selection of worst CTC grade per exam and creation of output variables to contain the results.     **
**                                                                                                      **
** - Creation of global macro strings variables which are vectors containing the list of all worst CTC  **
** grade variables per period.                                                                          **
**                                                                                                      **
** - Production of listings to detect errors.                                                           **
**                                                                                                      **
** LAB_TOX macro is made up of 9 sub-macros but these macros are hidden for the SAS user.               **
**                                                                                                      **
** Creation date  : 03-02-2011                                                                          **
** Version date   : 22-03-2012                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.1                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)

    PERIOD            :  Name of the variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

	DATEXAM           :  Variable containing the date of assessment of the laboratory examination (Required parameter)

    CTC               :  Variable containing the version of the CTC grading (v. 3 or 4). (Required parameter)

	OUTFILE           :  name of an outfile to contain all the listings to detect errors (Required parameter). 
						 This name will contain the whole address with the file name with RTF extension. 
						 For instance, OUTFILE = c:\temp\errors.rtf

    VARLIST           :  A vector which contains the ‘ROOT’ names of LAB tests which do not belong to the standard LAB 
						 tests listed in the dictionanary (see appendix 1). (Optional parameter)
						 For instance, TSH variable could be added in the list for a specific analysis but does not 
                         belong to the Dictionanary

    KEEPLIST          :  A vector which lists all the variable names the user would like to add in the OUTPUT dataset. 
						 It could be identification variables or flag that could be useful for the analysis that the 
						 statistician would like to keep in the OUTPUT dataset 
					     (For instance, date of assessment, cycle number, treatment arm, flags for ITT, PP analysis, etc.)
                         (Optional parameter). Example: KEEPLIST=CYCLE TRT1 ITT
 	
    LAB               :  Variable containing the information if the sample has been analyzed in our center (coded 1)
                         or in another center (coded 2). (Optional parameter)


    OUTDATA           :  name of a SAS dataset which contains one observation per PATID in which
						 all new outcome variables will be merged (Optional parameter). 
                         Default: OUTDATA = PATIENT. 

    PRINTOUTLIER      :  n
						 if n>0, indicates to SAS if you want to print outliers in your outfile and the number of 
						 line of outliers per LAB TESTS you want to print
						 WARNING: These listings could be very huge.
						 0, if you don’t want to print outliers in your outfile 
						 (Optional parameter) Default: PRINTOUTLIER = 20 

    LISTOUTLIER       :  List the LAB TEST you want to be print in your outliers listing 
                         For instance, if LISTOUTLIER= ANC WBC, it means that the outliers listing will concern only 
						 ANC and WBC lab test. 
						 (Optional parameter) Default: LISTOUTLIER = ALL

NOTES

See documentation in J:\UNIT\Stat\SAS Macros Documentation\LABTOX

*****************************************************************************************************************;

%MACRO LAB_TOX (data=,varlist=,keeplist=,period=,lab=,datexam=,ctc=,outdata=,OUTFILE=,PRINTOUTLIER=20,LISTOUTLIER=);

	%if %length(&outdata) = 0 %then %let outdata=PATIENT;
	%if %length(&lab) = 0 %then %let lab=lab;

	%let keeplist=&keeplist &datexam.;

	* Importation of LAB dictionnary;
	** If the user works with a laptop import from C:\SAS. Otherwise from K:\**;
	%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\lab_dictionnary.sas7bdat))) > 0 %then %do;
	libname dico "K:\SAS\EORTC macros\LABTOX\Dictionnary";
	%end;
	
	%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\LABTOX\Dictionnary\lab_dictionnary.sas7bdat))) = 0 %then %do;
	libname dico "C:\SAS\EORTC macros\LABTOX\Dictionnary";
	%end;


	%RECOGNITION(data=&data,varlist=&varlist,keeplist=&keeplist, period=&period);

	%IF NOT(&CHECK =0 OR  ( %length(&varlist) ne 0  and &checklist. = 0)) %THEN %goto fin ;

	%INPUT_UNIT(data=INPUT_VAR);

	%MV_GEN(data=INPUT_VAR);

	%LABLIST_GEN(data=&data,keeplist=&keeplist,period=&period,lab=&lab,datexam=&datexam.);

	%CONVERT_UNIT(data=LABLIST, period=&period,datexam=&datexam.,varlist=&varlist.);

	%INPUT_NORMALS (data=LABLIST, period=&period,lab=&lab,datexam=&datexam);

	%COMPUTE_CTC_GRADE (data=LABLIST,period=&period,ctc=&ctc,outdata=&outdata,varlist=&varlist.);

	%MV_LIST_GEN(data=LABLIST,period=&period);

	%DETECT_PROBLEMS(data=LABLIST,keeplist=&keeplist,OUTFILE=&outfile,datexam=&datexam,period=&period,PRINTOUTLIER=&PRINTOUTLIER,LISTOUTLIER=&LISTOUTLIER,ctc=&ctc);

	%fin: ;


%MEND;



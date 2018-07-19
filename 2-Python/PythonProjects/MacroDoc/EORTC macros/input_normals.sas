***********************************************************************************************************;
**                                                                                                        **
** MACRO INPUT_NORMALS                                                                                    **
** written by Jérôme RAPION                                                                               **
**                                                                                                        **
** Imputation for LLN and ULN values                                                                      **
**                                                                                                        **
** The purpose of this macro is to impute normals values for missing values concerning LLN and ULN        **
** variables using the imputation method LOCF “Last Observation Carried forward” within the same patient. **
**                                                                                                        **
** for instance: if a patient has missing values for a specific lab exam at cy 1 and cy 3:                **
** Baseline Cycle 1  Cycle 2   Cycle3	                                                                  **
**     4	Missing	   5	    Missing	                                                                  **
**       ------->        ---------->                                                                      **
** In this case, the value observed at baseline is imputed at cycle 1, and the value at cycle 2 is        **
** imputed at cycle 3.                                                                                    **
**                                                                                                        **
** This method is used only if the blood sample “has been analyzed in your center” (LAB=1)                ** 
** The input dataset and the output dataset will be LABLIST dataset                                       **   
**                                                                                                        **
**                                                                                                        **
** Creation date  : 18-11-2010                                                                            **
** Version date   : 04-11-2011                                                                            **
** Software       : SAS version 9.2                                                                       **
** Original author: Jérôme RAPION                                                                         **
** Modified by    : Jérôme RAPION                                                                         **
** macro version : 1.0                                                                                    **
**                                                                                                        **
************************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)

    PERIOD            :  Variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

    DATEXAM           :  Variable containing the date of assessment of the laboratory examination (Required parameter)

    LAB               :  Variable containing the information if the sample has been analyzed in our center (coded 1)
                         or in another center (coded 2). (Optional parameter)

**********************************************************************************************************;

%MACRO INPUT_NORMALS (data=,period=,lab=,datexam=);

	%if %length(&lab) = 0 %then %let lab=lab;

	** IMPUTATION "LOCF" ** ;

	data &data.11;
	set &data;
	if &LAB. = 1;
	run;

	data &data.22;
	set &data;
	if &LAB. ne 1;
	run;

	%sort(data=&data.11,var=patid labtest &datexam.);

	data &data.11;
		set &data.11;
		by patid labtest &datexam.;
		ZLLN=LLN;
		ZULN=ULN;
		retain OLD_LLN OLD_ULN;
		if first. labtest then do;
		OLD_LLN=.;
		OLD_ULN=.;
		end;
		IF ZLLN NE . THEN OLD_LLN = ZLLN;
		ELSE ZLLN = OLD_LLN;
		IF ZULN NE . THEN OLD_ULN = ZULN;
		ELSE ZULN = OLD_ULN;
	run;

	data &data;
	set &data.11 &data.22;
	run;
	%sort(data=&data,var=patid LABTEST  &datexam);

	** LOCF for LLN and ULN only for LAB=1**;
	data &data;
		set &data;
		if &LAB. in (1 .) then do; ** if &LAB is coded 1 ("in your center" or missing);

			if ZLLN ne . and LLN=. then LLN=ZLLN;
			if ZULN ne . and ULN=. then ULN=ZULN;
		end;
	run;

	%sort(data=&data,var=LABTEST patid);

	data &data.;
		set &data.;
		drop ZLLN ZULN OLD_ULN OLD_LLN ;
	run;


	proc datasets nolist;
		delete  &data.11 &data.22  ;	 			
	run;
	quit;

	%sort(data=&data.,var=patid LABTEST &datexam.);

%MEND;


**********************************************************************************************************;
** MACRO LABLIST_GEN                                                                                    **
** written by Jérôme RAPION                                                                             **
**                                                                                                      **
** Creation of a transposed dataset called LABLIST                                                      **
**                                                                                                      **
** This macro is supposed to be run after RECOGNITION, INPUT_UNIT and MV_GEN macros.                    **
** The input dataset entered into DATA parameter is read by PATID.                                      **
**                                                                                                      **
** For each patient, one line per lab test is  created                                                  **
** This LABLIST dataset will contain also values for LLN, ULN, REL, SAE, AT and UNIT variables          **
** It will be used to compute CTC grades afterwards inside the suite of macros.                         **
** This LABLIST dataset could also be used afterwards by the users to produce listings.                 **
**                                                                                                      **
**                                                                                                      **
** Creation date  : 17-11-2010                                                                          **
** Version date   : 26-09-2011                                                                          **
** Software       : SAS version 9.2                                                                     **
** Original author: Jérôme RAPION                                                                       **
** Modified by    : Jérôme RAPION                                                                       **
** macro version : 1.0                                                                                  **
**********************************************************************************************************

PARAMETERS

    DATA              :  Name of the input dataset containing Hematology and Biochemistry data (Required parameter)

    KEEPLIST          :  A vector which lists all the variable names the user would like to add in the OUTPUT dataset. 
						 It could be identification variables or flag that could be useful for the analysis that the 
						 statistician would like to keep in the OUTPUT dataset 
					     (For instance, date of assessment, cycle number, treatment arm, flags for ITT, PP analysis, etc.)
                         (Optional parameter). Example: KEEPLIST=CYCLE TRT1 ITT
 	
    PERIOD            :  Variable containing the period. The values of this variable will for instance
                         distinguish several period, such as ’Baseline’, ‘On study’ etc. (Required parameter). 

    LAB               :  Variable containing the information if the sample has been analyzed in our center (coded 1)
                         or in another center (coded 2). (Optional parameter)

    DATEXAM           :  Variable containing the date of assessment of the laboratory examination (Required parameter)

**********************************************************************************************************;


%MACRO LABLIST_GEN (data=,keeplist=,period=,lab=,datexam=);

	%if %length(&lab) = 0 %then %let lab=lab;
	%let keeplist=&keeplist &datexam.;

	* Macro used to seperate words in several macro variables (&KEEPLIST) ;
	* from a SUITE of words (in SEP) seperated by a seperator SEP ;
	* and to count the number of words (in N) - JRA, Oct2006*;

	%MACRO RCHV(SUITE=,COMPTE=,SEP= ,N=);
		%LOCAL NUMERO MOT;
		%LET NUMERO=1;
		%LET MOT=%NRBQUOTE(%SCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%DO %WHILE(&MOT ^=);
		%GLOBAL PV_&NUMERO &COMPTE&NUMERO;
		%LET &COMPTE&NUMERO=%TRIM(&MOT);
		%LET PV_&NUMERO=%SUBSTR(&MOT,1,1);
		%LET NUMERO=%EVAL(&NUMERO+1);
		%LET MOT=%NRBQUOTE(%QSCAN(%STR(&SUITE),&NUMERO,%STR(&SEP)));

		%PUT MOT : &MOT;


		%END;
		%GLOBAL &N;
		%LET &N=%EVAL(&NUMERO-1);
	%MEND RCHV;

%sort(data=&data,var=patid &period. &keeplist);

data LABLIST;
	set &DATA.;
	by patid;
	length LABTEST $10. UNIT $10.;
	_na=.;
	%do i=1 %to &nb_test;

	* JRA, 27SEP2011, keep value of lab test even if it is missing;
	      LABVALUE=&&var&i.;
	      LABTEST=upcase("&&root&i.");
	      UNIT=upcase("&&unit&i.");
	      if &&lln&i. ne _NA then LLN=&&lln&i.;
		  else LLN=.;
	      if &&uln&i. ne _NA then ULN=&&uln&i.;
		  else ULN=.;
		  if &&rvar&i. ne _NA then REL=&&rvar&i.;
		  else REL=.;
		  if &&svar&i. ne _NA then SAE=&&svar&i.;
		  else SAE=.;
		  if &&avar&i. ne _NA then ACT=&&avar&i.;
		  else ACT=.;
	      output;
	%end;

keep patid labvalue labtest unit lln uln rel sae act &keeplist &period. ;
run;


* JRA, 27SEP2011, remove all records with all numerical values which are missing;
data LABLIST;
set LABLIST;
		VAL=sum(LABVALUE,LLN,ULN,REL,SAE,ACT);
		if VAL >.Z;
drop VAL;
run;

data _lab;
set &data;
keep patid &period &keeplist &lab;
run;

%sort(data=LABLIST,var=patid &period. &keeplist. labtest);
%sort(data=_lab,var=patid &period. &keeplist.);

data LABLIST _datexam_missing;
	merge LABLIST(in=a) _lab;
	by patid &period. &keeplist.;
	if a;

	* JRA, 30MAR2011, remove all records with a date of lab exam missing or unknown;
	if &datexam > .Z then output LABLIST;
	* JRA, 19MAR2012, isolate all records with a date of lab exam missing or unknown in a separate dataset;
	else output _datexam_missing;

run;


%sort(data=LABLIST,var=patid labtest &period. &keeplist. );

	proc datasets nolist;
		delete   
		 _LAB 
		  ;
	run;
	quit;

%MEND;


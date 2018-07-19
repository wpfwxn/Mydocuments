/************************************************************************************
*
* SummaryB3.SAS
* *************
*
* Part of a suite of programs to perform a meta analysis on binary endpoint data.
* Input data can be in the form of either individual patient data or summary
* (literature) data - or both.
*
* This macro computes Ns Os O-E var O-E (according to PETOs method with Mantel-Haenszel
* type variance for each study) and puts everything into a summary dataset 
*
* MACRO SUMMARYB3 IS THE FIRST MODULE OF THE NEW MA SOFTWARE
* TO BE USED FOR BINARY ENDPOINT instead of SUMMARY (used for survival endpoints)
*
* Creation date  : 09/2006
* Software       : SAS version 9.1
* Original author: Jérôme RAPION 
* Based on summaryB2 macro written by Laurence Colette (01/2002)
*
************************************************************************************
*
* PARAMETERS
*
*    Data              :  Name of the original data file
*    Outname           :  Name for the output data set (SAS name, default is work.summary) 
*    RespVar           :  Response variable 
*    RespVal           :  Indicator of response (default=1)
*    TestVar           :  Test variable (treatment)
*    Trial             :  Trial indicator 
*    Group             :  Group indicator (in case of subgroup analysis)
*    Total             :  Indicator of the grouping for the subtotals
*    Litt              :  Name of an eventual data set containing num total (same names as in DATA),
*                         npat1-2, ndeath1-2 for the litterature based studies, see Notes
*    Blind             :  Name of a binary indicator with 1=blinded trial ;
*
*
*************************************************************************************
*
* HISTORY
*
*   Based on:
*     SummaryB2.SAS by Laurence COLLETTE (01/2002)
*     LC 20/03/2001: change LCL&conf2 to LCL_&conf2*
*     LC 28/03/2001: creation of the macro 
*     LC 08/01/2002: modification to allow use of literature only 
*     KM 31/05/2002: Modified to ensure unknown (.U) values are handled correctly.
*     June 2003: Updated documentation. 
*                Adapted so that, for graphs based purely on literature data, the 
*                treatment effect squares are solid. (KM)
*     JRA 20/09/2006: Replacement of summaryB2 macro by summaryB3 macro.
*                 Modification using ODS Output results from PROC FREQ
*                 for estimation per trial of N1,N2,O1, O2, O-E, Var(O-E),OR,IC95% and IC99%
*                 Replacement of VAR Parameter by TestVar parameter.
*                 Replacement of STRAT and SUBGROUP Parameter by 3 parameters : trial, group 
*                 and total
*                 Some errors have been discovered in the way of calculation of summary (N1 N2, 
*                 O1 O2) with a GROUP parameter in  SUMMARYB2 macro: now, GROUP parameter 
*                 is inserted in BY statement of PROC FREQ
*     JRA 17/10/2006: Correction: modification to keep labels and formats of GROUP or TOTAL 
*                 variables in the output dataset
*     JRA 02/03/2007: Correction of last MERGE between &LIB..&NAME and tit dataset with a BY.
*
*     JRA 13/11/2007: Add of a condition to avoid missing values to be taken in the results.
*                 Correction to avoid dupplicates in result file
*
*     JRA 18/04/2008: Add of some variables in the &Outname dataset (default = SUMMARY)
*                 for the display in FOREST2 macro, so as to keep FORMATS and LABELS of KEY VARIABLES
*				  WE NEED TO KEEP IN the OUTPUT DATASET (BY DEFAULT SUMMARY dataset) :
*					 - the name of the variable contained into &TRIAL ---> put in NTRIAL
*					 - the format of the variable contained into &TRIAL --> put in f&TRIAL
*					 - the variable &TESTVAR;
*     JRA 15/05/2008: add the possibility of a full litterature-based meta-analysis without individual data only for binary data (parameter DATA empty);
*
*     JRA 19/02/2009: Add the possibility to have several variables in GROUP parameter
*     In this case if GROUP parameter has several variables, TOTAL should not be used.
*     If several variables are entered in GROUP parameter with the use of TOTAL parameter in the same time
*     an ERROR message will appear and stop the execution of the macro;
*
*     JRA May 2010: In case of a full litterature-based meta-analysis then put _lit=0;
*     In this case, in the FOREST plot, the squares will be filled in and not empty.
*
*  NOTES
*
*  case of the use of LITT parameter:
*  - if LITT parameter is used with DATA parameter (mix of individual data and litterature based data meta-analysis), in this case, 
*  the litterature DATASET should contain  npat1-2 and ndeath1-2 variables
*  - if LITT parameter is used without DATA parameter (only full litterature-based meta-analysis), in this case, 
*  the litterature DATASET should contain  npat1-2 and ndeath1-2 and TREAT variables. The TREAT variable doesn't need to have any value but just
*  the correct label and format of this variable, so as FORESTB2 could work normally at the following step.
*************************************************************************************/

%MACRO summaryB3(DATA, outname, respvar, respVAL, TESTVAR, TRIAL, GROUP, TOTAL, litt, blind);

%global TESTV ;

%let TESTV=&TESTVAR;

** JRA, 8May2008, add the possibility of a full litterature-based meta-analysis;
%if %length(&data.)=0 %then %let data=NO;
%put DATA &DATA;

%if %length(&conf1.)=0 %then %let alpha1=95;
%if %length(&conf2.)=0 %then %let alpha2=99;
%if %length(&conf1.)>0 %then %let alpha1=&conf1.;
%if %length(&conf2.)>0 %then %let alpha2=&conf2.;

%if %length(%scan(&outname,2))^=0
                      %then %do ;
                      %let lib=%scan(&outname, 1);
                      %let name=%scan(&outname,2);
                      %end ;
%else %if %length(%scan(&outname,1))^=0
                      %then %do ;
                      %let name=%scan(&outname,1) ;
                      %let lib=WORK ;
                      %end ;
%else %do ;
      %let lib=WORK ;
      %let name=SUMMARY ;
      %end ;

%local TRIAL GROUP TOTAL;

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

** JRA, February 2009, decompositon of vector &GROUP with each element created into &&A&i variable;
** Number of elements of vector &group put into &nb variable;
%GLOBAL NB;
%LET NB=0;
%if %length(&group.)>0 %then %RCHV(SUITE=&group,COMPTE=A,SEP=" ",N=NB);

%if (%LENGTH(&total.)>0) and (&NB >1)  %then %do;

	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE A VARIABLE IN TOTAL PARAMETER ;
	%PUT AND IN THE SAME TIME SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT YOU SHOULD HAVE ONLY ONE VARIABLE IN GROUP PARAMETER IF YOU WANT TO USE TOTAL PARAMETER;
	%PUT OR NOTHING IN TOTAL PARAMETER IF YOU WANT TO HAVE SEVERAL VARIABLES IN GROUP PARAMETER;
	%PUT;

%goto fin ;

%end;

%if (%LENGTH(&trial.)=0)  %then %do;
	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE NOTHING IN TRIAL PARAMETER ;
	%PUT YOU SHOULD ENTER A VARIABLE IN TRIAL PARAMETER;
	%goto fin ;

%end;


* DELETE ANY EXISTING DATA SET &LIB.&NAME ;
proc datasets library=&lib nolist;
 delete &name ;
 run;
quit ;

* INITIALIZATION OF RESPVAL ;
%if %length(&respVAL.)=0 %then %let respVAL=1;

%IF &data ne NO %THEN %DO;

	data indata;
		set &data;
		format &testvar.;
	run;

	** NB OF LEVELS OF TESTVAR **;

	proc sort data=indata out=Nlevel(keep=&testvar. where=(&testvar. ne .)) nodupkey;
		by &testvar.;
	run;

	data _null_;
		set Nlevel end=final;
		n+1;
		call symput('VAR'||compress(N),compress(&TestVar.));
		if final then call symput('N',compress(N));
	run; 

	data indata;
		set &data;
	run;

	%sort(data=indata,var=&TRIAL. &GROUP. &TOTAL.);

	data blind;
		delete;
	run;

	%IF (%length(&BLIND.)>0) %THEN %DO;

		data blind;
			set indata;
			keep &TRIAL. &BLIND;
		run;

		proc sort data=blind nodupkey;
			by &TRIAL.;
		run;

	%END;

	****************************************************************;
	** CASE WHERE GROUP PARAMETER CONTAINS MORE THAN ONE VARIABLE **;
	****************************************************************;
	%IF &NB>1 %THEN %DO;

	   %DO I=1 %TO &NB ;** JRA, January 2009, loop on each element &&A&i of vector &GROUP;

	   proc sort data=indata;
		by &TRIAL. &&a&i. 
		;
		run;

		ods listing close;
		ods output CrossTabFreqs=__CrossTabFreqs;
		ods output ChisQ=__ChisQ;
		proc freq data=indata ;
		table &respvar.*&testvar./OUTEXPECT out=t all EXPECTED cl;
		by &TRIAL. &&a&i. ;
		run;
		ods output close;
		ods listing;

		%sort(data=__CrossTabFreqs,var=&trial.);

		data __ndeath;
			set __CrossTabFreqs;
			if &respvar.=&respval. and &testvar. ne .;
		run;

		data __npat;
			set __CrossTabFreqs;
			if &respvar.=. and &testvar. ne .;
		run;

		proc transpose data=__ndeath out=__tndeath(drop=_label_ _name_) prefix=ndeath;
		var Frequency;
		by &trial. &&a&i. ;
		run;

		proc transpose data=__npat out=__tnpat(drop=_label_ _name_) prefix=npat;
		var Frequency;
		by &trial. &&a&i. ;
		run;

		data __O_E;
			set __CrossTabFreqs;
			if &respvar.=&respval. and &Testvar.=&&VAR&n.;
			O_E=Frequency-EXPECTED;
			keep &trial. &&a&i. O_E;
		run;

		%sort(data=__tnpat,var=&trial. &&a&i.);
		%sort(data=__tndeath,var=&trial. &&a&i.);
		%sort(data=__O_E,var=&trial. &&a&i.);

		data __result ;
			merge __tndeath __tnpat __O_E;
			by &trial. &&a&i.;

			%IF %length(&&a&i.)>0 %THEN if &&a&i. >.Z;;
			%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;

		     O1=ndeath1;
		     O2=ndeath2;
		     N1=npat1;
		     N2=npat2;
		     O=O1+O2 ;
		     N=N1+N2 ;
		     if N ne 1 then varO_E=(N1*N2*(N-O)*O)/((N**2)*(N-1));
		     if N = 1 then varO_E=0;
			 drop O O1 O2 N N1 N2;
		run; 

		%IF (%length(&GROUP.)>0) %THEN %DO;
		 %sort(data=__result,var=&&a&i.);

		   ods listing close;
		    ods output summary=__summary;
			proc means data=__result sum;
			var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
			by &&a&i.;
			run;
		    ods output close;
		    ods listing;

			data __result&I;
				set __summary;
				drop Vname:;
				rename 
				ndeath1_sum=ndeath1
				ndeath2_sum=ndeath2
				npat1_sum=npat1
				npat2_sum=npat2
				O_E_sum=O_E
				VarO_E_sum=VarO_E;
				attrib ndeath1_sum 	ndeath2_sum npat1_sum npat2_sum O_E_sum VarO_E_sum 
		        label=" "
				;
				flag=&I.;
			run;

		%END;

	%END; *END DO I=1 TO &NB ;

	data __result;
		set %DO I=1 %TO &NB ; __result&i %END;;
	run;

	************;
	*** FOR ALL : computation of GRAND TOTAL IN CASE OF SEVERAL VARIABLES IN GROUP PARAMETER;

	ods listing close;
	ods output CrossTabFreqs=__CrossTabFreqsALL;
	ods output ChisQ=__ChisQALL;
	proc freq data=indata ;
	table &respvar.*&testvar./OUTEXPECT out=t all EXPECTED cl;
	by &TRIAL. ;
	run;
	ods output close;
	ods listing;

	%sort(data=__CrossTabFreqsALL,var=&trial.);

		data __ndeathALL;
			set __CrossTabFreqsALL;
			if &respvar.=&respval. and &testvar. ne .;
		run;

		data __npatALL;
			set __CrossTabFreqsALL;
			if &respvar.=. and &testvar. ne .;
		run;

		proc transpose data=__ndeathALL out=__tndeathALL(drop=_label_ _name_) prefix=ndeath;
		var Frequency;
		by &trial.  ;
		run;

		proc transpose data=__npatALL out=__tnpatALL(drop=_label_ _name_) prefix=npat;
		var Frequency;
		by &trial.  ;
		run;

		data __O_EALL;
			set __CrossTabFreqsALL;
			if &respvar.=&respval. and &Testvar.=&&VAR&n.;
			O_E=Frequency-EXPECTED;
			keep &trial. O_E &testvar.;
		run;

		%sort(data=__tnpatALL,var=&trial.);
		%sort(data=__tndeathALL,var=&trial.);
		%sort(data=__O_EALL,var=&trial. );

		data __resultALL ;
			merge __tndeathALL __tnpatALL __O_EALL;
			by &trial.;

			%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;

		     O1=ndeath1;
		     O2=ndeath2;
		     N1=npat1;
		     N2=npat2;
		     O=O1+O2 ;
		     N=N1+N2 ;
		     if N ne 1 then varO_E=(N1*N2*(N-O)*O)/((N**2)*(N-1));
		     if N = 1 then varO_E=0;
			 drop O O1 O2 N N1 N2;
		run; 

		ods listing close;
		ods output summary=__summaryALL;
		proc means data=__resultALL sum;
		var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
		run;
		ods output close;
		ods listing;

		data __resultALL;
			set __summaryALL;
			drop Vname:;
			rename 
			ndeath1_sum=ndeath1
			ndeath2_sum=ndeath2
			npat1_sum=npat1
			npat2_sum=npat2
			O_E_sum=O_E
			VarO_E_sum=VarO_E;
			attrib ndeath1_sum 	ndeath2_sum npat1_sum npat2_sum O_E_sum VarO_E_sum 
		    label=" "
			;
			flag=99;
		run;

		data __result;
			set __result __resultALL;
		run;

	*** end FOR ALL ;
	************;

	%END; *IF &NB>1 THEN  DO;


	****************************************************************;
	** CASE WHERE GROUP PARAMETER CONTAINS ONE VARIABLE OR NOTHING**;
	****************************************************************;
	%IF &NB<=1 %THEN  %DO;


	ods listing close;
	ods output CrossTabFreqs=__CrossTabFreqs;
	ods output ChisQ=__ChisQ;
	proc freq data=indata ;
	table &respvar.*&testvar./OUTEXPECT out=_t all EXPECTED cl;
	by &TRIAL. &GROUP. &TOTAL.;
	run;
	ods output close;
	ods listing;

	%sort(data=__CrossTabFreqs,var=&trial.);

	data __ndeath;
		set __CrossTabFreqs;
		if &respvar.=&respval. and &testvar. ne .;
	run;

	data __npat;
		set __CrossTabFreqs;
		if &respvar.=. and &testvar. ne .;
	run;

	proc transpose data=__ndeath out=__tndeath(drop=_label_ _name_) prefix=ndeath;
	var Frequency;
	by &trial. &group. &total.;
	run;

	proc transpose data=__npat out=__tnpat(drop=_label_ _name_) prefix=npat;
	var Frequency;
	by &trial. &group. &total.;
	run;

	data __O_E;
		set __CrossTabFreqs;
		if &respvar.=&respval. and &Testvar.=&&VAR&n.;
		O_E=Frequency-EXPECTED;
		keep &trial. &group. &total. O_E;
	run;

	%sort(data=__tnpat,var=&trial. &group. &total.);
	%sort(data=__tndeath,var=&trial. &group. &total.);
	%sort(data=__O_E,var=&trial. &group. &total.);

	data __result ;
		merge __tndeath __tnpat __O_E;
		by &trial. &group. &total.;

		%IF %length(&TOTAL.)>0 %THEN if &total. >.Z;;*JRA, 13Nov2007, to delete missing values;
		%IF %length(&GROUP.)>0 %THEN if &GROUP. >.Z;;
		%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;

	     O1=ndeath1;
	     O2=ndeath2;
	     N1=npat1;
	     N2=npat2;
	     O=O1+O2 ;
	     N=N1+N2 ;
	     if N ne 1 then varO_E=(N1*N2*(N-O)*O)/((N**2)*(N-1));
	     if N = 1 then varO_E=0;
		 drop O O1 O2 N N1 N2;
	run; 

	%IF (%length(&GROUP.)>0) %THEN %DO;
	 %sort(data=__result,var=&GROUP. &TOTAL.);

	   ods listing close;
	    ods output summary=__summary;
		proc means data=__result sum;
		var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
		by &GROUP. &TOTAL.;
		run;
	    ods output close;
	    ods listing;

		data __result;
			set __summary;
			drop Vname:;
			rename 
			ndeath1_sum=ndeath1
			ndeath2_sum=ndeath2
			npat1_sum=npat1
			npat2_sum=npat2
			O_E_sum=O_E
			VarO_E_sum=VarO_E;
			attrib ndeath1_sum 	ndeath2_sum npat1_sum npat2_sum O_E_sum VarO_E_sum 
	        label=" "
			;
		run;

	%END;

	%END;*IF &NB<=1 THEN  DO;


%END; *IF &data ne NO THEN DO;

data litt;delete;run;

%IF (%length(&litt.)>0) %THEN %DO;
data litt;
set &litt.;
keep &trial. &group. &total.  %IF &DATA= NO %THEN &testvar.; npat: ndeath:;
run;
%END;

%IF &DATA=NO %THEN %DO; 

	** JRA, 8May2008, add the possibility of a full litterature-based meta-analysis;
		data __result;
			delete;
		run;

	** JRA, 14JUN2010, Handling of TRIAL format;
	data _null_;
		set litt;
		myformat=trim(left(vformatn(&TRIAL.)));
		call symput('myformat',myformat);
	run;

	%put myformat &myformat.;


%END;  *IF &data = NO THEN DO;


data &LIB..&NAME.;
	set __result(in=a) 
	%IF (%length(&litt.)>0) %THEN litt (in=b);
	;

	%IF &NB <=1 %THEN %DO;

		WHERE
		%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)=0) %THEN &GROUP. NE . ;
		%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)>0) %THEN &GROUP. NE . AND &TOTAL. NE .;
		;

		%IF (%length(&GROUP.)>0) %THEN _Num=&GROUP.;;

		** JRA, 14JUN2010, Handling of TRIAL format and initialization of _Num variable;
		%IF (%length(&GROUP.)=0) AND &DATA=NO %THEN %do;
			%IF &myformat.= $ %then _Num=_n_;;
			%IF &myformat. ne $ %THEN _Num=&TRIAL.;;
		%END;
		%IF (%length(&GROUP.)=0) AND &DATA ne NO %THEN _Num=&TRIAL.;;


	%END;

	%IF &NB > 1 %THEN %DO;

		%DO J=1 %TO &NB;
			IF &&a&j. NE . THEN _Num=&&a&j.;
			if flag=99 then _Num=flag;
		%END;

	%END;

	 if a then _lit=0;
	 else _lit=1;

	%IF (%length(&litt.)>0) %THEN %DO;
		** JRA, 8May2008, add the possibility of a full litterature-based meta-analysis;
		 if b then do;

		     O1=ndeath1;
		     O2=ndeath2;
		     N1=npat1;
		     N2=npat2;
		     O=O1+O2 ;
		     N=N1+N2 ;
		     if N ne 1 then varO_E=(N1*N2*(N-O)*O)/((N**2)*(N-1));
		     if N = 1 then varO_E=0;
	    	 O_E=O2-(O*N2/N) ;
	   
			 drop O O1 O2 N N1 N2;

		 end;
	 %END;

	 if npat1=. then npat1=0;
	 if npat2=. then npat2=0;
	 if ndeath1=. then ndeath1=0;
	 if ndeath2=. then ndeath2=0;
	 if varO_E=. then varO_E=0;

	 * OR calculation and CI *;
     OR=. ;
     LCL&alpha1=. ;
     LCL_&alpha2=. ;
     UCL&alpha1=. ;
     UCL_&alpha2=. ;
     if (varO_E>0) then do ;
     OR=exp(O_E/varO_E);
     LCL&alpha1=exp(O_E/varO_E-(probit(1-(100-&alpha1)/200)/sqrt(varO_E)));
     UCL&alpha1=exp(O_E/varO_E+(probit(1-(100-&alpha1)/200)/sqrt(varO_E)));
     LCL_&alpha2=exp(O_E/varO_E-(probit(1-(100-&alpha2)/200)/sqrt(varO_E)));
     UCL_&alpha2=exp(O_E/varO_E+(probit(1-(100-&alpha2)/200)/sqrt(varO_E)));
	 end;

	%IF &DATA=NO %THEN _lit=0;; **JRA, May2010, if it is a full litterature-based meta-analysis, then put _lit=0;

run;

****************************************************************;
** CASE WHERE GROUP PARAMETER CONTAINS MORE THAN ONE VARIABLE **;
****************************************************************;
%IF &NB>1 %THEN %DO;

	data _data;
		set &data;

		if &TESTVAR. >.Z  and &TRIAL. >.Z;   
			%do i = 1 %to &nb;
				if &&a&i. >.Z;
			%end;

	run;

	data _trtfmt;
		set _data(obs=1);

			length f&TRIAL. NTRIAL   $20. ;

		f&TRIAL.=trim(left(vformatn(&TRIAL. )));
		NTRIAL="&TRIAL.";

		if f&TRIAL.="F" or f&TRIAL.="BEST" then f&TRIAL.="8";

		keep NTRIAL f&TRIAL.  &TESTVAR. &TRIAL.
		&a1.;
		;
	run; 

	*** Keep of the title;

		data mytit;
			set &data;
			%do i = 1 %to &nb;
				if &&a&i. >.Z;
			%end;
		run;

		data tit;
			set mytit(obs=1);
			title=vlabel(&RespVar.);
			name=vname(&RespVar.);
			if title=name then title="";
			keep title &a1;
		;
		run;


		%SORT(data=&LIB..&NAME.,var=&a1.);
		%SORT(data=tit,var=&a1.);
		%SORT(data=_Trtfmt,var=&&a1.);


	data &LIB..&NAME.;
		merge &LIB..&NAME.(in=a) tit 
			_Trtfmt; **JRA, APR 2008: add of _TRTFMT info in the output dataset;
		by
		&a1;
		;
		if a;
	run;

%END; *IF &NB>1 THEN  DO;



****************************************************************;
** CASE WHERE GROUP PARAMETER CONTAINS ONE VARIABLE OR NOTHING**;
****************************************************************;
%IF &NB <=1 %THEN %DO;

	proc contents data=&LIB..&NAME. out=t(keep=name) noprint;
	run;

	data t;
		set t;
		name=UPCASE(name);
		if name=UPCASE("&TRIAL.") then ind1=1;
		if name=UPCASE("&GROUP.") then ind2=1;
		if name=UPCASE("&TOTAL.") then ind3=1;
	run;

	ods listing close;
	ods output summary=__t(drop=vname:);
		proc means data=t sum;
		var ind1 ind2 ind3;
		run;
	    ods output close;
	ods listing;

	data _null_;
		set __t;
		call symput("sum1",ind1_Sum); 
		call symput("sum2",ind2_Sum); 
		call symput("sum3",ind3_Sum); 
	run;

	%IF &SUM1.=1 and &SUM2. ne 1 AND &SUM3. ne 1 and %length(&BLIND.)>0 %THEN %DO;

		%sort(data=&LIB..&NAME.,var=&TRIAL. &GROUP. &TOTAL.);
		%sort(data=blind,var=&TRIAL.);

		data &LIB..&NAME.;
			merge &LIB..&NAME. blind;
			by &TRIAL.;
		run;
	%END;

	%IF &SUM1.=1 and &SUM2.=1 AND &SUM3.=1         %THEN %let case=1;
	%IF &SUM1.=1 and &SUM2.=1 AND &SUM3. ne 1      %THEN %let case=2;
	%IF &SUM1.=1 and &SUM2. ne 1 AND &SUM3. ne 1   %THEN %let case=3;
	%IF &SUM1. ne 1 and &SUM2. = 1 AND &SUM3. = 1  %THEN %let case=4;
	%IF &SUM1. = 1 and &SUM2. ne 1 AND &SUM3. = 1  %THEN %let case=5;
	%IF &SUM1. ne 1 and &SUM2. = 1 AND &SUM3. ne 1 %THEN %let case=6;

	**JRA, APR 2008 - FOR THE DISPLAY in FOREST2 macro, so as to keep FORMATS and LABELS of KEY VARIABLES,
	WE NEED TO KEEP IN the OUTPUT DATASET (BY DEFAULT SUMMARY dataset) :
	 - the name of the variable contained into &TRIAL ---> put in NTRIAL
	 - the format of the variable contained into &TRIAL --> put in f&TRIAL
	 - the variable &TESTVAR;


	** JRA, 8May2008, add the possibility of a full litterature-based meta-analysis;
	%IF &DATA NE NO %THEN %DO;
		%if &case=6 or &case=4 or &case=5 %then %do;

			data _data;
			set &data;
			%if &case=4 %then if &TESTVAR. >.Z  and &TRIAL. >.Z  and &GROUP. >.Z  and &TOTAL. >.Z ;;
			%if &case=5 %then if &TESTVAR. >.Z  and &TRIAL. >.Z  and &TOTAL. >.Z  ;;
			%if &case=6 %then if &TESTVAR. >.Z  and &TRIAL. >.Z  and &GROUP. >.Z  ;;
			run;


		**JRA, 18-09-2008,MODIFY KEY MERGE IN CASE OF GROUP AND TOTAL OPTION;
		data _trtfmt;
			set _data(obs=1);

				length f&TRIAL. NTRIAL   $20. ;

			f&TRIAL.=trim(left(vformatn(&TRIAL. )));
			NTRIAL="&TRIAL.";

			if f&TRIAL.="F" or f&TRIAL.="BEST" then f&TRIAL.="8";

			keep NTRIAL f&TRIAL.  &TESTVAR. &TRIAL.
			%if &case = 4 %then  &GROUP.;
			%if &case = 5 %then  &TOTAL.;
			%if &case = 6 %then  &GROUP.;
			;
			run; 

		%end;
		%else %do;
			data _data;
			set &data;
			%if &case=1 %then if &TESTVAR. >.Z  and &TRIAL. >.Z  and &GROUP. >.Z  and &TOTAL. >.Z ;;
			%if &case=2 %then if &TESTVAR. >.Z  and &GROUP. >.Z ;;
			%if &case=3 %then if &TESTVAR. >.Z  and &TRIAL. >.Z   ;;
			run;

			data _trtfmt;
			set _data(obs=1);

			length  NTRIAL f&TRIAL. $20.;

			%if &case =1 or  &case =3  %then	%do;
				f&TRIAL.=trim(left(vformatn(&TRIAL. )));
				NTRIAL="&TRIAL.";
			if f&TRIAL.="F" or f&TRIAL.="BEST" then f&TRIAL.="8";
			%end;

			keep &TESTVAR.
			%if &case = 1 %then  f&TRIAL. NTRIAL  &TRIAL.  &TOTAL. &GROUP.;
			%if &case = 2 %then &GROUP.;
			%if &case = 3 %then f&TRIAL. NTRIAL &TRIAL.;
			;
			run; 

		%end;

	*** Keep of the title;

	%if &case=6 or &case=4 %then %do;
	data mytit;
		set &data(where=(&group > .Z));
	run;
	%end;
	%else %if &case=5 %then %do;
	data mytit;
		set &data(where=(&total > .Z));
	run;
	%end;
	%else %do;
	data mytit;
		set &data;
	run;
	%end;

	data tit;
		set mytit(obs=1);
		title=vlabel(&RespVar.);
		name=vname(&RespVar.);
		if title=name then title="";

		%if &case=1 %then keep title &trial &group &total;
		%if &case=2 %then keep title &group;
		%if &case=3 %then keep title &trial;
		%if &case=4 %then keep title &group;
		%if &case=5 %then keep title &total &trial;
		%if &case=6 %then keep title &group;
	;
	run;


	%IF &case=1 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var=&TRIAL. &GROUP. &TOTAL.);
		%SORT(data=tit,var=&TRIAL. &GROUP. &TOTAL.);
		%SORT(data=_Trtfmt,var=&TRIAL. &GROUP. &TOTAL.);
	%END;

	%IF &case=2 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var= &GROUP.);
		%SORT(data=tit,var= &GROUP.);
		%SORT(data=_Trtfmt,var= &GROUP.);
	%END;

	%IF &case=3 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var=&TRIAL. );
		%SORT(data=tit,var=&TRIAL. );
		%SORT(data=_Trtfmt,var=&TRIAL. );
	%END;

	%IF &case=4 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var= &GROUP. );
		%SORT(data=tit,var=&GROUP.  );
		%SORT(data=_Trtfmt,var= &GROUP.  );
	%END;

	%IF &case=5 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var=&TOTAL. &TRIAL. );
		%SORT(data=tit,var=&TOTAL. &TRIAL. );
		%SORT(data=_Trtfmt,var=&TOTAL. &TRIAL. );
	%END;

	%IF &case=6 %THEN %DO;
		%SORT(data=&LIB..&NAME.,var=&GROUP.);
		%SORT(data=tit,var=&GROUP.);
		%SORT(data=_Trtfmt,var=&GROUP.);
	%END;


	data &LIB..&NAME.;
		merge &LIB..&NAME.(in=a) tit 
		_Trtfmt; **JRA, APR 2008: add of _TRTFMT info in the output dataset;
		by
		%if &case=1 %then &trial &group &total;
		%if &case=2 %then &group;
		%if &case=3 %then &trial;
		%if &case=4 %then &group;
		%if &case=5 %then &total &trial;
		%if &case=6 %then &group;
		;
	run;


	%END;*IF &DATA NE NO THEN DO;

	%IF &DATA = NO %THEN %DO;

		data &LIB..&NAME.;
		set  &LIB..&NAME.;
					f&TRIAL.=trim(left(vformatn(&TRIAL. )));
					NTRIAL="&TRIAL.";
				if f&TRIAL.="F" or f&TRIAL.="BEST" then f&TRIAL.="8";
		run;

	%END;

%END; *IF &NB <=1 THEN DO;

* Clear temporary data sets;
***************************;

%IF &NB <=1 %THEN %DO;
	proc datasets library=WORK nolist ;
	   delete _t __tnpat __tndeath __npat __ndeath t __t _Trtfmt: mytit tit __result __O_E nlevel indata blind _data
		__crosstabfreqs __chisq __summary litt;

	   run;
	 quit;


%END;

%IF &NB >1 %THEN %DO;
	proc datasets library=WORK nolist ;
	   delete t mytit tit litt nlevel indata blind _data _Trtfmt: __result: __O_E:
       __crosstabfreqs: __chisq: __summary:
       __tnpat: __tndeath: __npat: __ndeath: ;
	   run;
	 quit;


%END;


%fin: ;


%mend summaryB3;



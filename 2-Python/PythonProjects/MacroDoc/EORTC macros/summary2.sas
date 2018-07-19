/************************************************************************************

Summary2.SAS

* MACRO SUMMARY2 IS THE FIRST MODULE OF THE NEW MA SOFTWARE ;
* It was created on the basis of the previous software MINISURV ;
* by Guido Hoctin-Boes and Luc Bijnens ;

**************
Creation date  : 09/2006
Software       : SAS version 9.1
Original author: Jérôme RAPION 
Based on summary macro written by Laurence Colette (09/96)

version n°2 of summary macro ;

************************************************************************************

PARAMETERS

    Data              :  Name of the original data file
    Outname           :  Name for the output data set (SAS name, default is work.summary) 
    TimeVar           :  Time variable (in days)
    CensVar           :  Censoring variable
    CensVal           :  Value of CensVar for a censored observation [default=l]
    TestVar           :  Test variable (treatment)
    Trial             :  Trial indicator 
    Group             :  Group indicator (in case of subgroup analysis)
    Total             :  Indicator of the grouping for the subtotals
    Litt              :  Name of an eventual data set containing num total (same names as in DATA),
                         npat1-2, ndeath1-2 O_E and VarO_E for the litterature based studies;
    Blind             :  Name of a binary indicator with 1=blinded trial ;

NOTES

	For all calculation, proc IML is not used any more but is replaced by ODS output statement
    from proc LIFETEST : it is more appropriate.

EXAMPLES

    %summary2(DATA=PAT30, outname=TEST, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN);

    %summary2(DATA=PAT30, outname=TEST1, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	GROUP=SEX,TOTAL=AGECAT);

    %summary2(DATA=PAT30, outname=TEST2, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	GROUP=SEX);

    %summary2(DATA=PAT30, outname=TEST3, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	GROUP=SEX,TOTAL=STUDTN);

************************************************************************************
HISTORY

* LC 09/96      : Creation of Summary.sas
* LC  20/03/2001: change LCL&conf2 to LCL_&conf2*;
* KM  31/05/2002: Modified to ensure unknown (.U) values are handled correctly.;
* JRA 13/09/2006: Replacement of summary macro by summary2 macro.
                  Modification using ODS Output results from PROC LIFETEST
                  for estimation per trial of N1,N2,O1, O2, O-E, Var(O-E),OR,IC95% and IC99%
                  Replacement of VAR Parameter by TestVar parameter.
                  Replacement of STRAT and SUBGROUP Parameter by 3 parameters : trial, group 
                  and total
				    Deletion of BENEFITS parameter
				    Add of title column in the output dataset to get back the label of &Timevar.

* JRA 17/10/2006: Correction: modification to keep labels and formats of GROUP or TOTAL variables 
				    in the output dataset

* JRA 20/02/2007: Add of a statement to be sure that VarO_E is positive.

* JRA 02/03/2007: Correction of last MERGE between &LIB..&NAME and tit dataset with a BY.

* JRA 19/10/2007: Add of a condition to avoid missing values to be taken in the results.
                  Correction to avoid dupplicates in result file

* JRA 18/04/2008: Add of some variables in the &Outname dataset (default = SUMMARY)
                  for the display in FOREST2 macro, so as to keep FORMATS and LABELS of KEY VARIABLES
					WE NEED TO KEEP IN the OUTPUT DATASET (BY DEFAULT SUMMARY dataset) :
					 - the name of the variable contained into &TRIAL ---> put in NTRIAL
					 - the format of the variable contained into &TRIAL --> put in f&TRIAL
					 - the variable &TESTVAR;

* JRA 18/09/2008: Correction of an error, for a key merge in a special case (TOTAL and GROUP options together)

* JRA 19/02/2009: Add the possibility to have several variables in GROUP parameter
* In this case if GROUP parameter has several variables, TOTAL should not be used.
* If several variables are entered in GROUP parameter with the use of TOTAL parameter in the same time
* an ERROR message will appear and stop the execution of the macro;

* JRA 27APR2010:  The ODS Output table for LOGRANK matrix has been changed from LoghomCov to LogrankhomCov in SAS 9.2;
* Need to adapt macro code according to this change;

* JRA 18MAY2010:  Add the possibility of a full litterature-based meta-analysis without individual data for TTE endpoint (parameter DATA empty);

************************************************************************************/

%MACRO summary2(DATA, outname, TIMEVAR, CENSVAR, CensVal, TESTVAR, TRIAL, GROUP, TOTAL, litt, blind);

%global TESTV;

%let TESTV=&TESTVAR;

** JRA, 18May2010, add the possibility of a full litterature-based meta-analysis;
%if %length(&data.)=0 %then %let data=NO;

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

** JRA, January 2009, decompositon of vector &GROUP with each element created into &&A&i variable;
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
quit;

* INITIALIZATION OF CENSVAL ;
%if %length(&censval)=0 %then %let censval=1;

** JRA, 18MAY2010, add the possibility to have a full literature-based meta-analysis without individual data 
** for time to event endpoint;

%IF &DATA=NO %THEN %DO; 

	%IF (%length(&litt.)>0) %THEN %DO;
	data __litt;
	set &litt.;* JRA, 14JUN2010, add of &testvar. parameter;
	keep &trial. &group. &total.  npat: ndeath: O_E varO_E &testvar.;
	run;
	%END;

	** JRA, 14JUN2010, Handling of TRIAL format;
	data _null_;
		set __litt;
		myformat=trim(left(vformatn(&TRIAL.)));
		call symput('myformat',myformat);
	run;

	%put myformat &myformat.;

	data &LIB..&NAME.;
		set __litt;
		;

		%IF &NB <=1 %THEN %DO;

			WHERE
			%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)=0) %THEN &GROUP. NE . ;
			%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)>0) %THEN &GROUP. NE . AND &TOTAL. NE .;
			;

			** JRA, 14JUN2010, Handling of TRIAL format and initialization of _Num variable;
			%IF (%length(&GROUP.)=0) %THEN %do;
				%IF &myformat.= $ %then _Num=_n_;;
				%IF &myformat. ne $ %THEN _Num=&TRIAL.;;
			%END;
		

			%IF (%length(&GROUP.)>0) %THEN _Num=&GROUP.;;

		%END;

		%IF &NB > 1 %THEN %DO;

			%DO J=1 %TO &NB;
				IF &&a&j. NE . THEN _Num=&&a&j.;
				if flag=99 then _Num=flag;
			%END;

		%END;

		 _lit=0;

	run;


%END ;


%IF &DATA ne NO %THEN %DO; 

	data indata;
		set &data;
		format &testvar.;
	run;

	** NB OF LEVELS OF TESTVAR **;

	proc sort data=indata out=Nlevel(keep=&testvar. where=(&testvar. > .Z)) nodupkey;
		by &testvar.;
	run;

	data _null_;
		set Nlevel end=final;
		n+1;
		call symput('VAR'||compress(N),compress(&TestVar.));
		if final then call symput('N',compress(N));
	run; 

	proc sort data=indata;
	by &TRIAL. &GROUP. %IF %length(&TOTAL.)>0 %THEN &TOTAL.;
	&TimeVar.
	;
	run;

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
		by &TRIAL. &&a&i. &TimeVar.
		;
		run;

		   ods listing close;
		   ods output censoredsummary=__CensoredSummary&i;
		      ods output homtests=__HomTests&i;
		      ods output homstats=__HomStats&i;
		      ods output LogrankHomCov=__LogHomCov&i;
		   proc lifetest data=indata notables;
		   time &TimeVar.*&CensVar.(&CensVal.);
		   strata &testvar.;
			by &TRIAL. &&a&i. ;
		   run ;
		   ods output close;
		   ods listing;

			proc sort data=__censoredsummary&i;
				by  &TRIAL. &&a&i. ;
			run;

			%DO J=1 %TO &N;
				%PUT VAR&j &&VAR&j ;

				data Summary&J&i;
					set __censoredsummary&i;
					by &TRIAL. &&a&i. ;
					IF &TestVar.=&&VAR&j.;
					npat&j=total;
					ndeath&j=failed;
					keep &TRIAL. &&a&i.  npat&j ndeath&j;
				run;

				%sort(data=summary&J&i,var=&TRIAL. &&a&i.);

			%END;

			data O_E&i;
				set __homstats&i;
				by &TRIAL. &&a&i. ;
				%IF (%length(&&a&i.)=0 ) %THEN if first.&TRIAL.;;
				%IF (%length(&&a&i.)>0 ) %THEN if first.&&a&i.;;
				rename LogRank=O_E;
				label LogRank=" ";
				keep &TRIAL. &&a&i. LogRank;
			run;
			%sort(data=O_E&i,var=&TRIAL. &&a&i. );

			data VarO_E&i;
				set __loghomcov&i;
				by &TRIAL. &&a&i. ;
				%IF (%length(&&a&i.)=0 ) %THEN if first.&TRIAL.;;
				%IF (%length(&&a&i.)>0 ) %THEN if first.&&a&i.;;
				rename _1=VarO_E;
				label _1=" ";
				_1=abs(_1);
				keep &TRIAL. &&a&i. _1;
			run;
			%sort(data=VarO_E&i,var=&TRIAL. &&a&i.);

			data result&i;
				merge %DO J=1 %TO &N; summary&J&i %END; O_E&i VarO_E&i;
				by &TRIAL. &&a&i. ;
				%IF %length(&GROUP.)>0 %THEN %do;
						if &&a&i. >.Z;;
				%end;
				%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;
			run;

			%sort(data=result&i,var=&&a&i.);

			ods listing close;
		    ods output summary=__summary&i;
			proc means data=result&i sum;
			var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
			by &&a&i.;
			run;
		    ods output close;
		    ods listing;

			data __summary&i;
				set __summary&i;
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
				flag=&i;
			run;

			data result&i;
				set __summary&i;
			run;

			%sort(data=result&i,var=&&a&i.);

		
		%END; *END DO I=1 TO &NB ;

		data result;
			set %DO I=1 %TO &NB ; result&i %END;;
		run;

		************;
		*** FOR ALL : computation of GRAND TOTAL IN CASE OF SEVERAL VARIABLES IN GROUP PARAMETER;

		ods listing close;
		ods output censoredsummary=__CensoredSummaryALL;
		ods output homtests=__HomTestsALL;
		ods output homstats=__HomStatsALL;
		ods output LogrankHomCov=__LogHomCovALL;
		 proc lifetest data=indata notables;
		 time &TimeVar.*&CensVar.(&CensVal.);
		 strata &testvar.;
		 by &TRIAL.  ;
		 run ;
		ods output close;
		 ods listing;

		 %DO J=1 %TO &N;

			data SummaryALL&j;
				set __CensoredSummaryALL;
				by &TRIAL. ;
				IF &TestVar.=&&VAR&j.;
				npat&j=total;
				ndeath&j=failed;
				keep &TRIAL. npat&j ndeath&j;
			run;
		  %END;

			data O_E_ALL;
				set __homstatsALL;
				if first.&TRIAL.;
				by &TRIAL.  ;
				rename LogRank=O_E;
				label LogRank=" ";
				keep &TRIAL. LogRank;
			run;
			%sort(data=O_E_ALL,var=&TRIAL.  );

			data VarO_E_ALL;
				set __loghomcovALL;
				if first.&TRIAL.;
				by &TRIAL.  ;
				rename _1=VarO_E;
				label _1=" ";
				_1=abs(_1);
				keep &TRIAL. _1;
			run;
			%sort(data=VarO_E_ALL,var=&TRIAL.);
			data resultALL;
				merge %DO J=1 %TO &N; SummaryALL&J %END; O_E_ALL VarO_E_ALL;
				by &TRIAL.  ;
				%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;
			run;

			ods listing close;
		    ods output summary=__summaryALL;
			proc means data=resultALL sum;
			var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
			run;
		    ods output close;
		    ods listing;

			data __summaryALL;
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
			run;

			data resultALL;
				set __summaryALL;
				_Num=99;
			run;


		*** end FOR ALL ;
		************;

		data &LIB..&NAME.;
			set result(in=a) &litt resultALL(in=a);
			
				%DO I=1 %TO &NB ;
					IF &&a&i. NE . THEN _Num=&&a&i.;;
				%END;

			if a then _lit=0;
			else _lit=1;

		run;

			data _data;
			set &data;
			if &TESTVAR. >.Z  and &TRIAL. >.Z;   
				%do i = 1 %to &nb;
					if &&a&i. >.Z;
				%end;
			run;



		**JRA, 18-09-2008,MODIFY KEY MERGE IN CASE OF GROUP AND TOTAL OPTION;
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
				title=vlabel(&TimeVar.);
				name=vname(&TimeVar.);
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
%IF &NB<=1 %THEN  %DO;

* CALCULATION OF STATISTICS **;

   ods listing close;
   ods output censoredsummary=__CensoredSummary;
      ods output homtests=__HomTests;
      ods output homstats=__HomStats;
      ods output LogrankHomCov=__LogHomCov;
   proc lifetest data=indata notables;
   time &TimeVar.*&CensVar.(&CensVal.);
   strata &testvar.;
	by &TRIAL. &GROUP. %IF %length(&TOTAL.)>0 %THEN &TOTAL.;;
   run ;
   ods output close;
   ods listing;

	proc sort data=__censoredsummary;
		by  &TRIAL. &GROUP. %IF %length(&TOTAL.)>0 %THEN &TOTAL.;;
	run;

	%DO I=1 %TO &N;
		%PUT VAR&I &&VAR&I ;

		data Summary&I;
			set __censoredsummary;
			by &TRIAL. &GROUP. &TOTAL.;
			IF &TestVar.=&&VAR&I.;
			npat&i=total;
			ndeath&i=failed;
			keep &TRIAL. &GROUP. &TOTAL. npat&i ndeath&i;
		run;

	%END;

	data O_E;
		set __homstats;
		by &TRIAL. &GROUP. &TOTAL.;
		%IF (%length(&GROUP.)=0 and %length(&TOTAL.)=0) %THEN if first.&TRIAL.;;
		%IF (%length(&GROUP.)>0 and %length(&TOTAL.)=0) %THEN if first.&GROUP.;;
		%IF (%length(&GROUP.)>0 and %length(&TOTAL.)>0) %THEN if first.&TOTAL.;;
		%IF (%length(&GROUP.)=0 and %length(&TOTAL.)>0) %THEN if first.&TOTAL.;;
		rename LogRank=O_E;
		label LogRank=" ";
		keep &TRIAL. &GROUP. &TOTAL. LogRank;
	run;

	data VarO_E;
		set __loghomcov;
		by &TRIAL. &GROUP. &TOTAL.;
		%IF (%length(&GROUP.)=0 and %length(&TOTAL.)=0) %THEN if first.&TRIAL.;;
		%IF (%length(&GROUP.)>0 and %length(&TOTAL.)=0) %THEN if first.&GROUP.;;
		%IF (%length(&GROUP.)>0 and %length(&TOTAL.)>0) %THEN if first.&TOTAL.;;
		%IF (%length(&GROUP.)=0 and %length(&TOTAL.)>0) %THEN if first.&TOTAL.;;
		rename _1=VarO_E;
		label _1=" ";
		_1=abs(_1);
		keep &TRIAL. &TOTAL. &GROUP. _1;
	run;

	%DO I=1 %TO &N; 
		%sort(data=summary&i,var=&TRIAL. &GROUP. &TOTAL.);
	%END; 
	%sort(data=O_E,var=&TRIAL. &GROUP. &TOTAL.);
	%sort(data=VarO_E,var=&TRIAL. &GROUP. &TOTAL.);

	data result;
		merge %DO I=1 %TO &N; summary&i %END; O_E VarO_E;
		by &TRIAL. &GROUP. &TOTAL.;
		%IF %length(&TOTAL.)>0 %THEN if &total. >.Z;;*JRA, 19Oct2007, to delete missing values;
		%IF %length(&GROUP.)>0 %THEN if &GROUP. >.Z;;
		%IF %length(&TRIAL.)>0 %THEN if &TRIAL. >.Z;;
	run;

	%IF (%length(&GROUP.)>0) %THEN %DO;
	 %sort(data=result,var=&GROUP. &TOTAL.);

	   ods listing close;
	    ods output summary=__summary;
		proc means data=result sum;
		var ndeath1 ndeath2 npat1 npat2 O_E VarO_E ;
		by &GROUP. &TOTAL.;
		run;
	    ods output close;
	    ods listing;

		data __summary;
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

		data result;
			set __summary;
		run;

		%sort(data=result,var=&TOTAL. &GROUP.);

	%END;

	data &LIB..&NAME.;
		set result(in=a) &litt;
		WHERE
		%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)=0) %THEN &GROUP. NE . ;
		%IF (%length(&GROUP.)>0) AND (%length(&TOTAL.)>0) %THEN &GROUP. NE . AND &TOTAL. NE .;
		;
		%IF (%length(&GROUP.)=0) %THEN _Num=&TRIAL.;;
		%IF (%length(&GROUP.)>0) %THEN _Num=&GROUP.;;
		if a then _lit=0;
		else _lit=1;

		 if npat1=. then npat1=0;
		 if npat2=. then npat2=0;
		 if ndeath1=. then ndeath1=0;
		 if ndeath2=. then ndeath2=0;
		 if O_E=. then O_E=0;
		 if VarO_E=. then VarO_E=0;

	run;

	proc contents data=&LIB..&NAME. out=t(keep=name) noprint;
	run;

	data t;
		set t;
		name=UPCASE(NAME);
		if name=upcase("&TRIAL.") then ind1=1;
		if name=upcase("&GROUP.") then ind2=1;
		if name=upcase("&TOTAL.") then ind3=1;
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
		title=vlabel(&TimeVar.);
		name=vname(&TimeVar.);
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
		%SORT(data=tit,var= &GROUP.  );
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
		if a;
	run;


	%END; * END IF NB<=1 THEN  DO;


	* Clear temporary data sets;
	***************************;

	%IF &NB <=1 %THEN %DO;
		proc datasets library=WORK nolist ;
		   delete result Indata O_E VarO_E __CensoredSummary  __HomTests __HomStats __LogHomCov __summary
				%DO I=1 %TO &N;
			      summary&I
			    %END;

			Nlevel t __T blind tit _Trtfmt mytit  _data;
		   run;
		 quit;


	%END;* END IF NB<=1 THEN  DO;

	%IF &NB >1 %THEN %DO;
		proc datasets library=WORK nolist ;
		   delete result Indata  _data
		   O_E_ALL VarO_E_ALL __CensoredSummaryALL __HomTestsALL __HomStatsALL __summaryALL __LogHomCovALL
		   resultALL
			%DO J=1 %TO &NB;
				%DO I=1 %TO &N;
			      summary&I&J 
			    %END;
				O_E&J VarO_E&J result&J __CensoredSummary&J 
				__summary&J __HomTests&J __HomStats&J __LogHomCov&J 
			%END;
				%DO I=1 %TO &N;
			      summaryALL&I 
			    %END;
			Nlevel blind tit _Trtfmt mytit;
		   run;
		 quit;


	%END;* END IF NB>1 THEN  DO;

%END; * END IF DATA NE NO;

%IF &DATA = NO %THEN %DO;
	data &LIB..&NAME.;
	set  &LIB..&NAME.;
			f&TRIAL.=trim(left(vformatn(&TRIAL. )));
			NTRIAL="&TRIAL.";
			if f&TRIAL.="F" or f&TRIAL.="BEST" then f&TRIAL.="8";
	run;

%END;* END IF DATA = NO;

***********************************************;
**   COMPUTATION OF OR and CI               ***;
***********************************************;

data &LIB..&NAME.;
      set &LIB..&NAME.;

	     OR=. ;
	     LCL&alpha1=. ;
	     LCL_&alpha2=. ;
	     UCL&alpha1=. ;
	     UCL_&alpha2=. ;
	     if (varO_E>0) then do ;
	     OR=exp(O_E/varO_E);

	     ** if alpha1=95, probit(1-(100-&alpha1)/200 = probit(0.975)=1.9599;
         ** if alpha2=99, probit(1-(100-&alpha1)/200 = probit(0.995)=2.5758;

	     LCL&alpha1=exp(O_E/varO_E-(probit(1-(100-&alpha1)/200)/sqrt(varO_E)));
	     UCL&alpha1=exp(O_E/varO_E+(probit(1-(100-&alpha1)/200)/sqrt(varO_E)));
	     LCL_&alpha2=exp(O_E/varO_E-(probit(1-(100-&alpha2)/200)/sqrt(varO_E)));
	     UCL_&alpha2=exp(O_E/varO_E+(probit(1-(100-&alpha2)/200)/sqrt(varO_E)));
		 end;

		 if npat1=. then npat1=0;
		 if npat2=. then npat2=0;
		 if ndeath1=. then ndeath1=0;
		 if ndeath2=. then ndeath2=0;
		 if O_E=. then O_E=0;
		 if VarO_E=. then VarO_E=0;

run;


%fin: ;

%mend summary2;



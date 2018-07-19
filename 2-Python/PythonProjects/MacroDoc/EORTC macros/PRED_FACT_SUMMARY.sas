/************************************************************************************

PRED_FACT_SUMMARY.SAS

* MACRO PRED_FACT_SUMMARY IS THE FIRST MODULE OF THE PRED_FACT macro ;
* It was created following the same inspiration as SUMMARY2 macro for meta-analysis;

* The purpose of this macro is to compute HRs, CIs and p values for interaction test and trend test  
* from the COX model using PROC PHREG

**************
Creation date  : 06/2014
Software       : SAS version 9.4
Original author: Jérôme RAPION 
inspired by SUMMARY2 macro for meta-analysis (written by Laurence COLLETTE)

version: 1.1

************************************************************************************

PARAMETERS

    Data              :  Name of the original data file (Required parameter)
    Outname           :  Name for the output data set (Optional parameter) (default = work.summary) 
    TimeVar           :  Time variable (in days) (Required parameter)
    CensVar           :  Censoring variable (Required parameter)
    CensVal           :  Value of CensVar for a censored observation (Optional parameter) (default=1)
    TestVar           :  Test variable (i.e. treatment arm) (Required parameter)
    VAR               :  List of variables to be analyzed. (Required parameter)
    ALPHA             :  Alpha for CI around hazard ratio (in range 0.0001-0.999) (Optional parameter) (default=0.05)
    TRENDINDIC        :  Parameter indicates to the macro if the trend test should be done or not;
                         1=Trend test should be done, 0=not. (Optional parameter) (by default TRENDINDIC=0)
                         see notes below;
NOTES

	 VAR parameter: it is a list of variables, but it may contain only one variable.
	 But, it should contain at list one variable.

     TRENDINDIC parameter: if VAR contains one variable, Trendindic takes only one value,
     If VAR contains several variables, TRENDINDIC should take several values, one per variable contained in VAR.
     If there are less values in TRENDINDIC parameter than in VAR parameter, this parameter will be completed with 0 
     so as to have the same number of values in TRENDINDIC and VAR paramter

EXAMPLES

    %PRED_FACT_SUMMARY(DATA=PAT30, outname=TEST1, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	VAR=SEX);

    %PRED_FACT_SUMMARY(DATA=PAT30, outname=TEST3, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	VAR=SEX AGECAT,TRENDINDIC=0 1);

    %PRED_FACT_SUMMARY(DATA=PAT30, TIMEVAR=PFS, CENSVAR=PFSC, TESTVAR=TREAT, TRIAL=STUDTN,
	VAR=SEX AGECAT WEICAT RADIO,TRENDINDIC=0 1 1 0);


************************************************************************************
REVISIONS

04SEPT2014, JRA: Modification of the way to compute the TREND test. In the previous version, the computation of the 
trend test was not correct.

10AUG2017, GIS: The PHReg output formerly named Type3 is now named ModelANOVA. Change were made.

************************************************************************************/

%MACRO PRED_FACT_SUMMARY(DATA, outname, TIMEVAR, CENSVAR, CensVal, TESTVAR, VAR, ALPHA,TRENDINDIC);

%IF &Alpha.=%STR()  %THEN %DO;
     %LET Alpha=0.05;
%END;

%if (%length(%cmpres(&TRENDINDIC))=0) %then %do;
	%let TRENDINDIC=0 ;
	%GLOBAL NBTR;
	%let NBTR=1;
	%let TR1=0;
%END;

%if %length(&TRENDINDIC.)>0 and &TRENDINDIC. ne 0 %then %RCHV(SUITE=&TRENDINDIC,COMPTE=TR,SEP=" ",N=NBTR);

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

%local VAR ;

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

***** JRA, January 2009, decompositon of vector &VAR with each element created into &&A&i variable;
** Number of elements of vector &VAR put into &nb variable;
%global TESTV NB alpha1 title ftrt;

%let TESTV=&TESTVAR;
%let alpha1=%sysevalf(100-&ALPHA*100);

%LET NB=0;
%if %length(&VAR.)>0 %then %RCHV(SUITE=%UPCASE(&VAR),COMPTE=A,SEP=" ",N=NB);


* DELETE ANY EXISTING DATA SET &LIB.&NAME ;
proc datasets library=&lib nolist;
 delete &name ;
run;
quit;

* INITIALIZATION OF CENSVAL ;
%if %length(&censval)=0 %then %let censval=1;

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
	by &VAR. 
	&TimeVar.
	;
	run;

****************************;
** MANAGEMENT of TREND tests;
****************************;

** CASE IF LESS VALUES GIVEN IN TRENDINDIC THAN IN GROUP PARAMETER: in this case, 0 is filling the blanks;
%if &NBTR lt &NB %then %do;

	%let NDIF=%EVAL(&NB-&NBTR);
	%put &NDIF;

	%DO K = %EVAL(&NBTR+1) %TO &NB;
		%LET TR&k=0;
		%PUT TR&k &&TR&k;
	%END;

%end;

** CASE IF LESS VALUES GIVEN IN GROUP THAN IN TRENDINDIC PARAMETER: in this case, the macro stops;

%if &NBTR gt &NB %then %do;

	%PUT ;%PUT WARNING !! ; %PUT ;
	%PUT IT IS NOT POSSIBLE TO HAVE MORE VALUES IN TRENDINDIC THAN IN GROUP PARAMETER ;
	%PUT YOU SHOULD HAVE THE SAME NUMBER OF VALUES IN BOTH PARAMTERS;
	%PUT OR LESS VALUES IN TRENDINDIC THAN IN GROUP PARAMETER (in this case, 0 is filling the blanks);
	%PUT;

	%goto fin ;

%end;

%DO I=1 %TO &NB ;** JRA, January 2009, loop on each element &&A&i of vector &VAR;

	   proc sort data=indata;
		by &&a&i. &TimeVar.
		;
		run;

		   ods listing close;
		   ods output censoredsummary=__CensoredSummary&i;
		   proc lifetest data=indata notables;
		   time &TimeVar.*&CensVar.(&CensVal.);
		   strata &testvar.;
			by &&a&i. ;
		   run ;
		   ods output close;
		   ods listing;

			proc sort data=__censoredsummary&i;
				by &&a&i. ;
			run;

			%DO J=1 %TO &N;
				%PUT VAR&j &&VAR&j ;

				data Summary&J&i;
					set __censoredsummary&i;
					by &&a&i. ;
					IF &TestVar.=&&VAR&j.;
					npat&j=total;
					ndeath&j=failed;
					keep &&a&i.  npat&j ndeath&j;
					if &&a&i. >.Z;
				run;

				%sort(data=summary&J&i,var=&&a&i.);

			%END;

			data result&i;
				merge %DO J=1 %TO &N; summary&J&i %END; ;
				by  &&a&i. ;
				%IF %length(&VAR.)>0 %THEN %do;
						if &&a&i. >.Z;;
				%end;
			run;

			%sort(data=result&i,var=&&a&i.);

			ods listing close;
		    ods output summary=__summary&i;
			proc means data=result&i sum;
			var ndeath1 ndeath2 npat1 npat2 ;
			by &&a&i.;
			run;
		    ods output close;
		    ods listing;

			data __summary&i;
				set __summary&i ;
				drop Vname:;
				rename 
				ndeath1_sum=ndeath1
				ndeath2_sum=ndeath2
				npat1_sum=npat1
				npat2_sum=npat2;
				attrib ndeath1_sum 	ndeath2_sum npat1_sum npat2_sum  
		        label=" "
				;
				flag=&i;

			run;

			data __result&i;
				set __summary&i;
			run;

			%sort(data=__result&i,var=&&a&i.);

		
		ods listing close;
		proc phreg data=&data;  
			class  &testvar. &&a&i./param=ref ref=first order = INTERNAL  ;
			model &timevar.*&censvar.(&censval.) = &testvar.|&&a&i. /rl type3(score) alpha=&alpha; 
			               nloptions tech=qn;
			hazardratio &testvar. /AT(&&a&i.=all) DIFF=REF alpha=&alpha;
			ods output ModelANOVA=__type3&i;
			ods output HazardRatios=__HazardRatios&i;
		run;

		proc phreg data=&data;  
			class  &testvar. &&a&i./param=ref ref=first order = INTERNAL  ;
			model &timevar.*&censvar.(&censval.) = &testvar.|&&a&i. /rl type3(score) alpha=&alpha; 
			               nloptions tech=qn;
			hazardratio &testvar. /AT(&&a&i.=all) DIFF=REF alpha=&alpha;
			ods output ModelANOVA=__type3&i;
			ods output HazardRatios=__HazardRatios&i.B;
			format &VAR.;
		run;

		%IF &&TR&I=1 %THEN %DO;

			proc phreg data=&data;  
				class  &testvar. /param=ref ref=first order = INTERNAL  ;
				model &timevar.*&censvar.(&censval.) = &testvar.|&&a&i. /rl type3(score) alpha=&alpha; 
                nloptions tech=qn;
				ods output ModelANOVA=__Trendtests&i;
				format &VAR.;
			run;

   		   data __Trendtests&i;
			set __Trendtests&i;
			flag=&i;
			trend=1;
			run;

		%END;

		%IF &&TR&I=0 %THEN %DO;

			data __Trendtests&i;
			flag=&i;
			trend=0;
			ProbScoreChiSq=.;
			run;

		%END;

		proc means data=&data nmiss ;
		var &&a&i.;
		output out=__nmiss&i nmiss=nmiss;
		run;

		data __nmiss&i ;
			set __nmiss&i ;
			&&a&i.="&&a&i.";
			flag=&i;
			label nmiss="n miss";
		run;
		%sort(data=__nmiss&i,var=&&a&i.);

		ods listing ;

		data __HazardRatios&i;	
		set __HazardRatios&i;	
		Description=upcase(Description);
		ind=_n_;
		run;

		data __HazardRatios&i.B;	
		set __HazardRatios&i.B;	
		ind=_n_;
		Description1=upcase(Description);
		WaldLower99=WaldLower;
		WaldUpper99=WaldUpper;

		keep description1 ind WaldLower99 WaldUpper99;	
		run;
		%sort(data=__HazardRatios&i,var=ind);
		%sort(data=__HazardRatios&i.B,var=ind);

		data __HazardRatios&i;
		merge __HazardRatios&i __HazardRatios&i.B;
		by ind;
		drop ind;
		run;

	
		data __HazardRatios&i; length Description1 $100.;set __HazardRatios&i; ;
			Description=TRANWRD (Description, "&&a&i.=","&&a&i.*" ) ;
			Description1=TRANWRD (Description1, "&&a&i.=","&&a&i.*" ) ;
			&&a&i.=scan(Description1,2,'*')*1;
			val=scan(Description,2,'*');
		run;

		
		data __type3&i;
		set __type3&i;
		flag=&i;
		run;

		data __Trendtests&i;
		set __Trendtests&i;
		flag=&i;
		trend=1;
		run;

		%sort(data=__result&i,var=&&a&i.);
		%sort(data=__HazardRatios&i,var=&&a&i.);

		data result&i;
			merge __result&i (in=a)__HazardRatios&i (keep=&&a&i. HazardRatio WaldLower: WaldUpper:);
			by &&a&i.;

			LCL&alpha1=WaldLower;
			UCL&alpha1=WaldUpper;
			OR=HazardRatio;
			drop HazardRatio WaldLower: WaldUpper:;

		run;


%END; *END DO I=1 TO &NB ;

		data __result;
			set %DO I=1 %TO &NB ; result&i %END;;
		run;

		data __nmiss;
			set %DO I=1 %TO &NB ; __nmiss&i %END;;
		run;

		data __HazardRatios;
			set %DO I=1 %TO &NB ; __HazardRatios&i %END;;
		run;

		data __type3;length effect VAR $100.;
			set %DO I=1 %TO &NB ; __type3&i %END;;

			a=index(effect,'*');
			VAR=scan(effect, -1);

			if a=0 then delete;
			drop a;
		pval="p="||compress(put(ProbScoreChiSq,8.3))||" (df="||compress(ScoreDF)||")";
		run;

		data __Trendtests;length effect VAR $100.;
			set %DO I=1 %TO &NB ; __Trendtests&i %END;;

			a=index(effect,'*');
			VAR=scan(effect, -1);

			if a=0 then delete;
			drop a;
		pvaltrend="p="||compress(put(ProbScoreChiSq,8.3));
		run;

		%sort(data=__type3,var=flag);
		%sort(data=__result,var=flag);

		data result;
			merge __result __type3(keep=flag pval ) __nmiss(keep=flag nmiss) __Trendtests (keep=flag pvaltrend trend);
			by flag;
			if not first.flag then pval="";
			if not first.flag then pvaltrend="";
		run;

		
		************;
		*** FOR ALL : computation of GRAND TOTAL IN CASE OF SEVERAL VAR IN VAR PARAMETER;

		ods listing close;
		ods output censoredsummary=__CensoredSummaryALL;
		 proc lifetest data=indata notables;
		 time &TimeVar.*&CensVar.(&CensVal.);
		 strata &testvar.;
		 run ;
		ods output close;
		 ods listing;

		 %DO J=1 %TO &N;

			data SummaryALL&j;
				set __CensoredSummaryALL;
				IF &TestVar.=&&VAR&j.;
				npat&j=total;
				ndeath&j=failed;
				keep npat&j ndeath&j;
			run;
		  %END;

			data resultALL;
				merge %DO J=1 %TO &N; SummaryALL&J %END; ;
			run;

			ods listing close;
		    ods output summary=__summaryALL;
			proc means data=resultALL sum;
			var ndeath1 ndeath2 npat1 npat2  ;
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
				npat2_sum=npat2;
				attrib ndeath1_sum 	ndeath2_sum npat1_sum npat2_sum 
		        label=" "
				;
			run;

			data resultALL;
				set __summaryALL;
				flag=99;
				_Num=99;
			run;

		ods listing close;

		proc phreg data=&data;  
			class  &testvar. /param=ref ref=first order = INTERNAL  ;
			model &timevar.*&censvar.(&censval.) = &testvar. /rl alpha=&ALPHA. ; 
			               nloptions tech=qn;
			ods output ParameterEstimates=__ParameterEstimates;
		run;


		ods listing ;
		%sort(data=__ParameterEstimates,var=parameter);

		data __ParameterEstimates;
		set __ParameterEstimates ;

		_Num=99;
			LCL&alpha1=HRLowerCL;
			UCL&alpha1=HRUpperCL;
			OR=HazardRatio;
			drop HazardRatio HRL: HRU:;
		run;


		*** end FOR ALL ;
		************;

		data &LIB..&NAME.;
			set result(in=a)  resultALL(in=a);
			
				%DO I=1 %TO &NB ;
					IF &&a&i. NE . THEN _Num=&&a&i.;;
				%END;

		run;

		%sort(data=&LIB..&NAME.,var=_Num);
		%sort(data=__ParameterEstimates,var=_num);

		data &LIB..&NAME.;
				merge &LIB..&NAME. (in=a) __ParameterEstimates (keep =_num LCL: UCL: OR ProbChiSq);
				by _num;
		run;

			data _data;
			set &data;
			if &TESTVAR. >.Z  ;   
				%do i = 1 %to &nb;
					if &&a&i. >.Z;
				%end;
			run;


		**JRA, 18-09-2008,MODIFY KEY MERGE IN CASE OF VAR AND TOTAL OPTION;
			data _trtfmt;
			set _data(obs=1);
			keep  &TESTVAR.  &TimeVar.
			&a1. ;
			run; 

			proc contents data=_trtfmt out=format noprint;
			run;

			data format;
			set format;
			name=upcase(name);
				if name in (%UPCASE("&TESTVAR.")) ;
					format=compress(format)||".";
				if format="." then format="8.";
				keep name format ;
			run;
			data _null_;
				set format;
				if name=%UPCASE("&TESTVAR.") then call symput("ftrt",compress(format));
			run;

		*** Keep of the title;
			data tit;
				set _trtfmt;
				title=vlabel(&TimeVar.);
				name=vname(&TimeVar.);
				if title=name then title="";
				keep title &a1;
			;
			run;

			data _null_;
				set tit(obs=1);
				call symput('title',title);*JRA, 30JUN2010: keep of the title in &title variable;
			run;	

		proc datasets library=WORK nolist ;
		   delete __result result Indata  _data 
		    __CensoredSummaryALL __HomTestsALL __HomStatsALL __summaryALL __LogHomCovALL __HazardRatios __ParameterEstimatesB __ParameterEstimates
		   resultALL __type3 __trendtests 
			%DO J=1 %TO &NB;
				%DO I=1 %TO &N;
			      summary&I&J 
			    %END;
				 result&J __CensoredSummary&J __HazardRatios&J __HazardRatios&J.B  __type3&J
				__summary&J __HomTests&J __HomStats&J __LogHomCov&J __result&J  __trendtests&J 
			%END;
				%DO I=1 %TO &N;
			      summaryALL&I 
			    %END;
			Nlevel  _Trtfmt tit  __nmiss: format;
		   run;
		 quit;


***********************************************;
**   COMPUTATION OF OR and CI               ***;
***********************************************;

data &LIB..&NAME.;
      set &LIB..&NAME.;

		 if npat1=. then npat1=0;
		 if npat2=. then npat2=0;
		 if ndeath1=. then ndeath1=0;
		 if ndeath2=. then ndeath2=0;

		_npat2=npat1;
		_npat1=npat2;
		_ndeath1=ndeath2;
		_ndeath2=ndeath1;

		npat1=_npat1;
		npat2=_npat2;
		ndeath1=_ndeath1;
		ndeath2=_ndeath2;

		drop _npat: _ndeath: _Num ;

run;

%sort(data=&LIB..&NAME.,var=flag);

%fin: ;

%SYMDEL NBTR NB;

%mend PRED_FACT_SUMMARY;


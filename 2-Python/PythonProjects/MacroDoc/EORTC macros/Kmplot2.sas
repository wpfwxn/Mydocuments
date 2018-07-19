/************************************************************************************
* KMPLOT2.SAS
*
* Macro to construct input dataset for KM curve
* 
* Curve plotted by EORTC software "GraphLib".
* There are 2 output ascii files: one is c:\temp\kmplot.dat
* the variables are time censor survival  stratum (in that order)
* the other one is c:\temp\kmlabels.dat and contains the standard labels
* values
*
* Version date    : 11/07/2013
* Software        : SAS version 9.3
* Original author : Laurence Collette
* Modified by : Jérôme RAPION
*************************************************************************************
* HISTORY
* 21/11/1995 KMplot macro (Laurence Collette) 
* Revisions
* 02/07/1996 Add options for increasing and test facilities
*            (Laurence Collette)
* 06/08/1996 Change temporary datasets names
*            Change macro name and parameters
*            Add default values for the parameters
*            (Albert Ivanov)
* May 1999   Add where clause (Laurence Collette)
* June 1999  Play with formats and labels to transform KMplot that used the KMlabels
*            dataset into KMplot2 that uses the formats and labels associated with 
*            the variables instead (Laurence Collette)
* 11/08/2000 Changes for SAS version 8
*            - To get p values, store output from PROC LIFETEST in dataset (_HOMTESTS)
*              instead of in text file KM.TXT
*              [Note that although dataset gets deleted, still appears on SAS Results
*               Tree - need a way to prevent this !!!!!!]
*            - Get format name using VFORMATN function
*            - Default variable name changed from _STRTUM_ (ver 6.12) to STRATUM
*            - Need to use uppercase value for _NAME_ in dataset _OUTTEST 
*            Changes for Wilcoxon
*            - Added Wilcoxon test and Wilcoxon trend test
*            Other changes
*            - Use PUT function with p values to avoid losing zeros when rounding
*              e.g. prevent 0.9000 becoming 0.9
*            (John Roberts)
* 18/01/2001 Add option PRINT with default NO to allow printing of basic statistics to the output
*            (Laurence Collette)
* 03/06/2002 Modified to ensure unknown (.U) values are handled correctly. (Kate Moncrieff);
* 18/09/2002 Adapted so that, when called by the INFERENCE macro, the Wald test statistic
*            can be printed on the curve. If TEST is WALD or WALDOVERALL, KMPLOT2 expects
*            to find a macro variable WALD containing the label (p=???) which is to appear. (KM);
* 23/10/2002 Added code to avoid w\arning that _homtests not created when only one level of strat var. (KM);
* 20May2003  Adapted to allow 'stratified for...' to be added to the label (p=???) when the Wald
*            test statistic is passed by the INFERENCE macro.  Instead of expecting a predefined macro 
*            variable named WALD containing p=???, KMPLOT2 now expects a variable named WALDLABEL 
*            containing the entire label to be printed.  (With this change, WALDOVERALL is no longer a 
*            possible value of TEST - TEST=WALD covers both situations.)
* 23Jun2003  P-value for overall logrank test now printed as 'p<0.0001' if < 0.0001, 
*            rounded to 4 dps if < 0.001 and rounded to 3 dps otherwise. (KM)
* 4Jul2003   Value assigned to Where= parameter added to labels files to be passed to GraphLib. (KM)
* 9Sep2003   Label of TestVar= parameter added to labels files (label name "strat"). 
*            Updated so that the Tit2 label now contains the where condition (if any),
*            rather than the treatment group.  (KM)
* 25Jul2006  The macro should not display the result of the p-value in the case of the simultaneous choice 
*            of TESTDATA option and TEST=WT or WO or WB or LW (any test containing a Wilcoxon test), 
*            Only the display of the several KM curves according to the TESTVAR variable will be possible but the display of
*            the p-value will not be done anymore. An ERROR message will be implemented if a user chooses simultaneously these 
*            two options. (JRA)    
* 10Jul2013  Add of value LWT (Logrank Trend Test & Wilcoxon trend test)
*            Add of Score test in case of generation of KMPLOT via INFERENCE2
*            Change of computation methods for trend test, using PROC LIFETEST, strata testvar / trend staements (JRA)
* 28nov2013  Deletion of ODS GRAPHICS ON statement (JRA)
*************************************************************************************
* PARAMETERS
*        data    = input raw data set
*        timevar = time variable (default = 'time')
*        censvar = censoring variable (default = 'censor')
*        censval = censoring value (default = 1)
*        testvar = stratum variable (default = 'strat')
*        test    = L|T|B|WO|WT|WB|LW|LWT|S
*            (L:Overall Logrank test, T:Trend Test, B:Both, 
*             WO:Overall Wilcoxon test, WT:Wilcoxon trend test, WB:Both Wilcoxon test & trend test,
*             LW:Overall Logrank & Overall Wilcoxon, LWT:Logrank Trend Test & Wilcoxon trend test
              S: Score test) ;
*        testdata= name of the sas data set containing the values of the tests
*                  from a meta-analysis
*        increas = Y if you want a death curve starting at 0 and increasing
*        missing = "number" where number is the code for the category "unknown" and you
*                  do not want that category on the plot (eg: missing=99) ;
*        where   = where condition ;
*        project = Project Name for Graphlib ;
*        print   = Y if you want the basic statistics (Logrank, O, N, O-E, ...) in the output (default='N') ;
************************************************************************************/


%macro KMplot2(data, timevar, censvar, censval, testvar, test,
               testdata, increas, missing, where, project, print ) ;

ODS GRAPHICS OFF;
 
* DELETE TEMPORARY DATASETS (should be none);
proc datasets library=work nolist ;
 delete _data _work _worklab _surv _survres _new _export _datetim _outtest;
 run ;
quit;


* INITIALIZE ASCII FILES;

filename asciidat "c:\temp\kmplot.dat" ;
filename asciilab "c:\temp\kmlabels.dat" ;

data _export ;
 file asciidat mod;
 ai=-1;
 put @1 ai  5.0 ;
 run;

data _export ;
 file asciilab mod;
 ai="newGraph ======================================";
 put ai;
 run;


*** DEFAULT PARAMETERS ;

* Assign default value if missing ;
%if %length(&censval)=0 %then   %let censval=1 ;
%if %length(&timevar)=0 %then   %let timevar=time ;
%if %length(&censvar)=0 %then   %let censvar=censor ;
%if %length(&testvar)=0 %then   %let testvar=strat ;
%if %length(&where.)=0 %then     %let where=1 ;
%if (%length(&missing)=0) %then %let missing=. ;
%if (%length(&print)=0) %then %let print=N ;


*** GET FORMAT NAME FOR STRATUM VARIABLE;

data _null_;
   set &data (obs=1);
   call symput('format',trim(left(vformatn(&testvar))));
run;
%if (%length(&format)=0) %then %let format=1;


*** CLEAN DATA - COUNT STRATA - REORDER VALUE LABELS ;

proc sort data=&data out=_data;
 by &testvar ;
 format &testvar 8.;
 run;

data _work (keep=&timevar &testvar _censor title testf) ;
 set _data end=lastobs ;
 by &testvar ;
 where ( (&where.) and &timevar>.Z and &timevar>=0 and &censvar>.Z 
         and &testvar>.Z and &testvar^=&missing);
 * creates a censor indicator identical to _CENSOR_ useful for the merge ;
 if (&censvar=&censval) then _censor=1 ; else _censor=0 ;
length title $45.;
 call label(&timevar,title ) ;
 testf=put(&testvar,&format..) ;
 format &testvar 8.;
 run;

data _worklab ;
set _work ;
retain count 0 ;
by &testvar ;
if (first.&testvar) then
do ; count=count+1 ; output;  end ;
drop _censor &timevar ;
run ;

* Put number of levels in macro var.
 (Used in following section to avoid w\arning that _HOMTESTS not created when only one level);
data _null_;
set _worklab end=LastObs;
if LastObs then call symput('NStrata',trim(left(put(Count,3.))));
run;

**** ANALYSIS ;

proc sort data=_work out=_work ;
 by &testvar &timevar _censor ;
 run ;

* Produce survival dataset(_survres) and p value dataset (_homtests);
%IF %EVAL(&NStrata.>1) %THEN %STR(ods output homtests=_homtests;);
%ELSE %STR(ods output;);
%if (&print=Y) %then %STR(*);
ods listing close;
proc lifetest data=_work outsurv=_survres notables;
 time &timevar*_censor(1);
 strata &testvar;
run;
ods output close;
%if (&print=Y) %then %STR(*);
ods listing;



proc sort data=_survres ;
 by &testvar ;
 run;

data _surv ;
 set _survres;
 by &testvar ;
 if (first.&testvar) then delete ;
  _censor=_CENSOR_ ;
  _surv=survival*100 ;
 %if (%upcase(&increas)=Y) %then %do ;
   _surv=100-_surv ;
 %end ;
 keep _censor _surv &timevar &testvar STRATUM ;
 run;


*** EXPORT CURVE ;

data _new ;
 retain _prev_  ;
 merge _work _surv ;
 by &testvar &timevar _censor ;
 _time=&timevar ;
 if (missing(_surv) and first.&testvar=0) then _surv=_prev_ ;
 if (missing(_surv) and first.&testvar=1) then do ;
   %if (%upcase(&increas)^=Y) %then %do ; _surv=100 ; %end ;
   %if (%upcase(&increas)=Y) %then %do ; _surv=0 ; %end ;
 end ;
 _stratum=STRATUM ;
 _prev_=_surv ;
 if  missing(_time) or _time<0 or missing(_censor) or missing(_stratum)  then delete ;
 drop  STRATUM &timevar ;
 run ;

data _export ;
 set _new;
 file asciidat mod;
 put @1  _stratum  5.0
     @6  _time     10.0
     @16 _surv 15.8
     @31 _censor   5.0;
 run;


**** TESTS ;

%let TE3=;
%let TR3=;


* Reads the (stratified) P-values from the Meta-analysis output ;

%if (%length(%trim(&testdata))^=0) %then %do ;
data &testdata ;
 set &testdata ;
 call symput('TE'||left(trim(TE)), TEv) ;
 call symput('TR'||left(trim(TTR)), TTRv) ;
 run ;
%end ;


* If a Trend test is requested, and there is no test data set from meta-analysis,
* this computes a test for trend ;  

%if ((%upcase(&test)=T or %upcase(&test)=B or %upcase(&test)=LWT) and %length(&TR3)=0 and %length(&TESTDATA)=0) %then %do ;


ods listing close;
proc lifetest data=_work; 
 time &timevar*_censor(1);
	strata &testvar / trend;
	format &testvar;
ods output trendtests = _trendtests; 
run;

data _null_ ;
 set _trendtests ;
 if (upcase(Test)='LOG-RANK') then do ;
 if Probz>0 then p='='||put(round(Probz,0.001),6.3);
 if Probz=0 then p='<0.001' ;
   call symput('TR3',left(trim(p))) ;
end;
run;

ods listing;



%end ;


* If a Wilcoxon Trend test is requested this computes the test ;

%if (
((%upcase(&test)=WT or %upcase(&test)=WB or %upcase(&test)=LWT ) 
and %length(&TESTDATA)=0 and %length(&TR3)=0)
OR
%upcase(&test)=LWT
) 

%then %do ;


ods listing close;
proc lifetest data=_work; 
 time &timevar*_censor(1);
	strata &testvar / trend;
	format &testvar;
ods output trendtests = _trendtests; 
run;

data _null_ ;
 set _trendtests ;
 if (upcase(Test)='WILCOXON') then do ;
 if Probz>0 then p='='||put(round(Probz,0.001),6.3);
 if Probz=0 then p='<0.001' ;
   call symput('TRWIL',left(trim(p))) ;
end;
run;

ods listing;

%end ;


* If a Logrank test is requested, and there is no test data set from meta-analysis,
* the following retrieves the value from the proc lifetest ODS output dataset;
%if ((%upcase(&test)=L or %upcase(&test)=B or %upcase(&test)=LW) 
     and %length(&TE3)=0 and %length(&TESTDATA)=0) %then %do ;
   data _null_;
      set _homtests;
      where test='Log-Rank';
      length PText $10;
      if missing(probchisq) then;
      else if probchisq<0.001 then PText='<0.001';
      else if probchisq<0.001 then PText='='||trim(left(put(round(probchisq,0.001),6.3)));
      else PText='='||trim(left(put(round(probchisq,0.001),5.3)));
      call symput('lr',trim(left(PText)));
      call symput('dflr',trim(left(DF)));
   run;
   %let TE3=&lr ;
   %IF &NStrata>2 %then %let TE3=&TE3. (df=&dflr.);

%end ;


* If a Wilcoxon test is requested, the following retrieves the value
* from the proc lifetest ODS output dataset;
%if (%upcase(&test)=WO or %upcase(&test)=WB or %upcase(&test)=LW and %length(&TESTDATA)=0) %then %do ;
   data _null_;
      set _homtests;
      where test='Wilcoxon';
      call symput('wilcox',put(round(probchisq,0.001),6.3));
      call symput('dfw',trim(left(DF)));
  run;
   %let TEWIL==&wilcox ;
   %IF &NStrata>2 %then %let TEWIL=&TEWIL. (df=&dfw.);

%end ;


*** DATE AND TIME ;

data _datetim (drop=date time);
 date=date() ;
 time=time() ;
 label=left(put(date,worddatx12.))||'  '||left(put(time,time5.));
 labref="Date" ;
 format date worddatx12. ;
 format time time6. ;
 file asciilab mod ;
 put labref label ;
 run ;


*** EXPORT LABELS ;

data _export ;
 set _worklab end=lastobs;
  file asciilab mod;
  format label $45. ;
  labref='Val'||trim(left(count)) ;
  label=testf ;
  put labref label ;
if lastobs then do ;
  labref='Tit1' ;
  label=title ;
  put labref label ;

  labref='Tit2' ;
  %IF %BQUOTE(&Where.)=1 %THEN %STR(label=' ';);
  %ELSE %STR(label="%BQUOTE(&Where.)";);
  put labref label ;

  labref='Project' ;
  label=symget('project');
  put labref label ;

  labref='Strat' ;
  label=vlabel(&TestVar.);
  put labref label ;

  labref='Where';
  %IF %BQUOTE(&Where.)=1 %THEN %STR(label='All records';);
  %ELSE %STR(label="%BQUOTE(&Where.)";);
  put labref label;

end ;
run;


*** EXPORT TESTS ;

%if (%length(&test)>0) %then %do ;

%let SCORELABEL_EXIST=%symexist(SCORELABEL);
%let StratVarLbl_EXIST=%symexist(StratVarLbl);


%local Stratif ;

%if &StratVarLbl_EXIST=1 %then %do;
	%if %length(&StratVarLbl.) ne 0 %then %let Stratif=stratified for &StratVarLbl.;
%end;

   data _export ;
      format labref $8. ; format label $200. ;

     * SCORE;
      %if (%upcase(&test)=S and %length(&TESTDATA)=0) %then %do ;
         labref="TREND"; /* Required but not used by Graphlib so value not important. */
         label="&ScoreLabel.";
         file asciilab mod;
         put labref label;
      %end ;

      * Logrank;
      %if (%upcase(&test)=L or %upcase(&test)=B or %upcase(&test)=LW) %then %do ;

		%IF &SCORELABEL_EXIST=1 %THEN %DO;
			%IF %length(&SCORELABEL.) ne 0 %THEN %DO;
	         labref="TREND"; 
	         label="&ScoreLabel.";
	         file asciilab mod;
	         put labref label;
			 %END;
		%END;

         labref="LOGRANK" ;
		 %IF %length(&Stratif.) ne 0 %THEN label="Overall Logrank test &Stratif: p&TE3s";;
		 %IF %length(&Stratif.) = 0 %THEN label="Overall Logrank test: p&TE3";;
         file asciilab mod ;
         put labref label ;
      %end ;


      * Trend test (logrank);
      %if (%upcase(&test)=T or %upcase(&test)=B) %then %do ;

		%IF &SCORELABEL_EXIST=1 %THEN %DO;
			%IF %length(&SCORELABEL.) ne 0 %THEN %DO;
	         labref="TREND"; 
	         label="&ScoreLabel.";
	         file asciilab mod;
	         put labref label;
			 %END;
		%END;

         labref="TRENDWIL" ;
		 %IF %length(&Stratif.) ne 0 %THEN label="Test for trend (Logrank) &Stratif: p&TE3s";;
		 %IF %length(&Stratif.) = 0 %THEN label="Test for trend (Logrank): p&TR3";;
         file asciilab mod ;
         put labref label ;
      %end ;


      * Logrank;
      %if (%upcase(&test)=LW and %length(&TESTDATA)=0) %then %do ;
         labref="LOGRANK" ;
         label="Overall Logrank test &Stratif: p&TE3";
         file asciilab mod ;
         put labref label ;
      %end ;

      * Wilcoxon;   
      %if ((%upcase(&test)=WO or %upcase(&test)=WB or %upcase(&test)=LW) and %length(&TESTDATA)=0) %then %do ;

		%IF &SCORELABEL_EXIST=1 %THEN %DO;
	 		%IF %length(&SCORELABEL.) ne 0 %THEN %DO;
	        labref="TREND"; 
	         label="&ScoreLabel.";
	         file asciilab mod;
	         put labref label;
	        %end ;
        %end ;

         labref="WILCOXON" ;
		 %IF %length(&Stratif.) ne 0 %THEN label="Overall Wilcoxon test &Stratif: p&TEWILs";;
		 %IF %length(&Stratif.) = 0 %THEN label="Overall Wilcoxon test: p&TEWIL";;
         file asciilab mod ;
         put labref label ;

      %end ;



      * Trend test (Wilcoxon);
      %if ((%upcase(&test)=WT or %upcase(&test)=WB) and %length(&TESTDATA)=0) %then %do ;

		%IF &SCORELABEL_EXIST=1 %THEN %DO;
	 		%IF %length(&SCORELABEL.) ne 0 %THEN %DO;
	         labref="TREND"; 
	         label="&ScoreLabel.";
	         file asciilab mod;
	         put labref label;
	        %end ;
        %end ;

         labref="TRENDWIL" ;
		 %IF %length(&Stratif.) ne 0 %THEN label="Test for trend (Wilcoxon) &Stratif: p&TEWILs";;
		 %IF %length(&Stratif.) = 0 %THEN label="Test for trend (Wilcoxon): p&TRWIL";;

         file asciilab mod ;
         put labref label ;
      %end ;


     * Trend test (Wilcoxon and Logrank);
      %if %upcase(&test)=LWT %then %do ;

 		%IF &SCORELABEL_EXIST=1 %THEN %DO;
	 		%IF %length(&SCORELABEL.) ne 0 %THEN %DO;
		         labref="TREND"; 
		         label="&ScoreLabel.";
		         file asciilab mod;
		         put labref label;
	        %end ;
        %end ;

         labref="LOGRANK" ;

		 %IF %length(&Stratif.) ne 0 %THEN label="Test for trend (Logrank) &Stratif: p&TE3s";;
		 %IF %length(&Stratif.) = 0 %THEN label="Test for trend (Logrank): p&TR3";;
         file asciilab mod ;
         put labref label ;

         labref="TRENDWIL" ;
		 %IF %length(&Stratif.) ne 0 %THEN label="Test for trend (Wilcoxon) &Stratif: p&TEWILs";;
		 %IF %length(&Stratif.) = 0 %THEN label="Test for trend (Wilcoxon): p&TRWIL";;


         file asciilab mod ;
         put labref label ;

      %end ;


   run ;
%end ;

%if %length(&TESTDATA)^=0 and (&TEST =WO or &TEST=WT or &TEST=LW or &TEST=WB) 
%then %do;
%put **************************************************************************************;
%put ***** ERROR : A wilcoxon test could not be computed in the case of a meta-analysis    ;
%put ***** You could not use simultaneously TESTDATA parameter and TEST=WT or WO or WB     ;
%put ***** or LW  option (any test containing a Wilcoxon test).                            ;
%put ***** Only the display of the several KM curves according to TESTVAR variable is done ; 
%put ***** No p-value of statistical test is displayed in this case.                       ;
%put **************************************************************************************;
%end;
   

* CLEAN UP;
proc datasets library=work nolist ;
 delete _data _work _worklab _surv _survres _new _export 
 _datetim _outtest _homtests;
 run ;
quit;

   
%mend KMplot2 ;


/************************************************************************************
*
* PRED_FACT.SAS
* *************
*
* Part of a suite of programs to produce a forest plot showing results from PROC PHREG.  
*
* This macro links all modules of the PRED_FACT_* family.  This is the only macro
* the end user needs to invoke in order to produce a forest plot showing HRs, CIs and p values for interaction test
* and trend test computed from a COX model using PROC PHREG on time-to-event data
*
*
* This version calls PRED_FACT_SUMMARY and PRED_FACT_FOREST.
* 
* Version date    : June 2014
* Software        : SAS version 9.4
* Original author : Jérôme RAPION
* inspired by ALLFIXED2 macro for meta-analysis (written by Laurence COLLETTE)
*
*************************************************************************************
*
* HISTORY
*
*   Inspired from: ALLFIXED2 (by Laurence Collette)
*   A lot a graphical setting and annotations are taken from FOREST2
*
*************************************************************************************
*
* PARAMETERS
*
*    DATA    : Name of the original data file (Required parameter) 
*    TIMEVAR : Time variable (in days) (Required parameter) 
*    CENSVAR : Censoring variable (Required parameter)
*    CENSVAL : Value of CensVar for a censored observation (Optional parameter) (default=1)
*    TESTVAR : Test variable (i.e. treatment arm) (Required parameter)
*    VAR     : List of variables to be analyzed. (Required parameter) 
*    ALPHA   : Alpha for CI around hazard ratio (in range 0.0001-0.999) (Optional parameter) (default=0.05) 
*    display : PNG, TIFF, JPEG, PDF or SVG (Optional parameter) (default=PNG)
*    OutFile : Path and name of the file to contain the graphical output without its extension
*    SCALE   : LIN or LOG, (Optional parameter) (default=LOG) 
*    Final   : Y to remove footnote date (Optional parameter) (default=N)
*    Colour  : Set to Y to obtain graph in colour, N for black and white (default=Y)
*    Where   : Selection condition (Optional parameter) (default: on all data) 
*    SUBTIT  : Subtitle to add (Optional parameter)
*               (not necessary to write your subtitle between quotes, by default= no subtitle)
* TRENDINDIC : Parameter indicates to the macro if the trend test should be done or not;
*              1=Trend test should be done, 0=not. (Optional parameter) (by default TRENDINDIC=0)
*              see notes below;
*
*************************************************************************************
*
* NOTES
*
*	 VAR parameter: it is a list of variables, but it may contain only one variable.
*	 But, it should contain at list one variable.
*
*    TRENDINDIC parameter: if VAR contains one variable, Trendindic takes only one value,
*    If VAR contains several variables, TRENDINDIC should take several values, one per variable contained in VAR.
*    If there are less values in TRENDINDIC parameter than in VAR parameter, this parameter will be completed with 0 
*    so as to have the same number of values in TRENDINDIC and VAR paramter
*
*    DISPLAY parameter: PDF extension
*    If a journal (JCO or Lancet for instance) ask you to get graphs in vectorial image (EPS, vectorialPDF), you should 
*    first save your graph in PDF format: DISPLAY=PDF, and maybe it is enough for your journal.
*    If the journal requires a plot into EPS format, ask IT support to transform your PDF file 
*    into EPS format via a graphical software.
*
*
************************************************************************************/

%macro PRED_FACT(DATA, TIMEVAR, CENSVAR, CENSVAL, 
					 TESTVAR, VAR,  
                     ALPHA,
                     display, outfile, 
                     Scale=, Final=, Colour=Y, Where=%STR(),Subtit=, TRENDINDIC=);

** JRA, 27May2010;
** Definition of SCALE default according to the type of endpoint (BIN or TTE);
%if %length(&SCALE.)=0 %then %do;
	%let SCALE=LOG;
%end;

** JRA, 31MAY2010;
%IF %LENGTH(&Subtit.)=0 %THEN %LET Subtit=;
%IF %LENGTH(&display.)=0 %THEN %LET display=PNG;
%LET Scale=%UPCASE(&Scale.);
%LET Final=%UPCASE(&Final.);
%LET Colour=%UPCASE(&Colour.);

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

%GLOBAL NB;
%LET NB=0;

** JRA, January 2009, decompositon of vector &VAR with each element created into &&A&i variable;
** Number of elements of vector &VAR put into &nb variable;
%if %length(&VAR.)>0 %then %RCHV(SUITE=&VAR,COMPTE=A,SEP=" ",N=NB);

%IF (%LENGTH(&Where.)>0) %THEN %DO;
   data __Selected;
   set &Data.;
   where &Where.;
   run;
   %LET Data=__Selected;
%END;

**************************************************************************;
* DEFAULT VALUES ; 

%LET summfile=WORK.SUMMARY ;

* default value of var is treat ;
%IF %LENGTH(&testvar.)=0 %THEN %LET testvar=trt;

* default censoring value is 1 ;
%if (%length(&censval)=0) %then %let censval=1 ;
**************************************************************************;

*************************;
* TIME TO EVENT DATA ****;
*************************;

%IF (%LENGTH(&timevar.)>0) %THEN %DO;

		%PRED_FACT_SUMMARY(data=&data, outname=&summfile, 
		timevar=&timevar, censvar=&censvar, censval=&censval, 
		testvar=&testvar, VAR=&VAR, ALPHA=&ALPHA.,TRENDINDIC=&TRENDINDIC ) ;


		%PRED_FACT_FOREST(summfile=work.summary, 
		              OUTFILE=&outfile,display=&display, 
		              VAR=&VAR,  
		 scale=&scale., final=&final., colour=&colour.,Subtit= &Subtit.);


%END;

%mend PRED_FACT ;


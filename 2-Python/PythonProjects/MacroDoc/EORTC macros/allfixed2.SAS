/************************************************************************************
*
* ALLFIXED2.SAS
* *************
*
* Part of a suite of programs to perform a meta analysis on time-to-event
* data.  Input data must be in the form of individual patient data.
*
* This macro links all parts of the time-to-event MA software.  This is the only macro
* the end user needs to invoke in order to perform a meta analysis on time-to-event data.
* This version calls summary2, test2, KMPlot2 and Forest2.
* 
* Version date    : February 2003
* Software        : SAS version 8.02
* Original author : Laurence Collette
*
*************************************************************************************
*
* HISTORY
*
*   Based on:
*     AllFixed2
*     SECOND ATTEMPT BY LC 20/02/96 ;
*     MODIFICATION BY LC 02/02/2001 to upgrade from ALLFIXED to enable the use of KMPLOT2 within the macro ;
*   Revisions:
*     MODIFICATION BY KM 18/02/2003: This version calls Forest2, not Forest.;
*     Feb 2003  Added parameters Scale, Final and Colour (Kate Moncrieff)
*     May 2003  Added Where parameter.
*               Updated documentation.   (KM)
*     Feb 2007  Removing of the call of MAINPUT macro and copy/paste of the same statement
*               inside ALLFIXED2.
*               Modifications due to previous modifications for TEST2, SUMMARY2 and FOREST2 (JRA)
*     Mar 2007  Add of re-initialization of TESTV gmobal macro variable at the end of ALLFIXED2
*     Oct 2007  Add a default value for DISPLAY parameter (default: DISPLAY=WORD, ie cgm extension)
*     Nov 2007  Add of parameter from ALLFIXEDB2 (respvar,respval,EndPoint) inside ALLFIXED2
*               Afterwards, only ALLFIXED2 could be used to do MA analysis such for time-to-event or binary data
*               So ALLFIXEDB2 macro will become obsolete.
*     May 2008  Add of possibility to run ALLFIXED with a litterature-based meta-analysis,
*               but only for binary data (JRA)
*    Sept 2008  Add of possibility to draw a reference line to a specific value of HR,
*               specified in ANYMARGIN parameter (JRA)
*    Feb 2009   Add the possibilty to have several variables in GROUP parameter
*               In this case if GROUP parameter has several variables, TOTAL should not be used. 
*               Add of Trendindic parameter to indicates if the TREND test should be done or not (JRA)
*    July 2010  Add of TTEBin parameter. This paramter is useful in case of a full litterature-based meta-analysis  
*               without individual data. In this case, without CENSVAR or RESPVAR filled in, the macro cannot know if 
*               the analysis is related to a binary or a time to event endpoint. (JRA)
*               Deletion of FORESTB2 macro call as its contents and functionalities are now included inside 
*               FOREST2 macro. (JRA)
*   Sept 2011   Add of WMF format in DISPLAY parameter
*               It is now possible to create a file with .wmf extension. (JRA)
*   June 2014   Add of SVG format in DISPLAY parameter
*               Modification of PDF format to make it vectorial
*               Set ROUND=Y as a default
*               Cosmetic changes: remove the bug where 'Missing' is displayed wrongly
*               in case of 2 variables with the same label, display of both labels instead of only one time 
*               Change of default for DISPLAY parameter (PNG instead of WORD) (JRA)
*               Deletion of WMF format possibility for DISPLAY parameter (not compatible with SAS 9.4). 
*               WMF format is replaced by this 'modified' PDF format which is a true vectoriel format (JRA)
*  Mar 2015     Add of TITLE parameter to let the user to to remove the title (JRA)
*               Deletion of SVG format in DISPLAY parameter which is not used in practice.
*************************************************************************************
*
* PARAMETERS
*
*    data    : individual patient data file 
*    timevar : time to event variable 
*    censvar : censoring variable 
*    censval : censored indicator
*    respvar : response variable 
*    respval : indicator of response (default=1) 
*    testvar : treament variable, variable whose effect is being analysed 
*    trial   : trial indicator 
*    group   : group indicator (in case of subgroup/pfanalysis) 
*    total   : indicator of the grouping for the subtotals 
*    missing : value which indicates unknown 
*    round   : Y if P=0.0056 printed P=0.006, N if P=0.0056 printed P<0.01 
*    literat : SAS file containing literature based data ;
*    blind   : variable indicating the blinding 
*    conf1   : first confidence level (used for individual CI,  (and for totals if Bonf=N), default=95 if bonf=N and 99 if bonf=Y) 
*    conf2   : second confidence level (used for totals if bonf=Y)
*    bonf    : Y implies that totals have CI at level conf2 (=95) and individual observations at level conf1 (=99) 
*    increas : Y if you want an increasing KM curve
*    labelsFP: SAS file containing the labels for the Forest Plot 
*    display : SCREEN, WORD or POWERPOINT (ie cgm extension), TIFF, PNG or PDF (default=PNG)
*    outfile : DOS location of the printable/importable Forest plot which is produced by the macro (without quotes)
*    Project : project name for the classification of the KMplot in VISTA (note: if no PROJECT is specified, no KMplot is computed)
*    Scale   : LIN(linear scale)|LOG(log scale). Default=LOG
*    Final   : Y to remove footnote 'Not for publication...' and date
*    Colour  : Set to Y to obtain graph in colour, N for black and white (default=Y)
*    Where   : Selection condition
*    EndPoint: Set to S if an event means "success", set to F if an event means "failure" (default=F)
*    Subtit  : Subtitle to add (default= no subtitle)
* Trendindic : Parameter indicates to the macro if the Trend test should be done or not;
*              1=Trend test should be done, 0=not. (by default TRENDINDIC=0)
*              see notes below;
*  Anymargin : value of a reference hazard ratio to be compared to. It draws a line in red to this value,
*              and displays this value in red.
*  TTEBin    : Parameter which indicates if a meta-analysis with time to event or binary endpoint should be run.
*              This parameter is really needed in case of a full litterature-base meta-analysis.
*			   Values: TTE (for time to event endpoint) or BIN (for binary endpoint)
*              By default, this parameter is filled in according to CENSVAR or RESPVAR filling.
*              In case of a full litterature-based meta-analysis without individual data, default is TTE ;
*    TITLE   : Set to Y to obtain a title (label of the endpoint variable taken from title variable in SUMMFILE dataset);
*            : N to remove the title (default=Y)
*************************************************************************************
*
* NOTES
*
*    Refer to the notes in the program header of the macro ALLFIXEDB2.SAS for information
*    on importing graphs into Word or PowerPoint.
*
*    Trendindic parameter: if GROUP contains one variable, Trendindic takes only one value,
*    If GROUP contains several variables, TRENDINDIC should take several values, one per variable contained in GROUP.
*    If there are less values in TRENDINDIC parameter than in GROUP parameter, this parameter will be completed with 0 
*    so as to have the same number of values in TRENDINDIC and GROUP paramter
*
*    TTEBin parameter:
*    if CENSVAR is filled in, TTEBin takes automatically the value TTE.
*    if RESPVAR is filled in, TTEBin takes automatically the value BIN.
*    if neither CENSVAR or RESPVAR are filled in, by default TTEBin takes the value TTE. 
*    If a user want to run a full litterature-based meta-analysis based on a binary endpoint, he should set TTEBin to BIN.
*
*    DISPLAY parameter: PDF extension
*    If a journal (JCO or Lancet for instance) ask you to get graphs in vectorial image (EPS, vectorialPDF), you should first save your graph 
*    in PDF format: DISPLAY=PDF, and maybe it is enough for your journal. 
*    If the journal requires a plot into EPS format, ask IT support to transform your PDF file into EPS format via a graphical software.
*
*
************************************************************************************/

%macro allfixed2(data, timevar, censvar, censval, respvar, respval,
					 testvar, trial, group, total, 
                     missing, round,literat,blind, conf1, conf2,
                     bonf, increas,labelsFP,display, outfile, Project, endpoint,
                     Scale=, Final=, Colour=Y, Where=%STR(),Subtit=,TRENDINDIC=,Anymargin=,TTEBin=, TITLE=Y);

proc datasets library=work nolist ;
 delete summary tests totals;
run ;

** JRA, 8May2008, add the possibility of a full litterature-based meta-analysis;
%if %length(&data.)=0 %then %let data=NO;

** JRA, 31MAY2010;
%LET TTEBin=%UPCASE(&TTEBin.);

** JRA, 18MAY2010, if TTEBin is not defined by the user, definition according to CENSAR or RESPVAR parameters;

** Default value for TTEBin parameter is TTE if DATA=NO;
%if %length(&TTEBin.)=0 and &data. = NO  %then %do;
	%let TTEBin=TTE;
%end;

%if %length(&TTEBin.)=0 and &data. ne NO %then %do;
	%if %length(&censvar.) >0 %then %let TTEBin=TTE;
	%if %length(&respvar.) >0 %then %let TTEBin=BIN;
%end;

** JRA, 31MAY2010, creation of the global macro TYP_ENDPOINT with the values of TTEBin parameter to distinguish if
** we are analyzing TTE or Binary data;
** TYP_ENDPOINT is used afterwards into FOREST2 macro;

%global typ_endpoint;
%let typ_endpoint=&TTEBin.;

** JRA, 27May2010;
** Definition of SCALE default according to the type of endpoint (BIN or TTE);
%if %length(&SCALE.)=0 %then %do;
	%if &TTEBin.=TTE %then %let SCALE=LOG;
	%if &TTEBin.=BIN %then %let SCALE=LIN;
%end;

** JRA, 27May2010;
** Definition of ENDPOINT default according to the type of endpoint (BIN or TTE);
%if %length(&ENDPOINT.)=0 %then %do;
	%if &TTEBin.=TTE %then %let ENDPOINT=;
	%if &TTEBin.=BIN %then %let ENDPOINT=F;
%end;

** JRA, 31MAY2010;
%IF %LENGTH(&Subtit.)=0 %THEN %LET Subtit=;
%IF %LENGTH(&labelsFP.)=0 %THEN %LET labelsFP=;
** JRA, 12JUNE2014, change the default from WORD to PNG;
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

** JRA, January 2009, decompositon of vector &GROUP with each element created into &&A&i variable;
** Number of elements of vector &group put into &nb variable;
%if %length(&group.)>0 %then %RCHV(SUITE=&group,COMPTE=A,SEP=" ",N=NB);

%if (%length(%cmpres(&anymargin))=0) %then %let anymargin=NO ;

%IF (%LENGTH(&Where.)>0) %THEN %DO;
   data __Selected;
   set &Data.;
   where &Where.;
   run;
   %LET Data=__Selected;
%END;

%if (%LENGTH(&trial.)=0) and (%LENGTH(&group.)=0)  %then %do;

%PUT ;%PUT WARNING :; %PUT;
%PUT IT IS MANDATORY TO ENTER A TRIAL NUMBER IF A GROUP INDICATOR IS NOT ENTERED;
%PUT;

%goto fin ;

%end;

%if (%LENGTH(&total.)>0) and (&NB >1)  %then %do;

%PUT ;%PUT WARNING !! ; %PUT ;
%PUT IT IS NOT POSSIBLE TO HAVE A VARIABLE IN TOTAL PARAMETER ;
%PUT AND IN THE SAME TIME SEVERAL VARIABLES IN GROUP PARAMETER;
%PUT YOU SHOULD HAVE ONLY ONE VARIABLE IN GROUP PARAMETER IF YOU WANT TO USE TOTAL PARAMETER;
%PUT OR NOTHING IN TOTAL PARAMETER IF YOU WANT TO HAVE SEVERAL VARIABLES IN GROUP PARAMETER;
%PUT;

%goto fin ;

%end;

%IF (%LENGTH(&trial.)=0) and (%LENGTH(&group.)>0) %THEN %DO;

   %let trial=one;

   data __data;
   set &Data.;
   &trial.=1;
   run;
   %LET Data=__data;

%END;

**************************************************************************;
* DEFAULT VALUES ; 
* (replace call of MAINPUT macro - JRA, 09-02-2007 );

* default output data set for the summary is WORK.SUMMARY ;
* default output data set for the tests  is WORK.TESTS ;
* default output data set for the totals is WORK.TOTALS ;

%let outtest=WORK.TESTS ;
%LET outtotal=WORK.TOTALS ;
%LET summfile=WORK.SUMMARY ;

* default value of var is treat ;
%IF %LENGTH(&testvar.)=0 %THEN %LET testvar=trt;

* default value for conf1 and conf2 ; * LCO, 21NOV2007;
%if (%length(&conf2)=0) %then %let conf2=95 ;
%if (%upcase(&BONF)=Y) %then %do ;
%if (%length(&conf1)=0) %then %let conf1=99 ;
%end ;
%if (%upcase(&BONF)^=Y) %then %do ;
%if (%length(&conf1)=0) %then %let conf1=95 ;
%end ;


** JRA, 11JUNE2014;
* default value for round is Y ;
%if (%length(&round)=0) %then %let round=Y ;

* default censoring value is 1 ;
%if (%length(&censval)=0) %then %let censval=1 ;
%if (%length(&respval)=0) %then %let respval=1 ;


%if (%length(&group)>0) %then %let reqtest=T ;
%if (%length(&group)=0) %then %let reqtest=L ;

**************************************************************************;

*************************;
* TIME TO EVENT DATA ****;
*************************;

%IF (%LENGTH(&timevar.)>0) %THEN %DO;
		* CREATES THE SUMMARY DATA SET ;

		 %summary2(data=&data, outname=&summfile, timevar=&timevar, censvar=&censvar, censval=&censval, testvar=&testvar, trial=&trial, group=&group, total=&total,litt=&literat,blind=&blind ) ;

		 * FIXED EFFECT MODEL;

		 %testf2(summfile=&summfile, outtest=&outtest, outtotal=&outtotal,
		           group=&group, total=&total, tren=&missing, round=&round,TRENDINDIC=&TRENDINDIC) ;

		* Call of KMPLOT2;
		%if (%length(&project)>0) %then %do ;

			%if (%length(&group)=0) %then %do ;

			%kmplot2(data=&data,timevar=&timevar, censvar=&censvar, censval=&censval, testvar=&testvar, 
			        test=&reqtest,testdata=work.tests, project=&Project, increas=&increas,missing=&missing) ;
			%end ;

			%if (%length(&group)>0) %then %do ;

			%kmplot2(data=&data,timevar=&timevar, censvar=&censvar, censval=&censval, testvar=&group, 
			        test=&reqtest,testdata=work.tests, project=&project, increas=&increas,missing=&missing) ;
			%end ;

		%end ;

		%Forest2(summfile=work.summary,totfile=work.totals, testfile=work.tests, labfile=&labelsFP, 
		              grfnr=&outfile,diamond=,display=&display, 
		              group=&group, total=&total, 
		              cumMA=N, bonf=&bonf, blinded=&blind,
		              scale=&scale., final=&final., colour=&colour.,EndPoint=,Subtit= &Subtit.,ANYMARGIN=&ANYMARGIN., TITLE=&TITLE.);

%END;

***********************;
*    BINARY DATA     **;
***********************;

%IF (%LENGTH(&respvar.)>0) %THEN %DO;

%if (%length(&Scale)=0)%THEN %let Scale=LIN;

	* CREATES THE SUMMARY DATA SET ;
	%summaryB3(data=&data, outname=&summfile, respvar=&respvar, respval=&respval, testvar=&testvar, trial=&trial, group=&group, total=&total, litt=&literat,blind=&blind ) ;

	* FIXED EFFECT MODEL;
	%testf2(summfile=&summfile, outtest=&outtest, outtotal=&outtotal,
		           group=&group, total=&total, tren=&missing, round=&round,TRENDINDIC=&TRENDINDIC) ;

	%Forest2(summfile=work.summary,totfile=work.totals, testfile=work.tests, labfile=&labelsFP, 
	              grfnr=&outfile,diamond=,display=&display, 
	              group=&group, total=&total, 
	              cumMA=N, bonf=&bonf, blinded=&blind,
	              scale=&scale., final=&final., colour=&colour., EndPoint=&EndPoint.,Subtit= &Subtit.,ANYMARGIN=&ANYMARGIN., TITLE=&TITLE.);

%END;

********************************************************;
*    CASE OF full litterature-based meta-analysis     **;
********************************************************;

%IF &DATA=NO %THEN %DO;

** JRA, 18MAY2010, add the possibility to have a full literature-based meta-analysis without individual data 
** either for binary or time to event endpoint;

 	%if &TTEBin. = TTE %then %do;
		* CREATES THE SUMMARY DATA SET in case of TTE data from Litterature;
		%summary2 (outname=&summfile, trial=&trial, group=&group, total=&total,litt=&literat,blind=&blind ) ;
	%end;

	%if &TTEBin. = BIN %then %do;
		* CREATES THE SUMMARY DATA SET in case of binary data from Litterature;
		%summaryB3(outname=&summfile, trial=&trial, group=&group, total=&total, testvar=&testvar,litt=&literat,blind=&blind ) ;
	%end;

** JRA, 31MAY2010;
*  As FORESTB2 is integreted into FOREST2 and replaced by FOREST2, the call of FORESTB2 macro as been removed;
	%testf2(summfile=&summfile, outtest=&outtest, outtotal=&outtotal,
	group=&group, total=&total, tren=&missing, round=&round,TRENDINDIC=&TRENDINDIC) ;

	%Forest2(summfile=work.summary,totfile=work.totals, testfile=work.tests, labfile=&labelsFP, 
	grfnr=&outfile,diamond=,display=&display, 
	group=&group, total=&total, 
	cumMA=N, bonf=&bonf, blinded=&blind,
	scale=&scale., final=&final., colour=&colour.,EndPoint=&EndPoint., Subtit= &Subtit.,ANYMARGIN=&ANYMARGIN., TITLE=&TITLE.);

%END;


* RE-INITIALIZATION OF TESTV GLOBAL MACRO VARIABLE;
%LET TESTV=;

%fin: ;

%mend allfixed2 ;


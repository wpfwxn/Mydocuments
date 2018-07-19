/*************************************************************************************

CIPlot2.SAS
**********

Constructs ASCII data files which can be read by the EORTC software "GraphLib" to
create cumulative incidence curves.  (Competing risks.)

Two files are created (or appended to):

  C:\Temp\KMPlot.DAT: contains coordinates for plotting curves
  C:\Temp\KMLabels.DAT: contains labels for annotating graph

Version date   : 20 fev 2014
number         : v1.2   
Software       : SAS version 9.3
Original author: Laurence Collette / Kate Moncrieff

Modified by    : Jerome RAPION

***********************************************************************************

PARAMETERS

    Data              :  Name of the data file (Required parameter)
    TestVar           :  Variable indicating levels for which different curves are required (Required parameter)
    Timevar           :  Variable containing time to the earliest event or time patient lost to follow-up (Required parameter)
    RiskVar           :  Variable indicating patient's outcome (see notes below) (Required parameter)
    Project           :  Project name (for GraphLib) (Optional parameter)
    Censval           :  Value of &RiskVar indicating patient was censored (default=0) (Optional parameter)
    RiskVal           :  Value of &RiskVar indicating patient experienced the event of interest (default=1) (Optional parameter)
    Where             :  Where condition (Optional parameter)
    Alpha             :  Alpha for CI around Cumulative incidence function (default=0.05)  (Optional parameter)
    Output            :  Path and name of the output RTF file. (Optional parameter)
                         You may put a CIPlot macro call inside an ODS RTF block. In this case OUTPUT parameter is not needed.
                         Example: H:\mydocuments\Cuminc_results.rtf (see notes)
    Unit              :  Time unit to be used for displaying results (D or W or M or Y) 
                         By default, Unit=D (Optional parameter)
    Graytest          :  Y to perform a gray test according to TESTVAR parameter. 
						 --> In this case a variable with at least 2 levels in TESTVAR parameter is needed.
					     N (default=N) (Optional parameter)
    Pepemori          :  Y to perform a Pepe - mori test according to TESTVAR parameter. 
						 --> In this case a variable with only 2 levels in TESTVAR parameter is needed.
					     N (default=N) (Optional parameter) 				
    FineAndGrayModel  :  Y to perform a FineAndGrayModel according to the list of variables contained into ModelVars parameter. 
						 --> In this case ModelVars should contain at least one variable.
					     N (default=N)(Optional parameter)
    ModelVars         :  List of variable(s) to include into the FineAndGray model	(Optional parameter)
                         --> This parameter is required if FineAndGrayModel=Y
    Class             :  List of categorical variables (Optional parameter) (see notes)
    ROUND             :  a digit to indicate the precision of the rounding for p-value,  
                         (default=4) (Optionnal parameter) (see notes)
    StratVar          :  Stratification variable. 
						 Gray Test and or Fine and Gray models will be stratified on this variable.(Optionnal parameter)
						 (default=1) (All data in 1 stratum, if missing)						 

NOTES

    For patients experiencing more than one event, only the earliest event is considered in the analysis.

    The &TimeVar variable should contain the time of the earliest event, whether this was the event of interest 
    or a competing event.  For censored patients, the &TimeVar variable should contain the time the patient was
    lost to follow up.

    The &RiskVar variable indicates which event the patient first experienced or whether the patient was 
    censored (neither event during the follow-up period).

    By default, the macro expects &RiskVar=0 to mean the patient was censored and &RiskVar=1 to mean the
    patient experienced the event of interest.  Use the &CensVal and &RiskVal parameters to override these defaults.

    Any patients with any other value of &RiskVar (even missing) are assumed to have experienced a competing
    event first.

	The computation of variance for CIF is inspired from CUMINC macro written by M. Pintilie
	based on the delta method (see book "Competing risks, a pratical perspective" M. Pintilie, 
	WILEY (2006));

    Output parameter:
    ----------------
    You may leave Output parameter empty. In this case,
    it means that you prefer to include %CIPLOT call inside a ODS RTF block.

    ODS RTF File="C:\TEMP\TEST.rtf" STYLE=EORTCStyle3;
		%CIPlot(Data=Patient, TestVar=Trt1,TimeVar=EarliestTime, RiskVar=EarliestEvent);
    ODS RTF CLOSE;
	
    If you don't want to include %CIPLOT call inside a ODS RTF block, you may use Output parameter:
	%CIPlot(Data=Patient, TestVar=Trt1,TimeVar=EarliestTime, RiskVar=EarliestEvent,Output=h:\mydocuments\Cuminc_results.rtf);

--------------------
	CLASS and MODELVARS:
	--------------------
	Into MODELVARS parameter, you should list all variables you want to include in your model, including categorical variables.
	Into CLASS, you put the list of variable you want to be considered as categorical variables.
	All the variables put into CLASS parameter should also be included into MODELVARS parameter.
	Otherwise an ERROR message will be generated and the macro will stop.
	With CLASS parameter, it avoids the user to create himself dummy variables for a variable with more than two levels.

 	------
	ROUND:
	------
	ROUND=2 means rounding with 0.01 precision. By default, ROUND =4 with 0.0001 precision.
   
EXAMPLE
 
    %CIPlot2(Data=Patient, TestVar=Trt1, 
            TimeVar=EarliestTime, RiskVar=EarliestEvent, Project=123456, Graytest=Y,
            ROUND=2, Where=%STR(Sex='F'));

************************************************************************************

REVISIONS

29Sept2003, KM: Correction to ensure WHERE= parameter is used.
                Value assigned to WHERE= parameter added to graph title.

26June2007, JRA: 
			Add of the computation of the variance for CIF (inspired from CUMINC macro written
			by M. Pintilie).
           	Add of the computation of Confidence limits for CIF.
			Add of ALPHA parameter for the calculation Confidence limits.
			Add of OUTPUT paramter to have an ODS RTF file containing results.
			
03Aug2007, JRA: 
			Use of Proc Lifetest results with ODS OUTPUT datasets.
			Keep of number of patient at risk and survival probabilities.
			Add of UNIT paramter, to let the display of results in days, months or years.

23Aug2007, JRA: 
			Re-numbering of each level of TESTVAR, from 1 to n 
			Replacement of TESTVAR value by this number of stratum in KMPLOT.dat file for 1rst column of KMPLOT.dat.
			Without this modification it may cause some problem because values of TESTVAR are not ordered

16Oct2007, JRA: 
			Changing of "working" variables names using "__" convention so as to avoid interactions with some paramters
			during the run of the macro

27May2009, JRA: 
			Remove of colomn "Number of type ... events" in the report

27Sep2010, JRA: 
			Modification of Output parameter, to let the user save it in another place than to C:\temp\CIFresults

06Oct2010, JRA: 
			Change of programming for WHERE parameter to avoid a bug in a particular case.

15Dec2011, JRA: 
			To be compliant with results from INFERENCE macro, the Time is lagged from 1 unit;
			It means that the number of patients at risk shown are now the one at the beginning of the previous interval.
			For instance for time=2 years, it represents N patients at risk at the beginning of [731 - 1096 ] days interval;
			In other words, it represents N patients at risk at the beginning of [2 - 3 ] years interval;

31Aug2012, JRA:
			Add the possibility to perform Gray test and/or Pepemori test if TESTVAR is filled in.
			Parameter GRAYTEST=Y and Pepemori=Y parameters.
			Add the possibility to perform a Fine and Gray model: FineAndGrayModel=Y
			with ModelVars parameter which is a list of parameters to be estimated by the model.
			Add of STRATVAR parameter to perform GRAYTEST or FINEANDGRAY model stratified.
			Add of ROUND parameter to round or not p-values of these tests.
			Remove of TESTSTRING parameter as we have GRAYTEST and PEPEMORI parameters now.
 
19fev2013, JRA:
            Add of CLASS parameter to avoid the user to create himself dummy variables for a variable
            with more than two levels.
            Modification of ROUND parameter
			Instead of Y/N, put of a digit to specify the precision of rounding.
            Keep the format of RISVAR variable in the output dataset
		    Add of p stratified label on the graph and in the output document in case of stratification 

10sept2013, JRA: Change of minor things in the comments, not in the code of the macro.
26sept2013, JRA: Change of the way to manage the check of SAS version to allow Graytest, Pepemori and Fine and Gray
            also with SAS version equal or upper to 9.3.

20fev2014, JRA: Because of the addition of ALPHA parameter in FINEANDGRAY macro, change of the call of FINEANDGRAY, adding this parameter.
           This let the user to get CIs for HR with Fine and Gray model according to this alpha.

************************************************************************************/


%MACRO CIPlot2(Data, TestVar, TimeVar, RiskVar, Project,
              CensVal=0, RiskVal=1, Where=, Alpha=0.05, Output=,Unit=D,
              Graytest=N,PepeMori=N,FineAndGrayModel=N,ModelVars=,stratvar=,CLASS=,ROUND=4);


* Macro used to seperate words in several macro variables (&&COMPTE&N) ;
* from a SUITE of words (in SEP) seperated by a seperator SEP ;
* and to count the number of words (in N) - JRA, Oct2006*;

%macro rchv(SUITE=,COMPTE=,SEP= ,N=);
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
%mend rchv;


%IF %UPCASE(&Graytest)=Y or %UPCASE(&PepeMori)=Y  or %UPCASE(&FineAndGrayModel)=Y  %THEN %DO;

**26sept2013: JRA, allow GREENLIGHT=Y also if the version is upper to 9.3, not only equal to 9.3;
%LOCAL GREENLIGHT versionBIG versionSMALL;

%let versionBIG=%scan(&sysver,1,.);
%let versionSMALL=%scan(&sysver,2,.);
%put versionBIG &versionBIG versionSMALL &versionSMALL;

%if (&versionBIG=9 and &versionSMALL>=3) or &versionBIG>= 10  %then %LET GREENLIGHT=Y;
%else %LET GREENLIGHT=N;

%put GREENLIGHT &GREENLIGHT;

%END;


* Standardise parameter values and set defaults;
 ***********************************************; 

* JRA, 06OCT2010, Initialization of Where parameter when it is empty;
%if %length(&WHERE)=0 %then %let WHERE=1;

%LET Unit=%SUBSTR(%UPCASE(&Unit.),1,1);

* Unit name (for printed output); 

%LOCAL UnitName;
%IF &Unit.=D %THEN %LET UnitName=Days;
%ELSE %IF &Unit.=W %THEN %LET UnitName=Weeks;
%ELSE %IF &Unit.=M %THEN %LET UnitName=Months;
%ELSE %IF &Unit.=Y %THEN %LET UnitName=Years;


* Prepare input data set;
************************;

* JRA, 06OCT2010, Apply WHERE parameter gor any cases (if where is empty, apply where=1);
data __Data;
	set &Data.;
	if &where.;
run;


proc sort data=__Data 
          out=__Data (keep=&TestVar. &TimeVar. &RiskVar.);
by &TestVar. &TimeVar.; *JRA, 06OCT2010, deletion of WHERE clause here;
run;

data __PatientData __Summary;
set __Data;
by &TestVar. &TimeVar.;
retain totevt totint tototh ;
where &TestVar. is not missing and &TimeVar. is not missing;
if missing(&TestVar.) then do;
   put '! DELETING PATIENT WITH UNKNOWN GROUP: ' _all_ /;
   delete;
end;
else if missing(&TimeVar.) then do;
   put '! DELETING PATIENT WITH UNKNOWN TIME TO EVENT/CENSORING: ' _all_ /;
   delete;
end;
if &RiskVar.=&RiskVal. then do;
   Any=1;
   EarliestEvent=1;
end;
else if &RiskVar.=&CensVal. then do;
   Any=0;
   EarliestEvent=.;
end;
else do;
   Any=1;
   EarliestEvent=2;
end;
cenxyz=EarliestEvent;
if cenxyz=. then cenxyz=0;


if first.&TimeVar then do  ;
	freq = 1 ;
	totint = (cenxyz = 1)  ;
	tototh = (cenxyz = 2)  ;
	end ;
else do ;
	freq = freq + 1 ;
	totint = totint + (cenxyz = 1)  ;
	tototh = tototh + (cenxyz = 2)  ;
	end ;
	totevt = totint + tototh  ;
	output __PatientData  ;

if last.&TimeVar then output __Summary  ;

keep &TestVar. Any &TimeVar. EarliestEvent cenxyz tot:;
rename &TimeVar.=TimeAny;
run;

* Count patients in each group;
******************************; 

proc means data=__PatientData noprint ;
by &TestVar.;
var &TestVar. ;
output out=_nobs n=nobs ;
run ;


* Compute overall survival (regardless of which event);
******************************************************;

** JRA, SEPT2010 - remove of ODS Output because noprint option for LIFETEST was needed;
** Recomputation with outsurv=_Sany ;

ODS GRAPHICS OFF;
proc lifetest data=__PatientData outsurv=_Sany noprint ;
	time timeany*any(0) ; 
	strata &TestVar. ;
run;

%sort(data=_Sany,var=&TestVar. timeany);
%sort(data=__patientdata,var=&TestVar. timeany totevt);

data __mypatientdata1;
    set __patientdata;
	if Any=0;
	totevt=0;
	_ord=2;
run;

data __mypatientdata2;
  set __patientdata;
  if Any=1;
  _ord=1;
run;

data __mypatientdata2;
	set __mypatientdata2;
	by &TestVar. timeany totevt;
	if last.timeany;
run;

data __mypatientdata;
	set __mypatientdata1 __mypatientdata2;
run;

%sort(data=__mypatientdata,var=&TestVar. timeany _ord);
%sort(data=_Sany,var=&TestVar. timeany);

data _Sany;
merge _Sany __mypatientdata (keep=&TestVar. timeany totevt);
	by &TestVar. timeAny;
	if _n_=1 then totevt=0;
run;
%sort(data=_Sany,var=&TestVar. timeany);

data _Sany;
	merge _Sany _nobs(keep= &TestVar. nobs);
	by &TestVar.;
	if TimeAny=0 then _censor_=0;
run;

data _Sany;
set _Sany;
by &TestVar. TimeAny;
	retain f;
	if first.&TestVar. then f=0;
	else f=f+totevt;

	failed=f;
    drop f;
run;

%sort(data=_Sany,var=&TestVar. timeany);
%sort(data=__patientdata,var=&TestVar. timeany);

data __allpat;
	merge __patientdata _sany;
	by &TestVar. timeany;
	if first.&TestVar. then ind=1;
	else ind+1;
run;

data __allpat;
	set __allpat;
	left=nobs-ind+1;
	drop ind;
run;


%sort(data=__allpat,var=&TestVar. timeany);
data __risk;
	set __allpat;
	by &TestVar. timeAny;
	retain s;
	if Survival ne . then S=survival;
	else s=sum(Survival,s);

	* For each group and time point, number of patients still at risk (neither event
	  and not yet censored) : risk variable;

	risk=lag(left);
	survtm=lag(s);

	if first.&TestVar. then do;
		survtm=survival;
		risk=left;
	end;
	drop s survival;
run;

proc sort data=__risk nodupkey;
	by &TestVar. timeAny;
run;


* For each group and time point, count number of patients censored or with (earliest) event
  at this time point and number of events at this time point (either type): EVENT variable;

data __AtRisk1;
	merge __PatientData __risk;
	by &TestVar. timeAny;
	retain Np 0 __event 0 cumevent 0;

	if Any=. then Any=0;

	if first.timeany then do;
		Np=1;
		__event=any;
	end;
	else do;
		np=np+1;
		__event=__event+any;
	end;

	if (first.&TestVar.) then cumevent=any ; *** cumulative nb of events;
	else cumevent=cumevent+any ; 

	if last.TimeAny;

run;
%sort(data=__AtRisk1,var=&TestVar. timeAny);

* Count number of primary events occuring at each time point (primary events occuring
  after the patient has already experienced a competing event will not be counted.);

data __AtRisk2;
	set __PatientData (where=(EarliestEvent=1));
	by &TestVar. timeAny;
	retain _Nlp 0 ;

		if first.timeany then do;
			_Nlp=1;
		end;
		else do;
			_nlp=_nlp+1;
		end;

	if last.TimeAny;
	keep _nlp &TestVar. TimeAny;
run;
%sort(data=__AtRisk2,var=&TestVar. timeAny);

data __AtRisk;
	merge __AtRisk1 __AtRisk2;
	by &TestVar. timeAny;
	retain cumevent2 0;
	if (_nlp=.) then _nlp=0 ;

	 if (first.&TestVar.) then cumevent2=_nlp ; *** cumulative nb of events;
	 else cumevent2=cumevent2+_nlp ; 
run;


* Calculate cumulative incidence;
********************************;
data _all3 ; 
set __AtRisk ;
by &TestVar. ;
retain ci 0;
term=survtm*_nlp/risk ; ** computes Sk(t) ; 
if (first.&TestVar.) then ci=term ;
else ci=ci+term ;

*** Unit settings ;
   %IF &Unit.=D %THEN time2=TimeAny;;
   %IF &Unit.=W %THEN time2=round(TimeAny/7,0.01);;
   %IF &Unit.=M %THEN time2=round(TimeAny/30.4375,0.01);;
   %IF &Unit.=Y %THEN time2=round(TimeAny/365.25,0.01);;

	*** Computation of new ID time variable according to units times **;
	xx=floor(time2)+1;

	x1=round(time2,1);

	if x1=time2 then NewTime=xx-1;
	else NewTime=xx;
	
	if TimeAny=0 and Newtime=0 then Newtime=1;

label __event='N events (either type)'
      SurvTm='Survival estimate (either event)'
      _NLP='N primary events'
      Risk='N still at risk (neither event)' ;

keep &TestVar. TimeAny Time2 CI __event Cumevent Cumevent2 SurvTm _NLP Risk Term NewTime x:;
run ;

data _all4;
	merge _all3 __summary(where=(TimeAny>=0));
	by &TestVar. TimeAny;

	if Any=. then do;
		Any=0;
		tototh=0;
		totint=0;
		totevt=0;
	end;

	** Computation of variance of CIF based on the delta method;
	** inspired from CUMINC macro - from Melania Pintillie;
	retain sprev f2 t2 p4 p5 p1 p2 p3 v ;
	if (first.&TestVar.) then do;
		sprev=1;
		f2 = tototh / risk ; 
		t2 = (totint/risk**2)*(1-totint/risk) ;
		p1 = totevt/(risk*(risk-totevt)) ;
		p2 = ci*totevt/(risk*(risk-totevt)) ;
		p3 = (ci**2)*totevt/(risk*(risk-totevt)) ;
		p4 = totint/(risk**2) ;
		p5 = (ci*totint)/(risk**2)  ;
		v = (totint*(1-f2)**2+tototh*ci**2)/(risk*(risk-1));
	end;
	else do;
		sprev = survtm ;
		f2 = f2 + sprev*(tototh/risk) ;
		t2 = t2 + (sprev**2)*(totint/risk**2)*(1-totint/risk) ;
		p1 = p1 + totevt/(risk*(risk-totevt)) ;
		p2 = p2 + ci*totevt/(risk*(risk-totevt)) ;
		p3 = p3 + (ci**2)*totevt/(risk*(risk-totevt)) ;
		p4 = p4 + sprev*totint/(risk**2) ;
		p5 = p5 + (sprev*ci*totint)/(risk**2)  ;
		if risk>1 then
		v=v+(totint*(1-f2)**2+tototh*ci**2)/(risk*(risk-1));
		else v=v;
	end ;

	vardelta = (ci**2)*p1 - 2*ci*p2 + p3 + t2 -2*ci*p4 + 2*p5 ;
	std=vardelta**0.5;

	y=abs(quantile('NORMAL',%sysevalf(&alpha*0.5)));
	IClow=ci-y*(vardelta**0.5);
	IChigh=ci+y*(vardelta**0.5);

	ci=ci*100;
	IClow=IClow*100;
	IChigh=IChigh*100;
	std=std*100;

	if IClow<0 then IClow=0;

	%let myalpha=%CMPRES(%sysevalf(100*(1-&alpha.)));

	label NewTime="Time (&UnitName.)" ;
	label TimeAny="Time (Days)" ;
	label Risk="Number of patients at risk" ;
	label _NLP="Number of type &RiskVal. events";
	label Cumevent2="Cumulative number of type &RiskVal. events";
	label ci="Cumulative Incidence Function (%)";
	label std="Standard deviation (%) for CIF type &RiskVal. events ";
	label IClow="Lower CL (%) for CIF at &myalpha %";
	label IChigh="Upper CL (%) for CIF at &myalpha %";

run;


%sort(data=_all4,var=&TestVar. NewTime timeAny);
data _display;
	set _all4;
	by &TestVar. NewTime timeAny;
	retain event1 0;

	if first.NewTime then event1=_Nlp;
	else event1=event1+_Nlp;

	if first.NewTime;

	label event1="Number of type &RiskVal. events";

	%IF &UNIT. = D %then %do ;
		event1=_Nlp;
		x=TimeAny;
	%end;

run;

** JRA, 15DEC2011, To be compliant with results from INFERENCE macro, the Time is lagged from 1 unit;
** It means that the number of patients at risk shown are now the one at the beginning of the previous interval. ;
** For instance for time=2 years, it represents N patients at risk at the beginning of [731 - 1096 ] days interval;
** In other words, it represents N patients at risk at the beginning of [2 - 3 ] years interval;

%if &UNIT=W or &UNIT=M or &UNIT=Y %then %do;

data _display;
set _display;
NewTime=Newtime-1;
run;

%end;

%sort(data=_display,var=&TestVar. NewTime);



** To center results inside RTF file **;
proc template;
define style mystyle ;
   parent=EORTCStyle1;
	style Data from Data/
	 Just=C;
end;
run;

** JRA, Sept2010, Change of ODS RTF Statement;
%if &Output^=%STR() %THEN %DO;
	OPTIONS ORIENTATION =PORTRAIT NODATE ;
	ODS RTF FILE="&Output" STYLE=mystyle bodytitle startpage=yes;
%END;



title1 "Cumulative incidence function for type &RiskVal. events";

%if &UNIT=W or &UNIT=M or &UNIT=Y %then %do;
footnote h=8pt "Number of patients at risks computed at the beginning of the interval";


** JRA, 15DEC2011, To be compliant with results from INFERENCE macro, the Time is lagged from 1 unit;
** Change of footnotes: the Time is lagged from 1 unit;

%if &UNIT=W %then 
footnote2 h=7pt "For instance, for Time=2 weeks, it represents N patients at risk at the beginning of [15-21 days] interval";;

%if &UNIT=M %then 
footnote2 h=7pt "For instance, for Time=2 months, it represents N patients at risk at the beginning of [62-91 days] interval";;

%if &UNIT=Y %then 
footnote2 h=7pt "For instance, for Time=2 years, it represents N patients at risk at the beginning of [731- 1096 days] interval";;
%end;


proc print data=_display label noobs ;
var &TestVar. NewTime Risk CumEvent2 CI std IClow IChigh;
format CI IClow IChigh 8.2  std 8.3 ;
run; 
footnote;
title;


* Combine CI result with patient data to get data for curves (need one record per patient);
******************************************************************************************;

* Note: The CI data set needs to be padded out to contain one record per patient in order 
        to ensure that the risk set totals are calculated correctly by GraphLib.  Each patient 
        should be included in the risk set up until the time of their eariest event (whether or 
        not it is the primary event).  This is why the minimum time (TimeAny) is used.;

data __Plot;
merge __PatientData 
      _all3 (keep=&TestVar. TimeAny CI __event
             rename=(CI=xCI) in=inAll where=(__event>0));
by &TestVar. TimeAny;
;
retain CI;
if first.&TestVar. then CI=0;
if not missing(xCI) then CI=xCI;
keep &TestVar. TimeAny CI EarliestEvent;
run;



%IF %UPCASE(&Graytest)=Y or %UPCASE(&PepeMori)=Y  or %UPCASE(&FineAndGrayModel)=Y  %THEN %DO;

	%IF &GREENLIGHT=Y %THEN %DO;


		** JRA, 29AUG2012, Gray test;
		%IF %UPCASE(&Graytest.)=Y %THEN %DO;

		proc freq data=&data noprint;
		tables &testvar/out=_mytestvar;
		run;

		data _null_;
		set _mytestvar end=final;
		n+1;
		if final then call symput('NBTESTVAR1',compress(n));
		run;


			%IF &NBTESTVAR1 > 1 %THEN %DO;
				%GRAYTEST(data=&data,Timevar=&timevar,TestVar=&testvar,RiskVar=&riskvar,CensVal=&CensVal,RiskVal=&RiskVal,ROUND=&ROUND,WHERE=&WHERE,StratVar=&stratvar.);


				%if %length(&stratvar) > 0 %then %do;

					title2 "Gray test stratified by &StratVar. (for &TESTVAR.)";
					proc print data=Graytest noobs label;
					var stat pvr df risk stratif;
					label pvr="p-value";
					label stratif="Stratification variable";
				    format stat 8.4 ;
					run;
					title2;

				%end;
				%if %length(&stratvar) = 0 %then %do;

					title2 "Gray test (for &TESTVAR.)";
					proc print data=Graytest noobs label;
					var stat pvr df risk ;
					label pvr="p-value";
				    format stat 8.4 ;
					run;
					title2;

				%end;

			%END;
			%ELSE %DO;

			%PUT ;%PUT ERROR: GRAY TEST;
			%PUT IF YOU WANT TO COMPUTE GRAY TEST, A VARIABLE WITH AT LEAST 2 LEVELS IS NEEDED INTO TESTVAR PARAMETER;
			%PUT;

			%END;


		%END;

		ODS RTF STARTPAGE=NO;

		** JRA, 29AUG2012, Pepemori test;
		%IF %UPCASE(&PepeMori.)=Y %THEN %DO;

		proc freq data=&data noprint;
		tables &testvar/out=_mytestvar2;
		run;

		data _null_;
		set _mytestvar2 end=final;
		n+1;
		if final then call symput('NBTESTVAR2',compress(n));
		run;


			%IF &NBTESTVAR2 =2 %THEN %DO;
				%PEPEMORI(data=&data,Timevar=&timevar,TestVar=&testvar,RiskVar=&riskvar,CensVal=&CensVal,RiskVal=&RiskVal,ROUND=&ROUND,WHERE=&WHERE);

				title2 "Pepe-Mori test (for &TESTVAR.)";
				proc print data=PepeMori noobs label;
				var stat pvr df risk ;
				label pvr="p-value";
			    format stat 8.4 ;
				run;
				title2;
			%END;
			%ELSE %DO;

			%PUT ;%PUT ERROR: PEPEMORI TEST;
			%PUT IF YOU WANT TO COMPUTE PEPEMORI TEST, A VARIABLE WITH EXACTLY 2 LEVELS IS NEEDED INTO TESTVAR PARAMETER;
			%PUT;

			%END;

		%END;


		** JRA, 29AUG2012, Fine and Gray model;
		%IF %UPCASE(&FineAndGrayModel.) =Y %THEN %DO;

			%LOCAL FLAG;
			%IF %LENGTH(&ModelVars) >0 %THEN %DO;

				%rchv(SUITE=&ModelVars,COMPTE=MODELVAR,SEP=" ",N=NBMODEL);
				


				%do i=1 %to &NBMODEL;

				%IF %UPCASE(&TESTVAR)=%UPCASE(&&MODELVAR&I) %THEN %DO;
					%LET FLAG=1;
				%END;

				%end;

				%FINEANDGRAY(data=&data,Timevar=&timevar,ModelVars=&ModelVars,CLASS=&CLASS,RiskVar=&RiskVar,RiskVal=&RiskVal,CensVal=&CensVal,WHERE=&where,stratvar=&stratvar,ROUND=&round, ALPHA=&ALPHA);

			%END;
			%ELSE %DO;

				%PUT ;%PUT ERROR: FINE AND GRAY MODEL;
				%PUT IF YOU WANT TO COMPUTE FINE AND GRAY MODEL, AT LEAST ONE VARIABLE IS NEEDED INTO MODELVARS LIST PARAMETER;
				%PUT;


			%END;

			%IF &FLAG NE 1 AND %LENGTH(&ModelVars) >0 %THEN %DO;

				%PUT ;%PUT WARNING: FINE AND GRAY MODEL;
				%PUT YOU USED A VARIABLE INTO TESTVAR PARAMETER (&TESTVAR) WHICH DOES NOT BELONG TO THE LIST OF VARIABLES CONTAINED INTO MODELVARS PARAMETER (&MODELVARS);
				%PUT IT MAY BE AN ERROR;

			%END; 

		%END;

	%END;
	%ELSE %DO;

	%PUT;
	%PUT ERROR:;
	%PUT YOU ARE NOT USING SAS 9.3;
	%PUT A VERSION OF AT LEAST SAS 9.3 IS NEEDED TO RUN GRAY TEST, PEPE MORI TEST OR FINE AND GRAY MODEL ;
	%PUT ;


	%END;

%END;



* Write to DAT file (for curves);
********************************;

**JRA, 23Aug2007, numbering of each level of TESTVAR*;
proc sort data=__Plot out=__id nodupkey;
by &TestVar.;
run;

data __id;
set __id;
_stratum=_n_;
run;	

%sort(data=__Plot,var=&Testvar. TimeAny);
%sort(data=__id,var=&Testvar. );
data __Plot;
merge __Plot __id(keep=&Testvar. _stratum);
by &TestVar. ;
run;

data graphci;
set __Plot;
if EarliestEvent=1 then LastCol=0;
else LastCol=1;
CI=100*CI;
run;


* Write to labels file;
**********************;

filename asciidat "c:\temp\KMPlot.dat" ;

	data _null_;
	 file asciidat mod;
	 ai=-1;
	 put @1 ai  5.0 ;
	 run;

   data _null_;
	set graphci;
	file asciidat mod;
	* (GraphLib uses the final column to calculate the risk set totals.  Patients having a
	   competing event can be treated as equivalent to censored patients for these calculations.);
	**JRA, 23Aug2007, put of "numero of stratum" instead of "value of level of TESTVAR"*;
	put  @1 _stratum  5.0
	     @6 TimeAny 10.0
	     @16 CI      15.8
	     @31 LastCol  5.0;
	run;


	filename asciilab "c:\temp\KMLabels.dat" ;

	data _null_;
	 file asciilab mod;
	 ai="newGraph ======================================";
	 put ai / @1 'CI Y' ;
	 run;

	*** DATE AND TIME ;

	data _null_;
	 date=date() ;
	 time=time() ;
	 label=left(put(date,worddatx12.))||'  '||left(put(time,time5.));
	 labref="Date" ;
	 format date worddatx12. ;
	 format time time6. ;
	 file asciilab mod ;
	 put labref label ;
	 run ;

	*** TITLES AND LABELS;

	* Create data set with one record per level of test variable;
	proc sort data=__Data 
	          out=__TestLevels (keep=&TestVar. &TimeVar. &RiskVar.)
	          nodupkey;
	by &TestVar.;
	where &TestVar. is not missing;
	run;

	* Get format of test variable;
	data _null_;
	set __TestLevels;
	if _n_=1;
	call symput('TestVarFmt',trim(left(vformat(&TestVar.))));
	run;

	** JRA, 20JUN2012, gray test;
	%if &Graytest.=Y %then %do;

		%IF &NBTESTVAR1 > 1 %THEN %DO;

		%let gtf='';
			data _null_;
			set graytest;
			pvv=pvr;
			pvvv=substr(pvv,1,1);
		   * JRA, 21FEV2012, create a flag variable to distinguish if we are in a case p <x.xx or p=x.xx;
			if pvvv="<" then pvvvf=1;
			if pvvv="0" then pvvvf=0;
			call symput('gtf',left(trim(pvvvf)));
			call symput('gt',left(trim(pvv)));
			run;

			%put &gt. &gtf.;

		%END;

	%end;

	** JRA, 20JUN2012, PepeMori test;
	%if &PepeMori.=Y %then %do;

		%IF &NBTESTVAR2 =2 %THEN %DO;

		%let pmf=0;
			data _null_;
			set pepemori;
			pvv=pvr;
		   * JRA, 21FEV2012, create a flag variable to distinguish if we are in a case p <x.xx or p=x.xx;
			pvvv=substr(pvv,1,1);
			if pvvv="<" then pvvvf=1;
			if pvvv="0" then pvvvf=0;
			call symput('pmf',left(trim(pvvvf)));
			call symput('pm',left(trim(pvv)));
			run;

			%put &pm. &pmf.;

		%END;

	%end;

	data _null_;
	set __TestLevels end=LastObs;
	file asciilab mod;
	length LabRef $10 Label $50;
	format LabRef $10. Label $50.;
	LabRef='Val'||trim(left(put(_n_,2.)));
	Label=trim(left(put(&TestVar.,&TestVarFmt.)));
	put LabRef Label;
	if Lastobs then do;
	   LabRef='Tit1';
	   Label='CI of '||trim(left(vlabel(&TimeVar.)));
	   put LabRef Label;
	   LabRef='Tit2';
	   Label=trim(left(vlabel(&RiskVar.)))||" = &RiskVal.";
	   *JRA, 07OCT2010, Change of programming for WHERE parameter;
	   %IF &Where. ne 1 %THEN %STR(Label=trim(left(Label))||", %BQUOTE(&Where.)";);
	   put LabRef Label;
	   LabRef='Project';
	   Label="&Project.";
	   put LabRef Label;
	   LabRef='Strat';
	   Label=vlabel(&TestVar.);
	   put LabRef Label;

	** JRA, 20JUN2012, gray test and pepe mori test;
	   %IF &Graytest.=Y and &PepeMori.=Y %THEN %DO;

		%IF &NBTESTVAR2 =2 %THEN %DO;
	       labref="LOGRANK" ;
		   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
	       %if &pmf.=1 %then label="Pepe-Mori test: p &pm";;
	       %if &pmf.=0 %then label="Pepe-Mori test: p= &pm";;
	       put LabRef Label;
		%END;

		%IF &NBTESTVAR1 > 1 %THEN %DO;
				%if %length(&stratvar) = 0 %then %do;
			       labref="WILCOXON" ;
				   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
				   %if &gtf.=1 %then label="Gray test: p &gt";;
				   %if &gtf.=0 %then label="Gray test: p= &gt";;
				   put LabRef Label;
			   %end;
				%if %length(&stratvar) > 0 %then %do;
			       labref="WILCOXON" ;
		 		   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
				   %if &gtf.=1 %then label="Gray test: p stratified &gt";;
				   %if &gtf.=0 %then label="Gray test: pstratified= &gt";;
				   put LabRef Label;
			   %end;
		%END;

	  %END;


	   %IF &Graytest.=Y and &PepeMori.=N %THEN %DO;

		%IF &NBTESTVAR1 > 1 %THEN %DO;

				%if %length(&stratvar) = 0 %then %do;
			       labref="WILCOXON" ;
				   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
				   %if &gtf.=1 %then label="Gray test: p &gt";;
				   %if &gtf.=0 %then label="Gray test: p= &gt";;
				   put LabRef Label;
			   %end;
				%if %length(&stratvar) > 0 %then %do;
			       labref="WILCOXON" ;
				   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
				   %if &gtf.=1 %then label="Gray test: p stratified &gt";;
				   %if &gtf.=0 %then label="Gray test: p stratified= &gt";;
				   put LabRef Label;
			   %end;

		%END;

	  %END; 

	   %IF &Graytest.=N and &PepeMori.=Y %THEN %DO;

		%IF &NBTESTVAR2 =2 %THEN %DO;
	       labref="LOGRANK" ;
		   * JRA, 21FEV2012, create two cases (in case p <x.xx or p=x.xx);
	       %if &pmf.=1 %then label="Pepe-Mori test: p &pm";;
	       %if &pmf.=0 %then label="Pepe-Mori test: p= &pm";;
		   put LabRef Label;
		%END;

	  %END;

	   labref='Where';
	   *JRA, 07OCT2010, Change of programming for WHERE parameter;
	   %IF &WHERE=1 %THEN %STR(Label='All records';);
	   %ELSE %STR(label="%BQUOTE(&Where.)";);
	   put LabRef Label;
	end;
	run;

	footnote;





** JRA, Sept2010, Change of ODS RTF Statement;
 %IF &Output.^=%STR() %THEN %DO;
	%STR(ODS RTF CLOSE;);
	%clean_table(path=&Output.);
 %END;


* Clear temporary data sets;
***************************;


proc datasets nolist ;
delete _: Productlimitestimates graphci graphciFandG;
quit;




%MEND CIPlot2;

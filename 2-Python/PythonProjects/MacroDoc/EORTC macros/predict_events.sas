/*************************************************************************************

PREDICT_EVENTS.SAS
**********

To make prediction of number of events to be received in the future in the 
presence of diminishing/evolving hazard with time since randomization 

Note: an existing (evolving) database is needed for this  

Version date   : 16 JUNE 2014
number         : v2.0 (base on the original version from Jan Bogaerts) 
Software       : SAS version 9.4
Original author: Jan BOGAERTS

Modified by    : Jerome RAPION

***********************************************************************************

PARAMETERS

    DATA              :  dataset name with one observation per patient (Required parameter)
    EXTRACTDATE       :  reference date for current database, in format eg.   15mar2006  
                         if left empty, the macro will take today's date (default=TODAY())
                         it is the date for which the first predictions will be made (Optional parameter)
    TIMEVAR           :  time variable of time to event endpoint in ds dataset (Required parameter)
						-->>  MUST BE IN DAYS ! 
    CENSVAR           :  Censoring variable (Required parameter)
    CENSVAL           :  Value of CENSVAR associated to censoring (default=1) (Optional parameter)
    UNIT              :  Time unit to be used for displaying results: Y | M | D (default in years) (Optional parameter)
    BASEDATE          :  variable of starting date for each patient in ds dataset (default= dor) (Optional parameter)
    TIMEPOINTS        :  time intervals (in the time unit set up into UNIT parameter) on which 
                         piecewise exponential model will be used for the computation of hazard rates.
                         Enter any sequence of increasing time points, not including 0
                         eg.   1 1.5 2 3 5 8 (Optional parameter)
                         this parameter may be empty: in this case the prediction of number of events will
                         be based on the overall hazard.
    FUTURE_TIME       :  the program will give prognoses up to this number of (time unit) in the
                         future (default=3)  (Optional parameter)
    DROPPED           :  variable on data set that identifies patients who are believed to be unavailable
                         for updates (lost to follow-up). Such patients will be seen as never contributing
                         in the prediction. 1=drop, 0=no drop. Default: no such variable (Optional parameter) 
    DROUPOUT          :  competing risk of dropout/loss to followup applied to all future data
                         expressed in ./time unit, (default=0)(Optional parameter) 
    ALLOW             :  number of days (or months, or years, depending on UNIT) to be added in case
                         of backlog. (default =  3 months or 0.25 year or 91.3125 days) (Optional parameter) 
    MAXEVENTS         :  number of events to be reached (Required parameter)  
    OUTFILE           :  Path and name of the file to contain the output tables and the plot for predicted events
                         with rtf extension (Required parameter)  
    ACCRUALFINAL      :  Is the accrual final or not ? Y|N (default =  Y) (Optional parameter) (see notes)
    TOTALSAMPLESIZE   :  Is the accrual is not considered as final, total number of patient to be reached (Optional parameter)
						 if ACCRUALFINAL=N, this parameter becomes required and should be filled in.
    ACCRUALRATE       :  Is the accrual is not considered as final, accrualrate to be applied in the predictions
						 (expressed in time UNIT) (Optional parameter)
    ALPHA             :  Alpha for CI around hazard rate estimates (Optional parameter)
                         (in range 0.0001-0.999, default=0.05)
	
***********************************************************************************

NOTES
   - ensure that each interval has sufficient number of events to get stable results  

   ACCRUALFINAL parameter:
   If the accrual is not considered as final, and accrualrate is not filled in, an accrual rate is estimated
   on a period of 6 months from &EXTRACTDATE.

***********************************************************************************
OUTPUT

   In OUTFILE document, you will get:

   - Test for trend in risk: a phreg regression using the number of the interval as a scalar as a rough tool 
    to see if hazards are evolving with time
   - Hazard rate estimates for time periods choosen into TIMEPOINTS parameter
   - Prognosis of the number of events for primary endpoint (with or without backlog)
       - if all patients would have follow-up up to today                                
       - if all patients would have full follow-up 1, 2, 3 etc (expressed in time unit) after today (up to     
        future_time)                                                                  
   - A plot with the predicted number of events vs time
	 The horizontal red line crosses the plots at the number of events choosen in MAXEVENTS parameter.



****************************************************************************************/


options merror;

%macro predict_events(DATA=,EXTRACTDATE=,timevar=,censvar=,censval=1,unit=Y,ALLOW=0.25,basedate=dor,
TIMEPOINTS=,future_time=3,dropped=,dropout=0,maxevents=,OUTFILE=,ACCRUALFINAL=Y,TOTALSAMPLESIZE=,ACCRUALRATE=,
ALPHA=0.05);

%if &UNIT=M and &ALLOW=0.25 %then %let ALLOW=3;
%if &UNIT=D and &ALLOW=0.25 %then %let ALLOW=91;

%let alpha1=%sysevalf(100-&ALPHA*100);
%put &alpha1;

%let check=0;
%let ccheck=0;
%let today=&EXTRACTDATE;
%let ACCRUALFINAL=%UPCASE(&ACCRUALFINAL);

%if &ACCRUALFINAL=N %then %do;
%if &TOTALSAMPLESIZE < &maxevents %then %goto fin ;
%end;

** To get path from OUTFILE parameter;
%let namefile=%scan(&outfile,-1,'\');

data _null_;
a="&outfile";
b="&namefile";
c=tranwrd(a,b,"");
call symput ('path',compress(c));
run;

footnote; title;

%local today_d i;
%LET i=1;
%DO %WHILE(%SCAN(&TIMEPOINTS.,&i.,%str( )) ne %STR());
   %LOCAL time&i.;
   %LET time&i.=%SCAN(&TIMEPOINTS.,&i.,%str( ));
   %LET i=%EVAL(&i.+1);
%END;

%LET nr_times=%EVAL(&i.-1);

*************************************;
** to get the time of the last event;
*************************************;
ods listing close;
      ods output productlimitestimates=__ProductLimitEstimates;

	  title;
      proc lifetest data=&data. method=KM ;
      time &timevar.*&CENSVAR.(&censval.);
      run;
      title;
      ods output close;
ods listing;

	  data __ProductLimitEstimatesALLevent;
	  set __ProductLimitEstimates;
	  if Censor=0;
	  %if &UNIT=Y %then efsy=&timevar./365.25;;
	  %if &UNIT=M %then efsy=&timevar./30.4375;;
	  %if &UNIT=D %then efsy=&timevar.;;
	  efsyrd=round(efsy,1);
	  run;

	  data __last;
	  set __ProductLimitEstimatesALLevent;
	  by stratum efsy;
	  if last.stratum;
	  run;

	  data _null_;
	  set __last;
	  call symput('lasttime',compress(efsyrd));
	  run;

	  %put lasttime &lasttime;

data ds_copy;
set &data.(keep= &timevar. &CENSVAR. &basedate. &dropped.);
_event_ny=(&CENSVAR. ne &censval.);

%if %length(&dropped.) > 0 %then dropped= &dropped.;; 
%if %length(&dropped.) = 0 %then dropped=9;; 

timevar_day=&timevar.;

** JRA, 17-01-2014, TIME UNIT conversions;
%IF &UNIT=Y %then &timevar.=&timevar./365.25;; ** in years;
%IF &UNIT=M %then &timevar.=&timevar./30.4375;; ** in months;

** allow variable in case of backlog;
allow=&allow;

**JRA, 22-01-2014: Creation of unit_label macro variable;

%IF &UNIT=Y %then %let unit_label=years;
%IF &UNIT=M %then %let unit_label=months;
%IF &UNIT=D %then %let unit_label=days;


%if &EXTRACTDATE ne %then today_d="&EXTRACTDATE."d;
   %else %do;
     today_d=today();
    %end;
	 ;
	 call symput('today_d',put(today_d,date9.));

low0=0;

%do j=1 %to &nr_times.;
   low&j.=&&time&j.;
   %end;
%do j=0 %to %eval(&nr_times.-1);
   %let hh=%eval(&j.+1);
   high&j.=&&time&hh.;
   %end;

high&nr_times.=&lasttime.;

%do j=0 %to %eval(&nr_times.);
   mid%eval(&j)=(high&j. - low&j.)/2 + low&j.;
   %end;


check = (today_d- &basedate.)-timevar_day;

run;

	proc means data =ds_copy noprint;
	var &basedate;
	output out=_nbpat n=n;
	run;

	data _null_;
	set _nbpat ;
	call symput('nbpat',n);
	run;

%if &ACCRUALFINAL=N %then %do;

	%sort(data=ds_copy,var=&basedate)

	data _null_;
	set ds_copy ;
	call symput('lastdt',&basedate);
	call symput('lastdtfmt',put(&basedate,date9.));
	run;


	data _sixmonth;
	set ds_copy;
	if (today_d-&basedate)< 30.4375*6;
	run;

	
	data _sixmonth1;	set _sixmonth;	if _n_=1;	run;
	%sort(data=_sixmonth,var=&basedate)
	data _null_;
	set _sixmonth1 ;
	call symput('firstdt',&basedate);
	call symput('firstdtfmt',put(&basedate,date9.));
	run;

	%let period6m=%sysevalf("&today_d."d - &firstdt.);
	%put period6m &period6m.;
	
	proc means data =_sixmonth noprint;
	var &basedate;
	output out=_nbaccru n=n;
	run;

	data _null_;
	set _nbaccru ;
	call symput('nbaccru',n);
	run;

	%let accrualrateperday=%sysevalf(&nbaccru./&period6m.);

	%if %length(&accrualrate.) ne 0 %then %do;
		%if &UNIT=Y %THEN %let accrualrateperday=%sysevalf(&accrualrate./365.25); 
		%if &UNIT=M %THEN %let accrualrateperday=%sysevalf(&accrualrate./30.4375); 
		%if &UNIT=D %THEN %let accrualrateperday=%sysevalf(&accrualrate./1); 
	%end;

	%put &accrualrateperday.;

	%let future_time_d=%sysevalf(&future_time.*365.25);
	%let additionalpatientfuture_time=%sysevalf(&future_time_d.*&accrualrateperday.);
	%let totalpat=%sysevalf(&additionalpatientfuture_time.+ &nbpat.);
	%let additionaltimepat=%sysevalf((&TOTALSAMPLESIZE.- &nbpat.)/&accrualrateperday.);
	%let additionalpat=%sysevalf(&additionaltimepat.*&accrualrateperday);

	%put &additionalpat.;


	data _null_;
	z=round( &additionalpat.,1);
	call symput('additionalpatient',z);
	run;


	%let periodtime=%sysevalf(&additionaltimepat./&additionalpatient);

	data addpat;
		do i=0 to %eval(&additionalpatient-1);
		todayn="&today_d."d;
		&basedate.n=&periodtime.*i+todayn;
		&CENSVAR=&censval;
		&timevar=0;

		today=todayn;
		&basedate=&basedate.n;
		format today &basedate ddmmyy10.;
		output;
		end;

	keep &basedate &CENSVAR &timevar ;
	run;

	data addpat;
	set addpat;

		timevar_day=&timevar.;
		allow=&allow;

		%if &today ne %then today_d="&today."d;
		   %else %do;
		     today_d=today();
		    %end;
			 ;
		low0=0;

		%do j=1 %to &nr_times.;
		   low&j.=&&time&j.;
		   %end;
		%do j=0 %to %eval(&nr_times.-1);
		   %let hh=%eval(&j.+1);
		   high&j.=&&time&hh.;
		   %end;

		high&nr_times.=&lasttime.;

		%do j=0 %to %eval(&nr_times.);
  		   mid%eval(&j)=(high&j. - low&j.)/2 + low&j.;
		   %end;

		_event_ny=(&CENSVAR. ne &censval.);
		%if %length(&dropped.) > 0 %then dropped= &dropped.;; 
		%if %length(&dropped.) = 0 %then dropped=9;; 
		addpat=1;

		%do j=0 %to &future_time;

			%if &UNIT=Y %then dt&j=today_d+%sysevalf(&j*365.25);;
			%if &UNIT=M %then dt&j=today_d+%sysevalf(&j*30.4375);;
			%if &UNIT=D %then dt&j=today_d+%sysevalf(&j*1);;


			format dt&j ddmmyy10.;

		%end;

		%do j=0 %to %eval(&future_time-1);

			if &basedate >=dt&j and &basedate <dt%eval(&j+1) then _order=%eval(&j+1);

		%end;

		%do j=0 %to &future_time;
		drop dt&j;
		%end;


	run;

	data ds_augmented;
	set ds_copy addpat;
	run;

	proc freq data=addpat noprint;
	tables _order / out=index outcum;
	run;


%end;


proc means data=ds_copy min noprint;
var check ;
output out=min;
run;

data _null_;
set min;
if _STAT_="MIN" then call symput ('check',compress(check));
run;

data _check;
set ds_copy;
if check=&check;
efs_date=&basedate + timevar_day;
format efs_date today_d ddmmyy10.;
run ;

data _null_;
set _check;
call symput('efs_date',put(efs_date,ddmmyy10.));
run;

%if &check < 0 %then %goto fin ;

proc means data=ds_copy max noprint;
var &timevar. ;
output out=max;
run;

data _null_;
set max;
if _STAT_="MAX" then call symput ('maxtime',compress(&timevar.));
run;

%if &today_d= %then %let today_d=&today.;

data mid;
set ds_copy(obs=1);
i=_n_;
keep mid: ;
run;

proc transpose data=mid out=tmid prefix=mid;
run;

data tmid;
set tmid;
i=_n_-1;
run;

data _null_;
	set tmid;
	call symput('mid'||compress(i),mid1);
run;


data per_period;
set ds_copy;
%do i=0 %to %eval(&nr_times.-1);
  if &timevar. ge low&i. then do; 
     __h=&i.;
     _h=&&mid&i.;
	 _dur=min(high&i.,&timevar.)-low&i.;
	 _ev=0;
	 if (_event_ny=1) and (&timevar. lt high&i.) then _ev=1;
	 output;
	 end;
  %end;
if (&timevar. ge low&nr_times.) then do; 
   _h=&&mid&nr_times.;
   __h=&nr_times.;
   _dur=&timevar.-low&nr_times.;
   _ev=_event_ny;
   output;
   end;
run;


proc sort data=per_period;
by __h;
run;


********************************************************************************************************;
***JRA, 20-01-2014, computation of lambdas (hazards) and its CLs for each timepoints, from proc lifetest;
********************************************************************************************************;

ods select none;
proc lifetest data=ds_copy outsurv=__OutSurv method=act intervals=0 &TIMEPOINTS. &maxtime. noprint alpha=&alpha.; 
	time &timevar*&CENSVAR(&censval.) ;
run;

data __OutSurv;
set __OutSurv;
__h=_n_-1;
run;

data per_period_last;
set per_period;
by __h;
if last.__h ;
run;


******************************************************************************************;
***JRA, 20-01-2014, computation of number of events at each timepoints, from proc lifetest;
******************************************************************************************;

ods select none;
proc lifetest data=ds_copy ; 
	time &timevar*&CENSVAR(&censval.) ;
	ods output ProductLimitEstimates=ProductLimitEstimates;
run;

ods select all;

%do i=1 %to &NR_TIMES.;


data ProductLimitEstimates&i;
set ProductLimitEstimates;
if &timevar <=&&time&i;
__h=%eval(&i-1);
run;

data ProductLimitEstimates&i;
set ProductLimitEstimates&i;
by STRATUM;
if last.STRATUM;
keep __h failed;
run;

%end;

data ProductLimitEstimates%eval(&nr_times.+1);
set ProductLimitEstimates;
by STRATUM;
if last.STRATUM;
__h=%eval(&nr_times.);
keep __h failed;
run;

data d_events;
%do i=1 %to %eval(&nr_times.+1);
set  ProductLimitEstimates&i ; 
output;
%end;
run;

data d_events;
set d_events;
fail_1=lag(failed);
if fail_1=. then fail_1=0;
d_events=failed-fail_1;
run;


data _cc;
	merge per_period_last(in=a) __OutSurv d_events (keep=d_events __h);
	by __h;
	if a;
	length _h_label $30.;
	lambda=hazard;
	lambda_ucl=HAZ_UCL;
	lambda_lcl=HAZ_LCL;

	label lambda="hazard rate (&unit_label.)";

	%do i=0 %to %eval(&nr_times.-1);
	   if __h=&i. then _h_label="From "!!put(low&i.,5.2)!!" to "!!put(high&i.,5.2)!!" &unit_label.";
	   %end;
	if __h=&nr_times. then _h_label="From "!!put(low&i.,5.2)!!" &unit_label. onwards";

	call symput('lambda'!!left(put(__h,2.)),lambda);

	keep lambda: _h_label __h low: high: d_events;

run;


data _cc;
set _cc;
length _h_label $30.;
maxevents=&maxevents.; * (added by Leen) put the macro var 'maxevents' as regular variable int _cc Database for export to R ;
%do i=0 %to %eval(&nr_times.-1);
   if __h=&i. then time_from= low&i.; * added by Leen (does it work like that?), I need low&i as a numeric; 
   %end;
if __h=&nr_times. then do;
	if __h=&nr_times. then time_from= low&nr_times.; * added by Leen (does it work like that?), I need low&i as a numeric;
	end;

label d_events="Nb of*events";
label lambda_lcl="&alpha1.%*LCL";
label lambda_ucl="&alpha1.%*UCL";
label _h_label="Time periods";


dropout=&dropout.;
backlog=&allow.;
run;

data __xcc;
set _cc;
run;


*** calculate extra duration that is potentially reached ***;
data xyz;
%if &ACCRUALFINAL=Y %then %do;
set ds_copy;;
%end;
%if &ACCRUALFINAL=N %then %do;
set Ds_augmented;;
%end;
length prognosis $45.;
_order=-1;
rest=&timevar.;
prognosis='Current';
output;


_order=0;

%IF &UNIT=Y %then rest=(today_d-&basedate.)/365.25;;
%IF &UNIT=M %then rest=(today_d-&basedate.)/30.4375;;
%IF &UNIT=D %then rest=(today_d-&basedate.)/1;;

prognosis="Full FUP to &today_d.";
output;


do i=1 to &future_time.;
   _order=i;
   rest=rest+1;
   prognosis="Full FUP to &today_d. +"!!trim(left(put(i,4.)))!!" &unit_label.";
   output;
   end;
run;


*** calculate piecewise exponential probability of an event in the extra duration 
       for those that are currently censored ***;

** JRA, 21-01-2014, without taking into account backlog;

data xyz1;
set xyz;
   *** extra time added in each interval ***;
if (_event_ny=0) %if &dropped. ne %then and (&dropped. ne 1); then do;
   %do j=0 %to %eval(&nr_times.-1);
      t&j.=max(0,min(high&j.,rest)-max(low&j.,&timevar.));
	  %end;
   t&nr_times.=max(0,rest-max(low&nr_times.,&timevar.));
   *** probability of event in extra interval ***;
   _event_ny=_event_ny
             +(1-exp(-(&lambda0.+&dropout.)*t0))*&lambda0./(&lambda0.+&dropout.)
			 %do j=1 %to &nr_times.;
			    +
				%do k=0 %to %eval(&j.-1);
				   exp(-(&&lambda&k.+&dropout.)*t&k.)*
                   %end;
                (1-exp(-(&&lambda&j.+&dropout.)*t&j.))*&&lambda&j./(&&lambda&j.+&dropout.)
				%end;
				;
   end;
run;


proc sort data=xyz1;
by _order;
run;

data xyzlast1;
set xyz1;
by _order;
retain predicted_events;
if first._order then predicted_events=0;
predicted_events=predicted_events+_event_ny;
if last._order then output;
run;

***********************************************;
** JRA, 21-01-2014, taking into account backlog;
***********************************************;

** JRA, 21-01-2014, duplication of XYZ taking into account backlog;
data xyz2;
set xyz;
rest2=rest;
if _order >= 0 then rest2 =rest-allow  ;
run;

*** calculate piecewise exponential probability of an event in the extra duration 
       for those that are currently censored ***;
data xyz2;
set xyz2;
   *** extra time added in each interval ***;
if (_event_ny=0) %if &dropped. ne %then and (&dropped. ne 1); then do;
   %do j=0 %to %eval(&nr_times.-1);
      t&j.=max(0,min(high&j.,rest2)-max(low&j.,&timevar.));
	  %end;
   t&nr_times.=max(0,rest2-max(low&nr_times.,&timevar.));
   *** probability of event in extra interval ***;
   _event_ny=_event_ny
             +(1-exp(-(&lambda0.+&dropout.)*t0))*&lambda0./(&lambda0.+&dropout.)
			 %do j=1 %to &nr_times.;
			    +
				%do k=0 %to %eval(&j.-1);
				   exp(-(&&lambda&k.+&dropout.)*t&k.)*
                   %end;
                (1-exp(-(&&lambda&j.+&dropout.)*t&j.))*&&lambda&j./(&&lambda&j.+&dropout.)
				%end;
				;
   end;
run;


proc sort data=xyz2;
by _order;
run;

data xyzlast2;
set xyz2;
by _order;
retain predicted_events2;
if first._order then predicted_events2=0;
predicted_events2=predicted_events2+_event_ny;
if last._order then output;

keep _order predicted_events2;
run;

** JRA, 21-01-2014, merge of the 2 output datasets with and without backlog;

%IF &ACCRUALFINAL=N %THEN %DO;

%sort(data=index,var=_order);

	data xyzlast3;
		merge xyzlast1 xyzlast2 index(keep=_order CUM_FREQ);
		by _order;
		retain nbpat;
		if CUM_FREQ=. and i=. then CUM_FREQ=0;
		if CUM_FREQ ne . then nbpat=&nbpat+CUM_FREQ;

		_predicted_events=floor(predicted_events);
		_predicted_events2=floor(predicted_events2);

	run;
%END;

%IF &ACCRUALFINAL=Y %THEN %DO;

	data xyzlast3;
		merge xyzlast1 xyzlast2 ;
		by _order;
		nbpat=&nbpat;
		_predicted_events=floor(predicted_events);
		_predicted_events2=floor(predicted_events2);
	run;
%END;

data _null_;
set xyzlast3;
where _order=0;
min=min(predicted_events,predicted_events2);
call symput('checkpredictions',compress(min));
run;



* added by Leen;
data ds_short;
set ds_copy;

%IF &UNIT=Y %then remaining_till_today = (today_d - (&basedate. + &timevar.*365.25))/365.25;; * lambda & timevar in years;
%IF &UNIT=M %then remaining_till_today = (today_d - (&basedate. + &timevar.*30.4375))/30.4375;; * lambda & timevar in months;
%IF &UNIT=D %then remaining_till_today = (today_d - (&basedate. + &timevar.));; * lambda & timevar in months;

timevar=&timevar.;

ACCRUALFINAL="&ACCRUALFINAL";

%if &ACCRUALFINAL=N %then %do;

accrualrateperday=&accrualrateperday;
TOTALSAMPLESIZE=&TOTALSAMPLESIZE;

%IF &UNIT=D %then accrualrate=accrualrateperday;;
%IF &UNIT=M %then accrualrate=accrualrateperday*30.4375;;
%IF &UNIT=Y %then accrualrate=accrualrateperday*365.25;;


%if %length(&accrualrate.) ne 0 %then accrualrate=&accrualrate.;; 


%end;

run;

data _null_;
a=round(&checkpredictions,1);
call symput ('ccheck',compress(a));
run;

%if &ccheck > &maxevents %then %goto fin ;
%IF &ACCRUALFINAL=Y and %length(&TOTALSAMPLESIZE.) ne 0  %THEN %goto fin ;
%IF &ACCRUALFINAL=Y and %length(&ACCRUALRATE.) ne 0  %THEN %goto fin ;
%IF &ACCRUALFINAL=N and %length(&TOTALSAMPLESIZE.) = 0  %THEN %goto fin ;

%if &ccheck <= &maxevents %then %do;

OPTIONS NONUMBER NODATE;
ODS RTF FILE="&OUTFILE" STYLE=EORTCStyle1 bodytitle startpage=no;


%if %length(&TIMEPOINTS.) > 0 %then %do; 



proc template;                                                                
   define table Stat.Phreg.ParameterEstimates ;     
      notes "Parameter Estimates Table";                                      
      dynamic Confidence NRows;                                               
      column Parameter GenericClassValue DF Estimate StdErr StdErrRatio ChiSq 
         ProbChiSq  HRLowerCL HRUpperCL HRLowerPLCL HRUpperPLCL    
         Label Lower Upper;                                                   
      header h1 h2 h3;                                                        
                                                                              
      define h1;                                                              
         text "Analysis of Maximum Likelihood Estimates";                     
         space = 1;                                                           
         spill_margin;                                                        
      end;                                                                    
                                                                              
      define h2;                                                              
         text Confidence BEST8. %nrstr("%% Hazard Ratio Confidence Limits");  
         space = 0;                                                           
         end = HRUpperCL;                                                     
         start = HRLowerCL;                                                   
         spill_margin = OFF;                                                  
      end;                                                                    
                                                                              
      define h3;                                                              
         text Confidence BEST8.                                               
            %nrstr("%% Hazard Ratio Profile Likelihood Confidence Limits");   
         space = 0;                                                           
         end = HRUpperPLCL;                                                   
         start = HRLowerPLCL;                                                 
         spill_margin = OFF;                                                  
      end;                                                                    
                                                                              
      define Parameter;                                                       
         header = "Parameter";                                                
         style = RowHeader;                                                   
         id;                                                                  
      end;                                                                    
                                                                              
      define GenericClassValue;                                               
         header = " ";                                                        
         pre_space = 1;                                                       
         style = RowHeader;                                                   
         id;                                                                  
         generic;                                                             
      end;                                                                    
                                                                              
      define DF;                                                              
         parent = Common.ParameterEstimates.DF;                               
      end;                                                                    
                                                                              
      define Estimate;                                                        
         header = ";Parameter;Estimate;";                                     
         format = D10.;                                                       
         parent = Common.ParameterEstimates.Estimate;                         
      end;                                                                    
                                                                              
      define StdErr;                                                          
         header = ";Standard;Error;";                                         
         format = D10.;                                                       
         parent = Common.ParameterEstimates.StdErr;                           
      end;                                                                    
                                                                              
      define StdErrRatio;                                                     
         header = ";StdErr;Ratio;";                                           
         format = 6.3;                                                        
      end;                                                                    
                                                                              
      define ChiSq;                                                           
         parent = Stat.Phreg.ChiSq;                                           
      end;                                                                    
                                                                              
      define ProbChiSq;                                                       
         parent = Stat.Phreg.ProbChiSq;                                       
      end;                                                                    
                                                                              
                                                                  
                                                                              
      define HRLowerCL;                                                       
         glue = 2;                                                            
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define HRUpperCL;                                                       
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define HRLowerPLCL;                                                     
         glue = 2;                                                            
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define HRUpperPLCL;                                                     
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define Lower;                                                           
         glue = 2;                                                            
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define Upper;                                                           
         format = 8.3;                                                        
         print_headers = OFF;                                                 
      end;                                                                    
                                                                              
      define Label;                                                           
         header = "Label";                                                    
      end;                                                                    
      col_space_max = 4;                                                      
      col_space_min = 1;                                                      
      required_space = NRows;                                                 
   end;                                                                       
run;


ods select ParameterEstimates;

proc phreg data=per_period ;
model _dur*_ev(0)=_h;
title "Test for trend in risk";
footnote h=8pt "NOTE: It tests if the hazard rate is constant or not over the specified timepoints";
run;

title;
footnote;

proc template;
	delete Stat.Phreg.ParameterEstimates;
run;


%end;



ods listing;
proc print data=_cc noobs label sumlabel="Total" split='*' ;

%if &TIMEPOINTS. ne %then %do;
title"Hazard rate estimates for time periods &TIMEPOINTS. &unit_label.";
%end;

%if &TIMEPOINTS. = %then %do;
title"Hazard rate estimates ";
%end;

var _h_label / style={font_size=1.3 cellwidth=250} ;
var  d_events lambda lambda_lcl lambda_ucl;

sum d_events/style =[just=l];
run;

	data _cc;
	set __xcc;
	run;

	PROC IML;

	RUN ExportDataSetToR("_CC","Hazard_rates" );
	RUN ExportDataSetToR("ds_short","data_timevar" );

	** Call or R program ;
	%include "K:\SAS\EORTC macros\R code\R code for predict events.sas";


	RUN ImportDataSetFromR("WORK.maxevents_t","maxevents_solution");
	QUIT;


	data Maxevents_t_lambda;
	set Maxevents_t;
	run;


	** Call or R program - UCL lambda part ;
	data _cc;
	set __xcc;
	lambda=lambda_ucl;
	run;

	PROC IML;

	RUN ExportDataSetToR("_CC","Hazard_rates" );
	RUN ExportDataSetToR("ds_short","data_timevar" );

	%include "K:\SAS\EORTC macros\R code\R code for predict events.sas";


	RUN ImportDataSetFromR("WORK.maxevents_t","maxevents_solution");
	QUIT;

	data Maxevents_t_ucl;
	set Maxevents_t;
	extra_time_plot_ucl=extra_time_plot;
	maxevent_extra_t_ucl=maxevent_extra_t;
	maxevent_extra_tb_ucl=maxevent_extra_tb;
	keep extra_time_plot_ucl total_events_b total_events maxevent_extra_t_ucl maxevent_extra_tb_ucl;
	run;

	** Call or R program - LCL lambda part ;
	data _cc;
	set __xcc;
	lambda=lambda_lcl;
	run;

	PROC IML;

	RUN ExportDataSetToR("_CC","Hazard_rates" );
	RUN ExportDataSetToR("ds_short","data_timevar" );

	** Call or R program ;
	%include "K:\SAS\EORTC macros\R code\R code for predict events.sas";


	RUN ImportDataSetFromR("WORK.maxevents_t","maxevents_solution");
	QUIT;

	data Maxevents_t_lcl;
	set Maxevents_t;
	extra_time_plot_lcl=extra_time_plot;
	maxevent_extra_t_lcl=maxevent_extra_t;
	maxevent_extra_tb_lcl=maxevent_extra_tb;
	keep extra_time_plot_lcl total_events_b total_events maxevent_extra_t_lcl maxevent_extra_tb_lcl;
	run;


	%sort(data=Maxevents_t_lambda,var=total_events);
	%sort(data=Maxevents_t_ucl,var=total_events);
	%sort(data=Maxevents_t_lcl,var=total_events);
	data Maxevents_t;
		merge Maxevents_t_lambda Maxevents_t_ucl Maxevents_t_lcl;
		by total_events;
	run;

	data Maxevents_t;
	set Maxevents_t;
		%IF &UNIT=Y %THEN %DO;
			extra_time_plot_day=extra_time_plot*365.25;
			extra_time_plot_day_ucl=extra_time_plot_ucl*365.25;
			extra_time_plot_day_lcl=extra_time_plot_lcl*365.25;

			maxevent_extra_t_day=maxevent_extra_t*365.25;
			maxevent_extra_tb_day=maxevent_extra_tb*365.25;

			maxevent_extra_t_lcl_day=maxevent_extra_t_lcl*365.25;
			maxevent_extra_tb_lcl_day=maxevent_extra_tb_lcl*365.25;

			maxevent_extra_t_ucl_day=maxevent_extra_t_ucl*365.25;
			maxevent_extra_tb_ucl_day=maxevent_extra_tb_ucl*365.25;

		%END;

		%IF &UNIT=M %THEN %DO;
			extra_time_plot_day=extra_time_plot*30.4375;
			extra_time_plot_day_ucl=extra_time_plot_ucl*30.4375;
			extra_time_plot_day_lcl=extra_time_plot_lcl*30.4375;

			maxevent_extra_t_day=maxevent_extra_t*30.4375;
			maxevent_extra_tb_day=maxevent_extra_tb*30.4375;

			maxevent_extra_t_lcl_day=maxevent_extra_t_lcl*30.4375;
			maxevent_extra_tb_lcl_day=maxevent_extra_tb_lcl*30.4375;

			maxevent_extra_t_ucl_day=maxevent_extra_t_ucl*30.4375;
			maxevent_extra_tb_ucl_day=maxevent_extra_tb_ucl*30.4375;

		%END;

	%if &today ne %then %do;today="&today."d;;%end;
	   %else %do;today=today();%end;;

		maxevent_date=today+maxevent_extra_t_day;
		maxevent_date_b=today+maxevent_extra_tb_day;


		maxevent_date_ucl=today+maxevent_extra_t_lcl_day;
		maxevent_date_b_ucl=today+maxevent_extra_tb_lcl_day;

		maxevent_date_lcl=today+maxevent_extra_t_ucl_day;
		maxevent_date_b_lcl=today+maxevent_extra_tb_ucl_day;

		extra_time_date=today+extra_time_plot_day;
		extra_time_date_ucl=today+extra_time_plot_day_ucl;
		extra_time_date_lcl=today+extra_time_plot_day_lcl;

		format today maxevent_date: extra_time_date: ddmmyy10.;
	run;

	
	proc means data=Maxevents_t noprint;
		var total_events total_events_b extra_time_plot extra_time_date;
		output out=_ev ;
	run;

	data _ev;
	set _ev;
		if _stat_='MIN' then min=min(total_events,total_events_b);
		if _stat_='MAX' then max=max(total_events,total_events_b);
		if _stat_='MAX' then maxtime=max(extra_time_plot);

		if _stat_='MIN' then mindate=extra_time_date;
		if _stat_='MAX' then maxdate=extra_time_date;

	run;

	data _null_;
	set _ev;
	if _stat_='MIN' then call symput('min_ev',min);
	if _stat_='MAX' then call symput('max_ev',max);
	if _stat_='MAX' then call symput('max_time',maxtime);
	if _stat_='MIN' then call symput('mindate',mindate);
	if _stat_='MAX' then call symput('maxdate',maxdate);
	run;

	data __ev;
		set _ev;
		_min=&min_ev;
		_max=&max_ev;
		_maxtime=&max_time;

		mindate=&mindate-60;
		maxdate=&maxdate+60;

	%if &today ne %then %do;today="&today."d;;%end;
	   %else %do;today=today();%end;;

		min=int(_min)-3;
		max=round(_max+1,1)+3;
		maxtime=round(_maxtime,1);
		range=max-min;
		_step=range*0.1;

		if _step >= 5 then step=round(_step,5);
		if _step < 5 then step=round(_step,1);

		rangedate=maxdate-mindate;

		tick0=((today-mindate)/rangedate*100)+0.1;

		format today ddmmyy10.;

	run;

	data _null_;
	set __ev;
		call symput ('minscale',compress(min));
		call symput ('maxscale',compress(max));
		call symput ('rangescale',compress(step));
		call symput ('maxtime',compress(maxtime));
		call symput ('mindatescale',compress(mindate));
		call symput ('maxdatescale',compress(maxdate));
	run;

	data __ev;
	set __ev;

		%do i=1  %to &maxtime;
			max&i=&i*365.25 ;

			tick&i=((today-mindate+max&i)/rangedate*100)+0.1;

		%end;

	run;

	data _null_;
	set __ev;
		%do i=0  %to &maxtime;
			call SYMPUT('tick'||compress(&i),compress(tick&i));
		%end;
	run;

	data Maxevents_t;
	set Maxevents_t;

		%do i=1 %to &maxtime.;
		today&i=today+365.25*&i;
		%end;

	format today: ddmmyy10.;
	run;


	data _null_;
	set Maxevents_t;
	where maxevent_date ne .;
	call symput ('maxevent_date',put(maxevent_date,ddmmyy10.));
	call symput ('maxevent_datenb',compress(maxevent_date));



	call symput ('maxevent_date_b',put(maxevent_date_b,ddmmyy10.));
	call symput ('maxevent_date_bnb',compress(maxevent_date_b));

		call symput ('today0',compress(today));

		%do i=1 %to &maxtime;
			call SYMPUT('today'||compress(&i),compress(today&i));
		%end;

	run;

	data _null_;
	set Maxevents_t;
	where maxevent_date_lcl ne .;
	call symput ('maxevent_date_lcl',put(maxevent_date_lcl,ddmmyy10.));
	call symput ('maxevent_date_lcl_nb',compress(maxevent_date_lcl));

	call symput ('maxevent_date_b_lcl',put(maxevent_date_b_lcl,ddmmyy10.));
	call symput ('maxevent_date_b_lcl_nb',compress(maxevent_date_b_lcl));

	run;

	data _null_;
	set Maxevents_t;
	where maxevent_date_ucl ne .;
	call symput ('maxevent_date_ucl',put(maxevent_date_ucl,ddmmyy10.));
	call symput ('maxevent_date_ucl_nb',compress(maxevent_date_ucl));

	call symput ('maxevent_date_b_ucl',put(maxevent_date_b_ucl,ddmmyy10.));
	call symput ('maxevent_date_b_ucl_nb',compress(maxevent_date_b_ucl));

	run;


	%put maxevent_date &maxevent_date maxevent_datenb &maxevent_datenb;
	%put maxevent_date_b &maxevent_date_b maxevent_date_bnb &maxevent_date_bnb;

	%put maxevent_date_lcl &maxevent_date_lcl maxevent_date_b_lcl &maxevent_date_b_lcl;
	%put maxevent_date_ucl &maxevent_date_ucl maxevent_date_b_ucl &maxevent_date_b_ucl;


	data today;
	%if &today ne %then %do; today="&today."d;; %end;
	%else %do; today=today();;%end;
	format today date9.;
	run;

	data _null_;
		set today;
		call symput('today',put(today,date9.));
	run;

	data __Annotate;
	function = "text";
	anchor = "BOTTOMLEFT";
	x1 = 0.5;
	y1= 0.5;
	width=150;
	textsize = 6;
	label = "Today: &today.";
	run;


%if &allow >0 %then %do;

	footnote1 height=12pt "PREDICT_EVENTS: &MAXEVENTS events will be observed around the &maxevent_date. [&maxevent_date_lcl ; &maxevent_date_ucl]";
	footnote2 height=8pt "(with assumed backlog of &ALLOW &unit_label: &MAXEVENTS events will be observed around the &maxevent_date_b. [&maxevent_date_b_lcl ; &maxevent_date_b_ucl])";

	proc print data=xyzlast3 noobs label split='*';
		var prognosis  ;
		var  _predicted_events _predicted_events2/style={just=c}  ;
		var nbpat / style={cellwidth=150 just=c} ;

		where _order>=0;
		title"Predicted number of events";

		label _predicted_events = "predicted events*without backlog";
		label _predicted_events2 = "predicted events*with assumed backlog*of &ALLOW &unit_label";

		%if &ACCRUALFINAL=N %then label nbpat="Expected number of patients";;
		%if &ACCRUALFINAL=Y %then label nbpat="Accrued number of patients";;

	run;

	footnote;

%end;

%if &allow =0 %then %do;

	footnote1 height=12pt "PREDICT_EVENTS: &MAXEVENTS events will be observed around the &maxevent_date. [&maxevent_date_lcl ; &maxevent_date_ucl]";

	proc print data=xyzlast3 noobs label split='*';
		var prognosis  ;
		var  _predicted_events/ style={just=c}  ;
		var nbpat / style={cellwidth=150 just=c} ;
		where _order>=0;
		title"Predicted number of events";

		label _predicted_events = "predicted events";
		%if &ACCRUALFINAL=N %then label nbpat="Expected number of patients";;
		%if &ACCRUALFINAL=Y %then label nbpat="Accrued number of patients";;

	run;

	footnote;

%end;


ods graphics on/OUTPUTFMT=png imagename="predict_events" reset=index
	width=700px height=540px ;

	ods listing gpath="c:\temp" sge=off style=MyStyleDefault ;

	title;
	proc sgplot data=Maxevents_t NOAUTOLEGEND
	sganno=__Annotate 
;

%if &allow >0 %then %do;
	  series x=extra_time_date  y=total_events /LINEATTRS=(color=blue pattern=solid THICKNESS= 1pt ) name="series1"
	  legendlabel="Predicted nb. of events without backlog" 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_ucl  y=total_events /LINEATTRS=(color=blue pattern=dot THICKNESS= 0.3pt) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_lcl  y=total_events /LINEATTRS=(color=blue pattern=dot THICKNESS= 0.3pt) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;

	  series x=extra_time_date  y=total_events_b /LINEATTRS=(color=green pattern=solid THICKNESS= 1pt) name="series2"
	  legendlabel="Predicted nb. of events with backlog" 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_ucl  y=total_events_b /LINEATTRS=(color=green pattern=dot THICKNESS= 0.3pt ) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_lcl  y=total_events_b /LINEATTRS=(color=green pattern=dot THICKNESS= 0.3pt ) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;


%end;
%if &allow =0 %then %do;
	  series x=extra_time_date  y=total_events /LINEATTRS=(color=blue pattern=solid THICKNESS= 1pt ) name="series1"
	  legendlabel="Predicted nb. of events without backlog" 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_ucl  y=total_events /LINEATTRS=(color=blue pattern=dot THICKNESS= 0.3pt) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
	  series x=extra_time_date_lcl  y=total_events /LINEATTRS=(color=blue pattern=dot THICKNESS= 0.3pt) 
		DATALABELATTRS=(Family=Arial Size=4pt ) ;
%end;

		refline &MAXEVENTS /LINEATTRS=(pattern=shortdash color=red THICKNESS = 1) axis=Y;
		refline &maxevent_datenb. /LINEATTRS=(pattern=shortdash color=red THICKNESS = 1) axis=X;

%if &allow >0 %then %do;
		refline &maxevent_date_bnb. /LINEATTRS=(pattern=shortdash color=red THICKNESS = 1) axis=X;
%end;

		label extra_time_date="Date";
		label total_events="Predicted number of events";

	 XAXIS  grid MINOR ; 
 
	%if &rangescale <= 15 %then %do;

	 YAXIS DISCRETEORDER=DATA 
		VALUES=(&minscale to &maxscale by &rangescale)  MINOR MINORCOUNT=%eval(&rangescale-1)
		grid ;

	%end;


	%if &rangescale > 15 %then %do;

	 YAXIS DISCRETEORDER=DATA 
		VALUES=(&minscale to &maxscale by &rangescale)  MINOR MINORCOUNT=14
		grid ;

	%end;

	%if &allow >0 %then %do;
	 	keylegend "series1" "series2"  / location=outside noborder 
		valueattrs=(Color=black Family=Arial Size=8);
	%end;

	run;

    ods listing gpath=none sge=off;
	ods graphics off;



%end; ** end if checkpredictions < maxevents;

ODS RTF CLOSE;

**JRA, 13032014, Deletion of LIFETEST title;
ods listing;
ods select all;

	filename path "&outfile.";

	data t;
		infile path LRECL=35000 firstobs=1 TRUNCOVER; 
		input word $ 1-32000;
		word=tranwrd(word,"The LIFETEST Procedure"," ");

	run;

	data _null_;
	set t;
	file path;
	put word; 
	run;

   proc datasets library=work nolist;
   delete t;
   quit;




	%fin: ;

%if &check < 0 %then %do;

	%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
	%PUT The last follow-up date (&efs_date) is more recent than the extraction date (&today);
	%PUT This is not possible;
	%PUT The macro stops because of this error;


%end;

%if &ccheck > &maxevents %then %do;

	%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
	%PUT The total number of events [MAXEVENTS= &MAXEVENTS] is estimated to be already reached at the extraction date (&today) : nb events= &ccheck. (taking into account backlog);
	%PUT Please check MAXEVENTS parameter;
	%PUT The macro stops because of this error;


%end;


%IF &ACCRUALFINAL=N %THEN %DO;

	%if &TOTALSAMPLESIZE < &maxevents and %length(&TOTALSAMPLESIZE.) ne 0 %then %do;

		%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
		%PUT TOTALSAMPLESIZE parameter is smaller than MAXEVENTS parameter;
		%PUT This is not possible;
		%PUT The macro stops because of this error;

	%end;
%END;

%IF &ACCRUALFINAL=Y and %length(&TOTALSAMPLESIZE.) ne 0  %THEN %DO;

		%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
		%PUT TOTALSAMPLESIZE parameter should NOT be filled in if the ACCRUAL is considered as FINAL;
		%PUT The macro stops because of this error;

%END;


%IF &ACCRUALFINAL=Y and %length(&ACCRUALRATE.) ne 0  %THEN %DO;

		%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
		%PUT ACCRUALRATE parameter should NOT be filled in if the ACCRUAL is considered as FINAL;
		%PUT The macro stops because of this error;

%END;


%IF &ACCRUALFINAL=N and %length(&TOTALSAMPLESIZE.) = 0  %THEN %DO;

		%PUT ;%PUT ERROR: PREDICT_EVENTS macro;
		%PUT TOTALSAMPLESIZE parameter should be filled in if the ACCRUAL is NOT considered as FINAL;
		%PUT The macro stops because of this error;

%END;


proc datasets nolist;
	delete ds_copy ds_short ds_augmented addpat d_events index min mid tmid
	max maxevents: per_period: Productlimitestimates: today xyz: _: _ParameterEstimates:
		;
quit;

%mend predict_events;


/*************************************************************************************

FINEANDGRAY.SAS
**********

Computes the Fine and Gray model: performs regression modelling of subdistribution functions in competing risk data.

One SAS dataset is created in WORK library: FINEANDGRAYMODEL dataset (or FINEANDGRAYSTRATIF dataset if stratified).
with Parameter, Estimate, Hazard ratio, Standard error, 2.5% lower confidence limit, 97.5% upper confidence limit,
p-value and p-value rounded. 

Version date   : 15 october 2012
Software       : SAS version 9.3
Original author: Jerome RAPION
version: 2.0

Ref: 
-----
Fine JP and Gray RJ (1999) - A proportional Hazards Model for the subdistribution of a Competing Risk, 
Journal of the American Statistical Association, VOL. 94, No.446 pp. 496-509.
Zhou B., Latouche A., Rocha V. and Fine J. (2011) - Competing Risks Regression for Stratified Data, 
Biometrics, VOL. 67 (2), pp. 661-670.
BOOK from Melania Pintilie, Competing risk, A pratical perspective, WILEY (Statistics in practise) (2004) 

***********************************************************************************

 REVISIONS:
 ---------
 Sept 2012     Modification of the macro to avoid problem with case sensitivity with variable names
               put in the list MODELVARS. It means that from now this list is not case sensitive anymore.

 February 2013 Add of CLASS parameter to avoid the user to create himself dummy variables for a variable
               with more than two levels.
               Modification of ROUND parameter
	       Instead of Y/N, put of a digit to specify the precision of rounding.

 February 2014 JRA, Add of ALPHA parameter and computation of CI for HR according to this alpha

               JRA, Computation of results using PROC PHREG and SAS code instead of using R code.
               This new macro is based on the code of pshreg.sas macro written by Georg Heinze, 
               Medical University of Vienna, Center of Medical Statistics, AUSTRIA.

			   see Paper: 
				PSHREG A SAS macro for proportional and nonproportional substribution hazards regression 
				with competing risk data
			   (saved in J:\UNIT\Stat\7. SAS and R\SAS TRAINING\Papers\COMPETING RISK)

			   see original macro code in http://cemsiis.meduniwien.ac.at/en/kb/science-research/software/statistical-software/pshreg/                

***********************************************************************************

PARAMETERS

    Data              :  Name of the data file (Required parameter)
    Timevar           :  Name of the time variable (Required parameter)
    RiskVar           :  Variable indicating patient's outcome (Required parameter) (see notes below)
    ModelVars         :  List of all variable(s) to include into the model (Required parameter)
    Class             :  List of categorical variables (Required parameter) (see notes)
    Censval           :  Value of &RiskVar indicating patient was censored (Optionnal parameter) (default=0)
    RiskVal           :  Value of &RiskVar indicating patient experienced the event of interest (Optionnal parameter)
						(default=1)
    StratVar          :  Stratification variable. Tests will be stratified on this variable.(Optionnal parameter)
						 (default=1) (All data in 1 stratum, if missing)						 
    Where             :  Where condition  (Optionnal parameter)
    ROUND             :  a digit to indicate the precision of the rounding for p-value,  
                         (default=4) (Optionnal parameter) (see notes)
    Alpha             :  Alpha for CI around hazard ratio (default=0.05)  (Optional parameter)
                     

NOTES

    For patients experiencing more than one event, only the earliest event is considered in the analysis (code 1).

    The &TimeVar variable should contain the time of the earliest event, whether this was the event of interest 
    or a competing event.  For censored patients, the &TimeVar variable should contain the time the patient was
    lost to follow up.

    The &RiskVar variable indicates which event the patient first experienced or whether the patient was 
    censored (neither event during the follow-up period).

    By default, the macro expects &RiskVar=0 to mean the patient was censored and &RiskVar=1 to mean the
    patient experienced the event of interest.  Use the &CensVal and &RiskVal parameters to override these defaults.

    Any patients with any other value of &RiskVar (even missing) are assumed to have experienced a competing
    event first.

	--------------------
	CLASS and MODELVARS:
	--------------------
	Into MODELVARS parameter, you should list all variables you want to include in your model, including categorical variables.
	Into CLASS, you put the list of variable you want to be considered as categorical variables.
	All the variables put into CLASSVARS parameter should also be included into MODELVARS parameter.
	Otherwise an ERROR message will be generated and the macro will stop.
	With CLASS parameter, it avoids the user to create himself dummy variables for a variable with more than two levels.
    
	------
	ROUND:
	------
	ROUND=2 means rounding with 0.01 precision. By default, ROUND =4 with 0.0001 precision.

EXAMPLE
 
    %FINEANDGRAY(Data=Patient, ModelVars=Trt1 Age1 Age2 Sex, TimeVar=Dsur, RiskVar=RISK);

************************************************************************************/


%macro FINEANDGRAY(data=, Timevar=, RiskVar=,ModelVars=,Class=, CensVal=0, RiskVal=1,stratvar=,alpha=0.05,where=,ROUND=4);

%if %length(&where) = 0 %then %let where=1; 

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

	%END;
	%GLOBAL &N;
	%LET &N=%EVAL(&NUMERO-1);
%mend rchv;

%rchv(SUITE=&ModelVars,COMPTE=VAR,SEP=" ",N=NB);
%rchv(SUITE=&CLASS,COMPTE=VARcla,SEP=" ",N=NBcla);


** Check if variables put into CLASS parameter belong to MODELVARS parameter;
%do i=1 %to &NBcla;
	%let flag&i =0;

	%do j=1 %to &NB;

		%if &&VAR&j = &&VARcla&i %then %do;
			%let flag&i=1;
		%end;
	
	%end;
%end;


	%let temp=0;
	%do k=1 %to &NBcla;
		%let temp=%eval(&temp. + &&flag&k..);
	%end;

%do j=1 %to &NB;
	%let othflag&j =1;
	%do i=1 %to &NBcla;

		%if &&VAR&j = &&VARcla&i %then %do;
			%let othflag&j=0;
		%end;
	
	%end;
%end;


%let newlist=;
	%do j=1 %to &NB;
		%if &&othflag&j=1 %then %let newlist=&newlist &&VAR&j ;
	%end;

%rchv(SUITE=&newlist,COMPTE=NEWVAR,SEP=" ",N=NBnew);


proc freq data=&data noprint;
%do i=1 %to &NBcla;
tables &&VARcla&i/out=_cla&i;
%END;
run;

%DO i=1 %to &NBcla;
	data _cla&i;
		set _cla&i;
		one=1;
	run;
%GCOUNT (DATA=_cla&i,BYVAR=one, OUTDATA=_ncla&i);

data _null_;
set _ncla&i;
call symput("flag_cla&i",c_cla&i);
run;

%if &&flag_cla&i <3 %then %do;

	%PUT ************************;
	%PUT ERROR:;
	%PUT PB with variable "&&VARcla&i" : the user don t need to put it in CLASS parameter as this variable has less than 3 levels.;
	%PUT FINEANDGRAY macro is stopped now.;
	%PUT ************************;

	%goto fin;

%end;

%END;


*if one or several variables from CLASS parameter do not belong to MODELVARS parameter
* then the macro is stopped.;

%IF &NBcla ne &temp %THEN %DO;
	%PUT ************************;
	%PUT ERROR:;
	%put One or several variable(s) contained into CLASS parameter does(do) not belong to MODELVARS parameter;
	%put FINEANDGRAY macro is stopped now.;
	%PUT ************************;
	%goto fin ;

%END;


%ELSE %DO;

ods rtf ;

data _work;
set &data;
	%if %length(&stratvar) > 0 %then  mystrat=&stratvar;
	%else mystrat=1;;
run;

%let censcrr=_censcrr_;
%let cengroup=&stratvar.;


data _work;
set _work;
if &RiskVar=&RiskVal then _status=1;
else if &RiskVar=&CensVal then _status=0;
 else _status=2;
_time=&TimeVar;
 %let ID=_id_;
 _id_=_n_;
 %let byempty=1;
 %let by=_by_;
 _by_=1;
	where &where;

	keep mystrat _status _time &stratvar. &ModelVars &by &id &TIMEVAR. ;
run;

proc sort data=_work;
by &by &cengroup ;
run;

ods graphics off;
proc lifetest data=_work noprint outsurv=_censdist;
time _time*_status(1,2);
by &by &cengroup;
run;
ods graphics;
  

data _censdist;
set _censdist;
 by &by &cengroup _time;
 if last._time;
 _Cdist_=survival;
 _time_=_time;
keep _time_ _time _cdist_ &by &cengroup;
 run;

proc sort;
by &by &cengroup _time ;
run;

data _dat01_crr;
set _work;
&censcrr=_status;
if _status=0 or _status=1;
_start_=0;
_stop_=&TimeVar;
run;

data _dat2_crr;
set _work;
if _status=2;
run;
proc sort;
by &by &cengroup _time ;
run;

data _dat2_crr;
merge _dat2_crr _censdist;
by &by &cengroup _time;
_denom_w_=_cdist_;
if _status=2;
drop _time_ _cdist_;
run;
proc sort;
by &by &cengroup _time;
run;

data _censdist2;
set _censdist;
drop _time;
run;


PROC SQL;
create table _censdist2_tmp as
select &by, _cdist_, %if &cengroup ne %then %do; &cengroup, %end;
  min(_time_) as _time_min_,
  max(_time_) as _time_max_
from _censdist2
group by &by, %if &cengroup ne %then %do; &cengroup, %end; _cdist_
order by &by, %if &cengroup ne %then %do; &cengroup, %end; _time_max_ asc
;

data _censdist2_tmp;
set _censdist2_tmp;
_line_=_n_;
ruN;

proc sql;
create table _censdist2_1_tmp as
select t1.&by, %if &cengroup ne %then %do; t1.&cengroup, %end; t1._cdist_, t2._time_max_ as _time_min_, t1._time_max_, t1._line_
from _censdist2_tmp t1
left outer join _censdist2_tmp t2
  on t1._line_ = t2._line_ + 1 and t1.&by = t2.&by %if &cengroup ne %then %do; and t1.&cengroup=t2.&cengroup %end;
;

create table _dat2a_crr as
 SELECT d.*, c._cdist_, c._time_min_, c._time_max_ FROM _dat2_crr d, _censdist2_1_tmp c
 where d._time <= c._time_max_ and d.&by = c.&by %if &cengroup ne %then %do; and d.&cengroup=c.&cengroup %end;
;


proc sort data=_dat2a_crr out=_dat2a_crr;
by &by &id _time_max_;
run;

data _dat2a_crr;
set _dat2a_crr;
_weight_=_cdist_/_denom_w_;
&censcrr=0;
if _time_min_<_time then do;
 _start_=0;
 _stop_=_time;  *** vorsicht, _start_ auf 0 setzen!***;
 output;
 _start_=_time;
 _stop_=_time_max_;
 output;
end;
else do;
  _start_=_time_min_;
  _stop_=_time_max_;
  output;
end;
run;

data _dat_crr;
set _dat01_crr _dat2a_crr;
if _weight_=. then _weight_=1;
drop _time_min_ _time_max_ _denom_w_ _cdist_ _status _time;
run;

proc sort;
by &by;
run;

proc template;

	define column Common.PValue;
		format = BEST12.;
	end;


	edit Stat.Phreg.ParameterEstimates;
		define column HazardRatio;
			format = BEST12.;
		end;
		define column HRLowerCL;                                                       
         format = BEST12.;                                                        
        end;                                                                    
        define column HRUpperCL;                                                       
         format = BEST12.;                                                        
       end;  

      define Estimate;                                                        
         format = BEST12.;                                                       
      end;                                                                    
                                                                              
      define StdErr;                                                          
         format = BEST12.;                                                       
      end;                                                                    
                                                                              
	end;
run;

	ods rtf exclude all;
	ods listing close;

	 proc phreg data=_dat_crr covs(aggregate) ;

		 %if &Class ne %then %do; 
			%do i=1 %to &NBcla;
		 	 	class &&VARcla&i (ref=first) ;;
			%end;
		 %end;;

		 model (_start_,_stop_)*&censcrr(0)=&ModelVars/rl alpha=&alpha.;
		 weight _weight_;
		 id &id;

		 strata mystrat;

		 %if &byempty=0 %then %do; by &by; %end;

 		 ods output ParameterEstimates=_ParameterEstimates;
		 run;

ods listing;
ods rtf select all;

data Fineandgraymodel;
length variables $100.;
set _ParameterEstimates;
variables=parameter;
coef=estimate;
std=stderr;
hr=HazardRatio;
lowci=HRLowerCL;
upci=HRUpperCL;
myrisk=&RiskVal;
pvalue=ProbChisq;

%IF &CLASS ne %THEN %DO;

	if ClassVal0 ne "" then do;

		variables="factor("||compress(variables)||")"||compress(ClassVal0);
	
	end;	

%END;

	** Roundings;
	%if &round=1  %then %do;
		if round(pvalue,0.1)=0 then pvr='< 0.1';
		else pvr=input(round(pvalue,0.1),$15.);
	%end;
	%if &round=2  %then %do;
		if round(pvalue,0.01)=0 then pvr='< 0.01';
		else pvr=input(round(pvalue,0.01),$15.);
	%end;
	%if &round=3  %then %do;
		if round(pvalue,0.001)=0 then pvr='< 0.001';
		else pvr=input(round(pvalue,0.001),$15.);
	%end;
	%if &round=4  %then %do;
		if round(pvalue,0.0001)=0 then pvr='< 0.0001';
		else pvr=input(round(pvalue,0.0001),$15.);
	%end;
	%if &round=5  %then %do;
		if round(pvalue,0.00001)=0 then pvr='< 0.00001';
		else pvr=input(round(pvalue,0.00001),$15.);
	%end;
	%if &round=6  %then %do;
		if round(pvalue,0.000001)=0 then pvr='< 0.000001';
		else pvr=input(round(pvalue,0.000001),$15.);
	%end;
	%if &round=7  %then %do;
		if round(pvalue,0.0000001)=0 then pvr='< 0.0000001';
		else pvr=input(round(pvalue,0.0000001),$15.);
	%end;
	%if &round=8  %then %do;
		if round(pvalue,0.00000001)=0 then pvr='< 0.00000001';
		else pvr=input(round(pvalue,0.00000001),$15.);
	%end;
	%if &round=9  %then %do;
		if round(pvalue,0.000000001)=0 then pvr='< 0.000000001';
		else pvr=input(round(pvalue,0.000000001),$15.);
	%end;
	%if &round>=10  %then %do;
		if round(pvalue,0.0000000001)=0 then pvr='< 0.0000000001';
		else pvr=input(round(pvalue,0.0000000001),$15.);
	%end;

	%let lcl=%sysevalf((&alpha/2)*100);
	%let ucl=%sysevalf((1-&alpha/2)*100);

	label variables="Parameter";
	label coef="Estimate";
	label hr="Hazard Ratio";
	label std="Standard error";
	label pvalue="P-value";
	label pvr="P-value rounded";
	label lowCI="&lcl%*LCL";
	label upCI="&ucl%*UCL";	

drop label parameter estimate stderr stderrratio ProbChisq HazardRatio HRLowerCL HRUpperCL Chisq

%IF &CLASS ne %THEN ClassVal0;;

run;

	ods listing;

	%if %length(&stratvar) = 0 %then title "Fine and Gray model";;
    
	%if %length(&stratvar) > 0 %then title "Fine and Gray model stratified by &StratVar.";;

	%let lcl=%sysevalf((&alpha/2)*100);
	%let ucl=%sysevalf((1-&alpha/2)*100);

	options nodate nonumber;
	proc print data=FineAndGrayModel noobs label split='*';
	var variables df coef std hr lowCI upCI pvr myrisk;
	label pvr="P-value";
	label lowCI="&lcl%*LCL";
	label upCI="&ucl%*UCL";	format coef std 8.4 hr lowCI upCI 8.2;
	label myrisk="Risk";
	run;
	title;


/*proc datasets noprint;
delete 
 _dat:
 _censdist:
 _pshregopt
 _work
 __x
 _ParameterEstimates
;
quit;*/


proc template;
	delete Stat.Phreg.ParameterEstimates;
	delete Common.PValue;
run;


title3;

%END;

%fin: ;

ods listing;

%mend;

